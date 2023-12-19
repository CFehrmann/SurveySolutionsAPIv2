#' Survey Solutions API call to generate and download the data
#'
#' Generates and downloads the data from your Survey Solutions server.
#'
#' @param server Survey Solutions server address
#' @param questID Questionnaire ID
#' @param version Questionnaire version
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param workspace server workspace, if nothing provided, defaults to primary
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param workStatus define which statuses the file should inlude (i.e. \emph{Restored,Created,SupervisorAssigned,InterviewerAssigned,
#' RejectedBySupervisor,ReadyForInterview,
#' SentToCapi,Restarted,Completed,ApprovedBySupervisor,
#' RejectedByHeadquarters,ApprovedByHeadquarters,Deleted}), if NULL all is exported
#' @param reloadTimeDiff time difference in hours between last generated file and now
#' @param inShinyApp if True, file interacts with shiny progress bar
#' @param verbose if TRUE, prints out information about the progress
#' @param weight_file file path to file with survey weights. if provided, the weights will be added to the export
#' @param from_date if provided, only interviews started on this date will be included
#' @param from_time if provided, only interviews started at this time will be included
#' @param to_date if provided, only interviews started until this date will be included
#' @param to_time if provided, only interviews started until this time will be included
#' @param addTranslation if not NULL, the translation name as specified in the designer, which will then be applied value labels
#' @param process_mapquestions should map questions be processed to spatial (sf) objects
#' @param combineFiles if TRUE, the export will be combined into single data.table, see details for the processing steps
#'
#'
#' @details
#'
#' This API call uses the tab export format and uses information from the included
#' questionnaire document to assign value labels to any factor variables. Currently this is done
#' for single select and multiselect questions.
#' If you export the data with \emph{combineFiles = FALSE} a list will be returned with the following structure:
#' \itemize{
#'   \item it is returned as a LIST with up to 4 different lists. The list names are:
#'           \itemize{
#'              \item \emph{main} Contains the top level data, and (if available interviewer comments)
#'              \item \emph{R1} All rosters in roster level 1
#'              \item \emph{R2} All rosters in roster level 2
#'              \item \emph{R3} All rosters in roster level 3
#'           }
#'   \item Number of lists depends on the level of roster nesting
#'   \item All variable names are transformed to lower case and categorical variables are consistently labeled
#'   \item Consistent id variables are generated with
#'          \itemize{
#'          \item interview__id transformed to id
#'          \item parent ids consistently number starting from id (questionnairid) to idX (maximum id3)
#'          }
#'   \item List elements are returned as data.tables
#'   \item Allows for specification of reload time (i.e. generation of new download file)
#'   \item PRESERVES categorical labels \emph{and} values.
#'   }
#' If however, you export the data with \emph{combineFiles = TRUE} a single data.table containing all the data in wide
#' format will be returned. This is the result of the following processing steps:
#' \itemize{
#'   \item First all roster files are cast into wide format.
#'   \item Second the rosters are merged from bottom to top.
#'   \item Third the result of the previous step is merged with the main file
#' }
#' The resulting data.table contains value labels for factor variables. If the file path
#' to a weight file is provided, the these will be added too, and is as such ready
#' for analysis.
#'
#'
#'
#' @export
#'


suso_export<-function(server = suso_get_api_key("susoServer"),
                      apiUser = suso_get_api_key("susoUser"),
                      apiPass = suso_get_api_key("susoPass"),
                      token = NULL,
                      workspace = suso_get_api_key("workspace"),
                      questID = NULL,
                      version = NULL,
                      from_date = NULL,
                      from_time = "00:00:00",
                      to_date = NULL,
                      to_time = "23:59:59",
                      workStatus=c("All", "SupervisorAssigned", "InterviewerAssigned",
                                   "RejectedBySupervisor", "Completed",
                                   "ApprovedBySupervisor",
                                   "RejectedByHeadquarters",
                                   "ApprovedByHeadquarters"),
                      addTranslation = NULL,
                      reloadTimeDiff=1,
                      inShinyApp=F,
                      verbose = FALSE,
                      weight_file = NULL,
                      process_mapquestions = FALSE,
                      combineFiles = TRUE) {

  # workspace default
  workspace<-.ws_default(ws = workspace)

  # check (.helpers.R)
  .check_basics(token, server, apiUser, apiPass)

  # check if workStatus is valid
  workStatus<-match.arg(workStatus)

  # check if questID is provided & correct
  if(is.null(questID)){
    stop("Please provide a questionnaire ID")
  } else {
    .checkUUIDFormat(questID)
  }

  # check if version is provided & numeric
  if(is.null(version)){
    stop("Please provide a questionnaire version")
  } else {
    .checkNum(version)
  }

  # parse questID and version to id$version
  qid<-paste0(questID, "$", version)

  # check from_date if provided
  if(!is.null(from_date)){
    .checkDate(from_date)
  }

  # check to_date if provided, if not and from_date is provided, set to_date to today
  if(!is.null(to_date)){
    .checkDate(to_date)
  } else if(!is.null(from_date)){
    to_date<-lubridate::today()
  }

  # check if from_date is before to_date
  if(!is.null(from_date) & !is.null(to_date)){
    .checkDateRange(from_date, to_date)
  }

  # build from datetime and to datetime
  if(!is.null(from_date)){
    from_datetime<-paste(from_date, from_time)

    # if to_time is not provided, set to_time to 23:59:59
    if(is.null(to_time)){
      nt<-now()
      to_time<-format(nt, "%H:%M:%S")
    }

    to_datetime<-paste(to_date, to_time)
    .checkDateTimeRange(from_datetime, to_datetime)
  } else {
    from_datetime<-NULL
    to_datetime<-NULL
  }


  # Build the URL, first for token, then for base auth
  if(!is.null(token)){
    url<-.baseurl_token(server, workspace, token, "export", version = "v2")
  } else {
    url<-.baseurl_baseauth(server, workspace, apiUser, apiPass, "export", version = "v2")
  }

  # get argument for class
  args<-.getargsforclass(workspace = workspace)

  # check if export file with same parameters is is available
  tryCatch(
    { resp<-url |>
      httr2::req_perform()

    # get the response data
    if(resp_has_body(resp)){
      # get body by content type
      if(resp_content_type(resp) == "application/json") {
        test_json<-resp_body_json(resp, simplifyVector = TRUE)
        exlist<-data.table::data.table(test_json)
      }
    } else {
      exlist<-data.table(character(0))
    }
    },
    error = function(e) .http_error_handler(e, "exp")
  )


  # CHECK for existing files
  if(nrow(exlist)>0) {
    # subset existing exports & check time diff parameter with last creation date
    # 1. Check qid
    qid<-stringr::str_remove_all(qid, "-")
    exlist_sub<-exlist[QuestionnaireId==qid]
    exlist_sub<-exlist_sub[HasExportFile==T]

    # 2. Check other paramters -->TD

    # 3. Check reload time diff (= difference between last file in
    # exlist start time)
    if(!is.null(reloadTimeDiff)) {
      .checkNum(reloadTimeDiff)

      # latest export start time
      time_limit<-exlist_sub[1, StartDate]
      time_limit<-lubridate::as_datetime(time_limit)
      # current time (!! in UTC)
      current_time<-lubridate::now(tzone = lubridate::tz(time_limit))
      # time difference between now and last start
      timeDiff<-difftime(current_time, time_limit, units = "hours")
      # If reloadtime is smaller, download existing, otherwise new
      if(!is.null(timeDiff) && length(timeDiff)>0 && timeDiff<=reloadTimeDiff) {
        if(interactive()) {
          cli::cli_alert_success(
            "Existing file within desired reload time difference
          of {as.integer(reloadTimeDiff)} hour(s)."
          )
        }
        exlist_sub<-exlist_sub[1]

      } else {
        if(interactive()){
          cli::cli_alert_info(
            "Existing file older than desired reload time difference
          of {as.integer(reloadTimeDiff)} hour(s). Creating new export"
          )
        }
        exlist_sub<-data.table(character(0))
      }


    }
  } else {
    exlist_sub<-data.table(character(0))
  }

  if(nrow(exlist_sub)>0) {
    # Get latest available file id (data sorted by date)
    jobid<-exlist_sub[1,JobId]

    if(interactive()){
      cli::cli_alert_success(
        "\nDownloading existing file with JobID {jobid}.\n"
      )
    }

    url<-url |>
      req_method("GET") |>
      req_url_path_append(jobid, "file") |>
      # add curl options (automatic redirect does not work!)
      httr2::req_options(
        followlocation = 1L,
        unrestricted_auth = 0L,
        tcp_keepalive = 1L
      )

  } else {
    # if exlist_sub is empty, start new export file

    if(interactive()){
      cli::cli_alert_info(
        "\nCreating new export file. This may take a while.\n"
      )
    }

    # add method post
    url<-url |>
      req_method("POST")

    # creat body list
    js_body<-list(
      ExportType = "Tabular", #required
      QuestionnaireId = qid, #required
      InterviewStatus = workStatus, #required
      From = from_datetime, # can be null
      To = to_datetime, # can be null
      TranslationId = addTranslation, #if not null then id
      IncludeMeta = TRUE #required
    )

    # add body
    url<-url |>
      req_body_json(
        js_body
      )

    # create new export file
    tryCatch(
      { resp<-url |>
        httr2::req_perform()

      # get the response data
      if(resp_has_body(resp)){
        # get body by content type
        if(resp_content_type(resp) == "application/json") {
          test_json<-resp_body_json(resp, simplifyVector = T)
          exlist1<-data.table::as.data.table((test_json))
          # name cancel and file link
          if(nrow(exlist1)==2) {
            exlist1[,LinkType:=c("cancel","download")][]
          }
          # return(exlist1)
        }
      }
      },
      error = function(e) .http_error_handler(e, "exp")
    )

    # check file status, and if creation completed, download
    jobid<-exlist1$JobId[1]
    status<-exlist1$ExportStatus[1]
    # remove json body
    url$body<-NULL
    # update path for details request
    url<-url |>
      req_method("GET") |>
      req_url_path_append(jobid)

    # perform reques in while loop until file is ready
    # i. add progress bar
    pb <- txtProgressBar(min = 0, max = 25, style = 3, char = "=")
    counter <- 1
    # ii. add while loop
    while(status != "Completed"){
      # get status
      tryCatch(
        { resp<-url |>
          httr2::req_perform()

        # get the response data
        if(resp_has_body(resp)){
          # get body by content type
          if(resp_content_type(resp) == "application/json") {
            test_json<-resp_body_json(resp, simplifyVector = T)
            status<-test_json$ExportStatus
          }
        }
        },
        error = function(e) .http_error_handler(e, "exp")
      )
      # update progress bar
      setTxtProgressBar(pb, counter)
      counter <- counter + 1
      Sys.sleep(2)
    }

    # close progress bar
    close(pb)

    # when finished get file
    url<-url |>
      req_method("GET") |>
      req_url_path_append("file") |>
      # add curl options (automatic redirect does not work!)
      httr2::req_options(
        followlocation = 1L,
        unrestricted_auth = 0L,
        tcp_keepalive = 1L
      )

  }

    # PROCESSING OF DOWNLOAD
    # DEBUG: print all inputs for check
    if(verbose){
      cat("\n\n")
      cat("Server: ", server, "\n")
      cat("Workspace: ", workspace, "\n")
      cat("Url: ", url$url, "\n")
      cat("Questionnaire ID: ", questID, "\n")
      cat("Questionnaire Version: ", version, "\n")
      cat("From Date: ", from_date, "\n")
      cat("From Time: ", from_datetime, "\n")
      cat("To Date: ", to_date, "\n")
      cat("To Time: ", to_datetime, "\n")
      cat("Work Status: ", workStatus, "\n")
      cat("Add Translation: ", addTranslation, "\n")
      cat("Reload Time Difference: ", reloadTimeDiff, "\n")
      cat("In Shiny App: ", inShinyApp, "\n")
      cat("\n\n")
    }


    # temp zip file
    tmp<-tempfile(fileext = ".zip")
    # download file
    tryCatch(
      { resp<-url |>
        httr2::req_perform(path = tmp)
      },
      error = function(e) .http_error_handler(e, "exp")
    )

    # unzip file with unzip package
    # create temp directory jobid
    tmpdir<-file.path(tempdir(), jobid)
    if(!dir.exists(tmpdir)) dir.create(tmpdir)
    # unzip file
    zip::unzip(tmp, exdir = tmpdir)

    # get the questionnaire content
    # i. unpack the json
    zip::unzip(file.path(tmpdir, "Questionnaire", "content.zip"), exdir = file.path(tmpdir, "Questionnaire"))
    # ii. read the json
    ajson<-tidyjson::read_json(file.path(tmpdir, "Questionnaire", "document.json"))
    # iii. created list with questions and validations
    allcontent<-.suso_transform_fullValid_q(ajson)
    allquestions<-allcontent$q

    # get questionnaire variable
    questName <- .get_first_tab_filename(file.path(tmpdir, "export__readme.txt"))

    # get section titles
    section_titles<-allquestions[type== "Group" & !is.na(L0) & is.na(L1), .(L0, Title, PublicKey, VariableName)]

    # get roster titles

    # dynamic roster title subset with quote and eval
    # Function to clear eval string
    .remove_na_patterns <- function(input_string) {
      # Remove '& is.na(NA)'
      modified_string <- gsub("& is\\.na\\(NA\\)", "", input_string)

      # Remove '& !is.na(NA)'
      modified_string <- gsub("& !is\\.na\\(NA\\)", "", modified_string)

      # Trim leading and trailing whitespace
      modified_string <- trimws(modified_string)

      return(modified_string)
    }

    # check for L0, L1, L2, L3 names in data.table
    Lall<-c("L0", "L1", "L2", "L3")
    Lindata<-names(allquestions)[names(allquestions) %in% Lall]

    #roscols<-c("Title", "PublicKey", "VariableName", "..JSON", "L0", "L1", "L2", "L3", "L4")
    roscols<-c("Title", "PublicKey", "VariableName", "..JSON", Lindata)

    # L1
    L1conditionNA<-sprintf("!is.na(%s) & !is.na(%s) & is.na(%s) & is.na(%s)", Lindata[1], Lindata[2], Lindata[3], Lindata[4])
    L1conditionNA<-.remove_na_patterns(L1conditionNA)
    L1condition<-paste("type== \"Group\" &", L1conditionNA)
    roster_titlesL1<-allquestions[eval(parse(text = L1condition)) , .SD, .SDcols=roscols]

    # L2 (only if L2 is in Lindata)
    if("L2" %in% Lindata & nrow(roster_titlesL1)>0){
      L2conditionNA<-sprintf("!is.na(%s) & !is.na(%s) & !is.na(%s) & is.na(%s)", Lindata[1], Lindata[2], Lindata[3], Lindata[4])
      L2conditionNA<-.remove_na_patterns(L2conditionNA)
      L2condition<-paste("type== \"Group\" &", L2conditionNA)
      roster_titlesL2<-allquestions[eval(parse(text = L2condition)) , .SD, .SDcols=roscols]

      # L3 (only if L3 is in Lindata)
      if("L3" %in% Lindata & nrow(roster_titlesL2)>0){
        L3conditionNA<-sprintf("!is.na(%s) & !is.na(%s) & !is.na(%s) & !is.na(%s)", Lindata[1], Lindata[2], Lindata[3], Lindata[4])
        L3conditionNA<-.remove_na_patterns(L3conditionNA)
        L3condition<-paste("type== \"Group\" &", L3conditionNA)
        roster_titlesL3<-allquestions[eval(parse(text = L3condition)) , .SD, .SDcols=roscols]
      }
    }

    # get roster titles and source questions keys
    if(exists("roster_titlesL1") && nrow(roster_titlesL1)>0){
      roster_titlesL1[,RsizeKey:=lapply(.SD, function(x) x[[1]]$RosterSizeQuestionId), .SDcols="..JSON", by = .(PublicKey)][,..JSON:=NULL]
      roster_titlesL1[,Rtype:=allquestions[PublicKey==RsizeKey, type], by=RsizeKey]
      roster_titlesL1[,Rvar:=allquestions[PublicKey==RsizeKey, VariableName], by=RsizeKey]
    }

    if(exists("roster_titlesL2") && nrow(roster_titlesL2)>0){
      roster_titlesL2[,RsizeKey:=lapply(.SD, function(x) x[[1]]$RosterSizeQuestionId), .SDcols="..JSON", by = .(PublicKey)][,..JSON:=NULL]
      roster_titlesL2[,Rtype:=allquestions[PublicKey==RsizeKey, type], by=RsizeKey]
      roster_titlesL2[,Rvar:=allquestions[PublicKey==RsizeKey, VariableName], by=RsizeKey]
    }

    if(exists("roster_titlesL3") && nrow(roster_titlesL3)>0){
      roster_titlesL3[,RsizeKey:=lapply(.SD, function(x) x[[1]]$RosterSizeQuestionId), .SDcols="..JSON", by = .(PublicKey)][,..JSON:=NULL]
      roster_titlesL3[,Rtype:=allquestions[PublicKey==RsizeKey, type], by=RsizeKey]
      roster_titlesL3[,Rvar:=allquestions[PublicKey==RsizeKey, VariableName], by=RsizeKey]
    }
    # bind for processing of merge variables
    nros<-length(Lindata)-1
    rosterall_cond<-paste0("list(",paste(sprintf("R%s=roster_titlesL%s", 1:nros, 1:nros), collapse = ","), ")")
    rosterall<-data.table::rbindlist(eval(rlang::parse_expr(rosterall_cond)), idcol = "roster", fill = TRUE)
    # "R1=roster_titlesL1,R2=roster_titlesL2,R3=roster_titlesL3"
    # rosterall<-data.table::rbindlist(list(R1=roster_titlesL1, R2=roster_titlesL2), idcol = "roster", fill = TRUE)

    rosterall[,RvarMerge:=paste0(.SD[1], "__id"), .SDcols = "VariableName",by=.(RsizeKey, Rtype, Rvar)]
    rosterall[,parentid1:=.SD[1], .SDcols = c("RvarMerge"), by=.(L0, L1)]
    rosterall[,parentid2:=.SD[1], .SDcols = c("RvarMerge"), by=.(L0, L1, L2)]

    # split again
    if(exists("roster_titlesL1") && nrow(roster_titlesL1)>0){
      roster_titlesL1<-rosterall[eval(parse(text = L1conditionNA))]
    }

    if(exists("roster_titlesL2") && nrow(roster_titlesL2)>0){
      roster_titlesL2<-rosterall[eval(parse(text = L2conditionNA))]
    }

    if(exists("roster_titlesL3") && nrow(roster_titlesL3)>0){
      roster_titlesL3<-rosterall[eval(parse(text = L3conditionNA))]
    }


    # get all questions
    # L0 = Section, L1=section index, L2=roster L1 index, L3=roster L2 index, L4=roster L3 index
    # create string for eval expression: .(L0, L1, L2, L3, L4, Title, PublicKey, VariableName, type, ..JSON)
    questcols<-sprintf(".(%s)", paste0(c(Lindata, "Title", "PublicKey", "VariableName", "type", "..JSON"), collapse = ","))
    quest<-.questionnaire_allquestions(allquestions)[,eval(parse(text = questcols))]
    # split by roster level



    # get gps questions
    qgps<-.questionnaire_gpsquestion(quest)

    # get list questions
    qlist<-quest[type %in% c("TextListQuestion")]

    # get response options for MultyOptionsQuestion, SingleQuestion
    qsel<-quest[type %in% c("MultyOptionsQuestion", "SingleQuestion")]
    # loop over questions and get response options
    qseloptions<-list()
    for(i in 1:nrow(qsel)){
      qseloptions[[qsel$VariableName[i]]]<-.questionnaire_answeroptions(qsel$..JSON[i])
    }
    # combine list with VariableName, AnswerValue, AnswerText
    qseloptions<-data.table::rbindlist(qseloptions, idcol = "VariableName")

    # add information from qsel
    qseloptions<-qseloptions[qsel, on = .(VariableName)]

    # separate by roster level
    qseloptionsL1<-qseloptions[eval(parse(text = L1conditionNA))]
    if(exists("roster_titlesL1") && nrow(roster_titlesL1)>0) qseloptionsL2<-qseloptions[eval(parse(text = L2conditionNA))]
    if(exists("roster_titlesL2") && nrow(roster_titlesL2)>0) qseloptionsL3<-qseloptions[eval(parse(text = L3conditionNA))]
    if(exists("roster_titlesL3") && nrow(roster_titlesL3)>0) {
      L4conditionNA<-paste0(L3conditionNA, " & !is.na(L3)")
      qseloptionsL4<-qseloptions[eval(parse(text = L4conditionNA))]}



    # get the survey data
    files<-.list_export_tab_files(tmpdir)

    ###################################################################################################
    ##                            DATA PROCESSING                                                     #
    ##            All data is extracted into a list seperated by its hirarichial positin in the
    ##            questionnaire. The structure is
    ##            [DF name]$main ... inferview comments, level 0
    ##            [DF name]$R1   ... level 1
    ##            [DF name]$R2   ... level 2 etc.
    ###################################################################################################

    #########################################
    ## Extracting files with loop

    ##  Functions for files
    file_collector<-list()
    file_collector.main<-list()
    file_collector.rost.L1<-list()
    file_collector.rost.L2<-list()
    file_collector.rost.L3<-list()

    ########################################
    ##  The export loop
    ##    - all from tmp dir
    ##    - checks roster by id
    for (file_zip in files) {
      name<-basename(tools::file_path_sans_ext(file_zip))
      # read the file
      tmp_file<-tryCatch(data.table::fread(file_zip),
                         error=function(e) return(NULL))

      # remove empty columns
      tmp_file<-.export_remove_na_columns(tmp_file)
      # get L0 data
      if(name==questName){
        # convert factor vars
        if(exists("qseloptionsL1") && nrow(qseloptionsL1)>0) tmp_file<-.export_convert_to_factor(tmp_file, qseloptions)
        file_collector.main[[name]] <- tmp_file
        #if(is.null(tmp_file)) {print(paste("ERROR in dta file:", file_zip));next()}
      }

      # get L1 data
      if(exists("roster_titlesL1") && name%in%roster_titlesL1$VariableName){
        # convert factor vars
        if(exists("qseloptionsL2") && nrow(qseloptionsL2)>0) tmp_file<-.export_convert_to_factor(tmp_file, qseloptions)
        file_collector.rost.L1[[name]] <- tmp_file
      }

      # get L2 data
      if(exists("roster_titlesL2") && name%in%roster_titlesL2$VariableName){
        # convert factor vars
        if(exists("qseloptionsL3") && nrow(qseloptionsL3)>0) tmp_file<-.export_convert_to_factor(tmp_file, qseloptions)
        file_collector.rost.L2[[name]] <- tmp_file
      }

      # get L3 data
      if(exists("roster_titlesL3") && name%in%roster_titlesL3$VariableName){
        # convert factor vars
        if(exists("qseloptionsL4") && nrow(qseloptionsL4)>0) tmp_file<-.export_convert_to_factor(tmp_file, qseloptions)
        file_collector.rost.L3[[name]] <- tmp_file
      }
    }


    ######################################################################
    ##  COLLECTING THE LISTS
    ##  1. MAIN (always exists, no check)
    file_collector[["main"]]<-file_collector.main
    ##  2. ROSTER LEVEL 1

    if(exists("file_collector.rost.L1")){
      file_collector[["R1"]]<-file_collector.rost.L1}
    ##  3. ROSTER LEVEL 2
    if(exists("file_collector.rost.L2")){
      file_collector[["R2"]]<-file_collector.rost.L2}
    ##  4. ROSTER LEVEL 3
    if(exists("file_collector.rost.L3")){
      file_collector[["R3"]]<-file_collector.rost.L3}

    # RETURN EITHER LIST WITH INDIVIDUAL FILES OR MERGED ROSTER AS LIST, LIKE SurveySolutionsAPI package
    if(!combineFiles) {
      return(file_collector)
    } else {
      ###################################################################################################
      ##                            DATA PROCESSING                                                     #
      # rosterall<-data.table::rbindlist(list(R1=roster_titlesL1, R2=roster_titlesL2, R3=roster_titlesL3), idcol = "roster")

      # a. dcast all data.tables ->create cast argument with eval/parse and from rosterall file
      #   step1: check if cast variable is numeric, if yes add prefix, take VariableName
      #   step2: create valvar from names not in cast argument, if 0 length, add VariableName_dummy and NA
      #   step3: dcast

      # file_collector<-copy(ccc)

      # L3:
      if(exists("file_collector.rost.L3")){
        for(rn in names(file_collector$R3)) {
          rid1<-roster_titlesL3[VariableName==rn, parentid1]
          rid2<-roster_titlesL3[VariableName==rn, parentid2]
          rmerge<-roster_titlesL3[VariableName==rn, RvarMerge]
          varname<-roster_titlesL3[VariableName==rn, VariableName]
          # check numeric
          if(is.numeric(file_collector$R3[[rn]][[rmerge]])) file_collector$R3[[rn]][[rmerge]]<-paste(varname, file_collector$R3[[rn]][[rmerge]], sep = "_")
          # get valvars
          valvar<-names(file_collector$R3[[rn]])[
            !(names(file_collector$R3[[rn]]) %in% c("interview__key", "interview__id", rid1, rid2, rmerge))
          ]
          # if none, add dummy
          if(length(valvar)==0) {
            dummy<-paste0(varname,"_dummy")
            file_collector$R3[[rn]][[dummy]]<-NA
            #set(file_collector$R2[[rn]], j = dummy, value = NA)
            valvar<-dummy
          }
          # create argume with sprintf eval(rlang::parse_expr(rosterall_cond))
          dcarg<-sprintf("interview__key + interview__id + %s + %s ~ %s", rid1, rid2, rmerge)
          # dcast
          file_collector$R3[[rn]]<-data.table::dcast(file_collector$R3[[rn]], eval(rlang::parse_expr(dcarg)), value.var = valvar)
        }

      }

      if(exists("file_collector.rost.L2")){
        for(rn in names(file_collector$R2)) {
          rid1<-roster_titlesL2[VariableName==rn, parentid1]
          rmerge<-roster_titlesL2[VariableName==rn, RvarMerge]
          varname<-roster_titlesL2[VariableName==rn, VariableName]
          # check numeric
          if(is.numeric(file_collector$R2[[rn]][[rmerge]])) file_collector$R2[[rn]][[rmerge]]<-paste(varname, file_collector$R2[[rn]][[rmerge]], sep = "_")
          # get valvars
          valvar<-names(file_collector$R2[[rn]])[
            !(names(file_collector$R2[[rn]]) %in% c("interview__key", "interview__id", rid1, rmerge))
          ]
          # if none, add dummy
          if(length(valvar)==0) {
            dummy<-paste0(varname,"_dummy")
            file_collector$R2[[rn]][[dummy]]<-NA
            #set(file_collector$R2[[rn]], j = dummy, value = NA)
            valvar<-dummy
          }
          # create argume with sprintf eval(rlang::parse_expr(rosterall_cond))
          dcarg<-sprintf("interview__key + interview__id + %s ~ %s", rid1, rmerge)
          # dcast
          file_collector$R2[[rn]]<-data.table::dcast(file_collector$R2[[rn]], eval(rlang::parse_expr(dcarg)), value.var = valvar)
        }

      }

      if(exists("file_collector.rost.L1")){
        for(rn in names(file_collector$R1)) {
          rmerge<-roster_titlesL1[VariableName==rn, RvarMerge]
          varname<-roster_titlesL1[VariableName==rn, VariableName]
          # check numeric
          if(is.numeric(file_collector$R1[[rn]][[rmerge]])) {
            file_collector$R1[[rn]][[rmerge]]<-paste(varname, file_collector$R1[[rn]][[rmerge]], sep = "_")
          }
          # get valvars
          valvar<-names(file_collector$R1[[rn]])[
            !(names(file_collector$R1[[rn]]) %in% c("interview__key", "interview__id", rmerge))
          ]
          # if none, add dummy
          if(length(valvar)==0) {
            dummy<-paste0(varname,"_dummy")
            file_collector$R1[[rn]][[dummy]]<-NA
            #set(file_collector$R1[[rn]], j = dummy, value = NA)
            valvar<-dummy
          }
          # create argume with sprintf eval(rlang::parse_expr(rosterall_cond))
          dcarg<-sprintf("interview__key + interview__id ~ %s", rmerge)
          # dcast
          file_collector$R1[[rn]]<-data.table::dcast(file_collector$R1[[rn]], eval(rlang::parse_expr(dcarg)), value.var = valvar)
        }
      } else {
        stop("Nothing to process", call. = F)
      }

      ## MERGE ##########################
      ## 3 Levels

      if(exists("roster_titlesL3") && nrow(roster_titlesL3)>0) {
        l4<-names(file_collector$R3)

        mainout<-file_collector$main[[questName]]

        # L3:
        # check for same parentid & mege all files within the group into one
        roster_titlesL3[, parentgroup:= .GRP, by = parentid2][, pargroupcount:=.N, by=parentgroup]
        # if group count larger 1 merge within

        for(rn in l4) {
          if(is.null(file_collector$R3[[rn]])) next()
          grcount<-roster_titlesL3[VariableName==rn, pargroupcount]
          rid2<-roster_titlesL3[VariableName==rn, parentid2]
          pros2<-stringr::str_remove(rid2, "__id")
          if(grcount>1) {
            grid<-roster_titlesL3[VariableName==rn, parentgroup]
            grmem<-roster_titlesL3[parentgroup == grid & VariableName!=rn, VariableName]
            for(rng in grmem) {
              file_collector$R3[[rn]]<-merge(
                file_collector$R3[[rn]],
                file_collector$R3[[rng]]
              )
              # set element to null
              file_collector$R3[[rng]]<-NULL
            }
          }

          file_collector$R2[[pros2]] <- merge(
            file_collector$R2[[pros2]],
            file_collector$R3[[rn]]
          )
        }

        # L2:
        l3<-names(file_collector$R2)
        # check for same parentid & mege all files within the group into one
        roster_titlesL2[, parentgroup:= .GRP, by = parentid1][, pargroupcount:=.N, by=parentgroup]
        # if group count larger 1 merge within
        for(rn in l3) {
          # next if null
          if(is.null(file_collector$R2[[rn]])) next()
          grcount<-roster_titlesL2[VariableName==rn, pargroupcount]
          rid2<-roster_titlesL2[VariableName==rn, parentid1]
          pros2<-stringr::str_remove(rid2, "__id")
          # merge within group if greater 1
          if(grcount>1) {
            grid<-roster_titlesL2[VariableName==rn, parentgroup]
            grmem<-roster_titlesL2[parentgroup == grid & VariableName!=rn, VariableName]
            for(rng in grmem) {
              if(is.null(file_collector$R2[[rng]])) next()
              file_collector$R2[[rn]]<-merge(
                file_collector$R2[[rn]],
                file_collector$R2[[rng]]
              )
              # set element to null
              file_collector$R2[[rng]]<-NULL
            }
          }

          file_collector$R1[[pros2]] <- merge(
            file_collector$R1[[pros2]],
            file_collector$R2[[rn]]
          )
        }


        # L1:
        l2<-names(file_collector$R1)

        # check for same parentid & mege all files within the group into one --> not necessary on top, check!!
        # roster_titlesL1[, parentgroup:= .GRP, by = parentid1][, pargroupcount:=.N, by=parentgroup]
        # if group count larger 1 merge within
        for(rn in l2) {
          # next if null
          if(is.null(file_collector$R1[[rn]])) next()

          mainout <- merge(
            mainout,
            file_collector$R1[[rn]]
          )
        }

        return(mainout)
      } else if(exists("roster_titlesL2") && nrow(roster_titlesL2)>0) {
        l3<-names(file_collector$R2)

        mainout<-file_collector$main[[questName]]

        # L2:
        # check for same parentid & mege all files within the group into one
        roster_titlesL2[, parentgroup:= .GRP, by = parentid1][, pargroupcount:=.N, by=parentgroup]
        # if group count larger 1 merge within
        for(rn in l3) {
          # next if null
          if(is.null(file_collector$R2[[rn]])) next()
          grcount<-roster_titlesL2[VariableName==rn, pargroupcount]
          rid2<-roster_titlesL2[VariableName==rn, parentid1]
          pros2<-stringr::str_remove(rid2, "__id")
          # merge within group if greater 1
          if(grcount>1) {
            grid<-roster_titlesL2[VariableName==rn, parentgroup]
            grmem<-roster_titlesL2[parentgroup == grid & VariableName!=rn, VariableName]
            for(rng in grmem) {
              file_collector$R2[[rn]]<-merge(
                file_collector$R2[[rn]],
                file_collector$R2[[rng]]
              )
              # set element to null
              file_collector$R2[[rng]]<-NULL
            }
          }

          file_collector$R1[[pros2]] <- merge(
            file_collector$R1[[pros2]],
            file_collector$R2[[rn]]
          )
        }


        # L1:
        l2<-names(file_collector$R1)
        # check for same parentid & mege all files within the group into one --> not necessary on top, check!!
        # roster_titlesL1[, parentgroup:= .GRP, by = parentid1][, pargroupcount:=.N, by=parentgroup]
        # if group count larger 1 merge within
        for(rn in l2) {
          # next if null
          mainout <- merge(
            mainout,
            file_collector$R1[[rn]]
          )
        }

        return(mainout)
      } else if(exists("roster_titlesL1") && nrow(roster_titlesL1)>0) {
        l2<-names(file_collector$R1)

        mainout<-file_collector$main[[questName]]

        # L1:
        # check for same parentid & mege all files within the group into one --> not necessary on top, check!!
        # roster_titlesL1[, parentgroup:= .GRP, by = parentid1][, pargroupcount:=.N, by=parentgroup]
        # if group count larger 1 merge within
        for(rn in l2) {
          # next if null
          mainout <- merge(
            mainout,
            file_collector$R1[[rn]]
          )
        }

        return(mainout)
      } else {
        stop("Nothing to process", call. = F)

      }

    }



  ############################################################################################################
}










































