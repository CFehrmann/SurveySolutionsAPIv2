# INFO: Helper functions for SurveySolutionsAPI package
#
# Check for required http response status.
#
# @param response response from \code{httr::GET()}
# @param status valid status, i.e. 200, 300


.check_response <- function(response, status = 200){
  if (inherits(response, "httr2_response")) {
    if (resp_status(response)!=status) {
      stop("Invalid request! Please check your input parameters.", call. = F)
    }
  } else if (inherits(response, "list")) {
    # !! CHECK how to handle multiple responses
    print("TBA")
  }
}

# check basic inputs
.check_basics<- function(token, server, apiUser, apiPass) {
  if(is.null(token)){
    if(is.null(server) | is.null(apiUser) | is.null(apiPass)){
      stop("Please provide either a token or server, apiUser, and apiPass")
    }
  } else {
    if(is.null(server)){
      stop("Please provide a server address")
    }
  }
}

# base url builder base auth
.baseurl_baseauth<-function(server, workspace, apiUser, apiPass, api, version = "v1"){
  # Build the URL
  url <- request(server) |>
    req_url_path_append(workspace) |>
    req_url_path_append("api") |>
    req_url_path_append(version) |>
    req_url_path_append(api) |>
    req_headers(`User-Agent` = "r_surveysolutionsapi_v2") |>
    req_auth_basic(apiUser, apiPass)
  return(url)
}

# base url builder token
.baseurl_token<-function(server, workspace, token, api, version = "v1"){
  # Build the URL
  url <- request(server) |>
    req_url_path_append(workspace) |>
    req_url_path_append("api") |>
    req_url_path_append(version) |>
    req_url_path_append(api) |>
    req_headers(`User-Agent` = "r_surveysolutionsapi_v2") |>
    req_auth_bearer_token(token)
  return(url)
}

# update url with queries
.addQuery<-function(url, ...){
  url<-url |>
    req_url_query(...)
  return(url)
}

# request generator, move to helpers?
.genrequests_w_offset<- function(i, url,lim = 100, off = 100, multi = TRUE) {
  if(multi) {
    url<-url |>
      req_url_query(
        Limit = lim,
        offset = off*i
      )
  } else {
    url<-url |>
      req_url_query(
        Limit = lim,
        offset = off+i
      )
  }
  return(url)
}

# transform multiple responses to data.table with path and jsonlite
.transformresponses_jsonlite<-function(path, data) {
  # i. Convert to json
  respfull <-jsonlite::fromJSON(path,simplifyVector = T, flatten = TRUE)

  # ii. Get identifying data
  # transform to wide format
  resp<-data.frame(respfull[[data]])
  return(resp)
}

# transform multiple responses to data.table without path and httr2::resp_body_json
.transformresponses<-function(resp, data) {
  # i. Convert to json
  respfull <-resp %>%
    resp_body_json(simplifyVector = T, flatten = TRUE)

  # ii. Get identifying data
  # transform to wide format
  resp<-data.frame(respfull[[data]])
  return(resp)
}

# drop empty columns (string/numeric) in dplyr pipline
.col_selector <- function(col) {
  return(!(all(is.na(col)) | all(col == "")))
}

# check input types
.checkNum<-function(x) {
  if (!is.numeric(x)) stop("Input must be numeric")
}

## workspace default check
.ws_default<-function(ws=NULL){
  ## workspace default
  if(!is.null(ws)) {
    # margs<-suso_getWorkspace()$Name
    # workspace<-match.arg(workspace, margs)
    ws<-ws
  } else {
    ws<-"primary"
  }
  return(ws)
}

# check UUID format
.checkUUIDFormat <- function(id) {
  # Regular expression for UUID format: 8-4-4-4-12
  uuid_pattern <- "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$"

  if(grepl(uuid_pattern, id)) {
    invisible(TRUE)
  } else {
    stop("Invalid UUID format. Please check your input.")
  }
}

# check suso key format
.checkIntKeyformat <- function(id) {
  # Regular expression for interview key format: 86-85-49-18
  key_pattern <- "^[0-9]{2}-[0-9]{2}-[0-9]{2}-[0-9]{2}$"

  if(grepl(key_pattern, id)) {
    invisible(TRUE)
  } else {
    stop("Invalid INterview Key format. Please check your input.")
  }
}

# check basic email format
.checkEmailFormat <- function(email) {
  # Basic email pattern: local-part@domain
  email_pattern <- "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$"

  if(grepl(email_pattern, email)) {
    invisible(TRUE)
  } else {
    stop("Invalid email format. Please check your input.")
  }
}

# check date with lubridate -->ymd
.checkDate<- function(date_string) {
  # parse the date using ymd
  parsed_date <- ymd(date_string, quiet = TRUE)

  # Check if the parsed date is NA (invalid) or if the format is not as expected
  if (is.na(parsed_date) || format(parsed_date, "%Y-%m-%d") != date_string) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

# check date range
.checkDateRange <- function(from_date, to_date) {
  # Parse the dates
  parsed_from_date <- ymd(from_date, quiet = TRUE)
  parsed_to_date <- ymd(to_date, quiet = TRUE)

  # Check if either date is invalid
  if (is.na(parsed_from_date) || format(parsed_from_date, "%Y-%m-%d") != from_date ||
      is.na(parsed_to_date) || format(parsed_to_date, "%Y-%m-%d") != to_date) {
    stop("Invalid date format. Please check your input.")
  }

  # Check if from_date is not before to_date
  if(parsed_from_date >= parsed_to_date) {
    stop("from_date must be before to_date")
  }
}

# check date time range
.checkDateTimeRange <- function(from_datetime, to_datetime) {
  # Parse the datetimes
  parsed_from_datetime <- ymd_hms(from_datetime, quiet = TRUE)
  parsed_to_datetime <- ymd_hms(to_datetime, quiet = TRUE)

  # Check if either datetime is invalid
  if (is.na(parsed_from_datetime) || format(parsed_from_datetime, "%Y-%m-%d %H:%M:%S") != from_datetime ||
      is.na(parsed_to_datetime) || format(parsed_to_datetime, "%Y-%m-%d %H:%M:%S") != to_datetime) {
    stop("Invalid date time format. Please check your input.")
  }

  # Check if from_datetime is not before to_datetime
  if(parsed_from_datetime >= parsed_to_datetime) {
    stop("from_date must be before to_date")
  }
}


# get args for class
.getargsforclass<-function(workspace = NULL) {
  # except server, apiUser, apiPass, token
  #args<-as.list(sys.call(-1))[-1]
  args<-as.list(match.call(
    definition = sys.function(-1),
    call = sys.call(-1)
  ))[-1]
  # defaults<-as.list(
  #   formals(
  #     sys.function(-1)
  #   )
  # )
  # remove server, apiUser, apiPass, token, by name with grepl
  args<-args[!grepl("server|apiUser|apiPass|token", names(args))]
  args$workspace<-workspace
  return(args)
}

# error handler
.http_error_handler <- function(error_condition, type = "ass") {
  # Use a switch or if-else to handle different types of errors
  error_type <- class(error_condition)[1]
  # error messages
  msg404<-switch(type,
                 "ass" = c("x" = "Questionnaire/Assignment/Assignee not found"),
                 "usr" = c("x" = "User not found"),
                 "exp" = c("x" = "Export process was not found")
  )
  msg406<-switch(type,
                 "ass" = c("x" = "Assignee cannot be assigned to assignment"),
                 "usr" = c("x" = "User is not an interviewer or supervisor")
  )

  msg400<-switch(type,
                 "ass" = c("x" = "Bad parameters provided or identifying data incorrect. See response details for more info"),
                 "usr" = c("x" = "User not found"),
                 "exp" = c("x" = "Request is malformed/Export file was not generated yet")
  )

  msg401<-c("x"="Unauthorized/User not authorized.")


  msg403<-c("x" = "Forbidden")

  msg409<-switch(type,
                 "usr" = c("x" = "User cannot be unarchived")
                 )

  withr::with_options(
    list(rlang_backtrace_on_error = "none"),
    switch(error_type,
           "httr2_http_404" = {
             cli::cli_abort(
               message = msg404,
               call = NULL,
             )
           },
           "httr2_http_406" = {
             cli::cli_abort(
               message = msg406,
               call = NULL,
             )
           },
           "httr2_http_400" = {
             cli::cli_abort(
               message = msg400,
               call = NULL,
             )
           },
           "httr2_http_401" = {
             cli::cli_abort(
               message = msg401,
               call = NULL,
             )
           },
           "httr2_http_403" = {
             cli::cli_abort(
               message = msg403,
               call = NULL,
             )
           },
           "httr2_http_409" = {
             cli::cli_abort(
               message = msg409,
               call = NULL,
             )
           },
           # Default case if the error type is not handled above
           cli::cli_abort(
             message = "The following other error occured:",
             call = NULL,
             parent = error_condition,
             .internal = T
           )
    )
  )
}

# list export tab files for processing
.list_export_tab_files <- function(directory) {
  # List all files in the directory
  all_files <- list.files(directory, pattern = "\\.tab$", full.names = TRUE)

  # List of files to exclude
  exclude_files <- c("assignment__actions.tab",
                     "interview__actions.tab",
                     "interview__comments.tab",
                     "interview__diagnostics.tab",
                     "interview__errors.tab")

  # Filter out the excluded files
  valid_files <- all_files[!basename(all_files) %in% exclude_files]

  return(valid_files)
}

# get questionnaire name from export__readme.txt
.get_first_tab_filename <- function(file_path) {
  # Read lines
  lines <- readLines(file_path)

  # regex for .tab
  tab_files <- grep("\\.tab$", lines, value = TRUE)

  # Check if any .tab file is found
  if (length(tab_files) > 0) {
    # Extract the first .tab filename without the extension
    first_tab_file <- sub("\\.tab$", "", tab_files[1])
    return(first_tab_file)
  } else {
    return(NULL)
  }
}

# get all questions from questionnaire
.questionnaire_allquestions <- function(dt) {
  # Check if 'type' column exists
  if (!("type" %in% names(dt))) {
    stop("The 'type' column does not exist in the data table.")
  }

  # Identify rows where 'type' contains 'Question'
  rows_with_question <- dt[grepl("Question", type), ]

  return(rows_with_question)
}

# get all questions from questionnaire
.questionnaire_gpsquestion <- function(dt) {
  # Check if 'type' column exists
  if (!("type" %in% names(dt))) {
    stop("The 'type' column does not exist in the data table.")
  }

  # Identify rows where 'type' contains 'Gps'
  rows_with_question <- dt[grepl("Gps", type), ]

  return(rows_with_question)
}

.questionnaire_answeroptions <- function(input_list) {
  # Initialize an empty data frame
  answers_df <- data.frame(AnswerValue = character(),
                           AnswerText = character(),
                           stringsAsFactors = FALSE)

  # Iterate through the outer list
  for (item in input_list) {
    # Check if 'Answers' is in the list
    if ("Answers" %in% names(item)) {
      # Extract the 'Answers' sublist
      answers <- item$Answers

      # Iterate through the 'Answers' sublist and add to the data frame
      for (answer in answers) {
        answers_df <- rbind(answers_df, data.frame(AnswerValue = answer$AnswerValue,
                                                   AnswerText = answer$AnswerText,
                                                   stringsAsFactors = FALSE))
      }
    }
  }

  return(answers_df)
}

# convert multi/single select to factor
.export_convert_to_factor <- function(dt, labels_dt) {
  dt<-copy(dt)
  # make values numeric if not
  if(!is.numeric(labels_dt$AnswerValue)) {
    labels_dt[, AnswerValue := as.numeric(AnswerValue)]
  }
  on.exit(
    rm(dt),
    gc()
  )
  # Unique base variable names
  base_vars <- unique(sub("__.*", "", names(dt)))
  # Use only base vars where factor is available
  base_vars <- base_vars[base_vars %in% labels_dt$VariableName]
  # Return dt if not base vars
  if (length(base_vars) == 0) {
    return(dt)
  }

  # Iterate over each base variable name
  for (base_var in base_vars) {
    # Find all columns related to this base variable
    related_cols <- grep(paste0("^", base_var, "__"), names(dt), value = TRUE)
    dt[, c(base_var) := numeric(.N)]
    # Consolidate into a single variable
    for (col in related_cols) {
      fval<-as.integer(sub(paste0("^", base_var, "__"), "", col))
      dt[[base_var]]<-data.table::fifelse(dt[[col]] == 1, fval, dt[[base_var]])
    }

    # Drop the original long-form columns -->suppress the warning
    suppressWarnings(
      dt[, (related_cols) := NULL]
    )

    # Check if the base variable name is in the VariableName column of labels_dt
    if (base_var %in% labels_dt$VariableName) {
      # Extract labels_dt
      label_rows <- labels_dt[VariableName == base_var]

      # Create factor
      dt$tmpvar<-factor(dt[[base_var]], levels = label_rows$AnswerValue, labels = label_rows$AnswerText)
      # Set NULL base_var
      dt[, c(base_var) := NULL][]
      # Rename tmpvar to base_var
      data.table::setnames(dt, "tmpvar", base_var)
    }
  }

  return(dt)
}

# remove full NA columns in suso data
.export_remove_na_columns <- function(dt) {

  # integers to remove
  intcols<-which(sapply(dt, function(x) is.integer(x) & all(x == -999999999L)))
  if(length(intcols)>0) dt[, (intcols) := NULL]
  # characters to remove
  charcols<-which(sapply(dt, function(x) !lubridate::is.POSIXt(x)  && is.character(x) && all(x == "##N/A##")))
  if(length(charcols)>0) dt[, (charcols) := NULL]

  return(dt)
}



# PARADATA ONLY ADD LATER
# Response Time
# #' @import data.table
# .calcTimeDiff<-function(DTfile, by=c("interview__id","DAY", "MONTH")){
#   DTfile<-copy(DTfile)
#   setorderv(DTfile, c("interview__id","date", "time", "counter"))
#   #DTfile<-DTfile[,.SD[.N>20], by=.(interview__id)]
#   DTfile[,resp_time:=as.integer(0)][ ,resp_time:=fifelse((data.table::shift(date, type = "lag")==date & action!="Paused"),
#                                                          as.integer(time-data.table::shift(time, type = "lag")), 0L), by=.(interview__id)][]
#   DTfile[, breaks:=fifelse(resp_time>120&!is.na(resp_time),1L,0L), by=.(interview__id)]
#   return(DTfile)
# }
# Unpack Function
# .unpack<-function(fp, allResponses, inShinyServer){
#   ####################################################
#   ##  1. Use fread from data.table (fastest, needs to be tested for reliability)
#   HEADER<-c("interview__id", "counter", "action", "responsible", "role", "time", "tz","var_resp")
#   file<-fread(file=fp,
#               header = T,
#               data.table = TRUE,
#               #col.names = HEADER,
#               fill = T,sep = "\t",
#               colClasses = list(character=c(1,3:8),
#                                 numeric=c(2)),
#               blank.lines.skip=TRUE,
#               encoding = "UTF-8")
#   names(file)<-HEADER
#   ## check lines
#   if(nrow(file)==0) return(NULL)
#   ## message if large file
#   if(nrow(file)>100000) cat("There are", nrow(file), "individual events. Processing may take a while!")
#
#   ## splitting response variable
#   #names(file)<-
#   ## Exract the VARIABLENAME
#   resps<-file[,tstrsplit(var_resp, "||", fixed=T, names = T, fill = "<NA>")][]
#   file[,var:=resps[,.(V1)]]
#   if (allResponses) {
#     ## Extract all responses if TRUE
#     resps1<-resps[,tstrsplit(V2, "(\\|)|(,)", fixed=F, names = T, fill = "<NA>")][]
#     resps2<-resps[,tstrsplit(V3, "(,)", fixed=F, names = T, fill = "<NA>")][]
#     splits <- (length(resps1))
#     file[,c(paste0("response", 1:(length(resps1)))):=resps1]
#     file[,c(paste0("rid", 1:(length(resps2)))):=resps2]
#   } else {
#     ## Extract only singel vector-->FASTER
#     file[,response:=resps[,.(V2)]]
#   }
#
#   if(inShinyServer) incProgress(amount = 0.25, message = "Data loaded")
#   if (nrow(file)==0) return(NULL)
#   ####################################################
#   ##  3. Create Factors
#   #file[,interview__id:=as.factor(interview__id)]
#   file[,action:=as.factor(action)]
#   file[,responsible:=as.factor(responsible)]
#   file[,role:=as.factor(role)]
#   #file[,var:=as.factor(var)]
#   ## only with all responses factorization of response
#   if (allResponses) {
#     for (i in 1:splits) {
#       ip<-paste0("response", (i))
#       file[,c(ip):=as.factor(get(ip))]
#     }
#   }
#
#   ##  4. DATE is adjusted for timezone!
#   file[,dateTime:=as_datetime(time)-hms(tz)][,c("tz"):=NULL]
#   file[,time:=as.ITime(dateTime)]
#   file[,date:=as.IDate(dateTime)]
#   file[,wDAY:=wday(date)]
#   file[,mDAY:=mday(date)]
#   file[,MONTH:=month(date)]
#   file[,WEEK:=isoweek(date)]
#   setkeyv(file, c("interview__id", "responsible"))
#   return(file)
#   prog<-prog+1
# }
#


#' Helper functions to transform list of questionnair structure to single data.table with all variables
#'
#' Uses tidyjson package
#'
#' \code{suso_transform_fullMeta} transforms the list containing the structure (\emph{operation.type = structure})
#' is transformed into a single data.table
#' with all variable names, types etc.. This also works with json strings manually exported from the server.
#'
#'
#' @param input returned by \code{suso_getQuestDetails} structure operation
#'
#' @keywords internal
#'
#' @return data.table with all variables in the questionnaire
#'

.suso_transform_fullValid_q <- function(input = NULL) {
  ##########################################
  ## v2.1 with validations
  ## IDs:
  ##    L0 = Section, L1=position inside section,
  ##    L2 = Roster/Subsection Nr, (when missing no roster)
  ##    L3 = position inside Roster/Subsection,
  ##    L4 = Roster/Subsection (when missing no roster)
  qfinal <-bind_rows(
    ######################################################
    ## first
    input |> spread_values(
      Id = jstring("Id"),
      LastEntryDate = jstring("LastEntryDate")
    ) |> enter_object("Children") |> gather_array("L0") |>
      spread_values(
        type = jstring("$type"),
        PublicKey = jstring("PublicKey"),
        Title = jstring("Title")
      ),
    ## second
    input |> enter_object("Children") |> gather_array("L0") |>
      enter_object("Children") |> gather_array("L1") |>
      spread_values(
        type = jstring("$type"),
        PublicKey = jstring("PublicKey"),
        Title = jstring("Title"),
        VariableName = jstring("VariableName"),
        QuestionScope = jnumber("QuestionScope"),
        QuestionText = jstring("QuestionText"),
        Featured = jlogical("Featured")

      ),
    ## third
    input |> enter_object("Children") |> gather_array("L0") |>
      enter_object("Children") |> gather_array("L1") |>
      enter_object("Children") |> gather_array("L2") |>
      spread_values(
        type = jstring("$type"),
        PublicKey = jstring("PublicKey"),
        Title = jstring("Title"),
        VariableName = jstring("VariableName"),
        QuestionScope = jnumber("QuestionScope"),
        QuestionText = jstring("QuestionText"),
        Featured = jlogical("Featured")
      ),
    ## fourth
    input |> enter_object("Children") |> gather_array("L0") |>
      enter_object("Children") |> gather_array("L1") |>
      enter_object("Children") |> gather_array("L2") |>
      enter_object("Children") |> gather_array("L3") |>
      spread_values(
        type = jstring("$type"),
        PublicKey = jstring("PublicKey"),
        Title = jstring("Title"),
        VariableName = jstring("VariableName"),
        QuestionScope = jnumber("QuestionScope"),
        QuestionText = jstring("QuestionText"),
        Featured = jlogical("Featured")
      ),
    ## fifth
    input |> enter_object("Children") |> gather_array("L0") |>
      enter_object("Children") |> gather_array("L1") |>
      enter_object("Children") |> gather_array("L2") |>
      enter_object("Children") |> gather_array("L3") |>
      enter_object("Children") |> gather_array("L4") |>
      spread_values(
        type = jstring("$type"),
        PublicKey = jstring("PublicKey"),
        Title = jstring("Title"),
        VariableName = jstring("VariableName"),
        QuestionScope = jnumber("QuestionScope"),
        QuestionText = jstring("QuestionText"),
        Featured = jlogical("Featured")
      ),
    ## sixth
    input |> enter_object("Children") |> gather_array("L0") |>
      enter_object("Children") |> gather_array("L1") |>
      enter_object("Children") |> gather_array("L2") |>
      enter_object("Children") |> gather_array("L3") |>
      enter_object("Children") |> gather_array("L4") |>
      enter_object("Children") |> gather_array("L5") |>
      spread_values(
        type = jstring("$type"),
        PublicKey = jstring("PublicKey"),
        Title = jstring("Title"),
        VariableName = jstring("VariableName"),
        QuestionScope = jnumber("QuestionScope"),
        QuestionText = jstring("QuestionText"),
        Featured = jlogical("Featured")
      ),
    ## seventh
    input |> enter_object("Children") |> gather_array("L0") |>
      enter_object("Children") |> gather_array("L1") |>
      enter_object("Children") |> gather_array("L2") |>
      enter_object("Children") |> gather_array("L3") |>
      enter_object("Children") |> gather_array("L4") |>
      enter_object("Children") |> gather_array("L5") |>
      enter_object("Children") |> gather_array("L6") |>
      spread_values(
        type = jstring("$type"),
        PublicKey = jstring("PublicKey"),
        Title = jstring("Title"),
        VariableName = jstring("VariableName"),
        QuestionScope = jnumber("QuestionScope"),
        QuestionText = jstring("QuestionText"),
        Featured = jlogical("Featured")
      ),
    ## eight
    input |> enter_object("Children") |> gather_array("L0") |>
      enter_object("Children") |> gather_array("L1") |>
      enter_object("Children") |> gather_array("L2") |>
      enter_object("Children") |> gather_array("L3") |>
      enter_object("Children") |> gather_array("L4") |>
      enter_object("Children") |> gather_array("L5") |>
      enter_object("Children") |> gather_array("L6") |>
      enter_object("Children") |> gather_array("L7") |>
      spread_values(
        type = jstring("$type"),
        PublicKey = jstring("PublicKey"),
        Title = jstring("Title"),
        VariableName = jstring("VariableName"),
        QuestionScope = jnumber("QuestionScope"),
        QuestionText = jstring("QuestionText"),
        Featured = jlogical("Featured")
      )
    ###########################################################
  ) |> dplyr::select_if(.col_selector)
  qfinal<-data.table(qfinal)
  ###########################
  ## dynamic use of sprintf
  ##  - do.call and eval
  allSections<-names(qfinal)[grepl("^L[0-7]$", names(qfinal))]
  sprExpr<-paste(rep("%02d", length(allSections)), collapse = "")
  allSections<-paste0(".(", paste(allSections, collapse = ","), ")")
  qfinal[,intID:=do.call(sprintf, c(list(sprExpr), qfinal[,eval(parse(text = allSections))]))]
  qfinal<-qfinal[,document.id:=NULL][]
  ## Get Validations
  valfinal_1<-.suso_transform_fullValid_val(input = input)
  if(!is.null(valfinal_1)){
    ###########################
    ## dynamic use of sprintf
    ##  - do.call and eval
    ##  - to harmonize ID, ID var is created here!!!
    allSections<-names(valfinal_1)[grepl("^L[0-7]$", names(valfinal_1))]
    sprExpr<-paste(rep("%02d", length(allSections)), collapse = "")
    allSections<-paste0(".(", paste(allSections, collapse = ","), ")")
    ## ID for validations
    valfinal_1[,intID:=do.call(sprintf, c(list(sprExpr), valfinal_1[,eval(parse(text = allSections))]))]
    ## ID for questionnaire
    qfinal[,intID:=do.call(sprintf, c(list(sprExpr), qfinal[,eval(parse(text = allSections))]))]
    qVar<-qfinal[,.(intID, VariableName)]
    valfinal_1<-valfinal_1[,.(intID, Expression, Message, Severity)][]
    setkeyv(valfinal_1, "intID"); setkeyv(qVar, "intID")
    valfinal_1<-valfinal_1[qVar, nomatch=0]
  }
  q_final<-list(q=qfinal, val=valfinal_1)
  return(q_final)
}


.suso_transform_fullValid_val <- function(input = NULL) {
  valfinal <-bind_rows(
    ######################################################
    ## VALIDATIONS (Start with L0)
    ######################################################
    input |> enter_object("Children") |> gather_array("L0") |>
      enter_object("Children") |> gather_array("L1") |>
      enter_object("ValidationConditions") |> gather_array("val1") |>
      spread_all(),
    ## third
    input |> enter_object("Children") |> gather_array("L0") |>
      enter_object("Children") |> gather_array("L1") |>
      enter_object("Children") |> gather_array("L2") |>
      enter_object("ValidationConditions") |> gather_array("val1") |>
      spread_all(),
    ## fourth
    input |> enter_object("Children") |> gather_array("L0") |>
      enter_object("Children") |> gather_array("L1") |>
      enter_object("Children") |> gather_array("L2") |>
      enter_object("Children") |> gather_array("L3") |>
      enter_object("ValidationConditions") |> gather_array("val1") |>
      spread_all(),
    ## fifth
    input |> enter_object("Children") |> gather_array("L0") |>
      enter_object("Children") |> gather_array("L1") |>
      enter_object("Children") |> gather_array("L2") |>
      enter_object("Children") |> gather_array("L3") |>
      enter_object("Children") |> gather_array("L4") |>
      enter_object("ValidationConditions") |> gather_array("val1") |>
      spread_all(),
    ## sixth
    input |> enter_object("Children") |> gather_array("L0") |>
      enter_object("Children") |> gather_array("L1") |>
      enter_object("Children") |> gather_array("L2") |>
      enter_object("Children") |> gather_array("L3") |>
      enter_object("Children") |> gather_array("L4") |>
      enter_object("Children") |> gather_array("L5") |>
      enter_object("ValidationConditions") |> gather_array("val1") |>
      spread_all(),
    ## seventh
    input |> enter_object("Children") |> gather_array("L0") |>
      enter_object("Children") |> gather_array("L1") |>
      enter_object("Children") |> gather_array("L2") |>
      enter_object("Children") |> gather_array("L3") |>
      enter_object("Children") |> gather_array("L4") |>
      enter_object("Children") |> gather_array("L5") |>
      enter_object("Children") |> gather_array("L6") |>
      enter_object("ValidationConditions") |> gather_array("val1") |>
      spread_all(),
    ## eight
    input |> enter_object("Children") |> gather_array("L0") |>
      enter_object("Children") |> gather_array("L1") |>
      enter_object("Children") |> gather_array("L2") |>
      enter_object("Children") |> gather_array("L3") |>
      enter_object("Children") |> gather_array("L4") |>
      enter_object("Children") |> gather_array("L5") |>
      enter_object("Children") |> gather_array("L6") |>
      enter_object("Children") |> gather_array("L7") |>
      enter_object("ValidationConditions") |> gather_array("val1") |>
      spread_all()
    ###########################################################
  ) |> dplyr::select_if(.col_selector)


  valfinal<-data.table(valfinal)
  if(nrow(valfinal)==0) {
    return(NULL)
  } else {
    return(valfinal)
  }
}
