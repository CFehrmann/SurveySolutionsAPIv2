#' Survey Solutions API call for assignment list
#'
#'
#' \code{suso_get_assignments} calls the Survey Solutions assingment API
#'
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param workspace If workspace name is provide requests are made regarding this specific workspace
#' @param questID only assignments for \emph{QuestionnaireId} are returned,
#' requires \code{version} being not NULL
#' @param version version of the questionnaire, only required with \code{questID}
#' @param AssId if NULL a list of all assignments on the server, if not NULL
#' the assignment details for a specific assignment ID
#' @param responsibleID the ID of the responsible user (Supervisor or Interviewer).
#' Retrieves all assignments for this user.
#' @param order.by determines the column by which the assignment list should be ordered, one of
#' \emph{Id}, \emph{ResponsibleName}, \emph{InterviewsCount}, \emph{Quantity}, \emph{UpdatedAtUtc}, \emph{CreatedAtUtc},
#' followed by ordering direction "ASC" or "DESC", e.g. "Id DESC" or "CreatedAtUtc ASC"
#' @param operations.type specifies the desired operation, one of assignmentQuantitySettings, history, or recordAudio,
#' if specified, requires also \emph{AssId} to be specified.
#'
#' @return Returns an S3 object of assignmentClass. If you select any of the operations types, then no data.frame is returned,
#' the data.table will be NULL, however any information returned from the API can be retrieved by using the \code{getinfo()}
#' function with the corresponding arguments.
#'
#' @examples
#' \dontrun{
#'
#' # get all assignment for specific workspace
#' asslist<-suso_get_assignments(
#'                    workspace = "myworkspace"
#'                    )
#' # get all assignment for specific responsible
#' asslist<-suso_get_assignments(
#'                    workspace = "myworkspace",
#'                    responsibleID = "a67d2b82-bf28-40cf-bd1a-7901225c0885"
#'                    )
#' # get the overall count
#' getinfo(asslist, "totalcount")
#'
#' #get all single assignment details
#' asslist<-suso_get_assignments(
#'                    workspace = "myworkspace",
#'                    AssId = 1
#'                    )
#' # retrieve the uid of the person responsible
#' getinfo(asslist, "responsibleid")
#'
#' # get the status of the quantitysettings
#' asslist<-suso_get_assignments(
#'                    workspace = "myworkspace",
#'                    AssId = 1,
#'                    operations.type = "assignmentQuantitySettings")
#' asslist
#' Null data.table (0 rows and 0 cols)
#'
#' getinfo(asslist, "canchangequantity")
#' [1] TRUE
#' }
#'
#'
#' @export
#'

suso_get_assignments<-function(server = suso_get_api_key("susoServer"),
                               apiUser = suso_get_api_key("susoUser"),
                               apiPass = suso_get_api_key("susoPass"),
                               token = NULL,
                               workspace = NULL,
                               questID = NULL,
                               AssId=NULL,
                               version= NULL,
                               responsibleID = NULL,
                               order.by=c("Id ASC", "Id DESC", "ResponsibleName DESC", "ResponsibleName ASC",
                                          "InterviewsCount DESC", "InterviewsCount ASC", "Quantity DESC",
                                          "Quantity ASC", "UpdatedAtUtc DESC", "UpdatedAtUtc ASC",
                                          "CreatedAtUtc DESC", "CreatedAtUtc ASC"),
                               operations.type = c("assignmentQuantitySettings",
                                                   "history",
                                                   "recordAudio")) {


  # workspace default
  workspace<-.ws_default(ws = workspace)

  # check (.helpers.R)
  .check_basics(token, server, apiUser, apiPass)

  # check input types
  # .checkNum(x = AssId)
  # .checkNum(x = version)

  # check for valid operations.type if provided
  # if not return empty character vector
  if(length(operations.type) == 1) {
    operations.type<-match.arg(operations.type)
  } else {
    operations.type<-character(0)
  }

  # check for valid order.by
  order.by<-match.arg(order.by)

  # Build the URL, first for token, then for base auth
  if(!is.null(token)){
    url<-.baseurl_token(server, workspace, token, "assignments")
  } else {
    url<-.baseurl_baseauth(server, workspace, apiUser, apiPass, "assignments")
  }

  # pass on function args for assignmentClass attributes, takes all args,
  # except server, apiUser, apiPass, token
  args<-as.list(match.call())[-1]
  # remove server, apiUser, apiPass, token, by name with grepl
  args<-args[!grepl("server|apiUser|apiPass|token", names(args))]

  # A. get all assignments (with parameters)
  if(length(operations.type)==0 && is.null(AssId)){
    # add order.by
    url<-.addQuery(url, Order = order.by)

    # add questID if not null
    if(!is.null(questID)){
      # check for valid version
      if(is.null(version)){
        stop("Please provide a version number for the questionnaire")
      } else {
        .checkNum(x = version)
        QID<-paste0(questID, "$", version)
        url<-.addQuery(url, QuestionnaireId = QID)
      }
    }
    # add responsibleID if not null
    if(!is.null(responsibleID)){
      url<-.addQuery(url, Responsible = responsibleID)
    }

    # generate and execute first request with limit 100 & offset 0
    url<-.addQuery(url, Limit=100, offset=0)
    resp<-req_perform(url)

    # get the response data
    if(resp_has_body(resp)){
      # get body by content type
      if(resp_content_type(resp) == "application/json") {
        test_json<-resp_body_json(resp, simplifyVector = TRUE)
        full_data1<-data.table::data.table(test_json$Assignments, key = "Id")

        # get the total count
        tot<-test_json$TotalCount
      }
    }


    # if yes, then generate a list of urls with offset
    if(tot>100){
      # request generator, move to helpers?
      .genrequests_w_offset<- function(i) {
        url<-url |>
          req_url_query(
            Limit = 100,
            offset = 100*i
          )
        return(url)
      }
      # get the remaining items & create a list of requests
      rest<-tot-100
      loop.counter<-ceiling(rest/100)
      df<-1:loop.counter
      requests <- lapply(df, .genrequests_w_offset)

      # generate list of tempfiles used in path to write response (seems to be faster than using resp_body_json)
      tmpfiles <- tempfile(sprintf("suso_%d", df), tmpdir = tempdir(), fileext = ".json")
      # check if length of requests and tmpfiles is the same
      if(!(length(requests)==length(tmpfiles))) {
        stop("Length of requests and tmpfiles is not the same")
      }

      # execute requests in parallel !! move new pool settings to options
      responses <- httr2::req_perform_parallel(
        requests, paths = tmpfiles,
        pool = curl::new_pool(host_con = 80, total_con = 80),
        on_error = "return"
      )

      # !! what to do with failures? Mayb while loop--> WITH return there are
      # failures, but length of tmpfiles must be subset!
      if(length(requests) != length(responses)) {
        stop("Length of requests and responses is not the same")
        # # i. get the feailed requests
        # requests_fail<-requests[which(is.null(responses))]
        # # ii. get the corresponding tmpfiles
        # tmpfiles_fail<-tmpfiles[which(is.null(responses))]
        # # iii. get the successful requests
        # responses<-responses |>
        #   resps_successes()
        # # iv. get the corresponding tmpfiles
      }

      # transform response from json tempfile takes 2.51 seconds
      full_data2<-lapply(tmpfiles, .transformresponses_jsonlite, "Assignments") |>
        data.table::rbindlist()
      # remove tempfile
      unlink(tmpfiles)

      # With assignemtne class for later use
      test_json$Assignments<-data.table::rbindlist(list(full_data1, full_data2))
      test_json<-assignmentClass(test_json, args)
      # test_json<-data.table::rbindlist(list(full_data1, full_data2))
      # modify data types
      if(nrow(test_json>0)) {
        test_json[,CreatedAtUtc:=lubridate::as_datetime(CreatedAtUtc)][,UpdatedAtUtc:=lubridate::as_datetime(UpdatedAtUtc)]
        test_json[,ReceivedByTabletAtUtc:=lubridate::as_datetime(ReceivedByTabletAtUtc)][]
        return(test_json)
      } else {
        return(NULL)
      }
    } else {
      test_json$Assignments<-full_data1
      test_json<-assignmentClass(test_json, args)
      # modify data types
      if(nrow(test_json>0)) {
        test_json[,CreatedAtUtc:=lubridate::as_datetime(CreatedAtUtc)][,UpdatedAtUtc:=lubridate::as_datetime(UpdatedAtUtc)]
        test_json[,ReceivedByTabletAtUtc:=lubridate::as_datetime(ReceivedByTabletAtUtc)][]
        return(test_json)
      } else {
        # return empty assignmentClass object
        test_json$Assignments<-data.table::data.table()
        test_json<-assignmentClass(test_json, args)
        return(test_json)
      }
    }
  } else if(length(operations.type)==0 && !is.null(AssId)) {
    # B. get assignment details for a single assignment
    .checkNum(x = AssId)
    url<-req_url_path_append(url, AssId)
    tryCatch(
      { resp<-url |>
        httr2::req_perform()

      # get the response data
      if(resp_has_body(resp)){
        # get body by content type
        if(resp_content_type(resp) == "application/json") {
          test_json<-resp_body_json(resp, simplifyVector = TRUE)
          test_json<-assignmentClass(test_json, args)
          return(test_json)
        }
      }
      },
      error = function(e) {
        # return empty assignmentClass object
        test_json<-list()
        test_json$IdentifyingData<-data.table::data.table()
        test_json<-assignmentClass(test_json, args)
        return(test_json)
      }
    )
  } else {
    # Selected operations.type
    # Selected operations.type
    if(operations.type=="assignmentQuantitySettings") {
      # Change the quantity
      .checkNum(x = AssId)
      url<-req_url_path_append(url, AssId, "assignmentQuantitySettings")
      tryCatch(
        { resp<-url |>
          httr2::req_perform()

        # get the response data
        if(resp_has_body(resp)){
          # get body by content type
          if(resp_content_type(resp) == "application/json") {
            test_json<-resp_body_json(resp, simplifyVector = TRUE)
            test_json$Assignments<-data.table::data.table()
            test_json<-assignmentClass(test_json, args)
            return(test_json)
          }
        }
        },
        error = function(e) {
          # return empty assignmentClass object
          test_json<-list()
          test_json$IdentifyingData<-data.table::data.table()
          test_json<-assignmentClass(test_json, args)
          return(test_json)
        }
      )

    } else if(operations.type=="history") {
      # get the history
      # Change the quantity
      .checkNum(x = AssId)
      url<-req_url_path_append(url, AssId, "history")
      tryCatch(
        { resp<-url |>
          httr2::req_perform()

        # get the response data
        if(resp_has_body(resp)){
          # get body by content type
          if(resp_content_type(resp) == "application/json") {
            test_json<-resp_body_json(resp, simplifyVector = TRUE)
            test_json$Assignments<-data.table::data.table(test_json$History)
            test_json<-assignmentClass(test_json, args)
            return(test_json)
          }
        }
        },
        error = function(e) {
          # return empty assignmentClass object
          test_json<-list()
          test_json$IdentifyingData<-data.table::data.table()
          test_json<-assignmentClass(test_json, args)
          return(test_json)
        }
      )

    } else if(operations.type=="recordAudio") {
      # get status of audio recording
      # Change the quantity
      .checkNum(x = AssId)
      url<-req_url_path_append(url, AssId, "recordAudio")
      tryCatch(
        { resp<-url |>
          httr2::req_perform()

        # get the response data
        if(resp_has_body(resp)){
          # get body by content type
          if(resp_content_type(resp) == "application/json") {
            test_json<-resp_body_json(resp, simplifyVector = TRUE)
            test_json$Assignments<-data.table::data.table(NULL)
            test_json<-assignmentClass(test_json, args)
            return(test_json)
          }
        }
        },
        error = function(e) {
          # return empty assignmentClass object
          test_json<-list()
          test_json$IdentifyingData<-data.table::data.table()
          test_json<-assignmentClass(test_json, args)
          return(test_json)
        }
      )

    }
  }

  ##################################################### END ################################################
}









