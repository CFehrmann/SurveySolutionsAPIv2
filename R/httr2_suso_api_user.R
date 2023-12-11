#' Survey Solutions API call for list of supervisors
#'
#' Gets list of supervisors
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param workspace If workspace name is provide requests are made regarding this specific workspace, if
#' no workspace is provided defaults to primary workspace.
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#'
#' @examples
#' \dontrun{
#' suso_getSV(
#'           workspace = "myworkspace"
#'           )
#' # or without any workspace and receive the default workspace
#' suso_getSV()
#'
#' }
#'
#'
#' @export

suso_getSV <- function(server = suso_get_api_key("susoServer"),
                       apiUser = suso_get_api_key("susoUser"),
                       apiPass = suso_get_api_key("susoPass"),
                       workspace = NULL,
                       token = NULL) {

  # Default workspace
  workspace <- .ws_default(ws = workspace)

  # check (.helpers.R)
  .check_basics(token, server, apiUser, apiPass)

  # Base URL and path
  # Build the URL, first for token, then for base auth
  if(!is.null(token)){
    url<-.baseurl_token(server, workspace, token, "supervisors")
  } else {
    url<-.baseurl_baseauth(server, workspace, apiUser, apiPass, "supervisors")
  }

  # generate and execute first request with limit 100 & offset 0
  url<-.addQuery(url, Limit=100, offset=1)

  resp<-req_perform(url)

  # get the response data
  if(resp_has_body(resp)){
    # get body by content type
    if(resp_content_type(resp) == "application/json") {
      test_json<-resp_body_json(resp, simplifyVector = TRUE)
      full_data1<-data.table::data.table(test_json$Users)

      # get the total count
      tot<-test_json$TotalCount
    }
  }

  # if yes, then generate a list of urls with offset
  if(tot>100){

    # show total number of supervisors
    if(interactive()){
      message(paste0("Total number of supervisors: ", tot, "\n\n"))
    }

    # get the remaining items & create a list of requests
    rest<-tot-100
    loop.counter<-ceiling(rest/100)
    df<-1:loop.counter
    requests <- lapply(df, .genrequests_w_offset, url, lim = 100, off = 1, multi = F)

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
    full_data2<-lapply(tmpfiles, .transformresponses_jsonlite, "Users") |>
      data.table::rbindlist()
    # remove tempfile
    unlink(tmpfiles)

    print(nrow(full_data2))

    # With User class for later use
    #test_json$Users<-data.table::rbindlist(list(full_data1, full_data2))
    #test_json<-UserClass(test_json)
    test_json<-data.table::rbindlist(list(full_data1, full_data2))
    # modify data types
    if(nrow(test_json>0)) {
      test_json[, CreationDate := lubridate::as_datetime(CreationDate)][]
      return(test_json)
    } else {
      return(NULL)
    }
  } else {
    test_json<-full_data1
    # modify data types
    if(nrow(test_json>0)) {
      test_json[, CreationDate := lubridate::as_datetime(CreationDate)][]
      return(test_json)
    } else {
      return(NULL)
    }
  }


  return(test_json)
}



#' Survey Solutions API call for list of interviewers
#'
#'
#' Get list of all interviewers by supervisor id, or a list of all interviewers in the workspace.
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param sv_id supervisor id, if NULL all interviewers in the workspace are returned
#' @param workspace If workspace name is provide requests are made regarding this specific workspace, if
#' no workspace is provided defaults to primary workspace.
#' @param token If Survey Solutions server token is provided \emph{usr} and \emph{pass} will be ignored
#'
#'
#' @examples
#' \dontrun{
#' suso_getINT(
#'           workspace = "myworkspace",
#'           sv_id = "xxxx-xxxx-xxxx-xxx"
#'           )
#' }
#'
#' @export
suso_getINT <- function(server=suso_get_api_key("susoServer"),
                        apiUser = suso_get_api_key("susoUser"),
                        apiPass = suso_get_api_key("susoPass"),
                        workspace = NULL,
                        token = NULL,
                        sv_id = NULL) {
  ## default workspace
  workspace<-.ws_default(ws = workspace)

  # check (.helpers.R)
  .check_basics(token, server, apiUser, apiPass)

  # Base URL and path
  # Build the URL, first for token, then for base auth
  if(!is.null(token)){
    url<-.baseurl_token(server, workspace, token, "supervisors")
  } else {
    url<-.baseurl_baseauth(server, workspace, apiUser, apiPass, "supervisors")
  }

  # internal function to get interviewers for a supervisor
  .svget<-function(sv_id) {
    url<-url |>
      req_url_path_append(sv_id) |>
      req_url_path_append("interviewers")

    # generate and execute first request with limit 100 & offset 0
    url<-.addQuery(url, Limit=100, offset=1)

    resp<-req_perform(url)

    # get the response data
    if(resp_has_body(resp)){
      # get body by content type
      if(resp_content_type(resp) == "application/json") {
        test_json<-resp_body_json(resp, simplifyVector = TRUE)
        full_data1<-data.table::data.table(test_json$Users)

        # get the total count
        tot<-test_json$TotalCount
      }
    }

    # if yes, then generate a list of urls with offset
    if(tot>100){

      # show total number of supervisors
      if(interactive()){
        message(paste0("Total number of supervisors: ", tot, "\n\n"))
      }

      # get the remaining items & create a list of requests
      rest<-tot-100
      loop.counter<-ceiling(rest/100)
      df<-1:loop.counter
      requests <- lapply(df, .genrequests_w_offset, url, lim = 100, off = 1, multi = F)

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
      full_data2<-lapply(tmpfiles, .transformresponses_jsonlite, "Users") |>
        data.table::rbindlist()
      # remove tempfile
      unlink(tmpfiles)

      print(nrow(full_data2))

      # With User class for later use
      #test_json$Users<-data.table::rbindlist(list(full_data1, full_data2))
      #test_json<-UserClass(test_json)
      test_json<-data.table::rbindlist(list(full_data1, full_data2))
      # modify data types
      if(nrow(test_json>0)) {
        test_json[, CreationDate := lubridate::as_datetime(CreationDate)][]
        return(test_json)
      } else {
        return(NULL)
      }
    } else {
      test_json<-full_data1
      # modify data types
      if(nrow(test_json>0)) {
        test_json[, CreationDate := lubridate::as_datetime(CreationDate)][]
        return(test_json)
      } else {
        return(NULL)
      }
    }
  }

  # if sv_id is provided, then get interviewers for this supervisor
  if(!is.null(sv_id)){
   # check if sv_id is valid
    .checkUUIDFormat(sv_id)
    # get interviewers for this supervisor
   test_json<-.svget(sv_id)

  } else {

    # if no sv_id is provided, then get all interviewers in the workspace
    # i. get all sv ids with suso_getSV
    allsv<-suso_getSV(server, apiUser, apiPass, workspace = workspace)
    # ii. get interviewers for each sv_id with lapply
    test_json<-lapply(allsv$UserId, .svget) |>
      data.table::rbindlist()

  }

  return(test_json)

}



#' Survey Solutions API call for info on interviewers
#'
#'
#' Get details of a single interviewer or retrieve the action log
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param int_id interviewer id
#' @param workspace If workspace name is provide requests are made regarding this specific workspace, if
#' no workspace is provided defaults to primary workspace.
#' @param token If Survey Solutions server token is provided \emph{usr} and \emph{pass} will be ignored
#' @param log If TRUE, then the audit log is returned
#' @param startDate Start data for the period of interest, date must be provided as character in the format:
#' YYYY-MM-DD, and will start at 00:00.00 server time of the specified day
#' @param endDate End date for the period of interest, date must be provided ascharacter in the format:
#' YYYY-MM-DD, and will end at 23:59:59 server time of the specified day. If not end date is provided, the current
#' date will be used
#'
#' @examples
#' \dontrun{
#' suso_getINT_info(
#'           workspace = "myworkspace",
#'           int_id = "xxxx-xxxx-xxxx-xxx"
#'           )
#' }
#'
#' @export
suso_getINT_info<-function(server=suso_get_api_key("susoServer"), apiUser = suso_get_api_key("susoUser"), apiPass = suso_get_api_key("susoPass"),
                           int_id = NULL, workspace = NULL, token = NULL, log = FALSE, startDate = NULL, endDate = NULL) {
  ## default workspace
  workspace<-.ws_default(ws = workspace)

  # check (.helpers.R)
  .check_basics(token, server, apiUser, apiPass)

  # Base URL and path
  # Build the URL, first for token, then for base auth
  if(!is.null(token)){
    url<-.baseurl_token(server, workspace, token, "interviewers")
  } else {
    url<-.baseurl_baseauth(server, workspace, apiUser, apiPass, "interviewers")
  }

  # check int_id is uuid
  .checkUUIDFormat(int_id)

  # append int_id to url
  url<-url |>
    req_url_path_append(int_id)

  # get interviewer details
  if(!log) {

    resp<-req_perform(url)

    # get the response data
    if(resp_has_body(resp)){
      # get body by content type
      if(resp_content_type(resp) == "application/json") {
        test_json<-resp_body_json(resp, simplifyVector = TRUE)
        # Export only records
        test_json<-data.table(t(unlist(test_json)))
        # Set date time to utc with lubridate
        if(nrow(test_json)>0) test_json[,CreationDate:=as_datetime(CreationDate)][]
        return(test_json)
      }
    }


  } else {
    # get interviewer logs

    # check start/end & format
    if(!is.null(startDate)) startDate<-paste(startDate, "00:00:00", sep = "T")
    if(!is.null(endDate)) endDate<-paste(endDate, "23:59:59", sep = "T")

    if(!is.null(startDate) && is.null(endDate)) endDate<-lubridate::format_ISO8601(Sys.time())

    # append actions-log to url
    url<-url |>
      req_url_path_append("actions-log")

    # add query parameters
    url<-.addQuery(url, start=startDate, end=endDate)

    resp<-req_perform(url)

    # get the response data
    if(resp_has_body(resp)){
      # get body by content type
      if(resp_content_type(resp) == "application/json") {
        test_json<-resp_body_json(resp, simplifyVector = TRUE)
        # Convert to data.table
        test_json<-data.table::data.table(test_json)
        # Convert Date
        if(nrow(test_json)>0){
          # # Set date time to utc with lubridate
          test_json[,Time:=lubridate::as_datetime(Time)][]
        }
        return(test_json)
      }
    }





  }


}


#' Survey Solutions API call for info on any user
#'
#'
#' Get any user's info, by either providing the user id, email, or the user name
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param user_id user id
#' @param user_name user name
#' @param user_email user email
#' @param workspace If workspace name is provide requests are made regarding this specific workspace, if
#' no workspace is provided defaults to primary workspace.
#' @param token If Survey Solutions server token is provided \emph{usr} and \emph{pass} will be ignored
#'
#' @examples
#' \dontrun{
#' suso_getUSR(
#'           workspace = "myworkspace",
#'           uid = "xxxx-xxxx-xxxx-xxx"
#'           )
#' }
#'
#' @export
#'
suso_getUSR<-function(server=suso_get_api_key("susoServer"), apiUser = suso_get_api_key("susoUser"), apiPass = suso_get_api_key("susoPass"),
                      user_id = NULL, user_name = NULL, user_email = NULL, workspace = NULL, token = NULL) {
  ## default workspace
  workspace<-.ws_default(ws = workspace)

  # check that at least one of uid, user_name, or user_email is provided
  if(is.null(user_id) && is.null(user_name) && is.null(user_email)) stop("Please provide at least one of uid, user_name, or user_email")


  # check (.helpers.R)
  .check_basics(token, server, apiUser, apiPass)

  # Base URL and path
  # Build the URL, first for token, then for base auth
  if(!is.null(token)){
    url<-.baseurl_token(server, workspace, token, "users")
  } else {
    url<-.baseurl_baseauth(server, workspace, apiUser, apiPass, "users")
  }

  # check uid is uuid if provided
  if(!is.null(user_id)) {
    .checkUUIDFormat(user_id)
    url<-url |>
      req_url_path_append(user_id)
    } else if(!is.null(user_email)) {
      .checkEmailFormat(user_email)
      url<-url |>
        req_url_path_append(user_email)
      } else if(!is.null(user_name)) {
        url<-url |>
          req_url_path_append(user_name)

      } else {
        stop("Please provide at least one of uid, user_name, or user_email")
      }

   # perfor request with tryCatch
   tryCatch({
     resp<-req_perform(url)

     # get the response data
     if(resp_has_body(resp)){
       # get body by content type
       if(resp_content_type(resp) == "application/json") {
         test_json<-resp_body_json(resp, simplifyVector = TRUE)
         # Export only records
         test_json<-data.table(t(unlist(test_json)))
         # Set date time to utc with lubridate
         if(nrow(test_json)>0) test_json[,CreationDate:=as_datetime(CreationDate)][]
         return(test_json)
       }
     }
   }, error = function(e) {
     stop("User not found")
   })
}



#' Survey Solutions API call (un-) archive user
#'
#'
#' (Un-)Archive user
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param user_id user id
#' @param archive if TRUE user will be archived or statys archived, if FALSE user will be unarchived or stays unarchived
#' @param workspace If workspace name is provide requests are made regarding this specific workspace, if
#' no workspace is provided defaults to primary workspace.
#' @param token If Survey Solutions server token is provided \emph{usr} and \emph{pass} will be ignored
#'
#'
#' @examples
#' \dontrun{
#' # you can archive a user by archive=T
#' suso_archUSR(
#'           workspace = "myworkspace",
#'           uid = "xxxx-xxxx-xxxx-xxx",
#'           archive = TRUE
#'           )
#' # and unarchive a user by archive=F
#' suso_archUSR(
#'           workspace = "myworkspace",
#'           uid = "xxxx-xxxx-xxxx-xxx",
#'           archive = FALSE
#'           )
#' }
#'
#' @export
#'
suso_archUSR<-function(server=suso_get_api_key("susoServer"), apiUser = suso_get_api_key("susoUser"), apiPass = suso_get_api_key("susoPass"),
                       user_id = NULL, archive = F, workspace = NULL, token = NULL) {
  ## default workspace
  workspace<-.ws_default(ws = workspace)

  # check (.helpers.R)
  .check_basics(token, server, apiUser, apiPass)

  # Base URL and path
  # Build the URL, first for token, then for base auth
  if(!is.null(token)){
    url<-.baseurl_token(server, workspace, token, "users")
  } else {
    url<-.baseurl_baseauth(server, workspace, apiUser, apiPass, "users")
  }

  # check uid is uuid if provided
  if(!is.null(user_id)) {
    .checkUUIDFormat(user_id)
    url<-url |>
      req_url_path_append(user_id)

    # check archive is logical
    if(!is.logical(archive)) stop("Please provide logical value for archive")

    arch<-ifelse(archive, "archive", "unarchive")
    url<-url |>
      req_url_path_append(arch)

    # modify request type
    url<-url |>
      httr2::req_method("PATCH")

    # perfor request with tryCatch
    tryCatch({
      resp<-req_perform(url)

      # get the response data
      test_json<-data.table(user=c(user_id), ArchDate = as_datetime(Sys.time()), IsArchived = archive)
      return(test_json)

    },
    httr2_http_404 = function(e) {
      stop("User not found")
    },
    httr2_http_400 = function(e) {
      stop("User id cannot be parsed")
    },
    httr2_http_401 = function(e) {
      stop("Unauthorized")
    })

  } else {
    stop("Please provide user_id")
  }


}


















