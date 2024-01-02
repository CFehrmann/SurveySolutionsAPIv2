#' Survey Solutions API call for workspace
#'
#'
#' \code{suso_getWorkspace} allows you to get the list of workspaces, information about individual workspace names
#' as well as workspace statuses. Workspaces as well as Workspaces information can only be accessed, if credentials
#' are eligible. For more details please read \url{https://docs.mysurvey.solutions/headquarters/accounts/workspaces/}
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param workspace If workspace name is provide requests are made regarding this specific workspace
#' @param status if status is \emph{TRUE} worskpace must be not NULL and status information about the specific
#' workspace is requested
#'
#' @examples
#' \dontrun{
#' # This assumes, that suso_PwCheck(workspace = "myworkspace") was
#' # sucessful
#'
#' # shows all workspaces in the system AND the user has access to
#' suso_createWorkspace(
#'           workspace = "myworkspace",
#'           status = F)
#'
#' # shows details for specific workspace myworkspace
#' suso_createWorkspace(
#'           workspace = "myworkspace",
#'           status = F)
#' }
#'
#'
#' @export


suso_getWorkspace <- function(server = suso_get_api_key("susoServer"),
                              apiUser = suso_get_api_key("susoUser"),
                              apiPass = suso_get_api_key("susoPass"),
                              token = NULL,
                              workspace = NULL,
                              status = FALSE) {
  # check (.helpers.R)
  .check_basics(token, server, apiUser, apiPass)

  # Base URL and path
  # Build the URL, first for token, then for base auth
  if(!is.null(token)){
    url<-.baseurl_token(server, NULL, token, "workspaces")
  } else {
    url<-.baseurl_baseauth(server, NULL, apiUser, apiPass, "workspaces")
  }

  if(is.null(workspace)) {
    tryCatch(
      {resp<-req_perform(url)},
      error = function(e) .http_error_handler(e, "wsp")
    )
    # get the response data
    if(resp_has_body(resp)){
      # get body by content type
      if(resp_content_type(resp) == "application/json") {
        test_json<-resp_body_json(resp, simplifyVector = T, flatten = F)
        # Export only records
        test_json<-data.table(test_json$Workspaces)
        # Set date time to utc with lubridate
        return(test_json[])
      }
    } else {
      return(data.table(NULL))
    }
  } else if(!is.null(workspace)) {
    if(status) {
      url<- url |>
        req_url_path_append("status") |>
        req_url_path_append(workspace)
    } else {
      url<- url |>
        req_url_path_append(workspace)

    }

    tryCatch(
      {resp<-req_perform(url)},
      error = function(e) .http_error_handler(e, "wsp")
    )
    # get the response data
    if(resp_has_body(resp)){
      # get body by content type
      if(resp_content_type(resp) == "application/json") {
        test_json<-resp_body_json(resp, simplifyVector = T, flatten = F)
        # Export only records
        test_json<-data.table(t(test_json))
        # Set date time to utc with lubridate
        return(test_json[])
      }
    } else {
      return(data.table(NULL))
    }


  }

}


#' Survey Solutions API call to create workspace
#'
#'
#' @description \code{suso_createWorkspace} Allows you to create a workspace with a specific system name and display name.
#' For more details please read \url{https://docs.mysurvey.solutions/headquarters/accounts/workspaces/}. To run this command
#' you require admin credentials.
#'
#' @details Be aware, that for using this call you require the ADMIN credentials, and not the regular API user credentials.
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions ADMIN user
#' @param apiPass Survey Solutions ADMIN password
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param new_workspace The name used by the system for this workspace. Make sure you follow the rules outlined
#' under \url{https://docs.mysurvey.solutions/headquarters/accounts/workspaces/}
#' @param displayName The name visible to the users
#'
#' @examples
#' \dontrun{
#' # Use Admin Credentials!
#' suso_createWorkspace(
#'           your_workspace = "myworkspace",
#'           new_workspace = "myworkspace1",
#'           displayName = "SpecialWorkspace",
#'           apiUser = "xxxxxx",
#'           apiPass = "xxxxxx")
#' }
#'
#'
#' @export


suso_createWorkspace <- function(server = suso_get_api_key("susoServer"),
                                 apiUser = suso_get_api_key("susoUser"),
                                 apiPass = suso_get_api_key("susoPass"),
                                 token = NULL,
                                 new_workspace = NULL,
                                 displayName = NULL) {

  # check (.helpers.R)
  .check_basics(token, server, apiUser, apiPass)

  if(is.null(new_workspace)) cli::cli_abort(c("x" = "Please provide a new workspace name"))

  # Base URL and path
  # Build the URL, first for token, then for base auth
  if(!is.null(token)){
    url<-.baseurl_token(server, NULL, token, "workspaces")
  } else {
    url<-.baseurl_baseauth(server, NULL, apiUser, apiPass, "workspaces")
  }
  url<- url |>
    req_body_json(
      list(Name=new_workspace, DisplayName=displayName)
    ) |>
    req_method("POST")
  tryCatch(
    {resp<-req_perform(url)},
    error = function(e) .http_error_handler(e, "wsp")
  )
  # get the response data
  if(resp_has_body(resp)){
    # get body by content type
    if(resp_content_type(resp) == "application/json") {
      test_json<-resp_body_json(resp, simplifyVector = T, flatten = F)
      # Export only records
      test_json<-data.table(t(test_json))
      # Set date time to utc with lubridate
      return(test_json[])
    }
  } else {
    return(data.table(NULL))
  }


}



#' Survey Solutions API call to assign workspace
#'
#'
#' @description \code{suso_assignWorkspace} Allows you to assign a workspace to a specific user.
#' For more details please read \url{https://docs.mysurvey.solutions/headquarters/accounts/workspaces/}. To run this command
#' you require admin credentials.
#'
#' @details Be aware, that for using this call you require admin credentials, and not the regular API user credentials.
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions ADMIN user
#' @param apiPass Survey Solutions ADMIN password
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param assign_workspace The workspace which you want to assign to the new user
#' @param uid The User ID of the user to be assigned.
#' @param sv_id The supervisor's ID to which the interviewer should be assigned to, if it is a supervisor who is assigned, just use the same
#' as in \emph{uid}.
#'
#' @return If succesfull, returns a data.table with the details as well as the Status message "Worspaces list updated".
#'
#' @examples
#' \dontrun{
#' # Use Admin Credentials!
#' suso_assignWorkspace(
#'           your_workspace = "myworkspace",
#'           assign_workspace = "myworkspace1",
#'           uid = "xxx-xxx-xxx-xxx-xxx",
#'           sv_id = "xxx-xxx-xxx-xxx-xxx",
#'           apiUser = "xxxxxx",
#'           apiPass = "xxxxxx")
#' }
#'
#'
#' @export


suso_assignWorkspace <- function(server = suso_get_api_key("susoServer"),
                                 apiUser = suso_get_api_key("susoUser"),
                                 apiPass = suso_get_api_key("susoPass"),
                                 token = NULL,
                                 assign_workspace = NULL,
                                 uid = NULL,
                                 sv_id = NULL) {


  # check (.helpers.R)
  .check_basics(token, server, apiUser, apiPass)

  # check if questID is provided & correct
  if(is.null(uid)){
    cli::cli_abort("Please provide a User ID")
  } else {
    .checkUUIDFormat(uid)
  }

  # if(is.null(sv_id)){
  #   cli::cli_abort("Please provide a Supervisor ID")
  # } else {
  #   .checkUUIDFormat(sv_id)
  # }

  if(is.null(assign_workspace)) cli::cli_abort(c("x" = "Please provide a workspace name"))

  # Base URL and path
  # Build the URL, first for token, then for base auth
  if(!is.null(token)){
    url<-.baseurl_token(server, NULL, token, "workspaces/assign")
  } else {
    url<-.baseurl_baseauth(server, NULL, apiUser, apiPass, "workspaces/assign")
  }

  js_ch<-list(
    UserIds=I(uid),
    Workspaces=data.table(
      Workspace=assign_workspace,
      SupervisorId=sv_id
    ),
    Mode=jsonlite::unbox("Assign")
  )

  url<- url |>
    req_body_json(
      js_ch
    ) |>
    req_method("POST")

  tryCatch(
    {resp<-req_perform(url)},
    error = function(e) .http_error_handler(e, "wsp")
  )

  # get the response data
  if(resp_has_body(resp)){
    # get body by content type
    if(resp_content_type(resp) == "application/json") {
      test_json<-resp_body_json(resp, simplifyVector = T, flatten = F)
      # Export only records
      test_json<-data.table((test_json$Errors))
      # Set date time to utc with lubridate
      return(test_json[])
    }
  } else {
    test_json<-data.table::data.table(UserIds = uid, Workspace = assign_workspace, SupervisorId = sv_id, Status = "Workspaces list updated")
    return(test_json)
  }


  }














