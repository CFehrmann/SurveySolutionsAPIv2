#' HTTP helper functions
#'
#' The functions in this file are used across all scripts in different locations.
#'
#'
#' @keywords internal
#' @noRd
#'


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

.addSlashToEnd <- function(url) {
  # Check if the URL ends with a slash
  if (substring(url, nchar(url)) != "/") {
    # Add a slash to the end if it doesn't
    url <- paste0(url, "/")
  }
  return(url)
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
    req_auth_basic(apiUser, apiPass) |>
    # when 500 return, retry
    req_retry(is_transient = \(resp) resp_status(resp) %in% c(429, 500, 503), max_tries = 2)
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
    req_auth_bearer_token(token)|>
    # when 500 return, retry
    req_retry(is_transient = \(resp) resp_status(resp) %in% c(429, 500, 503), max_tries = 2)
  return(url)
}

# update url with queries
.addQuery<-function(url, ...){
  url<-url |>
    req_url_query(...)
  return(url)
}

# progress bar for cli and shiny
.progress_bar_selector <- function(loop_expr,
                                   mess = getOption("suso.progressbar.message"),
                                   totit = 100, ...){
  args<-list(...)

  # Check if the session is interactive or Shiny
  if(!shiny::isRunning() && interactive()){
    # Use cli progress bar

    cli::cli_progress_bar(mess, total = totit)
    #on.exit(cli::cli_process_done())

    # Evaluate the while loop expression
    eval(loop_expr) #, envir = rlang::caller_env())

    # End the cli progress bar
    # cli::cli_end_progress()
  } else if(shiny::isRunning() && getOption("suso.useshiny")) {
    # Use Shiny progress bar
    shiny::withProgress(
      message = mess,
      detail = "This may take a while ...", value = 0, {
        eval(loop_expr)
      }
    )
  }
}

# request generator with offset query
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

# request generator with path
.genrequests_w_path<-function(i, url, addToPath, ...) {
  sv_id<-addToPath[i]
  url<-url |>
    req_url_path_append(sv_id) |>
    req_url_path_append(...)
  # generate and execute first request with limit 100 & offset 0
  url<-.addQuery(url, Limit=100, offset=1)
  return(url)
}

# generate lapply wit cli_progress_along/seq_along, function and arguments
.gen_lapply_with_progress<-function(vec, fun, stage, type ,workspace, ..., call = rlang::caller_env()) {
  force(vec)
  # force(fun)
  if(interactive()){
    pgtext<-sprintf("Creating %s for all %s in workspace %s.", stage, type, workspace)
    cli::cli_alert_info("\n {pgtext} \n")
    requests<-lapply(cli::cli_progress_along(vec, pgtext, total = length(vec), .envir = rlang::current_env()), fun, ...)
  } else {
    requests<-lapply(seq_along(vec), fun, ...)
  }
  return(requests)
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

# transform multiple responses to data.table with path and jsonlite
.transformresponses_jsonlite_iter<-function(i, pathVector, data) {
  # i. Convert to json
  path<-pathVector[i]
  respfull <-jsonlite::fromJSON(path,simplifyVector = T, flatten = TRUE)

  # ii. Get identifying data
  # transform to wide format
  resp<-data.frame(respfull[[data]])
  return(resp)
}

# transform multiple responses to data.table without path and httr2::resp_body_json
.transformresponses<-function(i ,resp, data) {
  # i. Convert to json
  respfull <-resp[[i]] |>
    resp_body_json(simplifyVector = T, flatten = TRUE)

  # ii. Get identifying data
  # transform to wide format
  resptmp<-data.frame(respfull[[data]])
  return(resptmp)
}


# error handler
.http_error_handler <- function(error_condition, type = "ass") {
  # Use a switch or if-else to handle different types of errors
  error_type <- class(error_condition)[1]
  # error messages
  msg404<-switch(type,
                 "ass" = c("x" = "Questionnaire/Assignment/Assignee not found."),
                 "usr" = c("x" = "User not found in current workspace."),
                 "exp" = c("x" = "Export process was not found."),
                 "wsp" = c("x" = "Workspace not found.")
  )
  msg406<-switch(type,
                 "ass" = c("x" = "Assignee cannot be assigned to assignment."),
                 "usr" = c("x" = "User is not an interviewer or supervisor.")
  )

  msg400<-switch(type,
                 "ass" = c("x" = "Bad parameters provided or identifying data incorrect. See response details for more info."),
                 "usr" = c("x" = "User not found"),
                 "exp" = c("x" = "Request is malformed/Export file was not generated yet"),
                 "wsp" = c("x" = "Validation failed.")
  )

  msg401<-c("x"="Unauthorized/User not authorized.")


  msg403<-switch(type,
                 "ass" = c("x" = "Forbidden"),
                 "usr" = c("x" = "Forbidden"),
                 "exp" = c("x" = "Forbidden"),
                 "wsp" = c("x" = "API credentials are not sufficient, please use admin credentials to perform this request.")
  )

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

# error body
.http_error_body <- function(resp) {
  httr2::resp_body_json(resp)$error  #, simplifyVector = T, flatten = TRUE)
}


