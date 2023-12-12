#' Survey Solutions API credentials
#'
#'
#' (this function has been inspired by the googleway package
#' \url{https://github.com/SymbolixAU/googleway},
#' an excellent package to use google geo-spatial API)
#' Retrieves the list of Survey Solutions credentials that have been set.
#'
#' @export
#' @import readr
#' @import stringr
suso_keys <- function() getOption("SurveySolutionsAPI")


print.suso_api <- function(x, ...) {

  for (i in 1:length(x)) {

    cat("Survey Solutions API credentials\n")

    for (j in 1:length(x[[i]])){
      cat(" - ", names(x[[i]])[j], ": ")
      key <- x[[i]][[j]]
      cat(ifelse(is.na(key), "", key), "\n")
    }
  }
}

#' Set Credentials
#'
#' Sets API credentials so it's available for all API calls. See details
#'
#' @param suso_server Survey Solutions server address
#' @param suso_user Survey Solutions API user
#' @param suso_password Survey Solutions API password
#' @param suso_token If Survey Solutions server token is provided \emph{suso_user} and \emph{suso_password} will be ignored
#' @param workspace server workspace, if nothing provided, defaults to primary
#'
#' @details
#' Use \code{suso_set_key} to make API keys available for all the \code{suso_}
#' functions, so you don't need to specify the credentials parameter within those
#' functions. The server address can be provided with or without https:\\ suffix,
#' nevertheless if it is missing, then the suffix will be added.
#'
#' In case \emph{suso_token} is provided, only token authentication will be attempted. For details on token authentication
#' in Survey Solutions please see \url{https://docs.mysurvey.solutions/headquarters/accounts/token-based-authentication/}.
#'
#'
#'
#' @export
#'
suso_set_key <- function(
  suso_server = "",
  suso_user = "",
  suso_password = "",
  workspace = "",
  suso_token = ""
) {
  # workspace default
  workspace<-.ws_default(ws = workspace)
  # get options
  options <- getOption("SurveySolutionsAPI")
  # sanitize string to ssl (http?)
  suso_server<-ifelse(stringr::str_count(suso_server, "https://")==1,
                      suso_server, paste0("https://", suso_server))
  # add to object
  options[['suso']][['susoServer']] <- suso_server
  options[['suso']][['susoUser']] <- suso_user
  options[['suso']][['susoPass']] <- suso_password
  options[['suso']][['workspace']] <- workspace
  class(options) <- "suso_api"
  options(SurveySolutionsAPI = options)
  invisible(NULL)

}


#' Clear Credentials
#'
#' Clears all the API credentials
#'
#' @export
suso_clear_keys <- function() {

  options <- list(
    suso = list(
      susoServer = NA_character_,
      susoUser = NA_character_,
      susoPass = NA_character_,
      workspace = NA_character_
    )
  )
  attr(options, "class") <- "suso_api"
  options(SurveySolutionsAPI = options)

}

#' Get credentials
#'
#' Get API credentials
#'
#' Get credentials, used as input in API calls
#'
#' @param api one of susoServer, susoUser, susoPass, or workspace
#'
#' @import data.table
#'
#' @export
#'
suso_get_api_key <- function(api = c("susoServer", "susoUser", "susoPass", "workspace")) {

  api<-match.arg(api)
  ## Return value for selected API component
  api <- getOption("SurveySolutionsAPI")[['suso']][[api]]
  if(is.na(api)) return(suso_get_default_key(api))

  return(api)
}

#' Checks if credentials are present
#'
#' Helper function
#'
#' @param api one of susoServer, susoUser, susoPass, or workspace
#'
#' @export
suso_get_default_key <- function(api = c("susoServer", "susoUser", "susoPass", "workspace")) {
  api<-match.arg(api)
  key <- getOption("SurveySolutionsAPI")[['suso']][[api]]
  if(is.na(key)) stop("No API credentials available Use either suso_set_key() to set a key, or provide it as a function argument directly", call. = F)
  return(key)
}


#' Utility function to check if credentials are correct
#'
#' This function returns a 200 status if the correct credentials have been provided. If credentials are correct but
#' user is not eligible to access the workspace, then a 403 error is returned.
#'
#' @param server Survey Solutions Server
#' @param apiUser API user
#' @param apiPass API password
#' @param workspace server workspace, if nothing provided, defaults to primary
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#'
#'
#' @export
suso_PwCheck<-function(server=suso_get_api_key("susoServer"),
                       apiUser=suso_get_api_key("susoUser"),
                       apiPass=suso_get_api_key("susoPass"),
                       workspace = suso_get_api_key("workspace"),
                       token = NULL) {
  ## workspace default
  workspace<-.ws_default(ws = workspace)
  # check (.helpers.R)
  .check_basics(token, server, apiUser, apiPass)
  # Build the URL, first for token, then for base auth
  if(!is.null(token)){
    url<-.baseurl_token(server, workspace, token, "supervisors")
  } else {
    url<-.baseurl_baseauth(server, workspace, apiUser, apiPass, "supervisors")
  }

  # add query
  url<-.addQuery(url, limit=200)

  ## Request
  test_detail<-tryCatch(
    {req_perform(url) |> resp_status()},
    error=function(e) {a<-400; return(a)}
  )
  # if interactive, print message & query
  if(interactive()){
    if(test_detail==200){
      cat("Credentials are correct & and the following successful request was performed:\n\n")
      url |> httr2::req_dry_run()

    } else {
      cat("Credentials are correct & and the following failed request was performed:\n\n")
      url |> httr2::req_dry_run()
    }
  }

  return(test_detail)
}




















