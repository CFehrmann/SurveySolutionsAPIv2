#' Configuration options used for the SurveySolutionsAPIv2 package
#'
#' @description
#' Below are the options and environment variables, that are used by the \pkg{SurveySolutionsAPIv2} package.
#'
#' @section Available options:
#' Bellow is a list of options, with their default values.
#'
#' \itemize{
#'   \item \option{suso.maxpar.req} specifies the maximum number of parallel requests, default is 100
#'   \item \option{suso.maxpar.con} specifies the maximum number of parallel connections,
#'   and must always be equal or greater than suso.maxpar.req, default is 100.
#' }
#'
#' @name suso.options
#'
NULL


#' Convenience Function to modify the maximum number of parallel requests
#'
#' @description
#' This function allows to modify the maximum number of parallel requests \option{suso.maxpar.req}
#' in a single call. It also checks the maximum number of parallel connections, and if lower than
#' the number of requests \code{max_req} then also sets the \option{suso.maxpar.con} option to
#' \code{max_req}.
#'
#' @param max_req set the number of parallel requests for all functions which
#'
#' @export
#'
suso_set_maxpar_req <- function(max_req = 100) {
  options(suso.maxpar.req = as.integer(max_req))
  if(interactive()) cli::cli_alert_info("Maximum parallel requests set to {max_req}.")
  max_con<-getOption("suso.maxpar.con")
  if (is.null(max_con) || max_con < max_req) {
    options(suso.maxpar.con = max_req)
    if(interactive()) cli::cli_alert_warning("Also adjusted maximum parallel connections to {max_req}.")
  }
  invisible(NULL)
}
