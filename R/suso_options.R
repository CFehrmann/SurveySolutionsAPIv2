#' Configuration options used for the SurveySolutionsAPIv2 package
#'
#' @description
#' Below are the options and environment variables, that are used by the \pkg{SurveySolutionsAPIv2} package.
#'
#' @section Available options:
#' Bellow is a list of options, with their default values.
#'
#' \itemize{
#'   \item \option{suso.maxpar.req} specifies the maximum number of parallel requests, default is 100.
#'   \item \option{suso.maxpar.con} specifies the maximum number of parallel connections,
#'   and must always be equal or greater than suso.maxpar.req, default is 100.
#'   \item \option{suso.para.break} specifies the maximum response time to be considered as a break in seconds, default 120.
#'   \item \option{suso.para.tz} specifies the local timezone for the processing of time values, default is \code{Sys.timezone()}.
#'   \item \option{suso.para.maxcore} specifies the number of cores used for parallel processing, default is \code{data.table::getDTthreads()-2}.
#'   \item \option{suso.para.plan} specifies plane used for parallel processing, default is \code{multisession}. For details please see
#'   \code{future::\link[future]{plan}}
#'   \item \option{suso.useshiny} should R shiny elements be used if running in shiny app, default is TRUE. Currently implemented for:
#'   \itemize{
#'      \item \code{\link{suso_PwCheck}}, providing feedback for credentials check.
#'      \item \code{\link{suso_export}}, providing different progress bars for interactive (cli) use and in shiny app use (\code{shiny::\link[shiny]{withProgress}},
#'      to also customize the progress bar message, i.e. in a different language, the \option{suso.progressbar.message} can
#'      be modified.
#'      \item TBD
#'   }
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
