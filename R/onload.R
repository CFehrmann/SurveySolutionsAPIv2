.onLoad <- function(...){
  ## taken from googleway package
  ##  creates options list

  if(is.null(getOption("SurveySolutionsAPI"))) {

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
  
  # add http option
  if(is.null(getOption("suso.url.http"))) {
    options(suso.url.http = FALSE)
  }
  # parallel requests
  if(is.null(getOption("suso.maxpar.req"))) {
    options(suso.maxpar.req = 100)
  }
  if(is.null(getOption("suso.maxpar.con"))) {
    # if max requests not NULL, then set con to same
    options(suso.maxpar.con = min(100, getOption("suso.maxpar.req")))
  }

  #mili seconds
  options(digits.secs = 3)

  # paradata breaks (in seconds--> used for durationNOBREAK)
  options(suso.para.break = 120)
  # tz defaults to system tz
  options(suso.para.tz = Sys.timezone())

  # number of cores for parallel processing
  options(suso.para.maxcore = (.detectCores()-2))

  # type of parallel i.e. multisession, sequential multicore
  options(suso.para.plan = "multisession")

  # option to use shiny features (i.e. showNotification)
  options(suso.useshiny = TRUE)
  options(suso.progressbar.message = "Creating new export file")
  options(suso.pwcheck.message_succ = "Credentials are correct and a successful
                              request\n was performed in workspace %s")
  options(suso.pwcheck.message_fail = "Credentials are incorrect and a failed
                              request\n was performed in workspace %s")

  # cli progress bar delay-->longer delay as api response is 0 when completed immediately.
  # oldoptclipgb<-getOption("cli.progress_show_after")
  # on.exit(options(cli.progress_show_after = oldoptclipgb))
  options(cli.progress_show_after = 4)

}
