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
  options(suso.para.maxcore = data.table::getDTthreads()-2)
}
