#' Create or update a calendar event
#'
#' Add or update a calendar event to or for an assignment or an interview.
#'
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param workspace server workspace, if nothing provided, defaults to primary
#' @param token If Survey Solutions server token is provided \emph{apiUser} and
#' \emph{apiPass} will be ignored
#' @param AssId numeric vector of assignment id(s)
#' @param intID UUID vector of interview id(s)
#' @param comment a comment string
#' @param startDate new start date, format must be: \code{2024-01-16}
#' @param startTime new start date, format must be: \code{01:41:14}
#' @param startTZ time zone of the tablet device, use \code{\link[base]{OlsonNames}}
#' @param publicKey UUID vector of calender event id(s)
#'
#' @details
#' This function creates a calendar event either for one or several assignments or one or
#' several interviews. Either one of \code{AssID} or \code{intID} must not be NULL.
#'
#' @return a data.table with the created event(s).
#'
#' @examplesIf suso_PwCheck()==200
#' # add calendar event to assignment
#' calEvass<-suso_createCALEV(AssId = 268,
#'                            startDate = "2024-01-17",
#'                            startTime = "10:40:00")
#'
#' # update the event
#' calEvup<-suso_updateCALEV(publicKey = calEvass$publicKey[1],
#'                          startDate = "2024-01-18",
#'                         startTime = "10:40:00")
#'
#' # delete the event
#' calEvDel<-suso_delCALEV(publicKey = calEvup$publicKey[1])
#'
#' @name calenderevent
NULL


#' @describeIn calenderevent add a calendar event
#' @export

suso_createCALEV <- function(server= suso_get_api_key("susoServer"),
                             apiUser=suso_get_api_key("susoUser"),
                             apiPass=suso_get_api_key("susoPass"),
                             workspace = suso_get_api_key("workspace"),
                             token = NULL,
                             intID = NULL,
                             AssId = NULL,
                             comment = NULL,
                             startDate = NULL,
                             startTime = NULL,
                             startTZ = "UTC") {

  # workspace default
  workspace<-.ws_default(ws = workspace)

  # check (.helpers.R)
  .check_basics(token, server, apiUser, apiPass)

  # check dates & TZ
  # check from_date if provided
  if(is.null(startDate) | is.null(startTime) | is.null(startTZ)){
    cli::cli_abort(c(
      "x" = "You have to provide a date, a time and a valid time zone to create a calendar event"
    ))
  } else {
    .checkDate(startDate)
    .checkTimeZone(startTZ)
    .checkTime(startTime)
  }

  # create date time
  startDateTime <- .genDateTimeCheckNow(startDate, startTime, startTZ)



  if(!is.null(AssId)){
    # assignments

    # check ass id
    .checkNum(AssId)

    result<-susographql::suso_gql_addassignmentcalendarevent(
      endpoint = paste0(server, "graphql"),
      workspace = workspace,
      user = apiUser,
      password = apiPass,
      assignmentId = AssId,
      comment = comment,
      newStart = startDateTime,
      startTimezone = startTZ
    )
    result<-data.table(t(unlist(result$addAssignmentCalendarEvent)))
  } else if(!is.null(intID)){
    # interviews

    # check interview uuid
    .checkUUIDFormat(intID[1])

    result<-susographql::suso_gql_addinterviewcalendarevent(
      endpoint = paste0(server, "graphql"),
      workspace = workspace,
      user = apiUser,
      password = apiPass,
      interviewId = intID,
      comment = comment,
      newStart = startDateTime,
      startTimezone = startTZ
    )
    result<-data.table(t(unlist(result$addInterviewCalendarEvent)))
  } else {
    cli::cli_abort(c("x" = "Please provide either assignment id(s) or interview id(s)."))
  }


  # transform dates
  if(nrow(result)>0) result<-.transform_datetime(result)
  return(result)
}


#' @describeIn calenderevent update a calendar event
#' @export

suso_updateCALEV <- function(server= suso_get_api_key("susoServer"),
                             apiUser=suso_get_api_key("susoUser"),
                             apiPass=suso_get_api_key("susoPass"),
                             workspace = suso_get_api_key("workspace"),
                             token = NULL,
                             publicKey = NULL,
                             comment = NULL,
                             startDate = NULL,
                             startTime = NULL,
                             startTZ = "UTC") {

  # workspace default
  workspace<-.ws_default(ws = workspace)

  # check (.helpers.R)
  .check_basics(token, server, apiUser, apiPass)

  # check dates & TZ
  # check from_date if provided
  if(is.null(startDate) | is.null(startTime) | is.null(startTZ)){
    cli::cli_abort(c(
      "x" = "You have to provide a date, a time and a valid time zone to create a calendar event"
    ))
  } else {
    .checkDate(startDate)
    .checkTimeZone(startTZ)
    .checkTime(startTime)
  }

  # create date time
  startDateTime <- .genDateTimeCheckNow(startDate, startTime, startTZ)



  if(!is.null(publicKey)){
    # check interview uuid
    .checkUUIDFormat(publicKey[1])

    result<-susographql::suso_gql_updatecalendarevent(
      endpoint = paste0(server, "graphql"),
      workspace = workspace,
      user = apiUser,
      password = apiPass,
      publicKey  = publicKey,
      comment = comment,
      newStart = startDateTime,
      startTimezone = startTZ
    )
    result<-data.table(t(unlist(result$updateCalendarEvent)))
  } else {
    cli::cli_abort(c("x" = "Please provide a calendar event public key."))
  }


  # transform dates
  if(nrow(result)>0) result<-.transform_datetime(result)
  return(result)
}

#' @describeIn calenderevent delete a calendar event
#' @export

suso_delCALEV <- function(server= suso_get_api_key("susoServer"),
                             apiUser=suso_get_api_key("susoUser"),
                             apiPass=suso_get_api_key("susoPass"),
                             workspace = suso_get_api_key("workspace"),
                             token = NULL,
                             publicKey = NULL) {

  # workspace default
  workspace<-.ws_default(ws = workspace)

  # check (.helpers.R)
  .check_basics(token, server, apiUser, apiPass)

  if(!is.null(publicKey)){
    # check interview uuid
    .checkUUIDFormat(publicKey[1])

    result<-susographql::suso_gql_deletecalendarevent(
      endpoint = paste0(server, "graphql"),
      workspace = workspace,
      user = apiUser,
      password = apiPass,
      publicKey  = publicKey
    )
    result<-data.table(t(unlist(result$deleteCalendarEvent)))
  } else {
    cli::cli_abort(c("x" = "Please provide a calendar event public key."))
  }


  # transform dates
  if(nrow(result)>0) result<-.transform_datetime(result)
  return(result)
}

