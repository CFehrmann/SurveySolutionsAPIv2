#' Survey Solutions API call for Summary Tables
#'
#' ATTENTION: CURRENTLY 500 ERROR, PROBLEM ON SURVEY SOLUTIONS API SIDE, OTHERWISE: Returns summary tables for individual questions.
#' If no responses had been provided, an empty table will be returned
#'
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param workspace server workspace, if nothing provided, defaults to primary
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param questID only assignments for \emph{QuestionnaireId} are returned, requires \code{version} being not NULL
#' @param version version of the questionnaire, only required with \code{questID}
#' @param qQuest provide \emph{QuestionnaireId} and \emph{version} to receive all questions and responses for a specific questionnaire
#' @param byTeam should the table contain reports by team
#' @export
#'

suso_get_stats <- function(server = suso_get_api_key("susoServer"),
                           apiUser = suso_get_api_key("susoUser"),
                           apiPass = suso_get_api_key("susoPass"),
                           workspace = suso_get_api_key("workspace"),
                           token = NULL,
                           questID = "", version = "",
                           qQuest = "", byTeam = TRUE) {

  ## workspace default
  workspace<-.ws_default(ws = workspace)
  # Csv structure is exported
  qExp <- "Csv"

  # check (.helpers.R)
  .check_basics(token, server, apiUser, apiPass)

  # Base URL and path
  # Build the URL, first for token, then for base auth
  if(!is.null(token)){
    url<-.baseurl_token(server, workspace, token, "statistics")
  } else {
    url<-.baseurl_baseauth(server, workspace, apiUser, apiPass, "statistics")
  }

  # check if questID is provided & correct
  if(!is.null(questID)){
    .checkUUIDFormat(questID)
  }

  # check if version is provided & numeric
  if(!is.null(version)){
    .checkNum(version)
  }

  ## Clear questionnaire ID
  questID <- str_remove_all(questID, "-")

  # # append int_id to url
  # url<-url |>
  #   req_url_query(
  #     query.questionnaireId = questID,
  #     query.version = version,
  #     query.question = qQuest,
  #     query.exportType = qExp,
  #     query.pivot = "false",
  #     query.expandTeams = ifelse(byTeam,"true","false")
  #   )

  # Query syntax from 01022024
  # https://mcw-demo.mysurvey.solutions/primary/api/v1/statistics?QuestionnaireId=b10435cc-1fe0-4888-9901-28182d25e746&
  # Version=7&Question=527a88d1-bbd9-3e62-a4ea-47f5650c26f3&exportType=Csv&Pivot=false&ExpandTeams=true

  url<-url |>
    req_url_query(
      QuestionnaireId = questID,
      Version = version,
      Question = qQuest,
      exportType = qExp,
      Pivot = "false",
      ExpandTeams = ifelse(byTeam,"true","false")
    )
  # req_dry_run(url)

  tryCatch(
    {resp<-req_perform(url)},
    error = function(e) .http_error_handler(e, "ass")
  )
  return(resp)
  # get the response data
  if(resp_has_body(resp)){
    # get body by content type
    if(resp_content_type(resp) == "application/json") {
      test_json<-resp_body_json(resp, simplifyVector = T, flatten = F)
      # Export only records
      test_json<-data.table(test_json$Answers)
      # Set date time to utc with lubridate
      return(test_json)
    }
  } else {
    return(data.table(NULL))
  }



}


#' Survey Solutions API call for questions and responses from single questionnaire
#'
#' Returns all questions for single questionnaire (ONLY if they contain responses), if you require all questions
#' from any questionnaire on the server, then you have to use \code{suso_getQuestDetails(...,
#' operation.type = "structure")}
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param workspace server workspace, if nothing provided, defaults to primary
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param questID only assignments for \emph{QuestionnaireId} are returned, requires \code{version} being not NULL
#' @param version version of the questionnaire, only required with \code{questID}
#'
#'
#' @export
#'

suso_getQuestionsQuestionnaire <- function(server = suso_get_api_key("susoServer"),
                                           apiUser = suso_get_api_key("susoUser"),
                                           apiPass = suso_get_api_key("susoPass"),
                                           workspace = suso_get_api_key("workspace"),
                                           token = NULL,
                                           questID = NULL, version = NULL) {
  ## workspace default
  workspace<-.ws_default(ws = workspace)
  # check (.helpers.R)
  .check_basics(token, server, apiUser, apiPass)

  # Base URL and path
  # Build the URL, first for token, then for base auth
  if(!is.null(token)){
    url<-.baseurl_token(server, workspace, token, "statistics/questions")
  } else {
    url<-.baseurl_baseauth(server, workspace, apiUser, apiPass, "statistics/questions")
  }

  # check if questID is provided & correct
  if(!is.null(questID)){
    .checkUUIDFormat(questID)
  }

  # check if version is provided & numeric
  if(!is.null(version)){
    .checkNum(version)
  }

  ## Clear questionnaire ID
  questID <- str_remove_all(questID, "-")


  url<-url |>
    req_url_query(
      QuestionnaireId = questID,
      Version = version
    )
  # req_dry_run(url)

  tryCatch(
    {resp<-req_perform(url)},
    error = function(e) .http_error_handler(e, "ass")
  )
  # get the response data
  if(resp_has_body(resp)){
    # get body by content type
    if(resp_content_type(resp) == "application/json") {
      test_json<-resp_body_json(resp, simplifyVector = T, flatten = F)
      # Export only records
      test_json<-data.table(test_json)
      # Set date time to utc with lubridate
      return(test_json)
    }
  } else {
    return(data.table(NULL))
  }
}
