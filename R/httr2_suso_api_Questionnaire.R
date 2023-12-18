#' Survey Solutions API call for questionnaire
#'
#'
#' \code{suso_getQuestDetails} implements all Questionnaire related API commands. It allows for different operation types,
#' see details bellow for further clarification.
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param workspace server workspace, if nothing provided, defaults to primary
#' @param token If Survey Solutions server token is provided \emph{usr} and \emph{pass} will be ignored
#' @param questID \emph{QuestionnaireId} for which details should be exported
#' @param version questionnaire version
#' @param operation.type if \emph{list} is specified a list of all questionnaires on the server. If
#' \emph{statuses} a vector of all questionnaire statuses. If \emph{structure} is specified, it returns a list
#' containing all questions, rosters etc. of the specific questionnaire, as well as all validations.
#' If \emph{interviews} is specified, all interviews for a specific questionnaire. See details bellow.
#'
#'
#' @details
#'
#' If list is selected, then list of questionnaires is returned.
#'
#' If statuses is selected, a list of all available questionnaire statuses is returned (deprecated).
#'
#' In case structure is chosen the return value is a list with two data.table elements:
#' \itemize{
#'   \item List element \emph{q} contains all questions, rosters etc.
#'   \item List element \emph{val} contains all validations
#' }
#' In this way it is straightforward to use the returen value for questionnaire manuals and the likes.
#'
#' In case interviews is selected, a list of all interviews for the specific questionnaire is returned.
#'
#' @export
#'
#'

suso_getQuestDetails <- function(server = suso_get_api_key("susoServer"),
                                 apiUser = suso_get_api_key("susoUser"),
                                 apiPass = suso_get_api_key("susoPass"),
                                 workspace = NULL,
                                 token = NULL,
                                 questID = NULL, version = NULL,
                                 operation.type = c("list", "statuses", "structure", "interviews")) {

  take <- 100
  # workspace default
  workspace<-.ws_default(ws = workspace)

  # check (.helpers.R)
  .check_basics(token, server, apiUser, apiPass)

  # check if questID is provided & correct
  if(!is.null(questID)){
    .checkUUIDFormat(questID)
  }

  # check if version is provided & numeric
  if(!is.null(version)){
    .checkNum(version)
  }

  # check if operation.type is provided & correct
  operation.type <- match.arg(operation.type)

  # define endpoint
  endpoint <- paste0(server, "graphql")
  print(endpoint)

  if (operation.type == "list") {
    #uses susographql package

    # first request
    quest1<-susographql::suso_gql_questionnaires(
      endpoint = endpoint,
      workspace = workspace,
      user = apiUser,
      password = apiPass,
      id = questID,
      version = version,
      take = take,
      skip = 0
    )

    # check if there are more questionnaires than 100
    tot<-quest1$questionnaires$totalCount

    # get first 100
    quest1<-data.table::data.table(quest1$questionnaires$nodes)
    # get the rest
    if(tot>take){
      # if yes, then get the rest in a loop
      rest<-tot-take
      steps<-ceiling(rest/take)
      quest2<-list()
      for(i in 1:steps){
        quest2[[i]]<-susographql::suso_gql_questionnaires(
          endpoint = endpoint,
          workspace = workspace,
          user = apiUser,
          password = apiPass,
          id = questID,
          version = version,
          take = take,
          skip = i*take
        )$questionnaires$nodes
      }

      # bind all together
      quest2<-data.table::rbindlist(quest2)
      quest1<-data.table::rbindlist(list(quest1,quest2))

    }

    # if empty return
    if(nrow(quest1)==0) {
      return(data.table::data.table(NULL))
    } else {
      # Modify names to match REST API
      data.table::setnames(quest1,
      old = c("questionnaireId", "variable", "version", "id", "title"),
      new = c("QuestionnaireId", "Variable", "Version", "QuestionnaireIdentity", "Title")
      )

      # Set date time to utc with lubridate
      # !! ATTENTION: susographql does currently not return the data, check and update
      # quest1[,LastEntryDate:=as_datetime(LastEntryDate)][]

      # return
      return(quest1)
    }
  }








  ##############################################
}
