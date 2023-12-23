#' Survey Solutions API call for Assignment Creation (httr2 version)
#'
#'
#' Creates assignment with prefilled data. Uses the httr2 package,
#' and specifically the \code{\link[httr2]{req_perform_parallel}} function
#'
#' @param df dataframe with upload data
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param workspace server workspace, if nothing provided, defaults to primary
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param QUID the questionnaire id
#' @param version the questionnaire version
#' @details Dataframe needs to be provided with columns
#' for prefilling data, as well as \emph{Quantity} and \emph{ResponsibleName}. Return value is a data.table,
#' with the prefilling data, if successful
#' @export
#'
#'

suso_createASS <- function(df = NULL,
                           server = suso_get_api_key("susoServer"),
                           apiUser = suso_get_api_key("susoUser"),
                           apiPass = suso_get_api_key("susoPass"),
                           workspace = suso_get_api_key("workspace"),
                           token = NULL,
                           QUID = NULL,
                           version = NULL) {
  # A. Check if all inputs are provided except for workspace which can be NULL
  if (is.null(df) | is.null(server) | is.null(apiUser) | is.null(apiPass) | is.null(QUID) | is.null(version)) {
    stop("Not all inputs provided")
  }

  # B. Check if is data.table if not transform to data.table
  if (!is.data.table(df)) {
    df <- data.table::as.data.table(df)
  }
  # C. Copy data.table to avoid side effects
  df <- data.table::copy(df)

  # D. Default workspace
  workspace <- .ws_default(ws = workspace)

  # E. Base URL and path
  base_url <- server
  # path <- file.path(workspace, "api", "v1", "assignments")

  # Build the URL, first for token, then for base auth
  if(!is.null(token)){
    base_url<-.baseurl_token(base_url, workspace, token, "assignments")
  } else {
    base_url<-.baseurl_baseauth(base_url, workspace, apiUser, apiPass, "assignments")
  }

  # F. Questionnaire ID
  quid <- paste0(QUID, "$", version)

  # G. Transform input df
  respname<-df$ResponsibleName
  quant<-df$Quantity
  df[, `:=`(ResponsibleName, NULL)][, `:=`(Quantity, NULL)]
  df<-as.data.frame(df)

  # H. Request
  # H.1. Function to generate requests
  genrequests<- function(i, base_url, respname, quant, quid, df) {
    js_ch <- list(
      Responsible = unbox(respname[i]),
      Quantity = unbox(quant[i]),
      QuestionnaireId = unbox(quid),
      IdentifyingData = data.frame(Variable = c(names(df)),
                                   Identity = rep("", length(names(df))),
                                   Answer = c(unlist(df[i,], use.names = FALSE)))
    )

    req <- base_url  |>
      req_body_json(js_ch)  |>
      req_method("POST")

    return(req)
  }
  # H.2. Execute request generation
  # requests <- lapply(1:nrow(df), genrequests)

  requests<-.gen_lapply_with_progress(
    1:nrow(df),
    genrequests,
    "requests", "assignment creation", workspace,
    base_url, respname, quant, quid, df
  )

  # H.3. Perform requests in parallel
  responses <- httr2::req_perform_parallel(
    requests,
    pool = curl::new_pool(host_con = 100, total_con = 100),
    on_error = "continue"
  )

  # if(FALSE) return(responses)
  # I. Response
  # I.1. Get faild responses
  failed<-responses %>% httr2::resps_failures()
  # I.2. Get successful responses
  responses<-responses %>% resps_successes()
  # I.2.1. Check if there are any successful responses and stop if not
  if(length(responses)==0){
    cli::cli_abort(c("x" = "No successful responses"), call = NULL)
  }
  # I.3. Create dataframe from successful responses
  # I.3.1. Function to transform response to dataframe
  transformresponse<-function(i, allresp) {
    # i. Convert to json
    resp<-allresp[[i]]
    respfull <-resp %>%
      resp_body_json(simplifyVector = T, flatten = TRUE)

    # ii. Get identifying data
    # transform to wide format
    resp<-data.frame(respfull$Assignment$IdentifyingData)
    reshaped_data <- as.vector(t(resp))
    new_col_names <- paste0(rep(names(resp), each = nrow(resp)), 1:nrow(resp))
    resp<-setNames(data.frame(matrix(reshaped_data, ncol = length(reshaped_data), byrow = TRUE)), new_col_names)
    # iii. Get other data
    nodf<-names(respfull$Assignment)[!grepl("IdentifyingData", names(respfull$Assignment))]
    for(x in nodf){
      resp[[x]] <- respfull$Assignment[[x]]
    }
    return(resp)
  }
  # transform w lapply
  status_list<-.gen_lapply_with_progress(
    responses,
    transformresponse,
    "responses", "assignments created", workspace,
    responses
  )
  # I.3.2. Convert to data.table and bind
  status_list<-data.table::rbindlist(status_list, fill = TRUE)

  return(status_list)
}










