# INFO: Helper functions for SurveySolutionsAPI package
#
# Check for required http response status.
#
# @param response response from \code{httr::GET()}
# @param status valid status, i.e. 200, 300


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

# check basic inputs
.check_basics<- function(token, server, apiUser, apiPass) {
  if(is.null(token)){
    if(is.null(server) | is.null(apiUser) | is.null(apiPass)){
      stop("Please provide either a token or server, apiUser, and apiPass")
    }
  } else {
    if(is.null(server)){
      stop("Please provide a server address")
    }
  }
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
    req_auth_basic(apiUser, apiPass)
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
    req_auth_bearer_token(token)
  return(url)
}

# update url with queries
.addQuery<-function(url, ...){
  url<-url |>
    req_url_query(...)
  return(url)
}

# request generator, move to helpers?
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

# transform multiple responses to data.table with path and jsonlite
.transformresponses_jsonlite<-function(path, data) {
  # i. Convert to json
  respfull <-jsonlite::fromJSON(path,simplifyVector = T, flatten = TRUE)

  # ii. Get identifying data
  # transform to wide format
  resp<-data.frame(respfull[[data]])
  return(resp)
}

# transform multiple responses to data.table without path and httr2::resp_body_json
.transformresponses<-function(resp, data) {
  # i. Convert to json
  respfull <-resp %>%
    resp_body_json(simplifyVector = T, flatten = TRUE)

  # ii. Get identifying data
  # transform to wide format
  resp<-data.frame(respfull[[data]])
  return(resp)
}

# drop empty columns (string/numeric) in dplyr pipline
.col_selector <- function(col) {
  return(!(all(is.na(col)) | all(col == "")))
}

# check input types
.checkNum<-function(x) {
  if (!is.numeric(x)) stop("Input must be numeric")
}

## workspace default check
.ws_default<-function(ws=NULL){
  ## workspace default
  if(!is.null(ws)) {
    # margs<-suso_getWorkspace()$Name
    # workspace<-match.arg(workspace, margs)
    ws<-ws
  } else {
    ws<-"primary"
  }
  return(ws)
}

# check UUID format
.checkUUIDFormat <- function(id) {
  # Regular expression for UUID format: 8-4-4-4-12
  uuid_pattern <- "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$"

  if(grepl(uuid_pattern, id)) {
    invisible(TRUE)
  } else {
    stop("Invalid UUID format. Please check your input.")
  }
}

# check basic email format
.checkEmailFormat <- function(email) {
  # Basic email pattern: local-part@domain
  email_pattern <- "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$"

  if(grepl(email_pattern, email)) {
    invisible(TRUE)
  } else {
    stop("Invalid email format. Please check your input.")
  }
}

# get args for class
.getargsforclass<-function(workspace = NULL) {
  # except server, apiUser, apiPass, token
  #args<-as.list(sys.call(-1))[-1]
  args<-as.list(match.call(
    definition = sys.function(-1),
    call = sys.call(-1)
  ))[-1]
  # defaults<-as.list(
  #   formals(
  #     sys.function(-1)
  #   )
  # )
  # remove server, apiUser, apiPass, token, by name with grepl
  args<-args[!grepl("server|apiUser|apiPass|token", names(args))]
  args$workspace<-workspace
  return(args)
}

# error handler
.http_error_handler <- function(error_condition) {
  # Use a switch or if-else to handle different types of errors
  error_type <- class(error_condition)[1]
  switch(error_type,
         "httr2_http_404" = {
           rlang::abort("Assignment or assignee not found", parent = error_condition)
         },
         "httr2_http_406" = {
           rlang::abort("Assignee cannot be assigned to assignment", parent = error_condition)
         },
         "httr2_http_400" = {
           stop("User id cannot be parsed", call. = FALSE)
         },
         "httr2_http_401" = {
           stop("Unauthorized", call. = FALSE)
         },
         # Default case if the error type is not handled above
         stop("An unknown error occurred", call. = FALSE)
  )
}

# PARADATA ONLY ADD LATER
# Response Time
# #' @import data.table
# .calcTimeDiff<-function(DTfile, by=c("interview__id","DAY", "MONTH")){
#   DTfile<-copy(DTfile)
#   setorderv(DTfile, c("interview__id","date", "time", "counter"))
#   #DTfile<-DTfile[,.SD[.N>20], by=.(interview__id)]
#   DTfile[,resp_time:=as.integer(0)][ ,resp_time:=fifelse((data.table::shift(date, type = "lag")==date & action!="Paused"),
#                                                          as.integer(time-data.table::shift(time, type = "lag")), 0L), by=.(interview__id)][]
#   DTfile[, breaks:=fifelse(resp_time>120&!is.na(resp_time),1L,0L), by=.(interview__id)]
#   return(DTfile)
# }
# Unpack Function
# .unpack<-function(fp, allResponses, inShinyServer){
#   ####################################################
#   ##  1. Use fread from data.table (fastest, needs to be tested for reliability)
#   HEADER<-c("interview__id", "counter", "action", "responsible", "role", "time", "tz","var_resp")
#   file<-fread(file=fp,
#               header = T,
#               data.table = TRUE,
#               #col.names = HEADER,
#               fill = T,sep = "\t",
#               colClasses = list(character=c(1,3:8),
#                                 numeric=c(2)),
#               blank.lines.skip=TRUE,
#               encoding = "UTF-8")
#   names(file)<-HEADER
#   ## check lines
#   if(nrow(file)==0) return(NULL)
#   ## message if large file
#   if(nrow(file)>100000) cat("There are", nrow(file), "individual events. Processing may take a while!")
#
#   ## splitting response variable
#   #names(file)<-
#   ## Exract the VARIABLENAME
#   resps<-file[,tstrsplit(var_resp, "||", fixed=T, names = T, fill = "<NA>")][]
#   file[,var:=resps[,.(V1)]]
#   if (allResponses) {
#     ## Extract all responses if TRUE
#     resps1<-resps[,tstrsplit(V2, "(\\|)|(,)", fixed=F, names = T, fill = "<NA>")][]
#     resps2<-resps[,tstrsplit(V3, "(,)", fixed=F, names = T, fill = "<NA>")][]
#     splits <- (length(resps1))
#     file[,c(paste0("response", 1:(length(resps1)))):=resps1]
#     file[,c(paste0("rid", 1:(length(resps2)))):=resps2]
#   } else {
#     ## Extract only singel vector-->FASTER
#     file[,response:=resps[,.(V2)]]
#   }
#
#   if(inShinyServer) incProgress(amount = 0.25, message = "Data loaded")
#   if (nrow(file)==0) return(NULL)
#   ####################################################
#   ##  3. Create Factors
#   #file[,interview__id:=as.factor(interview__id)]
#   file[,action:=as.factor(action)]
#   file[,responsible:=as.factor(responsible)]
#   file[,role:=as.factor(role)]
#   #file[,var:=as.factor(var)]
#   ## only with all responses factorization of response
#   if (allResponses) {
#     for (i in 1:splits) {
#       ip<-paste0("response", (i))
#       file[,c(ip):=as.factor(get(ip))]
#     }
#   }
#
#   ##  4. DATE is adjusted for timezone!
#   file[,dateTime:=as_datetime(time)-hms(tz)][,c("tz"):=NULL]
#   file[,time:=as.ITime(dateTime)]
#   file[,date:=as.IDate(dateTime)]
#   file[,wDAY:=wday(date)]
#   file[,mDAY:=mday(date)]
#   file[,MONTH:=month(date)]
#   file[,WEEK:=isoweek(date)]
#   setkeyv(file, c("interview__id", "responsible"))
#   return(file)
#   prog<-prog+1
# }
#
# ### FOR QUESTIONNAIRE STRUCTURE #########################
# .suso_transform_fullValid <- function(input = NULL) {
#   valfinal <-bind_rows(
#     ######################################################
#     ## VALIDATIONS (Start with L0)
#     ######################################################
#     input %>% enter_object("Children") %>% gather_array("L0") %>%
#       enter_object("Children") %>% gather_array("L1") %>%
#       enter_object("ValidationConditions") %>% gather_array("val1") %>%
#       spread_all(),
#     ## third
#     input %>% enter_object("Children") %>% gather_array("L0") %>%
#       enter_object("Children") %>% gather_array("L1") %>%
#       enter_object("Children") %>% gather_array("L2") %>%
#       enter_object("ValidationConditions") %>% gather_array("val1") %>%
#       spread_all(),
#     ## fourth
#     input %>% enter_object("Children") %>% gather_array("L0") %>%
#       enter_object("Children") %>% gather_array("L1") %>%
#       enter_object("Children") %>% gather_array("L2") %>%
#       enter_object("Children") %>% gather_array("L3") %>%
#       enter_object("ValidationConditions") %>% gather_array("val1") %>%
#       spread_all(),
#     ## fifth
#     input %>% enter_object("Children") %>% gather_array("L0") %>%
#       enter_object("Children") %>% gather_array("L1") %>%
#       enter_object("Children") %>% gather_array("L2") %>%
#       enter_object("Children") %>% gather_array("L3") %>%
#       enter_object("Children") %>% gather_array("L4") %>%
#       enter_object("ValidationConditions") %>% gather_array("val1") %>%
#       spread_all(),
#     ## sixth
#     input %>% enter_object("Children") %>% gather_array("L0") %>%
#       enter_object("Children") %>% gather_array("L1") %>%
#       enter_object("Children") %>% gather_array("L2") %>%
#       enter_object("Children") %>% gather_array("L3") %>%
#       enter_object("Children") %>% gather_array("L4") %>%
#       enter_object("Children") %>% gather_array("L5") %>%
#       enter_object("ValidationConditions") %>% gather_array("val1") %>%
#       spread_all(),
#     ## seventh
#     input %>% enter_object("Children") %>% gather_array("L0") %>%
#       enter_object("Children") %>% gather_array("L1") %>%
#       enter_object("Children") %>% gather_array("L2") %>%
#       enter_object("Children") %>% gather_array("L3") %>%
#       enter_object("Children") %>% gather_array("L4") %>%
#       enter_object("Children") %>% gather_array("L5") %>%
#       enter_object("Children") %>% gather_array("L6") %>%
#       enter_object("ValidationConditions") %>% gather_array("val1") %>%
#       spread_all(),
#     ## eight
#     input %>% enter_object("Children") %>% gather_array("L0") %>%
#       enter_object("Children") %>% gather_array("L1") %>%
#       enter_object("Children") %>% gather_array("L2") %>%
#       enter_object("Children") %>% gather_array("L3") %>%
#       enter_object("Children") %>% gather_array("L4") %>%
#       enter_object("Children") %>% gather_array("L5") %>%
#       enter_object("Children") %>% gather_array("L6") %>%
#       enter_object("Children") %>% gather_array("L7") %>%
#       enter_object("ValidationConditions") %>% gather_array("val1") %>%
#       spread_all()
#     ###########################################################
#   ) %>% dplyr::select_if(col_selector)
#
#
#   valfinal<-data.table(valfinal)
#   if(nrow(valfinal)==0) {
#     return(NULL)
#   } else {
#     return(valfinal)
#   }
# }
