# INFO: Helper functions for SurveySolutionsAPI package
#
# Check for required http response status.
#
# @param response response from \code{httr::GET()}
# @param status valid status, i.e. 200, 300


###############################################################################
## SEPARATE HELPER FUNCTIONS IN INDIVIDUAL UTILS FILE, I.E EXPORT, QUEST ETC.
###############################################################################
# remove full NA columns in suso data
.export_remove_na_columns <- function(dt) {

  # integers to remove
  intcols<-which(sapply(dt, function(x) is.integer(x) & all(x == -999999999L)))
  if(length(intcols)>0) dt[, (intcols) := NULL]
  # characters to remove
  charcols<-which(sapply(dt, function(x) !lubridate::is.POSIXt(x)  && is.character(x) && all(x == "##N/A##")))
  if(length(charcols)>0) dt[, (charcols) := NULL]

  return(dt)
}



# PARADATA ONLY ADD LATER
# Response Time

#' @noRd
#' @keywords internal
#'

.calcTimeDiff<-function(DTfile, by=c("interview__id","DAY", "MONTH")){
  DTfile<-copy(DTfile)
  setorderv(DTfile, c("interview__id","date", "time", "counter"))
  #DTfile<-DTfile[,.SD[.N>20], by=.(interview__id)]
  DTfile[,resp_time:=as.numeric(0)][ ,resp_time:=fifelse((data.table::shift(date, type = "lag")==date & action!="Paused"),
                                                         as.numeric(time-data.table::shift(time, type = "lag")), 0), by=.(interview__id)][]
  DTfile[, breaks:=fifelse(resp_time>120&!is.na(resp_time),1L,0L), by=.(interview__id)]
  return(DTfile)
}
# Unpack Function

#' @noRd
#' @keywords internal
#'
.unpack<-function(fp, allResponses, inShinyServer){
  ####################################################
  ##  1. Use fread from data.table (fastest, needs to be tested for reliability)
  HEADER<-c("interview__id", "counter", "action", "responsible", "role", "time", "tz","var_resp")
  file<-fread(file=fp,
              header = T,
              data.table = TRUE,
              #col.names = HEADER,
              fill = T,sep = "\t",
              colClasses = list(character=c(1,3:8),
                                numeric=c(2)),
              blank.lines.skip=TRUE,
              encoding = "UTF-8")
  names(file)<-HEADER
  ## check lines
  if(nrow(file)==0) return(NULL)
  ## message if large file
  if(nrow(file)>100000) cat("There are", nrow(file), "individual events. Processing may take a while!")

  ## splitting response variable
  #names(file)<-
  ## Exract the VARIABLENAME
  resps<-file[,tstrsplit(var_resp, "||", fixed=T, names = T, fill = "<NA>")][]
  file[,var:=resps[,.(V1)]]
  if (allResponses) {
    ## Extract all responses if TRUE
    resps1<-resps[,tstrsplit(V2, "(\\|)|(,)", fixed=F, names = T, fill = "<NA>")][]
    resps2<-resps[,tstrsplit(V3, "(,)", fixed=F, names = T, fill = "<NA>")][]
    splits <- (length(resps1))
    file[,c(paste0("response", 1:(length(resps1)))):=resps1]
    file[,c(paste0("rid", 1:(length(resps2)))):=resps2]
  } else {
    ## Extract only singel vector-->FASTER
    file[,response:=resps[,.(V2)]]
  }

  if(inShinyServer) incProgress(amount = 0.25, message = "Data loaded")
  if (nrow(file)==0) return(NULL)
  ####################################################
  ##  3. Create Factors
  #file[,interview__id:=as.factor(interview__id)]
  file[,action:=as.factor(action)]
  file[,responsible:=as.factor(responsible)]
  file[,role:=as.factor(role)]
  #file[,var:=as.factor(var)]
  ## only with all responses factorization of response
  if (allResponses) {
    for (i in 1:splits) {
      ip<-paste0("response", (i))
      file[,c(ip):=as.factor(get(ip))]
    }
  }

  ##  4. DATE is adjusted for timezone!
  file[,dateTime:=as_datetime(time)-lubridate::hms(tz)][,c("tz"):=NULL]
  file[,time:=as.ITime(dateTime)]
  file[,date:=as.IDate(dateTime)]
  file[,wDAY:=wday(date)]
  file[,mDAY:=mday(date)]
  file[,MONTH:=month(date)]
  file[,WEEK:=isoweek(date)][]
  setkeyv(file, c("interview__id", "responsible"))
  return(file)
  # prog<-prog+1
}
#
