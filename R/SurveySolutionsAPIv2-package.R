#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom data.table .BY
#' @importFrom data.table .EACHI
#' @importFrom data.table .GRP
#' @importFrom data.table .I
#' @importFrom data.table .N
#' @importFrom data.table .NGRP
#' @importFrom data.table .SD
#' @importFrom data.table :=
#' @importFrom data.table data.table as.data.table rbindlist as.data.table as.IDate as.ITime copy data.table fread is.data.table isoweek mday
#' month rbindlist setattr setkeyv setorderv shift tstrsplit wday
#' @importFrom dplyr bind_rows select_if union
#' @importFrom httr2 req_auth_basic req_auth_bearer_token req_body_json req_headers req_method req_perform req_perform_parallel req_error req_perform_iterative
#' req_url_path req_url_path_append req_url_query request resp_body_json resp_content_type resp_has_body resp_status resps_failures
#' resps_successes
#' @importFrom httr authenticate build_url GET parse_url
#' @importFrom jsonlite fromJSON unbox
#' @importFrom stats setNames
#' @importFrom lubridate as_datetime ymd ymd_hms now today
#' @importFrom stringr str_count str_detect str_extract str_replace_all str_split str_sub
#' @importFrom tidyjson enter_object gather_array spread_all jstring jnumber jlogical bind_rows spread_values
#' @importFrom zip unzip
#' @importFrom utils setTxtProgressBar txtProgressBar
## usethis namespace: end
NULL


## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "3.3.0")  {
  utils::globalVariables(c(
    ".", "i", "Quantity", "ResponsibleName", "type",
    "LinkType", "L0", "L1", "L2", "L3", "L4", "Title", "PublicKey",
    "AnswerValue", "..JSON", "Rsize", "Rtype", "RsizeKey", "Rvar",
    "RvarMerge", "parentid1", "parentid2", "parentgroup", "pargroupcount",
    "HasExportFile", "JobId",
    "resp_time",
    "action",
    "time",
    "interview__id",
    "breaks",
    "test_detail",
    "UPLOADdataCHECK",
    "QuestionnaireIdentity",
    "Variable",
    "action",
    "setNames",
    "incProgress",
    "rid",
    "duration",
    "durationNOBREAK",
    "m_resp_time_varTRIM",
    "var",
    "start",
    "startHour",
    "responsible",
    "m_resp_time_var",
    "m_diff_dev",
    "response",
    "var_resp",
    "response1",
    "response2",
    "lat",
    "long",
    "count",
    "counter",
    "handlers",
    "Timestamp",
    "LastEntryDate",
    "CreationDate",
    "document.id",
    "var_resp", "V1", "V2", "V3",
    "response",
    "responsible",
    "role",
    "dateTime","tz","wDAY","mDAY","MONTH", "WEEK",
    "UpdatedAtUtc", "InterviewDuration", "intID",
    "VariableName", "Expression", "Message", "Severity",
    "QuestionnaireId", "StartDate", "CompleteDate", "pid",
    "ExportType", "Time", "importDateUtc", "CreatedAtUtc",
    "ReceivedByTabletAtUtc"
  ))
}
