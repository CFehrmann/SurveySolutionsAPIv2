#' General helper functions
#'
#' The functions in this file are used across all scripts in different locations.
#'
#'
#' @keywords internal
#' @noRd
#'

# .cli_alert_interactive<-function(msg, type = "info"){
#   if(interactive()) {
#     if(type=="info") cli::cli_alert_info(msg)
#     if(type=="warning") cli::cli_alert_warning(msg)
#     if(type=="error") cli::cli_alert_error(msg)
#   }
# }

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
    if(interactive()) cli::cli_alert_info("No workspace provided. Using primary workspace.")
    ws<-"primary"
  }
  return(ws)
}

# check UUID format
.checkUUIDFormat <- function(id) {
  # Regular expression for UUID format: 8-4-4-4-12, also allow for version without -
  if(!is.null(id)) {
    hasHyphon<-stringr::str_detect(id, pattern = "-")
  } else {
    hasHyphon<-FALSE
  }
  if(hasHyphon) {
    uuid_pattern <- "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$"
  } else {
    uuid_pattern <- "^[0-9a-fA-F]{8}[0-9a-fA-F]{4}[0-9a-fA-F]{4}[0-9a-fA-F]{4}[0-9a-fA-F]{12}$"
  }
  if(!is.null(id) && grepl(uuid_pattern, id)) {
    invisible(TRUE)
  } else {
    cli::cli_abort(c("x" = "Invalid UUID format, or not provided. Please check your input."))
  }
}

# check suso key format
.checkIntKeyformat <- function(id) {
  # Regular expression for interview key format: 86-85-49-18
  key_pattern <- "^[0-9]{2}-[0-9]{2}-[0-9]{2}-[0-9]{2}$"

  if(grepl(key_pattern, id)) {
    invisible(TRUE)
  } else {
    stop("Invalid INterview Key format. Please check your input.")
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

# check date with lubridate -->ymd
.checkDate<- function(date_string) {
  # parse the date using ymd
  parsed_date <- ymd(date_string, quiet = TRUE)

  # Check if the parsed date is NA (invalid) or if the format is not as expected
  if (is.na(parsed_date) || format(parsed_date, "%Y-%m-%d") != date_string) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

# check date range
.checkDateRange <- function(from_date, to_date) {
  # Parse the dates
  parsed_from_date <- ymd(from_date, quiet = TRUE)
  parsed_to_date <- ymd(to_date, quiet = TRUE)

  # Check if either date is invalid
  if (is.na(parsed_from_date) || format(parsed_from_date, "%Y-%m-%d") != from_date ||
      is.na(parsed_to_date) || format(parsed_to_date, "%Y-%m-%d") != to_date) {
    stop("Invalid date format. Please check your input.")
  }

  # Check if from_date is not before to_date
  if(parsed_from_date > parsed_to_date) {
    stop("from_date must be before to_date")
  }
}

# check date time range
.checkDateTimeRange <- function(from_datetime, to_datetime) {
  # Parse the datetimes
  parsed_from_datetime <- ymd_hms(from_datetime, quiet = TRUE)
  parsed_to_datetime <- ymd_hms(to_datetime, quiet = TRUE)

  # Check if either datetime is invalid
  if (is.na(parsed_from_datetime) || format(parsed_from_datetime, "%Y-%m-%d %H:%M:%S") != from_datetime ||
      is.na(parsed_to_datetime) || format(parsed_to_datetime, "%Y-%m-%d %H:%M:%S") != to_datetime) {
    stop("Invalid date time format. Please check your input.")
  }

  # Check if from_datetime is not before to_datetime
  if(parsed_from_datetime >= parsed_to_datetime) {
    stop("from_date must be before to_date")
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
