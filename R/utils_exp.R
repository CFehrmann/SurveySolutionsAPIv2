#' Export helper functions
#'
#' The functions in this file are used across all scripts in different locations.
#'
#'
#' @keywords internal
#' @noRd
#'

# exclude survey solutions system variables from vector of variable names
.excludeSysVars<-function(x) {
  x<-x[!(x %in% c("sssys_irnd", "interview__status", "assignment__id", "has__errors"))]
  # exclude system variables ending in __id
  x<-x[!grepl("__id$", x)]
  return(x)
}

# list export tab files for processing
.list_export_tab_files <- function(directory) {
  # List all files in the directory
  all_files <- list.files(directory, pattern = "\\.tab$", full.names = TRUE)

  # List of files to exclude
  exclude_files <- c("assignment__actions.tab",
                     "interview__actions.tab",
                     "interview__comments.tab",
                     "interview__diagnostics.tab",
                     "interview__errors.tab")

  # Filter out the excluded files
  valid_files <- all_files[!basename(all_files) %in% exclude_files]

  return(valid_files)
}

# get questionnaire name from export__readme.txt
.get_first_tab_filename <- function(file_path) {
  # Read lines
  lines <- readLines(file_path)

  # regex for .tab
  tab_files <- grep("\\.tab$", lines, value = TRUE)

  # Check if any .tab file is found
  if (length(tab_files) > 0) {
    # Extract the first .tab filename without the extension
    first_tab_file <- sub("\\.tab$", "", tab_files[1])
    return(first_tab_file)
  } else {
    return(NULL)
  }
}

# get all questions from questionnaire
.questionnaire_allquestions <- function(dt) {
  # Check if 'type' column exists
  if (!("type" %in% names(dt))) {
    stop("The 'type' column does not exist in the data table.")
  }

  # Identify rows where 'type' contains 'Question'
  rows_with_question <- dt[grepl("Question", type), ]

  return(rows_with_question)
}
