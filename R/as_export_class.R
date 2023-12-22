#' Export Classes & Methods
#'
#' Export class extends the data.table class to include relevant methods
#'
#'
#'
#' @param x list returned by api call
#' @param ... additional attributes to be added to the ExportClass
#' @param varLabels data.table of variable labels as returned by
#' \code{suso_getQuestDetails(operation.type = "structure")}
#'
#' @export

exportClass<-function(x, varLabels, ...) {

  if(!is.null(x) && is.data.table(x)) {
    # check inputs
    if(!is.null(varLabels) && is.data.table(varLabels)) {
      # remove any NA in variable names
      labs<-varLabels[["VariableName"]]
      labs<-labs[!is.na(labs)]
      # Check for variable names in wide format
      base_vars <- unique(sub("__.*", "", names(x)))
      base_vars <- base_vars[base_vars %in% labs]
      if (length(base_vars) > 0) {
        # wide format
        # print(base_vars); print("****") ;print(labs); print("****") ;print(names(x))
        # first the wide
        for(attr in base_vars) {
          # get the pattern
          pat<-sprintf("^%s__+", attr)
          # get the title
          labTitle<-varLabels[VariableName == attr, QuestionText]
          # get the vars
          vars<-names(x)[stringr::str_which(names(x), pattern = pat)]
          # apply the labels
          if(length(vars)>0) {
            for(attr_1 in vars) {
              data.table::setattr(x, tolower(attr_1), labTitle)
            }
          }
        }

        # Second rest
        labs<-labs[!(labs %in% base_vars)]
        if(length(labs)>0) {
          for(attr in labs) {
            labTitle<-varLabels[VariableName == attr, QuestionText]
            data.table::setattr(x, tolower(attr), labTitle)
          }
        }


      } else {
        # long format/or main level
        labs<-labs[labs %in% names(x)]
        for(attr in labs) {
          labTitle<-varLabels[VariableName == attr, QuestionText]
          data.table::setattr(x, tolower(attr), labTitle)
        }

      }

      # add additonal NA values to attributes
      data.table::setattr(x, "na_numeric", -999999999L)

      ## define new class with data.table
      data.table::setattr(x, "class", base::union("exportClass", class(x)))

      invisible(x)

    } else {
      cli::cli_abort(c("x" = "Data can not be coerced to exportClass! Pleas check inputs."))
    }

    # add translation for all labels to attributes


  } else if(is.list(x)) {
    nx<-names(x)
    if("main" %in% nx) {
      iddt<-data.table::data.table(x$Users)
      attrloop<-names(x)[names(x)!="Users"]
    } else if("R1" %in% nx) {
      iddt<-data.table::data.table(x$IdentifyingData)
      attrloop<-names(x)[names(x)!="IdentifyingData"]
    }
    ## define new class with data.table
    ## add other information as attributes by loop over remaining names
    for(attr in attrloop) {
      data.table::setattr(iddt, tolower(attr), x[[attr]])
    }
    # ... must be non empty named list-->Contains all arguments passed to the function EXPLICITLY
    if(!is.null(...) && is.list(...) && length(...) > 0) {
      for(attr in names(...)) {
        data.table::setattr(iddt, tolower(attr), ...[[attr]])
      }
    }

    invisible(iddt)
  }
}

#' exportClass methods
#'
#' \code{getinfo} allows you to retrieve relevant additional information from the \code{ExportClass},
#' object depending on the api endpoint.
#'
#' @details To retrieve all availalbe arguments use \code{arg="arglist"}
#'
#'
#' @param obj object of exportClass
#' @param arg name of attribute, if \code{arg="arglist"} then it returns all available arguments
#'
#' @return the specific attribute
#'
#'
#' @examples
#' \dontrun{
#'
#' # retrieve the uid of the person responsible after retrieving details for specific User
#' getinfo(asslist, "responsibleid")
#'
#' # see all available attribute names
#' getinfo(asslist, "arglist")
#'
#' }
#'
#' @export
#'
getinfo <- function(obj, arg) {
  UseMethod("getinfo")
}

#' @export
getinfo.exportClass <- function(obj, arg) {
  if(arg=="arglist"){
    al<-names(attributes(obj))
    al<-al[!(al %in% c("names","row.names","class",".internal.selfref"))]
    return(al)
  } else {
    attr(obj, arg)
  }
}

#' Function to check if an object is of class exportClass
#'
#' @param x object to be checked
#'
#' @return TRUE if object is of class exportClass
#'
#' @export

is.exportClass<-function(x) {
  inherits(x, "exportClass")
}

######################################################################################
# ALL METHODS BELLOW REQUIRE IMPROVEMENTS IN TERMS OF STYLE AND CHECKS!!!
######################################################################################

#' S3 method to create a summary table of numeric variables for an exportClass object
#'
#' This function generates a summary table of numeric variables in a exportClass
#' object. The table includes statistics such as mean, standard deviation, maximum,
#' minimum, and count of NA values.
#'
#' @param x A exportClass object.
#' @param useDT Logical, if TRUE (default) the function returns a DataTable (DT) object (HTML table), otherwise
#' a data.table object is returned.
#' @param ... Additional arguments (not used).
#'
#' @return A DataTable (DT) object (HTML table) displaying the summary statistics.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load your package
#' export <- suso_export(questID = questlist$QuestionnaireId[7],
#'                       version = questlist$Version[7],
#'                       combineFiles = T,
#'                       reloadTimeDiff = 20)
#'
#' # Create a summary table
#' summary_data_table <- summaryTable(export)
#'
#' # Display the summary table
#' summary_data_table
#' }
#'
summaryTable <- function(x, useDT = TRUE, ...) {
  UseMethod("summaryTable")
}


#' @export
summaryTable.exportClass <- function(x, useDT = TRUE, ...) {

  # check if x is of class exportClass
  if (!is.exportClass(x)) {
    cli::cli_abort(c("x" = "x must be of class exportClass."))
  }

  # Get the numeric columns
  numeric_cols <- names(x)[sapply(x, is.numeric)]
  # If no nummeric columns are found return NULL and cli warning
  if (length(numeric_cols) == 0) {
    cli::cli_alert_warning("No numeric columns found in the data.")
    return(NULL)
  }

  # Get the additional NA values
  additional_na_values <- attr(x, "na_numeric")


  # Initialize an empty data.table for the summary
  summary_data <- data.table(
    Variable = character(0),
    Label = character(0),
    Mean = numeric(0),
    SD = numeric(0),
    Max = numeric(0),
    Min = numeric(0),
    NA_Count = numeric(0)
  )
  # set decimal digits to 3 with withr or use ...$digits if provided
  dig <- if(!is.null(list(...)$digits)) list(...)$digits else 3
    # Calculate summary statistics for each numeric column
    for (col in numeric_cols) {
      # replace additional NA values with NA
      x[[col]][x[[col]] == additional_na_values] <- NA
      summary_data <- data.table::rbindlist(
        list(summary_data,
             data.table(
               Variable = col,
               Label = attr(x, col),
               Mean = round(mean(x[[col]], na.rm = TRUE), digits = dig),
               SD = round(sd(x[[col]], na.rm = TRUE), digits = dig),
               Max = round(max(x[[col]], na.rm = TRUE), digits = dig),
               Min = round(min(x[[col]], na.rm = TRUE), digits = dig),
               NA_Count = sum(is.na(x[[col]]))
             )), fill = TRUE
      )
    }

    # If useDT is FALSE return the data.table
    if (!useDT) {
      return(summary_data)
    } else {
      # cli abort if DT is not found
      if (!(rlang::is_installed("DT"))) {
        cli::cli_abort(c("x" ="DT package is required to use this function and not found on your system."))
      }
      # Convert the summary data.table to a DT DataTable
      DT<-DT::datatable(summary_data, options = list(dom = 't'))
      return(DT)
    }
}



#' S3 method to create a box plot of numeric variables for an exportClass object
#'
#' This function generates a box plot for the numeric variables in a exportClass
#' object.
#'
#' @param x A summaryTable object.
#' @param useGGplot2 Logical, if TRUE (default) the function returns a ggplot2 object, otherwise
#' a base R boxplot is returned.
#' @param ... Additional arguments (not used).
#' @return A box plot visualizing the numeric variables.
#' @export
#'
#' @examples
#' \dontrun{
#' # Load your package
#' export <- suso_export(questID = questlist$QuestionnaireId[7],
#'                       version = questlist$Version[7],
#'                       combineFiles = T,
#'                       reloadTimeDiff = 20)
#'
#' # Create a summary table
#' boxplot_summary <- boxplot(export)
#'
#' # Display the summary table
#' boxplot_summary
#' }
#'
#' @export
boxplot_summary <- function(x, useGGplot2 = FALSE, ...) {
  UseMethod("boxplot_summary")
}


#' @export
boxplot_summary.exportClass <- function(x, useGGplot2 = FALSE,...) {
  # check if x is of class exportClass
  if (!is.exportClass(x)) {
    cli::cli_abort(c("x" = "x must be of class exportClass."))
  }

  # Get the numeric columns
  numeric_cols <- names(x)[sapply(x, is.numeric)]
  # If no nummeric columns are found return NULL and cli warning
  if (length(numeric_cols) == 0) {
    cli::cli_alert_warning("No numeric columns found in the data.")
    return(NULL)
  }

  # Get the additional NA values
  additional_na_values <- attr(x, "na_numeric")

  # Set the additonal NA values to NA
  for (col in numeric_cols) {
    # replace additional NA values with NA
    x[[col]][x[[col]] == additional_na_values] <- NA
  }

  # If useGGplot2 is TRUE return a ggplot2 boxplot
  if (useGGplot2) {
    # cli abort if ggplot2 is not found
    if (!(rlang::is_installed("DT"))) {
      cli::cli_abort(c("x" ="ggplot2 package is required to use this function and not found on your system."))
    }
    # Create a data.frame for ggplot2
    df <- data.table::melt(x, measure.vars = numeric_cols)

    # Create a ggplot2 box plot
    p <- ggplot2::ggplot(df, ggplot2::aes(x = variable, y = value)) +
      ggplot2::geom_boxplot() +
      ggplot2::labs(title = "Box Plot for Numeric Variables", x = "Variable", y = "Value") +
      ggplot2::theme_minimal()
    return(p)
  } else {
    # Initialize an empty list to store box plots
    boxplot_list <- list()

    # Generate box plots for each numeric column
    for (col in numeric_cols) {
      boxplot_list[[col]] <- boxplot(x[[col]])
    }

    # # Combine box plots into a single plot
    # boxplot_combined <- do.call("grid.arrange", boxplot_list)

    return(boxplot_list)
  }
}

