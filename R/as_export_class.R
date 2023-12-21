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
