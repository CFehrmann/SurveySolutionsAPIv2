#' Assignment Classes & Methods
#'
#' Assignments class extends the data.table class to include relevant methods
#'
#'
#'
#' @param x list returned by api call
#' @param ... additional attributes to be added to the assignmentClass
#'
#' @return assignmentClass object
#'
#' @details The assignmentClass is a data.table extended with additional attributes and methods, depending
#' on the api endpoint. The assignmentClass is used as input for the \code{getinfo} function.
#'
#'
#'
#' @export

assignmentClass<-function(x, ...) {
  nx<-names(x)
  if("Assignments" %in% nx) {
    iddt<-data.table::data.table(x$Assignments)
    attrloop<-names(x)[names(x)!="Assignments"]
  } else if("IdentifyingData" %in% nx) {
    iddt<-data.table::data.table(x$IdentifyingData)
    attrloop<-names(x)[names(x)!="IdentifyingData"]
  }
  ## define new class with data.table
  data.table::setattr(iddt, "class", base::union("assignmentClass", class(iddt)))
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

#' assignmentClass methods
#'
#' \code{getinfo} allows you to retrieve relevant additional information from the \code{assignmentClass},
#' object depending on the api endpoint.
#'
#' @details To retrieve all availalbe arguments use \code{arg="arglist"}
#'
#'
#' @param obj object of assignmentClass
#' @param arg name of attribute, if \code{arg="arglist"} then it returns all available arguments
#'
#' @return the specific attribute
#'
#'
#' @examples
#' \dontrun{
#'
#' # retrieve the uid of the person responsible after retrieving details for specific assignment
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
getinfo.assignmentClass <- function(obj, arg) {
  if(arg=="arglist"){
    al<-names(attributes(obj))
    al<-al[!(al %in% c("names","row.names","class",".internal.selfref"))]
    return(al)
  } else {
    attr(obj, arg)
  }
}
