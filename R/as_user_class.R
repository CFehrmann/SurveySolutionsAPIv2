#' User Classes & Methods
#'
#' Users class extends the data.table class to include relevant methods
#'
#'
#'
#' @param x list returned by api call
#' @param ... additional attributes to be added to the assignmentClass
#'
#' @export

UserClass<-function(x, ...) {
  nx<-names(x)
  if("Users" %in% nx) {
    iddt<-data.table::data.table(x$Users)
    attrloop<-names(x)[names(x)!="Users"]
  } else if("IdentifyingData" %in% nx) {
    iddt<-data.table::data.table(x$IdentifyingData)
    attrloop<-names(x)[names(x)!="IdentifyingData"]
  }
  ## define new class with data.table
  data.table::setattr(iddt, "class", base::union("UserClass", class(iddt)))
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

#' UserClass methods
#'
#' \code{getinfo} allows you to retrieve relevant additional information from the \code{UserClass},
#' object depending on the api endpoint.
#'
#' @details To retrieve all availalbe arguments use \code{arg="arglist"}
#'
#'
#' @param obj object of UserClass
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
getinfo.UserClass <- function(obj, arg) {
  if(arg=="arglist"){
    al<-names(attributes(obj))
    al<-al[!(al %in% c("names","row.names","class",".internal.selfref"))]
    return(al)
  } else {
    attr(obj, arg)
  }
}


#' Function to check if an object is of class UserClass
#'
#' @param x object to be checked
#'
#' @return TRUE if object is of class UserClass
#'
#' @export


is.UserClass <- function(x) {
  inherits(x, "UserClass")
}
