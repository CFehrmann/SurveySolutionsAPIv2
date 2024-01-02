#' Upload map to server
#'
#' Allows the user to upload a zip file of background maps and boundary files
#'
#' @details Resources for upload must meet the requirements specified under:
#' \url{https://docs.mysurvey.solutions/headquarters/mapsmanage/map-formats/},
#' Attention: this uses the GraphQL API, not the REST API.
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param workspace server workspace, if nothing provided, defaults to primary
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param path_to_zip either path to zip file or directory of files which will automatically be zipped as required
#'
#' @return Returns a data.table, with information about the uploaded maps
#'
#' @examples
#' \dontrun{
#' # either upload the content of a folder
#' suso_mapupload(workspace = "myworkspace",
#'               path_to_zip = "./mapfiles/")
#'
#' # or a zip file with the files included
#' suso_mapupload(workspace = "myworkspace",
#'               path_to_zip = "../mapfiles/maps.zip")
#' }
#'
#'
#'
#'
#' @export


suso_mapupload <- function(server= suso_get_api_key("susoServer"),
                           apiUser=suso_get_api_key("susoUser"),
                           apiPass=suso_get_api_key("susoPass"),
                           workspace = suso_get_api_key("workspace"),
                           token = NULL,
                           path_to_zip = NULL) {

  # workspace default
  workspace<-.ws_default(ws = workspace)

  # check (.helpers.R)
  .check_basics(token, server, apiUser, apiPass)

  if(is.null(path_to_zip)) cli::cli_abort(c("x" = "Please provide a a file or directory path."))

  fi<-file.info(path_to_zip)
  if(fi$isdir) {
    # directory to zip in temporary
    fl<-list.files(path_to_zip,
                   pattern = "(.shp)|(.shx)|(.dbf)|(.prj)|(.tpk)|(.tif)|(.tiff)|(.mmpk)",
                   full.names = T)
    tmpzip<-tempfile(fileext = ".zip")
    zip::zip(tmpzip, files = fl, mode = "cherry-pick")
    path_to_zip<-tmpzip

  } else if(!fi$isdir && tools::file_ext(path_to_zip)=="zip") {
    #path_to_zip

  } else {
    cli::cli_abort(c("x" = "File is neither a directory nor a .zip file"))
  }

  result<-susographql::suso_gql_uploadmap(
    endpoint = paste0(server, "graphql"),
    workspace = workspace,
    user = apiUser,
    password = apiPass,
    path_to_zip = path_to_zip
  )
  result<-data.table::data.table(result$uploadMap)
  if(nrow(result)>0) result[,importDateUtc:=lubridate::as_datetime(importDateUtc)][]
  return(result)
}


#' Receive maps currently uploaded to the server
#'
#' Allows the user to retrieve filtered or unfiltered map data.
#'
#' @details Attention: this uses the GraphQL API, not the REST API.
#'
#' @param server GraphQL endpoint of your server
#' @param workspace Server Workspace, if NULL uses default
#' @param apiUser your API username
#' @param apiPass API password
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param fileName name of the map on the server
#' @param importDateUtc Import date
#' @param size Size of the map
#' @param users Users to whom the maps are assigned
#' @param sortby_filename sort maps by file name, either ASC for ascending or DESC for descending
#' @param sortby_importeddateutc sort maps by import date in utc, either ASC for ascending or DESC for descending
#' @param sortby_size sort by map size, either ASC for ascending or DESC for descending
#' @param take take the specified integer numeber of maps
#' @param skip skip the first integer number of maps
#'
#' @return Returns a data.table, with all the maps and additonal information. If multiple users are assigned to a map,
#' the table is expanded, such that there is one user per map.
#'
#' @examples
#' \dontrun{
#' suso_mapinfo(workspace = "myworkspace")
#' }
#'
#' @export

suso_mapinfo <- function(server= suso_get_api_key("susoServer"),
                         apiUser=suso_get_api_key("susoUser"),
                         apiPass=suso_get_api_key("susoPass"),
                         workspace = suso_get_api_key("workspace"),
                         token = NULL,
                         fileName = NULL,
                         importDateUtc = NULL,
                         size = NULL,
                         users = NULL,
                         sortby_filename = NULL,
                         sortby_importeddateutc = NULL,
                         sortby_size = NULL,
                         take = NULL,
                         skip = NULL) {

  # workspace default
  workspace<-.ws_default(ws = workspace)

  # check (.helpers.R)
  .check_basics(token, server, apiUser, apiPass)

  result<-susographql::suso_gql_maps(
    endpoint = paste0(server, "graphql"),
    workspace = workspace,
    user = apiUser,
    password = apiPass
  )

  tc<-result$maps$totalCount
  fc<-result$maps$filteredCount

  if(fc>0){
    rc<-nrow(result$maps$nodes)
    resultdt<-result$maps$nodes
    # extract user list with tidyr::unnest
    resultdt<-tidyr::unnest(resultdt, "users", keep_empty = T)

    # create data.table
    resultdt<-data.table::data.table(resultdt)
    if(nrow(resultdt)>0) {
      #resultdt[, users:=gsub("c\\(|\\)", "",sapply(users, paste, collapse=""))]
      resultdt[,importDateUtc:=lubridate::as_datetime(importDateUtc)][]
    }
  } else if(fc==0) {
    resultdt<-data.table(NULL)
  }

  return(resultdt)

}

#' Assigns a map to a user
#'
#' Allows the user to assign a map to an interviewer to be used in CAPI data collection.
#'
#' @param server GraphQL endpoint of your server
#' @param workspace Server Workspace, if NULL uses default
#' @param apiUser your API username
#' @param apiPass API password
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param fileName the name of the map file on the server
#' @param userName the name of the interviewer to whom the map will be assigned to
#'
#'
#' @examples
#' \dontrun{
#' suso_mapassign(workspace = "myworkspace",
#'               fileName = "Lat9264Lon625_ALL.tif",
#'               userName = "INT0004")
#' }
#'
#' @export




suso_mapassign <- function(server= suso_get_api_key("susoServer"),
                           apiUser=suso_get_api_key("susoUser"),
                           apiPass=suso_get_api_key("susoPass"),
                           workspace = suso_get_api_key("workspace"),
                           token = NULL,
                           fileName = NULL,
                           userName = NULL) {

  # workspace default
  workspace<-.ws_default(ws = workspace)

  # check (.helpers.R)
  .check_basics(token, server, apiUser, apiPass)

  result<-susographql::suso_gql_addusertomap(
    endpoint = paste0(server, "graphql"),
    workspace = workspace,
    user = apiUser,
    password = apiPass,
    fileName = fileName,
    userName = userName
  )

  result<-result$addUserToMap
  result<-data.table::data.table(
    fileName=result$fileName,
    user=userName,
    shapeType=result$shapeType,
    importDateUtc=lubridate::as_datetime(result$importDateUtc)
  )
  return(result)

}






