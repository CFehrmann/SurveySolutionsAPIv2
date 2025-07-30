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

  # Identify rows with questions
  rows_with_question <- dt[!(type%in%c("Group", "StaticText", "Variable")), ]

  return(rows_with_question)
}


## SF (SIMPLE FEATURES) TRANSFORMATION FOR SPATIAL DATA

# polygons
.export_convert_to_sfpoly <- function(coord_strings, int__id, ..., caller) {
  process_coords <- function(coord_str) {
    # Split the string into lat-long pairs
    pairs <- strsplit(coord_str, ";")[[1]]
    # Split each pair into lat and long and convert to numeric
    coords <- matrix(as.numeric(unlist(strsplit(pairs, ","))), ncol = 2, byrow = TRUE)
    # Close the polygon if not closed
    if (!identical(coords[1, ], coords[nrow(coords), ])) {
      coords <- rbind(coords, coords[1, ])
    }
    # Return the coordinates
    return(coords)
  }

  polygons <- lapply(seq_along(coord_strings), function(i) {
    str<-coord_strings[i]
    id<-int__id[i]
    rosids<-list(...)
    names(rosids)<-sapply(names(rosids),
                          function(p) eval(as.symbol(p), envir = caller))

    if (str == "##N/A##") {
      return(NULL)
    } else {
      coords <- process_coords(str)
      # Create an sf polygon
      polygon <- sf::st_sfc(sf::st_polygon(list(coords)), crs = 4326)
      polygon <- sf::st_as_sf(polygon)
      polygon$interview__id<-id
      # check if .. is empty
      if(length(rosids)>0) {
        # namevec<-c("RvarMerge", "parentid1", "parentid2")
        for(k in names(rosids)) {
          rid<-rosids[[k]][i]
          polygon[[k]]<-rid
        }
      }
      return(polygon)
    }
  })

  # Remove NULL elements
  polygons <- polygons[!sapply(polygons, is.null)]

  # Create an sf object
  sf_polygons <- do.call(rbind, polygons)


  # Calculate area for each polygon
  sf_polygons$area <- sf::st_area(sf_polygons)

  return(sf_polygons)
}

# points (for gps, single point, multi point)
.export_convert_to_sfpoint <- function(coord_strings, int__id, ..., caller) {
  process_coords <- function(coord_str) {
    # Split the string into lat-long pairs
    pairs <- strsplit(coord_str, ";")[[1]]
    # Split each pair into lat and long and convert to numeric
    coords <- matrix(as.numeric(unlist(strsplit(pairs, ","))), ncol = 2, byrow = TRUE)
    # Return the coordinates
    return(coords)
  }

  points_list <- lapply(seq_along(coord_strings), function(i) {
    str<-coord_strings[i]
    id<-int__id[i]
    rosids<-list(...)
    names(rosids)<-sapply(names(rosids),
                          function(p) eval(as.symbol(p), envir = caller))
    if (str == "##N/A##") {
      return(NULL)
    } else {
      coords <- process_coords(str)
      # Create sf points
      point <- sf::st_sfc(sf::st_multipoint(coords), crs = 4326)
      point <- sf::st_as_sf(point)
      point$interview__id<-id
      # check if .. is empty
      if(length(rosids)>0) {
        # namevec<-c("RvarMerge", "parentid1", "parentid2")
        for(k in names(rosids)) {
          rid<-rosids[[k]][i]
          point[[k]]<-rid
        }
      }
      return(point)
    }
  })

  # Remove NULL elements
  points_list <- points_list[!sapply(points_list, is.null)]

  # Flatten the list of points
  sf_points <- do.call(rbind, points_list)

  return(sf_points)
}

# lines
.export_convert_to_sfline <- function(coord_strings, int__id, ..., caller) {
  process_coords <- function(coord_str) {
    # Split the string into lat-long pairs
    pairs <- strsplit(coord_str, ";")[[1]]
    # Split each pair into lat and long and convert to numeric
    coords <- matrix(as.numeric(unlist(strsplit(pairs, ","))), ncol = 2, byrow = TRUE)
    # Return the coordinates
    return(coords)
  }

  points_list <- lapply(seq_along(coord_strings), function(i) {
    str<-coord_strings[i]
    id<-int__id[i]
    rosids<-list(...)
    names(rosids)<-sapply(names(rosids),
                          function(p) eval(as.symbol(p), envir = caller))
    if (str == "##N/A##") {
      return(NULL)
    } else {
      coords <- process_coords(str)
      # Create sf points
      point <- sf::st_sfc(sf::st_linestring(coords), crs = 4326)
      point <- sf::st_as_sf(point)
      point$interview__id<-id
      # check if .. is empty
      if(length(rosids)>0) {
        # namevec<-c("RvarMerge", "parentid1", "parentid2")
        for(k in names(rosids)) {
          rid<-rosids[[k]][i]
          point[[k]]<-rid
        }
      }
      return(point)
    }
  })

  # Remove NULL elements
  points_list <- points_list[!sapply(points_list, is.null)]

  # Flatten the list of points
  sf_points <- do.call(rbind, points_list)

  return(sf_points)
}


# Get existing exports
.get_existing_exports <- function(server, workspace, token, apiUser, apiPass, qid, extype, workStatus) {
  url <- if (!is.null(token)) {
    .baseurl_token(server, workspace, token, "export", version = "v2")
  } else {
    .baseurl_baseauth(server, workspace, apiUser, apiPass, "export", version = "v2")
  }
  url_w_query <- .addQuery(url, exportType = extype, interviewStatus = workStatus, questionnaireIdentity = qid, hasFile = TRUE)
  resp <- tryCatch(httr2::req_perform(url_w_query), error = function(e) .http_error_handler(e, "exp"))
  if (httr2::resp_has_body(resp) && httr2::resp_content_type(resp) == "application/json") {
    return(data.table::as.data.table(httr2::resp_body_json(resp, simplifyVector = TRUE)))
  } else {
    return(data.table::data.table(character(0)))
  }
}

# Function to clear string for eval command
.remove_na_patterns <- function(input_string) {
  # Remove '& is.na(NA)'
  modified_string <- gsub("& is\\.na\\(NA\\)", "", input_string)
  
  # Remove '& !is.na(NA)'
  modified_string <- gsub("& !is\\.na\\(NA\\)", "", modified_string)
  
  # Trim leading and trailing whitespace
  modified_string <- trimws(modified_string)
  
  return(modified_string)
}


