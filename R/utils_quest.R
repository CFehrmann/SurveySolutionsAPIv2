#' Helper functions to transform list of questionnair structure to single data.table with all variables
#'
#' Uses tidyjson package
#'
#' \code{suso_transform_fullMeta} transforms the list containing the structure (\emph{operation.type = structure})
#' is transformed into a single data.table
#' with all variable names, types etc.. This also works with json strings manually exported from the server.
#'
#'
#' @param input returned by \code{suso_getQuestDetails} structure operation
#'
#'
#' @return data.table with all variables in the questionnaire
#'
#' @keywords internal
#' @noRd
#'

.suso_transform_fullValid_q <- function(input = NULL) {
  ##########################################
  ## v2.1 with validations
  ## IDs:
  ##    L0 = Section, L1=position inside section,
  ##    L2 = Roster/Subsection Nr, (when missing no roster)
  ##    L3 = position inside Roster/Subsection,
  ##    L4 = Roster/Subsection (when missing no roster)
  qfinal <-bind_rows(
    ######################################################
    ## first
    input |> spread_values(
      Id = jstring("Id"),
      LastEntryDate = jstring("LastEntryDate")
    ) |> enter_object("Children") |> gather_array("L0") |>
      spread_values(
        type = jstring("$type"),
        PublicKey = jstring("PublicKey"),
        Title = jstring("Title")
      ),
    ## second
    input |> enter_object("Children") |> gather_array("L0") |>
      enter_object("Children") |> gather_array("L1") |>
      spread_values(
        type = jstring("$type"),
        PublicKey = jstring("PublicKey"),
        Title = jstring("Title"),
        VariableName = jstring("VariableName"),
        QuestionScope = jnumber("QuestionScope"),
        QuestionText = jstring("QuestionText"),
        Featured = jlogical("Featured")

      ),
    ## third
    input |> enter_object("Children") |> gather_array("L0") |>
      enter_object("Children") |> gather_array("L1") |>
      enter_object("Children") |> gather_array("L2") |>
      spread_values(
        type = jstring("$type"),
        PublicKey = jstring("PublicKey"),
        Title = jstring("Title"),
        VariableName = jstring("VariableName"),
        QuestionScope = jnumber("QuestionScope"),
        QuestionText = jstring("QuestionText"),
        Featured = jlogical("Featured")
      ),
    ## fourth
    input |> enter_object("Children") |> gather_array("L0") |>
      enter_object("Children") |> gather_array("L1") |>
      enter_object("Children") |> gather_array("L2") |>
      enter_object("Children") |> gather_array("L3") |>
      spread_values(
        type = jstring("$type"),
        PublicKey = jstring("PublicKey"),
        Title = jstring("Title"),
        VariableName = jstring("VariableName"),
        QuestionScope = jnumber("QuestionScope"),
        QuestionText = jstring("QuestionText"),
        Featured = jlogical("Featured")
      ),
    ## fifth
    input |> enter_object("Children") |> gather_array("L0") |>
      enter_object("Children") |> gather_array("L1") |>
      enter_object("Children") |> gather_array("L2") |>
      enter_object("Children") |> gather_array("L3") |>
      enter_object("Children") |> gather_array("L4") |>
      spread_values(
        type = jstring("$type"),
        PublicKey = jstring("PublicKey"),
        Title = jstring("Title"),
        VariableName = jstring("VariableName"),
        QuestionScope = jnumber("QuestionScope"),
        QuestionText = jstring("QuestionText"),
        Featured = jlogical("Featured")
      ),
    ## sixth
    input |> enter_object("Children") |> gather_array("L0") |>
      enter_object("Children") |> gather_array("L1") |>
      enter_object("Children") |> gather_array("L2") |>
      enter_object("Children") |> gather_array("L3") |>
      enter_object("Children") |> gather_array("L4") |>
      enter_object("Children") |> gather_array("L5") |>
      spread_values(
        type = jstring("$type"),
        PublicKey = jstring("PublicKey"),
        Title = jstring("Title"),
        VariableName = jstring("VariableName"),
        QuestionScope = jnumber("QuestionScope"),
        QuestionText = jstring("QuestionText"),
        Featured = jlogical("Featured")
      ),
    ## seventh
    input |> enter_object("Children") |> gather_array("L0") |>
      enter_object("Children") |> gather_array("L1") |>
      enter_object("Children") |> gather_array("L2") |>
      enter_object("Children") |> gather_array("L3") |>
      enter_object("Children") |> gather_array("L4") |>
      enter_object("Children") |> gather_array("L5") |>
      enter_object("Children") |> gather_array("L6") |>
      spread_values(
        type = jstring("$type"),
        PublicKey = jstring("PublicKey"),
        Title = jstring("Title"),
        VariableName = jstring("VariableName"),
        QuestionScope = jnumber("QuestionScope"),
        QuestionText = jstring("QuestionText"),
        Featured = jlogical("Featured")
      ),
    ## eight
    input |> enter_object("Children") |> gather_array("L0") |>
      enter_object("Children") |> gather_array("L1") |>
      enter_object("Children") |> gather_array("L2") |>
      enter_object("Children") |> gather_array("L3") |>
      enter_object("Children") |> gather_array("L4") |>
      enter_object("Children") |> gather_array("L5") |>
      enter_object("Children") |> gather_array("L6") |>
      enter_object("Children") |> gather_array("L7") |>
      spread_values(
        type = jstring("$type"),
        PublicKey = jstring("PublicKey"),
        Title = jstring("Title"),
        VariableName = jstring("VariableName"),
        QuestionScope = jnumber("QuestionScope"),
        QuestionText = jstring("QuestionText"),
        Featured = jlogical("Featured")
      )
    ###########################################################
  ) |> dplyr::select_if(.col_selector)
  qfinal<-data.table(qfinal)
  ###########################
  ## dynamic use of sprintf
  ##  - do.call and eval
  allSections<-names(qfinal)[grepl("^L[0-7]$", names(qfinal))]
  sprExpr<-paste(rep("%02d", length(allSections)), collapse = "")
  allSections<-paste0(".(", paste(allSections, collapse = ","), ")")
  qfinal[,intID:=do.call(sprintf, c(list(sprExpr), qfinal[,eval(parse(text = allSections))]))]
  qfinal<-qfinal[,document.id:=NULL][]
  ## Get Validations
  valfinal_1<-.suso_transform_fullValid_val(input = input)
  if(!is.null(valfinal_1)){
    ###########################
    ## dynamic use of sprintf
    ##  - do.call and eval
    ##  - to harmonize ID, ID var is created here!!!
    allSections<-names(valfinal_1)[grepl("^L[0-7]$", names(valfinal_1))]
    sprExpr<-paste(rep("%02d", length(allSections)), collapse = "")
    allSections<-paste0(".(", paste(allSections, collapse = ","), ")")
    ## ID for validations
    valfinal_1[,intID:=do.call(sprintf, c(list(sprExpr), valfinal_1[,eval(parse(text = allSections))]))]
    ## ID for questionnaire
    qfinal[,intID:=do.call(sprintf, c(list(sprExpr), qfinal[,eval(parse(text = allSections))]))]
    qVar<-qfinal[,.(intID, VariableName)]
    valfinal_1<-valfinal_1[,.(intID, Expression, Message, Severity)][]
    setkeyv(valfinal_1, "intID"); setkeyv(qVar, "intID")
    valfinal_1<-valfinal_1[qVar, nomatch=0]
  }
  q_final<-list(q=qfinal, val=valfinal_1)
  return(q_final)
}

#' @keywords internal
#' @noRd
#'
.suso_transform_fullValid_val <- function(input = NULL) {
  valfinal <-bind_rows(
    ######################################################
    ## VALIDATIONS (Start with L0)
    ######################################################
    input |> enter_object("Children") |> gather_array("L0") |>
      enter_object("Children") |> gather_array("L1") |>
      enter_object("ValidationConditions") |> gather_array("val1") |>
      spread_all(),
    ## third
    input |> enter_object("Children") |> gather_array("L0") |>
      enter_object("Children") |> gather_array("L1") |>
      enter_object("Children") |> gather_array("L2") |>
      enter_object("ValidationConditions") |> gather_array("val1") |>
      spread_all(),
    ## fourth
    input |> enter_object("Children") |> gather_array("L0") |>
      enter_object("Children") |> gather_array("L1") |>
      enter_object("Children") |> gather_array("L2") |>
      enter_object("Children") |> gather_array("L3") |>
      enter_object("ValidationConditions") |> gather_array("val1") |>
      spread_all(),
    ## fifth
    input |> enter_object("Children") |> gather_array("L0") |>
      enter_object("Children") |> gather_array("L1") |>
      enter_object("Children") |> gather_array("L2") |>
      enter_object("Children") |> gather_array("L3") |>
      enter_object("Children") |> gather_array("L4") |>
      enter_object("ValidationConditions") |> gather_array("val1") |>
      spread_all(),
    ## sixth
    input |> enter_object("Children") |> gather_array("L0") |>
      enter_object("Children") |> gather_array("L1") |>
      enter_object("Children") |> gather_array("L2") |>
      enter_object("Children") |> gather_array("L3") |>
      enter_object("Children") |> gather_array("L4") |>
      enter_object("Children") |> gather_array("L5") |>
      enter_object("ValidationConditions") |> gather_array("val1") |>
      spread_all(),
    ## seventh
    input |> enter_object("Children") |> gather_array("L0") |>
      enter_object("Children") |> gather_array("L1") |>
      enter_object("Children") |> gather_array("L2") |>
      enter_object("Children") |> gather_array("L3") |>
      enter_object("Children") |> gather_array("L4") |>
      enter_object("Children") |> gather_array("L5") |>
      enter_object("Children") |> gather_array("L6") |>
      enter_object("ValidationConditions") |> gather_array("val1") |>
      spread_all(),
    ## eight
    input |> enter_object("Children") |> gather_array("L0") |>
      enter_object("Children") |> gather_array("L1") |>
      enter_object("Children") |> gather_array("L2") |>
      enter_object("Children") |> gather_array("L3") |>
      enter_object("Children") |> gather_array("L4") |>
      enter_object("Children") |> gather_array("L5") |>
      enter_object("Children") |> gather_array("L6") |>
      enter_object("Children") |> gather_array("L7") |>
      enter_object("ValidationConditions") |> gather_array("val1") |>
      spread_all()
    ###########################################################
  ) |> dplyr::select_if(.col_selector)


  valfinal<-data.table(valfinal)
  if(nrow(valfinal)==0) {
    return(NULL)
  } else {
    return(valfinal)
  }
}

# get all questions from questionnaire
.questionnaire_gpsquestion <- function(dt) {
  # Check if 'type' column exists
  if (!("type" %in% names(dt))) {
    stop("The 'type' column does not exist in the data table.")
  }

  # Identify rows where 'type' contains 'Gps'
  rows_with_question <- dt[grepl("Gps", type) | grepl("AreaQuestion", type), ]
  # create type1 with gps/map area/map point
  rows_with_question[,type1:=character(.N)]
  # gps
  rows_with_question[grepl("Gps", type), type1:="GPS"]
  # map area (!!ADD MULTIPOINT & LINE)
  for(i in 1:nrow(rows_with_question)) {
    if(grepl("AreaQuestion", rows_with_question[i, type])) {
      if(rows_with_question$..JSON[[i]]$Properties$GeometryType == 0) {
        rows_with_question[i, type1:="POLY"]
      } else if(rows_with_question$..JSON[[i]]$Properties$GeometryType == 1) {
        rows_with_question[i, type1:="LINE"]
      } else if(rows_with_question$..JSON[[i]]$Properties$GeometryType == 2) {
        rows_with_question[i, type1:="POINT"]
      } else if(rows_with_question$..JSON[[i]]$Properties$GeometryType == 3) {
        rows_with_question[i, type1:="POINT"]
      }
    }
  }
  # rows_with_question[grepl("AreaQuestion", type) && ..JSON[[]]$Properties$GeometryType == 0, type1:="POLY"]
  # rows_with_question[grepl("AreaQuestion", type) && ..JSON[[]]$Properties$GeometryType == 3, type1:="POINT"]

  # mark type of area with quest$q$..JSON[[22]]$Properties$GeometryType
  # 0 for polygon, 3 for single point

  return(rows_with_question[])
}

# categorical answer options
.questionnaire_answeroptions <- function(input_list, full_list) {
  # Initialize an empty data frame
  answers_df <- data.frame(AnswerValue = character(),
                           AnswerText = character(),
                           isLinked = logical(),
                           stringsAsFactors = FALSE)

  # Iterate through the outer list
  for (item in input_list) {
    # Check if 'Answers' is in the list
    if ("Answers" %in% names(item)) {
      # Extract the 'Answers' sublist

      if ("LinkedToQuestionId" %in% names(item)) {
        # if linked get max answers and creat response codes
        linkid<-item$LinkedToQuestionId
        tl<-full_list[PublicKey==linkid, .(..JSON)]
        tl<-tl$..JSON[[1]]
        # get the max count
        if ("MaxAnswerCount" %in% names(tl)) {
          maxCount<-tl$MaxAnswerCount
          varName<-tl$VariableName
          # generate levels and labels
          item$Answers<-mapply(
            function(x,y) list(AnswerValue = x, AnswerText = y),
            c(1:maxCount),
            sprintf("%s__%d", varName, 0:(maxCount-1)),
            SIMPLIFY = F, USE.NAMES = F
          )
          islink<-TRUE
        }

      } else {
        islink<-FALSE
      }

      answers <- item$Answers
      # Iterate through the 'Answers' sublist and add to the data frame
      for (answer in answers) {
        answers_df <- rbind(answers_df, data.frame(AnswerValue = answer$AnswerValue,
                                                   AnswerText = answer$AnswerText,
                                                   isLinked = islink,
                                                   stringsAsFactors = FALSE))
      }
    }
  }

  return(answers_df)
}

# remove HTML text
.questionnaire_remove_html_tags <- function(dt, column) {
  # Regular expression for HTML tags
  regex <- "<[^>]+>"

  # Applying gsub to remove HTML tags
  dt[, (column) := gsub(regex, "", get(column))]

  return(dt)
}

# convert multi/single select to factor
.export_convert_to_factor <- function(dt, labels_dt) {
  #dt<-copy(dt)
  # make values numeric if not
  if(!is.numeric(labels_dt$AnswerValue)) {
    labels_dt[, AnswerValue := as.numeric(AnswerValue)]
  }
  # on.exit(
  #   rm(dt),
  #   gc()
  # )
  # Unique base variable names
  base_vars <- unique(sub("__.*", "", names(dt)))
  # Use only base vars where factor is available
  base_vars <- base_vars[base_vars %in% labels_dt$VariableName]
  # Return dt if not base vars
  if (length(base_vars) == 0) {
    return(dt)
  }

  # Iterate over each base variable name
  for (base_var in base_vars) {
    # first check for single column (ie single select)
    if(base_var %in% names(dt)) {
      # Create factor
      label_rows <- labels_dt[VariableName == base_var]

      lev<-c(label_rows$AnswerValue)
      lab<-c(label_rows$AnswerText)

      dt[[base_var]]<-factor(dt[[base_var]],
                             levels = lev,
                             labels = lab)


    } else {

      # Find all columns related to this base variable
      related_cols <- grep(paste0("^", base_var, "__"), names(dt), value = TRUE)
      #dt[, c(base_var) := numeric(.N)]
      dt[[base_var]]<-numeric(nrow(dt))
      # Consolidate into a single variable
      for (col in related_cols) {
        fval<-as.integer(sub(paste0("^", base_var, "__"), "", col))
        dt[[base_var]]<-data.table::fifelse(dt[[col]] == 1, fval, dt[[base_var]])
      }

      # Drop the original long-form columns -->suppress the warning
      suppressWarnings(
        dt[, (related_cols) := NULL]
      )

      # Check if the base variable name is in the VariableName column of labels_dt
      if (base_var %in% labels_dt$VariableName) {
        # Extract labels_dt
        label_rows <- labels_dt[VariableName == base_var]

        lev<-c(label_rows$AnswerValue)
        lab<-c(label_rows$AnswerText)

        # Create factor
        dt[[base_var]]<-factor(dt[[base_var]],
                               levels = lev,
                               labels = lab)

      }
    }
  }

  return(dt)
}


