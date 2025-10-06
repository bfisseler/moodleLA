#' Resolves all objecttable and objectids in the provided Moodle logdata
#'
#' Returns the dataframe with Moodle logdata with additional column objectname
#'
#' @param dbp a pool object for db connection
#' @param prefix prefix for tables, e.g. "m_"
#' @param logdata dataframe with Moodle logdata
#'
#' @importFrom dplyr select filter distinct left_join
#' @return A dataframe with Moodle logdata with additional column objectname
#' @export

mdl_resolve_objectids <- function(dbp, prefix, logdata){

  supported_objecttable <- c("assign", "book", "choicegroup", "course_categories", "course_sections", "event", "folder", "forum", "forum_discussions", "feedback", "glossary", "imscp", "helixmedia", "hvp", "label", "lti", "lesson", "page", "quiz", "resource", "questionnaire", "survey", "url", "wiki", "workshop", "course", "wiki_pages", "assign_submission", "book_chapters", "forum_discussion_subs", "forum_posts", "forum_subscriptions", "feedback_complete", "glossary_entries", "quiz_attempts", "questionnaire_response", "survey_answers", "workshop_submissions")

  dfLogObjects <- logdata |> dplyr::select(objecttable, objectid) |> 
  dplyr::distinct() |> dplyr::filter(objecttable %in% supported_objecttable) |>
  dplyr::filter(objecttable %in% supported_objecttable)

  dfLogObjects <- split(dfLogObjects, dfLogObjects$objecttable)

  resolve_results <- lapply(dfLogObjects, function(data_subset) {
    resolve_objecttable(dbp, prefix, data_subset)
  })

  resolve_results <- do.call(rbind, resolve_results)

  logdata <- dplyr::left_join(logdata, resolve_results, by = c("objecttable" = "objecttable", "objectid" = "objectid"))
}

#' Resolves all objecttable and objectids in the provided Moodle logdata
#'
#' Returns the dataframe with Moodle logdata with additional column objectname
#'
#' @param dbp a pool object for db connection
#' @param prefix prefix for tables, e.g. "m_"
#' @param dfObjecttable dataframe with two columns; objecttable and objectid
#'
#' @importFrom dplyr left_join
#' @importFrom glue glue_safe
#' @importFrom pool  poolCheckout poolReturn dbGetQuery
#' @return A dataframe with Moodle logdata with additional column objectname
#' @noRd
resolve_objecttable <- function(dbp, prefix, dfObjecttable) {
  conn <- pool::poolCheckout(dbp)
  on.exit(pool::poolReturn(conn))
  # simple resolving
  simple <- c("assign","book","choicegroup","course_categories","course_sections","event","folder","forum","forum_discussions","feedback","glossary","imscp","helixmedia","hvp","label","lti","lesson","page","quiz","resource","questionnaire","survey","url","wiki", "workshop")
  if(dfObjecttable$objecttable[1] %in% simple){
    tableName <- dfObjecttable$objecttable[1]
    ids <- paste(dfObjecttable$objectid, collapse = ",")
    query <- glue::glue_safe("SELECT id AS objectid, name AS objectname FROM {prefix}{tableName} WHERE id IN ({ids});")
    result <- pool::dbGetQuery(conn, query)
    result <- dplyr::left_join(dfObjecttable, result)
  } else{ # nothing fits -> simply return NA
    dfObjecttable$objectname <- NA
    result <- dfObjecttable
  }
  
  return(result)
}