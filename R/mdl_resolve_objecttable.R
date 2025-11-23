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
  local_objecttable <- dfObjecttable$objecttable[1]
  if(local_objecttable %in% simple){
    tableName <- local_objecttable
    ids <- paste(dfObjecttable$objectid, collapse = ",")
    query <- glue::glue_safe("SELECT id AS objectid, name AS objectname FROM {prefix}{tableName} WHERE id IN ({ids});")
    result <- pool::dbGetQuery(conn, query)
    result <- dplyr::left_join(dfObjecttable, result, by = dplyr::join_by(objectid))
  } else if(local_objecttable == "course"){ # resolve course
    tableName <- local_objecttable
    ids <- paste(dfObjecttable$objectid, collapse = ",")
    query <- glue::glue_safe("SELECT id AS objectid, fullname AS objectname FROM {prefix}{tableName} WHERE id IN ({ids});")
    result <- pool::dbGetQuery(conn, query)
    result <- dplyr::left_join(dfObjecttable, result, by = dplyr::join_by(objectid))
  } else if(local_objecttable == "wiki_pages"){ # resolve wiki_pages
    tableName <- local_objecttable
    ids <- paste(dfObjecttable$objectid, collapse = ",")
    query <- glue::glue_safe("SELECT id AS objectid, title AS objectname FROM {prefix}{tableName} WHERE id IN ({ids});")
    result <- pool::dbGetQuery(conn, query)
    result <- dplyr::left_join(dfObjecttable, result, by = dplyr::join_by(objectid))
  } else if(local_objecttable == "book_chapters"){ # resolve book_chapters
    tableName <- local_objecttable
    ids <- paste(dfObjecttable$objectid, collapse = ",")
    query <- glue::glue_safe("SELECT id AS objectid, title AS objectname FROM {prefix}{tableName} WHERE id IN ({ids});")
    result <- pool::dbGetQuery(conn, query)
    result <- dplyr::left_join(dfObjecttable, result, by = dplyr::join_by(objectid))
  } else if(local_objecttable == "forum_discussion_subs"){ # resolve forum_discussion_subs
    tableName <- local_objecttable
    ids <- paste(dfObjecttable$objectid, collapse = ",")
    query <- glue::glue_safe("SELECT fds.id AS objectid, fd.name AS objectname
        FROM 
            {prefix}{tableName} fds
        JOIN 
            {prefix}forum_discussions fd ON fds.forum = fd.id
        WHERE 
            fds.id IN ({ids});")
    result <- pool::dbGetQuery(conn, query)
    result <- dplyr::left_join(dfObjecttable, result, by = dplyr::join_by(objectid))
  } else if(local_objecttable == "forum_posts"){ # resolve forum_posts
    tableName <- local_objecttable
    ids <- paste(dfObjecttable$objectid, collapse = ",")
    query <- glue::glue_safe("SELECT fp.id AS objectid, f.name AS objectname
        FROM 
            {prefix}{tableName} fp
        JOIN 
            {prefix}forum_discussions fd ON fp.discussion = fd.id
        JOIN
            {prefix}forum f ON fd.forum = f.id
        WHERE 
            fp.id IN ({ids});")
    result <- pool::dbGetQuery(conn, query)
    result <- dplyr::left_join(dfObjecttable, result, by = dplyr::join_by(objectid))
  } else if(local_objecttable == "forum_subscriptions"){ # resolve forum_subscriptions
    tableName <- local_objecttable
    ids <- paste(dfObjecttable$objectid, collapse = ",")
    query <- glue::glue_safe("SELECT fs.id AS objectid, f.name AS objectname
        FROM 
            {prefix}{tableName} fs
        JOIN
            {prefix}forum f ON fs.forum = f.id
        WHERE 
            fs.id IN ({ids});")
    result <- pool::dbGetQuery(conn, query)
    result <- dplyr::left_join(dfObjecttable, result, by = dplyr::join_by(objectid))
  } else if(local_objecttable == "feedback_completed"){ # resolve feedback_completed
    tableName <- local_objecttable
    ids <- paste(dfObjecttable$objectid, collapse = ",")
    query <- glue::glue_safe("SELECT fbc.id AS objectid, fb.name AS objectname
        FROM 
            {prefix}{tableName} fbc
        JOIN
            {prefix}feedback fb ON fbc.forum = fb.id
        WHERE 
            fbc.id IN ({ids});")
    result <- pool::dbGetQuery(conn, query)
    result <- dplyr::left_join(dfObjecttable, result, by = dplyr::join_by(objectid))
  } else if(local_objecttable == "glossary_entries"){ # resolve glossary_entries
    tableName <- local_objecttable
    ids <- paste(dfObjecttable$objectid, collapse = ",")
    query <- glue::glue_safe("SELECT ge.id AS objectid, g.name AS objectname
        FROM 
            {prefix}{tableName} ge
        JOIN
            {prefix}glossary g ON ge.glossaryid = g.id
        WHERE 
            ge.id IN ({ids});")
    result <- pool::dbGetQuery(conn, query)
    result <- dplyr::left_join(dfObjecttable, result, by = dplyr::join_by(objectid))
  } else if(local_objecttable == "quiz_attempts"){ # resolve quiz_attempts
    tableName <- local_objecttable
    ids <- paste(dfObjecttable$objectid, collapse = ",")
    query <- glue::glue_safe("SELECT qa.id AS objectid, q.name AS objectname
        FROM 
            {prefix}{tableName} qa
        JOIN
            {prefix}quiz q ON qa.quiz = q.id
        WHERE 
            qa.id IN ({ids});")
    result <- pool::dbGetQuery(conn, query)
    result <- dplyr::left_join(dfObjecttable, result, by = dplyr::join_by(objectid))
  } else if(local_objecttable == "questionnaire_response"){ # resolve questionnaire_response
    tableName <- local_objecttable
    ids <- paste(dfObjecttable$objectid, collapse = ",")
    query <- glue::glue_safe("SELECT qr.id AS objectid, q.name AS objectname
        FROM 
            {prefix}{tableName} qr
        JOIN
            {prefix}questionnaire q ON qr.questionnaireid = q.id
        WHERE 
            qr.id IN ({ids});")
    result <- pool::dbGetQuery(conn, query)
    result <- dplyr::left_join(dfObjecttable, result, by = dplyr::join_by(objectid))
  } else if(local_objecttable == "survey_answers"){ # resolve survey_answers
    tableName <- local_objecttable
    ids <- paste(dfObjecttable$objectid, collapse = ",")
    query <- glue::glue_safe("SELECT sa.id AS objectid, s.name AS objectname
        FROM 
            {prefix}{tableName} sa
        JOIN
            {prefix}survey s ON sa.survey = s.id
        WHERE 
            sa.id IN ({ids});")
    result <- pool::dbGetQuery(conn, query)
    result <- dplyr::left_join(dfObjecttable, result, by = dplyr::join_by(objectid))
  } else if(local_objecttable == "workshop_submissions"){ # resolve workshop_submissions
    tableName <- local_objecttable
    ids <- paste(dfObjecttable$objectid, collapse = ",")
    query <- glue::glue_safe("SELECT wss.id AS objectid, ws.name AS objectname
        FROM 
            {prefix}{tableName} wss
        JOIN
            {prefix}workshop ws ON wss.workshopid = ws.id
        WHERE 
            wss.id IN ({ids});")
    result <- pool::dbGetQuery(conn, query)
    result <- dplyr::left_join(dfObjecttable, result, by = dplyr::join_by(objectid))
  } else{ # nothing fits -> simply return NA
    dfObjecttable$objectname <- NA
    result <- dfObjecttable
  }
  
  return(result)
}