#' Provides a data frame with all forums in the specified course
#'
#' Returns a dataframe with Moodle forum data for the specified course. 
#'
#' @param prefix prefix for tables, e.g. "m_"
#' @param dbp a pool object for db connection
#' @param courseid the unique course ID of the course whose log data is being retrieved
#'
#' @importFrom pool dbGetQuery
#' @return A dataframe with the forum data, restricted to...
#' @export

mdl_forumlist <- function(dbp, prefix, courseid){
  # checkout pool
  conn <- pool::poolCheckout(dbp)
  on.exit(pool::poolReturn(conn))
  query <- mdl_forumlist_query(prefix, courseid)
  forumlist <- pool::dbGetQuery(conn, query)
}

#' Raw query for Moodle forum data
#'
#' Returns a SQL query to retrieve a list with all forums from a Moodle course
#'
#' @param prefix prefix for tables, e.g. "m_"
#' @param courseid the unique course ID of the course whose log data is being retrieved
#'
#' @importFrom glue glue
#' @return Character vector with SQL query
#' @export

mdl_forumlist_query <- function(prefix, courseid) {
  glue::glue_safe("SELECT
    id,
    name
FROM {prefix}forum
WHERE course = {courseid};")
}