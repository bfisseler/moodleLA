#' Provides a list of all groups of a Moodle course
#'
#' Returns a dataframe with a list of groups for the specified course. 
#'
#' @param prefix prefix for tables, e.g. "m_"
#' @param dbp a pool object for db connection
#' @param courseid the unique course ID of the course whose data is being retrieved
#'
#' @importFrom pool dbGetQuery
#' @importFrom dplyr select
#' @return A dataframe a list of all groups
#' @export

mdl_grouplist <- function(dbp, prefix, courseid){
  # checkout pool
  conn <- pool::poolCheckout(dbp)
  on.exit(pool::poolReturn(conn))
  query <- mdl_groupslist_query(prefix, courseid)
  grouplist <- pool::dbGetQuery(conn, query)
}

#' Raw query to retrieve all groups within a Moodle course
#'
#' Returns a SQL query to retrieve all groups and their groupings for the specified course. 
#'
#' @param prefix prefix for tables, e.g. "m_"
#' @param courseid the unique course ID of the course whose data is being retrieved
#'
#' @importFrom glue glue
#' @return Character vector with SQL query
#' @export

mdl_groupslist_query <- function(prefix, courseid) {
  glue::glue_safe("SELECT 
	g.id AS groupid,
    g.name AS groupname,
    gg.id AS groupingid,
	gg.name AS groupingname
FROM {prefix}groups AS g
LEFT JOIN {prefix}groupings_groups AS ggg ON g.id = ggg.groupid
LEFT JOIN {prefix}groupings AS gg ON ggg.groupingid = gg.id
WHERE g.courseid = {courseid}")
}