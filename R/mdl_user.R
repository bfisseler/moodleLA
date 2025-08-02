#' Provides a list of all users of a Moodle course
#'
#' Returns a dataframe with a list of all users (student role) for the specified course. 
#'
#' @param prefix prefix for tables, e.g. "m_"
#' @param dbp a pool object for db connection
#' @param courseid the unique course ID of the course whose data is being retrieved
#' @param returngroups if true, return all groups and groupings for each user
#'
#' @importFrom pool dbGetQuery
#' @importFrom dplyr select
#' @return A dataframe a list of all users
#' @export

mdl_userlist <- function(dbp, prefix, courseid, returngroups = FALSE){
  # checkout pool
  conn <- pool::poolCheckout(dbp)
  on.exit(pool::poolReturn(conn))
  query <- mdl_userlist_query(prefix, courseid)
  userlist <- pool::dbGetQuery(conn, query)
  if(returngroups){
    userlist <- userlist
  }else{
    userlist <- userlist |> select(-groupname, -grouping)
  }
}

#' Raw query for Moodle user list
#'
#' Returns a SQL query to retrieve all users for the specified course. 
#'
#' @param prefix prefix for tables, e.g. "m_"
#' @param courseid the unique course ID of the course whose data is being retrieved
#'
#' @importFrom glue glue
#' @return Character vector with SQL query
#' @export

mdl_userlist_query <- function(prefix, courseid) {
  glue::glue_safe("WITH grouplist AS(
SELECT 
	g.name AS group_name, 
	gg.name AS grouping_name, 
	u.id AS user_id
FROM {prefix}groups AS g
LEFT JOIN {prefix}groups_members AS gm ON g.id = gm.groupid
LEFT JOIN {prefix}user AS u ON gm.userid = u.id
LEFT JOIN {prefix}groupings_groups AS ggg ON g.id = ggg.groupid
LEFT JOIN {prefix}groupings AS gg ON ggg.groupingid = gg.id
WHERE g.courseid = {courseid})
SELECT 
    u.id, 
    u.username, 
    u.email,
    u.firstname,
    u.lastname,
    grouplist.group_name AS groupname,
    grouplist.grouping_name AS grouping
FROM {prefix}user AS u
JOIN {prefix}role_assignments AS ra ON u.id = ra.userid
JOIN {prefix}context AS c ON ra.contextid = c.id
JOIN {prefix}role AS r ON ra.roleid = r.id
FULL JOIN grouplist ON u.id = grouplist.user_id
WHERE c.contextlevel = 50 AND r.archetype = 'student' AND c.instanceid = {courseid}")
}