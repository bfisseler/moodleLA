#' Returns infos on all objects in a Moodle course
#'
#' Returns a dataframe names of all objects in a Moodle course
#'
#' @param prefix prefix for tables, e.g. "m_"
#' @param dbp a pool object for db connection
#' @param courseid the unique course ID of the course whose log data is being retrieved
#'
#' @importFrom pool dbGetQuery
#' @importFrom pool poolCheckout
#' @importFrom pool poolReturn
#' @return A dataframe with the name of all objects in a Moodle course
#' @export

mdl_course_objects <- function(dbp, prefix, courseid){
  conn <- pool::poolCheckout(dbp)
  on.exit(pool::poolReturn(conn))
  query <- mdl_course_objects_query(dbp, prefix, courseid)
  course_objects <- pool::dbGetQuery(conn, query)
  course_objects$module_name <- paste0("mod_", course_objects$module_name)
  return(course_objects)
}

#' Raw query to retrieve names of all objects in a Moodle course
#'
#' Returns a SQL query to retrieve names of all objects in a Moodle course. 
#'
#' @param dbp a pool object for db connection
#' @param prefix prefix for tables, e.g. "m_"
#' @param courseid the unique course ID of the course whose log data is being retrieved
#'
#' @importFrom glue glue
#' @importFrom pool dbGetQuery
#' @importFrom pool poolCheckout
#' @importFrom pool poolReturn
#' @return Character vector with SQL query
#' @export

mdl_course_objects_query <- function(dbp, prefix, courseid){
  conn <- pool::poolCheckout(dbp)
  on.exit(pool::poolReturn(conn))
  
  cObjIDs <- pool::dbGetQuery(conn, glue::glue_safe("SELECT course, module FROM {prefix}course_modules WHERE course = {courseid};"))
  dfModules <- pool::dbGetQuery(conn, glue::glue_safe("SELECT id, name FROM {prefix}modules;"))
  dfModules$shortname <- remove_vowels(dfModules$name)
  
  # generate WHEN conditions for all modules
  sql_conditions <- apply(dfModules, 1, function(x) {
    glue::glue("WHEN m.name = '{x['name']}' THEN {x['shortname']}.name")
  })
  sql_conditions <- paste(sql_conditions, collapse = "\n")
  
  # generate LEFT JOINS for all modules
  sql_joins <- apply(dfModules, 1, function(x) {
    glue::glue("LEFT JOIN {prefix}{x['name']} {x['shortname']} ON cm.instance = {x['shortname']}.id AND m.name = '{x['name']}'")
  })
  sql_joins <- paste(sql_joins, collapse = "\n")
  
  mdl_course_objects_query <- glue::glue_safe(
    "SELECT 
    -- cm.id AS module_id,
    cm.instance AS objectid,
    m.name AS module_name,
  CASE 
      {sql_conditions}
      ELSE 'Unknown Module'
    END AS objectname,
    c.fullname AS course_name
  FROM 
    m_course_modules cm
  JOIN 
    m_modules m ON cm.module = m.id
  JOIN 
    m_course c ON cm.course = c.id
  {sql_joins}
  WHERE 
    cm.course = {courseid}
  "
  )
}

# non-exported function
# used to shorten the module names

remove_vowels <- function(word) {
  gsub("[aeiouAEIOU]", "", word)
}