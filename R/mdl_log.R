#' Provides Moodle log data for the specified period 
#'
#' Returns a dataframe with Moodle log data for the specified course and time period. 
#'
#' @param prefix prefix for tables, e.g. "m_"
#' @param dbp a pool object for db connection
#' @param courseid the unique course ID of the course whose log data is being retrieved
#' @param startstamp Unix timestamp that marks the beginning of the time period from which the log data is being retrieved
#' @param stopstamp Unix timestamp that marks the end of the time period from which the log data is being retrieved
#'
#' @importFrom pool dbGetQuery
#' @importFrom pool poolCheckout
#' @importFrom pool poolReturn
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @return A dataframe with the logdata, restricted to tbd
#' @export

mdl_logdata <- function(dbp, prefix, courseid, startstamp, stopstamp){
  # check stop > start first
  if (!is.numeric(startstamp) & !is.numeric(stopstamp)) {
    stop("Error: Provide valid Unix timestamp.")
  }
  if(stopstamp <= startstamp){
    stop("Error: The stop timestamp must be greater than the start timestamp.")
  }
  
  #prefix <- "m_" #debugging
  # checkout pool
  conn <- pool::poolCheckout(dbp)
  on.exit(pool::poolReturn(conn))
  course_objects <- mdl_course_objects(dbp, prefix, courseid)
  query <- mdl_logdata_query(prefix, courseid, startstamp, stopstamp)
  logdata <- pool::dbGetQuery(conn, query)
  logdata <- dplyr::left_join(logdata, course_objects |> select(module_name, objectid, objectname), by = c("component" = "module_name", "objectid" = "objectid"))
}

#' Raw query for Moodle log data
#'
#' Returns a SQL query to retrieve Moodle log data for the specified course and time period. 
#'
#' @param prefix prefix for tables, e.g. "m_"
#' @param courseid the unique course ID of the course whose log data is being retrieved
#' @param startstamp Unix timestamp that marks the beginning of the time period from which the log data is being retrieved
#' @param stopstamp Unix timestamp that marks the end of the time period from which the log data is being retrieved
#'
#' @importFrom glue glue
#' @return Character vector with SQL query
#' @export

mdl_logdata_query <- function(prefix, courseid, startstamp, stopstamp) {
  glue::glue_safe("WITH userlist AS (
    SELECT 
      u.id AS userid,
      u.username AS username
    FROM {prefix}role_assignments AS ra
    JOIN {prefix}context AS context ON context.id = ra.contextid AND context.contextlevel = 50
    JOIN {prefix}course AS c ON c.id = context.instanceid AND c.id = {courseid}
    JOIN {prefix}user AS u ON u.id = ra.userid
    JOIN {prefix}course_completions AS cc ON cc.course = c.id AND cc.userid = u.id
    WHERE u.username LIKE 'q%'
    )
    SELECT 
        log.id AS id,
        ul.userid AS userid,
        ul.username AS username,
        log.relateduserid AS relateduserid,
        ul2.username AS relatedusername,
        log.courseid,
        log.timecreated,
        log.eventname AS eventname,
        log.component AS component,
        log.action AS action,
        log.target AS target,
        log.objecttable AS objecttable,
        log.objectid AS objectid,
        log.crud AS crud,
        log.edulevel
    FROM 
        {prefix}logstore_standard_log AS log
    LEFT JOIN userlist AS ul ON log.userid = ul.userid
    LEFT JOIN userlist AS ul2 ON log.relateduserid = ul2.userid
    WHERE (log.courseid = {courseid} OR log.courseid = 0) 
      AND (log.timecreated >= {startstamp} AND log.timecreated <= {stopstamp});")
}