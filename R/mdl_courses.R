#' Get Courses
#'
#' Returns a reference to the (cached) course table, with the most relevant columns selected.
#'
#' For convenience a join with the category table is made, and "category_name" added
#'
#' @param prefix prefix for tables, e.g. "m_"
#' @param dbp a pool object for db connection
#'
#' @importFrom pool dbGetQuery
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @return A dataframe with the course list.
#' @export
mdl_courses <- function(dbp, prefix){
  # checkout pool
  #prefix <- "m_" #debugging
  conn <- pool::poolCheckout(dbp)
  on.exit(pool::poolReturn(conn))
  
  courselist <- pool::dbGetQuery(conn, glue::glue_safe("SELECT * FROM {prefix}course;")) 
  courselist <- courselist |> 
    dplyr::select(id, category, sortorder, fullname) |> dplyr::filter(category > 1) |>
    dplyr::mutate(courseid = id) |>
    dplyr::mutate(categoryid = category) |>
    dplyr::left_join(
      pool::dbGetQuery(conn, glue::glue_safe("SELECT * FROM {prefix}course_categories;")) |>
        dplyr::select(categoryid = id, category_name = name)
    ) |>
    dplyr::select(courseid, categoryid, sortorder, fullname, category_name) |>
    dplyr::arrange(sortorder)
  #return(courselist)
}
