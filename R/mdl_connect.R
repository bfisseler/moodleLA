#' Get connection to Moodle database
#'
#' Returns a pool object; basically a wrapper for dbPool
#'
#' @param type connection type for database
#' @param host ip or host address
#' @param port database port number
#' @param name database name
#' @param user user name
#' @param passw password
#'
#' @importFrom pool dbPool
#' @return A dbplyr reference object.
#' @export
mdl_connect <- function(type, host, port, name, user, passw){
  # param check first
  if (missing(type) || missing(host) || missing(port) || 
      missing(name) || missing(user) || missing(passw)) {
    stop("All parameters (type, host, port, name, user, passw) are required.")
  }
  
  if (!is.numeric(port) || port < 1 || port > 65535) {
    stop("Invalid port; it must be a numeric value between 1 and 65535.")
  }
  
  # check db type and choose driver
  drv <- switch(type,
                mysql = RMySQL::MySQL(),
                postgresql = RPostgres::Postgres(),
                mariadb = RMariaDB::MariaDB(),
                stop("Invalid database type. Please use 'mysql', 'postgresql', or 'mariadb'")
  )
  pool <- tryCatch({
    pool::dbPool(
      drv,
      dbname = name,
      host = host,
      port = port,
      user = user,
      password = passw
    )
  }, error = function(e) {
    stop("Failed to create database connection: ", e$message)
  })
  #print(pool)
  return(pool)
}