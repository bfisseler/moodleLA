#' Load additional function for wrangling logdata
#'
#' Function loads additional functions from a YAML file.
#'
#' @param yaml_file path to file with additional functions
#'
#' @return a dataframe
#' @noRd
load_functions <- function(yaml_file){
  result <- tryCatch({
    yaml_data <- suppressWarnings(yaml::read_yaml(yaml_file), classes = "warning")
    df <- dplyr::bind_rows(lapply(yaml_data$functions, as.data.frame))
    return(df)
  }, error = function(e) {
    return(NULL)  # return NULL in case of error
  })
}

#' Load additional function for wrangling logdata
#'
#' Function loads additional functions from a YAML file.
#'
#' @param df dataframe with Moodle logdata
#' @param fun R code to run on df
#' @param fun_name unique name of the function to run on df
#'
#' @return a dataframe
#' @noRd
shape_logdata <- function(df, fun, fun_name){
  # create sandbox environment
  sandbox_env <- new.env(parent = baseenv()) #new.env(parent = emptyenv())
  
  # import functions into sandbox
  # --- BASE -------
  base_list <- c(
    # vector and basic operations
    "c", "list", "length", "seq", "rep", "seq_along", "seq_len",
    # base math
    "sum", "mean", "min", "max", "range",
    "abs", "sign", "round", "signif", "floor", "ceiling", "trunc",
    # logical and comparison
    "any", "all", "which", "which.max", "which.min", "identical",
    # strings and patterns
    "paste", "paste0", "sprintf", "substr", "substring",
    "grep", "grepl", "regexpr", "gregexpr",
    "sub", "gsub", "chartr", "nchar", "strsplit", "trimws",
    # Datums‑/Zeit‑Hilfen (base‑Version, nicht lubridate)
    "as.Date", "as.POSIXct", "as.POSIXlt", "difftime",
    "Sys.Date", "Sys.time", "structure", "unclass",
    "format", "as.character", "as.numeric", "as.integer", "as.logical",
    # Sortieren & Ordnung
    "order", "sort", "rank", "rev", "unique", "duplicated",
    # Daten‑Frames & Listen‑Manipulation
    "data.frame", "as.data.frame", "rbind", "cbind", "apply",
    "lapply", "sapply", "vapply", "mapply", "tapply",
    "Map", "Reduce", "split", "subset",
    # Misc.
    "stop", "warning", "message", "cat", "print", "summary",
    "dim", "nrow", "ncol", "rownames", "colnames"
  )
  for (fn in base_list) {
    # Jeder Name wird aus dem `base`‑Namespace fetch‑ed und ins Sandbox‑Env
    assign(fn,
           getExportedValue("base", fn),
           envir = sandbox_env)
  }
  
  # ---- DPLYR ------------------------------------------------------
  dplyr_funs <- getNamespaceExports("dplyr")
  
  for (fn in dplyr_funs) {
    assign(fn,
           getExportedValue("dplyr", fn),
           envir = sandbox_env)
  }
  
  # ---- LUBRIDATE -------------------------------------------------
  lubridate_funs <- getNamespaceExports("lubridate") #c("floor_date", "ymd_hms", "period", "as_datetime")
  for (fn in lubridate_funs) {
    assign(fn,
           getExportedValue("lubridate", fn),
           envir = sandbox_env)
  }
  
  # validate function
  
  # create function
  eval(parse(text = fun), envir = sandbox_env) 
  
  # get function object
  #env_fun <- get(fun_name, envir = sandbox_env, inherits = FALSE)
  
  # execute function
  
  result <- tryCatch({
    #result <- env_fun(df)
    result <- sandbox_env[[fun_name]](df)
    return(result)
  }, error = function(e) {
    return(NULL)  # return NULL in case of error
  })
}