#' Pseudonymize (anonymize) text with Microsoft Presidio
#'
#' @description
#' Sends the input text(s) to Presidio *Analyzer* and pipes the result
#' directly into Presidio *Anonymizer*.
#' Works with a single string or a character vector.
#'
#' @param text Character vector with one or more texts.
#' @param language Language code understood by Presidio (e.g. `"en"`, `"de"`).
#' @param analyzer_url Base URL of the Analyzer service *without* trailing slash.
#'        Default `"http://localhost:5002"`.
#' @param anonymizer_url Base URL of the Anonymizer service *without* trailing slash.
#'        Default `"http://localhost:5001"`.
#' @param anonymizers Named list that will be sent as Presidio anonymizers
#'        configuration (default hashes everything with SHA256); currently ignored.
#' @param raise_error Logical. If `TRUE` (default) aborts on first HTTP error,
#'        otherwise returns `NA` for that element.
#'
#' @importFrom httr2 request resp_body_json req_perform
#' @return A character vector of the same length as `text` containing the
#'         pseudonymized versions.
#'
#' @examples
#' \dontrun{
#' pseudonymize_text(
#'   c("John Smith lives in Berlin.",
#'     "Meine Telefonnummer ist 030-123456."),
#'   language = "de"
#' )
#' }
#' @export
presidio_pseudonymize_text <- function(text,
                              language        = "en",
                              analyzer_url    = "http://localhost:5002/analyze",
                              anonymizer_url  = "http://localhost:5001/anonymize",
                              anonymizers     = list(default = list(type = "hash")),
                              raise_error     = TRUE) {
  
  stopifnot(is.character(text))
  out <- vector("character", length(text))
  
  # # small helper function for error handling
  # is_in_loop <- function() {
  #   return(FALSE)
  # }
  
  
  for (i in seq_along(text)) {
    
    # check if text is empty
    if (is.na(text[i]) | text[i] == "") {
      out[i] <- NA_character_
      next
    }
    
    analyze_req <- httr2::request(analyzer_url) |>
      httr2::req_body_json(list(
        text = text[i],
        language = language
      ),  auto_unbox = TRUE)
    
    analyze_resp <- tryCatch(
      analyze_req |> httr2::req_perform() |>
        httr2::resp_body_json(),
      error = function(cnd) {
        out[i] <- NA_character_
        return(NULL)  # return NULL in case of error
      }
    )
    
    if (is.null(analyze_resp)) {
      next  # analyzer call return error > next loop
    }
    
    anonymizer_req <- httr2::request(anonymizer_url) |>
      httr2::req_body_json(list(
        text = text[i],
        analyzer_results = analyze_resp
      ),  auto_unbox = TRUE)
    
    anonymizer_resp <- tryCatch(
      anonymizer_req |> httr2::req_perform() |>
        httr2::resp_body_json(),
      error = function(cnd) {
        out[i] <- NA_character_
        return(NULL)  # return NULL in case of error
      }
    )
    
    if (!is.null(anonymizer_resp)) {
      out[i] <- anonymizer_resp$text
    } else {
      out[i] <- NA_character_  # Fehlerbehandlung für Anonymizer
    }
  }
  return(out)
}

#' Checks whether or not Microsoft Presidio supports the requested language
#'
#' @description
#' Sends the input text(s) to Presidio *Analyzer* and pipes the result
#' directly into Presidio *Anonymizer*.
#' Works with a single string or a character vector.
#'
#' @param text Character vector with one or more texts.
#' @param language Language code (e.g. `"en"`, `"de"`).
#' @param analyzer_url Base URL of the Analyzer service *without* trailing slash.
#'        Default `"http://localhost:5002"`.
#'
#' @importFrom httr2 request resp_body_json req_perform
#' @return Logical indicating support of requested language
#'
#' @export
presidio_language_supported <- function(language        = "en",
                                        analyzer_url    = "http://localhost:5002/analyze"){
  text <- "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua."  
  analyze_req <- httr2::request(analyzer_url) |>
    httr2::req_body_json(list(
      text = text,
      language = language
    ),  auto_unbox = TRUE)
  
  analyze_resp <- tryCatch(
    {
      analyze_req |> httr2::req_perform() |>
      httr2::resp_body_json()
      return(TRUE)
    },
    error = function(cnd) {
      # code to run when error is thrown
      return(FALSE)
    }
  )
  
  analyze_resp
}

