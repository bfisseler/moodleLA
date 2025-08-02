#' Generate a new pepper
#'
#' Basically generates a password of random length between 12 and 20
#'
#' @return A character vector of length one.
#' @noRd


genPepper <- function(){
  password(n = sample(12:20, 1), special = c("#", "?", "!", "@", "$", "%", "^","&", "*"))
}

# copied from: https://github.com/enricoschumann/password/blob/master/R/password.R
#' Generate password
#'
#' Function shuffles the rows of a dataframe into a random order.
#'
#' @param n integer: length of password
#' @param numbers logical: include numbers from 0 to 9?
#' @param case logical: use upper and lower case letters?
#' @param special character vector for special letters to use in password
#'
#' @return A character vector of length one.
#' @noRd

password <- function(n = 8, numbers = TRUE, case = TRUE,
                     special = c("?", "!", "&", "%", "$")) {
  resample <- function(x, ...)
    x[sample.int(length(x), ... , replace = TRUE)]
  from <- letters
  if (numbers)
    from <- c(from, 0:9)
  if (!identical(FALSE, special))
    from <- c(from, special)
  if (case) from <- c(from, LETTERS)
  res <- resample(from, n)
  paste(res, collapse = "")
}