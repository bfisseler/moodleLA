#' Shuffle a dataframe
#'
#' Function shuffles the rows of a dataframe into a random order.
#'
#' @param df a dataframe
#'
#' @return a dataframe
#' @noRd

shuffle <- function(df){
  df <- df[sample(nrow(df)), ]
}

#' Pseudonymise matricle number
#'
#' Pseudonymises data such as student matricle number.
#'
#' @param matricle a single value
#' @param salt a random value added to the input before hashing
#'
#' @importFrom digest sha1
#' @return A pseudonymised value based on input and salt.
#' @export

pseudonymize_matricle <- function(matricle, salt){
  matricle <- as.character(matricle)
  salt <- as.character(salt)
  if(grepl("^[q,Q, p, P]\\d{7}$", matricle)){ #matriculation number with leading q
    matricle <- substr(matricle, 2, 8) #reduce to 7-digit matriculation number
  }
  if(!is.na(matricle) & matricle != ""){
    matricle <- digest::sha1(paste0(matricle, salt))
  }else{
    matricle <- NA
  }
}

#' @rdname pseudonymize_matricle
#' @export
pseudonymise_matricle <- pseudonymize_matricle

#' Pseudonymise data
#'
#' Pseudonymises data such as usernames or groupnames.
#'
#' @param input a single value or a vector
#' @param salt a random value added to the input before hashing
#'
#' @importFrom digest sha1
#' @return Pseudonymised result based on input and salt, either a single value or a vector.
#' @export

pseudonymize <- function(input, salt){
  if(!is.vector(input)){
    moodleLA::pseudonymize_matricle(input, salt)
  }
  else{
    sapply(input, moodleLA::pseudonymize_matricle, salt=salt)
  }
}

#' @rdname pseudonymize
#' @export
pseudonymise <- pseudonymize