#' Provides Moodle forum data from the specified course
#'
#' Returns a dataframe with Moodle forum data for the specified course. 
#'
#' @param prefix prefix for tables, e.g. "m_"
#' @param dbp a pool object for db connection
#' @param courseid the unique course ID of the course whose log data is being retrieved
#' @param forumids vector of forum ids, if present, function only returns messages from these forums
#'
#' @importFrom pool dbGetQuery
#' @return A dataframe with the forum data, restricted to...
#' @export

mdl_forumdata <- function(dbp, prefix, courseid, forumids){
  # checkout pool
  conn <- pool::poolCheckout(dbp)
  on.exit(pool::poolReturn(conn))
  query <- mdl_forumdata_query(prefix, courseid, forumids)
  forumdata <- pool::dbGetQuery(conn, query)
}

#' Raw query for Moodle forum data
#'
#' Returns a SQL query to retrieve Moodle log data for the specified course. 
#'
#' @param prefix prefix for tables, e.g. "m_"
#' @param courseid the unique course ID of the course whose log data is being retrieved
#' @param forumids vector of forum ids, if present, function only returns messages from these forums
#'
#' @importFrom glue glue
#' @return Character vector with SQL query
#' @export

mdl_forumdata_query <- function(prefix, courseid, forumids) {
  forumids <- paste(forumids, collapse = ', ') # as glue_safe does not eval R, we have to do it first
  glue::glue_safe("WITH 
    -- select all students enrolled in the course
    userlist AS (
    SELECT 
    u.firstname
    , u.lastname
    , u.username
    , u.email
    , u.id AS userid
    FROM {prefix}role_assignments AS ra
    JOIN {prefix}context AS context ON context.id = ra.contextid AND context.contextlevel = 50
    JOIN {prefix}course AS c ON c.id = context.instanceid AND c.id = {courseid}
    JOIN {prefix}user AS u ON u.id = ra.userid
    JOIN {prefix}course_completions AS cc ON cc.course = c.id AND cc.userid = u.id
    WHERE u.username LIKE 'q%'),
    forums AS (
        SELECT 
            id,
            name
        FROM
            {prefix}forum
        WHERE
            course = {courseid}
    ),
    -- select all discussions from forums of the course
    discussions AS (
        SELECT
            d.id AS id, 
            d.course AS course, 
            d.forum AS forum_id,
            forums.name AS forumname, 
            d.name, 
            d.firstpost, 
            d.userid, 
            d.groupid
        FROM
            {prefix}forum_discussions AS d
        LEFT JOIN forums ON d.forum = forums.id 
        WHERE
            course = {courseid} AND
            userid IN (SELECT userid FROM userlist) AND
            forum IN ({forumids})
    ),
    forumposts AS(
    SELECT 
        fp.id AS id, 
        fp.discussion AS discussion, 
        fp.parent AS parent_id, 
        pi.userid AS userid, 
        pi.username AS username, 
        fp.created AS created, 
        fp.subject AS subject, 
        fp.message AS message,
        d.forum_id AS forum_id,
        d.forumname AS forumname
    FROM
        {prefix}forum_posts fp
    -- join user info
    LEFT JOIN (
        SELECT
            userid, username
        FROM 
            userlist
    ) pi ON fp.userid = pi.userid
    -- join discussion
    JOIN (
        SELECT
            id,
            forum_id,
            forumname
        FROM
            discussions
    ) d ON fp.discussion = d.id
    -- only students posts
    WHERE 
        fp.userid IN (SELECT userid FROM userlist)
    )
    SELECT 
        * 
    FROM 
        forumposts fp
    WHERE
        fp.discussion IN (SELECT id FROM discussions);")
}

#' Remove personal information from a text
#'
#' Helper function to remove email addresses, URLs, and phone numbers from text. 
#'
#' @param text A character from which to remove personal information
#' @param remove_email Remove email addresses from text
#' @param remove_url Remove URLs from text
#' @param remove_phonenumber Remove phone numbers from text
#' @param placeholder Shall the personal information be replaced by placeholders (EMAIL, URL, PHONE)
#'
#' @importFrom stringr str_replace_all str_squish
#' @return Returns the input with personal information removed.
#' @export

pseudonymize_text <- function(text, remove_email = TRUE, remove_url = TRUE, remove_phonenumber = TRUE, placeholder = TRUE){
  #print(text)
  # ALLCAPS to lower
  #text <- gsub("\\b([A-Z]+)\\b", tolower, text)
  # replace URLs like WhatsApp invitations
  if(remove_url){
    pattern <- "https?://[\\w.-]+(\\.[\\w.-]+)+(/[\\w/.-]*)*(\\?[\\w=&-]*)?" # also removes URL-Params; TODO: improve regex
    if(placeholder){
      text <- stringr::str_replace_all(text, pattern, "<URL>")
    }else{
      text <- stringr::str_replace_all(text, pattern, "")
    }
  }
  
  # replace emails
  if(remove_email){
    pattern <- "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}" # TODO: improve regex
    if(placeholder){
      text <- stringr::str_replace_all(text, pattern, "<EMAIL>")
    }else{
      text <- stringr::str_replace_all(text, pattern, "")
    }
  }
  
  if(remove_phonenumber){
    pattern <- "\\+?\\(?\\d{1,4}\\)?[\\s\\-\\/]?[0-9]{1,4}[\\s\\-\\/]?[0-9]{1,4}[\\s\\-\\/]?[0-9]{1,4}" # TODO: improve regex
    if(placeholder){
      text <- stringr::str_replace_all(text, pattern, "<PHONE_NUMBER>")
    }else{
      text <- stringr::str_replace_all(text, pattern, "")
    }
  }
  
  # remove " ' \" und \'
  text <- gsub("[\"'\\\\]", "", text)
  
  # @name
  text <- stringr::str_replace_all(text, "@[[:alpha:]]+\\w", "")

  text <- stringr::str_squish(text)
  # is string empty?
  if(nchar(text) == 0){
    text <- ""
  }
  return(text)
}

#' @rdname pseudonymize_text
#' @export
pseudonymise_text <- pseudonymize_text

#' Remove personal information from a text
#'
#' Function to remove email addresses, URLs, and phone numbers from text. It is basically a vectorized version of pseudonymize_text().
#'
#' @param text Either a single character/string or character vector from which to remove personal information
#' @param remove_email Remove email addresses from text
#' @param remove_url Remove URLs from text
#' @param remove_phonenumber Remove phone numbers from text
#' @param placeholder Shall the personal information be replaced by placeholders (EMAIL, URL, PHONE)
#'
#' @return Returns the input with personal information removed.
#' @export

pseudonymize_messages <- function(text, remove_email = TRUE, remove_url = TRUE, remove_phonenumber = TRUE, placeholder = TRUE){
  if(!is.vector(text)){
    pseudonymize_text(text, remove_email, remove_url, remove_phonenumber, placeholder)
  }
  else{
    sapply(text, pseudonymize_text, remove_email = remove_email, remove_url = remove_url, remove_phonenumber = remove_phonenumber, placeholder = placeholder)
  }
}

#' @rdname pseudonymize_messages
#' @export
pseudonymise_messages <- pseudonymize_messages

#' Remove names from text
#'
#' Function names from a text and replace it with <PERSON>
#'
#' @param text Either a single character/string or character vector from which to remove personal information
#' @param firstnames lastnames A character vector of names, same length as lastnames
#' @param lastnames A character vector of names, same length as firstnames
#'
#' @return Text with names replaces with <PERSON>
#' 
#' @importFrom quanteda tokens tokens_replace
#' @export

remove_names <- function(text, firstnames, lastnames){
  #we want to build this only onces
  names <- build_token_replace(firstnames, lastnames)
  
  # build tokens
  text_tokens <- quanteda::tokens(text)
  # replace names
  text_tokens <- quanteda::tokens_replace(text_tokens, names$names, names$pattern, case_insensitive = TRUE)
  # return text, see: https://stackoverflow.com/a/69494297
  text_tokens <- lapply(text_tokens, paste, collapse = " ")
  # reduce multiple occurances of <PERSON>
  text_tokens <- gsub("(<PERSON>)(\\s+\\1)+", "\\1", text_tokens)
}

#' Helper function for remove_names
#'
#' Generates a replacement pattern data frame for quanteda::tokens_replace
#'
#' @return A dataframe with columens names and pattern
#' @noRd

build_token_replace <- function(firstnames, lastnames){
  # we build a replacement pattern for quanteda::token_replace
  # the final pattern will have fullnames on top
  # fullnames first
  fullnames <- paste(firstnames, lastnames, sep = " ")
  
  # now single names
  names <- c(firstnames, lastnames)
  # remove any zu von und
  compounds <- c("von", "und", "vom", "zum", "der", "dem", "zu", "von und zu", "vom", "zum", "vom und zum", "von der", "von dem")
  names <- names[!names %in% compounds]
  # split double names into singles
  singlenames <-lapply(names, function(name) {
    unlist(strsplit(name, "-"))
  })
  singlenames <- unlist(singlenames)
  
  # names plus singlenames
  names <- unique(c(names, singlenames))
  
  # add genitives, especially useful for German text
  genitives <- paste0(names, "s")
  ats <- paste0("@", names)
  
  # names plus genetiv-s and @names
  names <- unique(c(names, genitives, ats))
  
  # add fullnames to the top of the names list
  names <- c(fullnames, names)
  
  # für tokens_replace ein pattern-vektor mit gleicher länge erzeugen
  df <- data.frame(
    names = names,
    pattern = "<PERSON>",
    stringsAsFactors = FALSE  # don't treat strings as factors
  )
}