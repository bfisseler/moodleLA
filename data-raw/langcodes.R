dfLangCodes <- data.frame(
  code = c("ca", "zh", "hr", "da", "nl", "en", "fi", "fr", "de", "el",
           "it", "ja", "ko", "lt", "mk", "nb", "pl", "pt", "ro", "sl",
           "es", "sv", "uk"),
  language = c("Catalan", "Chinese", "Croatian", "Danish", "Dutch", 
               "English", "Finnish", "French", "German", "Greek", 
               "Italian", "Japanese", "Korean", "Lithuanian", "Macedonian", 
               "Norwegian Bokmål", "Polish", "Portuguese", "Romanian", 
               "Slovenian", "Spanish", "Swedish", "Ukrainian"),
  stringsAsFactors = FALSE
)

listLangCodes <- setNames(as.list(dfLangCodes$code), dfLangCodes$language)

usethis::use_data(dfLangCodes, listLangCodes, internal = TRUE, overwrite = TRUE)