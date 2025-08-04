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

nicknames_de <- unique(readLines("data-raw/nicknames_de.csv"))
nicknames_en <- unique(readLines("data-raw/nicknames_en.csv"))
nicknames <- rbind(nicknames_de, nicknames_en)

usethis::use_data(dfLangCodes, listLangCodes, nicknames_de, nicknames_en, nicknames, internal = TRUE, overwrite = TRUE)