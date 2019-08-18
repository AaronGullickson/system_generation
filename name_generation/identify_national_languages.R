# read in data on primary languages by country and link it to our surname lists by language group

library(here)
library(htmltab)

url <- "https://www.infoplease.com/world/countries/languages-spoken-in-each-country-of-the-world"
languages <- htmltab(doc=url, which = "//table", header=0)
colnames(languages) <- c("country","languages")
languages$language <- trimws(sapply(strsplit(languages$languages, "[,;(0-9]"), function(x) {x[1]}))

#some cleanup

#anything before "Arabic" remove
languages$language[grepl("Arabic" ,languages$language)] <- "Arabic"

#anything before Turkic remove
languages$language[grepl("Turkic$" ,languages$language)] <- "Turkic"

#anything before Norwegian remove
languages$language[grepl("Norwegian$" ,languages$language)] <- "Norwegian"


#anything before or after Persian remove
languages$language[grepl("Persian" ,languages$language)] <- "Persian"

languages$language[grepl("and French$" ,languages$language)] <- "French"

#some words to remove
languages$language <- trimws(gsub("Castilian|less than|Standard|Creole and|Maltese and", "", languages$language))

languages <- languages[,c(1,3)]

#need to translate these into the categories of surnames_masterancestry as best as possible
#for missing values use US. I think the big missing category here is Persian and Hebrew
sort(unique(languages$language))

languages$lgroup <- NA
languages$lgroup[languages$language %in% c("English","Bislama","Tok Pisin")] <- "English"
languages$lgroup[languages$language %in% c("German","Luxermbourgish")] <- "German"
languages$lgroup[languages$language %in% c("Dutch")] <- "Dutch"
languages$lgroup[languages$language %in% c("German")] <- "German"
languages$lgroup[languages$language %in% c("Danish","Swedish","Norwegian","Icelandic")] <- "Scandanavian"
languages$lgroup[languages$language %in% c("French","Seselwa Creole")] <- "French"
languages$lgroup[languages$language %in% c("Italian")] <- "Italian"
languages$lgroup[languages$language %in% c("Spanish")] <- "Hispanic"
languages$lgroup[languages$language %in% c("Portuguese")] <- "Portugese"
languages$lgroup[languages$language %in% c("Russian","Belorussian","Ukrainian","Latvian","Lithuanian")] <- "Russian"
languages$lgroup[languages$language %in% c("Bulgarian","Bosnian","Croatian","Czech","Slovak","Slovenian","Macedonian")] <- "Slavic"
languages$lgroup[languages$language %in% c("Polish")] <- "Polish"
languages$lgroup[languages$language %in% c("Romanian","Moldovan")] <- "Romanian"
languages$lgroup[languages$language %in% c("Finnish","Estonian","Magyar")] <- "Finnish"
languages$lgroup[languages$language %in% c("Albanian")] <- "Albanian"
languages$lgroup[languages$language %in% c("Serbian","Serbian/Montenegrin")] <- "Serbian"
languages$lgroup[languages$language %in% c("Greek")] <- "Greek"
languages$lgroup[languages$language %in% c("Turkic","Turkish","Turkmen","Kazak","Kyrgyz","Uzbek")] <- "Turkish"
languages$lgroup[languages$language %in% c("Armenian")] <- "Armenian"
languages$lgroup[languages$language %in% c("Arabic","Amharic")] <- "Arabic"
languages$lgroup[languages$language %in% c("Swahili","Chichewa","IsiZulu","Kinyarwanda")] <- "African"
languages$lgroup[languages$language %in% c("Urdu")] <- "Pakistani"
languages$lgroup[languages$language %in% c("Hindi","Nepali","Maldivian Dhivehi","Sinhala","Bangla")] <- "Indian"
languages$lgroup[languages$language %in% c("Japanese")] <- "Japanese"
languages$lgroup[languages$language %in% c("Korean")] <- "Korean"
languages$lgroup[languages$language %in% c("Chinese","Mandarin")] <- "Chinese"
languages$lgroup[languages$language %in% c("Vietnamese","Khmer")] <- "Vietnamese"
languages$lgroup[languages$language %in% c("Bahasa Indonesia")] <- "Indonesian"
languages$lgroup[languages$language %in% c("Samoan","Marshallese","Nauruan","Palauan","Tetum","Tongan","Tuvaluan")] <- "Polynesian"
languages$lgroup[languages$language %in% c("Filipino")] <- "Filipino"

#whats left
sort(unique(languages$language[is.na(languages$lgroup)]))

# Big missing cases:
# Persian (and Tajik)
# Hebrew
# Malay languages (Bahasa Melayu, Malay)
# Sino-Tibetan (Burmese, Dzongkha)
# Thai (and related Lao)
# Mongolian
# Afar and Somali

languages$lgroup[is.na(languages$lgroup)] <- "U.S."

save(languages, file=here("name_generation", "output", "languages.RData"))
