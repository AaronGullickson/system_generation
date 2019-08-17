# read in data on primary languages by country and add to our name_data.

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
#for missing values use US. I think the big missing category here is Persian
sort(unique(languages$language))



#name_data <- read.csv(here("name_generation","output","name_nationality.csv"))

#temp <- data.frame(country=unique(name_data$country_name))

#temp <- merge(temp, languages, all=TRUE)
