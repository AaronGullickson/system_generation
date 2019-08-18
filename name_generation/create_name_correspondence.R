# using the geocoded data on planet names, link planets to languages and mythologies

library(here)

load(here("name_generation", "output", "languages.RData"))
load(here("name_generation", "output", "myth_sample.RData"))
name_data <- read.csv(here("name_generation","output","name_nationality.csv"))

temp <- data.frame(country=unique(name_data$country_name))
temp <- merge(temp, languages, all.x=TRUE)
#mismatches
temp[is.na(temp$language),]

name_data$country_name <- as.character(name_data$country_name)
name_data$country_name[name_data$country_name=="Czechia"] <- "Czech Republic"
name_data$country_name[name_data$country_name=="D.R."] <- "Dominican Republic"
name_data$country_name[name_data$country_name=="DR Congo"] <- "Congo, Democratic Republic of the"
name_data$country_name[name_data$country_name=="North Korea"] <- "Korea, North"
name_data$country_name[name_data$country_name=="South Korea"] <- "Korea, South"
name_data$country_name[name_data$country_name=="PRC"] <- "China"
name_data$country_name[name_data$country_name=="RSA"] <- "South Africa"
name_data$country_name[name_data$country_name=="The Gambia"] <- "Gambia"
name_data$country_name[name_data$country_name=="USA"] <- "United States"
name_data$country_name[name_data$country_name=="B&H"] <- "Bosnia and Herzegovina"

temp <- data.frame(country=unique(name_data$country_name))
temp <- merge(temp, languages, all.x=TRUE)
#mismatches
temp[is.na(temp$language),]

# the remaining cases have no entry in our data source but we can probably enter individually
name_corr <- merge(name_data, languages, by.x="country_name", by.y="country", all.x=TRUE, all.y=FALSE)
name_corr <- name_corr[,c("id","founding_name","search_name","country_iso","country_name","language","lgroup")]

temp <- name_corr$country_iso %in% places_sample$country
sum(temp=FALSE)
# Good, every country has some places in place sample

#replace a few language groups in English, cause ethnocentrism
name_corr$lgroup[name_corr$country_iso=="US"] <- "U.S."
name_corr$lgroup[name_corr$country_name=="Ireland"] <- "Irish"

##TODO: our geocoder did not separate out scotland in the United Kingdom

name_corr[is.na(name_corr$lgroup) & !is.na(name_corr$country_name),]
#TODO: fill in languages and language groups for these cases

#for mythology the one selected will increase the odds of those selected by a factor of 10. If NA, then just use the default

save(name_corr, file=here("name_generation", "output", "name_corr.RData"))
