# using the geocoded data on planet names, link planets to languages and mythologies

library(here)

load(here("name_generation", "output", "languages.RData"))
load(here("name_generation", "output", "myth_sample.RData"))
name_data <- read.csv(here("name_generation","output","name_nationality.csv"))

temp <- data.frame(country=unique(name_data$country_name))
temp <- merge(temp, languages, all.x=TRUE)
#mismatches
temp[is.na(temp$language),]

#correct some country names
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

## replace missing values for a few language/language groups
#Cook Islands
name_corr$language[name_corr$country_iso=="CK"] <- "Maori"
name_corr$lgroup[name_corr$country_iso=="CK"] <- "Polynesian"
#French Polynesia
name_corr$language[name_corr$country_iso=="PF"] <- "French"
name_corr$lgroup[name_corr$country_iso=="PF"] <- "French"
#Guernsey
name_corr$language[name_corr$country_iso=="GG"] <- "English"
name_corr$lgroup[name_corr$country_iso=="GG"] <- "English"
#Isle of Man
name_corr$language[name_corr$country_iso=="IM"] <- "English"
name_corr$lgroup[name_corr$country_iso=="IM"] <- "English"
#Saint Helena
name_corr$language[name_corr$country_iso=="SH"] <- "English"
name_corr$lgroup[name_corr$country_iso=="SH"] <- "English"

name_corr[is.na(name_corr$lgroup) & !is.na(name_corr$country_name),]
#Thule is listed as Greenland, but I think should be left as is (its mythological)


## create a myth1 and myth2 for preferred mythological pantheons
name_corr$myth1 <- NA
name_corr$myth2 <- NA

#fill in the big cases first
name_corr$myth1[!is.na(name_corr$lgroup) & name_corr$lgroup=="Scandanavian"] <- "Norse"
name_corr$myth1[!is.na(name_corr$lgroup) & (name_corr$lgroup=="Slavic" | name_corr$lgroup=="Russian")] <- "Slavic"
name_corr$myth1[!is.na(name_corr$lgroup) & name_corr$lgroup=="Chinese"] <- "Chinese"
name_corr$myth1[!is.na(name_corr$lgroup) & name_corr$lgroup=="African"] <- "African"
name_corr$myth1[!is.na(name_corr$lgroup) & name_corr$lgroup=="Irish"] <- "Celtic"
name_corr$myth1[!is.na(name_corr$lgroup) & name_corr$lgroup=="Polynesian "] <- "Oceanic"
name_corr$myth1[!is.na(name_corr$lgroup) & name_corr$lgroup=="Japanese"] <- "Japanese"
name_corr$myth1[!is.na(name_corr$lgroup) & name_corr$lgroup=="Finnish"] <- "Finnish"
name_corr$myth2[!is.na(name_corr$lgroup) & name_corr$lgroup=="Finnish"] <- "Norse"
name_corr$myth1[!is.na(name_corr$lgroup) & name_corr$lgroup=="German"] <- "Germanic"
name_corr$myth2[!is.na(name_corr$lgroup) & name_corr$lgroup=="German"] <- "Norse"
name_corr$myth1[!is.na(name_corr$lgroup) & name_corr$lgroup=="Italian"] <- "Roman"
name_corr$myth1[!is.na(name_corr$lgroup) & name_corr$lgroup=="Italian"] <- "Etruscan"
name_corr$myth1[!is.na(name_corr$lgroup) & name_corr$lgroup=="Greek"] <- "Greek"
name_corr$myth1[!is.na(name_corr$lgroup) & name_corr$lgroup=="Indian"] <- "Hindu"
name_corr$myth2[!is.na(name_corr$lgroup) & name_corr$lgroup=="U.S."] <- "Native American"
name_corr$myth1[!is.na(name_corr$lgroup) & name_corr$lgroup=="Arabic"] <- "Middle Eastern"
name_corr$myth2[!is.na(name_corr$lgroup) & name_corr$lgroup=="Arabic"] <- "Mesopotamian"
name_corr$myth1[!is.na(name_corr$lgroup) & (name_corr$lgroup=="Burmese" |
                                               name_corr$lgroup=="Vietnamese" |
                                               name_corr$lgroup=="Indonesian" |
                                               name_corr$lgroup=="Thai" |
                                               name_corr$lgroup=="Malay" |
                                               name_corr$lgroup=="Filipino")] <- "South East Asian"
name_corr$myth1[!is.na(name_corr$lgroup) & name_corr$lgroup=="French"] <- "Roman"
name_corr$myth2[!is.na(name_corr$lgroup) & name_corr$lgroup=="French"] <- "Celtic"
name_corr$myth1[!is.na(name_corr$lgroup) & name_corr$lgroup=="English"] <- "Celtic"
name_corr$myth2[!is.na(name_corr$lgroup) & name_corr$lgroup=="English"] <- "Norse"

#saints
name_corr$myth2[!is.na(name_corr$lgroup) & name_corr$lgroup=="Hispanic"] <- "Christian Saints"
name_corr$myth2[!is.na(name_corr$lgroup) & name_corr$lgroup=="Portugese"] <- "Christian Saints"

#country specific

#Australian Aboriginal
name_corr$myth2[!is.na(name_corr$country_name) & name_corr$country_name=="Australia"] <- "Australian Aboriginal"

#Mexico
name_corr$myth1[!is.na(name_corr$country_name) & name_corr$country_name=="Mexico"] <- "Aztec"
name_corr$myth2[!is.na(name_corr$country_name) & name_corr$country_name=="Mexico"] <- "Maya"

#Baltic
name_corr$myth1[!is.na(name_corr$country_name) & name_corr$country_name=="Latvia"] <- "Baltic"
name_corr$myth2[!is.na(name_corr$country_name) & name_corr$country_name=="Latvia"] <- "Slavic"
name_corr$myth1[!is.na(name_corr$country_name) & name_corr$country_name=="Lithuania"] <- "Baltic"
name_corr$myth2[!is.na(name_corr$country_name) & name_corr$country_name=="Lithuania"] <- "Slavic"

#Voudou
name_corr$myth1[!is.na(name_corr$country_name) & name_corr$country_name=="Haiti"] <- "Voudou"
name_corr$myth2[!is.na(name_corr$country_name) & name_corr$country_name=="Haiti"] <- "French"


#central america (Maya)
name_corr$myth1[!is.na(name_corr$country_name) & name_corr$country_name=="Guatemala"] <- "Maya"
name_corr$myth1[!is.na(name_corr$country_name) & name_corr$country_name=="Nicaragua"] <- "Maya"
name_corr$myth1[!is.na(name_corr$country_name) & name_corr$country_name=="Costa Rica"] <- "Maya"
name_corr$myth1[!is.na(name_corr$country_name) & name_corr$country_name=="Panama"] <- "Maya"


#south america (Inca, South American)
#don't do this for Argentina and Brazil
name_corr$myth1[!is.na(name_corr$country_name) & name_corr$country_name=="Colombia"] <- "Inca"
name_corr$myth2[!is.na(name_corr$country_name) & name_corr$country_name=="Colombia"] <- "South American"
name_corr$myth1[!is.na(name_corr$country_name) & name_corr$country_name=="Peru"] <- "Inca"
name_corr$myth2[!is.na(name_corr$country_name) & name_corr$country_name=="Peru"] <- "South American"
name_corr$myth1[!is.na(name_corr$country_name) & name_corr$country_name=="Chile"] <- "Inca"
name_corr$myth2[!is.na(name_corr$country_name) & name_corr$country_name=="Chile"] <- "South American"
name_corr$myth1[!is.na(name_corr$country_name) & name_corr$country_name=="Paraguay"] <- "Inca"
name_corr$myth2[!is.na(name_corr$country_name) & name_corr$country_name=="Paraguay"] <- "South American"
name_corr$myth1[!is.na(name_corr$country_name) & name_corr$country_name=="Uruguay"] <- "Inca"
name_corr$myth2[!is.na(name_corr$country_name) & name_corr$country_name=="Uruguay"] <- "South American"
name_corr$myth1[!is.na(name_corr$country_name) & name_corr$country_name=="Bolivia"] <- "Inca"
name_corr$myth2[!is.na(name_corr$country_name) & name_corr$country_name=="Bolivia"] <- "South American"
name_corr$myth1[!is.na(name_corr$country_name) & name_corr$country_name=="Venezuela"] <- "Inca"
name_corr$myth2[!is.na(name_corr$country_name) & name_corr$country_name=="Venezuela"] <- "South American"


save(name_corr, file=here("name_generation", "output", "name_corr.RData"))
