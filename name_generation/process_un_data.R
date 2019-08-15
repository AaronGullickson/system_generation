#so messy, UN

un <- read.csv(here("name_generation","input","un_languages_country.csv"))
colnames(un) <- c("country","year","area","sex","language","record.type","reliability","source_year","value")
un <- subset(un, sex=="Both Sexes" & area=="Total" & record.type!="Sample survey - de jure")

languages <- NULL
for(this_country in unique(un$country)) {
  current <- subset(un, country==this_country)
  #in case multiple years, take the most recent
  current <- subset(current, year==max(year))
  #drop Total, None, Not stated
  current <- subset(current, language!="Total" & language!="None" & language!="Not stated")
  current$percent <- round(100*prop.table(current$value),0)
  #drop if less than 5%
  current <- subset(current, percent>=5, select=c("country","language","percent"))
  languages <- rbind(languages, current)
}
