#update data from google sheets

library(googlesheets)
library(here)

#this is needed to authenticate, but may spit an error
gs_auth()

#### Flavor Data ####

load(here("name_generation","output","surnames.RData"))
lgroups <- unique(surnames$lgroup)

sheets <- gs_title("btech system name generation")

surname_flavor <- gs_read(sheets, "surname additions")[,-4]
city_flavor <- gs_read(sheets, "city additions")[,-4]
new_flavor <- gs_read(sheets, "new additions")[,-4]

#do some checks to make sure everything checks out

#fix some cases for our naming routines
replace_lgroups <- function(flavor_data) {
  flavor_data$lgroup[flavor_data$lgroup=="Hindi"] <- "Indian"
  flavor_data$lgroup[flavor_data$lgroup=="Spanish"] <- "Hispanic"
  flavor_data$lgroup[flavor_data$lgroup=="Portuguese"] <- "Portugese"
  flavor_data$lgroup[flavor_data$lgroup=="Scandinavian"] <- "Scandanavian"
  return(flavor_data)
}

check_flavor_data <- function(flavor_data) {
  not_contain_name <- !grepl("\\[NAME\\]", flavor_data$name)
  if(any(not_contain_name)) {
    warning(paste(flavor_data$name[not_contain_name], "does not contain [NAME]"))
  }
  missing_freq <- is.na(flavor_data$frequency)
  if(any(missing_freq)) {
    warning(paste(flavor_data$name[missing_freq], "does not contain a valid frequency"))
  }
  wrong_lgroup <- !(is.na(flavor_data$lgroup) | flavor_data$lgroup %in% lgroups)
  if(any(wrong_lgroup)) {
    warning(paste(flavor_data$lgroup[wrong_lgroup], "is not a valid language group"))
  }
}

check_flavor_data(surname_flavor <- replace_lgroups(surname_flavor))
check_flavor_data(city_flavor <- replace_lgroups(city_flavor))
check_flavor_data(new_flavor <- replace_lgroups(new_flavor))

save(surname_flavor, city_flavor, new_flavor, file=here("name_generation","output","flavor_data.RData"))

#### Bad Place Names ####

source(here("name_generation","process_place_names.R"))

#### Planet Culture Correspondence ####

sheets <- gs_title("Planet Culture Correspondence")
name_corr <- gs_read(sheets, "name_corr")

#run some checks
missing_iso <- !(name_corr$country_iso %in% places_sample$country)
if(any(missing_iso)) {
  warning(paste(name_corr$id[missing_iso], "does not have a valid country ISO code"))
}
missing_lgroup <- !is.na(name_corr$lgroup) & !(name_corr$lgroup %in% lgroups)
if(any(missing_lgroup)) {
  warning(paste(name_corr$id[missing_lgroup], "does not have a valid language group"))
}
load(here("name_generation","output","myth_sample.RData"))
missing_myth1 <- !is.na(name_corr$myth1) & !(name_corr$myth1 %in% unique(myth_sample$pantheon))
if(any(missing_myth1)) {
  warning(paste(name_corr$id[missing_myth1], "does not have a valid myth pantheon 1"))
}
missing_myth2 <- !is.na(name_corr$myth2) & !(name_corr$myth2 %in% unique(myth_sample$pantheon))
if(any(missing_myth2)) {
  warning(paste(name_corr$id[missing_myth2], "does not have a valid myth pantheon 2"))
}

save(name_corr, file=here("name_generation","output","name_corr.RData"))
