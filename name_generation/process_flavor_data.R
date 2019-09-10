library(googlesheets)
library(here)

load(here("name_generation","output","languages.RData"))
lgroups <- unique(languages$lgroup)

#this is needed to authenticate, but will spit an error
gs_ls()

sheets <- gs_title("btech system name generation")

surname_flavor <- gs_read(sheets, "surname additions")[,-4]
city_flavor <- gs_read(sheets, "city additions")[,-4]
new_flavor <- gs_read(sheets, "new additions")[,-4]

#do some checks to make sure everything checks out

check_data <- function(flavor_data) {
  not_contain_name <- !grepl("\\[NAME\\]", flavor_data$name)
  if(any(not_contain_name)) {
    warning(paste(flavor_data$name[not_contain_name], "does not contain [NAME]"))
  }
  missing_freq <- is.na(flavor_data$frequency)
  if(any(missing_freq)) {
    warning(paste(flavor_data$name[missing_freq], "does not contain a valid frequency"))
  }
  wrong_lgroup <- !(is.na(surname_flavor$lgroup) | surname_flavor$lgroup %in% lgroups)
  if(any(wrong_lgroup)) {
    warning(paste(flavor_data$lgroup[wrong_lgroup], "is not a valid language group"))
  }
}

check_data(surname_flavor)
check_data(city_flavor)
check_data(new_flavor)

save(surname_flavor, city_flavor, new_flavor, file=here("name_generation","output","flavor_data.RData"))
