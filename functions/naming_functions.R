#What do we need to name?
# landmasses
# capital city
# moons
# planets

#What resources will we draw on to grab a random name?
# actual world place names (with some probability of getting the occasional "New")
# mythological names TODO: add mythological places to this list
# surnames, perhaps applied with some kind of desriptor (e.g. Jackson's Mistake))
# TODO: Some sort of numeric coding for things that they never bothered to name
# TODO: a grab bag of somewhat random easter egg names

#each of the types should probably have a different distribution. For example,
#planets and moons should be more likely to pull mythological names, while cities and 
#landmasses should be more likely to be named after actual places or by surname

#There should be some relatively high but not perfect consistency. So if you name 
#one planet after a god, likely you will name other planets after a god in the same
#pantheon. If you name one landmass after a place in a given country, more likely to
#name other landmasses by other places in that country, etc. 

#load the data for the random draws in at the base of this script so it doesn't need
#to be loaded every time the function is loaded

#I will need to set up some data matrices that allows me to convert country id codes
#into likely mythological pantheons and surname groups

#some chance for moons to be just named by a number of something. Maybe some chance for 
#continents to be named like this as well (e.g. "One" and "Two"; "A" and "B")

#basic set up
# read in country code for a given planet
# check to see if planet is named by numerals. if so, then just assign all planets the 
# same numeral system
# Cycle through and name planets based on some probability of being named in a certain
# way. Based on first method chosen, give some higher probability to continue using this
# method.
# Within each planet, cycle through and name moons. Probably a higher probability of naming
# in the same way as the planet
# Landmasses similarly and capital city should be tied in, but may have different probs. 



library(wrswoR)
library(here)
load(here("name_generation","output","myth_sample.RData"))
load(here("name_generation","output","places_sample.RData"))

generate_system_names(system) {
  
}

generate_name <- function() {
  
  
  
}


sample_place_name <- function(c, n=1) {
  sample_country <- subset(places_sample, country==c)
  if(nrow(sample_country)==0) {
    warning(paste("No country named",c))
    return(NA)
  }
  if(n > nrow(sample_country)) {
    n <- nrow(sample_country)
  }
  sample_idx <- sample_int_expj(nrow(sample_country),
                                n,
                                prob=sample_country$weight)
  return(as.character(sample_country$name[sample_idx]))
}

sample_myth_name <- function(p, n=1) {
  sample_pantheon <- subset(myth_sample, pantheon==p)
  if(nrow(sample_pantheon)==0) {
    warning(paste("No pantheon named",p))
    return(NA)
  }
  if(n > nrow(sample_pantheon)) {
    n <- nrow(sample_pantheon)
  }
  sample_idx <- sample_int_expj(nrow(sample_pantheon),
                                n,
                                prob=sample_pantheon$popularity)
  return(as.character(sample_pantheon$name[sample_idx]))
}
