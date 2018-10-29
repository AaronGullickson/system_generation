#What do we need to name?
# landmasses
# capital city
# moons
# planets

#What resources will we draw on to grab a random name?
# actual world place names (with some probability of getting the occasional "New")
# mythological names
# surnames, perhaps applied with some kind of desriptor (e.g. Jackson's Mistake))

#each of the types should probably have a different distribution. For example,
#planets and moons should be more likely to pull mythological names, while cities and 
#landmasses should be more likely to be named after actual places or by surname

#There should be some relatively high but not perfect consistency. So if you name 
#one planet after a god, likely you will name other planets after a god in the same
#pantheon. If you name one landmass after a place in a given country, more likely to
#name other landmasses by other places in that country, etc. 

#load the data for the random draws in at the base of this script so it doesn't need
#to be loaded every time the function is loaded

#because we will be sampling with some weights, use that library that samples with weights
#rather than base sample, cause its super slow.

library(wrswoR)
load("output/myth_sample.RData")

generate_name <- function() {
  
  
  
}


sample_myth_name <- function(p,n=1) {
  sample_pantheon <- subset(myth_sample, pantheon==p)
  if(nrow(sample_pantheon)==0) {
    warning(paste("No pantheon named",p))
    return(NA)
  }
  sample_idx <- sample_int_expj(nrow(sample_pantheon),
                                n,
                                prob=sample_pantheon$popularity)
  return(as.character(sample_pantheon$name[sample_idx]))
}
