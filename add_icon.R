#read in the systems.xml data and add an icon identifier for every planet

#gas giant - randomly select gasg 
#ice giant - randomly select iceg 
#dwarf terrestrial - randomly select rock 
#giant terrestrial
# if gas giant atmostphere then randomly select gasg 
# otherwise same rules as terrestrials
# terrestrial
# if no atmosphere
#    randomly select rock  with some small chance of hell 
# otherwise check habitability
#    if habitable check water
#    water >= 75, select wet 
#    water >= 20, select green 
#    water >=0, select dry 
#    water = 0, select barren 
#    if not habitable, check temperature 
#       if above a certain level then select greenhouse
#       if not above a certain level, then check water
#          if no water then select barren
#          if water, then check temperature
#             if temperature > 0 then select badwater
#             if temperature < 0 then select frozen

library(xml2)
library(magrittr)
library(rlist)
library(here)
source(here("functions","data_functions.R"))

systems <- read_xml(here("output","systems.xml"))
events <- read_xml(here("output","system_events.xml"))
waystations <- read_xml(here("output","waystations.xml"))
waystation_data <- read.csv(here("input","waystations.csv"))

#get a list of waystation non surface base ids, so we can 
#know they are not habitable
spacestation_ids <- NULL
for(i in 1:xml_length(waystations)) {
  #technical identification
  planet <- xml_children(waystations)[[i]]
  id <- xml_text(xml_find_first(planet, "id"))
  temp <- trimws(as.character(waystation_data[waystation_data$id==id, "type"]))
  if(length(temp)==1 && temp[1] == "orbital") {
    spacestation_ids <- c(spacestation_ids, id)
  }
}

increment_idx <- function(idx, max_length) {
  idx <- idx+1
  if(idx>max_length) {
    #reset the idx
    idx <- 1
  }
  return(idx)
}



for(i in 1:xml_length(systems)) {
  system <- xml_children(systems)[[i]]
  id <- xml_text(xml_find_first(system, "id"))
  x <- as.numeric(xml_text(xml_find_first(system, "xcood")))
  y <- as.numeric(xml_text(xml_find_first(system, "ycood")))
  primary_slot <- as.numeric(xml_text(xml_find_first(system, "primarySlot")))
  planets <- xml_find_all(system, "planet")
  pevent_idx <- as.numeric(xml_text(xml_find_all(xml_find_all(get_system_id(events, id), "planet"), "sysPos")))
  cat(id)
  
  #set up number randomizers here so that we avoid duplicates
  #if for some reason we use them all up, then the increment_idx
  #function will cycle back to beginning
  gasg_sample <- paste("gasg", sample(1:38, 38), sep="")
  gasg_idx <- 1
  iceg_sample <- paste("iceg", sample(1:19, 19), sep="")
  iceg_idx <- 1
  rock_sample <- paste("rock", sample(1:17, 17), sep="")
  rock_idx <- 1
  asteroid_sample <- paste("asteroid", sample(1:3, 3), sep="")
  asteroid_idx <- 1
  hell_sample <- paste("hell", sample(1:37, 37), sep="")
  hell_idx <- 1
  wet_sample <- paste("wet", sample(1:21, 21), sep="")
  wet_idx <- 1
  green_sample <- paste("green", sample(1:57, 57), sep="")
  green_idx <- 1
  dry_sample <- paste("dry", sample(1:27, 27), sep="")
  dry_idx <- 1
  barren_sample <- paste("barren", sample(1:24, 24), sep="")
  barren_idx <- 1
  greenhouse_sample <- paste("greenhouse", sample(1:41, 41), sep="")
  greenhouse_idx <- 1
  frozen_sample <- paste("frozen", sample(1:62, 62), sep="")
  frozen_idx <- 1
  badwater_sample <- paste("badwater", sample(1:45, 45), sep="")
  badwater_idx <- 1
  
  for(planet in planets) {
    type <- xml_text(xml_find_first(planet, "type"))
    pressure <- xml_text(xml_find_first(planet, "pressure"))
    atmosphere <- xml_text(xml_find_first(planet, "atmosphere"))
    water <- xml_text(xml_find_first(planet, "water"))
    temperature <- xml_text(xml_find_first(planet, "temperature"))
    pos <- as.numeric(xml_text(xml_find_first(planet, "sysPos")))

    icon <- "default"
    if(type=="Gas Giant" | (type=="Giant Terrestrial" & pressure=="Very High")) {
      icon <- gasg_sample[gasg_idx]
      gasg_idx <- increment_idx(gasg_idx, length(gasg_sample))
    } else if(type=="Ice Giant") {
      icon <- iceg_sample[iceg_idx]
      iceg_idx <- increment_idx(iceg_idx, length(iceg_sample))
    } else if(type=="Dwarf Terrestrial") {
      icon <- rock_sample[rock_idx]
      rock_idx <- increment_idx(rock_idx, length(rock_sample))
    } else if(type=="Asteroid Belt") {
      icon <- asteroid_sample[asteroid_idx]
      asteroid_idx <- increment_idx(asteroid_idx, length(asteroid_sample))
    } else if(type=="Terrestrial" | type=="Giant Terrestrial") {
      #does it have atmosphere?
      if(pressure=="Vacuum" | pressure=="Trace") {
        #airless rock, but 1 in 100 are hell planets
        if(sample(1:100,1)==1) {
          icon <- hell_sample[hell_idx]
          hell_idx <- increment_idx(hell_idx, length(hell_sample))
        } else {
          icon <- rock_sample[rock_idx]
          rock_idx <- increment_idx(rock_idx, length(rock_sample))
        }
      } else {
        #a bit of a hack but if we ever get any event here, then it is habitable
        #because all planetary events CURRENTLY in database reflect social stuff
        #unless space station which is also checked
        if(pos %in% pevent_idx && !(id %in% spacestation_ids)) {
          if(water>=75) {
            icon <- wet_sample[wet_idx]
            wet_idx <- increment_idx(wet_idx, length(wet_sample))
          } else if(water>=20) {
            icon <- green_sample[green_idx]
            green_idx <- increment_idx(green_idx, length(green_sample))
          } else if(water>0) {
            icon <- dry_sample[dry_idx]
            dry_idx <- increment_idx(dry_idx, length(dry_sample))
          } else {
            icon <- barren_sample[barren_idx]
            barren_idx <- increment_idx(barren_idx, length(barren_sample))
          }
        } else {
          if(temperature>100 & (pressure=="High" | pressure=="Very High")) {
            icon <- greenhouse_sample[greenhouse_idx]
            greenhouse_idx <- increment_idx(greenhouse_idx, length(greenhouse_sample))
          } else {
            if(water>0) {
              if(temperature<0) {
                icon <- frozen_sample[frozen_idx]
                frozen_idx <- increment_idx(frozen_idx, length(frozen_sample))
              } else {
                icon <- badwater_sample[badwater_idx]
                badwater_idx <- increment_idx(badwater_idx, length(badwater_sample))
              }
            } else {
              icon <- barren_sample[barren_idx]
              barren_idx <- increment_idx(barren_idx, length(barren_sample))
            }
          }
        }
      }
    }
    
    #check for Terra because its special
    if(id=="Terra") {
      if(pos==1) {
        icon <- "mercury"
      }
      if(pos==2) {
        icon <- "venus"
      }
      if(pos==3) {
        icon <- "earth"
      }
      if(pos==4) {
        icon <- "mars"
      }
      if(pos==6) {
        icon <- "jupiter"
      }
      if(pos==7) {
        icon <- "saturn"
      }
      if(pos==8) {
        icon <- "uranus"
      }
      if(pos==9) {
        icon <- "neptune"
      }
    }
    
    
    #now redo the way moons are reported
    satellites <- xml_find_all(planet, "satellite")
    rock_sample <- paste("rock", sample(1:17, 17), sep="")
    rock_idx <- 1
    oddmoon_sample <- paste("oddmoon", sample(1:3, 3), sep="")
    oddmoon_idx <- 1
    
    for(satellite in satellites) {
      name <- xml_text(satellite)
      size <- xml_attr(satellite, "size")
      if(size=="medium" & sample(1:2,1)==1) {
        #medium moons may not be nice and spherical
        moon_icon <- oddmoon_sample[oddmoon_idx]
        oddmoon_idx <- increment_idx(oddmoon_idx, length(oddmoon_sample))
      } else {
        moon_icon <- rock_sample[rock_idx]
        rock_idx <- increment_idx(rock_idx, length(rock_sample))
      }
      xml_remove(satellite)
      new_satellite <- xml_add_child(planet, "satellite")
      xml_add_child(new_satellite, "name", name)
      xml_add_child(new_satellite, "size", size)
      xml_add_child(new_satellite, "icon", moon_icon)
    }
    
    #insert the icon tag
    xml_add_child(planet, "icon", icon)
  }
  cat("\n")
}
  
cat(as.character(systems), file = here("output","systems_wicons.xml"))

