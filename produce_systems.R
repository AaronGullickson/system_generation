#This script will read in the original planets XML data and use our system generation
#functions to generate data where it is missing. 

#Eventually this script will also use projection forwards and backwards in time to change
#population and USILR values, but for the moment we are just going to take a static snapshot

library(xml2)
library(magrittr)
source("system_creation_functions.R")

#TODO: read in planet events and and store so I can use this below
#and output a final planet events file

planets <- read_xml("data/planets.xml")
planet.table <- NULL
target.year <- 3047

systems <- xml_new_document() %>% xml_add_child("systems")

for(i in 1:xml_length(planets)) {
  
  #### Read in a planet's data ####
  
  #technical identification
  planet <- xml_children(planets)[[i]]
  id <- xml_text(xml_find_first(planet, "id"))
  name <- xml_text(xml_find_first(planet, "name"))
  x <- as.numeric(xml_text(xml_find_first(planet, "xcood")))
  y <- as.numeric(xml_text(xml_find_first(planet, "ycood")))
  
  #system information
  star <- xml_text(xml_find_first(planet, "spectralType"))
  sys_pos <- as.numeric(xml_text(xml_find_first(planet, "sysPos")))
  
  #planetary information - is this all Canon? 
  life <- xml_text(xml_find_first(planet, "lifeForm"))
  water <- xml_text(xml_find_first(planet, "percentWater"))
  gravity <- as.numeric(xml_text(xml_find_first(planet, "gravity")))
  temperature <- xml_text(xml_find_first(planet, "temperature"))
  pressure <- factor(xml_text(xml_find_first(planet, "pressure")),
                     levels=0:5,
                     labels=c("Vacuum","Trace","Low","Normal","High","Very High"))
  landmasses <- xml_find_all(planet, "landMass")
  continents <- NULL
  for(landmass in landmasses) {
    continents <- c(continents, xml_text(landmass))
  }
  moons <- NULL
  satellites <- xml_find_all(planet, "satellite")
  for(satellite in satellites) {
    moons <- c(moons, xml_text(satellite))
  }
  
  
  #social information
  faction <- xml_text(xml_find_first(planet, "faction"))
  hpg <- xml_text(xml_find_first(planet, "hpg"))
  sic <- xml_text(xml_find_first(planet, "socioIndustrial"))
  founding_year <- NA
  events <- xml_find_all(planet, "event")
  first_faction_found <- FALSE
  for(event in events) {
    year <- as.numeric(substr(xml_text(xml_find_first(event, "date")),1,4))
    if(year>target.year) {
      next
    }
    if(!is.na(xml_find_first(event, "faction"))) {
      faction <- xml_text(xml_find_first(event, "faction"))
      #for now if faction is multiple just take the first
      faction <- strsplit(faction,",")[[1]][1]
      #if this is the first time we hit a faction, then 
      #this is the founding year
      if(!first_faction_found) {
        founding_year <- year
        first_faction_found <- TRUE
      }
    }
    if(!is.na(xml_find_first(event, "hpg"))) {
      hpg <- xml_text(xml_find_first(event, "hpg"))
    }
    #only take changes to default SIC within 30 years of target date
    if(!is.na(xml_find_first(event, "socioIndustrial")) & (target.year-year)<=30) {
      sic <- xml_text(xml_find_first(event, "socioIndustrial"))
    }
  }
  desc <- xml_text(xml_find_first(planet, "desc"))
  
  # do some checks, if fail then skip for now
  # TODO: fix problems
  # ignore abandoned places or those with UND or NONE factions (mostly highways which
  # should be in the connector file not here)
  if(faction=="ABN" | faction=="UND" | faction=="NONE") {
    next
  }
  #drop if they are missing x or y coordinates (shouldnt happen)
  if(is.na(x) | is.na(y)) {
    next
  }
  #drop if they are missing founding year
  if(is.na(founding_year)) {
    next
  }
  
  #### Generate the System ####
  
  distance_terra <- sqrt(x^2+y^2)
  #TODO: better way to get faction type
  faction_type <- "Minor"
  if(faction=="CC" | faction=="DC" | faction=="FS" | faction=="LA" |
     faction=="FWL" |
     faction=="FC" | faction=="FRR" | faction=="SIC" | faction=="MERC" |
     faction=="CS") {
    faction_type <- "IS"
  }
  if(faction=="CBS" | faction=="CB" | faction=="CCC" | faction=="CCO" |
     faction=="CDS" | faction=="CFM" | faction=="CGB" | faction=="CGS" |
     faction=="CGS" | faction=="CHH" | faction=="CIH" | faction=="CJF" | 
     faction=="CMG" | faction=="CNC" | faction=="CSJ" | faction=="CSR" | 
     faction=="CSA" | faction=="CSV" | faction=="CSL" | faction=="CWI" |
     faction=="CW" | faction=="CLAN") {
    faction_type <- "Clan"
  }
  if(faction=="TC" | faction=="OA" | faction=="MOC" | faction=="MH") {
    faction_type <- "Periphery"
  }
  faction_type <- factor(faction_type)
  
  #Check for bad stellar types and correct
  if(!is.na(star)) {
    spectral_class <- substr(star,1,1)
    subtype <- as.numeric(substr(star,2,2))
    star_size <- substring(star, 3)
    
    if(is.na(star_size) | !(spectral_class %in% c("A","B","F","G","K","M")) | 
       is.na(subtype) | subtype <0 | subtype>9 |
       is.na(star_size) | nchar(star_size)==0 | 
       !(star_size %in% c("Ia","Ib","II","III","IV","V","VI","VII"))) {
      star <- NULL
    }
    
    #TODO: until we hack it M6V and M9V also produce no habitable planets
    if(!is.null(star)) {
      if(star=="M9V" | star=="M6V") {
        star <- NULL
      }
    }
    
  } else {
    star <- NULL
  }

  #FIXME: This is a bit of a hack, but non-canon star types were generated for pretty much every
  #system at some point. To figure out which are actually canon, we will use sys_pos 
  if(is.na(sys_pos)) {
    star <- NULL
  }
  
  system <- add_colonization(generate_system(star=star), distance_terra, 3047, founding_year,
                             faction_type)
  primary_slot <- which(system$planets$population==max(system$planets$population, na.rm=TRUE))[1]
  
  #### Output XML ####
  
  #create a system node
  system_node <- xml_add_child(systems, "system")
  
  xml_add_child(system_node, "id", id)
  xml_add_child(system_node, "name", name)
  
  
  if(is.null(star)) {
    xml_add_child(system_node, "spectralType", 
                  system$star)
  } else {
    xml_add_child(system_node, "spectralType", 
                  system$star, source="canon")
  }
  
  #now cycle through planets and create planet nodes
  for(i in 1:nrow(system$planets)) {
    planet <- system$planets[i,]
    if(i==primary_slot) {
      planet_node <- xml_add_child(system_node, "planet", primary="TRUE", 
                                   source="canon")
      xml_add_child(planet_node, "name", name, source="canon")
    } else {
      planet_node <- xml_add_child(system_node, "planet")
      xml_add_child(planet_node, "name", "Unnamed Planet")
    }
    
    xml_add_child(planet_node, "type", 
                  as.character(planet$type))
    xml_add_child(planet_node, "orbitalDist", 
                  planet$orbital_dis)
    
    #TODO: asteroid belts should not count for system position
    #also what to do we do if this does not match canon system position?
    xml_add_child(planet_node, "sysPos", 
                  i)
    
    if(i==primary_slot & !is.na(pressure)) {
      xml_add_child(planet_node, "pressure", 
                    as.character(pressure), 
                    source="canon")
    } else if(!is.na(planet$pressure)) {
      xml_add_child(planet_node, "pressure", 
                    as.character(planet$pressure))
    }
    
    if(!is.na(planet$atmosphere)) {
      xml_add_child(planet_node, "atmosphere", 
                    as.character(planet$atmosphere))
    }
    
    if(!is.na(planet$composition)) {
      xml_add_child(planet_node, "composition", 
                    as.character(planet$composition))
    }
   
    #if we adjust gravity then we will need to adjust diameter 
    #and density to. We will just multiply each by the square 
    #root of the proportional difference. This should also 
    #maintain correct orbital and escape velocity
    gravity_multiplier <- 1
    if(i==primary_slot & !is.na(gravity)) {
      gravity_multiplier <- gravity/planet$gravity
      xml_add_child(planet_node, "gravity", 
                    gravity, 
                    source="canon")
    } else if(!is.na(planet$gravity)) {
      xml_add_child(planet_node, "gravity", 
                    planet$gravity)
    }
    
    if(i==primary_slot & !is.na(temperature)) {
      xml_add_child(planet_node, "temperature", 
                    temperature, 
                    source="canon")
    }
    else if(!is.na(planet$temperature)) {
      xml_add_child(planet_node, "temperature", 
                    planet$temperature)
    }
    
    if(i==primary_slot & !is.na(water)) {
      xml_add_child(planet_node, "water", 
                    water, 
                    source="canon")
    }
    else if(!is.na(planet$water)) {
      xml_add_child(planet_node, "water", 
                    planet$water)
    }
    
    if(i==primary_slot & !is.na(life)) {
      xml_add_child(planet_node, "life", 
                    life, 
                    source="canon")
    }
    else if(!is.na(planet$life)) {
      xml_add_child(planet_node, "life", 
                    as.character(planet$life))
    }
    
    if(!is.na(planet$day_length)) {
      xml_add_child(planet_node, "dayLength", 
                    planet$day_length)
    }
    
    if(!is.na(planet$year_length)) {
      xml_add_child(planet_node, "yearLength", 
                    planet$year_length)
    }
    
    if(!is.na(planet$diameter)) {
      xml_add_child(planet_node, "diameter", 
                    sqrt(gravity_multiplier) * planet$diameter)
    }
    
    if(!is.na(planet$density)) {
      xml_add_child(planet_node, "density", 
                    sqrt(gravity_multiplier) * planet$density)
    }
    
    #TODO: population, SIC, and HPG data will eventually all go in 
    #planetevents.xml.
    # if(!is.na(planet$population)) {
    #   xml_add_child(planet_node, "population", 
    #                 planet$population, 
    #                 source="noncanon")
    # }
    # if(i==primary_slot & !is.na(sic)) {
    #   xml_add_child(planet_node, "socioIndustrial", 
    #                 sic, 
    #                 source="canon")
    # }
    # else if(!is.na(planet$tech)) {
    #   xml_add_child(planet_node, "socioIndustrial", 
    #                 paste(c(as.character(planet$tech), 
    #                         as.character(planet$industry), 
    #                         as.character(planet$output), 
    #                         as.character(planet$raw), 
    #                         as.character(planet$agriculture)),
    #                       collapse="-"), 
    #                 source="noncanon")
    # }
    
    if(i==primary_slot & !is.na(desc)) {
      xml_add_child(planet_node, "desc", 
                    desc, 
                    source="canon")
    }
    
    if(i==primary_slot & !is.null(continents)) {
      for(continent in continents) {
        xml_add_child(planet_node, "landMass", 
                      continent, 
                      source="canon")
      }
    } else if(!is.na(planet$continents) & planet$continents>0) {
      #pick random one to have capital
      capital <- sample(1:planet$continents, 1)
      for(i in 1:planet$continents) {
        landmass_name <- "Unnamed Landmass"
        if(i==capital) {
          landmass_name <- "Unnamed Landmass (Unnamed Capital)"
        }
        xml_add_child(planet_node, "landMass",
                      landmass_name)
      }
    }
    
    #we will detail all moons except small moons where we will just list number
    #assume named moons are never small
    if(i==primary_slot & !is.null(moons)) {
      for(moon in moons) {
        moon_size <- c("giant",rep("large",5),rep("medium",9))[sample(1:15),1]
        xml_add_child(planet_node, "satellite", size=moon_size,
                      moon, 
                      source="canon")
      }
    } else {
      if(planet$moons_giant>0) {
        for(i in 1:planet$moons_giant) {
          xml_add_child(planet_node, "satellite", size="giant",
                        "Unnamed Moon")
        }
      }
      if(planet$moons_large>0) {
        for(i in 1:planet$moons_large) {
          xml_add_child(planet_node, "satellite", size="large",
                        "Unnamed Moon")
        }
      }
      if(planet$moons_medium>0) {
        for(i in 1:planet$moons_medium) {
          xml_add_child(planet_node, "satellite", size="medium",
                        "Unnamed Moon")
        }
      }
      if(planet$moons_small>0) {
        xml_add_child(planet_node, "smallMoons",
                      planet$moons_small)
      }
    }
    
    #### Project Social Data in Time ####
    
    #TODO: Do this. it will be added to planet events rather than planets
    
  }

}

#TODO: a similar for-loop for connectors but no need to force habitation

cat(as.character(systems), file = "test_systems.xml")
