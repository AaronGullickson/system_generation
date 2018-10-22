#This script will read in the original planets XML data and use our system generation
#functions to generate data where it is missing. 

#Eventually this script will also use projection forwards and backwards in time to change
#population and USILR values, but for the moment we are just going to take a static snapshot

library(xml2)
library(magrittr)
library(rlist)
source("system_creation_functions.R")
source("data_functions.R")

planets <- read_xml("output/planets_initial.xml")
events <- read_xml("output/planetevents_initial.xml")
name_changes <- read_xml("input/0999_namechanges.xml")
canon_populations <- read.csv("input/canon_populations.csv", row.names=1)

planet.table <- NULL
target.year <- 3047
target_date <- as.Date(paste(target.year,"01","01",sep="-"))

#prepare the XML systems output
systems <- xml_new_document() %>% xml_add_child("systems")
systems_events <- xml_new_document() %>% xml_add_child("systems")
systems_name_changes <- xml_new_document() %>% xml_add_child("systems")

for(i in 1:xml_length(planets)) {
  
  #### Read in a planet's data ####
  
  #technical identification
  planet <- xml_children(planets)[[i]]
  id <- xml_text(xml_find_first(planet, "id"))
  name <- xml_text(xml_find_first(planet, "name"))
  x <- as.numeric(xml_text(xml_find_first(planet, "xcood")))
  y <- as.numeric(xml_text(xml_find_first(planet, "ycood")))
  
  cat(paste(id,"\n\treading in XML data..."))
  
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
  #First see if it is in the planet data
  faction <- xml_text(xml_find_first(planet, "faction"))
  hpg <- xml_text(xml_find_first(planet, "hpg"))
  sic <- xml_text(xml_find_first(planet, "socioIndustrial"))
  
  #check for faction change events
  faction_table <- get_event_data(events, id, "faction")
  founding_year <- NA
  terran_hegemony <- FALSE
  if(!is.null(faction_table)) {
    founding_year <- get_year(faction_table$date[1])
    #take the first faction in cases of multiple factions
    temp <- get_closest_event(faction_table, target_date)
    if(!is.na(temp)) {
      faction <- strsplit(temp,",")[[1]][1]
    }
    #figure out if this was Terran Hegemony world in 2750
    terran_hegemony <- grepl("(^TH$|TH,|,TH)", 
                             get_closest_event(faction_table, as.Date(paste(2750,"01","01",sep="-"))))
  }
  
  hpg_table <- get_event_data(events, id, "hpg")
  if(!is.null(hpg_table)) {
     temp <- get_closest_event(hpg_table, target_date)
     if(!is.na(temp)) {
       hpg <- temp
     }
  }
  
  sic_table <- get_event_data(events, id, "sic")
  if(!is.null(sic_table)) {
    #SIC must be within 20 years of target year
    temp <- get_closest_event(sic_table, target_date, 
                              as.Date(paste(target_year-20,"01","01",sep="-")))
    if(!is.na(temp)) {
      sic <- temp
    }
  }
  
  desc <- xml_text(xml_find_first(planet, "desc"))
  
  # do some checks, if fail then skip for now
  # TODO: fix problems
  
  #drop if they are missing x or y coordinates (shouldnt happen)
  if(is.na(x) | is.na(y)) {
    cat(paste("ERROR:", id, "is missing an x or y value. Skipping.\n"))
    next
  }
  
  # ignore abandoned places or those with UND or NONE factions (mostly highways which
  # should be in the connector file not here)
  if(is.na(faction) | faction=="UND" | faction=="NONE") {
    next
    cat(paste("ERROR:", id, "has a missing or unknown faction. Skipping.\n"))
  }
  
  #TODO: need to generate data for abandoned planets too
  if(faction=="ABN") {
    cat(paste(id, "is abandoned. Skipping.\n"))
    next
  }

  #drop if they are missing founding year
  if(is.na(founding_year)) {
    next
    cat(paste("ERROR:", id, "has a missing founding year. Skipping.\n"))
  }
  
  cat("done\n\tOrganizing data...")
  
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
  
  ## Fix founding year
  # There was considerable amounts of heaping in founding years because these were
  # determined in many cases by changes between maps with the faction change only 
  # occurring at the latter date. We want to allow these founding dates to vary between
  # the map dates, probably by drawing from uniform distribution. 
  
  #TODO: Ultimately, we will will also need to correct the faction change event and mark
  # it as non-canon
  
  # The following dates showed evidence of heaping and need to be corrected. We ignore
  # some potential small heaping cases for small year intervals.
  
  # 2172 - 131 cases. not from known SUCS map change, but clear heaping, distibute these
   #cases between 2118 and 2172
  # 2200 - 42 cases. not from known SUCS map change, but clear heaping, distribute these cases
  # between 2173 and 2200
  # 2271 - 232 cases. First SUCS map. Distributed these cases between 2118 and 2271. 
  # 2300 - 212 cases. not from known SUCS map change, but clear heaping, distribute these cases 
  # between 2201 and 2300
  # 2317 - 49 cases. SUCS map. Distribute these cases between 2272 and 2317.
  # 2367 - 43 cases. SUCS map. Distribute these cases between 2342 and 2367.
  # 2571 - 238 cases. SUCS map. Distribute these cases between 2368 and 2571.
  # 2750 - 531 cases. SUCS map. Distributed these cases between 2597 and 2750.
  
  #remaining heaps seem to be clans, hanseatic league, and back part of TC, not
  #adjusting them for now. 
  
  if(founding_year==2172) {
    founding_year <- sample(2118:2172, 1)
  }
  if(founding_year==2200) {
    founding_year <- sample(2173:2200, 1)
  }
  if(founding_year==2300) {
    founding_year <- sample(2201:2300, 1)
  }
  if(founding_year==2271) {
    founding_year <- sample(2118:2271, 1)
  }
  if(founding_year==2317) {
    founding_year <- sample(2272:2317, 1)
  }
  if(founding_year==2367) {
    founding_year <- sample(2342:2367, 1)
  }
  if(founding_year==2571) {
    founding_year <- sample(2368:2571, 1)
  }
  if(founding_year==2750) {
    founding_year <- sample(2597:2750, 1)
  }
  
  #read in canon population data
  if(id %in% rownames(canon_populations)) {
    canon_population <- canon_populations[id,]
  } else {
    canon_population <- NULL
  }
  
  cat("done\n\tGenerating base system and colonization data...")
  
  system <- add_colonization(generate_system(star=star), distance_terra, 3047, founding_year,
                             faction_type)
  primary_slot <- which(system$planets$population==max(system$planets$population, na.rm=TRUE))[1]
  
  #### Output XML ####
  
  cat("done\n\tOutputing base data to XML...")
  
  #create a system node
  system_node <- xml_add_child(systems, "system")
  system_event_node <- xml_add_child(systems_events, "system")
  
  xml_add_child(system_node, "id", id)
  xml_add_child(system_node, "name", name)
  xml_add_child(system_node, "xcood", x)
  xml_add_child(system_node, "ycood", y)
  xml_add_child(system_event_node, "id", id)
  
  if(is.null(star)) {
    xml_add_child(system_node, "spectralType", 
                  system$star)
  } else {
    xml_add_child(system_node, "spectralType", 
                  system$star, source="canon")
  }
  
  xml_add_child(system_node, "primarySlot", primary_slot)
  
  #now cycle through planets and create planet nodes
  for(j in 1:nrow(system$planets)) {
    cat(paste("\n",j))
    planet <- system$planets[j,]
    cat("\t\t\tplanet info")
    
    if(j==primary_slot) {
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
    xml_add_child(planet_node, "sysPos", j)
    
    if(j==primary_slot & !is.na(pressure)) {
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
    if(j==primary_slot & !is.na(gravity)) {
      gravity_multiplier <- gravity/planet$gravity
      xml_add_child(planet_node, "gravity", 
                    gravity, 
                    source="canon")
    } else if(!is.na(planet$gravity)) {
      xml_add_child(planet_node, "gravity", 
                    planet$gravity)
    }
    
    if(j==primary_slot & !is.na(temperature)) {
      xml_add_child(planet_node, "temperature", 
                    temperature, 
                    source="canon")
    }
    else if(!is.na(planet$temperature)) {
      xml_add_child(planet_node, "temperature", 
                    planet$temperature)
    }
    
    if(j==primary_slot & !is.na(water)) {
      xml_add_child(planet_node, "water", 
                    water, 
                    source="canon")
    }
    else if(!is.na(planet$water)) {
      xml_add_child(planet_node, "water", 
                    planet$water)
    }
    
    if(j==primary_slot & !is.na(life)) {
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
    
    if(j==primary_slot & !is.na(desc)) {
      xml_add_child(planet_node, "desc", 
                    desc, 
                    source="canon")
    }
    
    if(j==primary_slot & !is.null(continents)) {
      for(continent in continents) {
        xml_add_child(planet_node, "landMass", 
                      continent, 
                      source="canon")
      }
    } else if(!is.na(planet$continents) & planet$continents>0) {
      #pick random one to have capital
      capital <- sample(1:planet$continents, 1)
      for(k in 1:planet$continents) {
        landmass_name <- "Unnamed Landmass"
        if(j==capital) {
          landmass_name <- "Unnamed Landmass (Unnamed Capital)"
        }
        xml_add_child(planet_node, "landMass",
                      landmass_name)
      }
    }
    
    cat("\tmoon info")
    #we will detail all moons except small moons where we will just list number
    #assume named moons are never small
    if(j==primary_slot & !is.null(moons)) {
      for(moon in moons) {
        moon_size <- c("giant",rep("large",5),rep("medium",9))[sample(1:15,1)]
        xml_add_child(planet_node, "satellite", size=moon_size,
                      moon, 
                      source="canon")
      }
    } else {
      if(planet$moons_giant>0) {
        for(k in 1:planet$moons_giant) {
          xml_add_child(planet_node, "satellite", size="giant",
                        "Unnamed Moon")
        }
      }
      if(planet$moons_large>0) {
        for(k in 1:planet$moons_large) {
          xml_add_child(planet_node, "satellite", size="large",
                        "Unnamed Moon")
        }
      }
      if(planet$moons_medium>0) {
        for(k in 1:planet$moons_medium) {
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
    #figure out where to add these events
    
    #population
    cat("\tpopulation projection")
    if(!is.na(planet$population)) {
      
      current_planet_event_node <- xml_add_child(system_event_node, "planet")
      xml_add_child(current_planet_event_node, "sysPos", j)
      
      border_distance <- distance_to_border(x, y, faction)
      p2750 <- NULL
      p3025 <- NULL
      p3067 <- NULL
      p3079 <- NULL
      p3145 <- NULL
      if(!is.null(canon_population) & j==primary_slot) {
        if(!is.na(canon_population$X2750)) {
          p2750 <- canon_population$X2750
        }
        if(!is.na(canon_population$X3025)) {
          p3025 <- canon_population$X3025
        }
        if(!is.na(canon_population$X3067)) {
          p3067 <- canon_population$X3067
        }
        if(!is.na(canon_population$X3079)) {
          p3079 <- canon_population$X3079
        }
        if(!is.na(canon_population$X3145)) {
          p3145 <- canon_population$X3145
        }
      }
      pop <- project_population(planet$population, founding_year, faction_type,
                                border_distance, planet$agriculture, 
                                terran_hegemony = terran_hegemony,
                                p2750 = p2750, p3025 = p3025, p3067 = p3067,
                                p3079 = p3079, p3145 = p3145)
      #collect population values at 10 year intervals, plus the starting and final values.
      first_census_year <- founding_year + 10*(ceiling(founding_year/10)-(founding_year/10))
      last_year <- as.numeric(names(pop)[length(pop)])
      last_census_year <- 10*floor(last_year/10)
      #TODO: not safe if colony quickly died
      census_years <- c(founding_year, seq(from=first_census_year, to=last_census_year, by=10), last_year)
      census_pop <- round(pop[paste(census_years)])
      for(k in 1:length(census_years)) {
        #I don't know why its popping up a node for the root document, but
        #the [[1]] ensures that we only grab the correct node
        pop_event <- xml_add_child(current_planet_event_node, "event")
        xml_add_child(pop_event, "date", paste(census_years[k],"01","01",sep="-"))
        xml_add_child(pop_event, "population", census_pop[k], source="noncanon")
      }
      #add in canon populations
      if(!is.null(p2750)) {
        pop_event <- xml_add_child(current_planet_event_node, "event")
        sl_peak <- 2785
        if(terran_hegemony) {
          sl_peak <- 2767
        }
        xml_add_child(pop_event, "date", paste(sl_peak,"01","01",sep="-"))
        xml_add_child(pop_event, "population", p2750, source="canon")
      }
      if(!is.null(p3025)) {
        pop_event <- xml_add_child(current_planet_event_node, "event")
        xml_add_child(pop_event, "date", paste(3025,"01","01",sep="-"))
        xml_add_child(pop_event, "population", p3025, source="canon")
      }
      if(!is.null(p3067)) {
        pop_event <- xml_add_child(current_planet_event_node, "event")
        xml_add_child(pop_event, "date", paste(3067,"01","01",sep="-"))
        xml_add_child(pop_event, "population", p3067, source="canon")
      }
      if(!is.null(p3079)) {
        pop_event <- xml_add_child(current_planet_event_node, "event")
        xml_add_child(pop_event, "date", paste(3079,"01","01",sep="-"))
        xml_add_child(pop_event, "population", p3079, source="canon")
      }
      if(!is.null(p3145)) {
        pop_event <- xml_add_child(current_planet_event_node, "event")
        xml_add_child(pop_event, "date", paste(3145,"01","01",sep="-"))
        xml_add_child(pop_event, "population", p3145, source="canon")
      }
    }
  }
  
  #get existing planet events and move them over
  planet_events <- get_planet_id(events, id)
  primary_planet_events <- get_planet_syspos(system_event_node, primary_slot)
  for(event in xml_find_all(planet_events, "event")) {
    #all events except nadir and zenith charge should go into planet I think
    if(grepl("noncanon", xml_text(event))) {
      next
    }
    if(any(grepl("nadirCharge",xml_contents(event))) | 
       any(grepl("zenithCharge",xml_contents(event)))) {
      xml_add_child(system_event_node, event)
    } else {
      xml_add_child(primary_planet_events, event)
    }
  }
  
  #get name changes and move them over
  planet_name_changes <- get_planet_id(name_changes, id)
  if(length(planet_name_changes)>0) {
    system_name_node <- xml_add_child(systems_name_changes, "system")
    xml_add_child(system_name_node, "id", id)
    primary_planet_name_node <- xml_add_child(system_name_node, "planet")
    xml_add_child(primary_planet_name_node, "sysPos", primary_slot)
    for(planet_name_change in xml_find_all(planet_name_changes, "event")) {
      xml_add_child(primary_planet_name_node, event)
    }
  }
    
  cat("\n\tdone\n")
}

cat(as.character(systems), file = "systems.xml")
cat(as.character(systems_events), file = "system_events.xml")
cat(as.character(systems_name_changes), file = "system_namechanges.xml")

#TODO: a similar for-loop for connectors but no need to force habitation

