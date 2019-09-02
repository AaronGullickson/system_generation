#This script will read in the original planets XML data and use our system generation
#functions to generate data where it is missing. 

#Eventually this script will also use projection forwards and backwards in time to change
#population and USILR values, but for the moment we are just going to take a static snapshot

library(xml2)
library(magrittr)
library(dplyr)
library(rlist)
library(here)
library(tibble)
source(here("functions","system_creation_functions.R"))
source(here("functions","data_functions.R"))
source(here("functions","naming_functions.R"))
source(here("functions","network_functions.R"))


planets <- read_xml(here("output","planets_initial.xml"))
events <- read_xml(here("output","planetevents_initial.xml"))
name_changes <- read_xml(here("input","0999_namechanges.xml"))
canon_populations <- read.csv(here("input","canon_populations.csv"), row.names=1)

planet.table <- NULL
target.year <- 3047
target_date <- as.Date(paste(target.year,"01","01",sep="-"))

hpg_data <- data.frame(id=character(),
                       x=numeric(),
                       y=numeric(),
                       hpg=character(),
                       faction_type=character(),
                       tech=character(),
                       pop=numeric(),
                       founding_year=numeric(),
                       canon=logical())

#all events will now go into the master event table and then
#be processed after the initial looping. This is because MHQ
#doesn't allow for separate events with the same date and will
#just overwrite earlier ones with later ones. So we need to 
#collect all events before writing them to identify ones with the
#same date. This is especially important for the founding date because
#that will always have faction, population, and SIC.
event_table <- tibble(id=character(),
                      sys_pos=numeric(),
                      date=character(),
                      etype=character(),
                      event=character(),
                      canon=logical())

#prepare the XML systems output
systems <- xml_new_document() %>% xml_add_child("systems")
systems_events <- xml_new_document() %>% xml_add_child("systems")
systems_name_changes <- xml_new_document() %>% xml_add_child("systems")

small_sample <- sample(1:xml_length(planets), 50)

for(i in 1:xml_length(planets)) {
#for(i in small_sample) {
  
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
    #TODO: turn allow_later on to get post 3047 foundings, but wait until 
    #HPG is worked out
    #temp <- get_closest_event(faction_table, target_date, allow_later = TRUE)
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
  
  sic_table <- get_event_data(events, id, "socioIndustrial")
  if(!is.null(sic_table)) {
    #SIC must be between 3045 and 3080 to use
    temp <- get_closest_event(sic_table, as.Date(paste(3080,"01","01",sep="-")), 
                              as.Date(paste(3045,"01","01",sep="-")))
    if(!is.na(temp)) {
      sic <- temp
    }
  }
  
  desc <- xml_text(xml_find_first(planet, "desc"))
  
  # do some checks, if fail then skip for now
  # TODO: turn these into warnings so that we can see them at the end of sourcing
  
  #drop if they are missing x or y coordinates (shouldnt happen)
  if(is.na(x) | is.na(y)) {
    cat(paste("ERROR:", id, "is missing an x or y value. Skipping.\n"))
    next
  }
  
  # ignore abandoned places or those with UND or NONE factions (mostly highways which
  # should be in the connector file not here)
  if(is.na(faction) | faction=="UND" | faction=="NONE") {
    #TODO: I am losing some cases here of systems that were founded after the 
    #date I am checking on, so I should probably always take the first faction in the 
    #faction table in these cases even if it is after
    cat(paste("ERROR:", id, "has a missing or unknown faction. Skipping.\n"))
    next
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
  #this would be easier to do with an %in% but not going to mess with it now
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
  founding_year_canon <- TRUE
  if(founding_year==2172) {
    founding_year <- sample(2118:2172, 1)
    founding_year_canon <- FALSE
  }
  if(founding_year==2200) {
    founding_year <- sample(2173:2200, 1)
    founding_year_canon <- FALSE
  }
  if(founding_year==2300) {
    founding_year <- sample(2201:2300, 1)
    founding_year_canon <- FALSE
  }
  if(founding_year==2271) {
    founding_year <- sample(2118:2271, 1)
    founding_year_canon <- FALSE
  }
  if(founding_year==2317) {
    founding_year <- sample(2272:2317, 1)
    founding_year_canon <- FALSE
  }
  if(founding_year==2367) {
    founding_year <- sample(2342:2367, 1)
    founding_year_canon <- FALSE
  }
  if(founding_year==2571) {
    founding_year <- sample(2368:2571, 1)
    founding_year_canon <- FALSE
  }
  if(founding_year==2750) {
    founding_year <- sample(2597:2750, 1)
    founding_year_canon <- FALSE
  }
  
  #read in canon population data
  if(id %in% rownames(canon_populations)) {
    canon_population <- canon_populations[id,]
  } else {
    canon_population <- NULL
  }
  
  cat("done\n\tGenerating base system and colonization data...")
  
  system <- generate_system_names(add_colonization(generate_system(star=star), 
                                                   distance_terra, 3047, founding_year,
                                                   faction_type), 
                                  id)
  primary_slot <- which(system$planets$population==max(system$planets$population, na.rm=TRUE))[1]
  
  #### Output XML ####
  
  cat("done\n\tOutputing base data to XML...")
  
  #create a system node
  system_node <- xml_add_child(systems, "system")
  system_event_node <- xml_add_child(systems_events, "system")
  
  xml_add_child(system_node, "id", id)
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

  # project recharge stations and write to event data
  # sys_pos is zero for system-wide events
  if(system$recharge$nadir) {
    event_table <- event_table %>% 
      bind_rows(tibble(id=as.character(id),
                       sys_pos=0,
                       date=paste(founding_year,"01","01",sep="-"),
                       etype="rechargeNadir",
                       event=paste(system$recharge$nadir),
                       canon=FALSE))
  }
  if(system$recharge$zenith) {
    event_table <- event_table %>% 
      bind_rows(tibble(id=as.character(id),
                       sys_pos=0,
                       date=paste(founding_year,"01","01",sep="-"),
                       etype="rechargeZenith",
                       event=paste(system$recharge$zenith),
                       canon=FALSE))
  }
  
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
      xml_add_child(planet_node, "name", system$planets$name[j])
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
      continent_names <- strsplit(planet$continent_names, ",")[[1]]
      for(k in 1:planet$continents) {
        landmass_name <- continent_names[k]
        if(k==capital & !is.na(planet$population)) {
          landmass_name <- paste(continent_names[k], " (", planet$capitol_name, ")", sep="")
        }
        xml_add_child(planet_node, "landMass",
                      landmass_name)
      }
    }
    
    cat("\tmoon info")
    #we will detail all moons except small moons where we will just list number
    #assume named moons are never small
    if(!is.na(planet$moon_names)) {
      moon_names <- strsplit(planet$moon_names, ",")[[1]]
    }
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
                        moon_names[1])
          moon_names <- moon_names[-1]
        }
      }
      if(planet$moons_large>0) {
        for(k in 1:planet$moons_large) {
          xml_add_child(planet_node, "satellite", size="large",
                        moon_names[1])
          moon_names <- moon_names[-1]
        }
      }
      if(planet$moons_medium>0) {
        for(k in 1:planet$moons_medium) {
          xml_add_child(planet_node, "satellite", size="medium",
                        moon_names[1])
          moon_names <- moon_names[-1]
        }
      }
      if(planet$moons_small>0) {
        xml_add_child(planet_node, "smallMoons",
                      planet$moons_small)
      }
    }
        
    #### Project Social Data in Time ####
    #figure out where to add these events
    
    if(!is.na(planet$population)) {
      
      cat("\tprojections")
      
      #TODO: add this back into the new way events are written to XML
      #if(founding_year_canon) {
        #xml_add_child(current_planet_event_node, "foundYear", paste(founding_year))
      #}  else {
        #xml_add_child(current_planet_event_node, "foundYear", paste(founding_year),
        #              source="noncanon")
      #}
      
      if(!is.null(faction_table)) {
        faction_table$date[1] <- paste(founding_year,"01","01",sep="-")
        for(i in 1:nrow(faction_table)) {
          event_table <- event_table %>% 
            bind_rows(tibble(id=as.character(id),
                             sys_pos=j,
                             date=as.character(faction_table$date[i]),
                             etype="faction",
                             event=as.character(faction_table$event[i]),
                             canon=TRUE))
        }
      }
      
      border_distance <- distance_to_border(x, y, faction)
      
      #population
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
      event_table <- event_table %>% 
        bind_rows(tibble(id=as.character(id),
             sys_pos=j,
             date=paste(census_years,"01","01",sep="-"),
             etype="population",
             event=paste(census_pop),
             canon=FALSE))
      
      #add in canon populations
      if(!is.null(p2750)) {
        sl_peak <- 2785
        if(terran_hegemony) {
          sl_peak <- 2767
        }
        event_table <- event_table %>% 
          bind_rows(tibble(id=as.character(id),
                           sys_pos=j,
                           date=paste(sl_peak,"01","01",sep="-"),
                           etype="population",
                           event=paste(p2750),
                           canon=FALSE))
      }
      if(!is.null(p3025)) {
        event_table <- event_table %>% 
          bind_rows(tibble(id=as.character(id),
                           sys_pos=j,
                           date=paste(3025,"01","01",sep="-"),
                           etype="population",
                           event=paste(p3025),
                           canon=FALSE))
      }
      if(!is.null(p3067)) {
        event_table <- event_table %>% 
          bind_rows(tibble(id=as.character(id),
                           sys_pos=j,
                           date=paste(3067,"01","01",sep="-"),
                           etype="population",
                           event=paste(p3067),
                           canon=FALSE))
      }
      if(!is.null(p3079)) {
        event_table <- event_table %>% 
          bind_rows(tibble(id=as.character(id),
                           sys_pos=j,
                           date=paste(3079,"01","01",sep="-"),
                           etype="population",
                           event=paste(p3079),
                           canon=FALSE))
      }
      if(!is.null(p3145)) {
        event_table <- event_table %>% 
          bind_rows(tibble(id=as.character(id),
                           sys_pos=j,
                           date=paste(3145,"01","01",sep="-"),
                           etype="population",
                           event=paste(p3145),
                           canon=FALSE))
      }
      
      #SIC Codes
      tech <- planet$tech
      industry <- planet$industry
      raw <- planet$raw
      output <- planet$output
      agriculture <- planet$agriculture
      if(!is.na(sic)) {
        canon_sics <- separate_sics(sic)
        tech <- canon_sics$tech
        industry <- canon_sics$industry
        raw <- canon_sics$raw
        output <- canon_sics$output
        agriculture <- canon_sics$agriculture
      }
      sics_projections <- project_sics(tech, industry, raw, output, agriculture, 
                                       founding_year, pop, faction_type)
      event_table <- event_table %>% 
        bind_rows(tibble(id=as.character(id),
                         sys_pos=j,
                         date=paste(sics_projections$year,"01","01",sep="-"),
                         etype="socioIndustrial",
                         event=paste(sics_projections$sics),
                         canon=!is.na(sic) & 
                           (sics_projections$year>=3040 & sics_projections$year<3050)))
      
      # HPG - We need to do some extra work below to make sure the 
      # first circuit is connected so for the moment, we just want
      # to build a dataset of HPG information for later
      if((!is.na(planet$hpg) | !is.na(hpg)) & j==primary_slot) {
        canon <- TRUE
        if(is.na(hpg)) {
          hpg <- planet$hpg
          canon <- FALSE
        }
        hpg_data <- rbind(hpg_data,
                          data.frame(id=as.character(id),
                                     x=as.numeric(x),
                                     y=as.numeric(y),
                                     hpg=as.character(hpg),
                                     faction_type=as.character(faction_type),
                                     tech=as.character(tech),
                                     pop=planet$population,
                                     founding_year=founding_year,
                                     canon=canon))
      }
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
      xml_add_child(primary_planet_name_node, planet_name_change)
    }
  }
    
  cat("\n\tdone\n")
}

#### Fill in HPG Network ####

cat("\nFilling in gaps in HPG First Circuit...")

#hpg_data_backup <- hpg_data
hpg_data <- hpg_data[order(hpg_data$pop, decreasing = TRUE),]
hpg_data <- subset(hpg_data, !duplicated(hpg_data$id))
hpg_data$tech <- factor(hpg_data$tech,
                        levels=c("X","F","D","C","B","A"),
                        ordered = TRUE)
hpg_data$hpg <- toupper(as.character(hpg_data$hpg))
hpg_data$hpg[hpg_data$hpg=="NONE"] <- "X"

hpg_network <- get_network(hpg_data)

## Connect the First Circuit
#we grab the nearest system not connected to the terra network and then identify
#its whole network. We then take the two closest planets from each network and
#try to find a set of candidates that are within 50LY of each and select the
#best one by tech and population. If we can't connect the two with one
#intermediate, then we select from candidates that get the isolated network
#closer to the Terra network. We then reconstruct the network and do this all
#over again, until we have no more isolates. All of this is only done for IS
#systems.
while(sum(hpg_network$first$connect_terra==FALSE)>0) {
  #get the isolated network of the first unconnected planet
  isolate <- which(!hpg_network$first$connect_terra)[1]
  temp <- get_all_connected_nodes(hpg_network$network, isolate)
  if(!is.na(temp[1])) {
    isolate <- c(isolate, temp)
  }
  isolated_network <- hpg_network$first[isolate, c("id","x","y")]
  terra_network <- hpg_network$first[hpg_network$first$connect_terra,
                                     c("id","x","y")]
  closest_points <- find_closest_points(terra_network, isolated_network)
  candidates <- find_all_overlaps(closest_points)

  #if we have candidates, then pick one (for now randomly)
  if(nrow(candidates)>0) {
    #choose highest tech and use population as tiebreaker
    nominee <- candidates[order(candidates$tech, candidates$pop,
                                decreasing = TRUE),"id"][1]
  } else {
    candidates <- find_closer_planets(closest_points)
    if(nrow(candidates)>0) {
      #generally we want to choose higher tech, but first set up a
      #tier of 20 or more from ego system so that they are not piled up to close
      candidates$far_enough <- candidates$distance_iso>=20
      nominee <- candidates[order(candidates$far_enough, candidates$tech,
                                  candidates$pop, decreasing = TRUE),"id"][1]
    } else {
      #If we can't find anyway to connect this, then the closest point
      #should not be on the First Circuit
      hpg_data$hpg[hpg_data$id==closest_points$iso$id[1]] <- "B"
      hpg_network <- get_network(hpg_data)
      next
    }
  }

  #you have been promoted!
  hpg_data$hpg[hpg_data$id==nominee] <- "A"
  hpg_network <- get_network(hpg_data)
}

cat("done\n")

#once this is done then we can loop through hpg_data and use it to 
#project HPG events for each system and then add to event_table
cat("\nAdding HPG information to event data\n")
hpg_data$id <- as.character(hpg_data$id)
for(i in 1:nrow(hpg_data)) {
  hpg <- hpg_data[i,]
  cat("\t")
  cat(hpg$id)
  cat("....")
  #retrieve events for this system and planet - assume primary
  #planet has the HPG
  primary <- as.numeric(xml_text(xml_find_first(get_system_id(systems, hpg$id),
                                                "primarySlot")))

  #project the HPG information
  hpg_history <- project_hpg(hpg$hpg, sqrt(hpg$x^2+hpg$y^2),
                             hpg$founding_year,
                             as.character(hpg$faction_type))
  
  #add to event_table
  event_table <- event_table %>% 
    bind_rows(tibble(id=as.character(hpg$id),
                     sys_pos=primary,
                     date=paste(hpg_history$year,"01","01",sep="-"),
                     etype="hpg",
                     event=paste(hpg_history$hpg),
                     canon=hpg$canon))
  cat("done\n")
}

#### Write Event Data to XML ####

cat("\nWriting event data to XML\n")

#sort event data by id and then date
event_table$date <- as.Date(event_table$date)
event_table <- event_table[order(event_table$id, event_table$sys_pos, 
                                 event_table$date, event_table$etype),]
#in case there are duplicates, remove
event_table <- event_table[!duplicated(event_table[,c("id","sys_pos","date","etype")]),]

unique_ids <- unique(event_table$id)
for(uid in unique_ids) {
  unique_pos <- unique(subset(event_table, id==uid)$sys_pos)
  system_event_node <- get_system_id(systems_events, uid)[[1]]
  for(upos in unique_pos) {
    if(upos==0) {
      #overall system events go in position zero, the only events here 
      #are recharge stations
      top_node <- system_event_node      
    } else {
      #otherwise they go in planets
      top_node <- xml_add_child(system_event_node, "planet")
      xml_add_child(top_node, "sysPos", upos)
    }
    pevent_table <- subset(event_table, id==uid & sys_pos==upos)
    event_group <- NULL
    for(i in 1:nrow(pevent_table)) {
      event_group <- event_group %>% bind_rows(pevent_table[i,])
      #check to see if the next date is the same
      if(i!=nrow(pevent_table) && pevent_table$date[i]==pevent_table$date[i+1]) {
        #keep building the event group
        next
      } else {
        #write the event group to XML
        event <- xml_add_child(top_node, "event")
        xml_add_child(event, "date", as.character(event_group$date[1]))
        for(j in 1:nrow(event_group)) {
          if(event_group$canon[j]) {
            xml_add_child(event, event_group$etype[j], event_group$event[j], source="canon")
          } else {
            xml_add_child(event, event_group$etype[j], event_group$event[j], source="noncanon")
          }
        }
        event_group <- NULL
      }
    }
  }
}
cat("done\n")

#TODO: a similar for-loop for connectors but no need to force habitation

#### Write Out XML Data ####

cat(as.character(systems), file = here("output","systems.xml"))
cat(as.character(systems_events), file = here("output","system_events.xml"))
cat(as.character(systems_name_changes), file = here("output","system_namechanges.xml"))
write.csv(event_table, file=here("output","event_table.csv"))
