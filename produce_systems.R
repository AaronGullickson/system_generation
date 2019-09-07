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
library(stringr)
source(here("functions","system_creation_functions.R"))
source(here("functions","data_functions.R"))
source(here("functions","naming_functions.R"))
source(here("functions","network_functions.R"))

#set a seed to allow for reproducing the results
#set.seed(20)

options(nwarnings = 1000)

planets <- read_xml(here("output","planets_initial.xml"))
connectors <- read_xml(here("output","connectors_initial.xml"))
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
                       abandon_year=numeric(),
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

#for recolonization
# a list of recolonization faction events for later processing
recolonization <- list()
# a list of primary planets for recolonization
recolonized_planets <- list()

#prepare the XML systems output
systems <- xml_new_document() %>% xml_add_child("systems")
systems_events <- xml_new_document() %>% xml_add_child("systems")
systems_name_changes <- xml_new_document() %>% xml_add_child("systems")
systems_connectors <- xml_new_document() %>% xml_add_child("systems")

small_sample <- sample(1:xml_length(planets), 100)

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
  nadir_charge <- xml_text(xml_find_first(planet, "nadirCharge"))=="true"
  zenith_charge <- xml_text(xml_find_first(planet, "zenithCharge"))=="true"
  #it seems like all planets were given a default FALSE value here, which makes it difficult 
  #to sort out canon cases of no recharge stations. We will have to assume that where both
  #are false its non-canon and therefore ignore them
  if(!is.na(nadir_charge) && !nadir_charge && !is.na(zenith_charge) && !zenith_charge) {
    nadir_charge <- NA
    zenith_charge <- NA
  }
  
  #check for faction change events
  faction_table <- get_event_data(events, id, "faction")
  founding_year <- NA
  terran_hegemony <- FALSE
  abandon_year <- NA
  is_recolonized <- FALSE
  if(!is.null(faction_table)) {
    founding_year <- get_year(faction_table$date[1])
    
    if("ABN" %in% faction_table$event) {
      #in order to handle cases of abandoned planets that are re-founded and potentially abandoned 
      #again we need to truncate the faction table 
      #probably a better way to handle this than a for loop but damned if I can figure it out
      splits <- which(faction_table$event=="ABN")
      
      temp <- list()
      for(j in length(splits):1) {
        if(splits[j]==nrow(faction_table)) {
          next
        }
        temp[[j]] <- faction_table[(splits[j]+1):nrow(faction_table),]
        faction_table <- faction_table[1:splits[j],]
      }
      if(length(temp)>0) {
        recolonization[[id]] <- temp
        is_recolonized <- TRUE
      }
      
      abandon_year <- get_year(subset(faction_table, event=="ABN")$date)
    }
      
    #we allow_later=TRUE so we don't lose planets with founding dates later than target_date
    #remove any cases of ABN from what we feed in here as well so we get the faction before abandonment
    temp <- get_closest_event(subset(faction_table, event!="ABN"), 
                              target_date, allow_later = TRUE)
    
    #take the first faction in cases of multiple factions
    if(!is.na(temp)) {
      faction <- strsplit(temp,",")[[1]][1]
    }
    #figure out if this was Terran Hegemony world in 2750
    terran_hegemony <- grepl("(^TH$|TH,|,TH)", 
                             get_closest_event(subset(faction_table, event!="ABN"), 
                                               as.Date(paste(2750,"01","01",sep="-"))))
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

  #drop if they are missing x or y coordinates (shouldnt happen)
  if(is.na(x) | is.na(y)) {
    warning(paste("ERROR:", id, "is missing an x or y value. Skipping.\n"))
    next
  }
  
  #drop if they are missing founding year
  if(is.na(founding_year)) {
    warning(paste("ERROR:", id, "has a missing founding year. Skipping.\n"))
    next
  }
  
  # ignore abandoned places or those with UND or NONE factions (mostly highways which
  # should be in the connector file not here)
  if(is.na(faction) | faction=="UND" | faction=="NONE") {
    warning(paste("ERROR:", id, "has a missing or unknown faction. Skipping.\n"))
    next
  }
  
  if(faction=="ABN") {
    warning(paste(id, "is abandoned. Skipping.\n"))
    next
  }

  
  cat("done\n\tOrganizing data...")
  
  #### Generate the System ####
  
  distance_terra <- sqrt(x^2+y^2)
  #this would be easier to do with an %in% but not going to mess with it now
  faction_type <- get_faction_type(faction)
  
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
    
  } else {
    star <- NULL
  }

  #This is a bit of a hack, but non-canon star types were generated for pretty much every
  #system at some point. To figure out which are actually canon, we will use sys_pos 
  if(is.na(sys_pos)) {
    star <- NULL
  }
  
  ## Fix founding year
  # There was considerable amounts of heaping in founding years because these were
  # determined in many cases by changes between maps with the faction change only 
  # occurring at the latter date. We want to allow these founding dates to vary between
  # the map dates, probably by drawing from uniform distribution. 
  founding_year <- fix_founding_heaping(founding_year)
  
  #also check for heaping on abandon year. This function will return NA if abandon_year is NA 
  #already. We also need to feed in founding year to make sure we don't end up with abandonment
  #before founding
  abandon_year <- fix_abandoned_heaping(abandon_year, founding_year)
  
  #read in canon population data
  if(id %in% rownames(canon_populations)) {
    canon_population <- canon_populations[id,]
  } else {
    canon_population <- NULL
  }
  
  # use the name of the planet to determine if there is a specific system position to be filled
  temp <- gsub("\\(.*\\)", "", name)
  #first look for roman numerals
  canon_pos <- NA
  roman <- str_trim(str_extract(temp, "\\s+(I|II|III|IV|V|VI|VII|VIII|IX|X|XI|XII|XIII|XIV|XV)$"))
  if(!is.na(roman)) {
    canon_pos <- convert_roman2arabic(roman)
  }
  #now look for arabic numbering
  arabic <- str_trim(str_extract(temp, "\\s+\\d+$"))
  if(!is.na(arabic)) {
    canon_pos <- as.numeric(arabic)
  }
  if(!is.na(sys_pos)) {
    canon_pos <- sys_pos
  }
  
  cat("done\n\tGenerating base system and colonization data...")
  
  system <- generate_system_names(add_colonization(generate_system(star=star, habit_pos=canon_pos), 
                                                   distance_terra, 3047, founding_year,
                                                   faction_type, abandon_year), 
                                  id)
  primary_slot <- which(system$planets$population==max(system$planets$population, na.rm=TRUE))[1]
  
  #### Output XML ####
  
  cat("done\n\tOutputing base data to XML...")
  
  #create a system node
  system_event_node <- xml_add_child(systems_events, "system")
  xml_add_child(system_event_node, "id", id)
  
  system_node <- xml_add_child(systems, "system")
  
  write_system_xml(system_node, system, id, x, y, primary_slot,
                   star, gravity, pressure, temperature, water, 
                   life, continents, moons, desc)
  
        
  #### Project Social Data in Time ####

  #we add these events to event_table initially, so we can do some post-processing
  
  for(j in which(!is.na(system$planets$population))) {  
    
    planet <- system$planets[j,]
    
    if(!is.null(faction_table)) {
      #put in a new founding year as it may have been changed for heaping
      faction_table$date[1] <- paste(founding_year,"01","01",sep="-")
      #if there is an abandonment in the table it may also have changed
      if("ABN" %in% faction_table$event) {
        faction_table$date[faction_table$event=="ABN"] <- paste(abandon_year,"01","01",sep="-")
      }
      for(idx in 1:nrow(faction_table)) {
        event_table <- event_table %>% 
          bind_rows(tibble(id=as.character(id),
                           sys_pos=j,
                           date=as.character(faction_table$date[idx]),
                           etype="faction",
                           event=as.character(faction_table$event[idx]),
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
                              abandon_year = abandon_year,
                              p2750 = p2750, p3025 = p3025, p3067 = p3067,
                              p3079 = p3079, p3145 = p3145)
    #collect population values at 10 year intervals, plus the starting and final values.
    first_census_year <- founding_year + 10*(ceiling(founding_year/10)-(founding_year/10))
    last_year <- as.numeric(names(pop)[length(pop)])
    last_census_year <- 10*floor(last_year/10)
    if(last_census_year<=first_census_year) {
      #if colony died quickly or was founded close to 3145, this won't work so just put first population
      event_table <- event_table %>% 
        bind_rows(tibble(id=as.character(id),
                         sys_pos=j,
                         date=paste(founding_year,"01","01",sep="-"),
                         etype="population",
                         event=paste(pop[1]),
                         canon=FALSE))
    } else {
      #otherwise add census years to event list 
      census_years <- c(founding_year, seq(from=first_census_year, to=last_census_year, by=10), last_year)
      census_pop <- round(pop[paste(census_years)])
      event_table <- event_table %>% 
        bind_rows(tibble(id=as.character(id),
                         sys_pos=j,
                         date=paste(census_years,"01","01",sep="-"),
                         etype="population",
                         event=paste(census_pop),
                         canon=FALSE))
    }
    
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
    #if abandoned, add in abandonment year
    if(!is.na(abandon_year)) {
      event_table <- event_table %>% 
        bind_rows(tibble(id=as.character(id),
                         sys_pos=j,
                         date=paste(abandon_year,"01","01",sep="-"),
                         etype="population",
                         event="0",
                         canon=TRUE))
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
                                     founding_year, abandon_year, pop, faction_type)
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
                                   pop=max(pop),
                                   founding_year=founding_year,
                                   abandon_year=abandon_year,
                                   canon=canon))
    }
    
    #Recharge Stations - this should only be checked on the primary slot, 
    #but should have a sys_pos of zero so it will be written to the system
    if(j==primary_slot) {
      #first check to see if we have canon data for either recharge station
      #and replace if so
      if(!is.na(nadir_charge)) {
        system$recharge$nadir <- nadir_charge
      }
      if(!is.na(zenith_charge)) {
        system$recharge$zenith <- zenith_charge
      }
      
      recharge_data <- project_recharge(system$recharge, faction_type, 
                                        founding_year, sics_projections,
                                        pop, abandon_year)
      event_table <- event_table %>% 
        bind_rows(tibble(id=as.character(id),
                         sys_pos=0,
                         date=paste(recharge_data$year,"01","01",sep="-"),
                         etype=recharge_data$etype,
                         event=paste(recharge_data$event),
                         canon=FALSE))
    }
    
    if(j==primary_slot && is_recolonized) {
      #we need to save the planet information so that we can re-access it later for recolonization
      planet$previous_abandon_year <- abandon_year
      planet$border_distance <- border_distance
      planet$terran_hegemony <- terran_hegemony
      if(!is.na(sic)) {
        canon_sics <- separate_sics(sic)
        planet$tech <- canon_sics$tech
        planet$industry <- canon_sics$industry
        planet$raw <- canon_sics$raw
        planet$output <- canon_sics$output
        planet$agriculture <- canon_sics$agriculture
      }
      planet$nadir <- system$recharge$nadir
      planet$zenith <- system$recharge$zenith
      planet$distance_terra <- distance_terra
      planet$primary_slot <- primary_slot
      recolonized_planets[[id]] <- planet
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
      
      #check to see if changed to numeric style
      date <- xml_text(xml_find_first(planet_name_change, "date"))
      temp <- xml_text(xml_find_first(planet_name_change, "name"))
      roman <- grepl("\\s+(I|II|III|IV|V|VI|VII|VIII|IX|X|XI|XII|XIII|XIV|XV)$", 
                     temp)
      arabic <- grepl("\\s+\\d+$", temp)
      if(roman || arabic) {
        #change all the remaining system names except asteroid belts
        base_name <- ""
        if(roman) {
          base_name <- trimws(gsub("\\s+(I|II|III|IV|V|VI|VII|VIII|IX|X|XI|XII|XIII|XIV|XV)$", 
                                   "", temp))
        }
        if(arabic) {
          base_name <- trimws(gsub("\\s+\\d+$", "", temp))
        }
        pos <- 1
        for(j in 1:nrow(system$planets)) {
          if(j==primary_slot) {
            pos <- pos+1
            next
          }
          if(system$planets$type[j]=="Asteroid Belt") {
            next
          }
          planet_node <- xml_add_child(system_name_node, "planet")
          xml_add_child(planet_node, "sysPos", j)
          event <- xml_add_child(planet_node, "event")
          xml_add_child(event, "date", date)
          if(roman) {
            xml_add_child(event, "name", paste(base_name, convert_arabic2roman(pos)), source="noncanon")
          } else {
            xml_add_child(event, "name", paste(base_name, pos), source="noncanon")
          }
          pos <- pos+1
        }
      }
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
                             as.character(hpg$faction_type),
                             hpg$abandon_year)
  
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

#### Handle Recolonization Cases ####

cat("\nHandling Recolonization Efforts\n")
#I am getting 56 cases of recolonization and one of those cases is a double recolonization
recol_ids <- names(recolonization)
for(recol_id in recol_ids) {
  cat(recol_id)
  recol_attempts <- recolonization[[recol_id]]
  planet <- recolonized_planets[[recol_id]]
  #for now just do the first one - figure out the one case of double recolonization later
  for(i in 1:length(recol_attempts)) {
    faction_table <- recol_attempts[[i]]
    #some of these (e.g. Afleir) are just showing an abandoned event which makes no sense unless ABN was 
    #in this case we have an abandoned date of 2860 and then one immediately after for 2864.
    #added twice
    #check for singular abandoned factions from bad data
    if(nrow(faction_table==1) && faction_table$event[1]=="ABN") {
      next
    }
    #get faction
    temp <- get_closest_event(subset(faction_table, event!="ABN"), 
                              target_date, allow_later = TRUE)
    #take the first faction in cases of multiple factions
    if(!is.na(temp)) {
      faction <- strsplit(temp,",")[[1]][1]
    }
    faction_type <- get_faction_type(faction)
    founding_year <- get_year(faction_table$date[1])
    abandon_year <- NA
    if("ABN" %in% faction_table$event) {
      abandon_year <- get_year(subset(faction_table, event=="ABN")$date)
    }
    #read in canon population data
    if(id %in% rownames(canon_populations)) {
      canon_population <- canon_populations[id,]
    } else {
      canon_population <- NULL
    }
    #deal with potential founding year heaping, but be careful
    #not to go back before previous abandonment date
    founding_corrected <- fix_founding_heaping(founding_year)
    if(founding_corrected != founding_year) {
      if(!is.na(planet$previous_abandon_year) && founding_corrected<=planet$previous_abandon_year) {
        #add around 50 years (on average) to the abandon date and take the minimum of this value and 
        #the original founding year
        founding_year <- min(planet$previous_abandon_year+ceiling(rexp(1,1/50)), founding_year)
      } else {
        founding_year <- founding_corrected
      }
    }
    #now address abandon year heaping
    abandon_year <- fix_abandoned_heaping(abandon_year, founding_year)
    
    ## Creat the colony
    
    #faction history
    if(!is.null(faction_table)) {
      #fix founding year in case changed due to heaping
      faction_table$date[1] <- paste(founding_year,"01","01",sep="-")
      #if there is an abandonment in the table it may also have changed
      if("ABN" %in% faction_table$event) {
        faction_table$date[faction_table$event=="ABN"] <- paste(abandon_year,"01","01",sep="-")
      }
      for(i in 1:nrow(faction_table)) {
        event_table <- event_table %>% 
          bind_rows(tibble(id=as.character(recol_id),
                           sys_pos=planet$primary_slot,
                           date=as.character(faction_table$date[i]),
                           etype="faction",
                           event=as.character(faction_table$event[i]),
                           canon=TRUE))
      }
    }
    
    #POPULATION
    p2750 <- NULL
    p3025 <- NULL
    p3067 <- NULL
    p3079 <- NULL
    p3145 <- NULL
    if(!is.null(canon_population)) {
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
                              planet$border_distance, planet$agriculture, 
                              terran_hegemony = planet$terran_hegemony,
                              abandon_year = abandon_year,
                              p2750 = p2750, p3025 = p3025, p3067 = p3067,
                              p3079 = p3079, p3145 = p3145)
    #collect population values at 10 year intervals, plus the starting and final values.
    first_census_year <- founding_year + 10*(ceiling(founding_year/10)-(founding_year/10))
    last_year <- as.numeric(names(pop)[length(pop)])
    last_census_year <- 10*floor(last_year/10)
    if(last_census_year<=first_census_year) {
      #if colony died quickly or was founded close to 3145, this won't work so just put first population
      event_table <- event_table %>% 
        bind_rows(tibble(id=as.character(recol_id),
                         sys_pos=planet$primary_slot,
                         date=paste(founding_year,"01","01",sep="-"),
                         etype="population",
                         event=paste(pop[1]),
                         canon=FALSE))
    } else {
      #otherwise add census years to event list 
      census_years <- c(founding_year, seq(from=first_census_year, to=last_census_year, by=10), last_year)
      census_pop <- round(pop[paste(census_years)])
      event_table <- event_table %>% 
        bind_rows(tibble(id=as.character(recol_id),
                         sys_pos=planet$primary_slot,
                         date=paste(census_years,"01","01",sep="-"),
                         etype="population",
                         event=paste(census_pop),
                         canon=FALSE))
    }
    #if abandoned, add in abandonment year
    if(!is.na(abandon_year)) {
      event_table <- event_table %>% 
        bind_rows(tibble(id=as.character(recol_id),
                         sys_pos=planet$primary_slot,
                         date=paste(abandon_year,"01","01",sep="-"),
                         etype="population",
                         event="0",
                         canon=TRUE))
    }
    
    #SICS
    sics_projections <- project_sics(planet$tech, planet$industry, planet$raw, 
                                     planet$output, planet$agriculture, 
                                     founding_year, abandon_year, pop, faction_type)
    event_table <- event_table %>% 
      bind_rows(tibble(id=as.character(recol_id),
                       sys_pos=planet$primary_slot,
                       date=paste(sics_projections$year,"01","01",sep="-"),
                       etype="socioIndustrial",
                       event=paste(sics_projections$sics),
                       canon=FALSE))
    
    #HPG
    hpg_history <- project_hpg(planet$hpg, planet$distance_terra,
                               founding_year,
                               as.character(faction_type),
                               abandon_year)
    
    #add to event_table
    event_table <- event_table %>% 
      bind_rows(tibble(id=as.character(recol_id),
                       sys_pos=planet$primary_slot,
                       date=paste(hpg_history$year,"01","01",sep="-"),
                       etype="hpg",
                       event=paste(hpg_history$hpg),
                       canon=FALSE))
    
    #RECHARGE
    recharge_data <- project_recharge(list(nadir=planet$nadir, zenith=planet$zenith), 
                                      faction_type, founding_year, sics_projections,
                                      pop, abandon_year)
    event_table <- event_table %>% 
      bind_rows(tibble(id=as.character(recol_id),
                       sys_pos=0,
                       date=paste(recharge_data$year,"01","01",sep="-"),
                       etype=recharge_data$etype,
                       event=paste(recharge_data$event),
                       canon=FALSE))
    
  }
  cat("\n")
}
cat("done\n")

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

#### Create Systems for Connectors ####

cat("Creating System Connectors....")
for(i in 1:xml_length(connectors)) {
  #technical identification
  planet <- xml_children(connectors)[[i]]
  name <- xml_text(xml_find_first(planet, "name"))
  x <- as.numeric(xml_text(xml_find_first(planet, "xcood")))
  y <- as.numeric(xml_text(xml_find_first(planet, "ycood")))

  cat(name)
  cat("\n")
  #I can ignore the generated star information, since its all non-canon anyway
  system <- generate_connector_names(generate_system(habitable = FALSE))
  system_node <- xml_add_child(systems_connectors, "system")
  write_system_xml(system_node, system, name, x, y, 0)
  cat("\n")
}
cat("done\n")

#### Write Out XML Data ####

cat(as.character(systems), file = here("output","systems.xml"))
cat(as.character(systems_events), file = here("output","system_events.xml"))
cat(as.character(systems_name_changes), file = here("output","system_namechanges.xml"))
cat(as.character(systems_connectors), file = here("output","system_connectors.xml"))
write.csv(event_table, file=here("output","event_table.csv"))
