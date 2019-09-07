#some useful functions for dealing with data processing

#### Functions for processing initial XML data ####

#return planet nodeset by id. This can be used on 
#planets or planetevents file
get_planet_id <- function(full_xml, id) {
  planets <- xml_find_all(full_xml, "planet")
  planets[which(xml_text(xml_find_first(planets, "id"))==id)]
}

get_planet_syspos <- function(full_xml, syspos) {
  planets <- xml_find_all(full_xml, "planet")
  planets[which(xml_text(xml_find_first(planets, "sysPos"))==syspos)]
}

#return a data frame of type of events for a given planet
get_event_data <- function(events, id, event_type) {
  planet_events <- xml_find_all(get_planet_id(events, id), "event")
  dates <- NULL
  v_events <- NULL
  for(event in planet_events) {
    date <- xml_text(xml_find_first(event, "date"))
    chosen_event <- xml_find_first(event, event_type)
    if(!is.na(chosen_event)) {
      dates <- c(dates, date)
      v_events <- c(v_events, xml_text(chosen_event))
    } 
  }
  if(is.null(dates)) {
    return(NULL)
  }
  #remove any missing values that may occur due to bad date formatting
  events_df <- na.omit(data.frame(date=as.Date(dates,format=c("%Y-%m-%d")),
                                  event=v_events))
  events_df <- events_df[order(events_df$date),]
  return(events_df)
}

#get the closest event to the given date that is not later than the 
#given date and not before the min_date (ignore min_date if NA)
get_closest_event <- function(event_table, chosen_date, min_date=NA, allow_later=FALSE) {
  date_diff <- event_table$date-chosen_date
  if(sum(date_diff<=0)==0) {
    ##all the dates are later than the target date
    if(allow_later) {
      #it should already be sorted from earliest to latest, so take the first
      return(as.character(event_table$event[1]))
    } else {
      return(NA)
    }
  }
  if(!is.na(min_date)) {
    if(sum(abs(date_diff[date_diff<0])<(chosen_date-min_date))==0) {
      return(NA)
    }    
  }
  return(as.character(event_table$event[which(date_diff==max(date_diff[date_diff<=0]))[1]]))
}

#### Functions for post processing into systems ####

get_system_id <- function(full_xml, id) {
  systems <- xml_find_all(full_xml, "system")
  systems[which(xml_text(xml_find_first(systems, "id"))==id)]
}

get_planet_in_system <- function(full_xml, id, pos) {
  systems <- xml_find_all(full_xml, "system")
  system <- systems[which(xml_text(xml_find_first(systems, "id"))==id)]
  planets <- xml_find_all(system, "planet")
  return(planets[which(xml_text(xml_find_first(planets, "sysPos"))==pos)][[1]])
}

#### Functions for dealing with universe data ####

#extract the year from a date object as numeric
get_year <- function(date) {
  as.numeric(format(date,'%Y'))
}

get_faction_type <- function(faction) {
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
  return(faction_type)
}

fix_founding_heaping <- function(founding_year) {
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
  return(founding_year)
}

fix_abandoned_heaping <- function(abandon_year, founding_year) {
  if(is.na(abandon_year)) {
    return(abandon_year)
  }
  #2821 and 2822: end of the 1SW, so these should probably be distributed throughout the 1SW. 
  #There are also quite a few at 2822 including Lone Star which definitely died during 1SW.
  #2864: end of the 2SW, so distribute throughout 2SW.
  #2888: Not sure what this, but maybe a map related difference? Distribute between end of 2SW and 2888.
  #2900-2985. A lot of things on the 10s and 5s and a huge group at 2925. Probably distribute immediately 
  #proceeding 5 years.
  #3025: Distribute between 3000-3025.
  abandon_new <- NA
  if(abandon_year==2821 | abandon_year==2822) {
    abandon_new <- sample(2786:2822, 1)
  }
  if(abandon_year==2864) {
    abandon_new <- sample(2830:2864, 1)
  }
  if(abandon_year==2888) {
    abandon_new <- sample(2865:2888, 1)
  }
  if(abandon_year>=2900 & abandon_year<=2985 & 
     ((abandon_year %% 10)==0 | (abandon_year %% 10)==5)) {
    abandon_new <- abandon_year-sample(0:4,1)
  }
  if(abandon_year==3025) {
    abandon_new <- sample(3001:3025, 1)
  }
  #I need to make sure that the abandon year is never less than or equal to founding year. If I find that to 
  #be the case, then I will just add some random number to founding year and take the minimum of that or the 
  #original abandon year
  if(!is.na(abandon_new)) {
    if(abandon_new<=founding_year) {
      abandon_year <- min(founding_year+ceiling(rexp(1,1/100)), abandon_year)
    } else {
      abandon_year <- abandon_new
    }
  } 
  return(abandon_year)
}

#### Functions for writing to XML ####

#write the physical system to XML
write_system_xml <- function(system_node, system, id, x, y, primary_slot,
                             gravity=NA, pressure=NA, temperature=NA,
                             water=NA, life=NA, continents=NA, moons=NA,
                             desc=NA) {
  xml_add_child(system_node, "id", id)
  xml_add_child(system_node, "xcood", x)
  xml_add_child(system_node, "ycood", y)
  
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
  }
}
