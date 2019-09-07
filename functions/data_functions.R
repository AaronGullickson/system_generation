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
