#some useful functions for dealing with data processing

#return planet nodeset by id. This can be used on 
#planets or planetevents file
get_planet_id <- function(full_xml, id) {
  planets <- xml_find_all(full_xml, "planet")
  planets[which(xml_text(xml_find_first(planets, "id"))==id)]
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
  events_df <- data.frame(date=as.Date(dates,format=c("%Y-%m-%d")),
                          event=v_events)
  events_df <- events_df[order(events_df$date),]
  return(events_df)
}

#get the closest event to the given date that is not later than the 
#given date and not before the min_date (ignore min_date if NA)
get_closest_event <- function(event_table, chosen_date, min_date=NA) {
  date_diff <- faction_table$date-chosen_date
  if(sum(date_diff<=0)==0) {
    return(NA)
  }
  if(!is.na(min_date)) {
    if(sum(abs(date_diff[date_diff<0])<(chosen_date-min_date))==0) {
      return(NA)
    }    
  }
  return(as.character(event_table$event[which(date_diff==max(date_diff[date_diff<=0]))[1]]))
}

#extract the year from a date object as numeric
get_year <- function(date) {
  as.numeric(format(date,'%Y'))
}