#This script contains functions for creating systems and planets
library(tibble)
library(magrittr)
library(dplyr)
library(here)

#a bunch of constants by star type for use in calculations
solar_types <- read.csv(here("input","solar_type.csv"), row.names=1)
load(here("output","habitable_zones.RData"))

#### System Generation Functions ####

generate_system <- function(star=NULL, habitable=TRUE, habit_pos=NA) {
  
  if(!is.null(star)) {
    #break apart stype to make sure it makes sense
    spectral_class <- substr(star,1,1)
    subtype <- as.numeric(substr(star,2,2))
    star_size <- substring(star, 3)
    
    if(is.na(star_size) | !(spectral_class %in% c("A","B","F","G","K","M"))) {
      warning("Invalid spectral class provided.")
      star <- NULL
    }
    if(is.na(subtype) | subtype <0 | subtype>9) {
      warning("Invalid subtype provided.")
      star <- NULL
    }
    if(is.na(star_size) | nchar(star_size)==0 | 
       !(star_size %in% c("Ia","Ib","II","III","IV","V","VI","VII"))) {
      warning("Invalid star size provided.")
      star <- NULL
    }
    
    #check if the habitable position is possible for this star type
    if(!is.null(star) & !is.na(habit_pos) & habitable) {
      if((star_size=="V" && !(habit_pos %in% which(habitable_zones$main[,star]))) |
         (star_size!="V" && !(habit_pos %in% which(habitable_zones$nonmain[,paste(spectral_class, 
                                                                                    subtype, sep="")])))) { 
        warning(paste("Star type ", star, " is not a valid star type for a habitable planet in position ", habit_pos,
                      ". Star type randomly generated instead.", sep=""))
        star <- NULL
      }
    }
  }
  
  ## Sample a star type
  #if the habitable position is selected we need to resample until we get a star type 
  #that will work for the possible habitable positions
  
  done <- FALSE
  while(!done) {
    if(is.null(star)) {
      
      #if habit_pos is greater than 7, its impossible so make it 7
      if(!is.na(habit_pos) && habit_pos>7) {
        warning(paste("Habitable planet suggested in position", 
                      habit_pos, "is impossible in life zone. Ignoring position."))
        habit_pos <- 7
      }
      #if the habitable position is between 5 and 7, then we must roll on the 
      #hot star column to get something habitable
      hot_star <- FALSE
      if(!is.na(habit_pos) && habit_pos>4) {
        hot_star <- TRUE
      }
      
      #if no star type provided, then roll one up from life-friendly column
      #in CamOps
      star_roll <- roll_d6(2)
      
      # Star Type
      star_type <- c("F","M","G","K",rep("M",6),"F")
      if(habitable & !hot_star) {
        star_type <- c(rep("M",3),"K","K","G","G",rep("F",4))
      } else if(star_roll==12 | hot_star) {
        #hot stars!
        star_type <- c(rep("B",2),rep("A",7),"B","F")
        star_roll <- roll_d6(2)
      }
      
      spectral_class <- star_type[star_roll-1]
      subtype <- sample(0:9,1)
      star_size <- "V"
      
    }
    stype <- paste(spectral_class, subtype, star_size, sep="")
    
    #table only has V stars, so feed that in. We will override below
    stype_data <- solar_types[paste(spectral_class, subtype, "V", sep=""),]
    
    if(star_size != "V") {
      #according to CamOps pg 116, mulitiply luminosity by four and double the life 
      #zone values
      stype_data$luminosity <- stype_data$luminosity*4
      stype_data$distance_inner_au <- stype_data$distance_inner_au*2
      stype_data$distance_outer_au <- stype_data$distance_outer_au*2
    }
    
    #orbital slots
    orbital_slots <- 3+roll_d6(2)
    
    placement_constants <- c(0.4,0.7,1.0,1.6,2.8,5.2,10,19.6,
                             38.8,77.2,154,307.6,614.8,1229.2,
                             2458)
    orbital_placement <- stype_data$mass*placement_constants
    
    #if the system needs to be habitable, then randomly pick one of the habitable slots
    #and force it to produce a habitable planet
    life_zone <- orbital_placement>=stype_data$distance_inner_au & 
      orbital_placement<=stype_data$distance_outer_au
    #check to see if this selection works for the habitable position
    if(is.na(habit_pos)) {
      done <- TRUE
    } else {
      #check
      done <- habit_pos %in% which(life_zone)
    }
  }
  
  #make sure the number of orbital slots is at least equal to the minimum life zone
  if(habitable) {
    min_life_zone_slot <- min(which(life_zone))
    orbital_slots <- max(orbital_slots, min_life_zone_slot)
  }
  life_zone <- life_zone[1:orbital_slots]
  
  habitable_slot <- -1
  if(habitable) {
    habitable_slot <- which(life_zone)
    if(length(habitable_slot)>1) {
      #if more than one slot in habitable zone then choose randomly
      habitable_slot <- sample(habitable_slot,1)
    } 
  }
  if(!is.na(habit_pos)) {
    habitable_slot <- habit_pos
  }
  
  planets <- NULL
  swept_zone <- FALSE
  previous_inhabitable <- FALSE
  for(slot in 1:orbital_slots) {

    #If canon system position is chosen, then don't allow empty slots before
    #the canon system position
    empty_ok <- is.na(habit_pos) || slot>habit_pos
    
    planet <- generate_planet(orbital_placement[slot],slot==habitable_slot,
                              stype_data, allow_empty = empty_ok)
    
    #If habitable is true, then we need to ensure that at least on habitable
    #slot in the life zone is occupied by terrestrial planet
    if(slot==habitable_slot) {
      while(!planet$inhabitable) {
        planet <- generate_planet(orbital_placement[slot],TRUE,stype_data,
                                  allow_empty = empty_ok)
      }
    }
    
    #according to camOps, a gas giant should create a swept zone past its
    #orbital slot that disallows dwarf terrestrials and terrestrials. So if we
    #are forcing habitation, only allow gas giant past the habitable slot. Also,
    #according to camOps a gas giant should not be allowed next to an inhabited
    #planet so really it has to be at least two slots above habitable_slot
    if(habitable & slot < (habitable_slot+2)) {
      while(planet$type=="Gas Giant") {
        planet <- generate_planet(orbital_placement[slot],FALSE,stype_data,
                                  allow_empty = empty_ok)
      }
    }
    
    #now check if we are in a swept zone from a prior gas giant and if so 
    #then disallow Terrestrials and Dwarf Terrestrials
    if(swept_zone) {
      #no dwarf terrestrials or terrestrial
      while(planet$type=="Terrestrial" | planet$type=="Dwarf Terrestrial") {
        planet <- generate_planet(orbital_placement[slot],FALSE,stype_data,
                                  allow_empty = empty_ok)
      }
    }
    
    #According to the optional placement rules in CamOps, asteroid belts should
    #only be placed next to Gas Giants. If this is a forced habitation system,
    #then this means that asteroid belts should never be placed closer than the
    #habitable slot+1 because otherwise the habitable slot will end up in a
    #swept zone once the gas giant is picked
    if(habitable & slot < (habitable_slot+1)) {
      while(planet$type=="Asteroid Belt" | planet$type=="Gas Giant") {
        planet <- generate_planet(orbital_placement[slot],FALSE,stype_data,
                                  allow_empty = empty_ok)
      }
    }
    
    #now check if the previous slot was an Asteroid belt, and if the slot in front of it
    #was not a Gas Giant, put a gas giant here
    if(!is.null(planets)) {
      if(planets[nrow(planets),1]=="Asteroid Belt") {
        previous <- "None"
        if(nrow(planets)>1) {
          previous <- planets[nrow(planets)-1,1]
        }
        if(previous!="Gas Giant") {
          #last slot was an asteroid belt without a Gas Giant on the other side
          #so put a Gas Giant here. 
          while(planet$type!="Gas Giant") {
            planet <- generate_planet(orbital_placement[slot],FALSE,stype_data,
                                      allow_empty = empty_ok)
          }
        }
      }
    }
    
    #If habitable is not forced or we are not on the habitable slot, we still
    #need to make sure that we didn't radomly get a habitable planet next to gas
    #giant. It will be impossible for previous to be gas giant because of swept
    #zone effect so we only need to check if current is gas giant and previous
    #was inhabitable.
    if(previous_inhabitable) {
      while(planet$type=="Gas Giant") {
        planet <- generate_planet(orbital_placement[slot],FALSE,stype_data,
                                  allow_empty = empty_ok)
      }
    }
    
    #If we currently have a gas giant then create swept zone
    if(!swept_zone & planet$type=="Gas Giant") {
      swept_zone <- TRUE
    }
    
    previous_inhabitable <- planet$inhabitable
    
    if(planet$type!="Empty") {
      planets <- rbind(planets, unlist(planet))
    }
  }
  
  planets <- data.frame(planets, stringsAsFactors = FALSE)
  planets$type <- factor(planets$type)
  planets$pressure <- factor(planets$pressure, 
                             levels=c("Vacuum","Trace","Low","Normal","High","Very High"))
  planets$atmosphere <- factor(planets$atmosphere)
  planets$diameter <- as.numeric(planets$diameter)
  planets$density <- as.numeric(planets$density)
  planets$gravity <- as.numeric(planets$gravity)
  planets$escape_velocity <- as.numeric(planets$escape_velocity)
  planets$orbital_velocity <- as.numeric(planets$orbital_velocity)
  planets$day_length <- as.numeric(planets$day_length)
  planets$year_length <- as.numeric(planets$year_length)
  planets$continents <- as.numeric(planets$continents)
  planets$water <- as.numeric(planets$water)
  planets$temperature <- as.numeric(planets$temperature)
  planets$orbital_dist <- as.numeric(planets$orbital_dist)
  planets$life <- factor(planets$life,
                         levels=c("Microbes","Plants","Insects","Fish","Amphibians","Reptiles","Birds","Mammals"))
  planets$life_zone <- planets$life_zone=="TRUE"
  planets$inhabitable <- planets$inhabitable=="TRUE"
  planets$moons_giant <- as.numeric(planets$moons_giant)
  planets$moons_large <- as.numeric(planets$moons_large)
  planets$moons_medium <- as.numeric(planets$moons_medium)
  planets$moons_small <- as.numeric(planets$moons_small)
  planets$rings <- planets$rings=="TRUE"
  
  return(list(star=stype, planets=planets))
}

#TODO: asteroid belt characteristics
generate_planet <- function(radius, habitable_system, system_data, more_gradation=TRUE,
                            allow_empty=TRUE) {
 
  life_zone <- radius>=system_data$distance_inner_au & radius<=system_data$distance_outer_au
  outer <- radius > system_data$distance_outer_au
  
  #get basic type  
  InnerType <- c(rep("Empty",2),
                 "Asteroid Belt",
                 "Dwarf Terrestrial",
                 rep("Terrestrial",2),
                 "Giant Terrestrial",
                 rep("Gas Giant",2),
                 rep("Ice Giant",2))
  
  OuterType <- c(rep("Empty",2),
                 "Asteroid Belt",
                 "Dwarf Terrestrial",
                 rep("Gas Giant",3),
                 "Terrestrial",
                 "Giant Terrestrial",
                 rep("Ice Giant",2))
  
  if(outer) {
    type <- OuterType[roll_d6(2)-1]
  } else {
    type <- InnerType[roll_d6(2)-1]
  }
  
  #don't allow empties if allow_empty is false
  while(type=="Empty" & !allow_empty) {
    if(outer) {
      type <- OuterType[roll_d6(2)-1]
    } else {
      type <- InnerType[roll_d6(2)-1]
    }
  }
  
  #now fill in stuff
  diameter <- NA
  density <- NA
  day <- NA
  pressure <- NA
  atmosphere <- NA
  composition <- NA
  life <- NA
  water <- NA
  continents <- NA
  temperature <- NA
  inhabitable <- FALSE
  moons_giant <- 0
  moons_large <- 0
  moons_medium <- 0
  moons_small <- 0
  rings <- FALSE
  
  if(type=="Dwarf Terrestrial") {
    diameter <- 400+100*roll_d6(3)
    density <- roll_d6(1)
    day <- roll_d6(3)+12
    pressure <- "Vacuum"
    atmosphere <- "None"
    water <- 0
    continent <- NA
  } else if(type=="Terrestrial") {
    #Another tweak here. The diameter and density numbers for terrestrials
    #are producing average gravities well below 1.0 and too variable. Its 
    #likely inhabited worlds will be closer to earth gravity, so we are going 
    #to use formulas that produce earth means for diameter and density with
    #less variation in diameter.
    if(habitable_system) {
      diameter <- 9000+300*roll_d6(3)
      density <- 3+roll_d6(1)^(0.75)
    } else {
      diameter <- 2500+1000*roll_d6(2)
      density <- 2.5+roll_d6(1)^(0.75)
    }
    day <- roll_d6(3)+12
  } else if(type=="Giant Terrestrial") {
    diameter <- 12500+1000*roll_d6(1)
    density <- 2+roll_d6(1)
    day <- roll_d6(4)
    pressure <- "Very High"
    atmosphere <- "Toxic (Poisonous)"
    composition <- "Hydrogen and Helium, plus trace gases"
  } else if(type=="Gas Giant") {
    diameter <- 50000+10000*roll_d6(2)
    density <- 0.5+roll_d6(2)/10
    day <- roll_d6(4)
    pressure <- "Very High"
    atmosphere <-  "Toxic (Poisonous)"
    composition <- "Hydrogen and Helium, plus trace gases"
  } else if(type=="Ice Giant") {
    diameter <- 25000+5000*roll_d6(1)
    density <- 1+roll_d6(2)/10
    day <- roll_d6(4)
    pressure <- "Very High"
    atmosphere <-  "Toxic (Poisonous)"
    composition <- "Hydrogen and Helium, plus trace gases"
  } else if(type=="Asteroid Belt") {

  }
  
  #derived stats
  gravity <- (diameter/12742)*(density/5.5153)
  escape_velocity <- 11186*(diameter/12742)*sqrt(density/5.5153)
  orbital_velocity <- escape_velocity/sqrt(2)
  year_length <- sqrt(radius^3+system_data$mass)
  transit_distance <- sqrt((system_data$safe_jump*1000)^2+
                             (radius*149597871000)^2)
  transit_time <- (2 * sqrt(transit_distance/9.8))/(24*60*60)
  
  #determine pressure
  if(type=="Terrestrial" | (type=="Giant Terrestrial" & roll_d6(1)==6)) {
    pressure <- c(rep("Vacuum",2),"Trace",rep("Low",2),rep("Normal",2),
                  rep("High",2),rep("Very High",2))[roll_d6(2)-1]
  }
  
  #base temperature - this will be changed later if inhabitable
  if(is.na(temperature) & 
     !((type=="Giant Terrestrial" & pressure=="Very High") | 
       type=="Gas Giant" |
       type=="Ice Giant" |
       type=="Empty")) {
    pressure_multiplier <- 1
    if(!is.na(pressure)) {
      if(pressure=="Low") {
        pressure_multiplier <- 0.95
      } else if(pressure=="Normal") {
        pressure_multiplier <- 0.9
      } else if(pressure=="High") {
        pressure_multiplier <- 0.8
      } else if(pressure=="Very High") {
        pressure_multiplier <- 0.5
      }
    }
    temperature <- 277*system_data$luminosity^(0.25)*sqrt(1/(pressure_multiplier*radius))
  }
  
  #set life zone position modifier to one and change for habitable later
  life_mod <- 1
  #escape velocity modifier
  escape_velocity_mod <- escape_velocity/11186
    
  if(type=="Terrestrial" | (type=="Giant Terrestrial" & pressure != "Very High")) {
    habitable_roll <- roll_d6(2)
    #tweak: if we know this planet slot must be inhabited, then do not add
    #system habitability modifiers
    if(!habitable_system) {
      habitable_roll <- habitable_roll+system_data$habitability
    }
    if(pressure=="Low" | pressure=="High") {
      habitable_roll <- habitable_roll-1
    }
    if(type=="Giant Terrestrial") {
      habitable_roll <- habitable_roll-2
    }
    
    #check for habitability
    if(life_zone & pressure!="Vacuum" & pressure!="Trace" & pressure!="Very High" & habitable_roll>=9) {
      
      ## We have a habitable planet, lets do some additional rolls
      inhabitable <- TRUE
      life_mod <- (radius-system_data$distance_inner_au)/(system_data$distance_outer_au-system_data$distance_inner_au)
      composition <- "Nitrogen and Oxygen, plus trace gases"
      
      #temperature
      temp_roll <- round(roll_d6(2)*life_mod)
      if(pressure=="Low") {
        temp_roll <- temp_roll+1
      }
      if(pressure=="High") {
        temp_roll <- temp_roll-1
      }
      if(temp_roll<=0) {
        temperature <- 317
      } else if(temp_roll<5) {
        temperature <- 307
      } else if(temp_roll<10) {
        temperature <- 297
      } else {
        temperature <- 287
      }
        
      if(more_gradation) {
        temperature <- (temperature - 5)+sample(0:9,1)
      }
      
      ## Highest life form roll
      life_roll <- roll_d6(2)+system_data$habitability
      if(life_roll<=0) {
        life <- "Microbes"
      } else if(life_roll==1) {
        life <- "Plants"
      } else if(life_roll==2) {
        life <- "Insects"
      } else if(life_roll<5) {
        life <- "Fish"
      } else if(life_roll<7) {
        life <- "Amphibians"
      } else if(life_roll<9) {
        life <- "Reptiles"
      } else if(life_roll<11) {
        life <- "Birds"
      } else {
        life <- "Mammals"
      }
    }
    
    
    ## Atmospheric composition
    if(inhabitable) {
      atmo_roll <- roll_d6(2)
      if(type=="Giant Terrestrial") {
        atmo_roll <- atmo_roll-2
      }
      ## Another tweak here. The proportion of tainted atmospheres is quite high.
      ## presumably settlers would have selected on non-tainted worlds so add a bonus here
      ## if this needs to be a habitable system. But we also want to allow
      ## for some toxic environments, so lets change the values on the 
      ## atmospheric composition table
      if(atmo_roll<=2 & roll_d6(1)<4) {
        atmosphere <-  "Toxic (Poisonous)"
      } else if(atmo_roll<=3 & roll_d6(1)<=4) {
        atmosphere <- "Tainted (Poisonous)"
      } else {
        atmosphere <- "Breathable"
      }
    } else {
      if(pressure=="Vacuum") {
        atmosphere <-  "None"
      } else {
        atmosphere <- "Toxic (Poisonous)"
        #determine composition
        base_roll <- roll_d6(2)-1
        secondary_roll <- roll_d6(2)-1
        if(outer) {
          base_roll <- base_roll - 2
          secondary_roll <- secondary_roll - 2
        } else if(!life_zone) {
          base_roll <- base_roll + 2
          secondary_roll <- secondary_roll + 2
        }
        if(escape_velocity>12000) {
          base_roll <- base_roll - 1
          secondary_roll <- secondary_roll - 1
        } else if(escape_velocity<7000) {
          base_roll <- base_roll + 1
          secondary_roll <- secondary_roll + 1
        }
        base_roll <- min(max(base_roll, 1),11)
        secondary_roll <- min(max(secondary_roll, 1),11)
        base <- c(rep("Methane",2),
                  rep("Ammonia",2),
                  rep("Nitrogen",4),
                  rep("Carbon Dioxide",3))[base_roll]
        secondary <- c("Methane",
                       rep("Ammonia",4),
                       rep("Carbon Dioxide",3),
                       rep("Nitrogen",3))[secondary_roll]
        trace_roll <- roll_d6(2)-1
        trace_table <- c("Chlorine","None","Sulfur Dioxide",
                         "Carbon Dioxide","Argon","Methane",
                         "Water Vapor","Argon","Nitrous Oxide")
        if(trace_roll==2) {
          trace <- ""
        } else if(trace_roll==10) {
          trace_roll <- roll_d6(2)-1
          while(trace_roll>9 | trace_roll==2) {
            trace_roll <- roll_d6(2)-1
          }
          trace <- paste(", plus trace amounts of", 
                         trace_table[trace_roll])
          first_roll <- trace_roll
          trace_roll <- roll_d6(2)-1
          while(trace_roll>9 | trace_roll==2 | trace_roll==first_roll) {
            trace_roll <- roll_d6(2)-1
          }
          trace <- paste(trace, "and", 
                         trace_table[trace_roll])
        } else if(trace_roll==11) {
          trace_roll <- roll_d6(2)-1
          while(trace_roll>9 | trace_roll==2) {
            trace_roll <- roll_d6(2)-1
          }
          trace <- paste(", plus trace amounts of", 
                         trace_table[trace_roll])
          special_trace <- c("Helium","Complex Hydrocarbons",
                             "Nitric Acid","Phosphine",
                             "Hydrogen Peroxide","Hydrochloric Acid",
                             "Hydrogen Sulfide","Simple Hydrocarbons",
                             "Sulfuric Acid","Carbonyl Sulfide",
                             "Hydrofluoric Acid")[roll_d6(2)-1]
          trace <- paste(trace, "and", special_trace)
        } else {
          trace <- paste(", plus trace amounts of", 
                         trace_table[trace_roll])
        }
        if(base==secondary) {
          composition <- paste(base, trace, sep="")
        } else {
          composition <- paste(base, " and ", secondary, trace, sep="")
        }
        #change to caustic atmosphere depending on trace
        if(grepl("Chlorine", trace) | 
           grepl("Sulfur Dioxide", trace) |
           grepl("Nitric Acide", trace) |
           grepl("Hydrogen Peroxide", trace) |
           grepl("Hydrochloric Acid", trace) |
           grepl("Sulfuric Acid", trace) |
           grepl("Hydrofluoric Acid", trace)) {
          atmosphere <- "Toxic (Caustic)"
        }
      }
    }
    
    #per the optional rules, we will check for water on terrestrials
    #that meet certain conditions, regardless of habitability
    if(!inhabitable & (gravity<0.5 | 
                       pressure=="Vacuum" | pressure=="Trace" |
                       is.na(temperature) | temperature>323)) {
      water <- 0
      continents <- NA
    } else {
      ##Surface water roll
      water_roll <- round(roll_d6(2)*life_mod*escape_velocity_mod)
      if(type=="Giant Terrestrial") {
        water_roll <- water_roll+3
      }
      #another tweak to allow for more water on inhabited planets
      if(habitable_system) {
        water_roll <- water_roll+2
      }
      if(water_roll<0) {
        water <- 0
      } else if(water_roll==0) {
        water <- 5
      } else if(water_roll==1) {
        water <- 10
      } else if(water_roll==2) {
        water <- 20
      } else if(water_roll==3) {
        water <- 30
      } else if(water_roll<6) {
        water <- 40
      } else if(water_roll<8) {
        water <- 50
      } else if(water_roll<9) {
        water <- 60
      } else if(water_roll<10) {
        water <- 70
      } else if(water_roll<11) {
        water <- 80
      } else if(water_roll<12) {
        water <- 90
      } else {
        water <- 100
      }
      
      if(more_gradation) {
        #since we do not need things to be in 10 point increments, lets add single
        #digit variation fot water amount
        if(water>=5 & water<=95) {
          water <- (water-5)+sample(0:9,1)
        }
      }
      
      #continents
      if(water==0) {
        continents <- NA
      } else if(water==100) {
        continents <- 0
      } else {
        continents <- roll_d6(1)
        if(diameter<9000) {
          continents <- continents/2
        }
        if(water<30) {
          continents <- continents/2
        }
        if(diameter>15000) {
          continents <- continents*1.5
        }
        if(water>60) {
          continents <- continents*1.5
        }
        continents <- round(continents)
      }
    }
  }
  
  #calculate moons
  moon_roll <- roll_d6(1)
  if(type=="Dwarf Terrestrial") {
    if(moon_roll<3) {
      moons_medium <- max(0,roll_d6(1)-5)
      moons_small <- max(0,roll_d6(1)-3)
    } else if(moon_roll<5) {
      moons_small <- max(0,roll_d6(1)-2)
    } 
  } else if(type=="Terrestrial") {
    if(moon_roll<3) {
      moons_large <- max(0,roll_d6(1)-5)
    } else if(moon_roll<5) {
      moons_medium <- max(0,roll_d6(1)-3)
      moons_small <- max(0,roll_d6(1)-3)
    } else {
      moons_small <- max(0,roll_d6(2)-4)
      if(!inhabitable & roll_d6(1)==6) {
        rings <- TRUE
      }
    }
  } else if(type=="Giant Terrestrial") {
    if(moon_roll<3) {
      moons_giant <- max(0,roll_d6(1)-5)
      moons_small <- max(0,roll_d6(1)-3)
    } else if(moon_roll<5) {
      moons_large <- max(0,roll_d6(1)-4)
      moons_medium <- max(0,roll_d6(1)-3)
      moons_small <- max(0,roll_d6(1)-2)
    } else {
      moons_medium <- max(0,roll_d6(1)-3)
      moons_small <- max(0,roll_d6(2))
      if(!inhabitable & roll_d6(1)>=5) {
        rings <- TRUE
      }
    }
  } else if(type=="Gas Giant") {
    if(moon_roll<3) {
      moons_giant <- max(0,roll_d6(1)-4)
      moons_large <- max(0,roll_d6(1)-1)
      moons_medium <- max(0,roll_d6(1)-2)
      moons_small <- max(0,roll_d6(5))
      if(roll_d6(1)>=4) {
        rings <- TRUE
      }
    } else if(moon_roll<5) {
      moons_large <- max(0,roll_d6(1)-3)
      moons_medium <- max(0,roll_d6(1)-2)
      moons_small <- max(0,roll_d6(5))
      if(roll_d6(1)>=3) {
        rings <- TRUE
      }
    } else {
      moons_large <- max(0,roll_d6(1)-4)
      moons_medium <- max(0,roll_d6(1)-3)
      moons_small <- max(0,roll_d6(5))
      if(roll_d6(1)>=3) {
        rings <- TRUE
      }
    }
  } else if(type=="Ice Giant") {
    if(moon_roll<3) {
      moons_giant <- max(0,roll_d6(1)-4)
      moons_large <- max(0,roll_d6(1)-3)
      moons_small <- max(0,roll_d6(2))
    } else if(moon_roll<5) {
      moons_large <- max(0,roll_d6(1)-3)
      moons_medium <- max(0,roll_d6(1)-2)
      moons_small <- max(0,roll_d6(2))
      if(roll_d6(1)>=4) {
        rings <- TRUE
      }
    } else {
      moons_large <- max(0,roll_d6(1)-4)
      moons_medium <- max(0,roll_d6(1)-3)
      moons_small <- max(0,roll_d6(2))
      if(roll_d6(1)>=4) {
        rings <- TRUE
      }
    }
  }
 
  return(list(type=type, orbital_dist=radius, 
              inhabitable=inhabitable, life_zone=life_zone,
              pressure=pressure, atmosphere=atmosphere, composition=composition,
              gravity=round(gravity,2), temperature=round(temperature-273.15), 
              water=water, continents=continents, life=life, 
              day_length=day, year_length=round(year_length,1),
              transit_time=round(transit_time,2),
              diameter=diameter, density=round(density,4), 
              escape_velocity=round(escape_velocity),
              orbital_velocity=round(orbital_velocity),
              moons_giant=moons_giant, moons_large=moons_large, 
              moons_medium=moons_medium, moons_small=moons_small,
              rings=rings))
  
}

#faction type should be either "Clan", "IS", "Periphery", "Minor"
add_colonization <- function(system, distance_terra, current_year,
                             founding_year, faction_type) {
  
  faction_type <- factor(faction_type,
                         levels=c("IS","Clan","Periphery","Minor"),
                         labels=c("Inner Sphere","Clan",
                                  "Major Periphery","Minor Periphery"))
  if(is.na(faction_type)) {
    warning("Incorrect faction type. Must be \"Clan\", \"IS\", \"Periphery\", or \"Minor\". Assuming Inner Sphere.")
    faction_type <- factor("IS",
                           levels=c("IS","Clan","Periphery","Minor"),
                           labels=c("Inner Sphere","Clan",
                                    "Major Periphery","Minor Periphery"))
  }
  
  years_since_founding <- current_year-founding_year
  #CGL dates the end of SL era as 2764
  founding_sleague <- founding_year <= 2764
  
  system$planets$population <- NA
  system$planets$tech <- factor(NA,
                                levels=1:6,
                                labels=c("X","F","D","C","B","A"), 
                                ordered=TRUE)
  system$planets$output <- factor(NA,
                                  levels=1:5,
                                  labels=c("F","D","C","B","A"), 
                                  ordered=TRUE)
  system$planets$industry <- factor(NA,
                                    levels=1:5,
                                    labels=c("F","D","C","B","A"), 
                                    ordered=TRUE)
  system$planets$raw <- factor(NA,
                               levels=1:5,
                               labels=c("F","D","C","B","A"), 
                               ordered=TRUE)
  system$planets$agriculture <- factor(NA,
                                       levels=1:5,
                                       labels=c("F","D","C","B","A"), 
                                       ordered=TRUE)
  
  system$planets$hpg <- factor(NA,
                               levels=1:5,
                               labels=c("X","D","C","B","A"), 
                               ordered=TRUE)
  
  system$recharge <- list(nadir=FALSE, zenith=FALSE)
  
  for(slot in which(system$planets$inhabitable)) {
    
    planet <- system$planets[slot,]
    
    #population
    high_roll <- roll_d6(1)
    
    ##Tweaks: the clan numbers are super low and produce populations of 
    ## around 245,000 on average. Based on existing numbers this is low
    ## by an order of magnitude of 100, so we add two zeros to the base
    ## number here to get reasonable numbers. It shouldn't matter for existing
    ## worlds because we have population numbers for all clan homeworlds
    if(faction_type=="Clan") {
      if(high_roll>=5) {
        population <- 5000000*roll_d6(3)
      } else {
        population <- 1000000*roll_d6(3)
      }
    } else {
      #tweak: we are now going to use a function that we derived from applying
      #statistical models to the numbers from CamOps pg. 123. This will give us a 
      #smoother population distribution as a function of light years from Terra, while
      #still preserving variation from the roll. The script population_distance_models.R
      #contains the details. The star league model is a basic exponential decay model
      #while the more recent founding uses a spline model because of the peak around 500-600LY
      base_roll <- roll_d6(4)
      if(founding_sleague) {
        #base_roll <- base_roll+roll_d6(2)
        if(high_roll==6) {
          #we should probably put a cap on this or we will get somewhat ridiculously high numbers at zero distance
          #averageing about 17 billion. If we cap at 200LY from terra, then we will average 5.2 billion for high
          #rolls with a max of around 12 billion. 
          base_size <- exp(21.2080664-0.0059444*max(distance_terra, 200))
        } else {
          base_size <- exp(19.1844368-0.0064085*distance_terra)
        }
      } else {
        #need splines 
        before_spline <- distance_terra-550
        before_spline[before_spline>0] <- 0
        after_spline <- distance_terra-550
        after_spline[after_spline<0] <- 0
        if(high_roll==6) {
          base_size <- exp(16.0763067+0.0152113*before_spline-0.0072360*after_spline)
        } else {
          base_size <- exp(13.7805152+0.0152339*before_spline-0.0077084*after_spline)
        }
      }
      population <- base_size*base_roll
    }
    
    #check for modifiers
    modifier <- 1
    #uninhabitable
    if(grepl("Toxic", planet$atmosphere) | 
       planet$pressure=="Very High" |
       planet$pressure=="Vacuum" |
       planet$pressure=="Trace" | 
       planet$gravity>1.5) { 
      modifier <- modifier * 0.05
    }
    if(grepl("Tainted", planet$atmospher)) {
      modifier <- modifier * 0.8
    }
    if(planet$temperature>=38.85) {
      modifier <- modifier * 0.8
    }
    if(planet$gravity<0.8 | planet$gravity>1.2) {
      modifier <- modifier * 0.8
    }
    if(planet$water<40) {
      modifier <- modifier * 0.8
    }
    #ANOTHER TWEAK: to avoid ridiculous growth rates when we reverse project
    #populations, we need to apply some kind of reduction on colonies that 
    #were founded Star league era and earlier but late within that period.
    #lets try a uniform reduction in percent down to a max reduction of 0.5
    #for every year after 2500
    if(founding_sleague & founding_year>2500) {
      founding_mod <- 0.5 + 0.5*(2764 - founding_year)/(2764-2500)
      modifier <- modifier * founding_mod
    }
    
    planet$population <- modifier*population
    
    # USILR Codes
    #we will use numbers until done (also reverse the coding for christ sakes) 
    #watch out for tech. This one has two additional levels at either end
    #that the others dont have so the numbers dont line up.
    
    ##Tech
    tech <- 4
    if(faction_type=="Clan") {
      tech <- tech+1.5
    } else if(founding_sleague) {
      tech <- tech+0.5
    }
    if(faction_type!="Clan" & planet$population>(1*10^9)) {
      tech <- tech+1
    } else if(faction_type!="Clan" & planet$population<(1*10^8)) {
      tech <- tech-1
      if(planet$population<(1*10^6)) {
        tech <- tech-0.5
      }
    }
    if(faction_type=="Minor Periphery") {
      tech <- tech-0.25
    }
    #Tweak: apply gamma distribution to tech rating to create variation. A scale
    #of 0.2 seems to work pretty nicely in keepint it roughly in bounds
    tech <- round(rgamma(1, max(tech,0)/.2,scale=0.2))
    #Tweak: dont allow regressed and advanced worlds through random generation
    planet$tech <- factor(max(min(6,tech),2),
                                  levels=1:7,
                                  labels=c("X","F","D","C","B","A","A+"), 
                                  ordered=TRUE)
    
    #industry
    industry <- 2.5
    if(planet$tech>="B") {
      industry <- industry+0.75
    } else if(planet$tech<="F") {
      industry <- industry-0.75
    }
    if(faction_type=="Clan") {
      industry <- industry+1
    }
    if(faction_type!="Clan" & planet$population>(1*10^9)) {
      industry <- industry+1
      if(planet$population>(4*10^9)) {
        industry <- industry+1
      }
    } else if(faction_type!="Clan" & planet$population<(1*10^8)) {
      industry <- industry-0.5
      if(planet$population<(1*10^6)) {
        industry <- industry-0.25
      }
    }
    industry <- round(rgamma(1, max(industry,0)/.1,scale=0.1))
    planet$industry <- factor(max(min(5,industry),1),
                              levels=1:5,
                              labels=c("F","D","C","B","A"), 
                              ordered=TRUE)
    
    #output
    output <- 3
    if(faction_type=="Clan") {
      output <- output+0.75
    } else if(planet$population>(1*10^9)) {
      output <- output+1
    }
    if(planet$tech>="A") {
      output <- output+0.5
    } else if(planet$tech<="D") {
      output <- output-0.5
      if(planet$tech=="X") {
        output <- output-1
      }
    }
    if(planet$industry>="B") {
      output <- output+0.5
    } else if(planet$industry<="D") {
      output <- output-0.5
    }
    output <- round(rgamma(1, max(output,0)/.1,scale=0.1))
    planet$output <- factor(max(min(5,output),1),
                            levels=1:5,
                            labels=c("F","D","C","B","A"), 
                            ordered=TRUE)
    
    #raw materials
    raw <- 4
    if(planet$tech>="C") {
      raw <- raw+1
      if(planet$tech=="A+") {
        raw <- raw+1
      }
    }
    if(planet$density>5.5) {
      raw <- raw+1
    } else if(planet$density<4) {
      raw <- raw-1
    }
    if(faction_type!="Clan" & planet$population>(3*10^9)) {
      raw <- raw-1
    }
    #tweak - because clan pops are small and they have high
    #tech and industry, they get ridiculos raw materials, so 
    #we apply a straight penalty to take it down, plus
    #clan planets fluffed as resource poor 
    if(faction_type=="Clan") {
      raw <- raw-1.5
    }
    
    if(planet$output>="B") {
      raw <- raw-1
    }
    if(years_since_founding>250) {
      raw <- raw-1
    }
    raw <- round(rgamma(1, max(raw,0)/.1,scale=0.1))
    planet$raw <- factor(max(min(5,raw),1),
                         levels=1:5,
                         labels=c("F","D","C","B","A"), 
                         ordered=TRUE)
    
    #agriculture
    agriculture <- 3
    if(planet$tech>="B") {
      agriculture <- agriculture+1
    } else if(planet$tech<="F") {
      agriculture <- agriculture-1
    }
    if(planet$industry>="C") {
      agriculture <- agriculture+1
    }
    if(faction_type!="Clan" & planet$population>(1*10^9)) {
      agriculture <- agriculture-1
      if(planet$population>(5*10^9)) {
        agriculture <- agriculture-1
      }
    }
    #tweak - clan planets are fluffed as pretty barren
    if(faction_type=="Clan") {
      agriculture <- agriculture-1
    }
    if(planet$water<50) {
      agriculture <- agriculture-1
    }
    if(grepl("Tainted", planet$atmosphere)) {
      agriculture <- agriculture-1
    }
    if(grepl("Toxic", planet$atmosphere)) {
      agriculture <- agriculture-2
    }
    agriculture <- round(rgamma(1, max(agriculture,0)/.1,scale=0.1))
    planet$agriculture <- factor(max(min(5,agriculture),1),
                                 levels=1:5,
                                 labels=c("F","D","C","B","A"), 
                                 ordered=TRUE)

    #HPG Status
    #Tweak: the -1 per 100 LY from Terra was very harsh and 
    #produced distributions that did not match at all what the 
    #text of CamOps on pg. 132-133. We found that reducing
    #this penalty to -1 per 250LY produced much more reasonable
    #results. However, its worth noting that almost any LY penalty
    #will result in minor periphery powers having no HPG exclusively
    
    #After testing this out, the penalties are still too severe for distant
    #planets, so that you don't get a connected circuit. Ideally, there should
    #be some interaction with tech, so that high tech worlds are more likely to
    #get an HPG the further out they get.
    
    hpg_roll <- roll_d6(2)-floor(distance_terra/250)
    if(planet$population<(1*10^9)) {
      hpg_roll <- hpg_roll-1
    } else if(planet$population>(2*10^9)) {
      hpg_roll <- hpg_roll+1
    }
    if(planet$tech<="D") {
      hpg_roll <- hpg_roll-1
    }
    if(planet$industry<="D") {
      hpg_roll <- hpg_roll-1
    }
    if(planet$tech>="B" & distance_terra>250) {
      hpg_roll <- hpg_roll+1
      if(planet$tech>="A") {
        #remove distance penalty for A-rated planets
        hpg_roll <- hpg_roll+floor(distance_terra/250)
      }
    }
    if(current_year<2800) {
      hpg_roll <- hpg_roll+2
    }
    
    
    hpg_roll <- max(min(hpg_roll,12),1)
    
    hpg_table_is <- c(3,rep(4,9),rep(5,2))
    hpg_table_periphery <- c(2,3,rep(4,9),5)
    hpg_table_minor <- c(rep(1,10),3,4)
    
    hpg <- hpg_table_is[hpg_roll] 
    if(faction_type=="Clan") {
      #clan worlds are always A rated (pg. 133 CamOps)
      hpg <- 5
    } else if(faction_type=="Major Periphery") {
      hpg <- hpg_table_periphery[hpg_roll] 
    } else if(faction_type=="Minor Periphery") {
      hpg <- hpg_table_minor[hpg_roll] 
    } 
    
    #if a planet was founded after 2700, then don't allow
    #it to be a member of the first circuit
    if(founding_year>2700 & hpg==5 & faction_type!="Clan") {
      hpg==4
    } 
    
    planet$hpg <- factor(hpg,
                         levels=1:5,
                         labels=c("X","D","C","B","A"), 
                         ordered=TRUE)
    
    #ok, ready to update the planet
    system$planets[slot,] <- planet
    
  }
  
  #Recharge stations
  #we follow the table closely here but found it produced about 10% of IS systems
  #with a recharge stations whereas CamOps suggests a higher number less than 25% but
  #closer to it. So we added a positive modifier for planets with an industry rating of B
  #or higher
  recharge_roll <- roll_d6(2)
  if(max(system$planets$population,na.rm=TRUE)>=(2*10^9)) {
    recharge_roll <- recharge_roll+1
  } else if(max(system$planets$population,na.rm=TRUE)<(1*10^9)) {
    recharge_roll <- recharge_roll-1
  }
  if(max(system$planets$tech, na.rm=TRUE)<="D") {
    recharge_roll <- recharge_roll-1
  }
  if(max(system$planets$tech, na.rm=TRUE)>="A") {
    recharge_roll <- recharge_roll+1
  }
  if(max(system$planets$industry, na.rm=TRUE)<="D") {
    recharge_roll <- recharge_roll-1
  }
  if(max(system$planets$industry, na.rm=TRUE)>="B") {
    recharge_roll <- recharge_roll+1
  }
  if(current_year<2800) {
    recharge_roll <- recharge_roll+2
  }
  
  recharge_roll <- max(min(recharge_roll-1,11),1)
  
  recharge_is <- c(rep(0,8),rep(1,2),1)
  recharge_clan <- c(0,rep(1,7),rep(2,3))
  recharge_periphery <- c(rep(0,8),rep(1,3))
  recharge_minor <- c(rep(0,10),1)
  
  recharge_n <- recharge_is[recharge_roll] 
  if(faction_type=="Clan") {
    recharge_n <- recharge_clan[recharge_roll] 
  } else if(faction_type=="Major Periphery") {
    recharge_n <- recharge_periphery[recharge_roll] 
  } else if(faction_type=="Minor Periphery") {
    recharge_n <- recharge_minor[recharge_roll] 
  } 
  
  if(recharge_n>1) {
    system$recharge$nadir <- system$recharge$zenith <- TRUE
  } else if(recharge_n==1) {
    if(sample(1:2,1)==2) {
      system$recharge$nadir <- TRUE
    } else {
      system$recharge$zenith <- TRUE
    }
  }

  return(system)
}


#### Population Projection Functions ####

#TODO: put some hard upper limit on population sizes (i.e. carrying capacity)
#project population from year 3067 forwards and backwards
project_population <- function(base_pop, found_year, faction_type, border_distance, 
                               agriculture, terran_hegemony=FALSE,
                               p2750=NULL, p3025=NULL, p3067=NULL, p3079=NULL, p3145=NULL) {
  
  #base year is 3067
  base_year <- 3067
  if(!is.null(p3067)) {
    base_pop <- p3067
  } 
  
  if(found_year>base_year) {
    #a very small number of cases but we need to handle them separately
    #assume population is 3145 and go back to founding size of 50000
    full_growth_rates <- get_gompertz_rates(base_pop, 50000, 3145-found_year+1)
    #reverse project
    len <- length(full_growth_rates)
    full_pop <- base_pop/exp(c(cumsum(full_growth_rates[len:1])[len:1],0))
    names(full_pop) <- paste(found_year:3145)
    return(full_pop)
  }
  
  #get growth to end of DA first. If there are valid 3079 or 3145 dates, but not
  #3067, then ignore current base pop and reassign at the end.
  ##TODO: need to deal with some post 3067 foundings
  growth <- 0.001
  if(!is.null(p3079)) {
    if(!is.null(p3067)) {
      growth <- log(p3079/p3067)/(3079-3067)
    }
    growth_jihad <- growth_simulation(growth, 3079-3067, messiness=2)
    growth <- 0.001
    if(!is.null(p3145)) {
      growth <- log(p3145/p3079)/(3145-3079)
    }
    growth_da <- growth_simulation(growth, 3145-3079, messiness=2)
    growth_da <- c(growth_jihad, growth_da)
  } else {
    if(!is.null(p3145) & !is.null(p3067)) {
      growth <- log(p3145/p3067)/(3145-3067)
    }
    growth_da <- growth_simulation(growth, 3145-3067, messiness=2)
  }
  
  #check to see if I need to reassign 3067 date
  if(!is.null(p3079) & is.null(p3067)) {
    base_pop <- p3079/exp(sum(growth_jihad))
  } else if(!is.null(p3145) & is.null(p3067)) {
    base_pop <- p3145/exp(sum(growth_da))
  }
  
  if(faction_type=="Clan") {
    if(found_year<=2787) {
      #pentagon worlds
      #Op Klondike says about 6 million in Exodus fleet, so that should be an initial colonization size
      #of 1.2 million per pentagon planet. However, Op Klondike also says the population ws 4.5 million
      #after second exodus and about 2 million upon return. some of this is due to SL forces not being
      #counted here. Probably best to assume about 1.2 million with no growth rate because of 
      #founding of other colonies and then create artificial decline in 2802
      colony_size <- 1200000
      early_growth <- c(growth_simulation(0,2801-found_year), log(0.9/1.2))
      pop_second_exodus <- colony_size*exp(sum(early_growth))

      #now do SW style depopulation with the decline being about to 45% of population through 2822
      decline_rate <- log(0.45)/(2822-2802)
      #now random walk it
      growth_pent_wars <- growth_simulation(decline_rate, 2822-2802)
      #resample until I get something in the ballpark of the overall decline
      while(abs(exp(sum(growth_pent_wars))-0.45)>0.05) {
        growth_pent_wars <- growth_simulation(decline_rate, 2822-2802)
      }
      pop_post_war <- pop_second_exodus*exp(sum(growth_pent_wars))
     
      #now we are into the golden century, so gompertz to the base_pop
      growth_golden <- get_gompertz_rates(base_pop, pop_post_war, base_year-2822+1)
      #add noise
      growth_golden <- growth_golden+growth_simulation(0,base_year-2822)
      
      full_growth_rates <- c(early_growth, growth_pent_wars, growth_golden)
      
    } else {
      #kerensky cluster
      #Just use a straightforward gompertz curve. assume smaller initial colonization size for clans
      full_growth_rates <- get_gompertz_rates(base_pop, 10000, base_year-found_year+1)
      #add noise
      full_growth_rates <- full_growth_rates+growth_simulation(0,base_year-found_year)
      if(found_year<2802) {
        full_growth_rates[2802-2790+1] <- 2
      }
    }
  } else {
    #our population creation method in add_colonization assumes 2764, but I think 
    #we need to go back a little earlier or we will get some very weird growth rates for
    #colonies founded right before that period. 
    if(found_year>2700) {
      #just a straightforward gompertz curve
      full_growth_rates <- get_gompertz_rates(base_pop, 50000, base_year-found_year+1)
      #add noise
      full_growth_rates <- full_growth_rates+growth_simulation(0,base_year-found_year)
    } else {
      
      #lets assume a very slight average increase from 3025 to present
      growth_rate <- 0.001
      
      #check for existing population counts in 3025 and/or 2067 and adjust growth rate
      if(!is.null(p3025)) {
        growth_rate <- log(base_pop/p3025)/(base_year-3025)
      } 
      
      growth_post_sw <- growth_simulation(growth_rate, base_year-3025)
      pop_3sw <- base_pop/exp(sum(growth_post_sw))
      if(!is.null(p3025)) {
        while(abs((pop_3sw-p3025)/p3025)>0.02) {
          growth_post_sw <- growth_simulation(growth_rate, base_year-3025)
          pop_3sw <- base_pop/exp(sum(growth_post_sw))
        }
      }

      #now lets model succession wars depopulation. First sample an overall 
      #depopulation ratio for the whole period.
      sw_decline <- sample(seq(from=5,to=70,by=1), 1)
      #adjust by current pop
      if(base_pop>5000000000) {
        sw_decline <- sample(seq(from=1,to=10,by=1), 1)
      } else if(base_pop>1000000000) {
        sw_decline <- sample(seq(from=5,to=30,by=1), 1)
      } else if(base_pop>100000000) {
        sw_decline <- sample(seq(from=5,to=50,by=1), 1)
      }
      #if terran hegemony or border region then hit harder
      if(terran_hegemony | border_distance<=60) {
        sw_decline <- sw_decline+sample(seq(from=5,to=15,by=1), 1)
      }

      #if not IS, then not hit as hard
      if(faction_type!="IS") {
        sw_decline <- sw_decline-sample(seq(from=10,to=25,by=1), 1)
      }
      #agriculture makes you more self reliant
      if(agriculture=="A") {
        sw_decline <- sw_decline-sample(seq(from=5,to=15,by=1), 1)
      } else if(agriculture=="B") {
        sw_decline <- sw_decline-sample(seq(from=0,to=10,by=1), 1)
      } else if(agriculture=="C") {
        sw_decline <- sw_decline+sample(seq(from=0,to=15,by=1), 1)
      } else if(agriculture=="D") {
        sw_decline <- sw_decline+sample(seq(from=10,to=25,by=1), 1)
      } else {
        sw_decline <- sw_decline+sample(seq(from=20,to=35,by=1), 1)
      }
      
      sw_ratio <- 1-min(sw_decline,95)/100
      
      #if we have a star league pop size, then use that instead
      if(!is.null(p2750)) {
        sw_ratio <- pop_3sw/p2750
      }
      
      #calculate the end point based on being a part of the Terran Hegemony or not
      sl_peak <- 2785
      if(terran_hegemony) {
        sl_peak <- 2767
      }
      
      #use overall ratio to calculate average annual rate of decline.
      sw_decline_rate <- log(sw_ratio)/(3025-sl_peak)
      #now random walk it
      growth_sw <- growth_simulation(sw_decline_rate, 3025-sl_peak, messiness = 3)
      #resample until I get something in the ballpark of the overall decline
      tolerance <- 0.1
      if(!is.null(p2750)) {
        tolerance <- 0.01
      }
      while(abs(exp(sum(growth_sw))-sw_ratio)>tolerance) {
        growth_sw <- growth_simulation(sw_decline_rate, 3025-sl_peak, messiness = 3)
      }
      pop_sl <- pop_3sw/exp(sum(growth_sw))
      if(!is.null(p2750)) {
        pop_sl <- p2750
      }
  
      #from colonization to end of star league, fit a gompertz
      growth_initial <- get_gompertz_rates(pop_sl, 50000, sl_peak-found_year+1)
      #add noise
      growth_initial <- growth_initial+growth_simulation(0,sl_peak-found_year)
 
      #put it all together
      full_growth_rates <- c(growth_initial, growth_sw, growth_post_sw)
    }
  }
  
  #reverse projecting is easy
  len <- length(full_growth_rates)
  full_pop <- base_pop/exp(c(cumsum(full_growth_rates[len:1])[len:1],0))
  
  #finally, add forward projection to 3145
  full_pop <- c(full_pop, base_pop*exp(cumsum(growth_da)))
  
  #replace with any canon values that are not null
  names(full_pop) <- paste(found_year:3145)
  if(!is.null(p3025)) {
    full_pop["3025"] <- p3025
  }
  if(!is.null(p2750)) {
    full_pop[paste(sl_peak)] <- p2750
  }
  if(!is.null(p3079)) {
    full_pop["3079"] <- p3079
  }
  if(!is.null(p3145)) {
    full_pop["3145"] <- p3145
  }
  
  #plot(found_year:3145, full_pop, type="l", ylim=c(0, max(full_pop)))
  #abline(h=50000, col="green", lwd=2)
  #abline(v=2785, lty=2, col="red")
  #abline(v=3029, lty=2, col="red")
  
  return(full_pop)
}

#simulate a random walk of population growth rates for a given amount
#of time. 
# average - the average growth over the time period
# length - number of years to simulate
# increment - how much to increment the random walk by each time
# penalize - the mean value (as difference from average) at which to adjust sampling
#   to pull the value back closer to the average 
# max - the value (as difference from average) for the maximum growth rate. At this
# point the sampling will only draw 0 or change in the opposite direction
growth_simulation <- function(average, length, messiness=1) {
  growth <- c(average)
  
  #increment is equal to 5% of the level times messiness factor
  increment <- abs(average * 0.1 * messiness)
  if(increment==0) {
    increment <- 0.0002*messiness
  }
  #penalize if we get five times the increment away
  penalize <- increment * 5
  #cap if we the increement equals the average or more
  max <- increment * 10
  
  for(i in 1:length) {
    sampler <- c(increment,-1*increment,0)
    if(growth[i] > (average+max)) {
      sampler <- c(rep(-1*increment,2), 0)
    } else if(growth[i] > (average+penalize)) {
      sampler <- c(increment, rep(-1*increment,5), 0)
    }
    if(growth[i] < (average-max)) {
      sampler <- c(rep(increment,2), 0)
    } else if(growth[i] < (average-penalize)) {
      sampler <- c(-1*increment, rep(increment,5), 0)
    }
    growth <- c(growth, growth[i]+sample(sampler,1))
  }
  return(growth[-1])
}

get_gompertz_rates <- function(ending_pop, start_colony_size, time_length) {
  
  if(ending_pop<start_colony_size) {
    #a bit of hack, but this can't handle negative growth so 
    #we will change starting colony size to be 90% of ending pop
    start_colony_size <- 0.9*ending_pop
  }
  
  a <- ending_pop*1.005
  b <- -log(start_colony_size/a)
  c <- -log(-log(ending_pop/a)/b)/time_length
  t <- 0:time_length
  yt = a*exp(-(b*exp(-c*t)))
  rates <- log(yt[2:time_length]/yt[1:(time_length-1)])
  return(rates)
}

#### SICS Projection Functions ####

project_sics <- function(tech, industry, raw, output, agriculture,
                         founding_year, pop, faction_type) {
  
  sics <- list(tech=tech, industry=industry, raw=raw, output=output, 
               agriculture=agriculture)
  
  #current year should be between 3040 and 3049
  current_year <- 3040 + sample(0:9, 1)
  
  #if founded in the last 10 years then just use the one SIC code
  if(founding_year>=(current_year-10)) {
    return(data.frame(year=founding_year, sics=combine_sics(sics)))
  }
  
  sics_colony <- get_colony_sics(sics, founding_year, current_year,
                                 faction_type)
  
  if(founding_year<2700) {
    
    #we need a year of SL peak, and a first and second SW decline date
    #assign these somewhat randomly by drawing from exponential distribution
    
    #SL peak year (target 2600-2750, mean 2650)
    year_sl <- round(max(2600, founding_year+10)+rexp(1,.02))
    while(year_sl>2750) {
      year_sl <- round(max(2600, founding_year+10)+rexp(1,.02))
    }
    #1st SW drop (target 2840-2880, mean 2850)
    year_1sw <- round(2840+rexp(1,.1))
    while(year_1sw>2880) {
      year_1sw <- round(2840+rexp(1,.1))
    }
    #second SW drop (target 2890-2950, mean 2900)
    year_2sw <- round(2890+rexp(1,.1))
    while(year_2sw>2950) {
      year_2sw <- round(2890+rexp(1,.1))
    }
    
    #the input values will be considered to be the SIC values resulting
    #from IS renaissance. We will assume that these value are actually
    #close to the SL peak values. Derive SL peak values by allowing for
    #a little bit of wiggle. 
    odds_drop <- c(0,0,0.05,0.05,0.1,0.1)
    odds_increase <- c(4,2,1,1,1,0)
    sics_sl <- adjust_sics(sics, odds_drop, odds_increase,
                           pop_ratio = pop[paste(year_sl)]/pop[paste(current_year)],
                           year_sl-founding_year)
    #allow for small chance (10%) of a double-move
    if(sample(1:10, 1)==1) {
      sics_sl <- adjust_sics(sics_sl, odds_drop, odds_increase,
                             pop_ratio = pop[paste(year_sl)]/pop[paste(current_year)],
                             year_sl-founding_year)
    }
    
    #now roll for diminished SICS twice during SW period
    odds_drop <- c(0,0,0.3,0.7,3,100000)
    odds_increase <- c(0,0,0,0,0,0)
    sics_sw1 <- adjust_sics(sics_sl, odds_drop, odds_increase,
                            pop_ratio = pop[paste(year_1sw)]/pop[paste(year_sl)],
                            year_1sw-founding_year)
    sics_sw2 <- adjust_sics(sics_sw1, odds_drop, odds_increase,
                            pop_ratio = pop[paste(year_2sw)]/pop[paste(year_1sw)],
                            year_2sw-founding_year)
    
    sic_changes <- rbind(interpolate_sics(sics_colony, sics_sl, founding_year, 
                                          year_sl),
                         data.frame(year=year_1sw, sics=combine_sics(sics_sw1)), 
                         interpolate_sics(sics_sw2, sics, year_2sw, current_year, 
                                          3025))
    sic_changes
  } else {
    #just interpolate between colony and now 
    sic_changes <- interpolate_sics(sics_colony, sics, founding_year, current_year)
  }
  
  #forward project to 3145 with relatively small chance of change
  year_da <- round(3090+rexp(1,.04))
  while(year_da>3145) {
    year_da <- round(3090+rexp(1,.04))
  }
  odds_drop <- c(0,0,0.05,0.05,0.05,0.05)
  odds_increase <- c(0.5,0.4,0.3,0.25,0.15,0)
  sics_da <- adjust_sics(sics, odds_drop, odds_increase,
                         pop_ratio = pop["3145"]/pop[paste(current_year)],
                         3145-founding_year)
  sic_changes <- rbind(sic_changes,
                       interpolate_sics(sics, sics_da, current_year, year_da)[-1,])
  
  
  return(sic_changes)
}

get_colony_sics <- function(sics, founding_year, current_year, faction_type) {
  
  tech_colony <- factor("C", levels=levels(sics$tech), ordered=TRUE)
  industry_colony <- factor("D", levels=levels(sics$industry), ordered=TRUE)
  output_colony <- factor("D", levels=levels(sics$output), ordered=TRUE)
  if(faction_type=="Clan") {
    #clan colonies start better
    tech_colony <- factor("B", levels=levels(sics$tech), ordered=TRUE)
    industry_colony <- factor("C", levels=levels(sics$industry), ordered=TRUE)
    output_colony <- factor("C", levels=levels(sics$output), ordered=TRUE)
  }
  #raw materials should start as the same as current (colony age will come into
  #play when we randomize a bit)
  raw_colony <- as.numeric(sics$raw)
  raw_colony <- factor(raw_colony, levels=1:5,
                       labels=c("F","D","C","B","A"), ordered=TRUE)
  #agriculture should start as one less than current
  agriculture_colony <- max(as.numeric(sics$agriculture)-1,1)
  agriculture_colony <- factor(agriculture_colony, levels=1:5,
                               labels=c("F","D","C","B","A"), ordered=TRUE)
  
  sics_colony <- list(tech=tech_colony,
                      industry=industry_colony,
                      raw=raw_colony,
                      output=output_colony,
                      agriculture=agriculture_colony)
  
  #now lets apply some randomness to these values
  odds_drop <- c(0,0,0.1,0.1,0.1,0.1)
  odds_increase <- c(0.1,0.1,0.1,0.1,0.1,0)
  sics_colony <- adjust_sics(sics_colony, odds_drop, odds_increase,
                             1, 0)
}

#randomly drop or raise SIC codes based on vectors of odds of increase 
#or decrease for each level.
adjust_sics <- function(sics, odds_drop, odds_increase,
                        pop_ratio=1, colony_age) {
  
  tech <- sics$tech[1]
  industry <- sics$industry[1]
  raw <- sics$raw[1]
  output <- sics$output[1]
  agriculture <- sics$agriculture[1]
 
  pop_modifier <- min(max(pop_ratio, 0.2), 5)
  
  tech_new <- roll_sics(tech, odds_drop/pop_modifier, odds_increase*pop_modifier)
  #get rid of bottom category for others
  odds_drop <- odds_drop[-1]
  odds_increase <- odds_increase[-1]
  agriculture_new <- roll_sics(agriculture, odds_drop, odds_increase)
  #industry and raw materials should be affected by change in tech level
  if(tech_new>tech) {
    odds_increase <- odds_increase * 2
    odds_drop <- odds_drop * 0.5
  } else if(tech_new<tech) {
    odds_increase <- odds_increase * 0.5
    odds_drop <- odds_drop * 2
  }
  industry_new <- roll_sics(industry, odds_drop, odds_increase)
  #raw materials should also be affected by timing of settlement
  age_modifier <- 1
  if(colony_age < 250) {
    age_modifier <- 2
  }
  raw_new <- roll_sics(raw, odds_drop/age_modifier, odds_increase*age_modifier)
  #output should be affectd by industry rating and technology
  if(industry_new>industry) {
    odds_increase <- odds_increase * 2
    odds_drop <- odds_drop * 0.5
  } else if(industry_new<industry) {
    odds_increase <- odds_increase * 0.5
    odds_drop <- odds_drop * 2
  }
  output_new <- roll_sics(output, odds_drop, odds_increase)
  
  return(list(tech=tech_new, industry=industry_new, raw=raw_new, 
              output=output_new, agriculture=agriculture_new))
}

#do everything in terms of odds to makae sure we stay on a scale that adds
#up to one
roll_sics <- function(value, odds_drop, odds_increase) {
  probs <- cbind(odds_drop/(1+odds_drop+odds_increase),
                 1/(1+odds_drop+odds_increase),
                 odds_increase/(1+odds_drop+odds_increase))[as.numeric(value),]
  move <- sample(c(-1,0,1), 1, prob=probs)
  return(factor(levels(value)[as.numeric(value)+move],
                levels=levels(value), ordered=TRUE))
}

combine_sics <- function(sics) {
  return(paste(simplify2array(sics), collapse="-"))
}

separate_sics <- function(sics) {
  codes <- strsplit(sics, "-")[[1]]
  tech <- factor(codes[1],
                 levels=c("X","F","D","C","B","A"),
                 ordered=TRUE)
  codes <- factor(codes[-1],
                  levels=c("F","D","C","B","A"),
                  ordered=TRUE)
  
  return(list(tech=tech, industry=codes[1], raw=codes[2], output=codes[3], 
              agriculture=codes[4]))
}

interpolate_sics <- function(sics_start, sics_end, start_year, end_year,
                             start_interval=start_year, end_interval=end_year) {
  #interpolate intermediate values of SICS between, start_year
  #does not have to be same as starting year of the SICS if you want
  #to concentrate changes at a different point
  
  start <- sapply(sics_start, as.numeric)
  end <- sapply(sics_end, as.numeric)
  
  #figure out total changes for each group
  changes <- end-start
  
  total <- rbind(start)
  
  new_values <- start
  #always change the biggest absolute differences first
  while(sum(changes!=0)!=0) {
    max_change <- max(changes)
    min_change <- min(changes)
    #if both a min and max change are possible then decide randomly which to do
    do_max_change <- max_change>0
    do_min_change <- min_change<0
    if(do_max_change & do_min_change) {
      if(sample(c(TRUE,FALSE),1)) {
        do_min_change <- FALSE
      } else {
        do_max_change <- FALSE
      }
    }
    if(do_max_change) {
      new_values[max_change==changes] <- new_values[max_change==changes]+1
    } else {
      new_values[min_change==changes] <- new_values[min_change==changes]-1
    }
    total <- rbind(total, new_values)
    changes <- end-new_values
  }
  
  rownames(total) <- NULL
  total <- as.data.frame(total)
  total$tech <- factor(total$tech,
                       levels=1:6,
                       labels=levels(sics_end$tech),
                       ordered=TRUE)
  total$industry <- factor(total$industry,
                           levels=1:5,
                           labels=levels(sics_end$industry),
                           ordered=TRUE)
  total$raw <- factor(total$raw,
                      levels=1:5,
                      labels=levels(sics_end$raw),
                      ordered=TRUE)
  total$output <- factor(total$output,
                         levels=1:5,
                         labels=levels(sics_end$output),
                         ordered=TRUE)
  total$agriculture <- factor(total$agriculture,
                              levels=1:5,
                              labels=levels(sics_end$agriculture),
                              ordered=TRUE)
  
  #figure out years of change
  n <- nrow(total)-2
  if(n>0) {
    length <- (end_interval-start_interval)
    rate <- n/length
    
    #sample from waiting times with a mean equal to the length 
    waiting_times <- round(rexp(n,rate=rate))
    while(sum(waiting_times)>=length) {
      waiting_times <- round(rexp(n,rate=rate))
    }
    years <- c(start_year, start_interval + cumsum(waiting_times), end_year)
  } else {
    years <- c(start_year, end_year)
  }
  
  data.frame(year=years,
             sics=apply(total, 1, combine_sics))
  
}

#### HPG Projection Functions ####

# First HPG was on Terra, New Year's Day 2630. According to Sarna.net, HPG system expanded to 
# all IS and Periphery planets by Amaris coup. Major damage during the war, partially repaired by
# Comstar. 

# First, project from current value back to SL era. All periphery and IS should at least have B. Some
# chance of upgrading to A. This will be most likely on old Hegemony worlds. Its possible we may want to 
# do this with some kind of search function that ensures all places connected to A at peak rather than 
# randomly. Should be about 50 A-circuits in IS.

# From there, reverse project the founding date. Assume all planets linked between 2630 and 2730. 
# Use some kind of hazard function to get exact date with planets closer to Terra more likely to get 
# early dates, unless A-rated in which case earlier. 

# From there, check for destruction of HPG networks during the war and then rebuilding after the war.
# any current B rated that were A-rated before the war should be destroyed and then rebuilt as B-rated.
# some A-rated should also be destroyed and rebuilt as A-rated. According to Sarna, First Circuit is 
# rebuilt by 2785. By 2788, Comstar has assured neutrality of most HPGs so no destruction after this.

# Clans should be handled differently. All Clans should be A from date of colony founding. Also might
# need to check some periphery dates. 

## Ok, thinking this through some more and I think we should probably just see what A-rated HPGs we 
## have in Canon currently and then use some kind of different technique entirely to find remaining A-rated
## HPGs to get to 50 with good coverage in Inner Sphere/Periphery. Then downgrade all generated HPGs 
## to B-rated in automatic generation if they came in as A-rated. Then, the following:
## 1. Upgrade pre-selected planets to A-rated. 
## 2. Find founding date of HPG for all planets. All Periphery and IS planets get B-rated Should depend on range from Terra, but First Circuit will 
##    be faster
## 3. Some chance of destruction during Amaris Coup inside Hegemony and during 1SW outside Hegemony. If 
##    less than B now, then force destruction and figure out timing of C and D rated courier services. 
##    Otherwise rebuild First Circuit by 2785 and all others by 2800. 

## TODO: Then need to figure out Dark Age blackout

project_hpg <- function(base_hpg, distance_terra, founding_year, faction_type) {
  
  #build rate for things built right at start. Average build time is one over this
  building_time <- 0.33
  
  base_hpg <- as.character(base_hpg)
  #assume base HPG applies to right before Clan era
  initial_hpg <- base_hpg

  if(faction_type=="Clan") {
    #A-rated forever so just assign at founding_year
    hpg_table <- data.frame(year=round(founding_year+rexp(1, building_time)), 
                            hpg="A")
    return(hpg_table)
  } else {
    if(faction_type!="Minor" & founding_year<2765) {
      #if not minor periphery and SL era founding or earlier, 
      #then upgrade anything less than B to B for its initial HPG
      if(initial_hpg!="B" & initial_hpg!="A") {
        initial_hpg <- "B"
      }
    }
    #assume all building between 2630 and 2730
    
    #if founding later than 2730 then assume built at start
    if(founding_year>2730) {
      build_year <- round(founding_year+rexp(1, building_time))
    } else {
      #otherwise, get hazard rate and then apply
      #baseline hazard will be 20 years
      hazard <- 1/15
      #decrease hazard by distance to Terra
      hazard <- hazard * 150/distance_terra
      #increase hazard by five for first circuit
      if(initial_hpg=="A") {
        hazard <- 5*hazard
      }
      start_year <- 2630
      if(founding_year>start_year) {
        share <- 1-(founding_year-start_year)/(2730-start_year)
        if(share<0.1) {
          share <- 0.1
        }
        hazard <- hazard/share
        start_year <- founding_year
      }
      build_year <- round(start_year+rexp(1, hazard))
      if(build_year>2730) {
        build_year <- round(2730+rexp(1,1))
      }
    }
    hpg_table <- data.frame(year=build_year, hpg=initial_hpg)
  }
  
  #if initial and base are different, then must have been downgraded
  #at some point
  if(initial_hpg!=base_hpg) {
    #lets assume it got destroyed at some point during Amaris Coup/1SW
    destroy_year <- round(min(2766+rexp(1,1/50),2900))
    hpg_table <- rbind(hpg_table, 
                       data.frame(year=destroy_year, hpg=base_hpg))
  } else if((initial_hpg=="A" | initial_hpg=="B") & distance_terra>1) {
    #otherwise apply some chance of destruction during Amaris Coup/1SW and then rebuilding
    #strong chance of destruction close to core
    odds <- 0.7/(distance_terra^(1/4))
    prob <- odds/(1+odds)
    if(sample(1:100, 1)<=(prob*100)) {
      #gets destroyed
      destroy_year <- round(min(2766+rexp(1,1/7),2800))
      hpg_table <- rbind(hpg_table, 
                         data.frame(year=destroy_year, hpg="X"))
      #when is it rebuilt. Starting 2780 or immediately after destruction
      start_year <- 2780
      if(start_year<destroy_year) {
        start_year <- destroy_year+5
      }
      rebuild_year <- round(start_year+rexp(1,1/3))
      hpg_table <- rbind(hpg_table, 
                         data.frame(year=start_year, hpg=initial_hpg))
    }
    
  }
  
  return(hpg_table)
}

#### Recharge Station Projection ####

#Lets stry doing this by just applying the CamOps method to a star league peak year. If we get 
#less or the same stations, we ignore. If we get more stations, then we assign. Choose a starting 
#date for stations. Then if it goes down from SL high to now, choose extinction dates, mostly by
#end of 1SW (2820).

# Simulations suggest that would get us to about 33% of planets in IS having at least one recharge
# station at SL peak. My sense is that it should be higher than this, so lets try adding a +3 rather
# than +2 to the adjustment.

project_recharge <- function(recharge_current, faction_type, founding_year, sics_project, pop) {
  
  #default to about 50 years after colonization, on average
  build_rate <- 0.02
  #clans build much faster, about 5 years on average
  if(faction_type=="Clan") {
    build_rate <- 0.2
  }
  #if getting close to end of SL era then speed up build rate (20 years on average)
  if(founding_year>2650) {
    build_rate <- 0.05
  }
  if(founding_year>2750) {
    build_rate <- 0.2
  }
  
  recharge_history <- tibble(year=numeric(),
                             etype=character(),
                             event=logical())
  
  nrecharge_current <- recharge_current$nadir+recharge_current$zenith
  
  if(faction_type=="Clan") {
    if(recharge_current$zenith) {
      build_year <- founding_year+round(rexp(1, build_rate))
      recharge_history <- recharge_history %>% 
        bind_rows(tibble(year=build_year,
                         etype="zenithCharge",
                         event="true"))
    }
    if(recharge_current$nadir) {
      build_year <- founding_year+round(rexp(1, build_rate))
      recharge_history <- recharge_history %>% 
        bind_rows(tibble(year=build_year,
                         etype="nadirCharge",
                         event="true"))
    }
    return(recharge_history)
  }
  
  
  #if founding year after 2765, then no chance for building recharge stations
  if(founding_year>2765) {
    return(recharge_history)
  }
  
  #get best tech and industry rating from the sics_projection  
  tech <- factor("X",
                 levels=c("X","F","D","C","B","A"),
                 ordered = TRUE)
  industry <- factor("F",
                     levels=c("F","D","C","B","A"),
                     ordered=TRUE)
  sics_project$sics <- as.character(sics_project$sics)
  for(i in 1:nrow(sics_project)) {
    sics <- separate_sics(sics_project$sics[i])
    if(sics$tech>tech) {tech <- sics$tech}
    if(sics$industry>industry) {industry <- sics$industry}
  }
  
  population_sl <- pop["2765"]

  recharge_roll <- roll_d6(2)
  if(population_sl>=(2*10^9)) {
    recharge_roll <- recharge_roll+1
  } else if(population_sl<(1*10^9)) {
    recharge_roll <- recharge_roll-1
  }
  if(max(tech, na.rm=TRUE)<="D") {
    recharge_roll <- recharge_roll-1
  }
  if(max(tech, na.rm=TRUE)>="A") {
    recharge_roll <- recharge_roll+1
  }
  if(max(industry, na.rm=TRUE)<="D") {
    recharge_roll <- recharge_roll-1
  }
  if(max(industry, na.rm=TRUE)>="B") {
    recharge_roll <- recharge_roll+1
  }
  #bonus for star league peak to put us around 50%
  if(faction_type=="IS") {
    recharge_roll <- recharge_roll+3
  } else {
    recharge_roll <- recharge_roll+2
  }

  recharge_roll <- max(min(recharge_roll-1,11),1)
  
  recharge_is <- c(rep(0,8),rep(1,2),2)
  recharge_periphery <- c(rep(0,8),rep(1,3))
  recharge_minor <- c(rep(0,10),1)
  
  nrecharge_sl <- recharge_is[recharge_roll] 
  if(faction_type=="Periphery") {
    nrecharge_sl <- recharge_periphery[recharge_roll] 
  } else if(faction_type=="Minor") {
    nrecharge_sl <- recharge_minor[recharge_roll] 
  } 
  
  if(nrecharge_sl<=nrecharge_current) {
    if(recharge_current$zenith) {
      build_year <- founding_year+round(rexp(1, build_rate))
      recharge_history <- recharge_history %>% 
        bind_rows(tibble(year=build_year,
                         etype="zenithCharge",
                         event="true"))
    }
    if(recharge_current$nadir) {
      build_year <- founding_year+round(rexp(1, build_rate))
      recharge_history <- recharge_history %>% 
        bind_rows(tibble(year=build_year,
                         etype="nadirCharge",
                         event="true"))
    } 
  } else {
    #more stations, so decide on initial creation date(s) and then destruction date(s)
    hadZenith <- FALSE
    hadNadir <- FALSE
    if(nrecharge_sl==2) {
      #build both
      build_year_zenith <- founding_year+round(rexp(1, build_rate))
      recharge_history <- recharge_history %>% 
        bind_rows(tibble(year=build_year_zenith,
                         etype="zenithCharge",
                         event="true"))
      build_year_nadir <- founding_year+round(rexp(1, build_rate))
      recharge_history <- recharge_history %>% 
        bind_rows(tibble(year=build_year_nadir,
                         etype="nadirCharge",
                         event="true"))
      hadZenith = TRUE
      hadNadir = TRUE
    } else if(nrecharge_sl==1) {
      #build one
      if(sample(1:2,1)==1) {
        build_year_zenith <- founding_year+round(rexp(1, build_rate))
        recharge_history <- recharge_history %>% 
          bind_rows(tibble(year=build_year_zenith,
                           etype="zenithCharge",
                           event="true"))
        hadZenith = TRUE
      } else {
        build_year_nadir <- founding_year+round(rexp(1, build_rate))
        recharge_history <- recharge_history %>% 
          bind_rows(tibble(year=build_year_nadir,
                           etype="nadirCharge",
                           event="true"))
        hadNadir = TRUE
      }
    }
    #now check for destruction
    if(hadZenith & !recharge_current$zenith) {
      #destroy station
      recharge_history <- recharge_history %>% 
        bind_rows(tibble(year=max(min(2785+round(rexp(1,1/20)), 2850), 
                                  build_year_zenith+5),
                         etype="zenithCharge",
                         event="false"))
    }
    if(hadNadir & !recharge_current$nadir) {
      #destroy station
      recharge_history <- recharge_history %>% 
        bind_rows(tibble(year=max(min(2785+round(rexp(1,1/20)), 2850), 
                                  build_year_nadir+5),
                         etype="nadirCharge",
                         event="false"))
    }
  }
  
  return(recharge_history)
}

#### Utility Functions ####

#distance to border for a given faction
distance_to_border <- function(x0,y0,faction) {
  
  #return Inf if not one of the five IS factions
  
  #based on faction put in two options for x2 and y2 for a 
  #planet along the border
  if(faction=="LC" | faction=="LA") {
    #Lyran-DC border, use Outpost (15.426, 426.466)
    #Lyran-FWL border, use Green Stone (405.913, -79.963) 
    x2 <- c(15.426,405.913)
    y2 <- c(426.466,-79.963)
  } else if(faction=="DC" | faction=="FRR") {
    #Lyran-DC border, use Outpost (15.426, 426.466)
    #DC-FS border, use Crestobulus (452,426, 426.466)
    x2 <- c(15.426,452.426)
    y2 <- c(426.466,426.466)
  } else if(faction=="FS" | faction=="FC") {
    #DC-FS border, use Crestobulus (452,426, 426.466)
    #FS-CC border, use Desolate Plains (207.01,-361.318)
    x2 <- c(452.426,207.01)
    y2 <- c(426.466,-361.318)
  } else if(faction=="CC" | faction=="SIC") {
    #FS-CC border, use Desolate Plains (207.01,-361.318)
    #CC-FWL border, use Zathras (-84.041, -390.39)
    x2 <- c(-84.041)
    y2 <- c(-390.39)
  } else {
    return(Inf)
  }
  
  #general equation, where x1,y1 will always be terra at 0,0 (so dropped)
  #and x2,y2 is always some other planet along the border line
  return(min(abs(y2*x0-x2*y0)/sqrt(y2^2+x2^2)))

}


roll_d6 <- function(n) {
  return(sum(sample(1:6, n, replace=TRUE)))
}


