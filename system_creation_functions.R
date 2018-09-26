#This script contains functions for creating systems and planets

roll_d6 <- function(n) {
  return(sum(sample(1:6, n, replace=FALSE)))
}

generate_system <- function(habitable=TRUE) {
  
  ##### Star Type #####
  star_type <- c("F","M","G","K",rep("M",6),"F")
  if(habitable) {
    star_type <- c(rep("M",3),"K","K","G","G",rep("F",4))
  }

  spectral_class <- star_type[roll_d6(2)-1]
  subtype <- sample(0:9,1)
  ##A tweak here. Classes M2 or greater have no habitable zones so
  ##only allow 0 ad 1 subtypes if that class and habitable==true
  if(habitable & spectral_class=="M") {
    subtype <- sample(0:1,1)
  }
  
  stype <- paste(spectral_class,subtype,"V",sep="")
  stype_data <- read.csv("data/solar_type.csv", row.names=1)[stype,]
  
  ##### Orbital Slots #####
  
  #another tweak here. B8 through B0 have a habitable zone past the 
  #fifth position so its possible to get something out of the habitable
  #zone on a low roll. So set a minimum value based on star type
  minimum_slots <- 5
  if(habitable & spectral_class == "B") {
    if(subtype<9 & subtype>6) {
      minimum_slots <- 6
    } else if(subtype<=6 & subtype>2) {
      mimimum_slots <- 7
    } else {
      minimum_slots <- 8
    }
  }
  orbital_slots <- max(minimum_slots, roll_d6(2)+3)
  
  placement_constants <- c(0.4,0.7,1.0,1.6,2.8,5.2,10,19.6,
                           38.8,77.2,154,307.6,614.8,1229.2,
                           2458)
  orbital_placement <- stype_data$radius*placement_constants[1:orbital_slots]

  planets <- NULL
  #if the system needs to be habitable, then randomly pick one of the habitable slots
  #and force it to produce a habitable planet
  life_zone <- orbital_placement>=stype_data$distance_inner_au & 
    orbital_placement<=stype_data$distance_outer_au
  habitable_slot <- -1
  if(habitable) {
    habitable_slot <- which(life_zone)
    if(length(habitable_slot)>1) {
      #if more than one slot in habitable zone then choose randomly
      habitable_slot <- sample(habitable_slot,1)
    } 
  }
  i <- 1
  
  for(slot in 1:orbital_slots) {
    planet <- generate_planet(orbital_placement[slot],habitable,stype_data)
    #another tweak - if habitable is true, then we need to ensure
    #that at least on habitable slot in the life zone is occupied by terrestrial 
    #planet FIXME - not working but it was before
    if(slot==habitable_slot) {
      while(!is_habitable(planet) & i<5000) {
        planet <- generate_planet(orbital_placement[slot],habitable,stype_data)
        i <- i + 1
      }
    }
    planets <- rbind(planets, unlist(planet))
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
  planets$life <- factor(planets$life,
                         levels=c("Microbes","Plants","Insects","Fish","Amphibians","Reptiles","Birds","Mammals"))
  
  return(list(star=list(type=stype, charge=stype_data$charge),planets=planets,
              iterations=i))
}

generate_planet <- function(radius, habitable_system, system_data, more_gradation=TRUE) {
 
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
  
  #now fill in stuff
  diameter <- NA
  density <- NA
  day <- NA
  pressure <- NA
  atmosphere <- NA
  life <- "None"
  water <- NA
  continents <- NA
  temperature <- NA
  
  if(type=="Dwarf Terrestrial") {
    diameter <- 400+100*roll_d6(3)
    density <- roll_d6(1)
    day <- roll_d6(3)+12
    pressure <- "Vacuum"
    atmosphere <- "None"
  } else if(type=="Terrestrial") {
    #Another tweak here. The diameter and density numbers for terrestrials
    #are producing average gravities well below 1.0 and too variable. Its 
    #likely inhabited worlds will be closer to earth gravity, so we are going 
    #to use formulas that produce earth means for diameter and density with
    #less variation in diameter.
    if(habitable) {
      diameter <- 9000+500*roll_d6(2)
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
  } else if(type=="Gas Giant") {
    diameter <- 50000+10000*roll_d6(2)
    density <- 0.5+roll_d6(2)/10
    day <- roll_d6(4)
    pressure <- "Very High"
    atmosphere <-  "Toxic (Poisonous)"
  } else if(type=="Ice Giant") {
    diameter <- 25000+5000*roll_d6(1)
    density <- 1+roll_d6(2)/10
    day <- roll_d6(4)
    pressure <- "Very High"
    atmosphere <-  "Toxic (Poisonous)"
  } else if(type=="Asteroid Belt") {
    #TODO
  }
  
  #derived stats
  gravity <- (diameter/12742)*(density/5.5153)
  escape_velocity <- 11186*(diameter/12742)*sqrt(density/5.5153)
  orbital_velocity <- escape_velocity/sqrt(2)
  year_length <- sqrt(radius^3+system_data$mass)
  
  transit_distance <- sqrt((system_data$safe_jump*1000)^2+
                             (radius*149597871000)^2)
  transit_time <- (2 * sqrt(transit_distance/9.8))/(24*60*60)
  
  #determine habitability for terrestrial
  
  #life zone position modifier
  life_mod <- (radius-system_data$distance_inner_au)/(system_data$distance_outer_au-system_data$distance_inner_au)
  if(type=="Terrestrial" | (type=="Giant Terrestrial" & roll_d6(1)==6)) {
    pressure <- c(rep("Vacuum",2),"Trace",rep("Low",2),rep("Normal",2),
                  rep("High",2),rep("Very High",2))[roll_d6(2)-1]
    
    if(life_zone) {
      habitable_roll <- roll_d6(2)+system_data$habitability
      if(pressure=="Low" | pressure=="High") {
        habitable_roll <- habitable_roll-1
      }
      if(type=="Giant Terrestrial") {
        habitable_roll <- habitable_roll-2
      }
      #Tweak to make habitabile planets more likely in systems that must be inhabited
      if(habitable_system) {
        habitable_roll <- habitable_roll+2
      }
      if(pressure=="Vacuum" | pressure=="Trace" | pressure=="Very High" | habitable_roll<9) {
        #TODO: uninhabitable atmospheric composition 
        atmosphere <-  "Toxic (Poisonous)"
      } else {
        ## We have a habitable planet, although it could still be toxic if Giant Terrestrial
        
        ## Atmospheic composition roll
        atmo_roll <- roll_d6(2)
        if(type=="Giant Terrestrial") {
          atmo_roll <- atmo_roll-2
        }
        ## Another tweak here. The proportion of tainted atmospheres is quite high.
        ## presumably settlers would have selected on non-tainted worlds so add a bonus here
        ## if this needs to be a habitable system
        if(habitable_system) {
          atmo_roll <- atmo_roll + 3
        }
        if(atmo_roll<2) {
          atmosphere <-  "Toxic (Poisonous)"
        } else if(atmo_roll<7) {
          atmosphere <- "Tainted (Poisonous)"
        } else {
          atmosphere <- "Breathable"
        }
        
        ##Surface water roll
        escape_velocity_mod <- escape_velocity/11186
        water_roll <- round(roll_d6(2)*life_mod*escape_velocity_mod)
        if(type=="Giant Terrestrial") {
          water_roll <- water_roll+3
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
          continents <- 1
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
    } else {
      #TODO: uninhabitable atmospheric composition 
      atmosphere <- "Toxic (Poisonous)"
    }
  }
  
  #base temperature if not created above
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
  
  return(list(type=type, orbital_dist=radius, life_zone=life_zone,
              pressure=pressure, atmosphere=atmosphere, 
              gravity=round(gravity,2), temperature=round(temperature-273.15), 
              transit_time=round(transit_time,2),
              water=water, life=life, continents=continents,
              diameter=diameter, density=round(density,4), 
              escape_velocity=round(escape_velocity),
              orbital_velocity=round(orbital_velocity), 
              day_length=day, year_length=round(year_length,1)))
  
}

is_habitable <- function(planet) {
  return(!is.na(planet$atmosphere) & !is.na(planet$pressure) &
           (planet$atmosphere=="Breathable" | planet$atmosphere=="Tainted (Poisonous)") & 
           (planet$pressure=="High" | planet$pressure=="Normal" | planet$pressure=="Low"))
}
