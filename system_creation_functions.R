#This script contains functions for creating systems and planets

roll_d6 <- function(n) {
  return(sum(sample(1:6, n, replace=TRUE)))
}

generate_system <- function(star=NULL, habitable=TRUE) {
  
  if(!is.null(star)) {
    #break apart stype to make sure it makes sense
    spectral_class <- substr(star,1,1)
    subtype <- as.numeric(substr(star,2,2))
    star_size <- substring(star, 3)
    
    if(is.na(star_size) | !(spectral_class %in% c("A","B","F","G","K","M"))) {
      warning("Invalid spectral class provided.")
      return(NULL)
    }
    if(is.na(subtype) | subtype <0 | subtype>9) {
      warning("Invalid subtype provided.")
      return(NULL)
    }
    if(is.na(star_size) | nchar(star_size)==0 | 
       !(star_size %in% c("Ia","Ib","II","III","IV","V","VI","VII"))) {
      warning("Invalid star size provided.")
      return(NULL)
    }
    
  } else {
  
    #if no star type provided, then roll one up from life-friendly column
    #in CamOps
    star_roll <- roll_d6(2)
    
    ##### Star Type #####
    star_type <- c("F","M","G","K",rep("M",6),"F")
    if(habitable) {
      star_type <- c(rep("M",3),"K","K","G","G",rep("F",4))
    } else if(star_roll==12) {
      #hot stars!
      star_type <- c(rep("B",2),rep("A",7),"B","F")
      star_roll <- roll_d6(2)
    }
  
    spectral_class <- star_type[star_roll-1]
    subtype <- sample(0:9,1)
    ##A tweak here. Classes M6V and M9V have no planets in habitable zone, so 
    ##leave them out of substype sample if habitable==TRUE
    if(habitable & spectral_class=="M") {
      subtype <- sample(c(0,1,2,3,4,5,7,8),1)
    }
    star_size <- "V"
    
  }
  stype <- paste(spectral_class, subtype, star_size, sep="")
  
  #table only has V stars, so feed that in. We will override below
  stype_data <- read.csv("data/solar_type.csv", 
                         row.names=1)[paste(spectral_class, subtype, "V", sep=""),]
  
  #TODO: what about VI and VII?
  if(star_size != "V") {
    #according to CamOps pg 116, mulitiply luminosity by four and double the life 
    #zone values
    stype_data$luminosity <- stype_data$luminosity*4
    stype_data$distance_inner_au <- stype_data$distance_inner_au*2
    stype_data$distance_outer_au <- stype_data$distance_outer_au*2
  }
  
  ##### Orbital Slots #####
  orbital_slots <- 3+roll_d6(2)

  placement_constants <- c(0.4,0.7,1.0,1.6,2.8,5.2,10,19.6,
                           38.8,77.2,154,307.6,614.8,1229.2,
                           2458)
  orbital_placement <- stype_data$mass*placement_constants

  planets <- NULL
  #if the system needs to be habitable, then randomly pick one of the habitable slots
  #and force it to produce a habitable planet
  life_zone <- orbital_placement>=stype_data$distance_inner_au & 
    orbital_placement<=stype_data$distance_outer_au
  
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
  i <- 1
  for(slot in 1:orbital_slots) {
    planet <- generate_planet(orbital_placement[slot],slot==habitable_slot,stype_data)
    #another tweak - if habitable is true, then we need to ensure
    #that at least on habitable slot in the life zone is occupied by terrestrial 
    #planet 
    if(slot==habitable_slot) {
      while(!planet$inhabitable & i<5000) {
        planet <- generate_planet(orbital_placement[slot],TRUE,stype_data)
        i <- i + 1
      }
    }
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
  
  return(list(star=stype, planets=planets, iterations=i))
}

#TODO: generate moons and rings
#TODO: generate uinhabitable atmospheres, mostly to determine if atmo is caustic
#TODO: asteroid belt characteristics
#TODO: allow for some toxic habitable worlds besides Giant Terrestrials
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
  life <- NA
  water <- NA
  continents <- NA
  temperature <- NA
  inhabitable <- FALSE
  
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
      atmosphere <- "Toxic (Poisonous)"
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
 
  return(list(type=type, orbital_dist=radius, 
              inhabitable=inhabitable, life_zone=life_zone,
              pressure=pressure, atmosphere=atmosphere, 
              gravity=round(gravity,2), temperature=round(temperature-273.15), 
              transit_time=round(transit_time,2),
              water=water, life=life, continents=continents,
              diameter=diameter, density=round(density,4), 
              escape_velocity=round(escape_velocity),
              orbital_velocity=round(orbital_velocity), 
              day_length=day, year_length=round(year_length,1)))
  
}

plot_system <- function(system) {
  
  system_data <- read.csv("data/solar_type.csv", row.names=1)[system$star$type,]
  
  par(mar=c(0,0,0,0), bg="black")
  plot(-10,-10,
       ylim=c(2.4,2.6), 
       xlim=c(0,1.1*(system_data$radius+sqrt(max(system$planets$orbital_dist, na.rm=TRUE)))))
  
  library(plotrix)
  
  #remove empties
  system$planets <- subset(system$planets, type!="Empty")
  
  #draw green zone
  draw.circle(0,2.5,radius=system_data$radius+system_data$distance_outer_au, col="green", border="green")
  draw.circle(0,2.5,radius=system_data$radius+system_data$distance_inner_au, col="white", border="black")
  
  #draw orbit lines for slots
  for(slot in nrow(system$planets):1) {
    planet <- system$planets[slot,]
    draw.circle(0,2.5,radius=system_data$radius+sqrt(planet$orbital_dist),col="black",border="grey80")
  }
 
  #draw sun
  draw.circle(0,2.5,radius=system_data$radius, col="yellow", border="orange")
  
  for(slot in 1:nrow(system$planets)) {
    planet <- system$planets[slot,]
    if(planet$type=="Empty") {
      next
    }
    bg.choice <- "grey"
    col.choice <- "black"
    if(planet$type=="Terrestrial" | planet$type=="Giant Terrestrial") {
      if(is.na(planet$life)) {
        bg.choice <- "brown"
        col.choice <- "red"
      } else {
        bg.choice <- "darkgreen"
        col.choice <- "blue"
      }
    }
    if(planet$type=="Dwarf Terrestrial") {
      bg.choice <- "brown"
      col.choice <- "red"
    }
    if(planet$type=="Gas Giant") {
      bg.choice <- "purple"
      col.choice <- "violet"
    }
    if(planet$type=="Ice Giant") {
      bg.choice <- "skyblue"
      col.choice <- "blue"
    }
    points(sqrt(planet$orbital_dist)+system_data$radius, 2.5, pch=21, bg=bg.choice, col=col.choice,
           cex=log(planet$diameter)/log(12000))
  }
  
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
                               labels=c("None","D","C","B","A"), 
                               ordered=TRUE)
  
  system$recharge <- list(nadir=FALSE, zenith=FALSE)
  
  pop_table <- cbind(c(50000000,
                       10000000,
                        2500000,
                         500000,
                         100000,
                          10000,
                           2500),
                     c(   10000,
                        2000000,
                          50000,
                          20000,
                           5000,
                            500,
                            100))
  
  pop_table_high <- cbind(c(500000000,
                            100000000,
                             25000000,
                              5000000,
                              1000000,
                               200000,
                                50000),
                          c(   100000,
                             20000000,
                              1000000,
                               200000,
                                50000,
                                10000,
                                 2500))
  
  for(slot in which(system$planets$inhabitable)) {
    
    planet <- system$planets[slot,]
    
    ##### Population #####
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
    
    planet$population <- modifier*population
    
    ##### USILR #####
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
    if(faction=="Clan") {
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

    ##### HPG Status #####
    #Tweak: the -1 per 100 LY from Terra was very harsh and 
    #produced distributions that did not match at all what the 
    #text of CamOps on pg. 132-133. We found that reducing
    #this penalty to -1 per 250LY produced much more reasonable
    #results. However, its worth noting that almost any LY penalty
    #will result in minor periphery powers having no HPG exclusively
    
    hpg_roll <- roll_d6(2)-floor(distance_terra/250)
    if(planet$population<(1*10^9)) {
      hpg_roll <- hpg_roll-1
    } else if(hpg_roll>(2*10^9)) {
      hpg_roll <- hpg_roll+1
    }
    if(planet$tech<="D") {
      hpg_roll <- hpg_roll-1
    }
    if(planet$industry<="D") {
      hpg_roll <- hpg_roll-1
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
    
    planet$hpg <- factor(hpg,
                         levels=1:5,
                         labels=c("None","D","C","B","A"), 
                         ordered=TRUE)
    
    #ok, ready to update the planet
    system$planets[slot,] <- planet
    
  }
  
  ##### Recharge Stations #####
  recharge_roll <- roll_d6(2)
  if(max(system$planets$population,na.rm=TRUE)<(1*10^9)) {
    recharge_roll <- recharge_roll-1
  } else if(max(system$planets$population,na.rm=TRUE)<(2*10^9)) {
    recharge_roll <- recharge_roll+1
  }
  if(max(system$planets$tech, na.rm=TRUE)<="D") {
    recharge_roll <- recharge_roll-1
  }
  if(max(system$planets$industry, na.rm=TRUE)<="D") {
    recharge_roll <- recharge_roll-1
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

