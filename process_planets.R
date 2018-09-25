library(xml2)
planets <- read_xml("data/planets.xml")

planet.table <- NULL
target.year <- 3067

for(i in 1:xml_length(planets)) {
  planet <- xml_children(planets)[[i]]
  pname <- xml_text(xml_find_first(planet, "id"))
  xcood <- as.numeric(xml_text(xml_find_first(planet, "xcood")))
  ycood <- as.numeric(xml_text(xml_find_first(planet, "ycood")))
  faction <- xml_text(xml_find_first(planet, "faction"))
  hpg <- xml_text(xml_find_first(planet, "hpg"))
  sic <- xml_text(xml_find_first(planet, "socioIndustrial"))

  events <- xml_find_all(planet, "event")
  for(event in events) {
    year <- as.numeric(substr(xml_text(xml_find_first(event, "date")),1,4))
    if(year>target.year) {
      next
    }
    if(!is.na(xml_find_first(event, "faction"))) {
      faction <- xml_text(xml_find_first(event, "faction"))
      #for now if faction is multiple just take the first
      faction <- strsplit(faction,",")[[1]][1]
    }
    if(!is.na(xml_find_first(event, "hpg"))) {
      hpg <- xml_text(xml_find_first(event, "hpg"))
    }
    #only take changes to default SIC within 30 years of target date
    if(!is.na(xml_find_first(event, "socioIndustrial")) & (target.year-year)<=30) {
      sic <- xml_text(xml_find_first(event, "socioIndustrial"))
    }
  }
  
  #do some re-coding
  sic_codes <- c(NA,NA,NA,NA,NA)
  if(!is.na(sic)) {
    sic_codes <- strsplit(sic, "-")[[1]]
  }
  
  temp <- c(pname, xcood, ycood, sic_codes, faction, hpg)
  print(temp)
  planet.table <- rbind(planet.table, temp)
}


planets_new <- data.frame(name=planet.table[,1],
                          x=as.numeric(planet.table[,2]),
                          y=as.numeric(planet.table[,3]),
                          faction=factor(planet.table[,9]),
                          hpg=factor(planet.table[,10], levels=c("X","A","B","C","D"),ordered=TRUE),
                          tech=    factor(planet.table[,4], levels=c("A","B","C","D","F"), ordered = TRUE),
                          industry=factor(planet.table[,5], levels=c("A","B","C","D","F"), ordered = TRUE),
                          raw     =factor(planet.table[,6], levels=c("A","B","C","D","F"), ordered = TRUE),
                          output  =factor(planet.table[,7], levels=c("A","B","C","D","F"), ordered = TRUE),
                          agricul =factor(planet.table[,8], levels=c("A","B","C","D","F"), ordered = TRUE))

planets_new$distance <- sqrt(planets_new$x^2+planets_new$y^2)
planets_new$distance.sq <- planets_new$distance^2

#TODO: Add in capitals

#crude: for now lets remove planets that are more than 750 light years away from Terra
planets_new <- subset(planets_new, distance<750 & faction!="ABN")

library(mice)

imp <- mice(planets_new[,-c(1:3)], 1)
#that broke quicklly


library(ggplot2)

ggplot(planets_new, aes(x=x, y=y, color=tech))+
  geom_point(alpha=0.5)+
  theme_minimal()+
  scale_colour_brewer(direction=-1)
ggplot(cbind(planets_new[,1:3],complete(imp,1)), aes(x=x, y=y, color=tech))+
  geom_point(alpha=0.5)+
  theme_minimal()+
  scale_colour_brewer(direction=-1)


library(MASS)
library(VGAM)

model.tech <- vglm(cbind(planets_new$tech=="A", 
                         planets_new$tech=="B",
                         planets_new$tech=="C",
                         planets_new$tech=="D",
                         planets_new$tech=="F")~distance+distance.sq+faction+hpg, 
                   data=planets_new, family=cumulative(reverse=TRUE, parallel=TRUE))

x <- 0:750
y <- -0.00291*x+0.000009276*(x)^2
plot(x, y, type="l")


table(planets_new$tech, planets_new$industry)
table(complete(imp,1)$tech, complete(imp,1)$industry)
