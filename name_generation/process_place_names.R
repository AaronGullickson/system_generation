#readr package to the rescue
library(readr)
place_names <- read_delim(here("name_generation","output","place_names.tsv.gz"), 
                          "\t", escape_double = FALSE, col_names = FALSE, 
                          trim_ws = TRUE,
                          progress = TRUE)

colnames(place_names) <- c("name","feature_class","feature_code","country","population")
place_names$feature_class <- factor(place_names$feature_class)
place_names$feature_code <- factor(place_names$feature_code)
place_names$country <- factor(place_names$country)



#some tests

us_ppl <- subset(place_names, country=="US" & feature_code=="PPL")
egypt_ppl <- subset(place_names, country=="EG" & feature_code=="PPL")


places <-subset(place_names, feature_code=="ADM1" | feature_code=="ADM2" | feature_code=="ADM3" |
                  feature_code=="ADM4" | feature_code=="ADM5" | feature_code=="PPLA" | feature_code=="PPLA2" |
                  feature_code=="PPLA3" | feature_code=="PPLA4" | feature_code=="PPLC")


#set weights equal to the log of population size. If population is missing, then assign the lowest known
#value within the country. If no populations for the whole country, then multiply weights for admin divisions and 
#administative capitals in some fashion. 

library(magrittr)
pop_mins <- subset(places, population>0) %$% tapply(population,country,function(x) {
  y <- min(x)
  if(y<20) {
    y <- 20
  }
  return(y)})


places$population[places$population==0] <- pop_mins[places$country[places$population==0]]
places$weight <- (places$population)^(1/3)

TF <- places$feature_code=="PPLC" | places$feature_code=="PPLA"
places$weight[TF] <- 20*places$weight[TF]
TF <- places$feature_code=="PPLA2"
places$weight[TF] <- 15*places$weight[TF]
TF <- places$feature_code=="PPLA3"
places$weight[TF] <- 5*places$weight[TF]

TF <- places$feature_code=="ADM1"
places$weight[TF] <- 8*places$weight[TF]
TF <- places$feature_code=="ADM2"
places$weight[TF] <- 6*places$weight[TF]
TF <- places$feature_code=="ADM3"
places$weight[TF] <- 4*places$weight[TF]
TF <- places$feature_code=="ADM4"
places$weight[TF] <- 2*places$weight[TF]

places_sample <- as.data.frame(subset(places, select = c("name","country","weight")))
save(places_sample, file=here("name_generation","output","places_sample.RData"))
