## This script shows at what orbital slot the habitable positions are 
## for each star type. 

star_data <- read.csv("data/solar_type.csv", row.names=1)

placement_constants <- c(0.4,0.7,1.0,1.6,2.8,5.2,10,19.6,
                         38.8,77.2,154,307.6,614.8,1229.2,
                         2458)

pos <- NULL
for(i in 1:15) {
  pos <- cbind(pos, star_data$radius*placement_constants[i])
}

colnames(pos) <- paste("pos", 1:15, sep="")
star_data <- cbind(star_data, pos)

habitable_zone <- pos>=star_data$distance_inner_au & pos<=star_data$distance_outer_au

habitable_zone <- data.frame(habitable_zone)
rownames(habitable_zone) <- rownames(star_data)
