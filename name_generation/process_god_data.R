gods <- read.csv("input/gods.csv")

gods$name <- as.character(gods$name)
#remove everything in parenthesis
gods$name <- gsub("\\(.*\\)", "", gods$name)
#remove numbers
gods$name <- gsub("\\d", "", gods$name)
gods$pantheon <- as.character(gods$pantheon)
gods$pantheon <- trimws(gsub("Gods","",gods$pantheon))
gods$pantheon <- gsub("Hindu And Indian","Hindu", gods$pantheon)
gods <- subset(gods, !duplicated(name))
myth_sample <- gods

save(myth_sample, file="output/myth_sample.RData")
