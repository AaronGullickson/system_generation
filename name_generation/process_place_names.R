#readr package to the rescue
library(readr)
place_names <- read_delim("name_generation/place_names.tsv.gz", 
                          "\t", escape_double = FALSE, col_names = FALSE, 
                          trim_ws = TRUE,
                          progress = TRUE)

colnames(place_names) <- c("name","feature_class","feature_code","country")
place_names$feature_class <- factor(place_names$feature_class)
place_names$feature_code <- factor(place_names$feature_code)
place_names$country <- factor(place_names$country)

#some tests

us_ppl <- subset(place_names, country=="US" & feature_code=="PPL")
egypt_ppl <- subset(place_names, country=="EG" & feature_code=="PPL")
