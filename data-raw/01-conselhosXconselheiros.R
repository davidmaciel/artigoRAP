## code to prepare `conselhosXconselheiros` dataset goes here
if(!"tidyverse" %in% rownames(installed.packages())){
  install.packages("tidyverse")
}
library(tidyverse)

cons <- read.csv2("data-raw/sources/conselheiros.CSV")

usethis::use_data(conselhosXconselheiros, overwrite = TRUE)
