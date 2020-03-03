## code to prepare `vietnam_draft` dataset goes here
library(tidyverse)
usethis::use_data("vietnam_draft")
dat_draft <- read_csv("/Users/hathawayj/git/byuistats/data/Draft_vietnam/Draft_vietnam.csv")
use_data(dat_draft, internal = FALSE, overwrite = TRUE,  compress = "bzip2", version = 2)
