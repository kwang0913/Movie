#### Load Library ####
library(tidyverse)
library(lubridate)

#### Download Raw Data ####
# kaggle download link is changing every couple minutes.
# You need to copy paste your realtime link below.
# The data is from "https://www.kaggle.com/rounakbanik/the-movies-dataset"
kaggle <- ""
download.file(kaggle, "kaggle.zip", mode = "wb")
unzip("kaggle.zip")
file.remove("kaggle.zip")

grouplens <- "http://files.grouplens.org/datasets/movielens/ml-latest.zip"
download.file(grouplens, "grouplens.zip", mode = "wb")
unzip("grouplens.zip")
file.remove("grouplens.zip")

#### Import Raw Data ####
temp <- list.files(pattern = "*.csv")
temp %>% setNames(str_remove(temp, ".csv")) %>% map(read.csv) %>% list2env(envir = .GlobalEnv)

temp <- list.files(path = "./ml-latest", pattern = "*.csv")
temp %>% map2("./ml-latest/", ., paste0) %>% setNames(str_remove(temp, ".csv")) %>% 
  map(read.csv) %>% list2env(envir = .GlobalEnv)

#### Cleaning Data ####
movies_metadata$id <- movies_metadata$id %>% as.character() %>% as.integer()
movies_metadata$budget <- movies_metadata$budget %>% as.character() %>% as.numeric()
movies_metadata$original_language <- movies_metadata$original_language %>% as.character()
movies_metadata$popularity <- movies_metadata$popularity %>% as.character() %>% as.numeric()
movies_metadata$release_date <- movies_metadata$release_date %>% ymd()
movies_metadata$title <- movies_metadata$title %>% as.character()

movie <- movies_metadata %>% na.omit() %>% filter(vote_average != 0 & vote_average != 10) %>% 
  select(budget, id, original_language, title, popularity, release_date, revenue, runtime, 
         vote_average, vote_count)
## Kaggle dataset is actually from movielens. But Kaggle's is designed for python use. 
## So I use movielens data to get a better dataset
