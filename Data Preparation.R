#### Load Library ####
library(tidyverse)
library(lubridate)

#### Download Raw Data ####
kaggle <- "https://storage.googleapis.com/kaggle-datasets/3405/6663/the-movies-dataset.zip?GoogleAccessId=web-data@kaggle-161607.iam.gserviceaccount.com&Expires=1526953186&Signature=hSE4whQ0Cshe4zRq%2FHVgkx1hY2KhBqViVFjaxFosgp%2FRcvqXfK5PZZwHjBJnYF%2Fkh1dnlNq8rDHE5Ko91o2dzIxmI6OUYhQ%2FbFcBSmyiTPcZ7ew1WNkD4AZnNfT9QEZe%2FB%2BPM6jQ%2FZvtUbKhHnPz8IOk3GtPD%2BhoAD%2BrrmnW1Ojn1aBRss6pF%2BTjuYt%2F0l25wf1s%2F%2BONmzk9%2B1mcg7nEW8fwlkSaodkM0TOVqub6v9zKdTKSnhvTWZm%2BUQBhKqDzCry73niUvbWx83jixPiwSxkP8L88u%2BKxQPVbGlhXne44LxBrt%2Fgbsp3kkp7X5jfvhf3ur2GJH5TBeJ%2FQv3%2Bw%2BQ%3D%3D"
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
## Merge Movielens Data
movie_base <- ratings %>% select(-timestamp) %>% group_by(movieId) %>% nest() %>% 
  right_join(links) %>% right_join(movies)

## Merge Movielens Data and Kaggle Data  
movies_metadata$id <- movies_metadata$id %>% as.character() %>% as.integer()

movies_metadata %>% select(budget, id, original_language, popularity, release_date, revenue, 
                           runtime, status, video, vote_average, vote_count) %>% 
  full_join(movie_base, by = c("id" = "tmdbId")) %>% na.omit() %>% 
  select(-c(id, movieId, imdbId)) -> movie

## Clean
movie$budget <- movie$budget %>% as.character() %>% as.numeric()
movie$original_language <- movie$original_language %>% as.character()
movie$popularity <- movie$popularity %>% as.character() %>% as.numeric()
movie$release_date <- movie$release_date %>% ymd()
movie$title <- movie$title %>% as.character()
movie$genres <- movie$genres %>% as.character()

movie <- movie %>% na.omit() %>% filter(vote_average != 0 & vote_average != 10)
## Kaggle dataset is actually from movielens. But Kaggle's is designed for python use. 
## So I use movielens data to get a better dataset
