##
## data0: build pop- data sets wcde (all versions)
## data1: build batch data (all versions)
##

library(tidyverse)
library(fs)

##
## read in data
##
# education age sex data
e <- dir_ls(recurse = 3) %>%
  as_tibble() %>%
  rename(file = 1) %>%
  filter(str_detect(string = file, pattern = "epop"),
         str_detect(string = file, pattern = "rds")) %>%
  filter(str_detect(string = file, pattern = "wcde-")) %>%
  filter(str_detect(string = file, pattern = "wcde-v3")) %>%
  mutate(file = as.character(file),
         path = path_dir(file),
         path = path_dir(path),
         s = str_sub(string = path, start = 15, end = 17),
         s = str_remove_all(string = s, pattern = "\\/"),
         d0 = map(.x = file, .f = ~readRDS(.x)),
         version = str_sub(string = file, end = 7)) %>%
  group_by(version, s, path) %>%
  summarize(d1 = list(reduce(d0, bind_cols)))

# age sex data (maybe it covers more countries?)
p <- dir_ls(recurse = 3) %>%
  as_tibble() %>%
  rename(file = 1) %>%
  filter(str_detect(string = file, pattern = "pop"),
         str_detect(string = file, pattern = "rds"),
         str_detect(string = file, pattern = "prop", negate = TRUE),
         str_detect(string = file, pattern = "bpop", negate = TRUE),
         str_detect(string = file, pattern = "epop", negate = TRUE),
         str_detect(string = file, pattern = "pop-", negate = TRUE),
         str_detect(string = file, pattern = "batch", negate = TRUE)) %>%
  filter(str_detect(string = file, pattern = "v3")) %>%
  mutate(file = as.character(file),
         path = path_dir(file),
         path = path_dir(path),
         s = str_sub(string = path, start = 15, end = 17),
         s = str_remove(string = s, pattern = "\\/"),
         s = str_remove_all(string = s, pattern = "\\/"),
         d0 = map(.x = file, .f = ~readRDS(.x)),
         version = str_sub(string = file, end = 7)) %>%
  group_by(version, s, path) %>%
  summarize(d1 = list(reduce(d0, bind_cols)))

# p <- p %>%
#   mutate(n = map_dbl(.x = d0, .f = ~nrow(.x)))

##
## create pop-directories
##
for(i in 1:nrow(p)){
  for(j in c("/pop-total",
             "/pop-age", "/pop-sex", "/pop-edattain",
             "/pop-age-sex", "/pop-age-edattain", "/pop-sex-edattain",
             "/pop-age-sex-edattain")){
    dir_create(path = paste0(p$path[i],"/",j))
  }
}
# pop-age-sex does not have the all (age, sex) categories that pop has
# pop-age-sex-edattian does not have the all (age, sex and edattian) categories that epop has

##
## pop (no edu)
##

p <- p %>%
  # either grouping or fs path is killing speed
  mutate(path = as.character(path)) %>%
  group_by(version, s, path) %>%
  mutate(total = map(.x = d1, .f = function(d = .x){
    d %>%
      relocate(ncol(.) - 0:7) %>%
      filter(age == "All", sex == "Both", edu == "Total")})) %>%
  mutate(sex = map(.x = d1, .f = function(d = .x){
    d %>%
      relocate(ncol(.) - 0:7) %>%
      filter(age == "All", sex != "Both", edu == "Total")})) %>%
  mutate(age = map(.x = d1, .f = function(d = .x){
    d %>%
      relocate(ncol(.) - 0:7) %>%
      filter(age != "All", sex == "Both", edu == "Total")})) %>%
  mutate(`age-sex` = map(.x = d1, .f = function(d = .x){
    d %>%
      relocate(ncol(.) - 0:7) %>%
      filter(age != "All", sex != "Both", edu == "Total")})) %>%
  select(-d1) %>%
  pivot_longer(cols = -(1:3), names_to = "type", values_to = "d") %>%
  ungroup()


# # divide p0. very slow looping through nested tibbles
# p1 <- p0 %>%
#   select(-d) %>%
#   ungroup() %>%
#
# p2 <- p0$d

for(i in 1:nrow(p)){
  gc()
  d <- p$d[i][[1]]
  pp <- paste0(p$path[i], "/pop-", p$type[i])
  message(pp)
  for(j in 1:ncol(d)){
    f <- paste0(pp, "/", names(d)[j], ".rds")
    d %>%
      select(j) %>%
      saveRDS(file = f)
    if(j %% 50 == 0 | j == ncol(d))
      print(j)
  }
}


##
## pop-edattain
##

e <- e %>%
  mutate(path = as.character(path)) %>%
  group_by(version, s, path) %>%
  mutate(edattain = map(.x = d1, .f = function(d = .x){
    d %>%
      relocate(ncol(.) - 0:7) %>%
      filter(age == "All", sex == "Both", edu != "Total")})) %>%
  mutate(`age-edattain` = map(.x = d1, .f = function(d = .x){
    d %>%
      relocate(ncol(.) - 0:7) %>%
      filter(age != "All", sex == "Both", edu != "Total")})) %>%
  mutate(`sex-edattain` = map(.x = d1, .f = function(d = .x){
    d %>%
      relocate(ncol(.) - 0:7) %>%
      filter(age == "All", sex != "Both", edu != "Total")})) %>%
  mutate(`age-sex-edattain` = map(.x = d1, .f = function(d = .x){
    d %>%
      relocate(ncol(.) - 0:7) %>%
      filter(age != "All", sex != "Both", edu != "Total")})) %>%
  select(-d1) %>%
  pivot_longer(cols = -(1:3), names_to = "type", values_to = "d") %>%
  ungroup()


for(i in 1:nrow(p)){
  gc()
  d <- e$d[i][[1]]
  pp <- paste0(e$path[i], "/pop-", e$type[i])
  message(pp)
  for(j in 1:ncol(d)){
    f <- paste0(pp, "/", names(d)[j], ".rds")
    d %>%
      select(j) %>%
      saveRDS(file = f)
    if(j %% 50 == 0 | j == ncol(d))
      print(j)
  }
}


##
## check no 0MB file sizes
##
library(fs)
# dir_info(path = "./wcde-v1-single/", recurse = TRUE, type = "file") %>%
# dir_info(path = "./wcde-v2-single/", recurse = TRUE, type = "file") %>%
dir_info(path = "./wcde-v3-single/", recurse = TRUE, type = "file") %>%
  arrange(size)
