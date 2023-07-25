##
## convert data from samir for app
##
library(tidyverse)
library(readxl)
library(fs)

dimen <- read_excel("C:\\Users\\Guy\\Downloads\\wcde-shiny-0833c7fe7de5c0758ce6d5682e91b1537eb76010\\wcde-shiny-0833c7fe7de5c0758ce6d5682e91b1537eb76010/meta/dimension.xlsx")
ind <- read_excel("C:\\Users\\Guy\\Downloads\\wcde-shiny-0833c7fe7de5c0758ce6d5682e91b1537eb76010\\wcde-shiny-0833c7fe7de5c0758ce6d5682e91b1537eb76010/meta/indicator.xlsx")

##
## label names
##
d1_age <- dimen %>%
  filter(dim == "age") %>%
  select(code, name) %>%
  rename(ageno = code, age = name)

d1_bage <- dimen %>%
  filter(dim == "bage") %>%
  select(code, name) %>%
  rename(ageno = code, age = name)

d1_sage <- dimen %>%
  filter(dim == "sage") %>%
  select(code, name) %>%
  rename(ageno = code, age = name)

d1_sex <- dimen %>%
  filter(dim == "sex") %>%
  select(code, name) %>%
  rename(sexno = code, sex = name)

d1_edu <- dimen %>%
  filter(dim == "edu") %>%
  select(code, name) %>%
  rename(eduno = code, edu = name)

d <- dir_ls(path = "C:\\Users\\Guy\\Downloads\\wcde-shiny-0833c7fe7de5c0758ce6d5682e91b1537eb76010\\wcde-shiny-0833c7fe7de5c0758ce6d5682e91b1537eb76010\\df0", recurse = 2) %>%
  as_tibble() %>%
  rename("path" = 1) %>%
  mutate(file = path_file(path),
         scenario = str_sub(string = file, start = 3, end = 3),
         indicator = str_sub(string = file, start = 4),
         indicator = str_sub(string = indicator, end = -5),
         scenario = case_when(
           scenario == 6 ~ 21,
           scenario == 7 ~ 22,
           TRUE ~ as.numeric(scenario)
         )) %>%
  filter(indicator %in% ind$name)

for(i in 1:nrow(d)){
  i0 <- ind %>%
    filter(name == d$indicator[i])

  if(i0$df2only == 1 & d$scenario[i] != 2)
    next()

  d1a <- d1_age
  if(i0$bage == 1)
    d1a <- d1_bage
  if(i0$sage == 1)
    d1a <- d1_sage

 d0 <- read_csv(d$path[i], col_types = cols())

 d1 <- d0 %>%
   relocate(isono, year, sexno, ageno, eduno) %>%
   arrange(year, isono, sexno, ageno, eduno)

 d2 <- d1 %>%
   distinct() %>%
   drop_na() %>%
   spread(key = isono, value = names(d1)[ncol(d1)]) %>%
   left_join(d1a, by = "ageno") %>%
   left_join(d1_sex, by = "sexno") %>%
   left_join(d1_edu, by = "eduno") %>%
   mutate(period = paste(year, year+5, sep = "-"))

 dd <- paste0("wcde-v1-single/", d$scenario[i], "/", d$indicator[i])
 dir_create(dd)
 for(j in 1:ncol(d2)){
   d2 %>%
     select(all_of(j)) %>%
     saveRDS(file = paste0(dd, "/", names(d2)[j], ".rds"))
 }
 print(dd)
 gc()
}

# readRDS("./wcde-v1-single/1/asfr/100.rds")
