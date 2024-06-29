##
## data0: build pop- data sets wcde (all versions)
## data1: build batch data (all versions)
##

library(tidyverse)
library(fs)
library(readxl)

ind <- read_excel("../wcde-shiny/meta/indicator.xlsx")

d <- dir_ls(recurse = 3) %>%
  as_tibble() %>%
  rename(file = 1) %>%
  mutate(dir = str_remove(string = file, pattern = "/[^/]*$")) %>%
  filter(str_detect(string = file, pattern = "rds")) %>%
  filter(str_detect(string = file, pattern = "wcde-")) %>%
  filter(str_detect(string = file, pattern = "single")) %>%
  separate(col = file, into = c("v", "s", "i", "c"), sep = "/", remove = FALSE)
#%>%
# github wont allow over 100mb files .. added them to .gitignore
# filter(!i %in% c("epop", "prop"))

d0 <- d %>%
  select(-c, -file) %>%
  distinct()

d0 <- d0 %>%
  filter(v == "wcde-v3-single")

# d0 <- d0 %>%
#   filter(i %in% c("asfr", "tfr", "easfr", "etfr"))

##
## create batch directories
##
d0 %>%
  mutate(dir0 = str_remove(string = dir, pattern = "/[^/]*$")) %>%
  distinct(dir0) %>%
  mutate(dir0 = str_replace(string = dir0, pattern = "single", replacement = "batch")) %>%
  pull(dir0) %>%
  dir_create()

##
## create batch files
##
for(i in 1:nrow(d0)){
  message(paste(i, d0$dir[i]))
  x0 <- dir_ls(path = d0$dir[i]) %>%
    as_tibble() %>%
    mutate(
      value = as.character(value),
      n = str_remove(string = value, pattern = d$dir[i]),
      n = str_remove(string = n, pattern = ".rds"),
      n = str_sub(string = n, start = 2),
      v = map(
        .x = value,
        .f = ~readRDS(file = .x)
      ),
    ) %>%
    reframe(bind_cols(v))

  x1 <- wcde::wic_locations %>%
    select(isono, name)

  cc <- str_which(string = names(x0), pattern = "\\d{1,}")

  # ii <- wcde::wic_indicators %>%
  #   filter(indicator == d0$i[i])
  ii <- ind %>%
    filter(name == d0$i[i])
  col_lab <- d0$i[i]

  if(nrow(ii) == 0){
    ii <- tibble(
      age = str_detect(string = d0$i[i], pattern = "age"),
      sex = str_detect(string = d0$i[i], pattern = "sex"),
      edu = str_detect(string = d0$i[i], pattern = "edattain"),
      bage = FALSE,
      sage = FALSE,
      period = FALSE
    )
    col_lab <- "pop"
  }

  # cc <- which(names(x0) %in% country_code)
  x2 <- x0 %>%
    pivot_longer(cols = all_of(cc), names_to = "isono", values_to = col_lab) %>%
    select(-contains("no"), isono) %>%
    mutate(isono = as.numeric(isono)) %>%
    left_join(x1, by = "isono") %>%
    relocate(name, isono, year, period) %>%
    rename(country_code = isono) %>%
    {if(ii$edu) rename(., education=edu) else .} %>%
    {if(ii$edu) . else select(., -edu)} %>%
    {if(sum(ii$age, ii$bage, ii$sage) > 0) . else select(., -age)} %>%
    {if(ii$sex) . else select(., -sex)} %>%
    {if(ii$period) . else select(., -period)} %>%
    {if(!ii$period) . else select(., -year)} %>%
    drop_na(.)

  f <- d0$dir[i] %>%
    str_replace(pattern = "single", replacement = "batch") %>%
    paste0(".rds")

  x2 %>%
    saveRDS(file = f)
}

