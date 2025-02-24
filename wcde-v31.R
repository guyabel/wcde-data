##
## wcde-v1: single .rds files from old repo
## wcde-v2: single .rds files from wcde-shiny
## wcde-v3: single .rds files from dropbox zip
##

library(tidyverse)
library(fs)
library(readxl)

# need to add in age, sex, education and period labels in to single files
ind <- read_excel("../wcde-shiny/meta/indicator.xlsx")
dimen <- read_excel("../wcde-shiny/meta/dimension.xlsx")

# d <- unzip(zipfile = "./data-raw/wcde3_v13_1dec2023.zip", list = TRUE) %>%
# d <- unzip(zipfile = "C:\\Users\\Guy\\Dropbox\\WCDE for Guy\\wcde3.zip", list = TRUE) %>%
# d <- unzip(zipfile = "C://Users//Guy//Downloads//tfr.zip", list = TRUE) %>%
# d <- unzip(zipfile = "C:\\Users\\Guy Abel\\OneDrive - IIASA\\wcde3_18122024_e8.zip", list = TRUE) %>%
d <- unzip(zipfile = "C:\\Users\\Guy Abel\\OneDrive - IIASA\\wcde3_v14_share_toguy.zip", list = TRUE) %>%
  as_tibble() %>%
  rename(file = 1) %>%
  filter(str_detect(string = file, pattern = ".rda"),
         str_detect(string = file, pattern = "df"),
         str_detect(string = file, pattern = "flow", negate = TRUE)) %>%
  mutate(
         # i = str_sub(string = file, start = 10, end = -5),
         i = str_sub(string = file, start = 10, end = -5),
         s = case_when(
           str_detect(string = file, pattern = "df1") ~ 2,
           str_detect(string = file, pattern = "df2") ~ 1,
           str_detect(string = file, pattern = "df3") ~ 3,
           str_detect(string = file, pattern = "df4") ~ 22,
           str_detect(string = file, pattern = "df5") ~ 23,
           str_detect(string = file, pattern = "df7") ~ 4,
           str_detect(string = file, pattern = "df8") ~ 5,
         ),
         dest = paste0("./wcde-v31-single/",s,"/",i))

# create directories
dir_create(path = d$dest)

# dimen names
d1_age <- tibble(
  ageno = 0:21,
  age = c("All", paste0(0:20 * 5, "--", 0:20 * 5 + 4))
) %>%
  mutate(age = ifelse(ageno == 21, "100+", age))

d1_bage <- tibble(
  ageno = 0:10,
  age = c("All", "0--14", "0--19", "15+", "25+", "20--39", "40--64",
          "60+", "65+", "80+", "20--64")
)

d1_sage <- tibble(
  ageno = 0:25,
  age = c("Newborn", paste0(0:24 * 5, "--", 0:24 * 5 + 4))
) %>%
  mutate(age = ifelse(ageno == 25, "120+", age))

d1_sex <- tibble(
  sexno = 0:2,
  sex = c("Both", "Male", "Female")
)

d1_edu <- tibble(
  eduno = 0:10,
  edu = c("Total", names(wcde::wic_col8)[1:6], "Post Secondary", names(wcde::wic_col8)[7:9])
)


loading <- function(rdata_file){
  # rdata_file = d0
  e <- new.env()
  load(rdata_file, envir = e)
  x <- ls(envir = e)
  e[[x]]
}

for(i in 1:nrow(d)){
  i0 <- ind %>%
    filter(name == d$i[i])

  if(i0$df2only == 1 & d$s[i] != 2)
    next()

  d1a <- d1_age
  if(i0$bage == 1)
    d1a <- d1_bage
  if(i0$sage == 1)
    d1a <- d1_sage

  d0 <- unz(description = "C:\\Users\\Guy Abel\\OneDrive - IIASA\\wcde3_v14_share_toguy.zip",
            #description = "C:\\Users\\Guy Abel\\OneDrive - IIASA\\wcde3_18122024_e8.zip",
            #description = "C:\\Users\\Guy\\Dropbox\\WCDE for Guy\\wcde3.zip",
            # description = "C://Users//Guy//Downloads//tfr.zip",
            filename = d$file[i]) %>%
    loading() %>%
    as_tibble()
  closeAllConnections()

  # correct for dilek and samir inconsistencies
  if(i0$name %in% c("netedu")) {
    d0 <- d0 %>%
      select(-bageno) %>%
      mutate(ageno = 0)
  }
  if(i0$name %in% c("ggapedu15", "ggapedu25", "ggapmys15", "ggapmys25")) {
    d0 <- d0 %>%
      rename(ageno = age)
  }
  if(i0$name %in% c("cbr")){
    d0 <- d0 %>%
      mutate(sexno = 0)
  }

  d0 <- d0 %>%
    {if(i0$sage == 1) rename(., ageno = sageno) else .} %>%
    {if(i0$bage == 1) rename(., ageno = bageno) else .} %>%
    {if("ageno" %in% names(.)) . else mutate(., ageno = 0)} %>%
    {if("sexno" %in% names(.)) . else mutate(., sexno = 0)} %>%
    {if("eduno" %in% names(.)) . else mutate(., eduno = 0)}

  d1 <- d0 %>%
    relocate(isono, year, sexno, ageno, eduno) %>%
    arrange(year, isono, sexno, ageno, eduno)

  d2 <- d1 %>%
    distinct() %>%
    drop_na() %>%
    pivot_wider(values_from = ncol(.), names_from = "isono") %>%
    left_join(d1a, by = "ageno") %>%
    left_join(d1_sex, by = "sexno") %>%
    left_join(d1_edu, by = "eduno") %>%
    mutate(period = paste(year, year+5, sep = "-"))

  for(j in 1:ncol(d2))
    saveRDS(object = d2[,j],
            file = paste0(d$dest[i], "/", names(d2)[j], ".rds"))

  if(i %% 10 == 0)
    print(i)
}


# check
x <- dir_info("wcde-v31-single", recurse = TRUE, type = "file") %>%
  mutate(f = path_file(path)) %>%
  relocate(f) %>%
  filter(str_detect(string = f, pattern = "^[0-9]", negate = TRUE),
         str_detect(string = f, pattern = "^age|^edu|^sex|^period|^year", negate = TRUE))
x

# what is in v30, not in v31
v30 <- dir_ls("wcde-v30-single", recurse = TRUE, type = "file")
v31 <- dir_ls("wcde-v31-single", recurse = TRUE, type = "file")

setdiff(str_sub(string = v30, start = 17),
        str_sub(string = v31, start = 17))

setdiff(str_sub(string = v30, start = 17),
        str_sub(string = v31, start = 17)) %>%
  str_subset(pattern = "pop", negate = TRUE)
