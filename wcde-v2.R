##
## wcde-v1: single .rds files from old repo
## wcde-v2: single .rds files from wcde-shiny
##

library(tidyverse)
library(fs)

d <- dir_ls(path = "../wcde-shiny/", recurse = 2) %>%
  as_tibble() %>%
  rename(file = 1) %>%
  filter(str_detect(string = file, pattern = ".RData"),
         str_detect(string = file, pattern = "wcde-shiny/df")) %>%
  mutate(i = str_sub(string = file, start = 19),
         i = str_replace(string = i, pattern = ".RData", replacement = ".rds"),
         # on github ssp2 takes folder df1, ssp1 takes folder df2 because of samir's coding system
         s = case_when(
           str_detect(string = file, pattern = "df1") ~ 2,
           str_detect(string = file, pattern = "df2") ~ 1,
           str_detect(string = file, pattern = "df3") ~ 3,
           str_detect(string = file, pattern = "df4") ~ 22,
           str_detect(string = file, pattern = "df5") ~ 23,
         ),
         dest = paste0("./wcde-v2-single/",s,"/",i),
         dest_path_dir = path_dir(dest))


x <- d %>%
  distinct(dest_path_dir)
for(i in 1:nrow(x))
  dir_create(path = x$dest_path_dir[i])

loading <- function(rdata_file){
  # rdata_file = d
  e <- new.env()
  load(rdata_file, envir = e)
  e
}

for(i in 1:nrow(d)){
  d$file[i] %>%
    loading() %>%
    as.list() %>%
    as_tibble() %>%
    saveRDS(file = d$dest[i])
    # write_csv(file = d$dest_csv[i])
  if(i %% 100 == 0)
    print(i)
}

