##
## data0: build pop- data sets wcde (all versions)
## data1: build batch data (all versions)
## data2: build dim_limits helper data set for age, sex, education dimensions available
##

library(tidyverse)
library(fs)

d <- dir_ls(recurse = 3) %>%
  as_tibble() %>%
  rename(file = 1) %>%
  filter(str_detect(string = file, pattern = "rds"),
         str_detect(string = file, pattern = "wcde-"),
         str_detect(string = file, pattern = "single"),
         str_detect(string = file, pattern = "pop-", negate = TRUE)) %>%
  separate(col = file, into = c("v", "s", "i", "c"), sep = "/", remove = FALSE) %>%
  mutate(c = str_sub(string = c, end = -5))

d0 <- d %>%
  filter(c %in% c("age", "sex", "edu",
                  "ageno", "sexno", "eduno",
                  "year", "period")) %>%
  mutate(x = map(.x = file, .f = ~read_rds(.x)))

d1 <- d0 %>%
  select(-file, -c) %>%
  group_by(v, s, i) %>%
  reframe(d = list_cbind(x)) %>%
  unnest(cols = d) %>%
  mutate(v = str_sub(string = v, end = -8))

d2 <- d1 %>%
  select(-period) %>%
  mutate(yearno = (year -1950) /5,
         year = as.character(year)) %>%
  relocate(-ends_with("no")) %>%
  rename_with(.fn = ~paste0(.x, "_label"), .cols = 4:7) %>%
  rename_with(.fn = ~paste0(.x, "_code"), .cols = 8:11) %>%
  rename_with(.fn = ~str_remove(.x, "no")) %>%
  pivot_longer(cols = -(1:3),
               names_to = c("dim", ".value"),
               names_sep = "_") %>%
  distinct()

d2 %>%
  filter(is.na(label)) %>%
  tail()

d3 <- d2 %>%
  group_by(v, i, dim) %>%
  arrange(code) %>%
  summarise(first_label = first(label),
            frist_code = first(code),
            last_label = last(label),
            last_code = last(code))

write_csv(d2, "./data/dim.csv")
write_csv(d3, "./data/dim_limits.csv")
# write_csv(d3, "./data/dim_limits.csv")

x=
d2 %>%
  filter(dim == "edu",
         i == "epop") %>%
  distinct()
