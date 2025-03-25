##
## data0: build pop- data sets wcde (all versions)
## data1: build batch data (all versions)
## data2: build dim_limits helper data set for age, sex, education dimensions available
## data3: set v3 to latest v3. version - this will save you having to update the R package
##

library(tidyverse)
library(fs)

dir_copy("wcde-v31-batch", "wcde-v3-batch", overwrite = TRUE)
dir_copy("wcde-v31-single", "wcde-v3-single", overwrite = TRUE)
