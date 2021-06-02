#### 01 STARTUP ################################################################

# Load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)
#library(upgo)
#library(strr)
library(sf)
library(stringr)
library(tidyr)
library(qs)


# Set global variables ----------------------------------------------------

#if (Sys.info()["sysname"] != "Windows") {plan(multiprocess)}
col_palette <- 
  c("#FF6600", "#CC6699", "#3399CC", "#FFCC66", "#074387", "#6EEB83", "#008A43", "#FFD500", "#A80858")

scales::show_col(col_palette)
