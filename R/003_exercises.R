### R EXERCISES FOR RETRIEVING, JOINING AND MAPPING DATA #################

# Step 1: load libraries ------------------------------------------------
# You might have to install libraries if you do not have them using
# install.packages("NAME OF LIBRARY")

# The ones I usually use
library(sf)
library(stringr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(cancensus)
library(osmdata)
library(biscale)
library(scales)
library(patchwork)


# Exercise 1: Retrieve cancensus data ------------------------------------

# Retrieving cancensus data can be a pain depending on which variables you
# are interested in. The process is however always the same. Here is my skeleton 
# code for retrieving CT- or DA-level census data

# Step 1: see the regions and variables you would like to import

View(cancensus::list_census_regions("CA16")) # use CA11, CA06 if you want older censuses. The value in the region 
#column is the one you will want,
View(cancensus::list_census_vectors("CA16")) # the pop-up page will include all census variables. You might have to
#use your filter button to figure out which variables you will want! Once you know, you will want to past the vector
# value in the code below.

# Step 2: import
DA <-
  get_census(
    dataset = "CA16", regions = list(CSD = c("2466023")), level = "DA", #dataset in the census year you want. regions is the region you want (note the level, here it is CSD), level = is for how fined grained you want the data to be displayed (CT? DA?)
    vectors = c("v_CA16_4840", "v_CA16_4841"), #the vectors you want to get. it you want to do percentages, you will have to find both the value and the parent vectors. This can get tricky because there are "subtotals" at times. Lmk if you want and we can check together!
    geo_format = "sf") %>% #to let R know you want it to be spatialized
  st_transform(32618) %>%  #Mtl projection
  select(-c(`Shape Area`:Households, CSD_UID, CT_UID:`Area (sq km)`)) %>% #I take out the random variables I don't want in the DA dataset. The "general" columns of DA and CT are not the same, so this select is different for CT (ask me if you want it)
  set_names(c("dwellings", "GeoUID", "population", #By removing all the random variables in the manipulation before, I make it easier for myself to rename the columns so they are easy to work with (no quotes, :, majuscules, etc)
              "parent_condo", "condo", "geometry")) %>% #set_names work if you provide the same number of names than there are columns. It will give you an error if not. You can run (without assigning) the lines above, see the output in your console and set the names with that!
  mutate(p_condo = condo / parent_condo) %>% #to create a proportion column, I used the condo variable and the parent_condo variable from above. This will give me a percentage to map is easily (and meaningfully!)
  select(GeoUID, dwellings, population, p_condo) %>% # I do not neeed condo and parent_condo no more.
  as_tibble() %>% #prettier/easier to work with than the original format!
  st_as_sf(agr = "constant")

# You can use plot() to visualize this easily. The output is ugly but at least you'll know you are in Mtl!

plot(DA)

# Insteas of re-doing this every time you open your R (or save everything from the previous session in your workspace)
# you can save this output (or all your script's outputs) in a .Rdata file. Here is the code:

save(DA, # you can put more than one: so like CT, DA, streets, bike_lanes, etc!   
     file = "output/geometry.Rdata")

# My repo always has R, output and data folders (and a figures folder in the output folder). Makes it easy to 
# reuse and trace your work! All my .Rscript live in the R folder. All the .Rdata files I output live in the 
# output folder, and all the data from donnees montreal I download are in my data folder. The figures I produce
# live in my output/figures folder. You can load a .Rdata file doing this:

load("output/geometry.Rdata")


# Exercise 1: Spatial join of cancensus and open data  -------------------------

# We will use the tree felling dataset from Open Data Montreal
# https://donnees.montreal.ca/ville-de-montreal/abattage-arbres-publics

felling_raw <- read_csv("data/arbres-abattages.csv")

# The dataset is ugly, so we will clean it and make sure it is an sf dataset

felling_raw # to view the output

# We will start by changing the column names, giving it a geometry and maybe
# change the date column to take out the time! We need to read about each column
# on the website to know

felling <- 
  felling_raw %>% 
  as_tibble() %>% 
  select(-Coord_X, -Coord_Y) %>% # we have two sets of coordinates and only need one
  set_names(c("on_street", "id", "nb_felling", "type", "date", "longitude", "latitude")) %>% #once that is done we will add a geometry column
  filter(!is.na(longitude)) %>% # setting a geometry is only possible if there are no NA values for the points. Filtering with a !is.na is telling R to filter for only value that are NOT (!) NAs
  st_as_sf(coords = c("longitude", "latitude"), crs=4326) %>%  #you use the two columns names for lon-lat in coords, and put a crs. It is usually either 4326 or 32618 for Mtl
  st_transform(32618) %>%  #mtl projection
  mutate(date = as.Date(date, format = "%Y/%m/%d")) # since the time was always 00:00:00, it is more prudent to only have your date be Ymd

felling

# much cleaner!
# also, the on_street column tells us if the tree is on the street or no. Having it be a TRUE/FALSE column would make more sense.
# here is how we can do that!

felling <- 
  felling %>% 
  mutate(on_street = ifelse(on_street == "H", FALSE, TRUE)) # My statement is saying "if my value is H (for hors rue or not on street), make on_street be FALSE, if not, make it TRUE

# Join the felling and DA dataset to have the number of felling by DA!

felling_DA <- st_join(DA, felling) # a join will join the two datasets by common location and give the second dataset in the function the geometry of the first dataset. In that case, each DA will be given the geometry of the DA so we can do group_by with it!

# We lost half (!!!) of our tree fellings with the join. I honestly have no idea why, but in the normal world I would check it out!

felling_DA

# We can now see that each tree has the variables from the DA dataset (the p_condo one is random but whatever).
# we can now map the number of tree fellings by DA in the city of montreal

felling_DA %>% 
  group_by(GeoUID) %>% 
  summarize(number_fellings = n()) %>% 
  ggplot()+
  geom_sf(aes(fill=number_fellings), color=NA)+ # color=NA takes out the thick dark grey borders that we do not need
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 500) + # the scale_fill_gradient lets you choose your colors. you can go online and google "colors in R" to see the codes for all the colors you can use! 
  theme_void()

# Our plot is kinda ugly because we don't have a legend name but also because we have outliers that makes the map all yellow.
# You can inspect those and decide if it is legit to keep it in your map. Let's say it is. You can then adjust the limits of
# your scale using limits = c(X, Y) and oob = scales::squish to make the map legible, without filtering out the outliers (squishing the values)

felling_DA %>% 
  group_by(GeoUID) %>% 
  summarize(number_fellings = n()) %>% 
  ggplot()+
  geom_sf(aes(fill=number_fellings), color=NA)+ # color=NA takes out the thick dark grey borders that we do not need
  scale_fill_gradient2(low = "lightblue", mid = "pink", high = "red", # the scale_fill_gradient lets you choose your colors
                       midpoint = 125,
                       limits = c(0, 250),
                       oob = scales::squish) + 
  theme_void()

# better! colors are still ugly but you can play around

# Tips
# 1- When you do not know what to put as arguments in functions, use ?thefunction to see the help documentation
# 2- You can google your error message and see what other solutions people found (usually on Stack Overflow)
# 3- After spending 10-15 minutes on an issue, shoot me a message so we can figure out what is going on!
