years <- 
  food_businesses %>% 
  filter(statut == "Ouvert") %>% 
  distinct(name, address, .keep_all = TRUE) %>% 
  mutate(year = year(date)) %>% 
  filter(year != 2021) %>% 
  mutate(duplicates = 2020 - year + 1) %>%
  mutate(name_address = paste(name, address)) %>% 
  group_by(name_address) %>% 
  expand(duplicates = seq(1:duplicates))

food_businesses_open <- 
  food_businesses %>% 
  filter(statut == "Ouvert") %>% 
  distinct(name, address, .keep_all = TRUE) %>% 
  mutate(year = year(date)) %>% 
  filter(year != 2021) %>% 
  mutate(name_address = paste(name, address))

food_businesses %>% 
  filter(statut != "Ouvert") %>% 
  count(name, address) %>% View()
  distinct(name, address, .keep_all = TRUE)


food_businesses_201920 <- 
  full_join(food_businesses_open, years, by="name_address") %>% 
  mutate(year = year + (duplicates - 1)) %>%
  filter(year == 2020 | year == 2019) 

save(food_businesses_201920, file = "output/food_businesses_201920.Rdata")
