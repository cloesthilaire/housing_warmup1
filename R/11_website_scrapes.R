install.packages("rvest")
library(rvest)

websites_dataframe <- readxl::read_xlsx("data/websites.xlsx") 

website_urls <- 
  websites_dataframe %>% 
  filter(!is.na(Website)) %>% 
  select(Website) %>% 
  na.omit() %>% 
  pull(Website)

result <- vector("list", length(website_urls))


for (i in seq_along(website_urls)) {
  
  result[[i]] <- 
    
    read_html(website_urls[[i]]) %>% 
    html_element("body") %>% 
    html_text2() 
  
}

text_list <- unlist(result)

website_text <- data.frame(Website = website_urls, text = text_list)

websites_dataframe_with_text <- 
  websites_dataframe %>% 
  left_join(., website_text, by = "Website")

write_csv(websites_dataframe_with_text, file = "output/websites_dataframe_with_text")



website_test <- read_html("https://onessy.ca/fr/?utm_source=gmb&utm_medium=organic&utm_campaign=O%27Nessy") 

website_test %>% 
  html_element("body") %>% 
  html_text2()
  

read_html(website_urls[[5]]) %>% 
  html_element("p") %>% 
  html_text2() 

