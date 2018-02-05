# combine point locations with names of places that need to be batch processed in PCensus.
# outputs matching place names and locations
setwd("~/Projects/RStuff/Misc/PCensus")

library(readr)
library(dplyr)
library(stringr)

plc_names <- read_csv('Population Spreadsheet.csv') %>% 
  rename(Geographical.Name = LOCname)

bc_plc <- read_csv('cgn_bc_csv_eng.csv')
names(bc_plc)
# find matching places, get lat long and output

names(bc_plc) <- str_replace_all(string=names(bc_plc), pattern=" ", repl=".") 

# plc <-filter(bc_plc,Generic.Category == "Populated Place")# %>% 
  
plc <- right_join(bc_plc,plc_names, by="Geographical.Name") %>% 
  select(Geographical.Name,Generic.Category,Latitude,Longitude,Location,Generic.Term) %>% 
  group_by(Geographical.Name) %>% 
  distinct() %>% 
  filter(Generic.Category == "Populated Place")

write_csv(plc,'bc_liquor.csv')

write_csv(diff,'missing_location.csv')


# get multiple names
t<-summarise(plc,n=n(),
             n_cat = n_distinct(Generic.Category)) %>% 
  filter(n>2)

# find names in plc_names missing in plc
diff<-full_join(plc,plc_names,by="Geographical.Name") %>% 
  group_by(Geographical.Name) %>% 
  filter(is.na(Generic.Category))
