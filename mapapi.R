
##Reading in Data and Packages
library(httr)
library(jsonlite)
library(tidyr)
library(dplyr)
library(readr)
library(leaflet)
library(rgdal)
library(anytime)
#Reading in Dataframe of State Capitals
caps = read.table("state_capitals_ll.txt", sep = '\n', header = FALSE)
caps$V1 = as.character(caps$V1)
#Removing Duplicate DC row
caps = as.data.frame(caps[-c(46),])
#Putting data in tidy format
caps = separate(data = caps, col = `caps[-c(46), ]`, into = c("StateLat", 'Long'), sep = "-")
caps = separate(data = caps, col = StateLat, into = c("State", 'Lat'), sep = "  ")
caps$Lat = as.numeric(caps$Lat)
caps$Long = as.numeric(caps$Long)
caps = caps %>% mutate(Long = Long*-1)
#Adding full names of states
capitalnames = read_csv("list-state-capitals-us-764j.csv", skip = 1)
capitalnames = capitalnames[,c('Abr.', 'Capital', 'State')]
caps = merge(caps, capitalnames, by.x = 'State', by.y = 'Abr.')

##Pass Times for US Capitals

for (i in 1:length(caps$State)){
  respdata = GET("http://api.open-notify.org/iss-pass.json", query = list(lat = caps$Lat[i], lon = caps$Long[i]))
  respdata = fromJSON(rawToChar(respdata$content))
  caps$firstpass[i] = respdata$response$risetime[1]
  caps$secondpass[i] = respdata$response$risetime[2]
  caps$thirdpass[i] = respdata$response$risetime[3]
}

##Mapping the Data

statemap = readOGR("us-states.json")
#Combining JSON file with ISS data
statemap = merge(statemap, caps, by.x = 'name', by.y = 'State.y', all.x = TRUE, all.y = TRUE)
#Creating and formating popup and label
state_popup = paste0("<strong>Capital: </strong>", 
                        statemap$Capital,
                        "<br><strong>Next three ISS Pass Times(EST): </strong>", 
                        anytime(statemap$firstpass, tz = 'EST'), ', ', anytime(statemap$secondpass, tz = 'EST'),' ',anytime(statemap$thirdpass, tz = 'EST'))
state_label = paste0("<strong>Capital: </strong>", 
                         statemap$Capital,
                        "<br><strong>Next ISS Pass Time(EST): </strong>", 
                         anytime(statemap$firstpass, tz = 'EST')) %>% 
  lapply(htmltools::HTML)
capimage <- makeIcon('flag.jpg', iconWidth = 29, iconHeight = 15)
capmap = leaflet(data=statemap) %>% addTiles() %>% addPolygons() %>% 
  addMarkers(~Long, ~Lat, popup = ~state_popup, label = ~state_label, icon = capimage)
capmap



##Adding Connecting Lines

#Sorting the Data from East to West
statemapordered = statemap@data[order(statemap$Long, decreasing = TRUE), ]
capmap = leaflet(data=statemap) %>% addTiles() %>% addPolygons() %>% 
  addPolylines(data = statemapordered, ~Long, ~Lat, color = 'red') %>% 
  addMarkers(~Long, ~Lat, popup = ~state_popup, label = ~state_label, icon = capimage)
capmap
