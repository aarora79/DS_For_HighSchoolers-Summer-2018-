# crime dataset
library(tidyr)
library(tidyverse)
library(leaflet)
library(viridis)
library(lubridate)
library(zoo)
library(tidyquant)

# read the dataset
crime = read_csv("Crime.csv")
glimpse(crime)
View(crime)

# group by zipcode to color code map
# rename the zipcode field to name because we need it to merge
# with the geojson data
crime_by_zipcode <- crime %>%
  group_by(`Zip Code`) %>%
  summarize(incident_count = n()) %>%
  ungroup() %>%
  rename(name=`Zip Code`)

# read the geojson for Maryland
mc <- geojsonio::geojson_read("maryland-zips.geojson", what = "sp")

# create a new dataframe merged with the crime count
mc2 <- merge(mc, crime_by_zipcode, by="name")
# popup text to display
popup <- paste0("<strong>zipcode: </strong>", mc2$name, "<br><strong>Crime incidents: </strong>", mc2$incident_count)

# color by quantiles, so we make 7 quantiles so 0 to 14%, 14 to 28% and so on..
qpal <- colorQuantile("Blues",mc2$incident_count, n = 7)
fillColor <- qpal(mc2[["incident_count"]])

p <- leaflet(mc) %>%
  addTiles() %>%
  setView(-77.26717411757181, 39.30810137311853, 8) %>%
  addPolygons(smoothFactor = 0.3, fillOpacity = 1, weight =1,
              color = "black", 
              fillColor = fillColor,
              group="zipcode Boundaries",
              popup = popup)
p

#  top 10 cities by number of crime incidents
cities <- crime %>%
  group_by(City) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)

top_n_city_names <- cities$City

cities <- crime %>%
  filter(City %in% top_n_city_names) %>%
  mutate(dispatch_dttime = mdy_hm(`Dispatch Date/Time`)) %>%
  mutate(dispatch_year = year(dispatch_dttime),
         dispatch_month = month(dispatch_dttime)) %>%
  mutate(dispatch_year_mon = (paste0(dispatch_year, "-", dispatch_month, "-1"))) %>%
  group_by(City, dispatch_year_mon, `Crime Name1`) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(dispatch_year_mon = as.yearmon(dispatch_year_mon))
  


p = cities %>%
  ggplot(aes(dispatch_year_mon, count)) + geom_line(aes(col=`Crime Name1`)) +
  ggtitle(paste0("Crimes in cities, a timeseries representation")) +
  xlab("") + ylab("Count") +
  theme_tq() +
  theme(legend.title=element_blank()) +
  scale_y_log10(limits=c(1,max(cities$count))) +
  facet_wrap(~City)
p