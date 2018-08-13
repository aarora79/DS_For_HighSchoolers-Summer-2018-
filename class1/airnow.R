library(httr)
library(jsonlite)
library(ggplot2)
library(dplyr)
# get the API key from here -> https://docs.airnowapi.org/account/request/
# set the API_KEY env variable from the command line so that we dont have
# store it in the code

API_KEY <- Sys.getenv("API_KEY")




# http://www.airnowapi.org/aq/forecast/zipCode/?format=application/json&zipCode=20002&date=2018-07-11&distance=25&API_KEY=4D6DF4FB-45B0-480C-BB2E-FEB0D34E4616
airnow_api_host <- "http://www.airnowapi.org"
url <- paste0(airnow_api_host, "/aq/forecast/zipCode/?format=application/json&zipCode=20871&date=2018-08-12&distance=25&API_KEY=", API_KEY)
print(url)
resp   <- GET(url)
print(status_code(resp))

if(status_code(resp) == 200) {

  resp_df   <- fromJSON(
    content(resp, "text")
  )
  View(resp_df)

} else {
  cat(sprintf("there was an error getting the response, status code is %d", status_code(resp)))
}

resp_df %>%
ggplot(aes(x = as.Date(DateForecast), y = AQI)) +
  geom_line(aes(col=ParameterName), size = 2) +
  geom_point() +
  ggtitle("AQI values in Clarksburg", subtitle = "Measuring PM2.5 and O3") +
  theme_bw() + labs(caption="Data collected from the AirNow API") +
  xlab("Forecast Date") + ylab("Air Quality Index")



API_KEY <- Sys.getenv("API_KEY")

API_KEY <- Sys.getenv("API_KEY")

dates = c("2018-07-09","2018-07-10","2018-07-11","2018-07-12","2018-07-13","2018-07-14","2018-07-15")
get_data <- function(date){
  airnow_api_host <- "http://www.airnowapi.org"
  url <- paste0(airnow_api_host, "/aq/forecast/zipCode/?format=application/json&zipCode=20871&date=",date,"&distance=25&API_KEY=", API_KEY)
  print(url)
  resp   <- GET(url)
  #print(status_code(resp))
  print(resp)
  resp_df   <- fromJSON(content(resp, "text"))
  #View(head(resp_df, n = 2))
  return (head(resp_df, n = 2))
}

purrr::map(dates,get_data)


mean(2,3,4)
