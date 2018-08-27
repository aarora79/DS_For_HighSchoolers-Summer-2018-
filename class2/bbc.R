library(tidyr)
library(tidyverse)
library(httr)
library(dplyr)
library(futile.logger)
library(jsonlite)

# api end point
API <- "https://newsapi.org/v2/top-headlines?country=us&apiKey=__API_KEY__"


# read the api key from the environment variable 
# API key is storedin the environment variable by doing 
# sometihng like
# Sys.setenv(BBC_NEWS_API_KEY="eed8134a37b44bac8a4417f94a1e64f8") 
api_key <- Sys.getenv("BBC_NEWS_API_KEY")

# replace the place holder in the api key with the actual value
api_end_point <- str_replace(API, "__API_KEY__", api_key)
flog.info("the api end point is %s", api_end_point)

# all set invoke the api now
flog.info("going to invoke the api now")
resp <- GET(api_end_point)

# we have the response
resp_stats_code <- status_code(resp)
if (resp_stats_code == 200) {
  flog.info("the API call successed, we have data from the bbc news api end point")
  news   <- fromJSON(content(resp, "text"))
  
  flog.info("here is the structure of the response")
  flog.info(str(news))
  
  # store the articles in a new dataframe
  articles = news$articles
  
  # for this problem we are only interested in the sources
  View(articles$source)
  
  # lets draw a bar plot for the sources
  articles$source %>%
    ggplot(aes(x=name)) +
      geom_bar(stat="count", fill="steelblue")+
      #geom_text(aes(label=len), vjust=-0.3, size=3.5)+
      theme_minimal()
  
  # reorder the bars from high to low
  articles$source %>%
    ggplot(aes(x=reorder(name, name, function(x)-length(x)))) +
    geom_bar(stat="count", fill="steelblue") +
    xlab("Source") + ylab("Article Count") + 
    theme_minimal()
  
} else {
  flog.error("the api call failed, status code is %d", resp_stats_code)
}

