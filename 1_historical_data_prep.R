### Historical data prep file

## This workflow will pull the historical dataset from the Open data portal to create the imbedded file.
## This is to reduce the burden on the data pull when the app is launched and speed up load performance

## Libraries

library(dplyr)
library(lubridate)
library(tidyr)
library(RSocrata)
library(soql)


#1 Extract and process data from data.act.gov.au
# This code uses the Socrata API. I have pulled the data to 1 March 2020. From these data
# I will compute the long-term statistics


#Pull historic data

Query <- soql() %>%
  soql_add_endpoint("https://www.data.act.gov.au/resource/yuhh-28ai.json") %>%
  soql_simple_filter("VariableName", "Stream Discharge Ml/Day") %>%
  soql_where("DatetimeAEST > '2020-03-01T09:00.000'") %>% #dynamic date using sys.Date()
  soql_select("DatetimeAEST, Value, SiteID, QualityCode") %>%
  as.character()

streamflow_data <- read.socrata(Query)

#Prep data
streamflow_data$Value <- as.numeric(streamflow_data$Value)
streamflow_data$DatetimeAEST <- as.POSIXct(streamflow_data$DatetimeAEST, format = "%Y-%m-%dT%H:%M:%S")
streamflow_data$DatetimeAEST <- as_date(streamflow_data$DatetimeAEST)
streamflow_data$SiteID <- as.factor(streamflow_data$SiteID)
streamflow_data$QualityCode <- as.factor(streamflow_data$QualityCode)


# Extract metadata from data.act.gov.au

Query2 <- soql() %>%
  soql_add_endpoint("https://www.data.act.gov.au/resource/tsq4-63ge.json") %>%
  soql_select("Siteid, siteName, latitude, longitude") %>%
  as.character()

My_meta <- read.socrata(Query2)

My_meta$Siteid <- as.factor(My_meta$Siteid)
My_meta$latitude <- as.numeric(My_meta$latitude)
My_meta$longitude <- as.numeric(My_meta$longitude)

#filter to relevant sites in the Rainfall dataset
My_meta <- My_meta[My_meta$Siteid %in% streamflow_data$SiteID,]

My_meta$Name <- paste0(My_meta$Siteid, " - ", My_meta$siteName)

Intermediate <- My_meta %>%
  select(Siteid, Name)

streamflow_data <- left_join(streamflow_data, Intermediate, by = c("SiteID"="Siteid"))

#write.csv(streamflow_data, "streamflow_data.csv", row.names = FALSE)