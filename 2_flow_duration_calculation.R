#This analysis file is to compute flow duration statistics for each site.
#This will underpin one of the figures for the webapp
#This computation will consist of:

#1 Flow exceedance curve for each site
#2 Monthly interquarile range of the flow exceedance curve for each site

#Libraries
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

# Read in data

streamflow_data2 <- read.csv("streamflow_data.csv", header = T)
str(streamflow_data2)
streamflow_data2$DatetimeAEST <- as.Date(streamflow_data2$DatetimeAEST)
streamflow_data2$SiteID <- as.factor(streamflow_data2$SiteID)
streamflow_data2$QualityCode <- as.factor(streamflow_data2$QualityCode)

streamflow_data2 <- streamflow_data2 %>% 
  select(-Name)


max(streamflow_data2$DatetimeAEST)
min(streamflow_data$DatetimeAEST)

#bind data with data pulled from API
streamflow_data <- bind_rows(streamflow_data, streamflow_data2)

streamflow_data$DatetimeAEST <- as.Date(streamflow_data$DatetimeAEST)
streamflow_data$SiteID <- as.factor(streamflow_data$SiteID)


# FEC calculations

#Taken from work undertaken by Kat Vincent and myself for a project led by Kat


streamflow_data <- streamflow_data %>%
  filter(as.numeric(QualityCode) < 200) %>%
  group_by(SiteID) %>%
  arrange(desc(Value)) %>%
  mutate(ranked_flow = seq(1:length(Value))) %>%
  mutate(FEC = 100*(ranked_flow/(length(Value)+1))) %>%
  arrange(DatetimeAEST) %>% 
  select(-ranked_flow)


sites <- unique(streamflow_data$SiteID)
output <- list()
##First attempt at loop

for(i in 1:length(sites)){
  My_site <- sites[i]
  
  sub_streamflow_data <- streamflow_data %>%
    filter(SiteID == My_site)
  
  hold2 <- sub_streamflow_data$FEC
  hold1 <- numeric(length(hold2))
  
  hold1[sapply(My_vector, function(j) which.min(abs(j - hold2)))] <- 1
  
  sub_streamflow_data$hold1 <- hold1
  
  sub_streamflow_data <- sub_streamflow_data %>%
    filter(hold1 == 1) %>%
    select(SiteID, FEC, Value)
  
  sub_streamflow_data <- as.data.frame(sub_streamflow_data)
  
  output[[i]] <- sub_streamflow_data
  rm(hold1)
  rm(hold2)
  rm(sub_streamflow_data)
}
Site_FEC <- bind_rows(output)

write.csv(Site_FEC, "Site_FEC.csv", row.names = FALSE)

###

#test

streamflow_data2 %>%
  ggplot() +
  geom_line(aes(x = FEC, y = Value)) +
  scale_y_log10() +
  facet_wrap(~SiteID, scales = "free_y")
#Looks good!!

#IQR by month

IQR <- streamflow_data %>%
  mutate(Month = month(DatetimeAEST, label = TRUE, abbr = TRUE)) %>%
  group_by(SiteID, Month) %>%
  summarise(Low = quantile(Value, 0.05), First = quantile(Value, 0.25), Median = quantile(Value, 0.5), Last = quantile(Value, 0.75), High = quantile(Value, 0.95))

write.csv(IQR, "IQR.csv", row.names = FALSE)
