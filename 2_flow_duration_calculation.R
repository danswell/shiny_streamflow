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

streamflow_data <- read.csv("streamflow_data.csv", header = T)
str(streamflow_data)

streamflow_data$DatetimeAEST <- as.Date(streamflow_data$DatetimeAEST)
streamflow_data$SiteID <- as.factor(streamflow_data$SiteID)


# FEC calculations

#Taken from work undertaken by Kat Vincent and myself for a project led by Kat


streamflow_data <- streamflow_data %>%
  filter(QualityCode < 200) %>%
  group_by(SiteID) %>%
  arrange(desc(Value)) %>%
  mutate(ranked_flow = seq(1:length(Value))) %>%
  mutate(FEC = 100*(ranked_flow/(length(Value)+1))) %>%
  arrange(DatetimeAEST)

##Now to subset this down to whole numbers for simplicity

My_vector  <- as.data.frame(seq(0,99))
colnames(My_vector) <- c("Value")

My_vector <- 1:100
Stream_fec <- streamflow_data$FEC
#To extract the nearest FEC value to the whole number, I can use the function which.min

I <- 6:10
S <- numeric(length(T))
S[sapply(I, function(i) which.min(abs(i - T)))] <- 1
S


streamflow_index <- numeric(length(Stream_fec))
streamflow_index[sapply(My_vector, function(i) which.min(abs(i-Stream_fec)))] <- 1
streamflow_index

streamflow_data$streamflow_index <- streamflow_index


streamflow_data2 <- streamflow_data %>%
  filter(streamflow_index == 1)

#This works, now to write it into a loop?


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
    select(SiteID, Name, FEC, Value)
  
  sub_streamflow_data <- as.data.frame(sub_streamflow_data)
  
  output[[i]] <- sub_streamflow_data
  rm(hold1)
  rm(hold2)
  rm(sub_streamflow_data)
}
streamflow_data2 <- bind_rows(output)
###

#test

streamflow_data2 %>%
  ggplot() +
  geom_point(aes(x = FEC, y = Value)) +
  scale_y_log10() +
  facet_wrap(~SiteID, scales = "free_y")
#Looks good!!
