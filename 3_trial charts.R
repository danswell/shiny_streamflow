#This is where I try to work out the different charts

#Static FEC chart
Site_FEC %>% 
  filter(SiteID == "410729") %>% 
  ggplot()+
  geom_line(aes(x = FEC, y = Value), color = "blue")+
  scale_y_log10() + 
  theme_bw()+
  labs(x = "Flow exceedance Probability", y = "ML/Day")

##Monthly flow chart

IQR %>% 
  filter(SiteID == "410729") %>%
  ggplot()+
  geom_point(aes(x = Month, y = Median), color = "blue", size = 3) + 
  geom_errorbar(aes(x = Month, ymin = First, ymax = Last), width = 0, linetype = "dashed") + 
  scale_y_log10()+
  theme_bw()+
  labs(x = "Month", y = "Median Daily flow (ML/Day) +- IQR")

#Year
streamflow_data %>% 
  filter(SiteID == "410729") %>% 
  filter(DatetimeAEST > max(DatetimeAEST-365)) %>% 
  ggplot()+
  geom_line(aes(x = DatetimeAEST, y = Value), color = "blue") + 
  theme_bw()+
  labs(x = "Date", y = "ML/Day")

#30 days
streamflow_data %>% 
  filter(SiteID == "410729") %>% 
  filter(DatetimeAEST > max(DatetimeAEST-30)) %>% 
  ggplot()+
  geom_line(aes(x = DatetimeAEST, y = Value), color = "blue") + 
  theme_bw()+
  labs(x = "Date", y = "ML/Day")

#Week
streamflow_data %>% 
  filter(SiteID == "410729") %>% 
  filter(DatetimeAEST > max(DatetimeAEST-7)) %>% 
  ggplot()+
  geom_line(aes(x = DatetimeAEST, y = Value), color = "blue") + 
  theme_bw()+
  labs(x = "Date", y = "ML/Day")