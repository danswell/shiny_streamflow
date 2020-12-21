#This is where I try to work out the different charts

Site_FEC %>% 
  filter(SiteID == "410729") %>% 
  ggplot()+
  geom_line(aes(x = FEC, y = Value), color = "blue")+
  scale_y_log10() + 
  theme_bw()+
  labs(x = "Flow exceedance Probability", y = "ML/Day")

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