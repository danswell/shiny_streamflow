install.packages("rsconnect")

rsconnect::setAccountInfo(name='danswell',
                          token=Sys.getenv("Shiny_token"),
                          secret=Sys.getenv("shiny_secret"))

getwd()
<<<<<<< HEAD
rsconnect::deployApp("/cloud/project/App", appName = "Open_data_streamflow")
Y
=======
rsconnect::deployApp('C:/Users/dansw/Documents/R git2/shiny_streamflow/App', appName = "Open_data_streamflow")
Y
>>>>>>> 81ce441a441b85fa03785e0f09a07c3a3819e69b
