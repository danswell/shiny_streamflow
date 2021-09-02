install.packages("rsconnect")

rsconnect::setAccountInfo(name='danswell',
                          token=Sys.getenv("Shiny_token"),
                          secret=Sys.getenv("shiny_secret"))

getwd()
rsconnect::deployApp("/cloud/project/App", appName = "Open_data_streamflow")
Y