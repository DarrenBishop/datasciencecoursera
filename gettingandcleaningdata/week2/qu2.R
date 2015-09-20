library(sqldf)

acs = read.csv("getdata-data-ss06pid.csv")

sqldf("select pwgtp1 from acs where AGEP < 50")
