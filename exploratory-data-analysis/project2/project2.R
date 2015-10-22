library(dplyr)

## This first line will likely take a few seconds. Be patient!

nei = tbl_df(readRDS("summarySCC_PM25.rds"))

scc = tbl_df(readRDS("Source_Classification_Code.rds"))
