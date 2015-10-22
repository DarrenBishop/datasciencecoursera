library(dplyr)

readDate <- function() {
  
  nei = tbl_df(readRDS("summarySCC_PM25.rds"))
  
  scc = tbl_df(readRDS("Source_Classification_Code.rds"))
  
  nei %>% inner_join(scc, by = SCC)
}

#dt = readData()

dts = dt %>%
  group_by(year) %>%
  summarise(TotalPM2.5 = sum(Emissions))

png("plot1.png", width = 480, height = 480)

par(pin = c(5, 5), mar = c(3, 5, 3, 1))

xaxis = seq(1999, 2008, by=3)

ylim = with(dts, c(min(TotalPM2.5), max(TotalPM2.5))) / 1e6

yaxis = 2:8

plot(dts$year, dts$TotalPM2.5/1e6, type = "l", axes = F, ylab = "Total PM2.5 (millions)", col = "red")
title("Total Emissions from PM2.5 in the United States")
axis(side = 1, at = xaxis)
axis(side = 2, at = yaxis)
box()

dev.off()
