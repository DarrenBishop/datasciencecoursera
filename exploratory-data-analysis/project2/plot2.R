library(dplyr)

readData <- function() {
  
  nei = tbl_df(readRDS("summarySCC_PM25.rds"))
  
  scc = tbl_df(readRDS("Source_Classification_Code.rds"))
  
  nei %>% inner_join(scc, by = "SCC")
}

#dt = readData()

dts = dt %>%
  filter(fips == "24510") %>%
  group_by(year) %>%
  summarise(TotalPM2.5 = sum(Emissions))

png("plot2.png", width = 480, height = 480)

par(pin = c(5, 5), mar = c(3, 5, 3, 1))

xaxis = seq(1999, 2008, by=3)

ylim = with(dts, c(min(TotalPM2.5), max(TotalPM2.5)))

yaxis = pretty(seq(ylim[1], ylim[2], length.out = 6) / 1e3)

plot(dts$year, dts$TotalPM2.5 / 1e3, type = "l", axes = F, ylab = "Total PM2.5 (thousands)", col = "red")
title("Total Emissions from PM2.5 in Baltimore City, Maryland")
axis(side = 1, at = xaxis)
axis(side = 2, at = yaxis)
box()

dev.off()
