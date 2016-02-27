library(dplyr)
library(ggplot2)

readData <- function() {
  
  nei = tbl_df(readRDS("summarySCC_PM25.rds"))
  
  scc = tbl_df(readRDS("Source_Classification_Code.rds"))
  
  nei %>% inner_join(scc, by = "SCC")
}

#dt = readData()

dts = dt %>% filter(grepl("Mobile", EI.Sector) & fips == "24510") %>% group_by(EI.Sector, year) %>% summarise(TotalPM2.5 = sum(Emissions))

png("plot5.png", width = 720, height = 480)

print(qplot(year, log(TotalPM2.5), data = dts, geom = "line", main = "Motor Vehicle Emissions for Baltimore City, Maryland", col = factor(EI.Sector), ylim = c(0,7))
      + geom_line(size = 2)
)

dev.off()
