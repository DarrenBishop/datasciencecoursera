library(dplyr)
library(ggplot2)

readData <- function() {
  
  nei = tbl_df(readRDS("summarySCC_PM25.rds"))
  
  scc = tbl_df(readRDS("Source_Classification_Code.rds"))
  
  nei %>% inner_join(scc, by = "SCC")
}

dt = readData()

fips_name = c("24510" = "Baltimore City", "06037" = "Los Angeles County")

dts = dt %>% filter(grepl("Mobile", EI.Sector) & fips %in% c("24510", "06037")) %>% mutate(Location = fips_name[fips]) %>% group_by(Location, EI.Sector, year) %>% summarise(TotalPM2.5 = sum(Emissions))

png("plot6.png", width = 960, height = 480)

print(qplot(year, log(TotalPM2.5), data = dts, geom = "line", main = "Motor Vehicle Emissions", col = EI.Sector, facets = . ~ Location, ylim = c(0,9))
      + geom_line(size = 2)
)

dev.off()
