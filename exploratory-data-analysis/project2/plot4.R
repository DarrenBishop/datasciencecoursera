library(dplyr)
library(ggplot2)

readData <- function() {
  
  nei = tbl_df(readRDS("summarySCC_PM25.rds"))
  
  scc = tbl_df(readRDS("Source_Classification_Code.rds"))
  
  nei %>% inner_join(scc, by = "SCC")
}

dt = readData()

dts = dt %>% filter(grepl("Coal", EI.Sector)) %>% group_by(EI.Sector, year) %>% summarise(TotalPM2.5 = sum(Emissions))

png("plot4.png", width = 480, height = 480)

print(qplot(year, log(TotalPM2.5), data = dts, geom = "line", main = "Coal Combustion Across United States", col = factor(EI.Sector))
        + geom_line(size = 2)
        + scale_color_discrete("Source")
        + theme(legend.position="top", legend.direction="vertical")
)

dev.off()
