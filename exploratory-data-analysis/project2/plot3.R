library(dplyr)
library(ggplot2)

readData <- function() {
  
  nei = tbl_df(readRDS("summarySCC_PM25.rds"))
  
  scc = tbl_df(readRDS("Source_Classification_Code.rds"))
  
  nei %>% inner_join(scc, by = "SCC")
}

#dt = readData()

dts = dt %>%
  filter(fips == "24510") %>%
  group_by(type, year) %>%
  summarise(TotalPM2.5 = sum(Emissions))

png("plot3.png", width = 480, height = 480)

print(qplot(year, TotalPM2.5, data = dts, geom = "line", main = "Emissions in Baltimore City, Maryland", col = factor(type))
        + geom_line(size = 2)
        + scale_color_discrete("Type")
        + theme(legend.position="top")
      )

dev.off()
