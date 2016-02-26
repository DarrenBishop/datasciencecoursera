library(dplyr)

url = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"

destfile = "gdpdata.csv"

#download.file(url, destfile)

gdpdt = tbl_df(read.csv(file = destfile, skip = 4, nrows = 190))

gdpdt = rename(gdpdt, CountryCode = X, GDP.Rank = X.1)

#gdpdt$GDP.Rank = as.numeric(gdpdt$GDP.Rank)

url = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"

destfile = "edudata.csv"

#download.file(url, destfile)

edudt = tbl_df(read.csv(file = destfile))

joineddt = gdpdt %>%
  inner_join(edudt, by = "CountryCode") %>%
  arrange(desc(GDP.Rank)) %>%
  select(GDP.Rank, Income.Group) %>%
  group_by(Income.Group) %>%
  summarise_each(funs(mean))

print(joineddt)
