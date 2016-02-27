library(dplyr)

download_and_load <- function(url, destfile, header = T, skip = 0, take = -1, na = "NA") {
    
    if (!file.exists(destfile)) {
        
        download.file(url = url, destfile = destfile, mode = "wb")
    }
    
    tbl_df(read.csv(destfile, header = header, skip = skip, nrows = take, na.strings = na))
}

# Question 1
dt1 = download_and_load("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv", "getdata-data-ss06hid.csv")

split_names = mapply(strsplit, names(dt1), "wgtp")

print(split_names[123])

# Question 2
dt2 = download_and_load("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv", "getdata-data-GDP.csv", 5, 190, header = F)

dt2.1 = dt2 %>%
    select(num_range("V", c(1,2,4,5))) %>%
    rename(CountryCode = V1, Rank = V2, Economy = V4, GDP = V5) %>%
    mutate(CountryCode = as.character(CountryCode), Economy = as.character(Economy), GDP = as.numeric(gsub(',', '', GDP)))

print(mean(dt2.1$GDP))

# Question 3 (uses Qu.2 data)

dt3 = dt2.1 %>%
    filter(grepl("^United", Economy))

print(nrow(dt3))

# Quesstion 4 (uses Qu.2 data)

dt4 = download_and_load("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv",
                        "getdata-data-EDSTATS_Country.csv", na = "") %>%
    inner_join(dt2.1, by = "CountryCode") %>%
    filter(grepl("^Fiscal year end: June", Special.Notes))

# Question 5
library(quantmod)
library(lubridate)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)
year2012 = sampleTimes[year(sampleTimes) == "2012"]
print(length(year2012))
year2012.Mon = year2012[wday(year2012, label = T) == "Mon"]
print(length(year2012.Mon))
