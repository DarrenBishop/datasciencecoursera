library(data.table)
library(xlsx)
library(XML)

#dateDownloaded <- date()

fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
filedest <- "2006-microdata.csv"

#download.file(url = fileurl, destfile = filedest)

df <- read.csv(filedest, header = T)

val1M <- df[!is.na(df$VAL) & df$VAL == 24, ]

print(nrow(val1M))

xlsxurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"

xlsxdest <- "FDATA.gov_NGAP.xlsx"

#download.file(url = xlsxurl, destfile = xlsxdest)

#dateDownloaded <- date()

dat <- read.xlsx(xlsxdest, sheetIndex = 1, rowIndex = 18:23, colIndex = 7:15)

print(sum(dat$Zip*dat$Ext,na.rm=T))

xmlurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"

xmldest <- "restaurants.xml"

#download.file(url = xmlurl, destfile = xmldest)

doc <- xmlTreeParse(xmldest, useInternal = T)

rootNode <- xmlRoot(doc)

zips <- xpathSApply(rootNode, "//zipcode", xmlValue)

print(length(zips[zips == 21231]))

fileurl2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
filedest2 <- "2006-microdata2.csv"

#download.file(url = fileurl2, destfile = filedest2)

DT <- fread(filedest2)

repetitions <- 50

print("{mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)}")
print(rowMeans(replicate(repetitions, system.time({mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)}))))

print("tapply(DT$pwgtp15,DT$SEX,mean)")
print(rowMeans(replicate(repetitions, system.time(tapply(DT$pwgtp15,DT$SEX,mean)))))

print("sapply(split(DT$pwgtp15,DT$SEX),mean)")
print(rowMeans(replicate(repetitions, system.time(sapply(split(DT$pwgtp15,DT$SEX),mean)))))

print("DT[,mean(pwgtp15),by=SEX]")
print(rowMeans(replicate(repetitions, system.time(DT[,mean(pwgtp15),by=SEX]))))
