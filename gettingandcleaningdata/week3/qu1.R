library(dplyr)

url = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"

destfile = "microdata.csv"

#download.file(url, destfile)

dt = tbl_df(read.csv(file = destfile))

agricultureLogical = dt$ACR == 3 & dt$AGS == 6

print(head(which(agricultureLogical), 3))
