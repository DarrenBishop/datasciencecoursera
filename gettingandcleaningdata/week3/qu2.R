library(jpeg)

url = "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"

destfile = "getdata-jeff.jpg"

#download.file(url, destfile)

img = readJPEG(source = destfile, native = T)

print(quantile(img, c(0.3, 0.8)))