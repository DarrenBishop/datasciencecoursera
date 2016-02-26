df = read.fwf("getdata-wksst8110.for", c(10,9,4,9,4,9,4,9,4), header = F, sep = "\t", skip = 4)

print(head(df))

print(colSums(df[,2:9]))