dir.create("~/Workspace/datasciencecoursera/rprogramming/quiz1/", showWarnings = F)

setwd("~/Workspace/datasciencecoursera/rprogramming/quiz1/")

data <- read.csv("hw1_data.csv")

cat(sprintf("Names of data: %s\n\n", paste(names(data), collapse = ", ")))

#print(data[1:2,])

print(head(data, 2))

print(nrow(data))

#print(data[nrow(data)-1:0,])

print(tail(data, 2))

print(data[47,]$Ozo)

print(nrow(data[is.na(data$Ozo),]))

print(mean(data[!is.na(data$Ozo),]$Oz))

print(data[complete.cases(data) & data$Oz > 31 & data$T > 90, ])

print(mean(data[complete.cases(data) & data$Oz > 31 & data$T > 90, ]$Sol))

print(mean(data[data$Month == 6,]$T))

print(max(data[!is.na(data$Oz) & data$Month == 5,]$Oz))
