#library(httr)
#library(XML)
#
#data = GET("http://biostat.jhsph.edu/~jleek/contact.html")
#
#html = htmlParse(content(data, as="text"), asText=T)

conn = url("http://biostat.jhsph.edu/~jleek/contact.html")

html = readLines(conn)

close(conn)

print(tapply(html[c(10,20,30,100)], nchar))