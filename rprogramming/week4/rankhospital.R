rankhospital <- function(state, outcome, num = "best") {
  
  odf <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  vl <- data.frame(ci=c(11,17,23), row.names=c("heart attack", "heart failure", "pneumonia"))
  
  ## Check that state and outcome are valid
  if (!state %in% levels(factor(odf$State))) stop ("invalid state")
  if (!outcome %in% row.names(vl)) stop("invalid outcome")
  
  ci <- vl[outcome,1]
  odf[, ci] <- as.numeric(odf[, ci])
  sodf <- odf[odf$State == state,]
  if (is.numeric(num) & nrow(sodf) < num) return(NA)
  
  osodf <- sodf[order(sodf[,ci], sodf[,2]), c(2,ci)]
  
  osodf <- osodf[complete.cases(osodf), ]
  
  rank <-
    if (is.numeric(num)) num
    else if (num == "best") 1
    else nrow(osodf)
  
  osodf[rank,1]
}
