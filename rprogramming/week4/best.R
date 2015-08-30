best <- function(state, outcome) {
  ## Read outcome data
  odf <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  vl <- data.frame(ci=c(11,17,23), row.names=c("heart attack", "heart failure", "pneumonia"))
  
  ## Check that state and outcome are valid
  if (!state %in% levels(factor(odf$State))) stop ("invalid state")
  if (!outcome %in% row.names(vl)) stop("invalid outcome")
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  ci <- vl[outcome,1]
  name <- names(odf)[ci]
  odf[, ci] <- as.numeric(odf[, ci])
  minMR <- min(odf[odf$State == state, ci], na.rm = T)
  h <- odf[odf$State == state & odf[, ci] == minMR, 2]
  hospital <- head(sort(h[!is.na(h)]))
  hospital
}
