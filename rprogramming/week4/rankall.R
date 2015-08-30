rankall <- function(outcome, num = "best") {
  
  odf <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  vl <- data.frame(ci=c(11,17,23), row.names=c("heart attack", "heart failure", "pneumonia"))
  
  if (!outcome %in% row.names(vl)) stop("invalid outcome")
  
  ci <- vl[outcome,1]
  odf[, ci] <- as.numeric(odf[, ci])
  
  rankstate <- function(df) {
    
    state <- df[1,7]
    
    hospital <- if (is.numeric(num) & nrow(df) < num) NA
    else {
      oodf <- df[order(df[,ci], df[,2]), c(2,ci)]
      
      oodf <- oodf[complete.cases(oodf), ]
      
      rank <-
        if (is.numeric(num)) num
        else if (num == "best") 1
        else nrow(oodf)
      
      oodf[rank,1]
    }
    
    data.frame(hospital=c(hospital), state=c(state), row.names = c(state))
  }
  
  Reduce(rbind, by(odf, odf$State, rankstate))
}
