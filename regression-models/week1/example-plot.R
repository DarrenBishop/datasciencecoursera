library(ggplot2)
library(manipulate)

x <- c(0.18, -1.54, 0.42, 0.95)

w <- c(2, 1, 3, 1)

# myHist <- function(mu){
#     mse <- mean(w * (x - mu)^2)
#     g <- ggplot(data.frame(x = x), aes(x = x)) + geom_histogram(fill = "salmon", colour = "black", binwidth=0.25)
#     g <- g + geom_vline(xintercept = mu, size = 3)
#     g <- g + ggtitle(paste("mu = ", mu, ", MSE = ", round(mse, 4), sep = ""))
#     g
# }
# manipulate(myHist(mu), mu = slider(0.14, 0.4, step = 0.001))

mus = c(0.0025, 1.077, 0.3, 0.1471)

print(sapply(mus, function(mu) sum(w * ((x - mu)^2))))

print(sum(w * (x - mus[4])^2))
