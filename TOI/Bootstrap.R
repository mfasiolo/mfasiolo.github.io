
bootstr <- function(n, B, sig.mu){
  
  x <- rnorm(n, 4)
  
  xseq <- seq(min(x), max(x), length.out = 1e3)
  
  truePDF <- dnorm(xseq, mean(x), 1/sqrt(n))
  
  mubs <- rep(0, B)
  
  for (ii in 1:B) {
    xb <- sample(x, n, replace = TRUE) 
    mubs[ii] <- mean(xb)
  }
  
  ciBoot <- quantile(mubs, c(0.025, 0.975))
  
  mu.hat <- mean(x)
  mu.sd <- 1/sqrt(n)
  ciExac <- c(mu.hat - 1.96*mu.sd, mu.hat + 1.96*mu.sd)
  
  mub <- mean(x)*(n*sig.mu^2)/(n*sig.mu^2+1)
  sdb <- sqrt(sig.mu^2/(n*sig.mu^2+1))
  ciBayes <- c(mub - 1.96*sdb, mub + 1.96*sdb)

  hist(mubs, prob = TRUE, xlab = "mu", main = "")
  abline(v = ciExac[1], lwd = 1.5)
  abline(v = ciExac[2], lwd = 1.5)
  abline(v = ciBoot[1], col = 4, lwd = 1.5)
  abline(v = ciBoot[2], col = 4, lwd = 1.5)
  abline(v = ciBayes[1], col = 2, lwd = 1.5)
  abline(v = ciBayes[2], col = 2, lwd = 1.5)
  lines(xseq, truePDF, col = 1, lwd = 1.5)

 legend(x = "topright", c("Exact", "Bayes", "Bootstrap"), 
        col = c(1, 2, 4), lty = 1)
  
}

###################################################

library(manipulate)

# Example in the slides
set.seed(1)
bootstr(50, 1e3, 3)

# Interactive code
manipulate(bootstr(n, B, sig.mu), 
           n = slider(1, 1000),
           B = slider(10, 1e4, step = 10),
           sig.mu = slider(1, 10, step = 1))
