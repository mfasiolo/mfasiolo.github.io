library(manipulate)

post_pdf <- function(n, mu, sig_mu){
  
  x <- rnorm(n, mu)
  
  xseq <- seq(-4*sig_mu, 4*sig_mu, length.out = 1e3)
  
  priorpdf <- dnorm(xseq, 0, sig_mu)
  postpdf <- dnorm(xseq, 
                   mean(x) * n * sig_mu^2 / (n * sig_mu^2 + 1), 
                   sqrt(sig_mu^2 / (n * sig_mu^2 + 1)))
  likelihood <- sapply(xseq, function(.x) prod(dnorm(.x, x)))
  
  likelihood <- likelihood / mean(likelihood) / (max(xseq) - min(xseq))
  
  plot(xseq, postpdf, ylim = sort(range(c(priorpdf, postpdf, likelihood))), type = 'l', col = 2, 
       xlab = expression(mu), ylab = "Density", lwd = 1.5)
  lines(xseq, priorpdf, col = 1, lwd = 1.5)
  lines(xseq, likelihood, col = 4, lwd = 1.5)
  abline(v = 0, lty = 2)
  rug(x)
  
  legend(x = "topright", c("prior", "likelihood", "posterior"), col = c(1, 4, 2), lty = 1)
  
}

manipulate(post_pdf(n, mu, sig_mu), 
           n = slider(1, 100), 
           mu = slider(-1, 1, step = 0.1), 
           sig_mu = slider(0.5, 5, step = 0.1))
