goftests <- function(x, m1, m2, df)
{
k <- length(x)
  m1 <- m1 + .00000000000000001
  m2 <- m2 + .00000000000000001
  x2p <- sum(((m1-m2)^2)/m1)
  pvalp <- 1 - pchisq(x2p, df)
  g2 <- 2*sum(x*(log(m2/m1)))
  pvalg <- 1-pchisq(g2, df)
  cat("\n", "       Pearson test = ", round(x2p,2))
  cat("\n", " Degrees of freedom = ", df)
cat("\n", "p-value = ", round(pvalp,5))
cat("\n", "Deviance test = ", round(g2,2))
cat("\n", "       df = ", df)
cat("\n", " p-value = " , round(pvalg,5),"\n")
}


