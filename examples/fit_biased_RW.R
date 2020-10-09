## example for a biased RW

## sample size
nn <- 30

## negative bias
uu <- -0.2

## process SD
sigma_p <- 0.6
## obs SD
sigma_o <- 0.3

## generate process & obs errors
set.seed(123)
## process errors
ww <- rnorm(nn, 0, sigma_p)
## obs errors
vv <- rnorm(nn, 0, sigma_o)

## unbiased RW
xu <- cumsum(ww)
## biased RW
xb <- cumsum(ww + uu)

plot.ts(cbind(xu, xb))

## unbiasd RW with obs error
yu <- xu + vv
## biased RW with obs error
yb <- xb + vv

plot.ts(cbind(yu, yb))

## estimate bias with MARSS
## model defn for unbiased RW
mod_list <- list(
  B = matrix(1),
  U = matrix(0),
  Q = matrix("q"),
  Z = matrix(1),
  A = matrix(0),
  R = matrix("r")
)
## estimated parameters for unbiased RW
fit_rw <- MARSS::MARSS(matrix(yb, nrow = 1), mod_list)

## update model defn for biased RW
mod_list$U <- matrix("bias")
## estimated parameters for unbiased RW
fit_brw <- MARSS::MARSS(matrix(yb, nrow = 1), mod_list)

## compare AIC (biased RW is lower)
AIC(fit_rw, fit_brw)

## bootstrap bias estimate
boots <- MARSS::MARSSparamCIs(fit_brw, nboot = 1000)
## 95% CI
c(boots$par.lowCI$U, boots$par.upCI$U)

