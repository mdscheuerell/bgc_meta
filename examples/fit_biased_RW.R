## example for a biased RW

## sample size
nn <- 30

## positive bias
uu <- 0.3

## process SD
sigma_p <- 0.8
## obs SD
sigma_o <- 0.6

## generate process & obs errors
set.seed(1)
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

## get bias estimate
u_hat <- coef(fit_brw)$U

## bootstrap bias estimate
boots <- MARSS::MARSSparamCIs(fit_brw, nboot = 1000)
## 95% CI
c(boots$par.lowCI$U, boots$par.upCI$U)

## Is there a trend via Mann-Kendall? Yes
trend::mk.test(yb)
## estimate trend via Sen's method
sen_fit <- trend::sens.slope(yb)
sen_int <- median(yb - sen_fit$estimates * seq(nn))
y_hat <- sen_fit$estimates * seq(nn) + sen_int
sen_resid <- yb - y_hat

## estimate trend via lm()
# fit_lm <- lm(yb ~ seq(nn))
# summary(fit_lm)
# y_hat <- fitted(fit_lm)
# confint(fit_lm)

ww_hat <- residuals(fit_brw, type = "tT")
ww_hat <- subset(ww_hat, name=="model")$.resids

## plots of fits, residuals, and ACF
pdf("biased_rw_ex.pdf", width = 7, height = 5)

par(mfrow = c(2,3), mai = c(0.7, 0.7, 0.1, 0.1), omi = c(0, 0, 0.2, 0))

## BRW fit
plot.ts(yb, pch = 16, type = "o", cex = 1.2, las = 1,
        ylim = range(cbind(y_hat, yb)), ylab = expression(italic(y)))
text(x = 0, y = 12.5, expression(paste(hat(b), " = 0.36 (0.10, 0.62)")), pos = 4, cex = 0.8)
lines(t(fit_brw$states), col = "dodgerblue", lwd = 2)
mtext("A", 3, line = 0.5, adj = 0, cex = 0.8)
## BRW residuals
plot.ts(ww_hat, lwd = 2, las = 1, col = "dodgerblue",
        ylim = range(sen_resid), ylab = "Residuals")
mtext("B", 3, line = 0.5, adj = 0, cex = 0.8)
## BRW ACF
acf(ww_hat, las = 1, lag.max = 10,
    xlim = c(1,10), ylim = c(-0.6, 0.6))
mtext("C", 3, line = 0.5, adj = 0, cex = 0.8)

## Sen's slope
plot.ts(yb, pch = 16, type = "o", cex = 1.2, las = 1,
        ylim = range(cbind(y_hat, yb)), ylab = expression(italic(y)))
text(x = 0, y = 12.5, expression(paste(hat(b), " = 0.44 (0.39, 0.50)")), pos = 4, cex = 0.8)
lines(y_hat, col = "indianred", lwd = 2)
mtext("D", 3, line = 0.5, adj = 0, cex = 0.8)
## Sen's residuals
plot.ts(sen_resid, lwd = 2, las = 1, col = "indianred",
        ylim = range(sen_resid), ylab = "Residuals")
mtext("E", 3, line = 0.5, adj = 0, cex = 0.8)
## Sen's ACF
acf(sen_resid, las = 1, lag.max = 10,
    xlim = c(1,10), ylim = c(-0.6, 0.6))
mtext("F", 3, line = 0.5, adj = 0, cex = 0.8)

# ## lm fit
# plot.ts(yb, pch = 16, type = "o", cex = 1.2, las = 1,
#         ylim = range(cbind(y_hat, yb)), ylab = expression(italic(y)))
# text(x = 0, y = 12, expression(paste(hat(b), " = 0.43 (0.38, 0.48)")), pos = 4, cex = 0.8)
# lines(y_hat, col = "indianred", lwd = 2)
# mtext("D", 3, line = 0.5, adj = 0, cex = 0.8)
# ## lm residuals
# plot.ts(residuals(fit_lm), lwd = 2, las = 1, col = "indianred",
#         ylim = range(residuals(fit_lm)), ylab = "Residuals")
# mtext("E", 3, line = 0.5, adj = 0, cex = 0.8)
# ## lm ACF
# acf(residuals(fit_lm), las = 1, lag.max = 10,
#     xlim = c(1,10), ylim = c(-0.6, 0.6))
# mtext("F", 3, line = 0.5, adj = 0, cex = 0.8)

dev.off()

