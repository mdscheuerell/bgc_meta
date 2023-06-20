## example for a biased RW

## sample size
nn <- 30

## process SD
sigma_p <- 0.8
## obs SD
sigma_o <- 0.4

## generate process & obs errors
set.seed(123)

## process errors
# w1 <- rnorm(nn, 0, sigma_p)
# w2 <- rnorm(nn, 0, sigma_p)
# w3 <- rnorm(nn, 0, sigma_p)
# w4 <- rnorm(nn, 0, sigma_p)
# w5 <- rnorm(nn, 0, sigma_p)
# w6 <- rnorm(nn, 0, sigma_p)
# w7 <- rnorm(nn, 0, sigma_p)
# w8 <- rnorm(nn, 0, sigma_p)

ww <- matrix(rnorm(nn*8, 0, sigma_p), nn, 8)

## obs errors
# v1 <- rnorm(nn, 0, sigma_o)
# v2 <- rnorm(nn, 0, sigma_o)
# v3 <- rnorm(nn, 0, sigma_o)
# v4 <- rnorm(nn, 0, sigma_o)
# v5 <- rnorm(nn, 0, sigma_o)
# v6 <- rnorm(nn, 0, sigma_o)
# v7 <- rnorm(nn, 0, sigma_o)
# v8 <- rnorm(nn, 0, sigma_o)

vv <- matrix(rnorm(nn*8, 0, sigma_o), nn, 8)

## RW's
# x1 <- cumsum(w1)
# x2 <- cumsum(w2)
# x3 <- cumsum(w3)
# x4 <- cumsum(w4)
# x5 <- cumsum(w5)
# x6 <- cumsum(w6)
# x7 <- cumsum(w7)
# x8 <- cumsum(w8)

xx <- apply(ww, 2, cumsum)

## RW with obs error
## set 1: 1 obs of 8 states
# y1a <- x1 + v1
# y1b <- x2 + v2
# y1c <- x3 + v3
# y1d <- x4 + v4
# y1e <- x5 + v5
# y1f <- x6 + v6
# y1g <- x7 + v7
# y1h <- x8 + v8

y1 <- xx + vv

## set 2: 2 obs of 4 states
# y2a <- x1 + v1
# y2b <- x1 + v2
# y2c <- x2 + v3
# y2d <- x2 + v4
# y2e <- x3 + v5
# y2f <- x3 + v6
# y2g <- x4 + v7
# y2h <- x4 + v8

y2 <- xx[,c(1,1,2,2,3,3,4,4)] + vv

## set 3: 4 obs of 2 states
# y3a <- x1 + v1
# y3b <- x1 + v2
# y3c <- x1 + v3
# y3f <- x1 + v4
# y3e <- x3 + v5
# y3f <- x3 + v6
# y3g <- x3 + v7
# y3h <- x3 + v8

y3 <- xx[,c(1,1,1,1,3,3,3,3)] + vv

## set 4: 8 obs of 1 state
# y3a <- x2 + v1
# y3b <- x2 + v2
# y3c <- x2 + v3
# y3f <- x2 + v4
# y3e <- x2 + v5
# y3f <- x2 + v6
# y3g <- x2 + v7
# y3h <- x2 + v8

y4 <- xx[,rep(2, 8)] + vv


clr <- viridis::plasma(8, begin = 0.1, end = 0.95)

pdf("multiple_states_ex.pdf", width = 9, height = 3)

par(mfrow = c(1,4), mai = c(0.4, 0.4, 0.1, 0.1), omi = c(0, 0, 0.2, 0))

matplot(y1, las = 1, col = clr, type = "l", lty = "solid",
        xaxt = "n", yaxt = "n", ylab = "", xlab = "")
mtext("A) Catchment", 3, line = 0.5, adj = 0, cex = 1.2)
mtext(expression(italic(y)), 2, line = 1, cex = 1.2)
mtext("Time", 1, line = 1, cex = 1.2)

matplot(y2, las = 1, col = clr, type = "l", lty = "solid",
        xaxt = "n", yaxt = "n", ylab = "", xlab = "")
mtext("B) Site", 3, line = 0.5, adj = 0, cex = 1.2)
mtext(expression(italic(y)), 2, line = 1, cex = 1.2)
mtext("Time", 1, line = 1, cex = 1.2)

matplot(y3, las = 1, col = clr, type = "l", lty = "solid",
        xaxt = "n", yaxt = "n", ylab = "", xlab = "")
mtext("C) Region", 3, line = 0.5, adj = 0, cex = 1.2)
mtext(expression(italic(y)), 2, line = 1, cex = 1.2)
mtext("Time", 1, line = 1, cex = 1.2)

matplot(y4, las = 1, col = clr, type = "l", lty = "solid",
        xaxt = "n", yaxt = "n", ylab = "", xlab = "")
mtext("D) N America", 3, line = 0.5, adj = 0, cex = 1.2)
mtext(expression(italic(y)), 2, line = 1, cex = 1.2)
mtext("Time", 1, line = 1, cex = 1.2)

dev.off()

