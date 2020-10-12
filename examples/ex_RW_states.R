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
w1 <- rnorm(nn, 0, sigma_p)
w2 <- rnorm(nn, 0, sigma_p)
w3 <- rnorm(nn, 0, sigma_p)
## obs errors
v1 <- rnorm(nn, 0, sigma_o)
v2 <- rnorm(nn, 0, sigma_o)
v3 <- rnorm(nn, 0, sigma_o)

## RW's
x1 <- cumsum(w1)
x2 <- cumsum(w2)
x3 <- cumsum(w3)

## RW with obs error
## set 1: 3 obs of 3 states
y1a <- x1 + v1
y1b <- x2 + v2
y1c <- x3 + v3

## set 2: 3 obs of 2 state
y2a <- x1 + v1
y2b <- x1 + v2
y2c <- x2 + v3

## set 3: 3 obs of 1 state
y3a <- x1 + v1
y3b <- x1 + v2
y3c <- x1 + v3


pdf("multiple_states_ex.pdf", width = 7, height = 2.5)

par(mfrow = c(1,3), mai = c(0.7, 0.7, 0.1, 0.1), omi = c(0, 0, 0.2, 0))

rng <- range(c(y1a, y1b, y1c, y2a, y2b, y2c, y3a, y3b, y3c))
plot.ts(y1a, las = 1, ylab = expression(italic(y)), col = "dodgerblue", ylim = rng)
# lines(x1, lwd = 2, col = "dodgerblue")
lines(y1b, col = "indianred")
# lines(x2, lwd = 2, col = "indianred")
lines(y1c, col = "purple")
# lines(x3, lwd = 2, col = "purple")
mtext("A) 3 states", 3, line = 0.5, adj = 0, cex = 0.8)

plot.ts(y2a, las = 1, ylab = expression(italic(y)), col = "dodgerblue", ylim = rng)
lines(y2b,  col = "blue")
# lines(x1, lwd = 2, col = "darkblue")
lines(y2c, col = "indianred")
# lines(x2, lwd = 2, col = "purple")
mtext("B) 2 states", 3, line = 0.5, adj = 0, cex = 0.8)

plot.ts(y3a, las = 1, ylab = expression(italic(y)), col = "darkblue", ylim = rng)
lines(y3b, col = "blue")
lines(y3c, col = "dodgerblue")
# lines(x2, lwd = 2, col = "purple")
mtext("C) 1 state", 3, line = 0.5, adj = 0, cex = 0.8)

dev.off()

