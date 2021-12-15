#Bai tap 1
volume <- read.csv("volume.csv", header = TRUE); volume
attach(volume); names(volume)

x <- volume[, 2]
y <- volume[, 3]

t.test(x, y, alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)

test.leq.oneside <- function(x, y, mu0, sigma1, sigma2, alpha) {
  n <- length(x)
  m <- length(y)
  x.bar <- mean(x)
  y.bar <- mean(y)
  z0 <- (x.bar - y.bar - mu0) / sqrt((sigma1^2) / n + (sigma2^2) / m)
  p.value = pnorm(z0)
  cat('p-value = ', p.value, '\n')
  if (alpha >= p.value) {
    cat('Bac bo H0 voi alpha = ', alpha)
  } else {
    cat('Khong du co so de bac bo H0 voi alpha = ', alpha)
  }
  cat('\n')
}
test.leq.oneside(x, y, 0, 0.002, 0.0025, 0.05)
t.test(x, y, alternative = "less", var.equal = FALSE, conf.level = 0.95, correct = FALSE)

#Su dung z.test
install.packages('BSDA')
library(BSDA)

z.test(x, y, alternative = "less", mu = 0, sigma.x = 0.002, sigma.y = 0.0025, conf.level = 0.95)

test.geq.oneside <- function(x, y, mu0, sigma1, sigma2, alpha) {
  n <- length(x)
  m <- length(y)
  x.bar <- mean(x)
  y.bar <- mean(y)
  z0 <- (x.bar - y.bar - mu0) / sqrt((sigma1^2) / n + (sigma2^2) / m)
  p.value = 1 - pnorm(z0)
  cat('p-value = ', p.value, '\n')
  if (alpha >= p.value) {
    cat('Bac bo H0 voi alpha = ', alpha)
  } else {
    cat('Khong du co so de bac bo H0 voi alpha = ', alpha)
  }
  cat('\n')
}
test.geq.oneside(x, y, 0, 0.002, 0.0025, 0.05)
z.test(x, y, alternative = "greater", mu = 0, sigma.x = 0.002, sigma.y = 0.0025, conf.level = 0.95)


#Bai tap 4
cholesterol <- read.table('cholesterol.txt', header = TRUE)
attach(cholesterol); names(cholesterol)
#kiem dinh phuong sai
var.test(Before, After)$p.value
alpha <- 0.05
v.equal <- ifelse(var.test(Before, After)$p.value < alpha, FALSE, TRUE); v.equal
t.test(Before, After, alternative = "greater", var.equal = v.equal, conf.level = 0.95, paired = TRUE)
test.leq.oneside <- function(x, y, mu0, alpha) {
  D = x - y;
  d.bar = mean(D)
  d.sd = sd(D)
  n <- length(D)
  t0 = (d.bar - mu0) * sqrt(n) / d.sd
  p.value = pt(t0, n - 1)
  cat('p-value = ', p.value, '\n')
  cat('Voi muc y nghia alpha = ', alpha)
  if (p.value <= alpha) {
    cat(' bac bo Ho\n');
  } else {
    cat(' khong du co so bac bo H0\n')
  }
}
test.leq.oneside(Before, After, 0, 0.05)
t.test(Before, After, alternative = "less", var.equal = v.equal, conf.level = TRUE, paired = TRUE)

test.geq.oneside <- function(x, y, mu0, alpha) {
  D = x - y;
  d.bar = mean(D)
  d.sd = sd(D)
  n <- length(D)
  t0 = (d.bar - mu0) * sqrt(n) / d.sd
  p.value = 1 - pt(t0, n - 1)
  cat('p-value = ', p.value, '\n')
  cat('Voi muc y nghia alpha = ', alpha)
  if (p.value <= alpha) {
    cat(' bac bo Ho\n');
  } else {
    cat(' khong du co so bac bo H0\n')
  }
}
test.geq.oneside(Before, After, 0, 0.05)
t.test(Before, After, alternative = "greater", var.equal = v.equal, conf.level = TRUE, paired = TRUE)