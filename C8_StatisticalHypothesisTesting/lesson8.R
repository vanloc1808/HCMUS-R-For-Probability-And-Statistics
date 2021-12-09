#1. Kiem dinh gia thuyet cho ky vong
#Vi du bien heights
load ('heights.rda')
summary(heights)
hist(heights)
t.test(heights, mu = 160, conf.level = 0.95)
qt(0.975, 124)

#Bai tap 1
#Dua du lieu vao R
data1 <- read.csv("profit.csv", header = T)
names(data1)
pro <- data1$profit
#Cau a: Ve do thi histogram
hist(pro)
high.profit <- pro[pro > 65]
#Tinh trung binh mau, do lech chuan mau
pro.bar <- mean(high.profit)
pro.s <- sd(high.profit)
pro.length <- length(high.profit)
alpha <- 0.01
epsilon = qnorm(1 - alpha/2) * pro.s / sqrt(pro.length)
mu.lower = pro.bar - epsilon
mu.upper = pro.bar + epsilon
cat('Khoang tin cay ', 100 * (1 - alpha), '% cho mu la:\n')
cat('[', mu.lower, ', ', mu.upper, ']')
#Cau c
t.test(pro, alternative = "greater", mu = 60, conf.level =  0.99)
qt(0.995, 124)

#Bai tap 2
xi <- c(5, 6, 7, 8, 9, 10)
ni <- c(5, 10, 15, 20, 12, 8)
x <- rep(xi, ni); x
#Ve bieu do stem & leaf
stem(x)
#Cau b
#Viet  ham test.leq.oneside
test.leq.oneside <- function(x, mu0, alpha) {
  x.bar = mean(x)
  n = length(x)
  s = sd(x)
  t0 = (x.bar - mu0) * sqrt(n) / s
  p.value = pt(t0, n - 1)
  cat('Voi muc y nghia alpha = ', alpha, '\n')
  if (p.value <= alpha) {
    cat('Bac bo H0 voi p-value = ', p.value)
  } else {
    cat('Chua du co so de bac bo H0 voi p-value = ', p.value)
  }
  cat('\n')
}
#Ap dung
test.leq.oneside(x, 8, 0.05)

#2 Kiem dinh cho ti le
n <- 800
y <- 448
prop.test(y, n, p = 0.5, alternative = "greater", conf.level = 0.99)

#Bai tap 6
times <- read.csv("times.csv", header = T)
attach(times); names(times)
#a. Su dung prop.test
n = length(KHTN)
y = length(KHTN[KHTN > 5])
n; y
prop.test(y, n, p = 0.5, alternative = "two.sided", conf.level = 0.95)
