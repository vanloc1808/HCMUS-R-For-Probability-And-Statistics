#1.3 Y nghia khoang tin cay 95%
m = 50
n = 30
p = 0.5
alpha = 0.05
#Tung 30 dong xu can doi 50 lan

phat = rbinom(m, n, p) / n #Tinh ty le mau

epsilon = qnorm(1 - alpha / 2) * sqrt(phat * (1 - phat) / n) #dung sai

matplot(rbind(phat - epsilon, phat + epsilon), rbind(1:m, 1:m), type = "l", lty = 1, xlab = "Vi tri cac khoang tin cay", ylab = "50 khoan tin cay")
#Ve 50 khoang tin cay

abline(v = p) #Ve duong thang p =0.5

#Bai tap 1
#Tao mau 35 gia tri cua bien X(10, 5^2)
n <- 35
x <- rnorm(n, 10, 5); x

#Tim khoang tin cay 95%, alpha = 5%
x.bar = mean(x); x.bar
alpha <- 0.05
s <- 5

epsilon = qnorm(1 - alpha / 2) * s /sqrt(n); epsilon

cat('KTC', 100 * (1 - alpha), '% cho ky vong mu la: \n')
cat('[', x.bar - epsilon, ', ', x.bar + epsilon, ']')

#Bai tap 2
#a. Doc du lieu tu file data31.csv
data31 <- read.csv(("data31.csv"), header = T)
attach(data31); names(data31)
data31

#b. Viet ham ci.mean(x, alpha)
ci.mean <- function(x, alpha) {
  n <- length(x) #Do dai cua x
  
  x.bar = mean(x) #Trung binh cua x
  
  s = sd(x) #Do lech chuan mau cua x
  
  t = qt(1 - alpha / 2, n - 1)
  
  ep = t * s/sqrt(n)
  
  cat('KTC', 100 * (1-alpha), '% cua ky vong mu la:\n')
  cat('[', x.bar - ep, ", ", x.bar + ep, "]\n")
}

ci.mean(profit, 0.05)
ci.mean(profit, 0.01)

#Bai tap 3
#a. Doc du lieu tu file data32.csv
data32 <- read.csv("data32.csv", header = T)
attach(data32); names(data32)
data32
ci.mean(KHTN, 0.05)

#b. Viet ham ci.prop(f, n, alpha)
ci.prop <- function(f, n, alpha) {
  p.hat = f/n;
  eps = qnorm(1 - alpha/2) * sqrt(p.hat * (1 - p.hat) / n)
  cat('KTC', 100 * (1-alpha), '% cua ti le p la:\n')
  cat('[', p.hat - eps, ", ", p.hat + eps, "]\n")
}
ci.prop(length(KHTN[KHTN > 5]), length(KHTN), 0.1)
ci.prop(length(KHTN[KHTN > 5]), length(KHTN), 0.05)
ci.prop(length(KHTN[KHTN > 5]), length(KHTN), 0.01)

#Bai tap 4
a <- seq(1.2, 2.0, by = 0.2)
b <- seq(1.4, 2.2, by = 0.2)
m <- (a + b) / 2
N <- c(6, 34, 31, 42, 12)
x <- rep(m, N); x
ci.mean(x, 0.05)
