#Mo phong 1.1: Cho X1, ..., Xn ~ N(mu, sigma). Khi do, X.bar ~ N(mu, sigma^2/n)

#Gan cac gia tri
n <- 10
mu <- 2
sigma <- 2

#Tao vector X = (X1, X2, ..., Xn) ~ N(mu, sigma). 
vecX <- function(n) rnorm(n, mu, sigma)

vecX(n)

#Tinh gia tri trung binh mau
meanX <- function() mean(vecX(n))

meanX()

#Lap lai qua trinh tren m lan
sampleMeanX <- function(m) replicate(m, meanX())

sampleMeanX(10)

#Ve bieu do tan so cho du lieu vua tao
hist(sampleMeanX(10000), freq = 0, breaks = 40)

#Ve ham mat do cua phan phoi N(mu, sigma^2/n)
curve(dnorm(x, mu, sigma/sqrt(n)), col = "blue", lty = 1, lwd = 2, add = T)

#Mo phong 1.2: Cho X1, ..., Xn ~ N(mu, sigma). Khi do, (n - 1)S^2/sigma^2 ~ chi^2(n - 1)

#Tinh gia tri bien S = (n - 1) * var(vecX(n)) / sigma^2 
S <- function() (n - 1) * var(vecX(n)) / sigma^2

#Lap lai qua trinh tren m lan
sampleS <- function(m) replicate(m, S())

sampleS(10)

#Ve bieu do tan so cho du lieu vua tao
hist(sampleS(10000), freq = 0, breaks = 40)

#Ve ham mat do cua pp chi^2(n - 1)
curve(dchisq(x, n - 1), col = "red", lty = 1, lwd = 2, add = T)


#Bai tap
#Bai tap 1

xvec <- function(n) rnorm(n, 0, 1)
x1 <- xvec(2)
x2 <- xvec(2)
x1
x2

y <- function() {
  x1 <- rnorm(1)
  x2 <- rnorm(1)
  y <- x1^2 + x2^2
}

MauY<- function(n) replicate(n, y())

draw <- function(m) {
  #Ve bieu do tan suat
  hist(MauY(m), freq = 0, breaks = 40)
  
  #Ve ham mat do phan phoi chi binh phuong voi 2 bac tu do
  curve(dchisq(x, 2), col = "blue", lty = 1, lwd = 2, add = T)
}

par(mfrow = c(1, 3))
draw(100)
draw(1000)
draw(10000)
