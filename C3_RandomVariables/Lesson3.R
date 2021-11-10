#Vi du 1a
k <- 0:8
p <- function(k) choose(8, k)*0.3^k*0.7^(8 - k)
p(k)
sum(p(k))

#Vi du 1b
f <- function(x, mu = 0, sigma = 1) {
  1 / sqrt(2 * pi * sigma^2) * exp(-(x - mu)^2 / (2 * sigma^2))
}

#Tinh f(0)
f(0)

#Tinh tich phan tren toan mien xac dinh cua f
integrate(function(x) f(x, 0,1), -Inf, Inf)

#Vi du 2a
plot(k, p(k), type= "h", xlab = "k", ylab = "P(X = k)", main = "Ham xac suat cua bien ngau nhien X")

#Vi du 2b
curve(f(x,0,1), from = -3, to = 3, xlab = "x", ylab = "f(x)", main = "Ham mat do cua bien ngau nhien X")

#Ham phan phoi xac suat cua bien ngau nhien roi rac
F<-function(k) sum(p(0:k))
F<-Vectorize(F)
F(4)
F(0:3)

#Ham phan phoi xac suat cua bien ngau nhien lien tuc
F2<-function(a, mu = 0, sigma = 1) {
  integrate(function(x) f(x, mu, sigma), lower = -Inf, upper = a) $value
}
F2 <-Vectorize(F2)
F2(1.96)
F2(0:4)

#Vi du 4a
plot(stepfun(k, c(0, F(k))), ylab = "FX(x)", main = "cdf of X")

#Vi du 4b
curve(F2(x), from = -3, to = 3, ylab = "FX(x)", main = "cdf of X")

#Vi du 5a
K = k[F(k) >= 0.25]
K

#VI du 5b
#Tim nghiem cua phuong trinh F(x) - xp = 0
uniroot(function(x) F2(x) - 0.975, c(-3, 3)) $root

#Baitap
#Baitap1
fp<-function(p) 0.07*(p^(-0.93))

I<-function(a, b) {
  integrate(function(x) fp(x), lower = a, upper = b) $value
}

#Cau a
I(0, 0.2)
#Cau b
I(0, 1)

#Baitap2
x <- sample(1:5, 100, replace = TRUE, prob = c(0.1, 0.2, 0.4, 0.2, 0.1))
x

#Tao bang thong ke
table(x)

#Tinh co mau
n <- length(x)

#Lap bang tan so
table(x) / n

#Ve bieu do cot
hist(x, main = "Bieu do cot cua du lieu x")