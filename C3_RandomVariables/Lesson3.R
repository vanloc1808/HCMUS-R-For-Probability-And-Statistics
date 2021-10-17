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