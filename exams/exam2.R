#Bai kiem tra so 3
#Lop: 20CTT1TN2
#Ho ten: Nguyen Van Loc
#MSSV: 20120131

#Bai 1
profit <- read.csv("Profit-th05.csv", header = TRUE)
attach(profit); names(profit)
#Cau a
#Ta di kiem dinh gia thuyet
#H0: mu1 = mu2
#H1: mu1 != mu2 
#de tim khoang tin cay 95% cho su sai khac
t.test(Dist.1, Dist.3, alternative ="two.sided", var.equal = FALSE, conf.level = 0.95)
#Ket qua khi chay ham t.test o tren nhu sau:
#Welch Two Sample t-test

#data:  Dist.1 and Dist.3
#t = 2.8299, df = 388.07, p-value = 0.004898
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  7.457951 41.407851
#sample estimates:
#  mean of x mean of y 
#525.3214  500.8885 
#Vay khoang tin cay 95% cho su sai khac ve doanh so ban hang trung binh cua hai cua hang nay la
#[7.457951, 41.407851]
#Cau b
#Gia thuyet:
#H0: p1 <= p2
#H1: p1 > p2
y1 <- length(Dist.1[Dist.1 > 600]); y1
y2 <- length(Dist.3[Dist.3 > 600]); y2
n1 <- length(Dist.1); n1
n2 <- length(Dist.3); n2
y <- c(y1, y2)
n <- c(n1, n2)
prop.test(y, n, alternative = "greater", conf.level = 0.95, correct = TRUE)
#Ket qua khi chay ham prop.test o tren nhu sau:

#2-sample test for equality of proportions with continuity correction

#data:  y out of n
#X-squared = 0.94011, df = 1, p-value = 0.1661
#alternative hypothesis: greater
#95 percent confidence interval:
#  -0.02775669  1.00000000
#sample estimates:
# prop 1 prop 2 
#0.240  0.195 
#p-value = 0.1661
#Do alpha = 0.05 < 0.1661 = p-value nen ta khong du co so de bac bo H0
#Vay voi do tin cay 95%, ta khong the ket luan ti le cac ngay co doanh so cao tai quan 1 lon hon quan 3
#Cau c
prop.test.leq <- function(x, y, alpha) {
  nx <- length(x)
  yx <- length(x[x > 600])
  ny <- length(y)
  yy <- length(y[y > 600])
  
  px.hat = yx / nx;
  py.hat = yy / ny;
  
  p.hat = (yx + yy) / (nx + ny)
  
  z0 = (px.hat - py.hat) / sqrt(p.hat * (1 - p.hat) * (1 / nx + 1 / ny))
  
  p.value = pnorm(z0)
  cat('p-value = ', p.value, '\n')
  
  cat('Voi muc y nghia alpha = ', alpha)
  
  if (p.value <= alpha) {
    cat(' bac bo H0\n')
  } else {
    cat(' khong du co so de bac bo H0\n')
  }
}
prop.test.leq(Dist.1, Dist.3, 0.05)
#Ket qua:
#p-value =  0.8623171 
#Voi muc y nghia alpha =  0.05 khong du co so de bac bo H0

#Bai 2
#Cau a
#F = f(ft)
#Noi cach khac
#x: ft
#y: F
x <- c(600, 1000, 1250, 1600, 1800, 2100, 2500, 2900)
y <- c(56, 54, 56, 50, 47, 49, 47, 45)
plot(x, y)
abline(lm(y ~ x))
#Cau b
lm(y ~ x)
#Ket qua chay ham lm(y ~ x) nhu sau:
#Call:
#  lm(formula = y ~ x)
#
#Coefficients:
# (Intercept)            x  
#59.290662    -0.005115 
#Duong hoi quy uoc luong la:
#y.hat = 59.290662 - 0.005115x
#Dua vao duong thang hoi quy, ta duoc:
#Khi x tang len 1m thi y giam 0.005115 do F
#Vay, khi x tang len 1000m thi y giam 5.115 do F
#1 do C ung voi 1.8 do F nen 5.115 do F ung voi 3.41 do C
#Vay kinh nghiem tren de bai la khong dung