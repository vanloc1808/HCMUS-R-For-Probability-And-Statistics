#Bai 10: Hoi quy va tuong quan
#1. Mo hinh hoi quy tuyen tinh don
#Vi du 1. Nhip tim toi da
x <- c(18, 23, 25, 35, 65, 54, 34, 56, 72, 19, 23, 42, 18, 39, 37); #length(x);
y <- c(202, 186, 187, 180, 156, 169, 174, 172, 153, 199, 193, 174, 198, 183, 178); #length(y)
plot(x, y) #ve do thi
abline(lm(y ~ x)) #ve duong hoi quy
lm(y ~ x) #cac gia tri co ban cua phan tich hoi quy
result = lm(y ~ x)
summary(result)
res = resid(result)
summary(res)

#Phan tich thang du
#Vi du 2. 
par(mfrow = c(1, 2)) #chuan bi ve hai do thi tren mot cua so
plot(result$fitted.values, result$residuals, xlab = 'Fitted values', ylab = 'Residuals', main = 'Residuals vs Fitted') #do thi thang du theo gia tri hoi quy
abline(h = 0, lty = 3)
qqnorm(res) #do thi normal Q - Q
qqline(res) #duong thagn ly thuyet tren do thi Q - Q

#Bai tap
#Bai tap 1
#Cau a
y <- c(300, 250, 400, 550, 317, 389, 425, 289, 389, 559); length(y)
x <- c(3, 3, 4, 5, 4, 3, 6, 3, 4, 5); length(x)
plot(x, y)
abline(lm(y ~ x))
#Cau b
lm(y ~ x)