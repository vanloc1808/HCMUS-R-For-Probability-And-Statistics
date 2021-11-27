#Bai tap 1
x <- c(1, 2, 5, 7, -3, 0, 5, 1, 5, 6)
y <- c(2, 2, 0, -5, 7, 8, 11, 9, 3, 2)

#Cau a
x + y
x - y
x * y

#Cau b
z = x[x %% 2 == 0]
t = y[y %% 2 == 1]
z
t

#Cau c
x[x > 0]
y[y > 0]

#Cau d
mean(x)
sd(x)
#Tinh sai so chuan
sd(x)/sqrt(length(x))

mean(y)
sd(y)
#Tinh sai so chuan
sd(y)/sqrt(length(y))

#Cau e
max(x)
min(x)

max(y)
min(y)

#Cau f
sort(x)

#Cau g
#Luu bien x va y
save(x, file = "variable_x.rda")
save(y, file = "variable_y.rda")

sort(y, decreasing = TRUE)

#Cau g

#Bai 2
#Doc du lieu tu file "data01.csv" vao R va gan vao bien data01
data1 <- read.csv("data01.csv", header = TRUE)
attach(data1); names(data1)
data1
#a. Trung binh
mean(FPSA)
mean(TPSA)
# Phuong sai
var(FPSA)
var(TPSA)
# Trung vi
median(FPSA)
median(TPSA)
#b. Ve bieu do dang duong, boxplot
par(mfrow=c(2,2))
plot(FPSA, type = 'l')
boxplot(FPSA)
plot(TPSA, type = 'l')
boxplot(TPSA)
#c. Tach nhung gia tri cua FPSA co K = 0 va K = 1
#subset(du lieu, dieu kien tach)
fpsa0 <- subset(FPSA, K == 0)
fpsa0
fpsa1 <- subset(FPSA, K == 1)
fpsa1
#d. Doc du lieu tu file data02.csv vao R va gan vao bien data2
data2 <- read.csv("data02.csv", header = TRUE)
names(data2)
attach(data2)
#Merge 2 frames theo bien K
dat <- data.frame(data1[ , 1 : 3], data2)
dat
#e. Tao bien tPSA theo yeu cau
tPSA  <- Age
tPSA[Age <= 60] <- 0
tPSA[Age > 60 & Age <= 70] <- 1
tPSA[Age > 70] <- 2
#Tao bang thong ke cho tPSA
tab <- table(tPSA)
tab

#Cau 3
#a. Nhap cac so lieu
sv <- 1:10
ques1 <- c(3, 3, 3, 4, 3, 4, 3, 4, 3, 4)
ques2 <- c(5, 3, 5, 5, 2, 2, 5, 5, 4, 2)
ques3 <- c(1, 3, 1, 1, 1, 3, 1, 1, 1, 1)
#Tao bang diem
diem <- data.frame(sv, ques1, ques2, ques3)
#b. Tao bang ket qua rieng cho cau hoi 1 va cau hoi 2
tab1 <- table(ques1); tab1
tab2 <- table(ques2); tab2
tab3 <- table(ques3); tab3

#c. Ve bieu do bar cho 3 cau hoi
par(mfrow=c(1,3))
barplot(tab1)
barplot(tab2)
barplot(tab3)

#d. Ve barplot dang nam ngang
par(mfrow=c(2, 1))
barplot(tab2, horiz = T)
barplot(tab3, horiz = T)

#Bai 4
par(mfrow=c(1,2))
#a. Tao 100 mau pp nhi thuc n = 60, p = 0.4
x <- rbinom(100, 60, 0.4); x
hist(x, main = "Mo phong phan phoi nhi thuc")
#b. Tao 100 mau pp Poisson voi lambda = 4
y <- rpois(100, 4); y
hist(y, main = "Mo phong phan phoi Poisson")
#c. Tao 100 mau pp chuan voi mu = 50 va sigma = 4
z <- rnorm(100, 50, 4)
plot(density(z), main = "Ham mat do phan phoi chuan")
#d. Tao 100 mau pp mu voi lambda = 1/25
t <- rexp(100, 1/25)
plot(density(t), main = "Ham mat do phan phoi mu")

#Bai 5
#a. Doc du lieu
diesel.engine <- read.table("diesel_engine.dat", header = T)
diesel.engine
diesel.time <- read.table("diesel_time.csv", header = T, sep = ",")
diesel.time
attach(diesel.engine)
attach(diesel.time)
#b. Liet ke ten cac bien
names(diesel.engine)
names(diesel.time)
#c. Dem so gia tri bi khuyet
length(diesel.engine[diesel.engine == 'NA'])
#Xac dinh so bien khuyet trong bien speed va thay doi so lieu
speed[is.na(speed) == T] = 1500
load[is.na(load) == T] = 20
diesel.engine[, 2] <- speed
diesel.engine[, 3] <- load

#Cau e
diesel <- data.frame(diesel.engine, diesel.time)

#Cau f
run[delay < 1.000]

#Cau g
length(run[timing == 30])

#Cau h
par(mfrow = c(1, 3))
boxplot(speed)
boxplot(timing)
boxplot(delay)

#Cau i
plot(timing, speed)
plot(temp, press)

#Cau j
load = factor(load)

#Cau k
new.delay <- cut(delay, breaks = 4); new.delay
tab <- table(new.delay); tab
barplot(tab)

#Cau l
cutpoints <- c(0.283, 0.7, 0.95, 1.2, 1.56)
new.delay1 <- cut(delay, breaks = cutpoints); new.delay1
tab1 <- table(new.delay1); tab1
barplot(tab1)