#1. Doi net ve R

#2. Tai va cai dat R

#3. Cau truc lenh chung trong R
#3.1 Cau truc lenh trong R
x <- c()
example(sample)
x <- sample(5)
?sample
help(sample)
example(sample)

#3.2 Mot so phep toan so sanh hay logic trong R
x == 3
x != 3
x > 3
x < 3
x & 3
x | 3
!x
xor(x, 3)

#3.3 Cac ham toan hoc thuong dung trong R
x <- 10
log(x)
log10(x)
log(x, 3)
exp(x)
sqrt(x)
factorial(x)
choose(x, 4)
floor(x)
ceiling(x)
trunc(x)
round(x, digit = 4)
signif(x, digit = 5)
sin(x)
cos(x)
tan(x)
abs(x)
y <- 3
x%/%y
x%%y

#Mot so lenh lien quan
a <- c(2, 9 , 4, 1, 7, 10, 5, 8, 6, 11, 3)
a
length(a)
a[4]
a[-4]
a[1:4]
a[c(1, 4, 6)]
a[a > 3]
a[a < -2 | a > 2]

#Mot so ham ve vector
max(a)
min(a)
mean(a)
median(a)
range(a)
var(a)
sort(a)
order(a)
quantile(a)
cumsum(a)
cumprod(a)

#3.4 Cach dat ten trong R

#4. Buoc dau lam quen voi R
#4.1 Thiet lap thu muc lam viec
list.files()
rm(list = ls())

#4.2 Nhap du lieu
#4.2.1 Nhap du lieu truc tiep
#a. Dung ham c()
age <- c(45, 47, 54, 50, 43, 53)
bloodpress <- c(14, 13, 15, 12, 11, 13)
patient <- data.frame(age, bloodpress)
patient

#b. Dung lenh edit(data.frame())
patient <- edit(data.frame())
patient

#4.2.2 Nhap du lieu tu file
#a. Nhap tu file *.txt: Dung lenh read.table()
getwd()
setwd("F:\\GithubRepository\\R-Practice\\Introduction to R")
stat <- read.table("Solieu.txt", header = TRUE)
stat
names(stat)
save(stat, file = "stat.rda")

#b. Nhap tu file excel *.csv: Dung lenh read.csv()
data01 <- read.csv("data01.csv", header = TRUE)
data01
names(data01)
save(data01, file = "data01.rda")

#4.2.3 Tao mot day so bang cac ham seq, rep, gl
#a. Dung ham seq
a1 <- seq(6)
a1
a2 <- seq(9, 20)
a2
a3 <- seq(18, 8)
a3
a4 <- seq(1, 100, by = pi)
a4
a5 <- seq(length = 8, 1, 100)
a5

#b. Dung ham rep
b1 <- rep(3, 4)
b1
b2 <- rep((2:8), 3)
b2
b3 <- rep(seq(2, 10, by = pi), 5)
b3
b4 <- rep ( 1 : 4 , c ( 2 , 3 , 4 , 5 ) )
b4
b5 <- rep(1:10, each = 3)
b5

#c. Dung ham gl
c1 <- gl(2,6)
c1
c2 <- gl(2, 5, length = 40)
c2
c3 <- gl(3, 4, length = 50, label = c("C", "B", "A"))
c3