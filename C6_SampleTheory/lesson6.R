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

