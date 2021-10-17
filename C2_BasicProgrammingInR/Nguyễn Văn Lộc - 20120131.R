#Nguyen Van Loc - 20120131 - 20CTT1TN2 - Chieu T4 - Ca 2
data <- read.csv("giamcan.csv", header = TRUE)
data
attach(data)
names(data)

n <- length(Nguoi)
n
below65before <- length(Truoc[Truoc < 65])
below65before
below65before / n
below65after <- length(Sau[Sau < 65])
below65after
below65after / n

between6575before <- length(Truoc[Truoc >= 65 & Truoc <= 75])
between6575before
between6575before / n
between6575after <- length(Sau[Sau >= 65 & Sau <= 75])
between6575after
between6575after / n