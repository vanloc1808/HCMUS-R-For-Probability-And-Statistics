#Bai tap thuc hanh so 2
#Lop: 20CTT1TN2
#Ho va ten: Nguyen Van Loc
#MSSV: 20120131
# a/
#Chon cap gia thuyet:
#H0: p = 0.6
#H1: p > 0.6
#Giai thich: khi ta bac bo H0 thi ta co the thuc hien viec thay doi thang diem,
#con khi ta khong bac bo H0 thi ta khong nen thuc hien viec thay doi thang diem
#Vay khi ta thua nhan hoac bac bo H0 thi ta se co cau tra loi cho bai toan.


# b/
load("data04.rda")
y <- length(survey[survey == 1]); y
n <- length(survey); n
alpha <- 0.05
prop.test(y, n, p = 0.6, alternative = "greater", conf.level = 0.95)
#Ket qua khi ta chay ham prop.test nhu sau
#1-sample proportions test with continuity correction
#
#data:  y out of n, null probability 0.6
#X-squared = 3.763, df = 1, p-value = 0.0262
#alternative hypothesis: true p is greater than 0.6
#95 percent confidence interval:
#  0.6168459 1.0000000
#sample estimates:
#  p 
#0.7125

#Nhan xet: do p-vakue = 0.0262 < alpha = 0.05 nen ta bac bo H0 voi muc y nghia alpha = 0.05
#Ket luan: voi do tin cay 95% (muc y nghia 5%), Hoi dong Khoa hoc Truong DH Khoa hoc Tu nhien 
#co the thay doi thang diem tu thang 10 sang thang 4.

# c/
proptest.xeq <- function(f, n, p0, alpha) {
  p.hat <- f / n;
  z0 = (p.hat - p0) * sqrt(n) / sqrt(p0 * (1 - p0))
  p.value = 1 - pnorm(z0)
  if (p.value < alpha) {
    cat('o muc y nghia alpha = ', alpha, ', bac bo H0 voi p-value = ', p.value, '\n')
  } else {
    cat('o muc y nghia alpha = ', alpha, ', khong du co so de bac bo H0 voi p-value = ', p.value, '\n')
  }
}
proptest.xeq(length(survey[survey == 1]), length(survey), 0.6, 0.05)
#Em hong biet tai sao co sai so giua hai cai p.value nua a ;_; em tinh tay cung khong ra p.value cua ham prop.test
  