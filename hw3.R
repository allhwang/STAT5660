#Problem 50 on page 75
#table3.1
table3.1 <- cbind(read.table("table3.1.txt", header = T),
                  read.table("table3.1_1.txt", header = T))[,-4] 
head(table3.1)
attach(table3.1)

#Exact test
n <- nrow(table3.1)   #n
B <- sum(vi)      #B
b_alpha <- 1 + qbinom(p=0.05, size=n, prob=1/2, lower.tail=F)   #b_alpha,0.5
b_alpha
# 1 + qbinom(p=0.1, size=9, prob=1/2, lower.tail=F)  when alpha = 0.1, b_alpha,0.5 = 7
# [1] 7
B; n - b_alpha
pbinom(B, n, 0.5)   #p-value

