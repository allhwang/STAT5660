# Problem 5 on Page 134
table4.4 <- read.table("table4.4.txt", header = T)
alpha = 0.05
m <- length(table4.4$Olympics)
n <- length(table4.4$Karate)

u <- 0
for (i in table4.4$Olympics) {
  u <- u + length(which(table4.4$Karate > i)) + 
    length(which(table4.4$Karate == i))*0.5
}
u
w <- u + n*(n+1)/2
w_alpha <- 1+qwilcox(alpha, m, n, lower.tail=F) + n*(n+1)/2
w; w_alpha
pwilcox(u-1, m,n, lower.tail = F)     #p-value
wilcox.test(table4.4$Karate, table4.4$Olympics, alternative = "g")

# Problem 10 on Page 134
table4.7 <- read.table("table4.7.txt", header = T)
x <- table4.7$Healthy_geese
y <- table4.7$Lead.poisoned_geese[-8]

u <- 0
for (i in x) {
  u <- u + length(which(y > i)) + length(which(y == i))*0.5
}
u

m <- length(x)
n <- length(y)
w <-  u + n*(n+1)/2
w_alpha <- 1+qwilcox(alpha, m, n, lower.tail=F) +n*(n+1)/2

w; w_alpha

pwilcox(u-1, m,n, lower.tail = F)     #p-value
wilcox.test(y, x, alternative = "g")
