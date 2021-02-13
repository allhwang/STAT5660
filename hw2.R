#HW 2
#Problem 1
1-pnorm(1.7677)   # P-value

#Problem 2
table3.5 <- read.table("table3.5.txt",header = T)
table3.5$zminus5 <- table3.5$zi - 5   # zi - theta_0 which is 5
table3.5$abs.zminus5 <- abs(table3.5$zminus5)
table3.5$vi2 <-  ifelse(table3.5$zminus5 > 0, 1, 0) 
table3.5$ri <- rank(table3.5$abs.zminus5)
tp <- sum(table3.5$vi2 * table3.5$ri)
tp   # T plus
n <- nrow(table3.5)
et <- n * (n + 1) / 4
vt <- n * (n + 1) * (2 * n + 1) / 24
ts <- (tp  - et)/sqrt(vt)
ts   # T star

# Lecture version
#pnorm(tp, et  , sqrt(vt),lower.tail=F)
pnorm(ts, 0 , 1, lower.tail=F)  # P-value 

wilcox.test(table3.5$yi, table3.5$xi, paired=TRUE, alternative = "greater", mu = 5
            , exact = FALSE, correct = FALSE) #large sample approximation 
# a little different p value. It is because of ties.  (3.13) on page 42

# Textbook version (3.13) on page 42
print(foo <- tabulate(round( table3.5$ri )))
vt <- vt - sum(foo * (foo - 1) * (foo + 1)) / 48

pnorm(tp, et, sqrt(vt), lower.tail=F)

# Problem 3
table3.2 <- read.table("table3.2.txt", header = T)
ci <- wilcox.test(table3.2$Private - table3.2$Government, conf.int=T,
                  conf.level = 0.90,exact = FALSE)  #exact = FALSE for large sample approximation
ci$conf.int   # Confidence Interval