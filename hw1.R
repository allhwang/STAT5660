# Hw 1

prob.table <- cbind(0:10,round(pbinom(0:10, 10, .2, lower.tail = T),4),
                    round(pbinom(0:10, 10, .2, lower.tail = F),4))
colnames(prob.table) <- c("x", "P(X<=x)", "P(X>x)")
prob.table