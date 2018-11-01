stud <- read.csv("students_performance.csv", header = T)
View(stud)
av <- aov(stud$Math~stud$SEB)
summary(av)
summmary(av)
boxplot(stud$Math~stud$SEB)
hist(stud$Math)
av1 <- aov(stud$Math~stud$Race)
summary(av1)
boxplot(stud$Math~stud$Race)

av2 <- aov(stud$Math~stud$Prog)
summary(av2)
boxplot(stud$Math~stud$Prog)

# Multiple anova test
av3 <- aov(stud$Math~stud$Prog+stud$Gender)
summary(av3)
# Gender doesn't tell us anything

av4 <- aov(stud$Math~stud$Prog+stud$SEB)
summary(av4)

# Chi-Square goodness of fit
table(stud$SEB)
chisq.test(table(stud$SEB), p = c(0.25,0.5,0.25))

chisq.test(table(stud$Gender), p = c(0.5,0.5))

# Test of independence
chisq.test(stud$Gender,stud$School)

chisq.test(stud$Race,stud$SEB)

# To take of warning which is beacuse there are less than 5 observations in some celss
table(SEB = stud$SEB, Race = stud$Race)

#We use fisher test to correct it
fisher.test(stud$SEB,stud$Race)

# Interpret these results
chisq.test(stud$Race,stud$SEB)

chisq.test(stud$SEB,stud$School)

chisq.test(stud$School,stud$Race)

t.test(stud$Write~stud$School)

# Assuming that the  population variance of write data for both the schools is same.
t.test(stud$Write~stud$School, var.equal = T)

