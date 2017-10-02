data <- read.table("kellydat.txt", header = TRUE)
library(survminer)
library(survival)
library(plyr)

hist(data[data$fail == 1,]$mins, breaks = 200)
hist(data$mins)
data$mins <- data$nctdel*60

#comment about how male/female distribution is same (roughly 50/50) within races
count(data, c("sn1", "sn2", "sn3", "all4"))
count(data, c("black", "hisp"))

# Symptoms Survival Curve

fit <- survfit(Surv(mins, fail) ~ sn1 + sn2 + sn3 + all4, data = data)
ggsurvplot(fit, data = data, risk.table = FALSE) + labs(y = "Probability Unscanned")
?ggsurvplot
# Black-Hispanic-Other Survival Curve

fit1 <- survfit(Surv(mins, fail) ~ black + hisp, data = data)
ggsurvplot(fit1, data = data, risk.table = TRUE) + labs(y = "Probability Unscanned")

# Male-Female Survival Curve

fit2 <- survfit(Surv(mins, fail) ~ male, data = data)
ggsurvplot(fit2, data = data, risk.table = TRUE) + labs(y = "Probability Unscanned")






