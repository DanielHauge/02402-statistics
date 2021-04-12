D <- read.table("E:\\repo\\02402-statistics-exercises\\Project2/bmi2_data.csv", header=TRUE, sep=";", as.is=TRUE)
D$logbmi <- log(D$bmi)
D_model <- subset(D, id <= 840)
D_test <- subset(D, id >= 841)
n_model <- nrow(D_model)
n_test <- nrow(D_test)
n <- n_model+n_test


first <- head(D, 1)
last <- tail(D,1)
rbind(first,last)

summaryOfVar <- function(navn, data) {
  return (c(navn, sum(!is.na(data)), mean(data, na.rm=TRUE), var(data, na.rm=TRUE), sd(data, na.rm=TRUE), quantile(data, .25), quantile(data, .50), quantile(data, .75)))
}

printSummary <- function(summary){
  format <- "%s: \n\t Antal Observationer: %s \n\t Middelværdi: %s \n\t Variation: %s \n\t Standard afvigelse: %s \n\t Nedre kvartil: %s \n\t Median: %s \n\t Øvre kvartil: %s\n"
  formatted <- do.call(sprintf, c(fmt = format, as.list(summary)))
  return (formatted)
}


cat(printSummary(summaryOfVar("fastfood", D$fastfood)))
cat(printSummary(summaryOfVar("age", D$age)))
cat(printSummary(summaryOfVar("logbmi", D$logbmi)))


par(mfrow=c(1,2))
plot(D$age, D$logbmi, ylab="logbmi", xlab="age")
plot(D$fastfood, D$logbmi, ylab="logbmi", xlab="fastfood")

par(mfrow=c(1,2))
D_bmi_under <- subset(D_model, bmi <= 18.5)
D_bmi_normal <- subset(D_model, bmi > 18.5 & bmi < 25)
D_bmi_over <- subset(D_model, bmi > 25)
boxplot(D_bmi_under$age, D_bmi_normal$age, D_bmi_over$age, names=c("under", "normal", "over"), xlab="BMI", ylab="Alder")
boxplot(D_bmi_under$fastfood, D_bmi_normal$fastfood, D_bmi_over$fastfood, names=c("under", "normal", "over"), xlab="BMI", ylab="Fastfood")

par(mfrow=c(1,2))
D_ff_0 <- subset(D_model, fastfood < 0.1)
D_ff_1 <- subset(D_model, fastfood < 1.1 & fastfood > 0)
D_ff_6 <- subset(D_model, fastfood < 6.1 & fastfood > 1)
D_ff_24 <- subset(D_model, fastfood < 24.1 & fastfood > 6)
D_ff_78 <- subset(D_model, fastfood < 78.3 & fastfood > 24)
D_ff_big <- subset(D_model, fastfood > 78.2)
boxplot(D_ff_0$age, D_ff_1$age, D_ff_6$age, D_ff_24$age, D_ff_78$age, D_ff_big$age , names=c("0", "1", "6", "24", "78", "+"), xlab="Fastfood", ylab="Alder")
boxplot(D_ff_0$bmi, D_ff_1$bmi, D_ff_6$bmi, D_ff_24$bmi, D_ff_78$bmi, D_ff_big$bmi , names=c("0", "1", "6", "24", "78", "+"), xlab="Fastfood", ylab="Bmi")

par(mfrow=c(1,3))
hist(D$logbmi, xlab="logbmi", ylab="Tæthed",prob=TRUE, main = "Histogram over log bmi tæthed")
hist(D$fastfood, xlab="Fastfood", ylab="Tæthed",prob=TRUE, main = "Histogram over Fastfood tæthed")
hist(D$age, xlab="Alder", ylab="Tæthed",prob=TRUE, main = "Histogram over alders tæthed")

fit <- lm(logbmi ~ age + fastfood, data=D_model)
summary(fit)

par(mfrow=c(1,2))
plot(fit$fitted.values, fit$residuals, xlab = "Fittede værdier", ylab = "Residual")
qqnorm(fit$residuals, ylab = "Residualer", xlab = "Z-scores", main = "")
qqline(fit$residuals)

library(MESS)
wallyplot(fit$residuals, FUN=qqnorm , main="") # FUN=qqnorm.wally || residualplot

e_b <- fit$coefficients[2]
e_t <- qt(0.975, df=fit$df.residual)
e_v <- sqrt(diag(vcov(fit)))[2]

conf.inv <- e_b + c(-1,1)*e_t*e_v

confint(fit, level=0.95)

f_t <- (0.0023743602-0.001)/0.0003889714
f_p <- 2*(1-pt(f_t, df=fit$df.residual))
#c(f_t,f_p)

c(cor(D_model$bmi, D_model$fastfood),cor(D_model$bmi, D_model$age),cor(D_model$fastfood, D_model$age))

confint(fit)

summary(fit)

pred <- predict(fit, newdata = D_test, interval = "prediction")
cbind(pred)

conf <- predict(fit, newdata = D_test, interval = "confidence")
cbind(conf)

cat(sprintf("Prædiktion: [%s , %s]\n", exp(2.927972), exp(3.546015)))
cat(sprintf("Confidence: [%s , %s]", exp(3.225973 ), exp(3.248014)))