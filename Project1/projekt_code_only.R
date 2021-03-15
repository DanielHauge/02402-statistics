D <- read.table("E:\\repo\\02402-statistics-exercises\\Project1/bmi1_data.csv", header=TRUE, sep=";", as.is=TRUE)
D$bmi <- D$weight/(D$height/100)^2
D$logbmi <- log(D$bmi)
Dfemale <- subset(D, gender == 0)
Dmale <- subset(D, gender == 1)
n <- nrow(D)

first <- head(D, 1)
last <- tail(D,1)
rbind(first,last)

hist(D$bmi, xlab="BMI", ylab="Tæthed",prob=TRUE, main = sprintf("Histogram for BMI. \n x-bar = %#.2f | x-tilde = %#.2f | ∆: %#.2f " , mean(D$bmi), median(D$bmi), mean(D$bmi)- median(D$bmi)) )

par(mfrow=c(1,2))
hist(Dfemale$bmi, xlab="BMI", ylab="Tæthed",prob=TRUE, main = sprintf("Kvinder \n %#.2f | %#.2f | ∆: %#.2f" , mean(Dfemale$bmi), median(Dfemale$bmi), mean(Dfemale$bmi)-median(Dfemale$bmi)))
hist(Dmale$bmi,xlab="BMI", ylab="Tæthed",prob=TRUE, main = sprintf("Mænd \n %#.2f | %#.2f | ∆: %#.2f" , mean(Dmale$bmi), median(Dmale$bmi), mean(Dmale$bmi)- median(Dmale$bmi)) )

boxplot(Dfemale$bmi, Dmale$bmi, names=c("Kvinder", "Mænd"), xlab="Køn", ylab="BMI")

w <- D$bmi
alleSummary <- c(sum(!is.na(w)), mean(w, na.rm=TRUE), var(w, na.rm=TRUE), sd(w, na.rm=TRUE), quantile(w, .25), quantile(w, .50), quantile(w, .75))
w <- Dfemale$bmi
kvinderSummary <- c(sum(!is.na(w)), mean(w, na.rm=TRUE), var(w, na.rm=TRUE), sd(w, na.rm=TRUE), quantile(w, .25), quantile(w, .50), quantile(w, .75))
w <- Dmale$bmi
mændSummary <- c(sum(!is.na(w)), mean(w, na.rm=TRUE), var(w, na.rm=TRUE), sd(w, na.rm=TRUE), quantile(w, .25), quantile(w, .50), quantile(w, .75))

alpha <- mean(D$logbmi)
beta <- sd(D$logbmi)

par(mfrow=c(1,2))
qqnorm(D$logbmi, ylab = "Værdi", xlab = "Teoretiske Kvantiler", main = "Log BMI")
qqline(D$logbmi)

qqnorm(D$bmi, ylab = "Værdi", xlab = "Teoretiske Kvantiler", main = "BMI")
qqline(D$bmi)

m <- mean(D$logbmi)
s <- sd(D$logbmi)
t95 <- qt(0.975, (n-1))
konf <- m+(qt(0.975, (n-1))*(s/sqrt(length(D)))*c(-1,1))

tobs <- (alpha-log(25))/(beta/sqrt(n))
p <- 2 * (1-pt(abs(tobs), df=n-1))

ttestresults <- t.test(D$logbmi, mu=log(25))
# ttestresults

alphak <- mean(Dfemale$logbmi)
betak <- sd(Dfemale$logbmi)

par(mfrow=c(1,2))
qqnorm(Dfemale$logbmi, ylab = "Værdi", xlab = "Teoretiske Kvantiler", main = "Kvinder: Log BMI")
qqline(Dfemale$logbmi)

qqnorm(Dfemale$bmi, ylab = "Værdi", xlab = "Teoretiske Kvantiler", main = "Kvinder: BMI")
qqline(Dfemale$bmi)

alpham <- mean(Dmale$logbmi)
betam <- sd(Dmale$logbmi)

par(mfrow=c(1,2))
qqnorm(Dmale$logbmi, ylab = "Værdi", xlab = "Teoretiske Kvantiler", main = "Mænd: Log BMI")
qqline(Dmale$logbmi)

qqnorm(Dmale$bmi, ylab = "Værdi", xlab = "Teoretiske Kvantiler", main = "Mænd: BMI")
qqline(Dmale$bmi)

#t.test(Dmale$logbmi)
#t.test(Dfemale$logbmi)

kmedkinf <- c(exp(3.136525), exp(3.211669))
mmedkinf <- c(exp(3.231677), exp(3.289498))

xt1 <- mean(Dmale$logbmi)
xt2 <- mean(Dfemale$logbmi)
s1 <- sd(Dmale$logbmi)
s2 <- sd(Dfemale$logbmi)
n1 <- nrow(Dmale)
n2 <- nrow(Dfemale)

tkobs <- (xt1-xt2)/sqrt(((s1^2)/n1)+(s2^2)/n2)
vtop <- (((s1^2)/n1)+((s2^2)/n2))^2
vbot <- (((s1^2/n1)^2)/(n1-1))+(((s2^2/n2)^2)/(n2-1))
v <- vtop/vbot

pk <- 2*(1 - pt(tkobs, df=v))

ttestresults <- t.test(Dmale$logbmi,Dfemale$logbmi)
#ttestresults

mbmi <- mean(D$bmi)
sbmi <- sd(D$bmi)
mweight <- mean(D$weight)
sweight <- sd(D$weight)
covariance <- 1/(n-1)*sum((D$bmi-mbmi)*(D$weight-mweight))
correlation <- covariance/(sbmi*sweight)
fl <- c(c(first$bmi, first$weight),c(last$bmi, last$weight))

ffbmi <- cor(D$fastfood, D$bmi)
ffweight <- cor(D$weight, D$fastfood)

par(mfrow=c(1,3))
plot(D$bmi, D$weight, xlab="bmi", ylab="vægt")
plot(D$bmi, D$fastfood, xlab="bmi", ylab="fastfood")
plot(D$weight, D$fastfood, xlab="vægt", ylab="fastfood")