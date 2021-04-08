# Indlæs 'bmi2_data.csv' filen med data
D <- read.table("bmi2_data.csv", header = TRUE, sep = ";")


# Tilføj log-BMI til datasættet
D$logbmi <- log(D$bmi)


# Deldatasæt med de første 840 observationer (til model)
D_model <- subset(D, id <= 840)

# Deldatasæt med de sidste 7 observationer (til validering)
D_test <- subset(D, id >= 841)


# Estimer multipel lineær regressionsmodel
fit <- lm(logbmi ~ age + fastfood, data = D_model)

# Vis estimerede parametre mm.
summary(fit)


# Plots til modelkontrol

# Observationer mod fittede værdier
plot(fit$fitted.values, D_model$logbmi, xlab = "Fittede værdier",     
       ylab = "log(BMI)")

# Residualer mod hver af de forklarende variable
plot(D_model$FORKLARENDE_VARIABEL, fit$residuals, 
        xlab = "INDSÆT TEKST", ylab = "Residualer")

# Residualer mod fittede værdier
plot(fit$fitted.values, fit$residuals, xlab = "Fittede værdier", 
     ylab = "Residualer")

# Normal QQ-plot af residualerne
qqnorm(fit$residuals, ylab = "Residualer", xlab = "Z-scores", 
       main = "")
qqline(fit$residuals)


# Konfidensintervaller for modellens koefficienter
confint(fit, level = 0.95)


# Prædiktioner og 95% prædiktionsintervaller
pred <- predict(SLUTMODEL, newdata = D_test, interval = "prediction", 
              level = 0.95)

# Observerede værdier sammen med prædiktioner
cbind(id = D_test$id, logbmi = D_test$logbmi, pred)

