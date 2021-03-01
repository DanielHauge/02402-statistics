
###########################################################################
## Sæt working directory

## I RStudio kan man nemt sætte working directory med menuen 
## "Session -> Set Working Directory -> To Source File Location" 
## Bemærk: i R bruges kun "/" til separering i stier 
## (altså ingen backslash).
setwd("Erstat her med stien til den mappe, hvor projektfilerne er gemt.")


###########################################################################
## Indlæs data

## Indlæs data fra bmi1_data.csv
D <- read.table("bmi1_data.csv", header=TRUE, sep=";", as.is=TRUE)


###########################################################################
## Simpel opsummering af data

## Dimensionen af D (antallet af rækker og søjler)
dim(D)
## Søjle-/variabelnavne
names(D)
## De første rækker/observationer
head(D)
## De sidste rækker/observationer
tail(D)
## Udvalgte opsummerende størrelser
summary(D)
## En anden type opsummering af datasættet
str(D)


###########################################################################
## Beregn BMI

## Beregn BMI og tilføj som ny variabel i D
D$bmi <- D$weight/(D$height/100)^2


###########################################################################
## Histogram (empirisk tæthed)

## Histogram der beskriver den empiriske tæthed for BMI
## (histogram for BMI normaliseret så arealet er lig 1)
hist(D$bmi, xlab="BMI", prob=TRUE)


###########################################################################
## Deldatasæt vha. 'subset'

## Opdel i to deldatasæt (hhv. kvinder og mænd)
Dfemale <- subset(D, gender == 0)
Dmale <- subset(D, gender == 1)


###########################################################################
## Density histogrammer for kvinder hhv. mænd

## Density histogrammer der beskriver den empiriske
## tæthed for BMI for hhv. kvinder og mænd
hist(Dfemale$bmi, xlab="BMI (kvinder)", prob=TRUE)
hist(Dmale$bmi, xlab="BMI (mænd)", prob=TRUE)


###########################################################################
## Boxplot opdelt efter køn

## Boxplot af BMI opdelt efter køn
boxplot(Dfemale$bmi, Dmale$bmi, names=c("Kvinder", "Mænd"), 
        xlab="Køn", ylab="BMI")


###########################################################################
## Opsummerende størrelser for BMI

## Antal observationer i alt
## (medregner ej eventuelle manglende værdier)
sum(!is.na(D$bmi))
## Stikprøvegennemsnit (ej kønsopdelt)
mean(D$bmi, na.rm=TRUE)
## Stikprøvevarians (ej kønsopdelt)
var(D$bmi, na.rm=TRUE)
## osv. 
##
## Argumentet 'na.rm=TRUE' sørger for at størrelsen
## udregnes selvom der eventuelt er manglende værdier


###########################################################################
## qq-plot til modelkontrol

## Ny variabel 'logbmi' med log-transformeret BMI
D$logbmi <- log(D$bmi)
## qq-plot for log-transformeret BMI
qqnorm(D$logbmi)
qqline(D$logbmi)


###########################################################################
## T-test for en enkelt stikprøve

## T-test for en enkelt stikprøve foretaget på log-transformeret BMI
t.test(D$logbmi, mu=log(25))


###########################################################################
## Konfidensinterval (KI) for middelværdi og median

## Udtag data kun for kvinder
Dfemale <- subset(D, gender == 0)
## KI for middelværdien af log-BMI for kvinder
KI <- t.test(Dfemale$logbmi, conf.level=0.95)$conf.int
KI
## Transformer tilbage for at få KI for median BMI for kvinder
exp(KI)


###########################################################################
## Welch t-test for sammenligning af to stikprøver

## Sammenligning af logBMI for kvinder og mænd
t.test(D$logbmi[D$gender == 0], D$logbmi[D$gender == 1])


###########################################################################
## Beregning af korrelation

## Beregning af korrelation mellem udvalgte variable
cor(D[,c("weight","fastfood","bmi")], use="pairwise.complete.obs")

  
###########################################################################
## Delmængder i R

## Ekstra bemærkning om måder at udtage delmænger i R
##
## En logisk (logical) vektor med sandt (TRUE) eller falsk (FALSE) for 
## hver værdi i en kolonne i D - f.eks: Find alle kvinder i datasættet
D$gender == 0
## Vektoren kan bruges til at udtage data for kvinderne
D[D$gender == 0, ]
## Alternativt kan man bruge funktionen 'subset'
subset(D, gender == 0)
## Mere komplekse logiske udtryk kan laves, f.eks.: 
## Find alle kvinder under 55 kg
subset(D, gender == 0 & weight < 55)


###########################################################################
## Flere R-tips

## Lav en for-løkke med beregning af et par opsummerende størrelser
## og gem resultatet i en ny data.frame
Tbl <- data.frame()
for(i in 0:1){
  Tbl[i+1, "mean"] <- mean(D$bmi[D$gender == i])
  Tbl[i+1, "var"] <- var(D$bmi[D$gender == i])
}
row.names(Tbl) <- c("Kvinder","Mænd")
## Se hvad der er i Tbl
Tbl

## I R er der endnu mere kortfattede måder sådanne udregninger kan 
## udføres. For eksempel
aggregate(D$bmi, by=list(D$gender), function(x){ 
  c(mean=mean(x), var=var(x)) 
})
## Se flere smarte funktioner med: ?apply, ?aggregate og ?lapply
## og for ekstremt effektiv databehandling se f.eks. pakkerne: dplyr,
## tidyr, reshape2 og ggplot2.

## LaTeX tips:
##
## R-pakken "xtable" kan generere LaTeX tabeller og skrive dem direkte 
## ind i en fil, som derefter kan inkluderes i et .tex dokument.
## 
## R-pakken "knitr" kan anvendes meget elegant til at lave et .tex 
## dokument der inkluderer R-koden direkte i dokumentet. Dette 
## dokument og bogen er lavet med knitr.
