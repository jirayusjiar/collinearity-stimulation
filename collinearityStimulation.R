### Stimulated data (Collinearity)

## Set working directory
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)

## Import library
library(Hmisc)
library(car)
library(MASS)

## Init variables
set.seed(1)

## Generate Data
x = data.frame(x1=rnorm(20))
x$x2<- rnorm(20,mean=x$x1,sd=.1)
x$y<- rnorm(20,mean=3+x$x1+x$x2)
#x$y<- (x$y-min(x$y))/(max(x$y)-min(x$y))
plot(x)

## Varclus
vc <- varclus(~.,
              data=x,
              similarity = "spearman",
              trans="abs")
plot(vc)

## Build prediction model

model <- glm(y~x1+x2,
             data = x,
             family = binomial())
model <- lm(y~x1+x2,
            data = x)
model.ridge <- lm.ridge(y~x1+x2,
                        data = x,
                        lambda=seq(0,1,0.001))
matplot(model.ridge$lambda,t(model.ridge$coef),type="l",xlab=expression(lambda),
        ylab=expression(hat(beta)))
abline(h=0,lwd=2)
select(model.ridge)


model2 <- glm(y~x2+x1,
              data = x,
              family = binomial())
model2 <- lm(y~x2+x1,
             data = x)
model2.ridge <- lm.ridge(y~x2+x1,
                        data = x,
                        lambda=seq(0,0.1,0.001))

## VIF
model.vif <- rms::vif(model)
model.ridge.vif <- rms::vif(model.ridge)
model2.vif <- rms::vif(model2)
model2.ridge.vif <- rms::vif(model2.ridge)


## Anova
# Model1
# Likelyhood ratio chisquare
lrAnova <- Anova(model, test.statistic = "LR")
lrChiSquare <- lrAnova[, 1]
lrChiSquareRelative <- lrChiSquare / sum(lrChiSquare)

# F-test
lrAnova2 <- Anova(model, test.statistic = "F")
lrF <-
  lrAnova2$F
lrFRelative <- lrF / sum(lrF)

# Deviance
lrAnova3 <- anova(model)
lrDeviance <- lrAnova3$Deviance
lrDevianceRelative <- lrDeviance / sum(lrDeviance)

# Model2
# Likelyhood ratio chisquare
.lrAnova <- Anova(model2, test.statistic = "LR")
.lrChiSquare <- .lrAnova[, 1]
.lrChiSquareRelative <- .lrChiSquare / sum(.lrChiSquare)

# F-test
.lrAnova2 <- Anova(model2, test.statistic = "F")
.lrF <-
  .lrAnova2$F
.lrFRelative <- .lrF / sum(.lrF)

# Deviance
.lrAnova3 <- anova(model2)
.lrDeviance <- .lrAnova3$Deviance
.lrDevianceRelative <- .lrDeviance / sum(.lrDeviance)

summary(model)
summary(model2)
model.vif
model2.vif
lrAnova
.lrAnova
lrAnova2
.lrAnova2
lrAnova3
.lrAnova3

## EOF
