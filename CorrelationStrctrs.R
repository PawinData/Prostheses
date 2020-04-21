
load("prostheses.RData")

DATA
library(lattice)

DATA$Month.f <- factor(DATA$FU.Months, levels = c(0,3,12,24,60))
DATA$Type.f <- factor(DATA$Type, levels = c(1,2)) 


##Correlation Structures 
## Fit model with different cor struct 

model1 <- gls(nMTPM ~ Month.f*Type,
              data = DATA,
              correlation = corSymm(form = ~ 1 | ID),
              weights = varIdent(form = ~ 1 | FU.Months),
              na.action = na.exclude, method = "REML")
summary(model1) 

cov.mat <- getVarCov(model1, individual = 3) ##Check ind argument why 3? 
cov.mat
cov2cor(cov.mat)

##correlationstructs homoscedastic errors 
model2 <- gls(nMTPM ~ Month.f*Type,
              data = DATA,
              correlation = corCompSymm(form = ~ FU.Months | ID),
              na.action = na.exclude, method = "REML")

model3 <- gls(nMTPM ~ Month.f*Type,
              data = DATA,
              correlation = corAR1(form = ~ FU.Months| ID),
              na.action = na.exclude, method = "REML")

model4 <- gls(nMTPM ~ Month.f*Type,
              data = DATA,
              correlation = corLin(form = ~ FU.Months | ID),
              na.action = na.exclude, method = "REML")

####Gives error Error in gls(nMTPM ~ Month.f * Type, data = DATA, correlation = corLin(form = ~FU.Months |  : 
#### false convergence (8)

model5 <- gls(nMTPM ~ Month.f*Type,
              data = DATA,
              correlation = corGaus(form = ~ FU.Months | ID),
              na.action = na.exclude, method = "REML")

model6 <- gls(nMTPM ~ Month.f*Type,
              data = DATA,
              correlation = corExp(form = ~ 1 | ID),
              na.action = na.exclude, method = "REML")

#Compare 
anova(model2, model3, model5, model6) ##include model4 

#Model6 gives lowest c AIC and BIC 

##correlation structures + heteroscedastic errors

model7 <- gls(nMTPM ~ Month.f*Type,
              data = DATA,
              correlation = corCompSymm(form = ~ FU.Months | ID),
              weights =  varExp(form = ~ FU.Months),
              na.action = na.exclude, method = "REML")


model8 <- gls(nMTPM ~ Month.f*Type,
              data = DATA,
              correlation = corAR1(form = ~ FU.Months | ID),
              weights =  varExp(form = ~ FU.Months),
              na.action = na.exclude, method = "REML")


model9 <- gls(nMTPM ~ Month.f*Type,
              data = DATA,
              correlation = corLin(form = ~ FU.Months | ID),
              weights =  varExp(form = ~ FU.Months),
              na.action = na.exclude, method = "REML")


model10 <- gls(nMTPM ~ Month.f*Type,
               data = DATA,
               correlation = corGaus(form = ~ FU.Months | ID),
               weights =  varExp(form = ~ FU.Months),
               na.action = na.exclude, method = "REML")


model11 <- gls(nMTPM ~ Month.f*Type,
               data = DATA,
               correlation = corExp(form = ~ FU.Months | ID),
               weights =  varExp(form = ~ FU.Months),
               na.action = na.exclude, method = "REML")

#Compare Models with heteroscedastic errors

anova(model7, model8, model9, model10, model11)

##BIC and AIC lowest for model 7 

##Can we compare with LRT?


