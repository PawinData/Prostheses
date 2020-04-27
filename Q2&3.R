#1.Consider an elaborate model for the mean response. From the EDA, we consider 
# a structure below for now. And we will choose between Unstructured model  and (Semi-) Parametric model. 

model1 <- gls(nMTPM ~  Age*Sex+Month.f*Type.f+Sex*BMI+Sex*Month.f,
              data = DATA,
              method = "REML")


model2 <- gls(nMTPM ~  Age+Sex+FU.Months*Type+BMI,
              correlation = corSymm(form = ~ 1 | ID),
              weights = varIdent(form = ~ 1 | FU.Months),
              data = DATA,
              method = "REML")



summary(model1)
summary(model2)
model1 <- update(model1, method = "ML")
model2 <- update(model2, method = "ML")
anova(model1,model2)
#model1 is better than model2, we prefer Unstructured model  over (Semi-) Parametric model
#Add explanation for 
###################################################################

#2.Select a correlation structure
model3 <- gls(nMTPM ~ Age*Sex+Month.f*Type.f+Sex*BMI+Sex*Month.f,
              data = DATA,
              correlation = corSymm(form = ~ 1 | ID),
              weights = varIdent(form = ~ 1 | FU.Months),
              na.action = na.exclude, method = "REML")
summary(model3)


cov.mat <- getVarCov(model3, individual = 3) ##Check ind argument why 3? 
cov.mat
cov2cor(cov.mat) #We observe the  correlations decrease with time, so we try other correlation structures.

model4 <- gls(nMTPM ~ Age*Sex+Month.f*Type.f+Sex*BMI+Sex*Month.f,
              data = DATA,
              correlation = corCompSymm(form = ~ FU.Months | ID),
              na.action = na.exclude, method = "REML")

model5 <- gls(nMTPM ~ Age*Sex+Month.f*Type.f+Sex*BMI+Sex*Month.f,
              data = DATA,
              correlation = corAR1(form = ~ FU.Months| ID),
              na.action = na.exclude, method = "REML")

model6 <- gls(nMTPM ~ Age*Sex+Month.f*Type.f+Sex*BMI+Sex*Month.f,
              data = DATA,
              correlation = corLin(form = ~ FU.Months | ID),
              na.action = na.exclude, method = "REML")

model7 <- gls(nMTPM ~ Age*Sex+Month.f*Type.f+Sex*BMI+Sex*Month.f,
              data = DATA,
              correlation = corGaus(form = ~ FU.Months | ID),
              na.action = na.exclude, method = "REML")

model8 <- gls(nMTPM ~ Age*Sex+Month.f*Type.f+Sex*BMI+Sex*Month.f,
              data = DATA,
              correlation = corExp(form = ~ FU.Months | ID),
              na.action = na.exclude, method = "REML")

#Compare 
anova(model4,model5, model6, model7, model8) 

#Model4 gives lowest c AIC and BIC (model with Compound Symmetry Structure)


################################
##correlation structures + heteroscedastic errors

model9 <- gls(nMTPM ~ Age*Sex+Month.f*Type.f+Sex*BMI+Sex*Month.f,
              data = DATA,
              correlation = corCompSymm(form = ~ FU.Months | ID),
              weights =  varExp(form = ~ FU.Months),
              na.action = na.exclude, method = "REML")


model10 <- gls(nMTPM ~ Age*Sex+Month.f*Type.f+Sex*BMI+Sex*Month.f,
              data = DATA,
              correlation = corAR1(form = ~ FU.Months | ID),
              weights =  varExp(form = ~ FU.Months),
              na.action = na.exclude, method = "REML")


model11 <- gls(nMTPM ~ Age*Sex+Month.f*Type.f+Sex*BMI+Sex*Month.f,
              data = DATA,
              correlation = corLin(form = ~ FU.Months | ID),
              weights =  varExp(form = ~ FU.Months),
              na.action = na.exclude, method = "REML")


model12 <- gls(nMTPM ~ Age*Sex+Month.f*Type.f+Sex*BMI+Sex*Month.f,
               data = DATA,
               correlation = corGaus(form = ~ FU.Months | ID),
               weights =  varExp(form = ~ FU.Months),
               na.action = na.exclude, method = "REML")


model13 <- gls(nMTPM ~ Age*Sex+Month.f*Type.f+Sex*BMI+Sex*Month.f,
               data = DATA,
               correlation = corExp(form = ~ FU.Months | ID),
               weights =  varExp(form = ~ FU.Months),
               na.action = na.exclude, method = "REML")

#Compare Models with heteroscedastic errors

anova(model9, model10, model11, model12, model13)

##BIC and AIC lowest for model 9 (Compound Symmetry Structure)

##We can compare model 4 and model 9 
anova(model9, model4)
##model 9 gives the lowest AIC & BIC so we proceed with model 9 


#######################################################
#3.Now let us simplify the mean model by excluding non-significant terms.

summary(model9)

#exclude term Age
model14 <- update(model9, .~.-Age)
summary(model14)
anova(model14)

#exclude interaction between Sex and Age as the p-value shows the interaction is not significant (.844)

model15 <- gls(nMTPM ~ Month.f*Type.f+Sex*BMI+Sex*Month.f,
               data = DATA,
               correlation = corExp(form = ~ FU.Months | ID),
               weights =  corCompSymm(form = ~ FU.Months),
               na.action = na.exclude, method = "REML")

summary(model15)
anova(model15)

#remove interaction between Sex and BMI
model16 <- gls(nMTPM ~ Month.f*Type.f+Sex*Month.f,
               data = DATA,
               correlation = corCompSymm(form = ~ FU.Months | ID),
               weights =  varExp(form = ~ FU.Months),
               na.action = na.exclude, method = "REML")

summary(model16)
anova(model16)

#remove Sex as results show it is not significant 
model17 <- gls(nMTPM ~ Month.f*Type.f+Sex*Month.f-Sex,
               data = DATA,
               correlation = corCompSymm(form = ~ FU.Months | ID),
               weights =  varExp(form = ~ FU.Months),
               na.action = na.exclude, method = "REML")


summary(model17)
anova(model17)

#Consider the transformation log MTPM 
DATA$lnMTPM <- log(DATA$nMTPM + 1)

model18 <- gls(lnMTPM ~ Month.f*Type.f+Sex*Month.f-Sex,
               data = DATA,
               correlation = corCompSymm(form = ~ FU.Months | ID),
               weights =  varExp(form = ~ FU.Months),
               na.action = na.exclude, method = "REML")
summary(model18)
anova(model18)

# Regression coefficient interpretation: 
betas <- coef(model17) #Parameter estimates
V <- vcov(model17) #var cov matrix 
#contrast matrix ?


############################################################
#residual plots
plot(model17, resid(., type = "p") ~ fitted(.),type = c("p", "smooth"), lwd = 3)

plot(model17, resid(., type="p") ~ fitted(.)|Type,type = c("p", "smooth"), lwd = 3)

plot(model17, resid(., type="n") ~ fitted(.),type = c("p", "smooth"), lwd = 3)

plot(model17, resid(., type="n") ~ fitted(.)|Type,type = c("p", "smooth"), lwd = 3)

#the variance is not constant


################ Q3 

# Consider individuals as random 
# Linear mixed effects model 
model.1 <- lme(lnMTPM ~ Age*Sex+Month.f*Type.f+Sex*BMI+Sex*Month.f, 
               random = ~1|ID,
               data = DATA)
summary(model.1)
anova(model.1)

# Exclude Age 
model.2 <- update(model.1, .~.-Age)
summary(model.2)
anova(model.2)

getVarCov(model.2)

#Between subject variation 
bs <- getVarCov(model.2, type = "random.effects")  [1] #check 

#Within subject variation 
ws <- getVarCov(model.2, type ="conditional")[[1]][1,1] #check

# Marginal cov matrix 
getVarCov(model.2, type = "marginal", ind = 2)

#Intraclass correlation coefficient of reliability
reliab.coeficcient <- bs/(bs + ws)
reliab.coeficcient


