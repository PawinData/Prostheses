# Q.2 
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

#2.Select a correlation structure (same mean model but different covariance structures,
# the following models try compound symmetry, continuos ARI, linear Gaussian and constant variance in time 

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
## Extension of the correlation structures by assumption of heteroscedastic errors 

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
##model 9 gives the lowest AIC & BIC so we proceed with model 9, *add more explanation for model chosen 


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

#Should we consider this transformation? I run some of the graphs from the mean part and it doesn't seem to change a lot...

# Regression coefficient interpretation (from model 17) 

# (Intercept): mean migration measured by the MTMP at baseline (first day after surgery) for type 1 protheses group 
# Month.f3: change in mean MTMP for 3 months postoperation for type 1 protheses group   
# Month.f12: change in mean MTMP for 1 year postoperation for type 1 protheses group 
# Month.f24: change in mean MTMP for 2 years postoperation for type 1 protheses group 
# Month.f60: change in mean MTMP for 5 years postoperation for type 1 protheses group 
# Type.f2: difference in mean MTMP between type 1 group and type 2 group at baseline
# Month.f3:Type.f2: difference in rate of change in mean MTMP between type 1 and type 2, 3 months postoperation 
# Month.f12:Type.f2: difference in rate of change in mean MTMP between type 1 and type 2, 1 year postoperation 
# Month.f24:Type.f2: difference in rate of change in mean MTMP between type 1 and type 2, 2 years postoperation 
# Month.f60:Type.f2: difference in rate of change in mean MTMP between type 1 and type 2, 5 years postoperation 

# ¿? unsure about the interpretation of the following, is it difference in rate chanfe in mean MTMP for males for type 1? 

# Month.f0:SexMale    
# Month.f3:SexMale 
# Month.f12:SexMale 
# Month.f24:SexMale
# Month.f60:SexMale

# Intervals for 95% confidence level 

intervals(model17)


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


