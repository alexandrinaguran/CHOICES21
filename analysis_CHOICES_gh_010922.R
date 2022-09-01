## Choices study - children analysis 

#Set Working Directory
setwd('/home/gurana93/mnt/p/userdata/gurana93/shared/childstudy/data_Karla/logfiles')
#Load all libraries
# not all needed for this script, this is just a personal startup 
library(lme4)
library(ggthemes)
library(psych)
library(summarytools)
library(cowplot)
library(car)
library(tidyverse)
library(ggsignif)
library(ggplot2)
library(dplyr)
install.packages("effects")
library(effects)
library(summarytools)
install.packages("MuMIn")
library(MuMIn)
library(stats)
install.packages('mudata')
install.packages('rms')
library(rms)
install.packages('ResourceSelection')
library(ResourceSelection)
library(tidyverse)
library(dplyr) 
library(performance)

#citations
citation('lme4')
citation('BayesianFirstAid')


#ImportStudyData
File <- read.csv("final_data_choices_280322.csv", header = TRUE, sep = ";",  fileEncoding="latin1", dec = ",")
# this datafile can be made available upon reasonable request from the authors (see corresponding author in manuscript)


#######LATENCY ANALYSIS#########################################################
#make subsets for participants in latency analysis: all smaller 0.5 in lat excl. col
lat_all <- subset(File, latency_incl < 0.5) 
res_latency <- t.test(as.numeric(lat_all$ok_touch)~lat_all$Condition, paired = T, alternative = "less" )
TB_oktouch <- subset(lat_all, Condition == 0)
FB_oktouch <- subset(lat_all, Condition == 1)
mean(as.numeric(FB_oktouch$ok_touch))
mean(as.numeric(TB_oktouch$ok_touch))
res_latency


#of those, only those with correct trials in both conditions 
lat_correct <- subset(lat_all, choice_error < 1)

lat_correct <- lat_correct[c(1:6, 8:9, 11:12, 18:21, 25:26, 28:29, 31:32), ] #20:21
lat_correct
subs_TBcorr <- subset(lat_correct, Condition == 0)
subs_FBcorr <- subset(lat_correct, Condition == 1)


res_correct <- t.test(as.numeric(subs_TBcorr$ok_touch), as.numeric(subs_FBcorr$ok_touch), paired = TRUE)
res_correct

#result
#Paired t-test

#data:  as.numeric(subs_TBcorr$ok_touch) and as.numeric(subs_FBcorr$ok_touch)
#t = -0.45503, df = 8, p-value = 0.6612
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -2.797958  1.875736
#sample estimates:
#  mean of the differences 
#-0.4611111 

# now remove the participants that had only one correct 

# wrong and right trials latency
lat_error <- subset(lat_all, choice_error == 1)
lat_error
lat_error <- lat_error[-c(3, 4, 17), ] 
lat_right <- subset(lat_all, choice_error == 0)
lat_right <- lat_right[-c(20, 21, 23), ]
res_errors <- t.test(as.numeric(lat_error$ok_touch), as.numeric(lat_right$ok_touch), paired = FALSE, alternative = "less")
res_errors

ID <- as.factor(lat_all$ID)

model_nullLatency <-glmer(as.factor(ok_touch) ~ Choice + (1|ID), data = lat_all, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(model_nullLatency)


##lat_right_old <- lat_right
#results
#Welch Two Sample t-test

#data:  as.numeric(lat_error$ok_touch) and as.numeric(lat_right$ok_touch)
#t = -1.8136, df = 44.193, p-value = 0.07653
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -2.7184895  0.1430772
#sample estimates:
#  mean of x mean of y 
#4.476087  5.763793 

#####################################################END LATENCY################


#########BINOMIAL TESTS RESPONSE PATTERNS'######################################

#make subset for participants in main analysis: all smaller 0.5 (excl)
cho_all <- subset(File, excl < 0.5) # all subjects that we are taking anyhow
cho_excl <- subset(File, excl > 0.4) # subjects that were excluded
cho_TBonly <- subset(rbind(subset(cho_excl, TB_first == 1), subset(cho_all, Condition == 0)), Condition == 0) #bind together, excluded subjects where TB was first and all other subjects' TB response
cho_FBonly <- subset(rbind(subset(cho_excl, TB_first == 0), subset(cho_all, Condition == 1)), Condition == 1) # bind excl. subjects where FB was first and all others' FB response
#make subset for participants in first trial only: all smaller 1
#if TB first = 1 take TB, if TB first = 0, take FB row 
#cho_first <- subset(cho_all, TB_first == 1 & Condition == 0 | TB_first == 0 & Condition == 1)

#TBsubset <- subset(cho_all, Condition == 0) # i need to change this. I want to include the participants 
# whose first trial I can only use
#FBsubset <- subset(cho_all, Condition == 1)

TBchoiceBaited <-sum(cho_TBonly$Choice == 0, na.rm = T) 
TBchoiceCued   <-sum(cho_TBonly$Choice == 1, na.rm = T) 
FBchoiceBaited <-sum(cho_FBonly$Choice == 0, na.rm = T) 
FBchoiceCued   <-sum(cho_FBonly$Choice == 1, na.rm = T) 

TB = data.frame(TB= as.numeric(c(TBchoiceBaited, TBchoiceCued)))
TB_v0 <- binom.test(x = TB$TB, p = 0.5, alternative = "greater")
TB_v0
#Exact binomial test

#data:  TB$TB
#number of successes = 18, number of trials = 33, p-value = 0.3642
#alternative hypothesis: true probability of success is greater than 0.5
#95 percent confidence interval:
#  0.3894025 1.0000000
#sample estimates:
#  probability of success 
#0.5454545 

FB = data.frame(FB= as.numeric(c(FBchoiceBaited, FBchoiceCued)))
FB_v0 <- binom.test(x = FB$FB, p = 0.5, alternative = "greater")
FB_v0

#Exact binomial test

#data:  FB$FB
#number of successes = 23, number of trials = 35, p-value = 0.04477
#alternative hypothesis: true probability of success is greater than 0.5
#95 percent confidence interval:
#  0.5044808 1.0000000
#sample estimates:
#  probability of success 
#0.6571429 


# for plotting: 
FB_prop <- FB/sum(FB)
TB_prop <- TB/sum(TB)

#Exact binomial test

#data:  FB$FB
#number of successes = 23, number of trials = 35, p-value = 0.04477
#alternative hypothesis: true probability of success is greater than 0.5
#95 percent confidence interval:
#  0.5044808 1.0000000
#sample estimates:
#  probability of success 
#0.6571429 


### correlation

ageTOM <- File[!duplicated(File[c('ID')]), ]
ageTOM <- subset(ageTOM, excl <1)
agevals <- ageTOM$age
TOMvals <- ageTOM$TOM
cor_agextom <- cor.test(as.numeric(agevals), TOMvals, method = "pearson")
cor_agextom

# Result
#Pearson's product-moment correlation

#t = -0.61512, df = 34, p-value = 0.5426
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.4190042  0.2316084
#sample estimates:
#       cor 
#-0.1049101 


##### figure 
### ggplot is better
#data for ggplot: 
gplot_Data_prop <- data.frame(matrix(nrow = 4, ncol = 0))
gplot_Data_prop$Condition = c("FB", "FB", "TB", "TB");
gplot_Data_prop$Choice = c("baited", "cued", "baited", "cued");
gplot_Data_prop$props = data.frame(props = c(0.6571429, 0.3428571, 0.5454545, 0.4545455)) # these values were calculated from absolute counts/number of trials in condition, see above! 
#gplot_Data_prop$yvals = data.frame(yvals = c(23, 12, 18, 15))
ggplot(gplot_Data_prop, aes(x = Condition, y = yvals,  fill = Choice)) + 
  geom_bar(stat = 'identity', position = 'fill', aes(fill = Choice)) +
  scale_fill_manual(values = c("#DADAEB", "#9E9AC8"))

ggplot() + 
  theme(text = element_text(size = 20), axis.title = element_text(size = 22), legend.text = element_text(size = 15), legend.title = element_text(size = 18), 
        panel.background = element_rect(fill = "#ffffff"), panel.border = element_rect(fill = NA)) + 
  geom_bar(aes(y = unlist(props), 
               x = Condition, 
               fill = Choice, width = 0.7),
           data = gplot_Data_prop,
           stat = "identity") + 
  scale_fill_manual(values = c("#ff7A5C", "#56C4A3")) + 
  labs(y = "Proportions of Choices") + 
  geom_hline(aes(yintercept = 0.5), linetype = 2, color = 'grey')



#### GLMs ######################################################################

## setup 

BasicModel=data.frame(cho_all)
BasicModel

#RenamingVariables
Subject=BasicModel$ID
Age=BasicModel$age
Sex=BasicModel$sex
Condition=BasicModel$Condition
Choice=BasicModel$Choice
TOM=BasicModel$TOM

## renaming in data frame! not creaating new (Rina)
colnames(BasicModel) <- c("Subject", "Sex", "Age", "Condition", "Choice", "TOM", "excl", "ok_stand", "stand_touch", "ok_touch", "choice_error", "latency_incl", "TB_first")
head(BasicModel)


#Centering and factoring/numeric 
BasicModel$Age <- as.numeric(BasicModel$Age)
BasicModel$sex <- as.numeric(scale.default(BasicModel$Sex))
BasicModel$sex <- as.factor(BasicModel$sex)
BasicModel$Condition <- as.factor(BasicModel$Condition)
#Condition <- as.numeric(Condition) #?? 
BasicModel$Choice <- as.factor(BasicModel$Choice)
BasicModel$TOM <- as.numeric(BasicModel$TOM)
head(BasicModel)

#First: Model Assumptions
# no assumptions beyond independence of observations



# # # # # # FIXED EFFECTS MODELS  # # # # # # # # # # # # # # # # # # # # # # #
# stepwise inclusion of effects

## Nullmodel, Rina 
#Null = glmer(Choice ~ 1, data = BasicModel, family = binomial, control = glmerControl(optimizer = "bobyqa"))
NullM = glmer(Choice ~ (1|Subject), data = BasicModel, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(NullM)


#ModelZero, but with effect for model selection process "Nullmodel" is "ModelNoEffect at bottom"
FECond=glmer(Choice ~ Condition + (1|Subject), data=BasicModel, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(FECond)
anova(NullM, FECond) #ns difference between models

#print(ModelZero, corr=F)
#coef(ModelZero)
#r.squaredGLMM(ModelZero)
## compare, Rina 

FEall = glmer(Choice ~ Condition + Age + Sex + TOM + (1|Subject), data=BasicModel, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(FEall)
# no indication for Sex to predict Choices, p > 0.9
anova(NullM, FEall) # Null performs better

#leaving out sex
FE_CAT = glmer(Choice ~ Condition + Age + TOM + (1|Subject), data=BasicModel, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(FE_CAT)
anova(NullM, FE_CAT) # trend for CAT Model to outperform (condition, Age, TOM) AIC smaller :). Condition seems to contribute least


FE_AT = glmer(Choice ~  Age + TOM + (1|Subject), data=BasicModel, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(FE_AT)
anova(NullM, FE_AT) # FE of age and TOM outperform over null model 

# will even more simple models be preferred? 
FE_A = glmer(Choice  ~ Age + (1|Subject), data=BasicModel, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(FE_A)
anova(FE_A, FE_AT) # no difference. AT explains more variance (a little lower AIC) but the model is more complex which is penalized by the AIC

FE_T = glmer(Choice  ~ TOM + (1|Subject), data=BasicModel, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(FE_T)
anova(FE_T, FE_AT) # no difference. AT explains more variance (a little lower AIC) but the model is more complex which is penalized by the AIC

#summary of final FEs
summary(FE_AT)

#Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#Family: binomial  ( logit )
#Formula: Choice ~ Age + TOM + (1 | Subject)
#Data: BasicModel#
#Control: glmerControl(optimizer = "bobyqa")

#AIC      BIC   logLik deviance df.resid 
#88.6     97.3    -40.3     80.6       60 

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-1.2387 -0.8365 -0.5040  0.8140  2.2653 

#Random effects:
#  Groups  Name        Variance Std.Dev.
#Subject (Intercept) 0.2022   0.4496  
#Number of obs: 64, groups:  Subject, 32

#Fixed effects:
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)   4.6926     4.0882   1.148    0.251
#Age          -1.1021     0.7182  -1.534    0.125
#TOM           0.6995     0.4885   1.432    0.152

#Correlation of Fixed Effects:
#  (Intr) Age   
#Age -0.980       
#TOM -0.271  0.086

# # # # # # # INTERACTIONS # # # # # # # # # # # # # # # # # # # # # # # # # # #

#Cond x TOM

AT_CxT = glmer(Choice ~ Age + TOM + Condition:TOM + (1|Subject), data = BasicModel, family = binomial, control =glmerControl(optimizer = "bobyqa"))
summary(AT_CxT)
anova(FE_AT, AT_CxT)# ns different

#Cond x Age 
AT_CxA = glmer(Choice ~ Age + TOM + Condition:Age + (1|Subject), data = BasicModel, family = binomial, control =glmerControl(optimizer = "bobyqa"))
summary(AT_CxA)
anova(FE_AT, AT_CxA)# ns different

# TOMx Age
AT_AxT = glmer(Choice ~ Age + TOM + Age:TOM + (1|Subject), data = BasicModel, family = binomial, control =glmerControl(optimizer = "bobyqa"))
summary(AT_AxT)
anova(FE_AT, AT_AxT)# ns different, p = 0.145, nominally, the model has a slightly lower AIC 


# # # # # # # # with random slopes # # # # # # # # # # # # # # # # # # # # # # # 

AT_RSC = glmer( Choice ~ Age + TOM + (1+Condition|Subject), data=BasicModel, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(AT_RSC)
anova(AT_RSC, FE_AT) # random slopes model outperforms the non-random slopes model 

# # #  re-try for interaction in this model with RS. 

ATrsC_CxT = glmer(Choice ~ Age + TOM + Condition:TOM + (1+Condition|Subject), data=BasicModel, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(ATrsC_CxT)
anova(AT_RSC, ATrsC_CxT) # n.s

ATrsC_CxA = glmer( Choice ~ Age + TOM + Condition:Age + (1+Condition|Subject), data=BasicModel, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(ATrsC_CxA)
anova(AT_RSC, ATrsC_CxA) #n.s.

ATrsC_AxT = glmer( Choice ~ Age + TOM + Age:TOM + (1+Condition|Subject), data=BasicModel, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(ATrsC_AxT)
anova(AT_RSC, ATrsC_AxT) # very n.s.


## ## ## ## ## ## ## FINAL BEST MODEL!!!
summary(AT_RSC)
anova(NullM, AT_RSC)
#Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#Family: binomial  ( logit )
#Formula: Choice ~ Age + TOM + (1 + Condition | Subject)
#Data: BasicModel
#Control: glmerControl(optimizer = "bobyqa")

#AIC      BIC   logLik deviance df.resid 
#86.9     99.8    -37.4     74.9       58 

#Scaled residuals: 
#  Min       1Q   Median       3Q      Max 
#-1.32313 -0.26257 -0.01902  0.12009  2.57377 

#Random effects:
#  Groups  Name        Variance Std.Dev. Corr 
#Subject (Intercept) 736.9    27.15         
#Condition1  686.9    26.21    -1.00
#Number of obs: 64, groups:  Subject, 32

#Fixed effects:
#  Estimate Std. Error z value Pr(>|z|)  
#(Intercept)  18.4444    10.1086   1.825   0.0681 .
#Age          -3.7124     1.9372  -1.916   0.0553 .
#TOM           0.8043     0.6571   1.224   0.2209  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr) Age   
#Age -0.994       
#TOM  0.227 -0.324
#optimizer (bobyqa) convergence code: 0 (OK)
#boundary (singular) fit: see ?isSingular

# plot age effect 
ggeffects::ggpredict(AT_RSC, c("Age")) %>% 
  plot()
ggeffects::ggpredict(AT_RSC, c("TOM")) %>% 
  plot()


# to plot interactions 
plot_Age_int <- ggeffects::ggpredict(AT_RSC, c("Age"))
plot_TOM_int <- ggeffects::ggpredict(AT_RSC, c("TOM"))


#panel.background = element_rect(fill = "#ffffff"), panel.border = element_rect(fill = NA)) 



plot(plot_Age_int) + theme(text = element_text(size = 15), axis.title = element_text(size = 20), 
                           panel.background = element_rect(fill = "#ffffff"), panel.border = element_rect(fill = NA), 
                           panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ 
  coord_cartesian(ylim = c(0, 0.89)) + labs (y = "Probability to pick \ncued container")
plot(plot_TOM_int) + theme(text = element_text(size = 15), axis.title = element_text(size = 20), 
                           panel.background = element_rect(fill = "#ffffff"), panel.border = element_rect(fill = NA), 
                           panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  coord_cartesian(ylim = c(0, 0.89))
#################### SPEECH ANALYSIS ###########################################


File_speech <- read.csv("final_data_choices_280322.csv", header = TRUE, sep = ";",  fileEncoding="latin1", dec = ",")

speech_incl <- subset(File_speech, speech_incl < 0.5)
head(speech_incl)


SpeechData=data.frame(speech_incl)
SpeechData


SpeechData$age <- as.numeric(SpeechData$age)
#SpeechData$sex <- as.numeric(scale.default(SpeechData$Sex))
#SpeechData$sex <- as.factor(SpeechData$sex)
SpeechData$Condition <- as.factor(SpeechData$Condition)
#Condition <- as.numeric(Condition) #?? 
SpeechData$Choice <- as.factor(SpeechData$Choice)
SpeechData$TOM <- as.numeric(SpeechData$TOM)


# # # # # #  set up GLMs with speech as predictor, as done above.

SpNullM = glmer(Choice ~ (1|ID), data = SpeechData, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(SpNullM)


#ModelZero, but with effect for model selection process "Nullmodel" is "ModelNoEffect at bottom"
SpFECond=glmer(Choice ~ Condition + (1|ID), data=SpeechData, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(SpFECond)
anova(SpNullM, SpFECond) #ns difference between models

#print(ModelZero, corr=F)
#coef(ModelZero)
#r.squaredGLMM(ModelZero)
## compare, Rina 

SpFEall = glmer(Choice ~ Condition + age + sex + TOM + speech_raw + (1|ID), data=SpeechData, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(SpFEall)
# no indication for Sex to predict Choices, p > 0.9
anova(SpNullM, SpFEall) # Null performs better

# removing sex and condition 
SpFE_ATS = glmer(Choice ~ age + TOM + speech_raw + (1|ID), data=SpeechData, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(SpFE_ATS)
anova(SpNullM, SpFE_ATS) # almost better

# removing TOM (highest p, p = 0.38)
SpFE_AS = glmer(Choice ~ age + speech_raw + (1|ID), data=SpeechData, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(SpFE_AS)
anova(SpNullM, SpFE_AS) # performs better than null! 

# removing Speech (highest p, p = 0.18) 
SpFE_A = glmer(Choice ~ age + (1|ID), data=SpeechData, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(SpFE_A)
anova(SpFE_AS, SpFE_A) # n.s. difference, but lower AIC (extra explained variance by Speech is minimal)
anova(SpNullM, SpFE_A) # performs even better than null

# # # # # # # # # # # # # # # # # INTERACTIONS # # # # # # # # # # # # # # # # #

SpFE_AS_CxT = glmer(Choice ~ Condition:TOM + age + speech_raw + (1|ID), data=SpeechData, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(SpFE_AS_CxT)
anova(SpFE_AS, SpFE_AS_CxT) # nope 

SpFE_AS_CxS = glmer(Choice ~ Condition:speech_raw + age + speech_raw + (1|ID), data=SpeechData, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(SpFE_AS_CxS)
anova(SpFE_AS, SpFE_AS_CxS) # nope 

SpFE_AS_CxA = glmer(Choice ~ Condition:age + age + speech_raw + (1|ID), data=SpeechData, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(SpFE_AS_CxA)
anova(SpFE_AS, SpFE_AS_CxA) # nope 

SpFE_AS_SxA = glmer(Choice ~ speech_raw:age + age + speech_raw + (1|ID), data=SpeechData, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(SpFE_AS_SxA) # model did not converge
anova(SpFE_AS, SpFE_AS_SxA) # 

SpFE_A_SxA = glmer(Choice ~ speech_raw:age + age + (1|ID), data=SpeechData, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(SpFE_A_SxA) #
anova(SpFE_A, SpFE_A_SxA)


SpFE_AS_SxT = glmer(Choice ~ speech_raw:TOM + age + speech_raw + (1|ID), data=SpeechData, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(SpFE_AS_SxT) # nope!! this is important, no link between speech and TOM in the subtests?! (but abs of evi is not evi of abs) 
anova(SpFE_AS, SpFE_AS_SxT) # nope

# # # # # # # # # # # RANDOM SLOPE # # # # # # # # # # # # # # # # # # # # # # #

SpFE_AS_SxA_rsC = glmer(Choice ~ Speech:age + age + Speech + (1+Condition|ID), data=SpeechData, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(SpFE_AS_SxA_rsC) # model did not converge! 
anova(SpFE_AS_SxA, SpFE_AS_SxA_rsC) # 

SpFE_A_rsC = glmer(Choice ~ age + (1+Condition|ID), data=SpeechData, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(SpFE_A_rsC) # model did not converge! 
anova(SpFE_A, SpFE_A_rsC) # 
anova(SpNullM, SpFE_A_rsC)


####################### final best model for speech ############################
summary(SpFE_A)
#Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#Family: binomial  ( logit )
#Formula: Choice ~ age + (1 + Condition | ID)
#Data: SpeechData
#Control: glmerControl(optimizer = "bobyqa")

#AIC      BIC   logLik deviance df.resid 
#61.7     70.6    -25.8     51.7       39 

#Scaled residuals: 
#  Min       1Q   Median       3Q      Max 
#-1.78562 -0.14793 -0.00459  0.18827  2.05772 

#Random effects:
#  Groups Name        Variance Std.Dev. Corr 
#ID     (Intercept) 547.2    23.39         
#Condition1  502.2    22.41    -1.00
#Number of obs: 44, groups:  ID, 22

#Fixed effects:
#  Estimate Std. Error z value Pr(>|z|)  
#(Intercept)   22.043     11.018   2.001   0.0454 *
#  age           -4.113      2.054  -2.003   0.0452 *
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr)
#age -0.999
#optimizer (bobyqa) convergence code: 0 (OK)
#boundary (singular) fit: see ?isSingular


# plot speech x age 
ggeffects::ggpredict(SpFE_AS_SxA, c("Speech")) %>% 
  plot()
ggeffects::ggpredict(SpFE_AS_SxA, c("age")) %>% 
  plot()
ggeffects::ggpredict(SpFE_AS_SxA, c("age", "Speech")) %>% 
  plot()


################################################################################
# # # # # # # # # # # # # 1st Trial analysis # # # # # # # # # # # # # # # # # #
## from above, to help: 
gplot_Data_prop <- data.frame(matrix(nrow = 4, ncol = 0))
gplot_Data_prop$Condition = c("FB", "FB", "TB", "TB");
gplot_Data_prop$Choice = c("baited", "cued", "baited", "cued");
gplot_Data_prop$props = data.frame(props = c(0.6571429, 0.3428571, 0.5454545, 0.4545455)) # these values were calculated from absolute counts/number of trials in condition, see above! 
#gplot_Data_prop$yvals = data.frame(yvals = c(23, 12, 18, 15))
ggplot(gplot_Data_prop, aes(x = Condition, y = yvals,  fill = Choice)) + 
  geom_bar(stat = 'identity', position = 'fill', aes(fill = Choice)) +
  scale_fill_manual(values = c("#DADAEB", "#9E9AC8"))

ggplot() + 
  theme(text = element_text(size = 20), axis.title = element_text(size = 22), legend.text = element_text(size = 15), legend.title = element_text(size = 18), 
        panel.background = element_rect(fill = "#ffffff"), panel.border = element_rect(fill = NA)) + 
  geom_bar(aes(y = unlist(props), 
               x = Condition, 
               fill = Choice, width = 0.7),
           data = gplot_Data_prop,
           stat = "identity") + 
  scale_fill_manual(values = c("#ff7A5C", "#56C4A3")) + 
  labs(y = "Proportions of Choices") + 
  geom_hline(aes(yintercept = 0.5), linetype = 2, color = 'grey')



#make subset for participants in first trial only: all smaller 1
#if TB first = 1 take TB, if TB first = 0, take FB row 
cho_first <- subset(cho_all, TB_first == 1 & Condition == 0 | TB_first == 0 & Condition == 1)

cho_all <- subset(File, excl < 0.5)
cho_excl <- subset(File, excl > 0.4)
cho_TBonly1st <- subset(rbind(subset(cho_excl, TB_first == 1), subset(cho_all, Condition == 0 & TB_first == 1)), Condition == 0)
cho_FBonly1st <- subset(rbind(subset(cho_excl, TB_first == 0), subset(cho_all, Condition == 1 & TB_first == 0)), Condition == 1) 

cho_first <- rbind(cho_TBonly1st, cho_FBonly1st)

first_choice_comparison <- t.test(Choice ~Condition, data = cho_first, paired = F)
first_choice_comparison #n.s difference between conditions
myTable_1stchoice <- table(cho_first$Choice, cho_first$Condition)

cho_TB <- subset(cho_first, Condition == 0) 


cho_FB <- subset(cho_first, Condition == 1)
FB = data.frame(FB= as.numeric(c(9, 7)))
FB_v0 <- binom.test(x = FB$FB, p = 0.5, alternative = "less")
FB_v0


TB = data.frame(TB= as.numeric(c(12, 8)))
TB_1stch <- binom.test(x = TB$TB, p = 0.5, alternative = "greater")
TB_1stch


FB1st_prop <- FB/sum(FB)
TB1st_prop <- TB/sum(TB)
FB1st_prop
TB1st_prop

gplot_Data_1stchoiceprops = gplot_Data_prop
myTable = table(cho_first$Choice, cho_first$Condition)

gplot_Data_1stchoiceprops[1,4] =myTable[1,2]/(myTable[1,2] + myTable[2, 2])
gplot_Data_1stchoiceprops[2,4] =myTable[2,2]/(myTable[1,2] + myTable[2, 2])
gplot_Data_1stchoiceprops[3,4] =myTable[1,1]/(myTable[1,1] + myTable[2, 1])
gplot_Data_1stchoiceprops[4,4] =myTable[2,1]/(myTable[1,1] + myTable[2, 1])
gplot_Data_1stchoiceprops[1,5] =myTable[1,2]
gplot_Data_1stchoiceprops[2,5] =myTable[2,2]
gplot_Data_1stchoiceprops[3,5] =myTable[1,1]
gplot_Data_1stchoiceprops[4,5] =myTable[2,1]
gplot_Data_1stchoiceprops <- subset (gplot_Data_1stchoiceprops, select = c(-Counts, -props))



ggplot() + 
  theme(text = element_text(size = 20), axis.title = element_text(size = 22), legend.text = element_text(size = 15), legend.title = element_text(size = 18), 
        panel.background = element_rect(fill = "#ffffff"), panel.border = element_rect(fill = NA)) + 
  geom_bar(aes(y = unlist(V4), 
               x = Condition, 
               fill = Choice, width = 0.7),
           data = gplot_Data_1stchoiceprops,
           stat = "identity") + 
  scale_fill_manual(values = c("#ff7A5C", "#56C4A3")) + 
  labs(y = "Proportions of Choices") + 
  geom_hline(aes(yintercept = 0.5), linetype = 2, color = 'grey')



#Welch Two Sample t-test

#data:  Choice by Condition
#t = -0.63439, df = 25.544, p-value = 0.5315
#alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
#95 percent confidence interval:
#  -0.4981668  0.2633490
#sample estimates:
#  mean in group 0 mean in group 1 
#0.4210526       0.5384615 



################# CBQ ANALYSIS #################################################


File_CBQ <- read.csv("final_data_choices_280322.csv", header = TRUE, sep = ";",  fileEncoding="latin1", dec = ",")

CBQ_incl <- subset(File_CBQ, excl_CBQ < 0.5)
head(CBQ_incl)


cbqData=data.frame(CBQ_incl)
cbqData


cbqData$age <- as.numeric(cbqData$age)
#SpeechData$sex <- as.numeric(scale.default(SpeechData$Sex))
#SpeechData$sex <- as.factor(SpeechData$sex)
cbqData$Condition <- as.factor(cbqData$Condition)
#Condition <- as.numeric(Condition) #?? 
cbqData$Choice <- as.factor(cbqData$Choice)
cbqData$TOM <- as.numeric(cbqData$TOM)
cbqData$CBQ <- as.numeric(cbqData$CBQ)


# # # # # #  set up GLMs with speech as predictor, as done above.

cbqNullM = glmer(Choice ~ (1|ID), data = cbqData, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(cbqNullM)

cbqCBQ = glmer(Choice ~ CBQ + (1|ID), data = cbqData, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(cbqCBQ)
anova(cbqCBQ, cbqNullM)

cbqCBQxCond = glmer(Choice ~ CBQ:Condition + (1|ID), data = cbqData, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(cbqCBQxCond)
anova(cbqCBQxCond, cbqNullM)


### Speech and TOM stuff -- not used
speechdata <- BasicModel %>% slice(19:66)
speechdata

File <- read.csv("SpeechModelData.csv", header = TRUE, sep = ";",  fileEncoding="latin1", dec = ",")
speechcoldf=data.frame(File)
Speach = speechcoldf$Speech

speachdata = cbind(speechdata, Speach)



NullSM = glmer(Choice ~ (1|Subject), data = speachdata, family = binomial, control=glmerControl(optimizer="bobyqa"))


ModelSM = glmer(Choice ~ Speach + TOM + (1|Subject), data = speachdata, family = binomial, control=glmerControl(optimizer="bobyqa") )
summary(ModelSM)
anova(NullSM, ModelSM)

FullSM = glmer(Choice ~ Condition:TOM +(1 |Subject), data = speachdata, family = binomial, control=glmerControl(optimizer="bobyqa") )
summary(FullSM)

anova(NullSM, FullSM)

#--> no effect of speech



########################################################################################################################################################################################################
########################################################################################################################################################################################################
########################################################################################################################################################################################################
########################################################################################################################################################################################################

# comparative data analysis 


#ImportStudyData
FileComp <- read.csv("comparativedata_coded_exclusion_250822_120d.csv", header = TRUE, sep = ";",  fileEncoding="latin1", dec = ",")


#make subset for participants in main analysis: all smaller 0.5
cho_Compall <- subset(FileComp, excl < 0.5)
cho_human <- subset(cho_Compall, species == 'H')
cho_dog <- subset(cho_Compall, species == 'D')

cho_alldogs <- subset(FileComp, species == 'D')
cho_alldogs_exclKids <- rbind(cho_human, cho_alldogs)

#make subset for participants in first trial only: all smaller 1
#if TB first = 1 take TB, if TB first = 0, take FB row 
# cho_first <- subset(cho_all, TB_first == 1 & Condition == 0 | TB_first == 0 & Condition == 1)



TB = data.frame(TB= as.numeric(c(18, 15)))
TB_v0 <- binom.test(x = TB$TB, p = 0.5, alternative = "greater")
TB_v0
#Exact binomial test

#data:  TB$TB
#number of successes = 18, number of trials = 33, p-value = 0.3642
#alternative hypothesis: true probability of success is greater than 0.5
#95 percent confidence interval:
#  0.3894025 1.0000000
#sample estimates:
#  probability of success 
#0.5454545 

FB = data.frame(FB= as.numeric(c(23, 12)))
FB_v0 <- binom.test(x = FB$FB, p = 0.5, alternative = "greater")
FB_v0


# for plotting: 
FB_prop <- FB/sum(FB)
TB_prop <- TB/sum(TB)

#Exact binomial test

#data:  FB$FB
#number of successes = 23, number of trials = 35, p-value = 0.04477
#alternative hypothesis: true probability of success is greater than 0.5
#95 percent confidence interval:
#  0.5044808 1.0000000
#sample estimates:
#  probability of success 
#0.6571429 

### calculating relative risks see https://stats.stackexchange.com/questions/176847/effect-size-of-a-binomial-test-and-its-relation-to-other-measures-of-effect-size

ch_prob <- 0.5

cho_hum_FB <- subset(cho_human, Condition == 1)
cho_hum_FB
cho_hum_TB <- subset(cho_human, Condition == 0)
cho_hum_TB

nCh_H_FB <- length(cho_hum_FB$Choice)
nCh_H_TB <- length(cho_hum_TB$Choice)
nSc_H_FB <- length(subset(cho_hum_FB, Choice == 0)$Choice)
nSc_H_TB <- length(subset(cho_hum_TB, Choice == 0)$Choice)


RR_HFB = ((23/35)/ch_prob) # number of successes is number of baited container choices. Divided by total number of choices. 
RR_HFB
RR_HTB = ((18/33)/ch_prob) # but this bin. test was not sig. 
RR_HTB
# in dogs, count the number of successes. 
cho_dog_FB <- subset(cho_dog, Condition == 1)
cho_dog_FB
cho_dog_TB <- subset(cho_dog, Condition == 0)
cho_dog_TB

# count all the events in dog data --> length of Choice column 
nCh_D_FB <- length(cho_dog_FB$Choice)
nCh_D_FB
nCh_D_TB <- length(cho_dog_TB$Choice)
nCh_D_TB
# count successes (baited choices) in each cond in dogs 
nSc_D_FB <- length(subset(cho_dog_FB, Choice == 0)$Choice)
nSc_D_FB
nSc_D_TB <- length(subset(cho_dog_TB, Choice == 0)$Choice)
nSc_D_TB

RR_DFB <- ((nSc_D_FB/nCh_D_FB)/ch_prob)
RR_DFB
RR_DTB <- ((nSc_D_TB/nCh_D_TB)/ch_prob)
RR_DTB

## also run the binomial tests of the dog data 

DFB = data.frame(DFB= as.numeric(c(nSc_D_FB, (nCh_D_FB - nSc_D_FB))))
DFB_v0 <- binom.test(x = DFB$DFB, p = 0.5, alternative = "greater")
DFB_v0

#Exact binomial test

#data:  DFB$DFB
#number of successes = 37, number of trials = 70, p-value = 0.3601
#alternative hypothesis: true probability of success is greater than 0.5
#95 percent confidence interval:
#  0.4238586 1.0000000
#sample estimates:
#  probability of success 
#0.5285714 



DTB = data.frame(DTB= as.numeric(c(nSc_D_TB, (nCh_D_TB - nSc_D_TB))))
DTB_v0 <- binom.test(x = DTB$DTB, p = 0.5, alternative = "greater") ## different direction!! 
DTB_v0

#Exact binomial test

#data:  DTB$DTB
#number of successes = 24, number of trials = 74, p-value = 0.001686
#alternative hypothesis: true probability of success is less than 0.5
#95 percent confidence interval:
#  0.0000000 0.4248298
#sample estimates:
#  probability of success 
#0.3243243 


# change direction of RR for TB 
RR_DTB_reverse <-  (((nCh_D_TB - nSc_D_TB)/nCh_D_TB)/ch_prob)
RR_DTB_reverse




#### run glmm 

## test for homogeneity of variances between dogs and humans 

leveneTest(Choice ~ species, data = cho_Compall)

#Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
#group   1  0.2566 0.6131
#      182 

# homogeneity of variance is given 
NullComp <- glmer(Choice ~ (1|ID), data=cho_alldogs_exclKids, family = binomial, control=glmerControl(optimizer="bobyqa"))

model_comp <- glmer(Choice ~  species*Condition + (1|ID), data=cho_alldogs_exclKids, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(model_comp) # only intercept is sig. 
model_comp_simplified <- glmer(Choice ~  Condition + species:Condition + (1|ID), data=cho_alldogs_exclKids, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(model_comp_simplified)
anova(NullComp, model_comp_simplified) # not better than null

model_ME <- glmer(Choice ~  Condition + species + (1|ID), data=cho_alldogs_exclKids, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(model_ME)
anova(NullComp, model_ME) #

model_interaction <-  glmer(Choice ~  species:Condition + (1|ID), data=cho_alldogs_exclKids, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(model_interaction)
anova(NullComp, model_interaction) #

#### inverted levels 

cho_alldogs_exclKids$species_reverse <- relevel(as.factor(cho_alldogs_exclKids$species), ref="H")
cho_alldogs_exclKids$Condition_reverse <- abs(cho_alldogs_exclKids$Condition -1 )

#reeverse
model_comp_reverse <- glmer(Choice ~  species_reverse*Condition_reverse + (1|ID), data=cho_alldogs_exclKids, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(model_comp_reverse) # only intercept is sig. 

model_dog_reverse <- glmer(Choice ~  species_reverse*Condition + (1|ID), data=cho_alldogs_exclKids, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(model_dog_reverse) # 

model_FB_reverse <- glmer(Choice ~  species*Condition_reverse + (1|ID), data=cho_alldogs_exclKids, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(model_FB_reverse) # 


# model comp tells me: Condition effect in dogs
# model dog rev tells me condition effect in humans 

# model comp tells me Species effect in TB
#model FB comp tells me Species effect in FB


emmeans(model_comp, 'species')
emmeans(model_comp, 'Condition')

emmeans(model_comp, 'species', 'Condition')
emmeans(model_comp, 'Condition', 'species')

