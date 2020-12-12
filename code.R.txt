---
title: "ADA—final"
author: "Baoling Zhen"
date: "12/1/2020"
output: html_document
---

```{r}
setwd("C:/Users/baoli/Box/ADA final project/adult_2017_sas")
library(sas7bdat)
chis<-read.sas7bdat("adult.sas7bdat")
```

## view data & subset

```{r }
chis1<-chis[c("INS65","AH16","AK28","DSTRS12")]
##delect missing value & complete case
chis1[chis1<0]<-NA  
chis_c<- chis1[complete.cases(chis1), ] 
summary(chis_c)
str(chis_c)

```

## variable

# neighborhood safety: AK28
# 1 ALL OF THE TIME 23013 54.37
# 2 MOST OF THE TIME 15628 36.92
# 3 SOME OF THE TIME 3046 7.20
# 4 NONE OF THE TIME 626 1.48

## delay in care: AH16

##TYPE OF CURRENT HEALTH COVERAGE SOURCE FOR ELDERLY 65+: INS65
# 1 MEDICARE + MEDI-CAL (MEDICAID) 3045 7.19
# 2 MEDICARE + OTHER 11295 26.68
# 3 MEDICARE ONLY 987 2.33
# 4 OTHER ONLY 691 1.63
# 5 UNINSURED 80 0.19

## distress: DSTRS12

```{r }
##make dummy variable
# neighborhood safety
chis_c$AK28[chis_c$AK28=="1"]<-1
chis_c$AK28[chis_c$AK28=="2"]<-1
chis_c$AK28[chis_c$AK28=="3"]<-0
chis_c$AK28[chis_c$AK28=="4"]<-0
# distress
chis_c$DSTRS12[chis_c$DSTRS12=='2']<-0
# delay in care
chis_c$AH16[chis_c$AH16=='2']<-0

## transfer to factor
chis_c$INS65<-as.factor(chis_c$INS65)
# check
str(chis_c)
summary(chis_c)

##after recoding
# neighborhood safety: AK28
# ALL OF THE TIME 23013 54.37/MOST OF THE TIME--1
# SOME OF THE TIME 3046 7.20/NONE OF THE TIME 626 1.48--0

## delay in care: AH16
# yes-1, no-0

##TYPE OF CURRENT HEALTH COVERAGE SOURCE FOR ELDERLY 65+: INS65
# 1 MEDICARE + MEDI-CAL (MEDICAID) 3045 7.19
# 2 MEDICARE + OTHER 11295 26.68
# 3 MEDICARE ONLY 987 2.33
# 4 OTHER ONLY 691 1.63
# 5 UNINSURED 80 0.19

## distress: DSTRS12
# yes-1, no-0
```

logic regression
```{r}
#logistic model with bmi as a predictor
Logit <- glm( DSTRS12~ INS65+AH16+AK28, data=chis_c, family="binomial")
summary(Logit)

#calculate and print ORs and 95% CIs  
OR<-exp(cbind(OR = coef(Logit), confint(Logit))) #calculate ORs and 95% CIs
OR #print ORs and 95% CIs

```

