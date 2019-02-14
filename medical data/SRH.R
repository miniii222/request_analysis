
library(Hmisc)
setwd("C:/Users/wjssm/Desktop/dy")
Q716=spss.get("HN16_ALL.sav",use.value.labels = TRUE)
Q717=spss.get("HN17_ALL.sav",use.value.labels = TRUE)

library(plyr)
Q7=rbind.fill(Q716,Q717)



###  age
####  ~39 : 1
#### 40~59 : 2
#### 60~80 :3

Q7=Q7[Q7$age>=19,]
Q7$age <- ifelse(Q7$age >= 60, 3,Q7$age)
Q7$age <- ifelse(Q7$age >=40, 2,Q7$age)
Q7$age <- ifelse(Q7$age >=19, 1,Q7$age)



### npins : 민간의료보험가입여부

Q7$npins = with(Q7, ifelse(Q7$npins==2,0,npins))
Q7$npins = with(Q7, ifelse(Q7$npins==1,1,npins))
Q7$npins = with(Q7, ifelse(Q7$npins>=8,NA,npins))


### D.1.1 data : 주관적 건강상태(SRH)

Q7$D.1.1 = with(Q7, ifelse(Q7$D.1.1<=3,1,D.1.1))
Q7$D.1.1 = with(Q7, ifelse(Q7$D.1.1>=8,NA,D.1.1))
Q7$D.1.1 = with(Q7, ifelse(Q7$D.1.1>=4,0,D.1.1))
Q7$SRH = Q7$D.1.1


### 질병 변수

# DE1.pr : 당뇨병(DM)
Q7$DM = with(Q7, ifelse(DE1.pr == 9 , NA,
                        ifelse(DE1.pr ==8,0,DE1.pr)))
# DI1.pr : 고혈압(hypertension)
Q7$hypertension = with(Q7, ifelse(DI1.pr == 9 , NA,
                                  ifelse(DI1.pr ==8,0,DI1.pr)))
# DC1.pr : 위암
Q7$stomach = with(Q7, ifelse(DC1.pr == 9 , NA,
                             ifelse(DC1.pr ==8,0,DC1.pr)))
# DC2.pr : 간암
Q7$liver = with(Q7, ifelse(DC2.pr == 9 , NA,
                           ifelse(DC2.pr ==8,0,DC2.pr)))
# DC3.pr : 대장암
Q7$colorectal = with(Q7, ifelse(DC3.pr == 9 , NA,
                                ifelse(DC3.pr ==8,0,DC3.pr)))
# DC4.pr : 유방암
Q7$breast = with(Q7, ifelse(DC4.pr == 9 , NA,
                            ifelse(DC4.pr ==8,0,DC4.pr)))
# DC5.pr : 자궁경부암
Q7$cervical = with(Q7, ifelse(DC5.pr == 9 , NA,
                              ifelse(DC5.pr ==8,0,DC5.pr)))
# DC6.pr : 갑상선암
Q7$thyroid = with(Q7, ifelse(DC6.pr == 9 , NA,
                             ifelse(DC6.pr ==8,0,DC6.pr)))


library(dplyr)
Select = c("age","sex","SRH","npins", "DM","ho.incm",'edu','hypertension',
           'stomach','liver','colorectal','breast','cervical','thyroid')
Dat = Q7 [,Select]
#na chek
sapply(Dat, function(x) sum(is.na(x)))


#only complete case
Dat$ok = complete.cases(Dat)
Dat = Dat[Dat$ok==1,]
sum(!complete.cases(Dat))
head(Dat)



library(MASS)
library(moonBook)
tab2=mytable(npins~.-ok, data=Dat)
tab2
#mycsv(tab2,file="table1.csv") #csv 파일로 저장하는 코드!!


#visualization
library(ggplot2); library(dplyr)

Dat %>% group_by(age) %>% summarise(npins_p = mean(npins)) %>% 
  ggplot()+geom_bar(aes(factor(age), npins_p,
                        fill = factor(age)), stat = 'identity')+
  xlab("age")


Dat %>% group_by(sex) %>% summarise(npins_p = mean(npins)) %>% 
  ggplot()+geom_bar(aes(factor(sex), npins_p,
                        fill = factor(sex)), stat = 'identity')+
  xlab("sex")

Dat %>% group_by(SRH) %>% summarise(npins_p = mean(npins)) %>% 
  ggplot()+geom_bar(aes(factor(SRH), npins_p,
                        fill = factor(SRH)), stat = 'identity')+
  xlab("SRH")

Dat %>% group_by(DM) %>% summarise(npins_p = mean(npins)) %>% 
  ggplot()+geom_bar(aes(factor(DM), npins_p,
                        fill = factor(DM)), stat = 'identity')+
  xlab("DM")

Dat %>% group_by(ho.incm) %>% summarise(npins_p = mean(npins)) %>% 
  ggplot()+geom_bar(aes(factor(ho.incm), npins_p,
                        fill = factor(ho.incm)), stat = 'identity')+
  xlab("ho.incm")

Dat %>% group_by(edu) %>% summarise(npins_p = mean(npins)) %>% 
  ggplot()+geom_bar(aes(factor(edu), npins_p,
                        fill = factor(edu)), stat = 'identity')+
  xlab("edu")

Dat %>% group_by(hypertension) %>% summarise(npins_p = mean(npins)) %>% ggplot()+geom_bar(aes(factor(hypertension), npins_p,
                                                                                              fill = factor(hypertension)), stat = 'identity')+
  xlab("hypertension")

Dat %>% group_by(stomach) %>% summarise(npins_p = mean(npins)) %>% 
  ggplot()+geom_bar(aes(factor(stomach), npins_p,
                        fill = factor(stomach)), stat = 'identity')+
  xlab("stomach")

Dat %>% group_by(liver) %>% summarise(npins_p = mean(npins)) %>% 
  ggplot()+geom_bar(aes(factor(liver), npins_p,
                        fill = factor(liver)), stat = 'identity')+
  xlab("liver")

Dat %>% group_by(colorectal) %>% summarise(npins_p = mean(npins)) %>% 
  ggplot()+geom_bar(aes(factor(colorectal), npins_p,
                        fill = factor(colorectal)), stat = 'identity')+
  xlab("colorectal")

Dat %>% group_by(breast) %>% summarise(npins_p = mean(npins)) %>% 
  ggplot()+geom_bar(aes(factor(breast), npins_p,
                        fill = factor(breast)), stat = 'identity')+
  xlab("breast")

Dat %>% group_by(cervical) %>% summarise(npins_p = mean(npins)) %>% 
  ggplot()+geom_bar(aes(factor(cervical), npins_p,
                        fill = factor(cervical)), stat = 'identity')+
  xlab("cervical")

Dat %>% group_by(thyroid) %>% summarise(npins_p = mean(npins)) %>% 
  ggplot()+geom_bar(aes(factor(thyroid), npins_p,
                        fill = factor(thyroid)), stat = 'identity')+
  xlab("thyroid")





# modeling

### 1. npins ~ SRH

m1 = glm(npins~SRH, family = binomial, data=Dat)
summary(m1) #SRH p-value <0.05 통계적으로 유의한 변수
round(exp(cbind("odds ratio"=coef(m1), confint(m1))),2)



#odds ratio > 1 주관적 건강상태가 좋을수록 민간의료보험가입할 Odds가 높다.


### 2. 환자 info variables


m3 = glm(npins ~ age+factor(sex)+edu+ho.incm+SRH, data = Dat, family = binomial)
summary(m3)


#- 모두 통계적으로 유의미한 변수ㄷ
#- age 의 계수만 음수.나이가 어릴수록 민간의료보험가입한다.
#- 여자일수록, 교육수준이 높을수록, 소득수준이 높을수록, 주관적 건강상태가 좋을수록 민간의료보험가입할 확률이 높다.



### 3. 환자 info + 질병변수들

m4 <- glm(npins ~ age+factor(sex)+edu+ho.incm+SRH+
            DM+hypertension+stomach+liver+colorectal+breast+    
            cervical+thyroid, data = Dat, family = binomial)
summary(m4)


#- 통계적으로 유의미하지않은 변수들도 보인다
#- 따라서 step을 이용하여 변수선택.



m4.final = step(m4)
summary(m4.final)


#- 추가된 질병변수들을 중심으로 살펴보면,
#- DM(당뇨병) 에 걸리지 않을수록,
#- hypertension(고혈압)에 걸리지 않을수록,
#- stomach(위암)에 걸리지 않을수로,
#- 민간의료보험가입할 확률이 높다.



### 4. controlling income

income1 = glm(npins~SRH, subset =(ho.incm==1), family = binomial, data=Dat)
round(exp(cbind("odds ratio"=coef(income1), confint(income1))),2)

income2 = glm(npins~SRH, subset =(ho.incm==2), family = binomial, data=Dat)
round(exp(cbind("odds ratio"=coef(income2), confint(income2))),2)

income3 = glm(npins~SRH, subset =(ho.incm==3), family = binomial, data=Dat)
round(exp(cbind("odds ratio"=coef(income3), confint(income3))),2)

income4 = glm(npins~SRH, subset =(ho.incm==4), family = binomial, data=Dat)
round(exp(cbind("odds ratio"=coef(income4), confint(income4))),2)



#- 어떤 소득분위를 가지든 odds ratio가 1보다 크다.
#- 소득분위에 따라 Odds ratio가 크게 달라지지 않는다.
#- 소득분위의 관계없이 주관적 건강상태가 좋을수록 민간의료보험가입할 Odds가 높다.


### 5-1. ho.incm * SRH


m2.1 = glm(npins~SRH + factor(ho.incm),family = binomial,data=Dat)

#교호작용 추가한 모델
m2.1.interaction = glm(npins~SRH*factor(ho.incm),family =binomial,data=Dat)

anova(m2.1, m2.1.interaction, test="LRT")


# - m2.1 모델(SRH + factor(ho.incm))과 m2.1모델에 SRH, factor(ho.incm)의 교호작용을 추가한 모델을 anova로 비교.
# - p-value 가 굉장히 큰 값.->accept H0
# - 교호작용을 추가하지 않은 경우가 더 좋은 모델이라고 할 수 있다.


### 5-2. age * SRH


m2.2 = glm(npins~SRH + factor(age),family = binomial,data=Dat)

#교호작용 추가한 모델
m2.2.interaction = glm(npins~SRH*factor(age),family =binomial,data=Dat)

anova(m2.2, m2.2.interaction, test="LRT")


# - m2.2 모델(SRH + factor(age))과 m2.1모델에 SRH, factor(age)의 교호작용을 추가한 모델을 anova로 비교.
# - p-value < 0.05 .-> reject H0
# - 교호작용을 추가한 경우가 더 좋은 모델이라고 할 수 있다.


### 5-3. sex * SRH


m2.3 = glm(npins~SRH + factor(sex),family = binomial,data=Dat)

#교호작용 추가한 모델
m2.3.interaction = glm(npins~SRH*factor(sex),family =binomial,data=Dat)

anova(m2.3, m2.3.interaction, test="LRT")


# - m2.3 모델(SRH + factor(sex))과 SRH, factor(sex)의 교호작용을 추가한 모델을 anova로 비교.
# - p-value > 0.05.->accept H0
# - 교호작용을 추가하지 않은 경우가 더 좋은 모델이라고 할 수 있다.

### 5-4. edu * SRH


m2.4 = glm(npins~SRH + factor(edu),family = binomial,data=Dat)

#교호작용 추가한 모델
m2.4.interaction = glm(npins~SRH*factor(edu),family =binomial,data=Dat)

anova(m2.4, m2.4.interaction, test="LRT")


# - m2.4 모델(SRH + factor(edu))과 m2.4모델에 SRH, factor(edu)의 교호작용을 추가한 모델을 anova로 비교.
# - p-value > 0.05 값.->accept H0
# - 교호작용을 추가하지 않은 경우가 더 좋은 모델이라고 할 수 있다.


### 5-5. ho.incm * DM


m2.5 = glm(npins~SRH + factor(DM),family = binomial,data=Dat)

#교호작용 추가한 모델
m2.5.interaction = glm(npins~SRH*factor(DM),family =binomial,data=Dat)

anova(m2.5, m2.5.interaction, test="LRT")


# - m2.5 모델(SRH + factor(DM))과 m2.5모델에 SRH, factor(DM)의 교호작용을 추가한 모델을 anova로 비교.
# - p-value < 0.05 .-> reject H0
# - 교호작용을 추가한 경우가 더 좋은 모델이라고 할 수 있다.


### 5-6. hypertension * SRH


m2.6 = glm(npins~SRH + factor(hypertension),family = binomial,data=Dat)

#교호작용 추가한 모델
m2.6.interaction = glm(npins~SRH*factor(hypertension),family =binomial,data=Dat)

anova(m2.6, m2.6.interaction, test="LRT")


# - m2.6 모델(SRH + factor(hypertension))과 m2.1모델에 SRH, factor(hypertension)의 교호작용을 추가한 모델을 anova로 비교.
# - p-value > 0.05.->accept H0
# - 교호작용을 추가하지 않은 경우가 더 좋은 모델이라고 할 수 있다.


### 5-7. stomach * SRH


m2.7 = glm(npins~SRH + factor(stomach),family = binomial,data=Dat)

#교호작용 추가한 모델
m2.7.interaction = glm(npins~SRH*factor(stomach),family =binomial,data=Dat)

anova(m2.7, m2.7.interaction, test="LRT")


# - m2.7 모델(SRH + factor(stomach))과 m2.7모델에 SRH, factor(stomach)의 교호작용을 추가한 모델을 anova로 비교.
# - p-value > 0.05.->accept H0
# - 교호작용을 추가하지 않은 경우가 더 좋은 모델이라고 할 수 있다.
