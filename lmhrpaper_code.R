#loading libraries
library(readxl)
library(RNHANES)
library(tidyverse)
library(gtools)
library(tableone)
library(patchwork)

#setting up working directory
setwd("C:/Users/adria/Dropbox/UIEM/Proyectos/LMHR")

#Getting NHANES' data
nhanes1 <-  nhanes_load_data("TRIGLY_G", "2011-2012")
nhanes2 <- nhanes_load_data("HDL_G","2011-2012")
nhanes3 <- nhanes_load_data("BMX_G","2011-2012")
nhanes4 <- nhanes_load_data("DEMO_G","2011-2012")

#Merging NHANES' datasets and excluding children
nhanes_lipids <- merge(nhanes1,nhanes2,by="SEQN")
nhanes_demo <- merge(nhanes3,nhanes4,by="SEQN") %>% filter(RIDAGEYR>17)
nhanes_data <- merge(nhanes_lipids,nhanes_demo,by="SEQN")

#CHolesterol Super Survey data upload
data <- read_excel("data.xlsx")

#How many and why people will be excluded?
#Because they were on lipid lowering medications.
sum(data$lipid_low_rx==0, na.rm = TRUE)
#Because the days between their lab tests were less than 30.
sum(data$days_btwn_tests<28, na.rm = TRUE)
#Because the days between their lab tests were more than five years.
sum(data$days_btwn_tests>1825, na.rm = TRUE)
#Because an unreliable carbohydrate intake report.
sum(data$net_carbs<0, na.rm = TRUE)
#Because an unreliable carbohydrate intake report.
sum(data$net_carbs> 130, na.rm = TRUE) 
#Because they were younger than 18 years old.
sum(data$age<18, na.rm = TRUE) 
#Because they were older than 100 years old.
sum(data$age>100, na.rm = TRUE) 
#Because an unreliable triglyceride result.
sum(data$pTG<20, na.rm = TRUE) 
#Because an unreliable triglyceride result.
sum(data$pTG>1500, na.rm = TRUE) 
#Because an unreliable triglyceride result.
sum(data$cTG<20, na.rm = TRUE)
#Because an unreliable triglyceride result.
sum(data$cTG>1500, na.rm = TRUE)
#Because an unreliable LDL result.
sum(data$pLDL<30, na.rm = TRUE)
#Because an unreliable LDL result.
sum(data$pLDL>1000, na.rm = TRUE)
#Because an unreliable LDL result.
sum(data$cLDL<30, na.rm = TRUE)
#Because an unreliable LDL result.
sum(data$cLDL>1000, na.rm = TRUE)
#Because an unreliable BMI report.
sum(data$bmi<10, na.rm = TRUE)
#Because an unreliable BMI report.
sum(data$bmi>50, na.rm = TRUE)
#Because an unreliable HDL result.
sum(data$cHDL<10, na.rm = TRUE)
#Because an unreliable HDL result.
sum(data$cHDL>200, na.rm = TRUE)
#Because an unreliable HDL result.
sum(data$pHDL<10, na.rm = TRUE)
#Because an unreliable HDL result.
sum(data$pHDL>200, na.rm = TRUE)

#Implementing filtering criteria
data <- data %>% 
  select(age,lmhr,gender,net_carbs,lipid_low_rx,days_btwn_tests,cTG,cLDL,cHDL,pLDL,pHDL,pTG,bmi) %>% 
    filter(lipid_low_rx==1) %>%
    filter(days_btwn_tests>=28) %>%
    filter(days_btwn_tests<=1825) %>%
    filter(net_carbs>=0) %>%
    filter(net_carbs<= 130) %>%
    filter(age>=18) %>%
    filter(age<=100) %>%
    filter(pTG>=20) %>%
    filter(pTG<=1500) %>%
    filter(cTG>=20) %>%
    filter(cTG<=1500) %>%
    filter(pLDL>=30) %>%
    filter(pLDL<=1000)  %>%
    filter(cLDL>=30) %>%
    filter(cLDL<=1000) %>%
    filter(bmi>=10) %>%
    filter(bmi<=50) %>%
    filter(cHDL>=10) %>%
    filter(cHDL<=200) %>%
    filter(pHDL>=10) %>%
    filter(pHDL<=200) %>%
#creating response variables
      mutate(delta_LDL= cLDL-pLDL) %>%
      mutate(delta_HDL= cHDL-pHDL) %>%
      mutate(delta_TG= cTG-pTG) %>%
      mutate(cTGtoHDL= cTG/cHDL) %>%
      mutate(pTGtoHDL= pTG/pHDL) %>%
      mutate(deltaTGtoHDL=cTGtoHDL-pTGtoHDL) %>% 
#Removing duplicates
  distinct()

#Descriptive Statistics
#Cuantitative variables
table_all <- CreateContTable(data=data, vars = c("age",
"gender","net_carbs","cTG","cLDL","cHDL","pLDL","pHDL",
"pTG","bmi","delta_LDL","delta_HDL","delta_TG","cTGtoHDL",
"pTGtoHDL","deltaTGtoHDL"))
summary(table_all)

table_lmhr <- CreateContTable(data=data,strata="lmhr", vars = c("gender",
"age","bmi","net_carbs","pLDL","cLDL","delta_LDL","pHDL",
"cHDL","delta_HDL","pTG","cTG","delta_TG","pTGtoHDL",
"cTGtoHDL","deltaTGtoHDL"))
summary(table_lmhr)

#Categorical variables
table_all_cat <- CreateCatTable(data=data, vars = c("gender"))
summary(table_all_cat)

table_lmhr_cat <- CreateCatTable(data=data,strata="lmhr", vars = c("gender"))
summary(table_lmhr_cat)

#Percentile distributions
#Carb intake
qnetcarbs <- quantile(data$net_carbs, probs = c(0.05,0.25,0.5,0.75,0.95))
qnetcarbs

#BMI
qbmi <- quantile(data$bmi, probs = c(0.05,0.25,0.5,0.75,0.95))
qbmi

#Current TG
qcTG <- quantile(data$cTG, probs = c(0.05,0.25,0.5,0.75,0.95))
qcTG

#Current LDL
qcLDL <- quantile(data$cLDL, probs = c(0.05,0.25,0.5,0.75,0.95))
qcLDL

#Current HDL
qcHDL <- quantile(data$cHDL, probs = c(0.05,0.25,0.5,0.75,0.95))
qcHDL

#Current preTG
qpTG <- quantile(data$pTG, probs = c(0.05,0.25,0.5,0.75,0.95))
qpTG

#Current preLDL
qpLDL <- quantile(data$pLDL, probs = c(0.05,0.25,0.5,0.75,0.95))
qpLDL

#Current pHDL
qpHDL <- quantile(data$pHDL, probs = c(0.05,0.25,0.5,0.75,0.95))
qpHDL

#Age
qage <- quantile(data$age, probs = c(0.05,0.25,0.5,0.75,0.95))
qage

#LDL change
qdelta_LDL <- quantile(data$delta_LDL, probs = c(0.05,0.25,0.5,0.75,0.95))
qdelta_LDL

#Tryglyceride change
qdelta_TG <- quantile(data$delta_TG, probs = c(0.05,0.25,0.5,0.75,0.95))
qdelta_TG

#HDL change
qdelta_HDL <- quantile(data$delta_HDL, probs = c(0.05,0.25,0.5,0.75,0.95))
qdelta_HDL

#Prior Trig/HDL ratio
qpTGtoHDL <- quantile(data$pTGtoHDL, probs = c(0.05,0.25,0.5,0.75,0.95))
qpTGtoHDL

#Current Trig/HDL ratio
qcTGtoHDL <- quantile(data$cTGtoHDL, probs = c(0.05,0.25,0.5,0.75,0.95))
qcTGtoHDL

#Trig/HDL ratio change
qdelta_TGtoHDL <- quantile(data$deltaTGtoHDL, probs = c(0.05,0.25,0.5,0.75,0.95))
qdelta_TGtoHDL

#It is already clear they are not normally distributed but tested anyway.
shapiro.test(data$bmi)
shapiro.test(data$pLDL)
shapiro.test(data$cLDL)
shapiro.test(data$pHDL)
shapiro.test(data$cHDL)
shapiro.test(data$pTG)
shapiro.test(data$cTG)
shapiro.test(data$delta_TG)
shapiro.test(data$delta_LDL)
shapiro.test(data$delta_HDL)
shapiro.test(data$pTGtoHDL)
shapiro.test(data$cTGtoHDL)

#LMHR vs not LMHR plots
bmi <- ggplot(data=data,mapping = aes(x=bmi,fill=factor(lmhr)))+
  geom_density(inherit.aes = TRUE,alpha=0.3)+
  ggtitle("BMI by Phenotype")+
  scale_y_continuous(name="Density",breaks=waiver())+
  scale_x_continuous(name="BMI",breaks=waiver())+
  scale_fill_discrete(name = "Phenotype",labels = c("non-LMHR", "LMHR"))+
  theme_classic(base_size = 10)

age <- ggplot(data=data,mapping = aes(x=age,fill=factor(lmhr)))+
  geom_density(inherit.aes = TRUE,alpha=0.3)+
  ggtitle("Age by Phenotype")+
  scale_y_continuous(name="Density",breaks=waiver())+
  scale_x_continuous(name="Age",breaks=waiver())+
  scale_fill_discrete(name = "Phenotype",labels = c("non-LMHR", "LMHR"))+
  theme_classic(base_size = 10)

pLDL <- ggplot(data=data,mapping = aes(x=pLDL,fill=factor(lmhr)))+
  geom_density(inherit.aes = TRUE,alpha=0.3)+
  ggtitle("LDL prior to Low-Carb Diet")+
  scale_y_continuous(name="Density",breaks=waiver())+
  scale_x_continuous(name="mg/dL",breaks=waiver())+
  scale_fill_discrete(name = "Phenotype",labels = c("non-LMHR", "LMHR"))+
  theme_classic(base_size = 10)
pLDL

cLDL <- ggplot(data=data,mapping = aes(x=cLDL,fill=factor(lmhr)))+
  geom_density(inherit.aes = TRUE,alpha=0.3)+
  ggtitle("LDL current, on Low-Carb Diet")+
  scale_y_continuous(name="Density",breaks=waiver())+
  scale_x_continuous(name="mg/dL",breaks=waiver())+
  scale_fill_discrete(name = "Phenotype",labels = c("non-LMHR", "LMHR"))+
  theme_classic(base_size = 10)
cLDL

pTG <- ggplot(data=data,mapping = aes(x=pTG,fill=factor(lmhr)))+
  geom_density(inherit.aes = TRUE,alpha=0.3)+
  ggtitle("Triglycerides prior to Low-Carb Diet")+
  scale_y_continuous(name="Density",breaks=waiver())+
  scale_x_continuous(name="mg/dL",breaks=waiver(),limits = c(0,300))+
  scale_fill_discrete(name = "Phenotype",labels = c("non-LMHR", "LMHR"))+
  theme_classic(base_size = 10)
pTG

cTG <- ggplot(data=data,mapping = aes(x=cTG,fill=factor(lmhr)))+
  geom_density(inherit.aes = TRUE,alpha=0.3)+
  ggtitle("Triglycerides current, on Low-Carb Diet")+
  scale_y_continuous(name="Density",breaks=waiver())+
  scale_x_continuous(name="mg/dL",breaks=waiver(),limits = c(0,300))+
  scale_fill_discrete(name = "Phenotype",labels = c("non-LMHR", "LMHR"))+
  theme_classic(base_size = 10)
cTG

pHDL <- ggplot(data=data,mapping = aes(x=pHDL,fill=factor(lmhr)))+
  geom_density(inherit.aes = TRUE,alpha=0.3)+
  ggtitle("HDL prior to Low Carb-Diet")+
  scale_y_continuous(name="Density",breaks=waiver())+
  scale_x_continuous(name="mg/dL",breaks=waiver())+
  scale_fill_discrete(name = "Phenotype",labels = c("non-LMHR", "LMHR"))+
  theme_classic(base_size = 10)
pHDL

cHDL <- ggplot(data=data,mapping = aes(x=cHDL,fill=factor(lmhr)))+
  geom_density(inherit.aes = TRUE,alpha=0.3)+
  ggtitle("HDL current, on Low-Carb Diet")+
  scale_y_continuous(name="Density",breaks=waiver())+
  scale_x_continuous(name="mg/dL",breaks=waiver())+
  scale_fill_discrete(name = "Phenotype",labels = c("non-LMHR", "LMHR"))+
  theme_classic(base_size = 10)
cHDL

pTGtoHDL <- ggplot(data=data,mapping = aes(x=pTGtoHDL,fill=factor(lmhr)))+
  geom_density(inherit.aes = TRUE,alpha=0.3)+
  ggtitle("Trig/HDL ratio prior to Low-Carb Diet")+
  scale_y_continuous(name="Density",breaks=waiver())+
  scale_x_continuous(breaks=waiver(), limits = c(0,7))+
  scale_fill_discrete(name = "Phenotype",labels = c("non-LMHR", "LMHR"))+
  theme_classic(base_size = 10)
pTGtoHDL

cTGtoHDL <- ggplot(data=data,mapping = aes(x=cTGtoHDL,fill=factor(lmhr)))+
  geom_density(inherit.aes = TRUE,alpha=0.3)+
  ggtitle("Trig/HDL ratio current, on Low-Carb Diet")+
  scale_y_continuous(name="Density",breaks=waiver())+
  scale_x_continuous(breaks=waiver(), limits = c(0,7))+
  scale_fill_discrete(name = "Phenotype",labels = c("non-LMHR", "LMHR"))+
  theme_classic(base_size = 10)
cTGtoHDL

delta_ldl_plot <- ggplot(data=data,
mapping = aes(x=factor(lmhr),y=delta_LDL,fill=factor(lmhr)))+
  geom_violin(inherit.aes = TRUE,alpha=0.3)+
  geom_boxplot(alpha=0.3,width=0.3)+
  ylab("Delta LDL")+
  ggtitle("Delta LDL by Phenotype")+
  theme_classic()+
  scale_fill_discrete(name = "Phenotype", labels = c("non-LMHR", "LMHR"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
delta_ldl_plot

delta_hdl_plot <- ggplot(data=data,
mapping = aes(x=factor(lmhr),y=delta_HDL,fill=factor(lmhr)))+
  geom_violin(inherit.aes = TRUE,alpha=0.3)+
  geom_boxplot(alpha=0.3,width=0.3)+
  ylab("Delta HDL")+
  ggtitle("Delta HDL by Phenotype")+
  theme_classic()+
  scale_fill_discrete(name = "Phenotype", labels = c("non-LMHR", "LMHR"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
delta_hdl_plot

delta_tg_plot <- ggplot(data=data,
mapping = aes(x=factor(lmhr),y=delta_TG,fill=factor(lmhr)))+
  geom_violin(inherit.aes = TRUE,alpha=0.3)+
  geom_boxplot(alpha=0.3,width=0.3)+
  ylab("Delta Triglycerides")+
  ggtitle("Delta Triglycerides by Phenotype")+
  theme_classic()+
  scale_fill_discrete(name = "Phenotype", labels = c("non-LMHR", "LMHR"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
delta_tg_plot

delta_tgtohdl_plot <- ggplot(data=data,
mapping = aes(x=factor(lmhr),y=deltaTGtoHDL,fill=factor(lmhr)))+
  geom_violin(inherit.aes = TRUE,alpha=0.3)+
  geom_boxplot(alpha=0.3,width=0.3)+
  ylab("Delta Trig/HDL")+
  ggtitle("Delta Trig/HDL by Phenotype")+
  theme_classic()+
  scale_fill_discrete(name = "Phenotype", labels = c("non-LMHR", "LMHR"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
delta_tgtohdl_plot

#Hypothesis testing. 
#Post-hoc analysis or p-value adjustment aren't needed, there are only 2 groups.

#LMHR are leaner than LHMR
kruskal.test(age~lmhr,data=data)
#But not older
kruskal.test(age~lmhr,data=data)

#LMHR respond more than non LMHR
kruskal.test(delta_LDL~lmhr,data=data)
kruskal.test(delta_HDL~lmhr,data=data)
kruskal.test(delta_TG~lmhr,data=data)
kruskal.test(deltaTGtoHDL~lmhr,data=data)

#LMHR vs non LMHR before and after
kruskal.test(pLDL~lmhr,data=data)
kruskal.test(cLDL~lmhr,data=data)
kruskal.test(pHDL~lmhr,data=data)
kruskal.test(cHDL~lmhr,data=data)
kruskal.test(pTG~lmhr,data=data)
kruskal.test(cTG~lmhr,data=data)
kruskal.test(pTGtoHDL~lmhr,data=data)
kruskal.test(cTGtoHDL~lmhr,data=data)

#Linear models
#Can we predict your LDL change? (with your baseline metrics)
m1 <- glm(data$delta_LDL~data$pHDL+data$pTG, family = "gaussian")
summary(m1)
confint(m1)

m2 <- glm(data$delta_LDL~data$pHDL+data$pTG+data$bmi, family = "gaussian")
summary(m2)
confint(m2)

m3 <- glm(data$delta_LDL~data$bmi+data$pTGtoHDL, family = "gaussian")
summary(m3)
confint(m3)

#Can we predict your phenotype? (Using your LDL change and your BMI)
m4 <- glm(data$lmhr~data$bmi+data$delta_LDL, family = "binomial")
summary(m4)
confint(m4)

#Creating heatmap
#BMI vs pre HDL
#Building quantile categories
bmi_quantile <- quantcut(data$bmi,5)
prehdl_quantile <- quantcut(data$pHDL,5)

#Heatmap BMI vs HDL
hmap_df <- aggregate(x=data$delta_LDL,
                  by= list(bmi_quantile,prehdl_quantile),
                  FUN=median,na.rm=TRUE)
colnames(hmap_df) <- c("BMI","HDL","MedianLDLchange")

heatmap_plot <- ggplot(data=hmap_df,aes(x=BMI,y=HDL))+
  geom_tile(aes(fill=MedianLDLchange))+
  ggtitle("LDL change by BMI and HDL before a LCD")+
  scale_y_discrete(name="HDL before LCD (quintile range)",breaks=waiver())+
  scale_x_discrete(name="BMI (quintile range)",breaks=waiver())+
  scale_fill_gradient(name="LDL change",low="gold1",high="green4",n.breaks=10)+
geom_text(aes(BMI,HDL,label=round(MedianLDLchange,1)),color="black",size=4)
heatmap_plot

##Comparing vs NHANES
#Central tendency values of NHANES data
#LDL NHANES
mean(nhanes_data$LBDLDL, na.rm=TRUE)
sd(nhanes_data$LBDLDL, na.rm=TRUE)
median(nhanes_data$LBDLDL, na.rm=TRUE)

#TG NHANES
mean(nhanes_data$LBXTR, na.rm=TRUE)
sd(nhanes_data$LBXTR, na.rm=TRUE)
median(nhanes_data$LBXTR, na.rm=TRUE)

#HDL NHANES
mean(nhanes_data$LBDHDD, na.rm=TRUE)
sd(nhanes_data$LBDHDD, na.rm=TRUE)
median(nhanes_data$LBDHDD, na.rm=TRUE)

#BMI NHANES
mean(nhanes_data$BMXBMI, na.rm=TRUE)
sd(nhanes_data$BMXBMI, na.rm=TRUE)
median(nhanes_data$BMXBMI, na.rm=TRUE)

#Non-LMHR vs NHANES
nhanesvsnonlmhr <- data %>% 
  select(lmhr,pLDL,pTG,pHDL,cLDL,cTG,cHDL,bmi) %>% 
  filter(lmhr==0)

#previous markers
nLMHRpLDLvsNHANES <- t.test(nhanesvsnonlmhr$pLDL,nhanes_data$LBDLDL)
nLMHRpLDLvsNHANES
nLMHRpHDLvsNHANES <- t.test(nhanesvsnonlmhr$pHDL,nhanes_data$LBDHDD)
nLMHRpHDLvsNHANES
nLMHRpTGvsNHANES <- t.test(nhanesvsnonlmhr$pTG,nhanes_data$LBXTR)
nLMHRpTGvsNHANES

#current markers
nLMHRcLDLvsNHANES <- t.test(nhanesvsnonlmhr$cLDL,nhanes_data$LBDLDL)
nLMHRcLDLvsNHANES
nLMHRcHDLvsNHANES <- t.test(nhanesvsnonlmhr$cHDL,nhanes_data$LBDHDD)
nLMHRcHDLvsNHANES
nLMHRcTGvsNHANES <- t.test(nhanesvsnonlmhr$cTG,nhanes_data$LBXTR)
nLMHRcTGvsNHANES

#BMI
nLMHRBMIvsNHANES <- t.test(nhanesvsnonlmhr$bmi,nhanes_data$BMXBMI)
nLMHRBMIvsNHANES

#LMHR vs NHANES
nhanesvslmhr <- data %>% 
  select(lmhr,pLDL,pTG,pHDL,cLDL,cTG,cHDL,bmi) %>% 
  filter(lmhr==1)

#previous markers
lmhrpLDLvsNHANES <- t.test(nhanesvslmhr$pLDL,nhanes_data$LBDLDL)
lmhrpLDLvsNHANES
lmhrpHDLvsNHANES <- t.test(nhanesvslmhr$pHDL,nhanes_data$LBDHDD)
lmhrpHDLvsNHANES
lmhrpTGvsNHANES <- t.test(nhanesvslmhr$pTG,nhanes_data$LBXTR)
lmhrpTGvsNHANES

#current markers
lmhrcLDLvsNHANES <- t.test(nhanesvslmhr$cLDL,nhanes_data$LBDLDL)
lmhrcLDLvsNHANES
lmhrcHDLvsNHANES <- t.test(nhanesvslmhr$cHDL,nhanes_data$LBDHDD)
lmhrcHDLvsNHANES
lmhrcTGvsNHANES <- t.test(nhanesvslmhr$cTG,nhanes_data$LBXTR)
lmhrcTGvsNHANES

#BMI
lmhrBMIvsNHANES <- t.test(nhanesvslmhr$bmi,nhanes_data$BMXBMI)
lmhrBMIvsNHANES

#NHANES comparison Plots
#LDL
nhanes_pLDL <- ggplot(data=data,mapping = aes(x=pLDL,fill=factor(lmhr)))+
  geom_density(inherit.aes = TRUE,alpha=0.3)+
  geom_density(inherit.aes = FALSE, data=nhanes_data,mapping = (aes(x=LBDLDL)))+
  ggtitle("LDL prior to Low-Carb Diet")+
  scale_y_continuous(name="Density",breaks=waiver())+
  scale_x_continuous(name="mg/dL",breaks=waiver())+
  scale_fill_discrete(name = "Phenotype",labels = c("non-LMHR", "LMHR","NHANES 2012"))+
  theme_classic(base_size = 12)
nhanes_pLDL

nhanes_cLDL <- ggplot(data=data,mapping = aes(x=cLDL,fill=factor(lmhr)))+
  geom_density(inherit.aes = TRUE,alpha=0.3)+
  geom_density(inherit.aes = FALSE, data=nhanes_data,mapping = (aes(x=LBDLDL)))+
  ggtitle("LDL current, on Low-Carb Diet")+
  scale_y_continuous(name="Density",breaks=waiver())+
  scale_x_continuous(name="mg/dL",breaks=waiver())+
  scale_fill_discrete(name = "Phenotype",labels = c("non-LMHR", "LMHR","NHANES 2012"))+
  theme_classic(base_size = 12)
nhanes_cLDL

#TRIGS
nhanes_pTG <- ggplot(data=data,mapping = aes(x=pTG,fill=factor(lmhr)))+
  geom_density(inherit.aes = TRUE,alpha=0.3)+
  geom_density(inherit.aes = FALSE, data=nhanes_data,mapping = (aes(x=LBXTR)))+
  ggtitle("Triglycerides prior to Low-Carb Diet")+
  scale_y_continuous(name="Density",breaks=waiver())+
  scale_x_continuous(name="mg/dL",breaks=waiver(),limits = c(0,500))+
  scale_fill_discrete(name = "Phenotype",labels = c("non-LMHR", "LMHR","NHANES 2012"))+
  theme_classic(base_size = 12)
nhanes_pTG

nhanes_cTG <- ggplot(data=data,mapping = aes(x=cTG,fill=factor(lmhr)))+
  geom_density(inherit.aes = TRUE,alpha=0.3)+
  geom_density(inherit.aes = FALSE, data=nhanes_data,mapping = (aes(x=LBXTR)))+
  ggtitle("Triglycerides current, on Low-Carb Diet")+
  scale_y_continuous(name="Density",breaks=waiver())+
  scale_x_continuous(name="mg/dL",breaks=waiver(),limits = c(0,500))+
  scale_fill_discrete(name = "Phenotype",labels = c("non-LMHR", "LMHR","NHANES 2012"))+
  theme_classic(base_size = 12)
nhanes_cTG

#HDL
nhanes_pHDL <- ggplot(data=data,mapping = aes(x=pHDL,fill=factor(lmhr)))+
  geom_density(inherit.aes = TRUE,alpha=0.3)+
  geom_density(inherit.aes = FALSE, data=nhanes_data,mapping = (aes(x=LBDHDD)))+
  ggtitle("HDL prior to Low-Carb Diet")+
  scale_y_continuous(name="Density",breaks=waiver())+
  scale_x_continuous(name="mg/dL",breaks=waiver())+
  scale_fill_discrete(name = "Phenotype",labels = c("non-LMHR", "LMHR","NHANES 2012"))+
  theme_classic(base_size = 12)
nhanes_pHDL

nhanes_cHDL <- ggplot(data=data,mapping = aes(x=cHDL,fill=factor(lmhr)))+
  geom_density(inherit.aes = TRUE,alpha=0.3)+
  geom_density(inherit.aes = FALSE, data=nhanes_data,mapping = (aes(x=LBDHDD)))+
  ggtitle("HDL current, on Low-Carb Diet")+
  scale_y_continuous(name="Density",breaks=waiver())+
  scale_x_continuous(name="mg/dL",breaks=waiver())+
  scale_fill_discrete(name = "Phenotype",labels = c("non-LMHR", "LMHR","NHANES 2012"))+
  theme_classic(base_size = 12)
nhanes_cHDL

nhanes_bmi <- ggplot(data=data,mapping = aes(x=bmi,fill=factor(lmhr)))+
  geom_density(inherit.aes = TRUE,alpha=0.3)+
  geom_density(inherit.aes = FALSE, data=nhanes_data,mapping = (aes(x=BMXBMI)))+
  ggtitle("BMI vs NHANES")+
  scale_y_continuous(name="Density",breaks=waiver())+
  scale_x_continuous(name="kg/m2",breaks=waiver())+
  scale_fill_discrete(name = "Phenotype",labels = c("non-LMHR", "LMHR","NHANES"))+
  theme_classic(base_size = 12)
nhanes_bmi