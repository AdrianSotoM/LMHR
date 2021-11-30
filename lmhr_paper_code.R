# INITIAL STEPS
# Data is available at https://github.com/AdrianSotoM
# Please, address all correspondence about this code to adrian.sotom@incmnsz.mx

###############################################################################

# Setting up working directory. Make sure you choose your own!
setwd("C:/Users/adria/Dropbox/UIEM/Proyectos/LMHR")

###############################################################################

# PACKAGES SETUP

# If you don't have "easystats" yet, you'll need to install it (see below).
# Since "easystats" is not currently available in CRAN, pacman::p_load
# won't work. Therefore, it needs to be installed separately.
# remove the "#" in the line below and press "Enter".

# install.packages("easystats", repos = "https://easystats.r-universe.dev")

# Now, confirm you have "pacman" installed. If you don't have "pacman" remove 
# the "#" in the line below and press "Enter".

# install.packages("pacman") 

# After this, feel free (and we hope you'll want) to go line by line.
# Alternatively, you should now be able to simply "Ctrl/Cmd + Shift + Enter". 
# Running the whole script will take a few minutes and we've set up a sound 
# notification to let you know when it's done in case you want to do
# something else.

# You'll be asked to solve duplicate functions conflicts (see below)
pacman::p_load(dplyr,tidyr,ggstatsplot,readxl,tableone,car,easystats,
               patchwork,MASS,see,qqplotr,bootStepAIC,performance,
               rpart,rpart.plot,gtools,broom,lmtest,visdat,parameters,
               ggcharts,conflicted,RNHANES,rattle,mlogit,MLmetrics)

## Solving duplicate functions conflicts
conflict_prefer("select","dplyr")
conflict_prefer("filter", "dplyr")

###############################################################################

## That's it! Just press "Ctrl/Cmd + Shift + Enter" to get all plots and results!

###############################################################################

# DATA UPLOAD.
## Cholesterol Super Survey raw data
data <- read_excel("lmhrpaper_data.xlsx")

###############################################################################

# DATA FILTERING
## Please, note filters will overlap in most excluded cases (they won't add up).

# How many and why people will be excluded? (FIGURE 1)
## Because they were on lipid lowering medications.
sum(data$lipid_low_rx==0, na.rm = TRUE)
## Because the days between their lab tests were less than 30.
sum(data$days_btwn_tests<28, na.rm = TRUE)
## Because the days between their lab tests were more than five years.
sum(data$days_btwn_tests>1825, na.rm = TRUE)
## Because an unreliable carbohydrate intake report.
sum(data$net_carbs<0, na.rm = TRUE)
## Because an unreliable carbohydrate intake report.
sum(data$net_carbs> 130, na.rm = TRUE) 
## Because they were younger than 18 years old.
sum(data$age<18, na.rm = TRUE) 
## Because they were older than 100 years old.
sum(data$age>100, na.rm = TRUE) 
## Because an unreliable triglyceride result.
sum(data$pTG<20, na.rm = TRUE) 
## Because an unreliable triglyceride result.
sum(data$pTG>1500, na.rm = TRUE) 
## Because an unreliable triglyceride result.
sum(data$cTG<20, na.rm = TRUE)
## Because an unreliable triglyceride result.
sum(data$cTG>1500, na.rm = TRUE)
## Because an unreliable LDL result.
sum(data$pLDL<30, na.rm = TRUE)
## Because an unreliable LDL result.
sum(data$pLDL>1000, na.rm = TRUE)
## Because an unreliable LDL result.
sum(data$cLDL<30, na.rm = TRUE)
## Because an unreliable LDL result.
sum(data$cLDL>1000, na.rm = TRUE)
## Because an unreliable BMI report.
sum(data$bmi<10, na.rm = TRUE)
## Because an unreliable BMI report.
sum(data$bmi>50, na.rm = TRUE)
## Because an unreliable HDL result.
sum(data$cHDL<10, na.rm = TRUE)
## Because an unreliable HDL result.
sum(data$cHDL>200, na.rm = TRUE)
## Because an unreliable HDL result.
sum(data$pHDL<10, na.rm = TRUE)
## Because an unreliable HDL result.
sum(data$pHDL>200, na.rm = TRUE)


# Implementing filtering criteria
data <- data %>% 
  dplyr::select(age,lmhr,gender,net_carbs,pTC,cTC,lipid_low_rx,days_btwn_tests,
                cTG,cLDL,cHDL,pLDL,pHDL,pTG,bmi) %>%   
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
## Creating response variables
      mutate(delta_LDL= cLDL-pLDL) %>% 
      mutate(delta_HDL= cHDL-pHDL) %>% 
      mutate(delta_TG= cTG-pTG) %>% 
      mutate(cTGtoHDL= cTG/cHDL) %>% 
      mutate(pTGtoHDL= pTG/pHDL) %>% 
      mutate(deltaTGtoHDL=cTGtoHDL-pTGtoHDL) %>%
# Formatting decimals
      mutate(across(where(is.numeric), round, 2)) %>% 
# Removing duplicates
  distinct()

###############################################################################

# DESCRIPTIVE STATISTICS
## Percentile distributions
### Age
quantile(data$age, probs = c(0.05,0.25,0.5,0.75,0.95))
### Carbohydrate intake
quantile(data$net_carbs, probs = c(0.05,0.25,0.5,0.75,0.95))
### BMI
quantile(data$bmi, probs = c(0.05,0.25,0.5,0.75,0.95))
### Current LDL
quantile(data$cLDL, probs = c(0.05,0.25,0.5,0.75,0.95))
### Current HDL
quantile(data$cHDL, probs = c(0.05,0.25,0.5,0.75,0.95))
### Current Triglycerides
quantile(data$cTG, probs = c(0.05,0.25,0.5,0.75,0.95))
### Current Triglycerides/HDL ratio.
quantile(data$cTGtoHDL, probs = c(0.05,0.25,0.5,0.75,0.95))
### LDL before a Low Carbohydrate Diet.
quantile(data$pLDL, probs = c(0.05,0.25,0.5,0.75,0.95))
### HDL before a Low Carbohydrate Diet.
quantile(data$pHDL, probs = c(0.05,0.25,0.5,0.75,0.95))
### Triglycerides before a Low Carbohydrate Diet.
quantile(data$pTG, probs = c(0.05,0.25,0.5,0.75,0.95))
### Triglycerides/HDL ratio before a Low Carbohydrate Diet.
quantile(data$pTGtoHDL, probs = c(0.05,0.25,0.5,0.75,0.95))
### Change in LDL after a Low Carbohydrate Diet.
quantile(data$delta_LDL, probs = c(0.05,0.25,0.5,0.75,0.95))
### Change in HDL after a Low Carbohydrate Diet.
quantile(data$delta_HDL, probs = c(0.05,0.25,0.5,0.75,0.95))
### Change in Triglycerides after a Low Carbohydrate Diet.
quantile(data$delta_TG, probs = c(0.05,0.25,0.5,0.75,0.95))
### Change in Triglycerides/HDL ratio after a Low Carbohydrate Diet.
quantile(data$deltaTGtoHDL, probs = c(0.05,0.25,0.5,0.75,0.95))

###############################################################################

# Table 1
table_all <- CreateContTable(data=data, vars = c("age",
"cTG","cLDL","cHDL","pLDL","pHDL","pTG","bmi","delta_LDL","delta_HDL",
"delta_TG","cTGtoHDL","pTGtoHDL","deltaTGtoHDL"))

summary(table_all)
table_all_sex <- CreateCatTable(data=data, vars = c("gender"))
summary(table_all_sex)

###############################################################################

# EXPLORATORY ANALYSES

# Due to the exploratory purpose of our study, and to the transverse origin 
# of the data, we decided to used all available data for the linear models 
# and no train-test split was considered. 

# Therefore, the predictive accuracy of the models still needs to be validated 
# in prospective studies and no clinical decisions should be based on the
# predictions of these models.

## Predicting LDL change with linear models. (SUPPLEMENTAL TABLE 1)
### We filtered out the "hypothesis related variables" (i.e. "LMHR" category).
datamodels <- data %>% dplyr::select(age,gender,bmi,cTG,cHDL,pLDL,pHDL,pTG,
                                     pTGtoHDL,cTGtoHDL,delta_HDL,delta_LDL,
                                     delta_TG,deltaTGtoHDL)

### Model 0. Hypothesis-naive (all-variables in the data set).
m0 <- lm(delta_LDL~.,data=datamodels)
summary(m0)
confint(m0)

### Evaluating how consistent is the relevance of each potential predictor.
m0_step <- stepAIC(m0,direction = "backward", trace = FALSE)
m0_step

m0_boot <- boot.stepAIC(m0,datamodels,B=100, )
m0_boot

###############################################################################

# EVALUATING CANDIDATE MODELS
## (SUPPLEMENTAL TABLE 2)
### Model 1. Predicting LDL changes with Triglycerides and HDL.
m1 <- lm(delta_LDL~pHDL+pTG,data = datamodels)
summary(m1)
confint(m1)

### Model 2. Predicting LDL changes with Triglycerides/HDL ratio.
m2 <- lm(delta_LDL~pTGtoHDL,data = datamodels)
summary(m2)
confint(m2)

### Model 3. Predicting LDL changes with Triglycerides/HDL ratio and BMI.
m3 <- lm(delta_LDL~pTGtoHDL+bmi,data = datamodels)
summary(m3)
confint(m3)

## LMHR can be seen or defined as a linear model for predicting someone is lean,
## given their lipid markers.
## Predicting BMI with lipid markers
## Please note this model is included in Main Table 2 as m4.
lmhrlm <- lm(bmi~pLDL+pHDL+pTG,data = datamodels)
summary(lmhrlm)
confint(lmhrlm)

lmhrlm_perf <- compare_performance(lmhrlm)
lmhrlm_perf

###############################################################################

# HEATMAPS
## To represent LDL change more clearly, values from this heatmap were used to 
## create the heat/3D bar chartS that appears in the paper.

## LDL change given PRIOR BMI & Trig/HDL. (FIGURE 2)
### Building quantile categories
bmi_quantile <- quantcut(data$bmi,4)
pTGtoHDL_quantile <- quantcut(data$pTGtoHDL,4)

### Building heatmap dataframe
hmap_df <- aggregate(x=data$delta_LDL,
                     by= list(bmi_quantile,pTGtoHDL_quantile),
                     FUN=median,na.rm=TRUE)
colnames(hmap_df) <- c("BMIquant","pTGtoHDL_quantile","MedianLDLchange")

### Heatmap plot
heatmap_plot1 <- ggplot(data=hmap_df,aes(x=BMIquant,y=pTGtoHDL_quantile))+
  geom_tile(aes(fill=MedianLDLchange))+
  ggtitle("LDL change by BMI and prior TG/HDL before a LCD")+
  scale_y_discrete(name="Prior TG/HDL before a LCD (quartile range)",
                   breaks=waiver())+
  scale_x_discrete(name="BMI (quartile range)",breaks=waiver())+
  scale_fill_gradient(name="Median LDL change",low="#DB4437",high="#4285F4",
                      n.breaks=10)+
  geom_text(aes(BMIquant,pTGtoHDL_quantile,label=round(MedianLDLchange,1)),
            color="black",size=4)+
  ggplot2::theme(text = element_text(size = 12),
                 plot.title = element_text(size=20,face = "bold"),
                 axis.title.x = element_text(size=16, colour = "black"),
                 axis.title.y = element_text(size=16, colour = "black")
  )
ggsave("heatmap_plot1.tiff", units="cm", width=24, height=24, dpi=500, 
       compression = 'lzw')
heatmap_plot1

## LDL change given BMI & Trig/HDL ON A LOW CARB DIET. (SUPPLEMENTAL FIGURE 1)
## Building quantile categories
bmi_quantile <- quantcut(data$bmi,4)
cTGtoHDL_quantile <- quantcut(data$cTGtoHDL,4)

## Building heatmap dataframe
hmap_df <- aggregate(x=data$delta_LDL,
                     by= list(bmi_quantile,cTGtoHDL_quantile),
                     FUN=median,na.rm=TRUE)
colnames(hmap_df) <- c("BMIquant","cTGtoHDL_quantile","MedianLDLchange")

## Heatmap plot
heatmap_plot2 <- ggplot(data=hmap_df,aes(x=BMIquant,y=cTGtoHDL_quantile))+
  geom_tile(aes(fill=MedianLDLchange))+
  ggtitle("LDL change by BMI and current TG/HDL before a LCD")+
  scale_y_discrete(name="Current TG/HDL before a LCD (quartile range)",
                   breaks=waiver())+
  scale_x_discrete(name="BMI (quartile range)",breaks=waiver())+
  scale_fill_gradient(name="Median LDL change",low="#DB4437",high="#4285F4",
                      n.breaks=10)+
  geom_text(aes(BMIquant,cTGtoHDL_quantile,label=round(MedianLDLchange,1)),
            color="black",size=4)+
  ggplot2::theme(text = element_text(size = 12),
                 plot.title = element_text(size=20,face = "bold"),
                 axis.title.x = element_text(size=16, colour = "black"),
                 axis.title.y = element_text(size=16, colour = "black")
  )
ggsave("heatmap_plot2.tiff", units="cm", width=24, height=24, dpi=500, 
       compression = 'lzw')
heatmap_plot2

###############################################################################

# Comparing all models. (SUPPLEMENTAL FIGURE 2)
## To show BMI does not tell the whole story, model 5 showd predicting LDLc 
## changes with BMI alone.
m5 <- lm(delta_LDL~bmi, data=data)
summary(m5)
confint(m5)

comp <- compare_performance(m1,m2,m3,m5)
comp
plot(comp)


### Model 3 was the best model we found and we checked its linear assumptions.
check_model(m3)

###############################################################################

# RANDOM REGRESSION TREES (SUPPLEMENTAL FIGURE 3)
tree_map1 <- rpart(formula = delta_LDL ~ gender + bmi + pTG + pHDL + pTGtoHDL,
                   data= data, method = 'anova')

rpart.plot(tree_map1,cex=0.7,box.palette = "RdBu")

###############################################################################

# COMPARING LMHR CATEGORIES

## (SUPPLEMENTAL Table 3)
table_lmhr <- CreateContTable(data=data,strata="lmhr", vars = c("age","bmi",
"net_carbs","cTC","pTC","pLDL","cLDL","delta_LDL","pHDL","cHDL","delta_HDL",
"pTG","cTG","delta_TG","pTGtoHDL","cTGtoHDL","deltaTGtoHDL"))
summary(table_lmhr)

table_lmhr_sex <- CreateCatTable(data=data,strata="lmhr", vars = c("gender"))
summary(table_lmhr_sex)

# Hypothesis tests.
## LMHRs are leaner than LHMR
wilcox.test(bmi~lmhr,data=data)

## But not older
wilcox.test(age~lmhr,data=data)

## LMHR's LDL and HDL changes more than non LMHR's
wilcox.test(delta_LDL~lmhr,data=data)
wilcox.test(delta_HDL~lmhr,data=data)
wilcox.test(delta_TG~lmhr,data=data)
wilcox.test(deltaTGtoHDL~lmhr,data=data)

## LMHR vs non-LMHR before and after comparisons
# LDL is not different before a LCD
wilcox.test(pLDL~lmhr,data=data)

# But is after a LCD
wilcox.test(cLDL~lmhr,data=data)

# HDL was different and LCD exacerbated those differences.
wilcox.test(pHDL~lmhr,data=data)
wilcox.test(cHDL~lmhr,data=data)

# Triglycerides were different and LCD exacerbated those differences.
wilcox.test(pTG~lmhr,data=data)
wilcox.test(cTG~lmhr,data=data)

# Trig/HDL ratio was different and LCD exacerbated those differences.
wilcox.test(pTGtoHDL~lmhr,data=data)
wilcox.test(cTGtoHDL~lmhr,data=data)

###############################################################################

# Comparing LMHRs vs NHANES. 

## Getting NHANES' data
### LDL (in mg/dL) is encoded as "LBLDL" and Triglycerides (in mg/dL) as "LBXTR"
nhanes1 <-  nhanes_load_data("TRIGLY_G", "2011-2012")
### HDL (in mg/dL) is encoded as "LBDHDD"
nhanes2 <- nhanes_load_data("HDL_G","2011-2012")
### BMI (kg/m2) is encoded as "BMXBMI"
nhanes3 <- nhanes_load_data("BMX_G","2011-2012")
### AGE (in years) is encoded as "RIDAGEYR"
nhanes4 <- nhanes_load_data("DEMO_G","2011-2012")

### Data years and encoding can be corroborated at: 
### https://wwwn.cdc.gov/nchs/nhanes/search/default.aspx

## Merging NHANES' data sets and excluding children
nhanes_lipids <- merge(nhanes1,nhanes2,by="SEQN")
nhanes_demo <- merge(nhanes3,nhanes4,by="SEQN") %>% filter(RIDAGEYR>17)
nhanes_data <- merge(nhanes_lipids,nhanes_demo,by="SEQN")

## Descriptive statistics of NHANES data
## LDL in NHANES
mean(nhanes_data$LBDLDL, na.rm=TRUE)
sd(nhanes_data$LBDLDL, na.rm=TRUE)
median(nhanes_data$LBDLDL, na.rm=TRUE)

## Triglycerides in NHANES
mean(nhanes_data$LBXTR, na.rm=TRUE)
sd(nhanes_data$LBXTR, na.rm=TRUE)
median(nhanes_data$LBXTR, na.rm=TRUE)

## HDL in NHANES
mean(nhanes_data$LBDHDD, na.rm=TRUE)
sd(nhanes_data$LBDHDD, na.rm=TRUE)
median(nhanes_data$LBDHDD, na.rm=TRUE)

## BMI in NHANES
mean(nhanes_data$BMXBMI, na.rm=TRUE)
sd(nhanes_data$BMXBMI, na.rm=TRUE)
median(nhanes_data$BMXBMI, na.rm=TRUE)

###############################################################################

# NHANES comparison Plots (FIGURE 3)

## BMI
nhanes_bmi <- ggplot(data=data,mapping = aes(x=bmi,fill=factor(lmhr)))+
  geom_density(inherit.aes = TRUE,alpha=0.3)+
  geom_density(inherit.aes = FALSE, data=nhanes_data,mapping = (aes(x=BMXBMI))
               ,alpha=0.3)+
  ggtitle("BMI vs NHANES")+
  scale_y_continuous(name="Density",breaks=waiver())+
  scale_x_continuous(name="kg/m2",breaks=waiver(),limits = c(10,60))+
  scale_fill_discrete(name = "Phenotype",labels = c("LMHR","non-LMHR"),
                      type = c("#4285F4","#DB4437"))+
  theme_classic(base_size = 10)
ggsave("nhanes_bmi.tiff", units="cm", width=24, height=24, dpi=500, 
       compression = 'lzw')
nhanes_bmi

## Previous LDL
nhanes_pLDL <- ggplot(data=data,mapping = aes(x=pLDL,fill=factor(lmhr)))+
  geom_density(inherit.aes = TRUE,alpha=0.3)+
  geom_density(inherit.aes = FALSE, data=nhanes_data,mapping = (aes(x=LBDLDL))
               ,alpha=0.3)+
  ggtitle("LDL prior to Low-Carb Diet")+
  scale_y_continuous(name="Density",breaks=waiver())+
  scale_x_continuous(name="mg/dL",breaks=waiver(),limits = c(20,500))+
  scale_fill_discrete(name = "Phenotype",labels = c("LMHR","non-LMHR"),
                      type = c("#4285F4","#DB4437"))+
  theme_classic(base_size = 10)
ggsave("nhanes_pLDL.tiff", units="cm", width=24, height=24, dpi=500, 
       compression = 'lzw')
nhanes_pLDL

## Current LDL
nhanes_cLDL <- ggplot(data=data,mapping = aes(x=cLDL,fill=factor(lmhr)))+
  geom_density(inherit.aes = TRUE,alpha=0.3)+
  geom_density(inherit.aes = FALSE, data=nhanes_data,mapping = (aes(x=LBDLDL))
               ,alpha=0.3)+
  ggtitle("LDL current, on Low-Carb Diet")+
  scale_y_continuous(name="Density",breaks=waiver())+
  scale_x_continuous(name="mg/dL",breaks=waiver(),limits = c(20,500))+
  scale_fill_discrete(name = "Phenotype",labels = c("LMHR","non-LMHR"),
                      type = c("#4285F4","#DB4437"))+
  theme_classic(base_size = 10)
ggsave("nhanes_cLDL.tiff", units="cm", width=24, height=24, dpi=500, 
       compression = 'lzw')
nhanes_cLDL

## Previous Triglycerides
nhanes_pTG <- ggplot(data=data,mapping = aes(x=pTG,fill=factor(lmhr)))+
  geom_density(inherit.aes = TRUE,alpha=0.3)+
  geom_density(inherit.aes = FALSE, data=nhanes_data,mapping = (aes(x=LBXTR))
               ,alpha=0.3)+
  ggtitle("Triglycerides prior to Low-Carb Diet")+
  scale_y_continuous(name="Density",breaks=waiver())+
  scale_x_continuous(name="mg/dL",breaks=waiver(),limits = c(0,500))+
  scale_fill_discrete(name = "Phenotype",labels = c("LMHR","non-LMHR"),
                      type = c("#4285F4","#DB4437"))+
  theme_classic(base_size = 10)
ggsave("nhanes_pTG.tiff", units="cm", width=24, height=24, dpi=500, 
       compression = 'lzw')
nhanes_pTG

## Current Triglycerides
nhanes_cTG <- ggplot(data=data,mapping = aes(x=cTG,fill=factor(lmhr)))+
  geom_density(inherit.aes = TRUE,alpha=0.3)+
  geom_density(inherit.aes = FALSE, data=nhanes_data,mapping = (aes(x=LBXTR))
               ,alpha=0.3)+
  ggtitle("Triglycerides current, on Low-Carb Diet")+
  scale_y_continuous(name="Density",breaks=waiver())+
  scale_x_continuous(name="mg/dL",breaks=waiver(),limits = c(0,500))+
  scale_fill_discrete(name = "Phenotype",labels = c("LMHR","non-LMHR"),
                      type = c("#4285F4","#DB4437"))+
  theme_classic(base_size = 10)
ggsave("nhanes_cTG.tiff", units="cm", width=24, height=24, dpi=500, 
       compression = 'lzw')
nhanes_cTG

## Previous HDL
nhanes_pHDL <- ggplot(data=data,mapping = aes(x=pHDL,fill=factor(lmhr)))+
  geom_density(inherit.aes = TRUE,alpha=0.3)+
  geom_density(inherit.aes = FALSE, data=nhanes_data,mapping = (aes(x=LBDHDD))
               ,alpha=0.3)+
  ggtitle("HDL prior to Low-Carb Diet")+
  scale_y_continuous(name="Density",breaks=waiver())+
  scale_x_continuous(name="mg/dL",breaks=waiver(),limits = c(20,200))+
  scale_fill_discrete(name = "Phenotype",labels = c("LMHR","non-LMHR"),
                      type = c("#4285F4","#DB4437"))+
  theme_classic(base_size = 10)
ggsave("nhanes_pHDL.tiff", units="cm", width=24, height=24, dpi=500, 
       compression = 'lzw')
nhanes_pHDL

## Current HDL
nhanes_cHDL <- ggplot(data=data,mapping = aes(x=cHDL,fill=factor(lmhr)))+
  geom_density(inherit.aes = TRUE,alpha=0.3)+
  geom_density(inherit.aes = FALSE, data=nhanes_data,mapping = (aes(x=LBDHDD))
               ,alpha=0.3)+
  ggtitle("HDL current, on Low-Carb Diet")+
  scale_y_continuous(name="Density",breaks=waiver())+
  scale_x_continuous(name="mg/dL",breaks=waiver(),limits = c(20,200))+
  scale_fill_discrete(name = "Phenotype",labels = c("LMHR","non-LMHR"),
                      type = c("#4285F4","#DB4437"))+
  theme_classic(base_size = 10)
ggsave("nhanes_cHDL.tiff", units="cm", width=24, height=24, dpi=500, 
       compression = 'lzw')
nhanes_cHDL

## Compiling panels
panel1 <- nhanes_pLDL+nhanes_cLDL+nhanes_pHDL+nhanes_cHDL+nhanes_pTG+nhanes_cTG+
  plot_layout(ncol=2,nrow = 3)
ggsave("panel1.tiff", units="in", width=20, height=24, dpi=500, 
       compression = 'lzw')
panel1

###############################################################################

## BMI (SUPPLEMENTAL FIGURE 4)
bmi_plot <- ggbetweenstats(
  data=data,x=lmhr,y=bmi,type="np",
  xlab = "Phenotype", ylab = "BMI (kg/m^2)",
  outlier.tagging = TRUE,outlier.coef = 1.5,outlier.label = NULL,
  outlier.label.args = list(color = "darkgray"),
  ggtheme = ggplot2::theme_classic(),
  package = "yarrr",
  palette = "google", 
  title = "BMI by phenotype",
  caption = NULL) + # modifying the plot further
  ggplot2::scale_y_continuous(
    limits = c(10,45),
    breaks = seq(0, 45, by= 5))+
  ggplot2::theme(text = element_text(size = 16),
                 plot.title = element_text(size=24,face = "bold"),
                 axis.title.x = element_blank(),
                 axis.title.y = element_text(size=14, colour = "gray40"))
ggsave("bmi_plot.tiff", units="in", width=10, height=10, dpi=1000, 
       compression = 'lzw')
bmi_plot

###############################################################################

# LETTING YOU KNOW IT FINISHED!
pacman::p_load(beepr)
beep(8)
