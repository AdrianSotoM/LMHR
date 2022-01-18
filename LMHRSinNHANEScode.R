library(RNHANES)
library(dplyr)

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
###https://wwwn.cdc.gov/nchs/nhanes/search/default.aspx

## Merging NHANES' data sets and excluding children
nhanes_lipids <- merge(nhanes1,nhanes2,by="SEQN")
nhanes_demo <- merge(nhanes3,nhanes4,by="SEQN") %>% filter(RIDAGEYR>17)
nhanes_data <- merge(nhanes_lipids,nhanes_demo,by="SEQN")

### Individual criteria
sum(nhanes_data$LBDLDL>199, na.rm = TRUE)
sum(nhanes_data$LBDHDD>79, na.rm = TRUE)
sum(nhanes_data$LBXTR<71, na.rm = TRUE)

### All criteria
nhanes_data <- nhanes_data %>% filter(LBDLDL>199 & LBDHDD>79 & LBXTR<71)

sum(nhanes_data$LBDLDL>199, na.rm = TRUE)
sum(nhanes_data$LBDHDD>79, na.rm = TRUE)
sum(nhanes_data$LBXTR<71, na.rm = TRUE)
