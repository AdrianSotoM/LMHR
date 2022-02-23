###### SET UP #####
#Directory set up
setwd("C:/Users/adria/Dropbox/UIEM/LEAD/Proyectos/LMHR")

#Package load
pacman::p_load(dplyr,RNHANES,readr)

# Getting NHANES' data
hdl1 <-  nhanes_load_data("HDL_D","2005-2006")
hdl2 <- nhanes_load_data("HDL_E","2007-2008")
hdl3 <- nhanes_load_data("HDL_F","2009-2010")
hdl4 <- nhanes_load_data("HDL_G","2011-2012")
hdl5 <- nhanes_load_data("HDL_H","2013-2014")
ldl1 <-  nhanes_load_data("TCHOL_D","2005-2006")
ldl2 <- nhanes_load_data("TCHOL_E","2007-2008")
ldl3 <- nhanes_load_data("TCHOL_F","2009-2010")
ldl4 <- nhanes_load_data("TCHOL_G","2011-2012")
ldl5 <- nhanes_load_data("TCHOL_H","2013-2014")
tg1 <-  nhanes_load_data("TRIGLY_D","2005-2006")
tg2 <- nhanes_load_data("TRIGLY_E","2007-2008")
tg3 <- nhanes_load_data("TRIGLY_F","2009-2010")
tg4 <- nhanes_load_data("TRIGLY_G","2011-2012")
tg5 <- nhanes_load_data("TRIGLY_H","2013-2014")

### Merging datasets
hdl <- bind_rows(hdl1,hdl2,hdl3,hdl4,hdl5)
ldl <- bind_rows(ldl1,ldl2,ldl3,ldl4,ldl5)
tg <- bind_rows(tg1,tg2,tg3,tg4,tg5)

nhanes <- merge(hdl,ldl,by="SEQN")
nhanes <- merge(nhanes,tg,by="SEQN")

### Individual criteria
sum(nhanes$LBDLDL>199, na.rm = TRUE)
sum(nhanes$LBDHDD>79, na.rm = TRUE)
sum(nhanes$LBXTR<71, na.rm = TRUE)

### All lipid criteria
nhanes <- nhanes %>% filter(LBDLDL>199 & LBDHDD>79 & LBXTR<71)

sum(nhanes$LBDLDL>199, na.rm = TRUE)
sum(nhanes$LBDHDD>79, na.rm = TRUE)
sum(nhanes$LBXTR<71, na.rm = TRUE)
