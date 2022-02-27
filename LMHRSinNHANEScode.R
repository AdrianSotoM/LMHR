setwd("C:/Users/adria/Dropbox/UIEM/LEAD/Proyectos/LMHR/NHANES")
pacman::p_load(dplyr,rio,rquery)

rm(list=ls())

localDirectoryPath<- getwd()
filesDirectoryPath<- paste0(localDirectoryPath,'/files')

varList <- list(
  'LBDHDL' = 'Lab13.XPT',
  'LBXTC' = 'Lab13.XPT',
  'LBDLDL' = 'LAB13AM.XPT',
  'LBXTR' = 'LAB13AM.XPT',
  'LBXSCH' = 'LAB18.XPT',
  'LBXSTR' = 'LAB18.XPT',
  'LBDLDL' = 'L13AM_B.XPT',
  'LBXTR' = 'L13AM_B.XPT',
  'LB2HDL' = 'l13_2_b.XPT',
  'LB2LDL' = 'l13_2_b.XPT',
  'LB2TC' = 'l13_2_b.XPT',
  'LB2TR' = 'l13_2_b.XPT',
  'LBDHDL' = 'l13_b.XPT',
  'LBXTC' = 'l13_b.XPT',
  'LB2SCH' = 'L40_2_B.XPT',
  'LB2STR' = 'L40_2_B.XPT',
  'LBXSCH' = 'L40_B.XPT',
  'LBXSTR' = 'L40_B.XPT',
  'LBDLDL' = 'L13AM_C.XPT',
  'LBXTR' = 'L13AM_C.XPT',
  'LBXHDD' = 'l13_c.XPT',
  'LBXTC' = 'l13_c.XPT',
  'LBXSCH' = 'L40_C.XPT',
  'LBXSTR' = 'L40_C.XPT',
  'LBXSCH' = 'BIOPRO_D.XPT',
  'LBXSTR' = 'BIOPRO_D.XPT',
  'LBDHDD' = 'HDL_D.XPT',
  'LBXTC' = 'TCHOL_D.XPT',
  'LBDLDL' = 'TRIGLY_D.XPT',
  'LBXTR' = 'TRIGLY_D.XPT',
  'LBXSCH' = 'BIOPRO_E.XPT',
  'LBXSTR' = 'BIOPRO_E.XPT',
  'LBDHDD' = 'HDL_E.XPT',
  'LBXTC' = 'TCHOL_E.XPT',
  'LBDLDL' = 'TRIGLY_E.XPT',
  'LBXTR' = 'TRIGLY_E.XPT',
  'LBXSCH' = 'BIOPRO_F.XPT',
  'LBXSTR' = 'BIOPRO_F.XPT',
  'LBDHDD' = 'HDL_F.XPT',
  'LBXTC' = 'TCHOL_F.XPT',
  'LBDLDL' = 'TRIGLY_F.XPT',
  'LBXTR' = 'TRIGLY_F.XPT',
  'LBXSCH' = 'BIOPRO_G.XPT',
  'LBXSTR' = 'BIOPRO_G.XPT',
  'LBDHDD' = 'HDL_G.XPT',
  'LBXTC' = 'TCHOL_G.XPT',
  'LBDLDL' = 'TRIGLY_G.XPT',
  'LBXTR' = 'TRIGLY_G.XPT',
  'LBXSCH' = 'BIOPRO_H.XPT',
  'LBXSTR' = 'BIOPRO_H.XPT',
  'LBDHDD' = 'HDL_H.XPT',
  'LBXTC' = 'TCHOL_H.XPT',
  'LBDLDL' = 'TRIGLY_H.XPT',
  'LBXTR' = 'TRIGLY_H.XPT',
  'LBXSCH' = 'BIOPRO_H.XPT',
  'LBXSTR' = 'BIOPRO_H.XPT',
  'LBXSCH' = 'BIOPRO_I.XPT',
  'LBXSTR' = 'BIOPRO_I.XPT',
  'LBDHDD' = 'HDL_I.XPT',
  'LBXTC' = 'TCHOL_I.XPT',
  'LBDLDL' = 'TRIGLY_I.XPT',
  'LBXTR' = 'TRIGLY_I.XPT',
  'LBXSCH' = 'BIOPRO_J.XPT',
  'LBXSTR' = 'BIOPRO_J.XPT',
  'LBDHDD' = 'HDL_J.XPT',
  'LBXTC' = 'TCHOL_J.XPT',
  'LBDLDLM' = 'TRIGLY_J.XPT',
  'LBDLDL' = 'TRIGLY_J.XPT',
  'LBXTR' = 'TRIGLY_J.XPT',
  'LBDHDD' = 'P_HDL.XPT',
  'LBXTC' = 'P_TCHOL.XPT',
  'LBDLDLM' = 'P_TRIGLY.XPT',
  'LBDLDL' = 'P_TRIGLY.XPT',
  'LBXTR' = 'P_TRIGLY.XPT'
)
​
fileDf <-NULL
​
for(varInd in 1:length(varList)){
  varName = names(varList)[varInd]
  fileName = varList[varInd][[1]]
  print(cat(varName,' = ',fileName))
  
  raw <- import(fileName, format = "xpt")
  
  extractedDF <- data.frame(raw[,c('SEQN',varName)])
  
  if(is.null(fileDf)){
    fileDf <- extractedDF
  }else{
    fileDf <- natural_join(fileDf, extractedDF, 
                           by = "SEQN",
                           jointype = "FULL")
    
  }
}
​
write.csv(fileDf,"data", row.names = FALSE)
​
data <- read.csv("data")
colnames(data) <- c("id","hdl1","ldl1","chol1","tg1","chol","tg2","hdl2","hdl3",
                "ldl2","ldl3","hdl4","chol2","tg3","chol3","tg4")
​
### Patients meeting individual criteria
sum(data$ldl1>199|data$ldl2>199|data$ldl3>199, na.rm = TRUE)
sum(data$hdl1>79|data$hdl2>79|data$hdl3>79|data$hdl4>79, na.rm = TRUE)
sum(data$tg1<71|data$tg2<71|data$tg3<71|data$tg4<71, na.rm = TRUE)
​
### Patients meeting all lipid criteria
data <- data %>% filter(data$ldl1>199|data$ldl2>199|data$ldl3>199)
data <- data %>% filter(data$hdl1>79|data$hdl2>79|data$hdl3>79|data$hdl4>79)
data <- data %>% filter(data$tg1<71|data$tg2<71|data$tg3<71|data$tg4<71)
