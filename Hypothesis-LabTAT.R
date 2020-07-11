setwd("C:\\Users\\Minal\\Downloads\\assignments\\hypothesis T")


library(readxl)
#library(xlsx) 
library(openxlsx)
library(WriteXLS)

getwd()
LabTAT<-read.csv("LabTAT.csv")
View(LabTAT)
#Normality test
boxplot(LabTAT$Laboratory.1, LabTAT$Laboratory.2,LabTAT$Laboratory.3,LabTAT$Laboratory.4)
#Anova Test
Stacked_Group <- stack(LabTAT)
View(Stacked_Group)
plot(Stacked_Group$ind,Stacked_Group$values)
var.test(lm(Stacked_Group$ind ~ 1), lm(Stacked_Group$values ~ 1))
#var.test(Stacked_Group$values, Stacked_Group$ind)
Anova_results <- aov(values~ind,data = Stacked_Group)
summary(Anova_results)
# p-value = 0.00002 < 0.05 


#attach(LabTAT)

#colnames(LabTAT)


