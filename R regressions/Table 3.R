library(foreign)
library(plm)

#Table 3 - Program Impact on Child Mortality
data <- read.csv("AEJ_child_mortality.csv")
#####Number of deaths

#Under -5 deaths
model <- plm(death_u5~treatment, 
             data = data,
             index = c("branchid"),
             model = "within")
summary(model)
#Infant deaths
model_u1 <- plm(death_u1~treatment, 
             data = data,
             index = c("branchid"),
             model = "within")
summary(model_u1)
#Neonatal deaths
model_u1m <- plm(death_u1m~treatment, 
                data = data,
                index = c("branchid"),
                model = "within")
summary(model_u1m)

