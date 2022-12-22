setwd("D:/")

library(readxl)
regression <- read_excel("D:/regression2.xlsx")
View(regression)

library(manipulate)
library(olsrr)

# 1. Detecting multicollinearity using correlation matrix
cor(regression, method = "pearson")


# 2&3. Detecting multicollinearity using Tolerance value and variance inflation factor
# Full model

Binary <- glm(regression$LSTBinary~ regression$RVI +regression$IPVI+ regression$DVI+
                regression$NDVI + regression$NDWI
              +regression$NDMI,
              data = regression, family = "binomial")
summary(Binary)

ols_regress(regression$LSTBinary~ regression$RVI+regression$IPVI+ regression$DVI+
                       regression$NDVI + regression$NDWI+regression$NDMI,
                     data = regression)

#Model 1 USing RVI as dependent variable (auxillary regression) to check multicollinearity 

ols_regress(regression$RVI~regression$IPVI+ regression$DVI+
              regression$NDVI + regression$NDWI+regression$NDMI,
            data = regression)
# Tolerance formula is (1-R2)
(1-0.199)
#Variance Inflation Factor
(1/(1-0.199))

#Model 1 USing IPVI as dependent variable (auxillary regression) to check multicollinearity 

ols_regress(regression$IPVI~ regression$RVI+ regression$DVI+
              regression$NDVI + regression$NDWI+regression$NDMI,
            data = regression)
# Tolerance formula is (1-R2) if tolerance value is less than 0.1, there is a multicollinearity
(1-1)
#Variance Inflation Factor (1/(1-R2)) if VIF is more than 10, there is a multicollinearity
(1/(1-1))

#Model 1 USing DVI as dependent variable (auxillary regression) to check multicollinearity 

ols_regress(regression$DVI ~regression$IPVI+ regression$RVI+
              regression$NDVI + regression$NDWI+regression$NDMI,
            data = regression)
# Tolerance formula is (1-R2)
(1- 0.966)
#Variance Inflation Factor
(1/(1- 0.966))

#Model 1 USing NDVI as dependent variable (auxillary regression) to check multicollinearity 

ols_regress(regression$NDVI ~regression$DVI + regression$IPVI+ regression$RVI+
               regression$NDWI+regression$NDMI,
            data = regression)
# Tolerance formula is (1-R2)
(1- 1)
#Variance Inflation Factor
(1/(1- 1))

#Model 1 USing NDVI as dependent variable (auxillary regression) to check multicollinearity 

ols_regress(regression$NDWI ~ regression$NDVI+regression$DVI + regression$IPVI+ regression$RVI+
              regression$NDMI,
            data = regression)
# Tolerance formula is (1-R2)
(1- 0.949)
#Variance Inflation Factor
(1/(1-0.949 ))

ols_regress(regression$NDMI ~ regression$NDWI +regression$NDVI+regression$DVI + regression$IPVI+ regression$RVI
              ,
            data = regression)
# Tolerance formula is (1-R2)
(1- 0.776)
#Variance Inflation Factor
(1/(1-0.776))

# After checking multicollinearity, IPVI, DVI< NDVI, and NDWI are highly correlated and violates the regression 

Binary <- glm(regression$LSTBinary~ regression$RVI #+regression$IPVI+ regression$DVI+
              #regression$NDVI + regression$NDWI
              +regression$NDMI,
              data = regression, family = "binomial")
summary(Binary)

