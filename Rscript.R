library(expm) 
library(MASS)
library(readxl)
library(dplyr)
library(car)
library(tidyr)
library(purrr)
library(GGally)

##### Rural Community Facilities ######
data.RawCom <- read_excel("C:/Users/reyza/OneDrive - Bina Nusantara/Sem.6/MS-Paper/RuralCommunityFacilities.xlsx")
data.Community <- data.RawCom[,2:ncol(data.RawCom)]
names(data.Community)  <- c('x1','x2','x3','x4','x5','x6','x7','x8')
data.Community
#Testing
library(mvShapiroTest)
#H0 = variable pada Community terdistribusi multivariate normal 
#H1 = variable pada Community Tidak terdistribusi normal multivariate
mvShapiro.Test(as.matrix(data.Community))

hasil <- summary(powerTransform(data.Community))
hasil
x1 <- data.Community[,1]^hasil$result[1]
x2 <- data.Community[,2]^hasil$result[2]
x3 <- data.Community[,3]^hasil$result[3]
x4 <- data.Community[,4]^hasil$result[4]
x5 <- data.Community[,5]^hasil$result[5]
x6 <- data.Community[,6]^hasil$result[6]
x7 <- data.Community[,7]^hasil$result[7]
x8 <- data.Community[,8]^hasil$result[8]

transformed.Com <- data.frame(x1,x2,x3,x4,x5,x6,x7,x8)
mvShapiro.Test(as.matrix(transformed.Com))

# Multicolinearitas
data.Community$DummyY <- seq(1)
model <- lm(DummyY ~., data = data.Community)
vif(model)


#Linearitas
ggpairs(data.Community[1:8])

##### Rural Characteristics#####
data.RawChar <- read_excel("C:/Users/reyza/OneDrive - Bina Nusantara/Sem.6/MS-Paper/ruralCommunityCharacteristics.xlsx", sheet = "All")
data.RawChar
data.Characteristic <- data.RawChar[,2:ncol(data.RawChar)]

names(data.Characteristic) <- c("y1","y2","y3")
#Testing
library(mvShapiroTest)
#H0 = variable pada Community terdistribusi multivariate normal 
#H1 = variable pada Community Tidak terdistribusi normal multivariate
mvShapiro.Test(as.matrix(data.Characteristic))

hasil <- summary(powerTransform(data.Characteristic))
hasil
y1 <- data.Characteristic[,1]^hasil$result[1]
y2 <- data.Characteristic[,2]^hasil$result[2]
y3 <- data.Characteristic[,3]^hasil$result[3]

transformed.Char <- data.frame(y1,y2,y3)
mvShapiro.Test(as.matrix(transformed.Char))

## Multicolinearitas
data.Characteristic$DummyY <- seq(1)
library(car)
model <- lm(DummyY ~., data = data.Characteristic)
vif(model)

#Lineritas
ggpairs(data.Characteristic[1:3])


##### Canonical Analysis #####

library(dplyr)
combined_df <- cbind(transformed.Char %>% dplyr::select(y1,y2,y3), transformed.Com  %>% dplyr::select(x1,x2,x3,x4,x5,x6,x7,x8))
rho.all<- cor(combined_df)
rho.all
rho.11 <- rho.all[1:3,1:3] 
rho.22 <- rho.all[4:11,4:11]
rho.22
rho.12 <- rho.all[1:3,4:11]
rho.12
rho.21 <- rho.all[4:11,1:3]
rho.21


##### U(Y --> anggota paling kecil) bisa juga yang dependent#####
inv2.rho.11 <- solve(sqrtm(rho.11))
inv.rho.22 <- solve(rho.22)

A = inv2.rho.11 %*% rho.12 %*% inv.rho.22 %*% rho.21 %*% inv2.rho.11

#ini  untuk cari yang kanonik kuadrat $values
cuadratic.Can <- eigen(A)$values
cuadratic.Can

#untuk nilai korelasi kanonik
sqrt(cuadratic.Can)

#Bobot Kanonik for U
Weight.CanonicalA <- eigen(A)$vectors
Weight.CanonicalA

#Uk
U.1 <- t(Weight.CanonicalA[,1]) %*%  inv2.rho.11
U.2 <- t(Weight.CanonicalA[,2]) %*%  inv2.rho.11
U.3 <- t(Weight.CanonicalA[,3]) %*%  inv2.rho.11
U.1
U.2
U.3
##### V(X->anggotanya paling banyak) bisa juga yang independent ####
inv2.rho.22 <- solve(sqrtm(rho.22))
inv.rho.11 <- solve(rho.11)

B <- inv2.rho.22 %*% rho.21 %*% inv.rho.11 %*% rho.12 %*% inv2.rho.22

#Bobot Kanonik for V
Weight.CanonicalB <- eigen(B)$vectors
Weight.CanonicalB

#Vk
V.1 <- t(Weight.CanonicalB[,1]) %*%  inv2.rho.22
V.2 <- t(Weight.CanonicalB[,2]) %*%  inv2.rho.22
V.3 <- t(Weight.CanonicalB[,3]) %*%  inv2.rho.22

V.1
V.2
V.3
##### Simultaneous Testing ####
lambda.stat <- det(rho.all)/(det(rho.11) * (det(rho.22)))
lambda.stat


#Loadings
library(CCA)
library(CCP)
cc1 <- cc(transformed.Com, transformed.Char) 
cc2 <- comput(transformed.Com, transformed.Char, cc1)

x_scores <- cc2$xscores
y_scores <- cc2$yscores

# Calculate canonical loadings
yx_loadings <- cor(transformed.Char, x_scores)
xy_loadings <- cor(transformed.Com, y_scores)
xx_loadings <- cor(transformed.Com, x_scores)
yy_loadings <- cor(transformed.Char, y_scores)
