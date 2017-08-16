#Call Library
library(readxl)
library("gdata")

#Create excess return

ExcessM1_Port = M1_Port - Rf
ExcessM2_Port = M2_Port - Rf
ExcessM3_Port = M3_Port - Rf
ExcessM4_Port = M4_Port - Rf
ExcessM5_Port = M5_Port - Rf
ExcessM6_Port = M6_Port - Rf
ExcessM7_Port = M7_Port - Rf
ExcessM8_Port = M8_Port - Rf
ExcessM9_Port = M9_Port - Rf
ExcessM10_Port = M10_Port - Rf

ExcessRm = Rm - Rf

#///////////////////////////////////////////////////////////////
#------------------------Five-Factors---------------------------
#///////////////////////////////////////////////////////////////

#Regression

MultivariateReg01<- lm(ExcessM1_Port ~ ExcessRm + SMB_Port + HML_Port )
MultivariateReg02<- lm(ExcessM2_Port ~ ExcessRm + SMB_Port + HML_Port )
MultivariateReg03<- lm(ExcessM3_Port ~ ExcessRm + SMB_Port + HML_Port )
MultivariateReg04<- lm(ExcessM4_Port ~ ExcessRm + SMB_Port + HML_Port )
MultivariateReg05<- lm(ExcessM5_Port ~ ExcessRm + SMB_Port + HML_Port )
MultivariateReg06<- lm(ExcessM6_Port ~ ExcessRm + SMB_Port + HML_Port )
MultivariateReg07<- lm(ExcessM7_Port ~ ExcessRm + SMB_Port + HML_Port )
MultivariateReg08<- lm(ExcessM8_Port ~ ExcessRm + SMB_Port + HML_Port )
MultivariateReg09<- lm(ExcessM9_Port ~ ExcessRm + SMB_Port + HML_Port )
MultivariateReg10<- lm(ExcessM10_Port ~ ExcessRm + SMB_Port + HML_Port )


#Keep coeff t-value p-value separately
RegCoeffs01 <- MultivariateReg01$coeff
RegCoeffs02 <- MultivariateReg02$coeff
RegCoeffs03 <- MultivariateReg03$coeff
RegCoeffs04 <- MultivariateReg04$coeff
RegCoeffs05 <- MultivariateReg05$coeff
RegCoeffs06 <- MultivariateReg06$coeff
RegCoeffs07 <- MultivariateReg07$coeff
RegCoeffs08 <- MultivariateReg08$coeff
RegCoeffs09 <- MultivariateReg09$coeff
RegCoeffs10 <- MultivariateReg10$coeff





Regtvalue01 <- summary(MultivariateReg01)$coefficients[,3]
Regtvalue02 <- summary(MultivariateReg02)$coefficients[,3]
Regtvalue03 <- summary(MultivariateReg03)$coefficients[,3]
Regtvalue04 <- summary(MultivariateReg04)$coefficients[,3]
Regtvalue05 <- summary(MultivariateReg05)$coefficients[,3]
Regtvalue06 <- summary(MultivariateReg06)$coefficients[,3]
Regtvalue07 <- summary(MultivariateReg07)$coefficients[,3]
Regtvalue08 <- summary(MultivariateReg08)$coefficients[,3]
Regtvalue09 <- summary(MultivariateReg09)$coefficients[,3]
Regtvalue10 <- summary(MultivariateReg10)$coefficients[,3]



Regpvalue01 <- summary(MultivariateReg01)$coefficients[,4]
Regpvalue02 <- summary(MultivariateReg02)$coefficients[,4]
Regpvalue03 <- summary(MultivariateReg03)$coefficients[,4]
Regpvalue04 <- summary(MultivariateReg04)$coefficients[,4]
Regpvalue05 <- summary(MultivariateReg05)$coefficients[,4]
Regpvalue06 <- summary(MultivariateReg06)$coefficients[,4]
Regpvalue07 <- summary(MultivariateReg07)$coefficients[,4]
Regpvalue08 <- summary(MultivariateReg08)$coefficients[,4]
Regpvalue09 <- summary(MultivariateReg09)$coefficients[,4]
Regpvalue10 <- summary(MultivariateReg10)$coefficients[,4]




RegRsq01 <- summary(MultivariateReg01)$r.squared
RegRsq02 <- summary(MultivariateReg02)$r.squared
RegRsq03 <- summary(MultivariateReg03)$r.squared
RegRsq04 <- summary(MultivariateReg04)$r.squared
RegRsq05 <- summary(MultivariateReg05)$r.squared
RegRsq06 <- summary(MultivariateReg06)$r.squared
RegRsq07 <- summary(MultivariateReg07)$r.squared
RegRsq08 <- summary(MultivariateReg08)$r.squared
RegRsq09 <- summary(MultivariateReg09)$r.squared
RegRsq10 <- summary(MultivariateReg10)$r.squared




RegSE01 <- summary(MultivariateReg01)$sigma
RegSE02 <- summary(MultivariateReg02)$sigma
RegSE03 <- summary(MultivariateReg03)$sigma
RegSE04 <- summary(MultivariateReg04)$sigma
RegSE05 <- summary(MultivariateReg05)$sigma
RegSE06 <- summary(MultivariateReg06)$sigma
RegSE07 <- summary(MultivariateReg07)$sigma
RegSE08 <- summary(MultivariateReg08)$sigma
RegSE09 <- summary(MultivariateReg09)$sigma
RegSE10 <- summary(MultivariateReg10)$sigma


RegAdRsq01 <- summary(MultivariateReg01)$adj.r.squared
RegAdRsq02 <- summary(MultivariateReg02)$adj.r.squared
RegAdRsq03 <- summary(MultivariateReg03)$adj.r.squared
RegAdRsq04 <- summary(MultivariateReg04)$adj.r.squared
RegAdRsq05 <- summary(MultivariateReg05)$adj.r.squared
RegAdRsq06 <- summary(MultivariateReg06)$adj.r.squared
RegAdRsq07 <- summary(MultivariateReg07)$adj.r.squared
RegAdRsq08 <- summary(MultivariateReg08)$adj.r.squared
RegAdRsq09 <- summary(MultivariateReg09)$adj.r.squared
RegAdRsq10 <- summary(MultivariateReg10)$adj.r.squared



#--------------------Create Matrix Coeff------------------------

Coeff = matrix(NA,ncol=10,nrow=4)

Coeff[1,1] = RegCoeffs01[1]
Coeff[1,2] = RegCoeffs02[1]
Coeff[1,3] = RegCoeffs03[1]
Coeff[1,4] = RegCoeffs04[1]
Coeff[1,5] = RegCoeffs05[1]
Coeff[1,6] = RegCoeffs06[1]
Coeff[1,7] = RegCoeffs07[1]
Coeff[1,8] = RegCoeffs08[1]
Coeff[1,9] = RegCoeffs09[1]
Coeff[1,10] = RegCoeffs10[1]

Coeff[2,1] = RegCoeffs01[2]
Coeff[2,2] = RegCoeffs02[2]
Coeff[2,3] = RegCoeffs03[2]
Coeff[2,4] = RegCoeffs04[2]
Coeff[2,5] = RegCoeffs05[2]
Coeff[2,6] = RegCoeffs06[2]
Coeff[2,7] = RegCoeffs07[2]
Coeff[2,8] = RegCoeffs08[2]
Coeff[2,9] = RegCoeffs09[2]
Coeff[2,10] = RegCoeffs10[2]

Coeff[3,1] = RegCoeffs01[3]
Coeff[3,2] = RegCoeffs02[3]
Coeff[3,3] = RegCoeffs03[3]
Coeff[3,4] = RegCoeffs04[3]
Coeff[3,5] = RegCoeffs05[3]
Coeff[3,6] = RegCoeffs06[3]
Coeff[3,7] = RegCoeffs07[3]
Coeff[3,8] = RegCoeffs08[3]
Coeff[3,9] = RegCoeffs09[3]
Coeff[3,10] = RegCoeffs10[3]

Coeff[4,1] = RegCoeffs01[4]
Coeff[4,2] = RegCoeffs02[4]
Coeff[4,3] = RegCoeffs03[4]
Coeff[4,4] = RegCoeffs04[4]
Coeff[4,5] = RegCoeffs05[4]
Coeff[4,6] = RegCoeffs06[4]
Coeff[4,7] = RegCoeffs07[4]
Coeff[4,8] = RegCoeffs08[4]
Coeff[4,9] = RegCoeffs09[4]
Coeff[4,10] = RegCoeffs10[4]

#-------------------Create Matrix t-value-----------------------

t_value = matrix(NA,ncol=10,nrow=5)

t_value[1,1] = Regtvalue01[1]
t_value[1,2] = Regtvalue02[1]
t_value[1,3] = Regtvalue03[1]
t_value[1,4] = Regtvalue04[1]
t_value[1,5] = Regtvalue05[1]
t_value[1,6] = Regtvalue06[1]
t_value[1,7] = Regtvalue07[1]
t_value[1,8] = Regtvalue08[1]
t_value[1,9] = Regtvalue09[1]
t_value[1,10] = Regtvalue10[1]

t_value[2,1] = Regtvalue01[2]
t_value[2,2] = Regtvalue02[2]
t_value[2,3] = Regtvalue03[2]
t_value[2,4] = Regtvalue04[2]
t_value[2,5] = Regtvalue05[2]
t_value[2,6] = Regtvalue06[2]
t_value[2,7] = Regtvalue07[2]
t_value[2,8] = Regtvalue08[2]
t_value[2,9] = Regtvalue09[2]
t_value[2,10] = Regtvalue10[2]

t_value[3,1] = Regtvalue01[3]
t_value[3,2] = Regtvalue02[3]
t_value[3,3] = Regtvalue03[3]
t_value[3,4] = Regtvalue04[3]
t_value[3,5] = Regtvalue05[3]
t_value[3,6] = Regtvalue06[3]
t_value[3,7] = Regtvalue07[3]
t_value[3,8] = Regtvalue08[3]
t_value[3,9] = Regtvalue09[3]
t_value[3,10] = Regtvalue10[3]

t_value[4,1] = Regtvalue01[4]
t_value[4,2] = Regtvalue02[4]
t_value[4,3] = Regtvalue03[4]
t_value[4,4] = Regtvalue04[4]
t_value[4,5] = Regtvalue05[4]
t_value[4,6] = Regtvalue06[4]
t_value[4,7] = Regtvalue07[4]
t_value[4,8] = Regtvalue08[4]
t_value[4,9] = Regtvalue09[4]
t_value[4,10] = Regtvalue10[4]

t_value[5,1] = RegRsq01
t_value[5,2] = RegRsq02
t_value[5,3] = RegRsq03
t_value[5,4] = RegRsq04
t_value[5,5] = RegRsq05
t_value[5,6] = RegRsq06
t_value[5,7] = RegRsq07
t_value[5,8] = RegRsq08
t_value[5,9] = RegRsq09
t_value[5,10] = RegRsq10
