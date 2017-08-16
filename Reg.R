#Call Library
library(readxl)
library("gdata")

#Create excess return

ExcessBAVA_Port = BAVA_Port - Rf
ExcessBBVA_Port = BBVA_Port - Rf
ExcessBCVA_Port = BCVA_Port - Rf
ExcessBDVA_Port = BDVA_Port - Rf
ExcessBEVA_Port = BEVA_Port - Rf

ExcessBAVB_Port = BAVB_Port - Rf
ExcessBBVB_Port = BBVB_Port - Rf
ExcessBCVB_Port = BCVB_Port - Rf
ExcessBDVB_Port = BDVB_Port - Rf
ExcessBEVB_Port = BEVB_Port - Rf

ExcessBAVC_Port = BAVC_Port - Rf
ExcessBBVC_Port = BBVC_Port - Rf
ExcessBCVC_Port = BCVC_Port - Rf
ExcessBDVC_Port = BDVC_Port - Rf
ExcessBEVC_Port = BEVC_Port - Rf

ExcessBAVD_Port = BAVD_Port - Rf
ExcessBBVD_Port = BBVD_Port - Rf
ExcessBCVD_Port = BCVD_Port - Rf
ExcessBDVD_Port = BDVD_Port - Rf
ExcessBEVD_Port = BEVD_Port - Rf

ExcessBAVE_Port = BAVE_Port - Rf
ExcessBBVE_Port = BBVE_Port - Rf
ExcessBCVE_Port = BCVE_Port - Rf
ExcessBDVE_Port = BDVE_Port - Rf
ExcessBEVE_Port = BEVE_Port - Rf

ExcessRm = Rm - Rf

#///////////////////////////////////////////////////////////////
#------------------------Five-Factors---------------------------
#///////////////////////////////////////////////////////////////

#Regression

MultivariateReg01<- lm(ExcessBAVA_Port ~ ExcessRm + SMB_Port 
                     + HML_Port + UMD_Port + CMV_Port )
MultivariateReg02<- lm(ExcessBBVA_Port ~ ExcessRm + SMB_Port 
                     + HML_Port + UMD_Port + CMV_Port )
MultivariateReg03<- lm(ExcessBCVA_Port ~ ExcessRm + SMB_Port 
                     + HML_Port + UMD_Port + CMV_Port )
MultivariateReg04<- lm(ExcessBDVA_Port ~ ExcessRm + SMB_Port 
                     + HML_Port + UMD_Port + CMV_Port )
MultivariateReg05<- lm(ExcessBEVA_Port ~ ExcessRm + SMB_Port 
                     + HML_Port + UMD_Port + CMV_Port )

MultivariateReg06<- lm(ExcessBAVB_Port ~ ExcessRm + SMB_Port 
                     + HML_Port + UMD_Port + CMV_Port )
MultivariateReg07<- lm(ExcessBBVB_Port ~ ExcessRm + SMB_Port 
                     + HML_Port + UMD_Port + CMV_Port )
MultivariateReg08<- lm(ExcessBCVB_Port ~ ExcessRm + SMB_Port 
                     + HML_Port + UMD_Port + CMV_Port )
MultivariateReg09<- lm(ExcessBDVB_Port ~ ExcessRm + SMB_Port 
                     + HML_Port + UMD_Port + CMV_Port )
MultivariateReg10<- lm(ExcessBEVB_Port ~ ExcessRm + SMB_Port 
                     + HML_Port + UMD_Port + CMV_Port )

MultivariateReg11<- lm(ExcessBAVC_Port ~ ExcessRm + SMB_Port 
                     + HML_Port + UMD_Port + CMV_Port )
MultivariateReg12<- lm(ExcessBBVC_Port ~ ExcessRm + SMB_Port 
                     + HML_Port + UMD_Port + CMV_Port )
MultivariateReg13<- lm(ExcessBCVC_Port ~ ExcessRm + SMB_Port 
                     + HML_Port + UMD_Port + CMV_Port )
MultivariateReg14<- lm(ExcessBDVC_Port ~ ExcessRm + SMB_Port 
                     + HML_Port + UMD_Port + CMV_Port )
MultivariateReg15<- lm(ExcessBEVC_Port ~ ExcessRm + SMB_Port 
                     + HML_Port + UMD_Port + CMV_Port )

MultivariateReg16<- lm(ExcessBAVD_Port ~ ExcessRm + SMB_Port 
                     + HML_Port + UMD_Port + CMV_Port )
MultivariateReg17<- lm(ExcessBBVD_Port ~ ExcessRm + SMB_Port 
                     + HML_Port + UMD_Port + CMV_Port )
MultivariateReg18<- lm(ExcessBCVD_Port ~ ExcessRm + SMB_Port 
                     + HML_Port + UMD_Port + CMV_Port )
MultivariateReg19<- lm(ExcessBDVD_Port ~ ExcessRm + SMB_Port 
                     + HML_Port + UMD_Port + CMV_Port )
MultivariateReg20<- lm(ExcessBEVD_Port ~ ExcessRm + SMB_Port 
                     + HML_Port + UMD_Port + CMV_Port )

MultivariateReg21<- lm(ExcessBAVE_Port ~ ExcessRm + SMB_Port 
                     + HML_Port + UMD_Port + CMV_Port )
MultivariateReg22<- lm(ExcessBBVE_Port ~ ExcessRm + SMB_Port 
                     + HML_Port + UMD_Port + CMV_Port )
MultivariateReg23<- lm(ExcessBCVE_Port ~ ExcessRm + SMB_Port 
                     + HML_Port + UMD_Port + CMV_Port )
MultivariateReg24<- lm(ExcessBDVE_Port ~ ExcessRm + SMB_Port 
                     + HML_Port + UMD_Port + CMV_Port )
MultivariateReg25<- lm(ExcessBEVE_Port ~ ExcessRm + SMB_Port 
                     + HML_Port + UMD_Port + CMV_Port )


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

RegCoeffs11 <- MultivariateReg11$coeff
RegCoeffs12 <- MultivariateReg12$coeff
RegCoeffs13 <- MultivariateReg13$coeff
RegCoeffs14 <- MultivariateReg14$coeff
RegCoeffs15 <- MultivariateReg15$coeff

RegCoeffs16 <- MultivariateReg16$coeff
RegCoeffs17 <- MultivariateReg17$coeff
RegCoeffs18 <- MultivariateReg18$coeff
RegCoeffs19 <- MultivariateReg19$coeff
RegCoeffs20 <- MultivariateReg20$coeff

RegCoeffs21 <- MultivariateReg21$coeff
RegCoeffs22 <- MultivariateReg22$coeff
RegCoeffs23 <- MultivariateReg23$coeff
RegCoeffs24 <- MultivariateReg24$coeff
RegCoeffs25 <- MultivariateReg25$coeff





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

Regtvalue11 <- summary(MultivariateReg11)$coefficients[,3]
Regtvalue12 <- summary(MultivariateReg12)$coefficients[,3]
Regtvalue13 <- summary(MultivariateReg13)$coefficients[,3]
Regtvalue14 <- summary(MultivariateReg14)$coefficients[,3]
Regtvalue15 <- summary(MultivariateReg15)$coefficients[,3]

Regtvalue16 <- summary(MultivariateReg16)$coefficients[,3]
Regtvalue17 <- summary(MultivariateReg17)$coefficients[,3]
Regtvalue18 <- summary(MultivariateReg18)$coefficients[,3]
Regtvalue19 <- summary(MultivariateReg19)$coefficients[,3]
Regtvalue20 <- summary(MultivariateReg20)$coefficients[,3]

Regtvalue21 <- summary(MultivariateReg21)$coefficients[,3]
Regtvalue22 <- summary(MultivariateReg22)$coefficients[,3]
Regtvalue23 <- summary(MultivariateReg23)$coefficients[,3]
Regtvalue24 <- summary(MultivariateReg24)$coefficients[,3]
Regtvalue25 <- summary(MultivariateReg25)$coefficients[,3]






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

Regpvalue11 <- summary(MultivariateReg11)$coefficients[,4]
Regpvalue12 <- summary(MultivariateReg12)$coefficients[,4]
Regpvalue13 <- summary(MultivariateReg13)$coefficients[,4]
Regpvalue14 <- summary(MultivariateReg14)$coefficients[,4]
Regpvalue15 <- summary(MultivariateReg15)$coefficients[,4]

Regpvalue16 <- summary(MultivariateReg16)$coefficients[,4]
Regpvalue17 <- summary(MultivariateReg17)$coefficients[,4]
Regpvalue18 <- summary(MultivariateReg18)$coefficients[,4]
Regpvalue19 <- summary(MultivariateReg19)$coefficients[,4]
Regpvalue20 <- summary(MultivariateReg20)$coefficients[,4]

Regpvalue21 <- summary(MultivariateReg21)$coefficients[,4]
Regpvalue22 <- summary(MultivariateReg22)$coefficients[,4]
Regpvalue23 <- summary(MultivariateReg23)$coefficients[,4]
Regpvalue24 <- summary(MultivariateReg24)$coefficients[,4]
Regpvalue25 <- summary(MultivariateReg25)$coefficients[,4]


#--------------------Create Matrix Coeff------------------------

a = matrix(NA,ncol=5,nrow=5)

a[1,1] = RegCoeffs01[1]
a[2,1] = RegCoeffs02[1]
a[3,1] = RegCoeffs03[1]
a[4,1] = RegCoeffs04[1]
a[5,1] = RegCoeffs05[1]

a[1,2] = RegCoeffs06[1]
a[2,2] = RegCoeffs07[1]
a[3,2] = RegCoeffs08[1]
a[4,2] = RegCoeffs09[1]
a[5,2] = RegCoeffs10[1]

a[1,3] = RegCoeffs11[1]
a[2,3] = RegCoeffs12[1]
a[3,3] = RegCoeffs13[1]
a[4,3] = RegCoeffs14[1]
a[5,3] = RegCoeffs15[1]

a[1,4] = RegCoeffs16[1]
a[2,4] = RegCoeffs17[1]
a[3,4] = RegCoeffs18[1]
a[4,4] = RegCoeffs19[1]
a[5,4] = RegCoeffs20[1]

a[1,5] = RegCoeffs21[1]
a[2,5] = RegCoeffs22[1]
a[3,5] = RegCoeffs23[1]
a[4,5] = RegCoeffs24[1]
a[5,5] = RegCoeffs25[1]


b = matrix(NA,ncol=5,nrow=5)

b[1,1] = RegCoeffs01[2]
b[2,1] = RegCoeffs02[2]
b[3,1] = RegCoeffs03[2]
b[4,1] = RegCoeffs04[2]
b[5,1] = RegCoeffs05[2]

b[1,2] = RegCoeffs06[2]
b[2,2] = RegCoeffs07[2]
b[3,2] = RegCoeffs08[2]
b[4,2] = RegCoeffs09[2]
b[5,2] = RegCoeffs10[2]

b[1,3] = RegCoeffs11[2]
b[2,3] = RegCoeffs12[2]
b[3,3] = RegCoeffs13[2]
b[4,3] = RegCoeffs14[2]
b[5,3] = RegCoeffs15[2]

b[1,4] = RegCoeffs16[2]
b[2,4] = RegCoeffs17[2]
b[3,4] = RegCoeffs18[2]
b[4,4] = RegCoeffs19[2]
b[5,4] = RegCoeffs20[2]

b[1,5] = RegCoeffs21[2]
b[2,5] = RegCoeffs22[2]
b[3,5] = RegCoeffs23[2]
b[4,5] = RegCoeffs24[2]
b[5,5] = RegCoeffs25[2]


s = matrix(NA,ncol=5,nrow=5)

s[1,1] = RegCoeffs01[3]
s[2,1] = RegCoeffs02[3]
s[3,1] = RegCoeffs03[3]
s[4,1] = RegCoeffs04[3]
s[5,1] = RegCoeffs05[3]

s[1,2] = RegCoeffs06[3]
s[2,2] = RegCoeffs07[3]
s[3,2] = RegCoeffs08[3]
s[4,2] = RegCoeffs09[3]
s[5,2] = RegCoeffs10[3]

s[1,3] = RegCoeffs11[3]
s[2,3] = RegCoeffs12[3]
s[3,3] = RegCoeffs13[3]
s[4,3] = RegCoeffs14[3]
s[5,3] = RegCoeffs15[3]

s[1,4] = RegCoeffs16[3]
s[2,4] = RegCoeffs17[3]
s[3,4] = RegCoeffs18[3]
s[4,4] = RegCoeffs19[3]
s[5,4] = RegCoeffs20[3]

s[1,5] = RegCoeffs21[3]
s[2,5] = RegCoeffs22[3]
s[3,5] = RegCoeffs23[3]
s[4,5] = RegCoeffs24[3]
s[5,5] = RegCoeffs25[3]


h = matrix(NA,ncol=5,nrow=5)

h[1,1] = RegCoeffs01[4]
h[2,1] = RegCoeffs02[4]
h[3,1] = RegCoeffs03[4]
h[4,1] = RegCoeffs04[4]
h[5,1] = RegCoeffs05[4]

h[1,2] = RegCoeffs06[4]
h[2,2] = RegCoeffs07[4]
h[3,2] = RegCoeffs08[4]
h[4,2] = RegCoeffs09[4]
h[5,2] = RegCoeffs10[4]

h[1,3] = RegCoeffs11[4]
h[2,3] = RegCoeffs12[4]
h[3,3] = RegCoeffs13[4]
h[4,3] = RegCoeffs14[4]
h[5,3] = RegCoeffs15[4]

h[1,4] = RegCoeffs16[4]
h[2,4] = RegCoeffs17[4]
h[3,4] = RegCoeffs18[4]
h[4,4] = RegCoeffs19[4]
h[5,4] = RegCoeffs20[4]

h[1,5] = RegCoeffs21[4]
h[2,5] = RegCoeffs22[4]
h[3,5] = RegCoeffs23[4]
h[4,5] = RegCoeffs24[4]
h[5,5] = RegCoeffs25[4]



u = matrix(NA,ncol=5,nrow=5)

u[1,1] = RegCoeffs01[5]
u[2,1] = RegCoeffs02[5]
u[3,1] = RegCoeffs03[5]
u[4,1] = RegCoeffs04[5]
u[5,1] = RegCoeffs05[5]

u[1,2] = RegCoeffs06[5]
u[2,2] = RegCoeffs07[5]
u[3,2] = RegCoeffs08[5]
u[4,2] = RegCoeffs09[5]
u[5,2] = RegCoeffs10[5]

u[1,3] = RegCoeffs11[5]
u[2,3] = RegCoeffs12[5]
u[3,3] = RegCoeffs13[5]
u[4,3] = RegCoeffs14[5]
u[5,3] = RegCoeffs15[5]

u[1,4] = RegCoeffs16[5]
u[2,4] = RegCoeffs17[5]
u[3,4] = RegCoeffs18[5]
u[4,4] = RegCoeffs19[5]
u[5,4] = RegCoeffs20[5]

u[1,5] = RegCoeffs21[5]
u[2,5] = RegCoeffs22[5]
u[3,5] = RegCoeffs23[5]
u[4,5] = RegCoeffs24[5]
u[5,5] = RegCoeffs25[5]


c = matrix(NA,ncol=5,nrow=5)

c[1,1] = RegCoeffs01[6]
c[2,1] = RegCoeffs02[6]
c[3,1] = RegCoeffs03[6]
c[4,1] = RegCoeffs04[6]
c[5,1] = RegCoeffs05[6]

c[1,2] = RegCoeffs06[6]
c[2,2] = RegCoeffs07[6]
c[3,2] = RegCoeffs08[6]
c[4,2] = RegCoeffs09[6]
c[5,2] = RegCoeffs10[6]

c[1,3] = RegCoeffs11[6]
c[2,3] = RegCoeffs12[6]
c[3,3] = RegCoeffs13[6]
c[4,3] = RegCoeffs14[6]
c[5,3] = RegCoeffs15[6]

c[1,4] = RegCoeffs16[6]
c[2,4] = RegCoeffs17[6]
c[3,4] = RegCoeffs18[6]
c[4,4] = RegCoeffs19[6]
c[5,4] = RegCoeffs20[6]

c[1,5] = RegCoeffs21[6]
c[2,5] = RegCoeffs22[6]
c[3,5] = RegCoeffs23[6]
c[4,5] = RegCoeffs24[6]
c[5,5] = RegCoeffs25[6]

#-------------------Create Matrix t-value-----------------------

t_a = matrix(NA,ncol=5,nrow=5)

t_a[1,1] = Regtvalue01[1]
t_a[2,1] = Regtvalue02[1]
t_a[3,1] = Regtvalue03[1]
t_a[4,1] = Regtvalue04[1]
t_a[5,1] = Regtvalue05[1]

t_a[1,2] = Regtvalue06[1]
t_a[2,2] = Regtvalue07[1]
t_a[3,2] = Regtvalue08[1]
t_a[4,2] = Regtvalue09[1]
t_a[5,2] = Regtvalue10[1]

t_a[1,3] = Regtvalue11[1]
t_a[2,3] = Regtvalue12[1]
t_a[3,3] = Regtvalue13[1]
t_a[4,3] = Regtvalue14[1]
t_a[5,3] = Regtvalue15[1]

t_a[1,4] = Regtvalue16[1]
t_a[2,4] = Regtvalue17[1]
t_a[3,4] = Regtvalue18[1]
t_a[4,4] = Regtvalue19[1]
t_a[5,4] = Regtvalue20[1]

t_a[1,5] = Regtvalue21[1]
t_a[2,5] = Regtvalue22[1]
t_a[3,5] = Regtvalue23[1]
t_a[4,5] = Regtvalue24[1]
t_a[5,5] = Regtvalue25[1]


t_b = matrix(NA,ncol=5,nrow=5)

t_b[1,1] = Regtvalue01[2]
t_b[2,1] = Regtvalue02[2]
t_b[3,1] = Regtvalue03[2]
t_b[4,1] = Regtvalue04[2]
t_b[5,1] = Regtvalue05[2]

t_b[1,2] = Regtvalue06[2]
t_b[2,2] = Regtvalue07[2]
t_b[3,2] = Regtvalue08[2]
t_b[4,2] = Regtvalue09[2]
t_b[5,2] = Regtvalue10[2]

t_b[1,3] = Regtvalue11[2]
t_b[2,3] = Regtvalue12[2]
t_b[3,3] = Regtvalue13[2]
t_b[4,3] = Regtvalue14[2]
t_b[5,3] = Regtvalue15[2]

t_b[1,4] = Regtvalue16[2]
t_b[2,4] = Regtvalue17[2]
t_b[3,4] = Regtvalue18[2]
t_b[4,4] = Regtvalue19[2]
t_b[5,4] = Regtvalue20[2]

t_b[1,5] = Regtvalue21[2]
t_b[2,5] = Regtvalue22[2]
t_b[3,5] = Regtvalue23[2]
t_b[4,5] = Regtvalue24[2]
t_b[5,5] = Regtvalue25[2]


t_s = matrix(NA,ncol=5,nrow=5)

t_s[1,1] = Regtvalue01[3]
t_s[2,1] = Regtvalue02[3]
t_s[3,1] = Regtvalue03[3]
t_s[4,1] = Regtvalue04[3]
t_s[5,1] = Regtvalue05[3]

t_s[1,2] = Regtvalue06[3]
t_s[2,2] = Regtvalue07[3]
t_s[3,2] = Regtvalue08[3]
t_s[4,2] = Regtvalue09[3]
t_s[5,2] = Regtvalue10[3]

t_s[1,3] = Regtvalue11[3]
t_s[2,3] = Regtvalue12[3]
t_s[3,3] = Regtvalue13[3]
t_s[4,3] = Regtvalue14[3]
t_s[5,3] = Regtvalue15[3]

t_s[1,4] = Regtvalue16[3]
t_s[2,4] = Regtvalue17[3]
t_s[3,4] = Regtvalue18[3]
t_s[4,4] = Regtvalue19[3]
t_s[5,4] = Regtvalue20[3]

t_s[1,5] = Regtvalue21[3]
t_s[2,5] = Regtvalue22[3]
t_s[3,5] = Regtvalue23[3]
t_s[4,5] = Regtvalue24[3]
t_s[5,5] = Regtvalue25[3]


t_h = matrix(NA,ncol=5,nrow=5)

t_h[1,1] = Regtvalue01[4]
t_h[2,1] = Regtvalue02[4]
t_h[3,1] = Regtvalue03[4]
t_h[4,1] = Regtvalue04[4]
t_h[5,1] = Regtvalue05[4]

t_h[1,2] = Regtvalue06[4]
t_h[2,2] = Regtvalue07[4]
t_h[3,2] = Regtvalue08[4]
t_h[4,2] = Regtvalue09[4]
t_h[5,2] = Regtvalue10[4]

t_h[1,3] = Regtvalue11[4]
t_h[2,3] = Regtvalue12[4]
t_h[3,3] = Regtvalue13[4]
t_h[4,3] = Regtvalue14[4]
t_h[5,3] = Regtvalue15[4]

t_h[1,4] = Regtvalue16[4]
t_h[2,4] = Regtvalue17[4]
t_h[3,4] = Regtvalue18[4]
t_h[4,4] = Regtvalue19[4]
t_h[5,4] = Regtvalue20[4]

t_h[1,5] = Regtvalue21[4]
t_h[2,5] = Regtvalue22[4]
t_h[3,5] = Regtvalue23[4]
t_h[4,5] = Regtvalue24[4]
t_h[5,5] = Regtvalue25[4]



t_u = matrix(NA,ncol=5,nrow=5)

t_u[1,1] = Regtvalue01[5]
t_u[2,1] = Regtvalue02[5]
t_u[3,1] = Regtvalue03[5]
t_u[4,1] = Regtvalue04[5]
t_u[5,1] = Regtvalue05[5]

t_u[1,2] = Regtvalue06[5]
t_u[2,2] = Regtvalue07[5]
t_u[3,2] = Regtvalue08[5]
t_u[4,2] = Regtvalue09[5]
t_u[5,2] = Regtvalue10[5]

t_u[1,3] = Regtvalue11[5]
t_u[2,3] = Regtvalue12[5]
t_u[3,3] = Regtvalue13[5]
t_u[4,3] = Regtvalue14[5]
t_u[5,3] = Regtvalue15[5]

t_u[1,4] = Regtvalue16[5]
t_u[2,4] = Regtvalue17[5]
t_u[3,4] = Regtvalue18[5]
t_u[4,4] = Regtvalue19[5]
t_u[5,4] = Regtvalue20[5]

t_u[1,5] = Regtvalue21[5]
t_u[2,5] = Regtvalue22[5]
t_u[3,5] = Regtvalue23[5]
t_u[4,5] = Regtvalue24[5]
t_u[5,5] = Regtvalue25[5]


t_c = matrix(NA,ncol=5,nrow=5)

t_c[1,1] = Regtvalue01[6]
t_c[2,1] = Regtvalue02[6]
t_c[3,1] = Regtvalue03[6]
t_c[4,1] = Regtvalue04[6]
t_c[5,1] = Regtvalue05[6]

t_c[1,2] = Regtvalue06[6]
t_c[2,2] = Regtvalue07[6]
t_c[3,2] = Regtvalue08[6]
t_c[4,2] = Regtvalue09[6]
t_c[5,2] = Regtvalue10[6]

t_c[1,3] = Regtvalue11[6]
t_c[2,3] = Regtvalue12[6]
t_c[3,3] = Regtvalue13[6]
t_c[4,3] = Regtvalue14[6]
t_c[5,3] = Regtvalue15[6]

t_c[1,4] = Regtvalue16[6]
t_c[2,4] = Regtvalue17[6]
t_c[3,4] = Regtvalue18[6]
t_c[4,4] = Regtvalue19[6]
t_c[5,4] = Regtvalue20[6]

t_c[1,5] = Regtvalue21[6]
t_c[2,5] = Regtvalue22[6]
t_c[3,5] = Regtvalue23[6]
t_c[4,5] = Regtvalue24[6]
t_c[5,5] = Regtvalue25[6]


#-------------------Create Matrix p-value-----------------------

p_a = matrix(NA,ncol=5,nrow=5)

p_a[1,1] = Regpvalue01[1]
p_a[2,1] = Regpvalue02[1]
p_a[3,1] = Regpvalue03[1]
p_a[4,1] = Regpvalue04[1]
p_a[5,1] = Regpvalue05[1]

p_a[1,2] = Regpvalue06[1]
p_a[2,2] = Regpvalue07[1]
p_a[3,2] = Regpvalue08[1]
p_a[4,2] = Regpvalue09[1]
p_a[5,2] = Regpvalue10[1]

p_a[1,3] = Regpvalue11[1]
p_a[2,3] = Regpvalue12[1]
p_a[3,3] = Regpvalue13[1]
p_a[4,3] = Regpvalue14[1]
p_a[5,3] = Regpvalue15[1]

p_a[1,4] = Regpvalue16[1]
p_a[2,4] = Regpvalue17[1]
p_a[3,4] = Regpvalue18[1]
p_a[4,4] = Regpvalue19[1]
p_a[5,4] = Regpvalue20[1]

p_a[1,5] = Regpvalue21[1]
p_a[2,5] = Regpvalue22[1]
p_a[3,5] = Regpvalue23[1]
p_a[4,5] = Regpvalue24[1]
p_a[5,5] = Regpvalue25[1]


p_b = matrix(NA,ncol=5,nrow=5)

p_b[1,1] = Regpvalue01[2]
p_b[2,1] = Regpvalue02[2]
p_b[3,1] = Regpvalue03[2]
p_b[4,1] = Regpvalue04[2]
p_b[5,1] = Regpvalue05[2]

p_b[1,2] = Regpvalue06[2]
p_b[2,2] = Regpvalue07[2]
p_b[3,2] = Regpvalue08[2]
p_b[4,2] = Regpvalue09[2]
p_b[5,2] = Regpvalue10[2]

p_b[1,3] = Regpvalue11[2]
p_b[2,3] = Regpvalue12[2]
p_b[3,3] = Regpvalue13[2]
p_b[4,3] = Regpvalue14[2]
p_b[5,3] = Regpvalue15[2]

p_b[1,4] = Regpvalue16[2]
p_b[2,4] = Regpvalue17[2]
p_b[3,4] = Regpvalue18[2]
p_b[4,4] = Regpvalue19[2]
p_b[5,4] = Regpvalue20[2]

p_b[1,5] = Regpvalue21[2]
p_b[2,5] = Regpvalue22[2]
p_b[3,5] = Regpvalue23[2]
p_b[4,5] = Regpvalue24[2]
p_b[5,5] = Regpvalue25[2]


p_s = matrix(NA,ncol=5,nrow=5)

p_s[1,1] = Regpvalue01[3]
p_s[2,1] = Regpvalue02[3]
p_s[3,1] = Regpvalue03[3]
p_s[4,1] = Regpvalue04[3]
p_s[5,1] = Regpvalue05[3]

p_s[1,2] = Regpvalue06[3]
p_s[2,2] = Regpvalue07[3]
p_s[3,2] = Regpvalue08[3]
p_s[4,2] = Regpvalue09[3]
p_s[5,2] = Regpvalue10[3]

p_s[1,3] = Regpvalue11[3]
p_s[2,3] = Regpvalue12[3]
p_s[3,3] = Regpvalue13[3]
p_s[4,3] = Regpvalue14[3]
p_s[5,3] = Regpvalue15[3]

p_s[1,4] = Regpvalue16[3]
p_s[2,4] = Regpvalue17[3]
p_s[3,4] = Regpvalue18[3]
p_s[4,4] = Regpvalue19[3]
p_s[5,4] = Regpvalue20[3]

p_s[1,5] = Regpvalue21[3]
p_s[2,5] = Regpvalue22[3]
p_s[3,5] = Regpvalue23[3]
p_s[4,5] = Regpvalue24[3]
p_s[5,5] = Regpvalue25[3]


p_h = matrix(NA,ncol=5,nrow=5)

p_h[1,1] = Regpvalue01[4]
p_h[2,1] = Regpvalue02[4]
p_h[3,1] = Regpvalue03[4]
p_h[4,1] = Regpvalue04[4]
p_h[5,1] = Regpvalue05[4]

p_h[1,2] = Regpvalue06[4]
p_h[2,2] = Regpvalue07[4]
p_h[3,2] = Regpvalue08[4]
p_h[4,2] = Regpvalue09[4]
p_h[5,2] = Regpvalue10[4]

p_h[1,3] = Regpvalue11[4]
p_h[2,3] = Regpvalue12[4]
p_h[3,3] = Regpvalue13[4]
p_h[4,3] = Regpvalue14[4]
p_h[5,3] = Regpvalue15[4]

p_h[1,4] = Regpvalue16[4]
p_h[2,4] = Regpvalue17[4]
p_h[3,4] = Regpvalue18[4]
p_h[4,4] = Regpvalue19[4]
p_h[5,4] = Regpvalue20[4]

p_h[1,5] = Regpvalue21[4]
p_h[2,5] = Regpvalue22[4]
p_h[3,5] = Regpvalue23[4]
p_h[4,5] = Regpvalue24[4]
p_h[5,5] = Regpvalue25[4]



p_u = matrix(NA,ncol=5,nrow=5)

p_u[1,1] = Regpvalue01[5]
p_u[2,1] = Regpvalue02[5]
p_u[3,1] = Regpvalue03[5]
p_u[4,1] = Regpvalue04[5]
p_u[5,1] = Regpvalue05[5]

p_u[1,2] = Regpvalue06[5]
p_u[2,2] = Regpvalue07[5]
p_u[3,2] = Regpvalue08[5]
p_u[4,2] = Regpvalue09[5]
p_u[5,2] = Regpvalue10[5]

p_u[1,3] = Regpvalue11[5]
p_u[2,3] = Regpvalue12[5]
p_u[3,3] = Regpvalue13[5]
p_u[4,3] = Regpvalue14[5]
p_u[5,3] = Regpvalue15[5]

p_u[1,4] = Regpvalue16[5]
p_u[2,4] = Regpvalue17[5]
p_u[3,4] = Regpvalue18[5]
p_u[4,4] = Regpvalue19[5]
p_u[5,4] = Regpvalue20[5]

p_u[1,5] = Regpvalue21[5]
p_u[2,5] = Regpvalue22[5]
p_u[3,5] = Regpvalue23[5]
p_u[4,5] = Regpvalue24[5]
p_u[5,5] = Regpvalue25[5]


p_c = matrix(NA,ncol=5,nrow=5)

p_c[1,1] = Regpvalue01[6]
p_c[2,1] = Regpvalue02[6]
p_c[3,1] = Regpvalue03[6]
p_c[4,1] = Regpvalue04[6]
p_c[5,1] = Regpvalue05[6]

p_c[1,2] = Regpvalue06[6]
p_c[2,2] = Regpvalue07[6]
p_c[3,2] = Regpvalue08[6]
p_c[4,2] = Regpvalue09[6]
p_c[5,2] = Regpvalue10[6]

p_c[1,3] = Regpvalue11[6]
p_c[2,3] = Regpvalue12[6]
p_c[3,3] = Regpvalue13[6]
p_c[4,3] = Regpvalue14[6]
p_c[5,3] = Regpvalue15[6]

p_c[1,4] = Regpvalue16[6]
p_c[2,4] = Regpvalue17[6]
p_c[3,4] = Regpvalue18[6]
p_c[4,4] = Regpvalue19[6]
p_c[5,4] = Regpvalue20[6]

p_c[1,5] = Regpvalue21[6]
p_c[2,5] = Regpvalue22[6]
p_c[3,5] = Regpvalue23[6]
p_c[4,5] = Regpvalue24[6]
p_c[5,5] = Regpvalue25[6]

#///////////////////////////////////////////////////////////////
#------------------------Five-Factors---------------------------
#///////////////////////////////////////////////////////////////

#Regression

FFReg01<- lm(ExcessBAVA_Port ~ ExcessRm + SMB_Port + HML_Port )
FFReg02<- lm(ExcessBBVA_Port ~ ExcessRm + SMB_Port + HML_Port )
FFReg03<- lm(ExcessBCVA_Port ~ ExcessRm + SMB_Port + HML_Port )
FFReg04<- lm(ExcessBDVA_Port ~ ExcessRm + SMB_Port + HML_Port )
FFReg05<- lm(ExcessBEVA_Port ~ ExcessRm + SMB_Port + HML_Port )

FFReg06<- lm(ExcessBAVB_Port ~ ExcessRm + SMB_Port + HML_Port )
FFReg07<- lm(ExcessBBVB_Port ~ ExcessRm + SMB_Port + HML_Port )
FFReg08<- lm(ExcessBCVB_Port ~ ExcessRm + SMB_Port + HML_Port )
FFReg09<- lm(ExcessBDVB_Port ~ ExcessRm + SMB_Port + HML_Port )
FFReg10<- lm(ExcessBEVB_Port ~ ExcessRm + SMB_Port + HML_Port )

FFReg11<- lm(ExcessBAVC_Port ~ ExcessRm + SMB_Port + HML_Port )
FFReg12<- lm(ExcessBBVC_Port ~ ExcessRm + SMB_Port + HML_Port )
FFReg13<- lm(ExcessBCVC_Port ~ ExcessRm + SMB_Port + HML_Port )
FFReg14<- lm(ExcessBDVC_Port ~ ExcessRm + SMB_Port + HML_Port )
FFReg15<- lm(ExcessBEVC_Port ~ ExcessRm + SMB_Port + HML_Port )

FFReg16<- lm(ExcessBAVD_Port ~ ExcessRm + SMB_Port + HML_Port )
FFReg17<- lm(ExcessBBVD_Port ~ ExcessRm + SMB_Port + HML_Port )
FFReg18<- lm(ExcessBCVD_Port ~ ExcessRm + SMB_Port + HML_Port )
FFReg19<- lm(ExcessBDVD_Port ~ ExcessRm + SMB_Port + HML_Port )
FFReg20<- lm(ExcessBEVD_Port ~ ExcessRm + SMB_Port + HML_Port )

FFReg21<- lm(ExcessBAVE_Port ~ ExcessRm + SMB_Port + HML_Port )
FFReg22<- lm(ExcessBBVE_Port ~ ExcessRm + SMB_Port + HML_Port )
FFReg23<- lm(ExcessBCVE_Port ~ ExcessRm + SMB_Port + HML_Port )
FFReg24<- lm(ExcessBDVE_Port ~ ExcessRm + SMB_Port + HML_Port )
FFReg25<- lm(ExcessBEVE_Port ~ ExcessRm + SMB_Port + HML_Port )

#Keep coeff t-value p-value separately

FFCoeffs01 <- FFReg01$coeff
FFCoeffs02 <- FFReg02$coeff
FFCoeffs03 <- FFReg03$coeff
FFCoeffs04 <- FFReg04$coeff
FFCoeffs05 <- FFReg05$coeff

FFCoeffs06 <- FFReg06$coeff
FFCoeffs07 <- FFReg07$coeff
FFCoeffs08 <- FFReg08$coeff
FFCoeffs09 <- FFReg09$coeff
FFCoeffs10 <- FFReg10$coeff

FFCoeffs11 <- FFReg11$coeff
FFCoeffs12 <- FFReg12$coeff
FFCoeffs13 <- FFReg13$coeff
FFCoeffs14 <- FFReg14$coeff
FFCoeffs15 <- FFReg15$coeff

FFCoeffs16 <- FFReg16$coeff
FFCoeffs17 <- FFReg17$coeff
FFCoeffs18 <- FFReg18$coeff
FFCoeffs19 <- FFReg19$coeff
FFCoeffs20 <- FFReg20$coeff

FFCoeffs21 <- FFReg21$coeff
FFCoeffs22 <- FFReg22$coeff
FFCoeffs23 <- FFReg23$coeff
FFCoeffs24 <- FFReg24$coeff
FFCoeffs25 <- FFReg25$coeff

FFtvalue01 <- summary(FFReg01)$coefficients[,3]
FFtvalue02 <- summary(FFReg02)$coefficients[,3]
FFtvalue03 <- summary(FFReg03)$coefficients[,3]
FFtvalue04 <- summary(FFReg04)$coefficients[,3]
FFtvalue05 <- summary(FFReg05)$coefficients[,3]

FFtvalue06 <- summary(FFReg06)$coefficients[,3]
FFtvalue07 <- summary(FFReg07)$coefficients[,3]
FFtvalue08 <- summary(FFReg08)$coefficients[,3]
FFtvalue09 <- summary(FFReg09)$coefficients[,3]
FFtvalue10 <- summary(FFReg10)$coefficients[,3]

FFtvalue11 <- summary(FFReg11)$coefficients[,3]
FFtvalue12 <- summary(FFReg12)$coefficients[,3]
FFtvalue13 <- summary(FFReg13)$coefficients[,3]
FFtvalue14 <- summary(FFReg14)$coefficients[,3]
FFtvalue15 <- summary(FFReg15)$coefficients[,3]

FFtvalue16 <- summary(FFReg16)$coefficients[,3]
FFtvalue17 <- summary(FFReg17)$coefficients[,3]
FFtvalue18 <- summary(FFReg18)$coefficients[,3]
FFtvalue19 <- summary(FFReg19)$coefficients[,3]
FFtvalue20 <- summary(FFReg20)$coefficients[,3]

FFtvalue21 <- summary(FFReg21)$coefficients[,3]
FFtvalue22 <- summary(FFReg22)$coefficients[,3]
FFtvalue23 <- summary(FFReg23)$coefficients[,3]
FFtvalue24 <- summary(FFReg24)$coefficients[,3]
FFtvalue25 <- summary(FFReg25)$coefficients[,3]

FFpvalue01 <- summary(FFReg01)$coefficients[,4]
FFpvalue02 <- summary(FFReg02)$coefficients[,4]
FFpvalue03 <- summary(FFReg03)$coefficients[,4]
FFpvalue04 <- summary(FFReg04)$coefficients[,4]
FFpvalue05 <- summary(FFReg05)$coefficients[,4]

FFpvalue06 <- summary(FFReg06)$coefficients[,4]
FFpvalue07 <- summary(FFReg07)$coefficients[,4]
FFpvalue08 <- summary(FFReg08)$coefficients[,4]
FFpvalue09 <- summary(FFReg09)$coefficients[,4]
FFpvalue10 <- summary(FFReg10)$coefficients[,4]

FFpvalue11 <- summary(FFReg11)$coefficients[,4]
FFpvalue12 <- summary(FFReg12)$coefficients[,4]
FFpvalue13 <- summary(FFReg13)$coefficients[,4]
FFpvalue14 <- summary(FFReg14)$coefficients[,4]
FFpvalue15 <- summary(FFReg15)$coefficients[,4]

FFpvalue16 <- summary(FFReg16)$coefficients[,4]
FFpvalue17 <- summary(FFReg17)$coefficients[,4]
FFpvalue18 <- summary(FFReg18)$coefficients[,4]
FFpvalue19 <- summary(FFReg19)$coefficients[,4]
FFpvalue20 <- summary(FFReg20)$coefficients[,4]

FFpvalue21 <- summary(FFReg21)$coefficients[,4]
FFpvalue22 <- summary(FFReg22)$coefficients[,4]
FFpvalue23 <- summary(FFReg23)$coefficients[,4]
FFpvalue24 <- summary(FFReg24)$coefficients[,4]
FFpvalue25 <- summary(FFReg25)$coefficients[,4]


#--------------------Create Matrix Coeff------------------------

FF_a = matrix(NA,ncol=5,nrow=5)

FF_a[1,1] = FFCoeffs01[1]
FF_a[2,1] = FFCoeffs02[1]
FF_a[3,1] = FFCoeffs03[1]
FF_a[4,1] = FFCoeffs04[1]
FF_a[5,1] = FFCoeffs05[1]

FF_a[1,2] = FFCoeffs06[1]
FF_a[2,2] = FFCoeffs07[1]
FF_a[3,2] = FFCoeffs08[1]
FF_a[4,2] = FFCoeffs09[1]
FF_a[5,2] = FFCoeffs10[1]

FF_a[1,3] = FFCoeffs11[1]
FF_a[2,3] = FFCoeffs12[1]
FF_a[3,3] = FFCoeffs13[1]
FF_a[4,3] = FFCoeffs14[1]
FF_a[5,3] = FFCoeffs15[1]

FF_a[1,4] = FFCoeffs16[1]
FF_a[2,4] = FFCoeffs17[1]
FF_a[3,4] = FFCoeffs18[1]
FF_a[4,4] = FFCoeffs19[1]
FF_a[5,4] = FFCoeffs20[1]

FF_a[1,5] = FFCoeffs21[1]
FF_a[2,5] = FFCoeffs22[1]
FF_a[3,5] = FFCoeffs23[1]
FF_a[4,5] = FFCoeffs24[1]
FF_a[5,5] = FFCoeffs25[1]


#-------------------Create Matrix t-value-----------------------

FF_t_a = matrix(NA,ncol=5,nrow=5)

FF_t_a[1,1] = FFtvalue01[1]
FF_t_a[2,1] = FFtvalue02[1]
FF_t_a[3,1] = FFtvalue03[1]
FF_t_a[4,1] = FFtvalue04[1]
FF_t_a[5,1] = FFtvalue05[1]

FF_t_a[1,2] = FFtvalue06[1]
FF_t_a[2,2] = FFtvalue07[1]
FF_t_a[3,2] = FFtvalue08[1]
FF_t_a[4,2] = FFtvalue09[1]
FF_t_a[5,2] = FFtvalue10[1]

FF_t_a[1,3] = FFtvalue11[1]
FF_t_a[2,3] = FFtvalue12[1]
FF_t_a[3,3] = FFtvalue13[1]
FF_t_a[4,3] = FFtvalue14[1]
FF_t_a[5,3] = FFtvalue15[1]

FF_t_a[1,4] = FFtvalue16[1]
FF_t_a[2,4] = FFtvalue17[1]
FF_t_a[3,4] = FFtvalue18[1]
FF_t_a[4,4] = FFtvalue19[1]
FF_t_a[5,4] = FFtvalue20[1]

FF_t_a[1,5] = FFtvalue21[1]
FF_t_a[2,5] = FFtvalue22[1]
FF_t_a[3,5] = FFtvalue23[1]
FF_t_a[4,5] = FFtvalue24[1]
FF_t_a[5,5] = FFtvalue25[1]


#-------------------Create Matrix p-value-----------------------

FF_p_a = matrix(NA,ncol=5,nrow=5)

FF_p_a[1,1] = FFpvalue01[1]
FF_p_a[2,1] = FFpvalue02[1]
FF_p_a[3,1] = FFpvalue03[1]
FF_p_a[4,1] = FFpvalue04[1]
FF_p_a[5,1] = FFpvalue05[1]

FF_p_a[1,2] = FFpvalue06[1]
FF_p_a[2,2] = FFpvalue07[1]
FF_p_a[3,2] = FFpvalue08[1]
FF_p_a[4,2] = FFpvalue09[1]
FF_p_a[5,2] = FFpvalue10[1]

FF_p_a[1,3] = FFpvalue11[1]
FF_p_a[2,3] = FFpvalue12[1]
FF_p_a[3,3] = FFpvalue13[1]
FF_p_a[4,3] = FFpvalue14[1]
FF_p_a[5,3] = FFpvalue15[1]

FF_p_a[1,4] = FFpvalue16[1]
FF_p_a[2,4] = FFpvalue17[1]
FF_p_a[3,4] = FFpvalue18[1]
FF_p_a[4,4] = FFpvalue19[1]
FF_p_a[5,4] = FFpvalue20[1]

FF_p_a[1,5] = FFpvalue21[1]
FF_p_a[2,5] = FFpvalue22[1]
FF_p_a[3,5] = FFpvalue23[1]
FF_p_a[4,5] = FFpvalue24[1]
FF_p_a[5,5] = FFpvalue25[1]

