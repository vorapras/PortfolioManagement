
#2x3 2x2 and 2x2x2x2 must be ran before this

#///////////////////////////////////////////////////////////////
#-----------Correlation btw Diff Vers of Same Facs--------------
#///////////////////////////////////////////////////////////////

SMB_11 = cor(SMB_Port,SMB_Port)
SMB_12 = cor(SMB_Port,SMB2_Port)
SMB_13 = cor(SMB_Port,SMB3_Port)
SMB_22 = cor(SMB2_Port,SMB2_Port)
SMB_23 = cor(SMB2_Port,SMB3_Port)
SMB_33 = cor(SMB3_Port,SMB3_Port)

HML_11 = cor(HML_Port,HML_Port)
HML_12 = cor(HML_Port,HML2_Port)
HML_13 = cor(HML_Port,HML3_Port)
HML_22 = cor(HML2_Port,HML2_Port)
HML_23 = cor(HML2_Port,HML3_Port)
HML_33 = cor(HML3_Port,HML3_Port)

UMD_11 = cor(UMD_Port,UMD_Port)
UMD_12 = cor(UMD_Port,UMD2_Port)
UMD_13 = cor(UMD_Port,UMD3_Port)
UMD_22 = cor(UMD2_Port,UMD2_Port)
UMD_23 = cor(UMD2_Port,UMD3_Port)
UMD_33 = cor(UMD3_Port,UMD3_Port)

CMV_11 = cor(CMV_Port,CMV_Port)
CMV_12 = cor(CMV_Port,CMV2_Port)
CMV_13 = cor(CMV_Port,CMV3_Port)
CMV_22 = cor(CMV2_Port,CMV2_Port)
CMV_23 = cor(CMV2_Port,CMV3_Port)
CMV_33 = cor(CMV3_Port,CMV3_Port)

#Create Matrix
SMB_cor = matrix(NA,nrow=3,ncol=3)
HML_cor = matrix(NA,nrow=3,ncol=3)
UMD_cor = matrix(NA,nrow=3,ncol=3)
CMV_cor = matrix(NA,nrow=3,ncol=3)

SMB_cor[1,1] = SMB_11
SMB_cor[1,2] = SMB_12
SMB_cor[1,3] = SMB_13
SMB_cor[2,1] = SMB_12
SMB_cor[2,2] = SMB_22
SMB_cor[2,3] = SMB_23
SMB_cor[3,1] = SMB_13
SMB_cor[3,2] = SMB_23
SMB_cor[3,3] = SMB_33

HML_cor[1,1] = HML_11
HML_cor[1,2] = HML_12
HML_cor[1,3] = HML_13
HML_cor[2,1] = HML_12
HML_cor[2,2] = HML_22
HML_cor[2,3] = HML_23
HML_cor[3,1] = HML_13
HML_cor[3,2] = HML_23
HML_cor[3,3] = HML_33

UMD_cor[1,1] = UMD_11
UMD_cor[1,2] = UMD_12
UMD_cor[1,3] = UMD_13
UMD_cor[2,1] = UMD_12
UMD_cor[2,2] = UMD_22
UMD_cor[2,3] = UMD_23
UMD_cor[3,1] = UMD_13
UMD_cor[3,2] = UMD_23
UMD_cor[3,3] = UMD_33

CMV_cor[1,1] = CMV_11
CMV_cor[1,2] = CMV_12
CMV_cor[1,3] = CMV_13
CMV_cor[2,1] = CMV_12
CMV_cor[2,2] = CMV_22
CMV_cor[2,3] = CMV_23
CMV_cor[3,1] = CMV_13
CMV_cor[3,2] = CMV_23
CMV_cor[3,3] = CMV_33











