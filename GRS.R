#GRS Regression
library("GRS.test")

# Fama-French 3-factor model
factor.mat = data.frame(cbind(ExcessRm,SMB_Port,HML_Port))

# portfolio returns
ret.mat = data.frame(cbind(M1_Port,M2_Port,M3_Port,M4_Port,M5_Port,
                          M6_Port,M7_Port,M8_Port,M9_Port,M10_Port))

# Table 9C of Fama-French (1993)
GRS_STAT = GRS.test(ret.mat,factor.mat)$GRS.stat
GRS_pvalue = GRS.test(ret.mat,factor.mat)$GRS.pval
