#Call Library
library(readxl)
library(data.table)

#Import data
Return <- read_excel("~/Desktop/Data61M_C.xlsx")
Size <- read_excel("~/Desktop/Data61M_C.xlsx", 
                   sheet = "MV")
BM <- read_excel("~/Desktop/Data61M_C.xlsx", 
                 sheet = "BM")
MoM <- read_excel("~/Desktop/Data61M_C.xlsx", 
                  sheet = "MoM")
Vol <- read_excel("~/Desktop/Data61M_C.xlsx", 
                  sheet = "Vol")
Rf<- read_excel("~/Desktop/Data61M_C.xlsx", 
                sheet = "Rf")

#Create ret to macth back the return
#Take out dates and Rf ready to match by ID
ret = Return[,2:ncol(Return)]

rf = Rf[,2]
Rf_ann = rf*(1/12)
Rf <- data.matrix(Rf_ann)

ExcessRm = Rm - Rf

#///////////////////////////////////////////////////////////////
#------------------SORTED size BY Median------------------------
#///////////////////////////////////////////////////////////////

#Create "sizes" from Size Data column 2 to last one
#take dates in the first column out
sizes = Size[,2:ncol(Size)]

#Create lists for Big and Small
Big_index = list(nrow(sizes))
Small_index = list(nrow(sizes))

#For Loop: Start with defining i take value from 1 to no.row(sizes)
for(i in 1: nrow(sizes)){
  
  #Create row_sizes for each rows
  #Take value out period by period
  row_sizes = sizes[i,]
  
  #Create break piont from Median value
  med_row  = median(as.numeric(row_sizes), na.rm = TRUE)
  
  #small_med match value which lower than median
  small_med = which(row_sizes <= med_row)
  big_med = which(row_sizes > med_row)
  
  #Keep value by listing it out in the created lists
  Small_index[[i]] =   small_med
  Big_index[[i]] =  big_med 
}

#///////////////////////////////////////////////////////////////
#-------------------SORTED value BY Median----------------------
#///////////////////////////////////////////////////////////////

#Create "bm" from BM Data column 2 to last one
#take dates in the first column out
bm = BM[,2:ncol(BM)]

#Create lists for High, Neutral and Low
High_index = list(nrow(bm))
Low_index = list(nrow(bm))

#For Loop: Start with defining i take value from 1 to no.row(bm)
for(j in 1: nrow(bm)){
  
  #Create row_bm for each rows
  #Take value out period by period
  row_bm = bm[j,]
  
  #Create break piont
  med_row  = median(as.numeric(row_bm), na.rm = TRUE)
  
  #small_med match value which lower than median
  high_med = which(row_bm > med_row)
  low_med = which(row_bm <= med_row)
  
  #Keep value by listing it out in the created lists
  High_index[[j]] =   high_med
  Low_index[[j]] =  low_med
}

#///////////////////////////////////////////////////////////////
#------------------SORTED momentum BY Median-------------------
#///////////////////////////////////////////////////////////////

#Create "mom" from MoM Data column 2 to last one
#take dates in the first column out
mom = MoM[,2:ncol(MoM)]

#Create lists
Up_index = list(nrow(mom))
Down_index = list(nrow(mom))

#For Loop
for(j in 1: nrow(mom)){
  
  #Create row_mom for each rows
  #Take value out period by period
  row_mom = mom[j,]
  
  #Create break pionts from 30th and 70th percentile
  med_row  = median(as.numeric(row_mom), na.rm = TRUE) 
  
  #small_med match value which lower than median
  up_med = which(row_mom > med_row)
  down_med = which(row_mom <= med_row)
  
  #Keep value by listing it out in the created lists
  Up_index[[j]] =   up_med
  Down_index[[j]] =  down_med
}

#///////////////////////////////////////////////////////////////
#-----------------SORTED volatility BY Median-------------------
#///////////////////////////////////////////////////////////////

#Create "mom" from MoM Data column 2 to last one
#take dates in the first column out
vol = Vol[,2:ncol(Vol)]

#Create lists
Calm_index = list(nrow(vol))
Vol_index = list(nrow(vol))

#For Loop
for(j in 1: nrow(vol)){
  
  #Create row_vol for each rows
  #Take value out period by period
  row_vol = vol[j,]
  
  #Create break pionts from 30th and 70th percentile
  med_row  = median(as.numeric(row_vol), na.rm = TRUE) 
  
  #small_med match value which lower than median
  calm_med = which(row_vol < med_row)
  vol_med = which(row_vol >= med_row)
  
  #Keep value by listing it out in the created lists
  Calm_index[[j]] =   calm_med
  Vol_index[[j]] =  vol_med
}

#///////////////////////////////////////////////////////////////
#-------------------------Intersect-----------------------------
#///////////////////////////////////////////////////////////////

#Create list
SH_index = list(nrow(Return))
SL_index = list(nrow(Return))
SU_index = list(nrow(Return))
SD_index = list(nrow(Return))
SC_index = list(nrow(Return))
SV_index = list(nrow(Return))

BH_index = list(nrow(Return))
BL_index = list(nrow(Return))
BU_index = list(nrow(Return))
BD_index = list(nrow(Return))
BC_index = list(nrow(Return))
BV_index = list(nrow(Return))


for(k in 1:nrow(Return)){
  
  SH_inter = intersect(Small_index[[k]],High_index[[k]])
  SL_inter = intersect(Small_index[[k]],Low_index[[k]])
  SU_inter = intersect(Small_index[[k]],Up_index[[k]])
  SD_inter = intersect(Small_index[[k]],Down_index[[k]])
  SC_inter = intersect(Small_index[[k]],Calm_index[[k]])
  SV_inter = intersect(Small_index[[k]],Vol_index[[k]])
  
  BH_inter = intersect(Big_index[[k]],High_index[[k]])
  BL_inter = intersect(Big_index[[k]],Low_index[[k]])
  BU_inter = intersect(Big_index[[k]],Up_index[[k]])
  BD_inter = intersect(Big_index[[k]],Down_index[[k]])
  BC_inter = intersect(Big_index[[k]],Calm_index[[k]])
  BV_inter = intersect(Big_index[[k]],Vol_index[[k]])
  
  
  #Keep value by listing it out in the created lists
  SH_index[[k]] =  SH_inter
  SL_index[[k]] =  SL_inter
  SU_index[[k]] =  SU_inter
  SD_index[[k]] =  SD_inter
  SC_index[[k]] =  SC_inter
  SV_index[[k]] =  SV_inter
  
  BH_index[[k]] =  BH_inter
  BL_index[[k]] =  BL_inter
  BU_index[[k]] =  BU_inter
  BD_index[[k]] =  BD_inter
  BC_index[[k]] =  BC_inter
  BV_index[[k]] =  BV_inter
}

#///////////////////////////////////////////////////////////////
#---------------------Tarced back return------------------------
#///////////////////////////////////////////////////////////////

#Create lists 
SH_ret = list(nrow(ret))
SL_ret = list(nrow(ret))
SU_ret = list(nrow(ret))
SD_ret = list(nrow(ret))
SC_ret = list(nrow(ret))
SV_ret = list(nrow(ret))

BH_ret = list(nrow(ret))
BL_ret = list(nrow(ret))
BU_ret = list(nrow(ret))
BD_ret = list(nrow(ret))
BC_ret = list(nrow(ret))
BV_ret = list(nrow(ret))


#For loop to match ID back to return from each row in list
for(p in 1: nrow(ret)){
  
  #Temporarily create macthing value for each ID and return
  sh_ret = unlist(ret[p,SH_index[[p]]])
  sl_ret = unlist(ret[p,SL_index[[p]]])
  su_ret = unlist(ret[p,SU_index[[p]]])
  sd_ret = unlist(ret[p,SD_index[[p]]])
  sc_ret = unlist(ret[p,SC_index[[p]]])
  sv_ret = unlist(ret[p,SV_index[[p]]])
  
  bh_ret = unlist(ret[p,BH_index[[p]]])
  bl_ret = unlist(ret[p,BL_index[[p]]])
  bu_ret = unlist(ret[p,BU_index[[p]]])
  bd_ret = unlist(ret[p,BD_index[[p]]])
  bc_ret = unlist(ret[p,BC_index[[p]]])
  bv_ret = unlist(ret[p,BV_index[[p]]])
  
  #Keep value by listing it out in the created lists
  SH_ret[[p]] =   as.numeric(sh_ret)
  SL_ret[[p]] =   as.numeric(sl_ret)
  SU_ret[[p]] =   as.numeric(su_ret)
  SD_ret[[p]] =   as.numeric(sd_ret)
  SC_ret[[p]] =   as.numeric(sc_ret)
  SV_ret[[p]] =   as.numeric(sv_ret)
  
  BH_ret[[p]] =   as.numeric(bh_ret)
  BL_ret[[p]] =   as.numeric(bl_ret)
  BU_ret[[p]] =   as.numeric(bu_ret)
  BD_ret[[p]] =   as.numeric(bd_ret)
  BC_ret[[p]] =   as.numeric(bc_ret)
  BV_ret[[p]] =   as.numeric(bv_ret)
}

#///////////////////////////////////////////////////////////////
#-------------------Mean return by period-----------------------
#///////////////////////////////////////////////////////////////

#Created Weights
SH_weight = list(nrow(ret))
SL_weight = list(nrow(ret))
SU_weight = list(nrow(ret))
SD_weight = list(nrow(ret))
SC_weight = list(nrow(ret))
SV_weight = list(nrow(ret))

BH_weight = list(nrow(ret))
BL_weight = list(nrow(ret))
BU_weight = list(nrow(ret))
BD_weight = list(nrow(ret))
BC_weight = list(nrow(ret))
BV_weight = list(nrow(ret))

for(p in 1: nrow(ret)){
  
  sh_weight = unlist(sizes[p,SH_index[[p]]])
  sl_weight = unlist(sizes[p,SL_index[[p]]])
  su_weight = unlist(sizes[p,SU_index[[p]]])
  sd_weight = unlist(sizes[p,SD_index[[p]]])
  sc_weight = unlist(sizes[p,SC_index[[p]]])
  sv_weight = unlist(sizes[p,SV_index[[p]]])
  
  bh_weight = unlist(sizes[p,BH_index[[p]]])
  bl_weight = unlist(sizes[p,BL_index[[p]]])
  bu_weight = unlist(sizes[p,BU_index[[p]]])
  bd_weight = unlist(sizes[p,BD_index[[p]]])
  bc_weight = unlist(sizes[p,BC_index[[p]]])
  bv_weight = unlist(sizes[p,BV_index[[p]]])
  
  SH_weight[[p]] = as.numeric(sh_weight)
  SL_weight[[p]] = as.numeric(sl_weight)
  SU_weight[[p]] = as.numeric(su_weight)
  SD_weight[[p]] = as.numeric(sd_weight)
  SC_weight[[p]] = as.numeric(sc_weight)
  SV_weight[[p]] = as.numeric(sv_weight)
  
  BH_weight[[p]] = as.numeric(bh_weight)
  BL_weight[[p]] = as.numeric(bl_weight)
  BU_weight[[p]] = as.numeric(bu_weight)
  BD_weight[[p]] = as.numeric(bd_weight)
  BC_weight[[p]] = as.numeric(bc_weight)
  BV_weight[[p]] = as.numeric(bv_weight)
}

#Create lists to keep return value
SH_ret_t = list(nrow(ret))
SL_ret_t = list(nrow(ret))
SU_ret_t = list(nrow(ret))
SD_ret_t = list(nrow(ret))
SC_ret_t = list(nrow(ret))
SV_ret_t = list(nrow(ret))

BH_ret_t = list(nrow(ret))
BL_ret_t = list(nrow(ret))
BU_ret_t = list(nrow(ret))
BD_ret_t = list(nrow(ret))
BC_ret_t = list(nrow(ret))
BV_ret_t = list(nrow(ret))

for(p in 1: nrow(ret)){
  
  #Calculate the average reurn fot that period
  sh_ret_t = weighted.mean(as.numeric(SH_ret[[p]]),
                           SH_weight[[p]], na.rm = TRUE)
  sl_ret_t = weighted.mean(as.numeric(SL_ret[[p]]),
                           SL_weight[[p]], na.rm = TRUE)
  su_ret_t = weighted.mean(as.numeric(SU_ret[[p]]),
                           SU_weight[[p]], na.rm = TRUE)
  sd_ret_t = weighted.mean(as.numeric(SD_ret[[p]]),
                           SD_weight[[p]], na.rm = TRUE)
  sc_ret_t = weighted.mean(as.numeric(SC_ret[[p]]),
                           SC_weight[[p]], na.rm = TRUE)
  sv_ret_t = weighted.mean(as.numeric(SV_ret[[p]]),
                           SV_weight[[p]], na.rm = TRUE)
  
  bh_ret_t = weighted.mean(as.numeric(BH_ret[[p]]),
                           BH_weight[[p]], na.rm = TRUE)
  bl_ret_t = weighted.mean(as.numeric(BL_ret[[p]]),
                           BL_weight[[p]], na.rm = TRUE)
  bu_ret_t = weighted.mean(as.numeric(BU_ret[[p]]),
                           BU_weight[[p]], na.rm = TRUE)
  bd_ret_t = weighted.mean(as.numeric(BD_ret[[p]]),
                           BD_weight[[p]], na.rm = TRUE)
  bc_ret_t = weighted.mean(as.numeric(BC_ret[[p]]),
                           BC_weight[[p]], na.rm = TRUE)
  bv_ret_t = weighted.mean(as.numeric(BV_ret[[p]]),
                           BV_weight[[p]], na.rm = TRUE)
  
  #Keep value by listing it out in the created lists
  SH_ret_t[[p]] =   as.numeric(sh_ret_t)
  SL_ret_t[[p]] =   as.numeric(sl_ret_t)
  SU_ret_t[[p]] =   as.numeric(su_ret_t)
  SD_ret_t[[p]] =   as.numeric(sd_ret_t)
  SC_ret_t[[p]] =   as.numeric(sc_ret_t)
  SV_ret_t[[p]] =   as.numeric(sv_ret_t)
  
  BH_ret_t[[p]] =   as.numeric(bh_ret_t)
  BL_ret_t[[p]] =   as.numeric(bl_ret_t)
  BU_ret_t[[p]] =   as.numeric(bu_ret_t)
  BD_ret_t[[p]] =   as.numeric(bd_ret_t)
  BC_ret_t[[p]] =   as.numeric(bc_ret_t)
  BV_ret_t[[p]] =   as.numeric(bv_ret_t)
}

#Chnage from NaN to zero for lists
SH_ret_t_z = rapply( SH_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
SL_ret_t_z = rapply( SL_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
SU_ret_t_z = rapply( SU_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
SD_ret_t_z = rapply( SD_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
SC_ret_t_z = rapply( SC_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
SV_ret_t_z = rapply( SV_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )

BH_ret_t_z = rapply( BH_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BL_ret_t_z = rapply( BL_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BU_ret_t_z = rapply( BU_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BD_ret_t_z = rapply( BD_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BC_ret_t_z = rapply( BC_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BV_ret_t_z = rapply( BV_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )


#///////////////////////////////////////////////////////////////
#--------------------COMBINE AS MATRIX--------------------------
#///////////////////////////////////////////////////////////////

#Create the matrix
SH_Port = matrix(NA,ncol=1,nrow=nrow(ret))
SL_Port = matrix(NA,ncol=1,nrow=nrow(ret))
SU_Port = matrix(NA,ncol=1,nrow=nrow(ret))
SD_Port = matrix(NA,ncol=1,nrow=nrow(ret))
SC_Port = matrix(NA,ncol=1,nrow=nrow(ret))
SV_Port = matrix(NA,ncol=1,nrow=nrow(ret))

BH_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BL_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BU_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BD_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BC_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BV_Port = matrix(NA,ncol=1,nrow=nrow(ret))


#For loop to combine 
for(q in 1:nrow(ret)){
  
  #create temporary value
  temp1 = SH_ret_t_z[[q]]
  temp2 = SL_ret_t_z[[q]]
  temp3 = SU_ret_t_z[[q]]
  temp4 = SD_ret_t_z[[q]]
  temp5 = SC_ret_t_z[[q]]
  temp6 = SV_ret_t_z[[q]]
  
  temp7 = BH_ret_t_z[[q]]
  temp8 = BL_ret_t_z[[q]]
  temp9 = BU_ret_t_z[[q]]
  temp10 = BD_ret_t_z[[q]]
  temp11 = BC_ret_t_z[[q]]
  temp12 = BV_ret_t_z[[q]]
  
  #put the value in matrix
  SH_Port[q] = temp1
  SL_Port[q] = temp2
  SU_Port[q] = temp3
  SD_Port[q] = temp4
  SC_Port[q] = temp5
  SV_Port[q] = temp6
  
  BH_Port[q] = temp7
  BL_Port[q] = temp8
  BU_Port[q] = temp9
  BD_Port[q] = temp10
  BC_Port[q] = temp11
  BV_Port[q] = temp12
}

#Get overall portfolio return
SH_all_ret = mean(SH_Port)
SL_all_ret = mean(SL_Port)
SU_all_ret = mean(SU_Port)
SD_all_ret = mean(SD_Port)
SC_all_ret = mean(SC_Port)
SV_all_ret = mean(SV_Port)

BH_all_ret = mean(BH_Port)
BL_all_ret = mean(BL_Port)
BU_all_ret = mean(BU_Port)
BD_all_ret = mean(BD_Port)
BC_all_ret = mean(BC_Port)
BV_all_ret = mean(BV_Port)

#///////////////////////////////////////////////////////////////
#----------------SMB, HML, UMD, CMV Port------------------------
#///////////////////////////////////////////////////////////////

SMB2_Port = ((SH_Port + SL_Port + SU_Port + 
               SD_Port + SC_Port +SV_Port)/6 - 
  (BH_Port + BL_Port + BU_Port + 
     BD_Port + BC_Port +BV_Port)/6)

HML2_Port = (SH_Port + BH_Port)/2 - (SL_Port + BL_Port)/2

UMD2_Port = (SU_Port + BU_Port)/2 - (SD_Port + BD_Port)/2

CMV2_Port = (SC_Port + BC_Port)/2 - (SV_Port + BV_Port)/2

#///////////////////////////////////////////////////////////////
#---------------------Summary Statistics------------------------
#///////////////////////////////////////////////////////////////

#Get overall portfolio return
SMB2_Mean = mean(SMB2_Port)
HML2_Mean = mean(HML2_Port)
UMD2_Mean = mean(UMD2_Port)
CMV2_Mean = mean(CMV2_Port)

SMB2_SD = sd(SMB2_Port)
HML2_SD = sd(HML2_Port)
UMD2_SD = sd(UMD2_Port)
CMV2_SD = sd(CMV2_Port)

SMB2_t = t.test(as.vector(SMB2_Port))
HML2_t = t.test(as.vector(HML2_Port))
UMD2_t = t.test(as.vector(UMD2_Port))
CMV2_t = t.test(as.vector(CMV2_Port))

#Create Matrix
STAT2_sum = matrix(NA,ncol=4,nrow=3)

STAT2_sum[1,1] = SMB2_Mean*12
STAT2_sum[2,1] = SMB2_SD*sqrt(12)
STAT2_sum[3,1] = SMB2_t[[1]]

STAT2_sum[1,2] = HML2_Mean*12
STAT2_sum[2,2] = HML2_SD*sqrt(12)
STAT2_sum[3,2] = HML2_t[[1]]

STAT2_sum[1,3] = UMD2_Mean*12
STAT2_sum[2,3] = UMD2_SD*sqrt(12)
STAT2_sum[3,3] = UMD2_t[[1]]

STAT2_sum[1,4] = CMV2_Mean*12
STAT2_sum[2,4] = CMV2_SD*sqrt(12)
STAT2_sum[3,4] = CMV2_t[[1]]

#///////////////////////////////////////////////////////////////
#------------------Statistics S B Factors-----------------------
#///////////////////////////////////////////////////////////////

#Create the desired variables
HML2_S_Port = SH_Port - SL_Port
HML2_B_Port = BH_Port - BL_Port
HML2_SB_Port = HML2_S_Port - HML2_B_Port

UMD2_S_Port = SU_Port - SD_Port
UMD2_B_Port = BU_Port - BD_Port
UMD2_SB_Port = UMD2_S_Port - UMD2_B_Port

CMV2_S_Port = SC_Port - SV_Port
CMV2_B_Port = BC_Port - BV_Port
CMV2_SB_Port = CMV2_S_Port - CMV2_B_Port

#Calculate their statistics
HML2_S_Mean = mean(HML2_S_Port)
UMD2_S_Mean = mean(UMD2_S_Port)
CMV2_S_Mean = mean(CMV2_S_Port)

HML2_S_SD = sd(HML2_S_Port)
UMD2_S_SD = sd(UMD2_S_Port)
CMV2_S_SD = sd(CMV2_S_Port)

HML2_S_t = t.test(as.vector(HML2_S_Port))
UMD2_S_t = t.test(as.vector(UMD2_S_Port))
CMV2_S_t = t.test(as.vector(CMV2_S_Port))


HML2_B_Mean = mean(HML2_B_Port)
UMD2_B_Mean = mean(UMD2_B_Port)
CMV2_B_Mean = mean(CMV2_B_Port)

HML2_B_SD = sd(HML2_B_Port)
UMD2_B_SD = sd(UMD2_B_Port)
CMV2_B_SD = sd(CMV2_B_Port)

HML2_B_t = t.test(as.vector(HML2_B_Port))
UMD2_B_t = t.test(as.vector(UMD2_B_Port))
CMV2_B_t = t.test(as.vector(CMV2_B_Port))


HML2_SB_Mean = mean(HML2_SB_Port)
UMD2_SB_Mean = mean(UMD2_SB_Port)
CMV2_SB_Mean = mean(CMV2_SB_Port)

HML2_SB_SD = sd(HML2_SB_Port)
UMD2_SB_SD = sd(UMD2_SB_Port)
CMV2_SB_SD = sd(CMV2_SB_Port)

HML2_SB_t = t.test(as.vector(HML2_SB_Port))
UMD2_SB_t = t.test(as.vector(UMD2_SB_Port))
CMV2_SB_t = t.test(as.vector(CMV2_SB_Port))

#Matrix for each vriables
#Create Matrix
STAT_HML2_SB = matrix(NA,ncol=3,nrow=3)

STAT_HML2_SB[1,1] = HML2_S_Mean*12
STAT_HML2_SB[2,1] = HML2_S_SD*sqrt(12)
STAT_HML2_SB[3,1] = HML2_S_t[[1]]

STAT_HML2_SB[1,2] = HML2_B_Mean*12
STAT_HML2_SB[2,2] = HML2_B_SD*sqrt(12)
STAT_HML2_SB[3,2] = HML2_B_t[[1]]

STAT_HML2_SB[1,3] = HML2_SB_Mean*12
STAT_HML2_SB[2,3] = HML2_SB_SD*sqrt(12)
STAT_HML2_SB[3,3] = HML2_SB_t[[1]]


STAT_UMD2_SB = matrix(NA,ncol=3,nrow=3)

STAT_UMD2_SB[1,1] = UMD2_S_Mean*12
STAT_UMD2_SB[2,1] = UMD2_S_SD*sqrt(12)
STAT_UMD2_SB[3,1] = UMD2_S_t[[1]]

STAT_UMD2_SB[1,2] = UMD2_B_Mean*12
STAT_UMD2_SB[2,2] = UMD2_B_SD*sqrt(12)
STAT_UMD2_SB[3,2] = UMD2_B_t[[1]]

STAT_UMD2_SB[1,3] = UMD2_SB_Mean*12
STAT_UMD2_SB[2,3] = UMD2_SB_SD*sqrt(12)
STAT_UMD2_SB[3,3] = UMD2_SB_t[[1]]


STAT_CMV2_SB = matrix(NA,ncol=3,nrow=3)

STAT_CMV2_SB[1,1] = CMV2_S_Mean*12
STAT_CMV2_SB[2,1] = CMV2_S_SD*sqrt(12)
STAT_CMV2_SB[3,1] = CMV2_S_t[[1]]

STAT_CMV2_SB[1,2] = CMV2_B_Mean*12
STAT_CMV2_SB[2,2] = CMV2_B_SD*sqrt(12)
STAT_CMV2_SB[3,2] = CMV2_B_t[[1]]

STAT_CMV2_SB[1,3] = CMV2_SB_Mean*12
STAT_CMV2_SB[2,3] = CMV2_SB_SD*sqrt(12)
STAT_CMV2_SB[3,3] = CMV2_SB_t[[1]]

#///////////////////////////////////////////////////////////////
#-------------------Corr btw Diff Factors-----------------------
#///////////////////////////////////////////////////////////////

ExRm_ExRm = cor(ExcessRm,ExcessRm)
ExRm_SMB = cor(ExcessRm,SMB2_Port)
ExRm_HML = cor(ExcessRm,HML2_Port)
ExRm_UMD = cor(ExcessRm,UMD2_Port)
ExRm_CMV = cor(ExcessRm,CMV2_Port)

SMB_SMB = cor(SMB2_Port,SMB2_Port)
SMB_HML = cor(SMB2_Port,HML2_Port)
SMB_UMD = cor(SMB2_Port,UMD2_Port)
SMB_CMV = cor(SMB2_Port,CMV2_Port)

HML_HML = cor(HML2_Port,HML2_Port)
HML_UMD = cor(HML2_Port,UMD2_Port)
HML_CMV = cor(HML2_Port,CMV2_Port)

UMD_UMD = cor(UMD2_Port,UMD2_Port)
UMD_CMV = cor(UMD2_Port,CMV2_Port)

CMV_CMV = cor(UMD2_Port,CMV2_Port)

#Create Matrix
Facs2_cor = matrix(NA,nrow=5,ncol=5)

Facs2_cor[1,1] = ExRm_ExRm
Facs2_cor[2,1] = ExRm_SMB
Facs2_cor[3,1] = ExRm_HML
Facs2_cor[4,1] = ExRm_UMD
Facs2_cor[5,1] = ExRm_CMV

Facs2_cor[1,2] = ExRm_SMB
Facs2_cor[2,2] = SMB_SMB
Facs2_cor[3,2] = SMB_HML
Facs2_cor[4,2] = SMB_UMD
Facs2_cor[5,2] = SMB_CMV

Facs2_cor[1,3] = ExRm_HML
Facs2_cor[2,3] = SMB_HML
Facs2_cor[3,3] = HML_HML
Facs2_cor[4,3] = HML_UMD
Facs2_cor[5,3] = HML_CMV

Facs2_cor[1,4] = ExRm_UMD
Facs2_cor[2,4] = SMB_UMD
Facs2_cor[3,4] = HML_UMD
Facs2_cor[4,4] = UMD_UMD
Facs2_cor[5,4] = UMD_CMV

Facs2_cor[1,5] = ExRm_CMV
Facs2_cor[2,5] = SMB_CMV
Facs2_cor[3,5] = HML_CMV
Facs2_cor[4,5] = UMD_CMV
Facs2_cor[5,5] = CMV_CMV

#///////////////////////////////////////////////////////////////
#-------------------Reg four explain fifth----------------------
#///////////////////////////////////////////////////////////////

Facs2_reg1 = lm(ExcessRm ~ 
                  SMB2_Port + HML2_Port + UMD2_Port + CMV2_Port )
Facs2_reg2 = lm(SMB2_Port ~ 
                  ExcessRm + HML2_Port + UMD2_Port + CMV2_Port )
Facs2_reg3 = lm(HML2_Port ~ 
                  ExcessRm + SMB2_Port + UMD2_Port + CMV2_Port )
Facs2_reg4 = lm(UMD2_Port ~ 
                  ExcessRm + SMB2_Port + HML2_Port + CMV2_Port )
Facs2_reg5 = lm(CMV2_Port ~ 
                  ExcessRm + SMB2_Port + HML2_Port + UMD2_Port)

#Create Matrix

Facs2_ExRm_reg = matrix(NA,ncol=7,nrow=3)

Facs2_ExRm_reg[1,1] = Facs2_reg1$coeff[1]
Facs2_ExRm_reg[1,3] = Facs2_reg1$coeff[2]
Facs2_ExRm_reg[1,4] = Facs2_reg1$coeff[3]
Facs2_ExRm_reg[1,5] = Facs2_reg1$coeff[4]
Facs2_ExRm_reg[1,6] = Facs2_reg1$coeff[5]
Facs2_ExRm_reg[1,7] = summary(Facs2_reg1)$r.squared
Facs2_ExRm_reg[2,1] = summary(Facs2_reg1)$coeff[1,3]
Facs2_ExRm_reg[2,3] = summary(Facs2_reg1)$coeff[2,3]
Facs2_ExRm_reg[2,4] = summary(Facs2_reg1)$coeff[3,3]
Facs2_ExRm_reg[2,5] = summary(Facs2_reg1)$coeff[4,3]
Facs2_ExRm_reg[2,6] = summary(Facs2_reg1)$coeff[5,3]
Facs2_ExRm_reg[3,1] = summary(Facs2_reg1)$coeff[1,4]
Facs2_ExRm_reg[3,3] = summary(Facs2_reg1)$coeff[2,4]
Facs2_ExRm_reg[3,4] = summary(Facs2_reg1)$coeff[3,4]
Facs2_ExRm_reg[3,5] = summary(Facs2_reg1)$coeff[4,4]
Facs2_ExRm_reg[3,6] = summary(Facs2_reg1)$coeff[5,4]


Facs2_SMB_reg = matrix(NA,ncol=7,nrow=3)

Facs2_SMB_reg[1,1] = Facs2_reg2$coeff[1]
Facs2_SMB_reg[1,2] = Facs2_reg2$coeff[2]
Facs2_SMB_reg[1,4] = Facs2_reg2$coeff[3]
Facs2_SMB_reg[1,5] = Facs2_reg2$coeff[4]
Facs2_SMB_reg[1,6] = Facs2_reg2$coeff[5]
Facs2_SMB_reg[1,7] = summary(Facs2_reg2)$r.squared
Facs2_SMB_reg[2,1] = summary(Facs2_reg2)$coeff[1,3]
Facs2_SMB_reg[2,2] = summary(Facs2_reg2)$coeff[2,3]
Facs2_SMB_reg[2,4] = summary(Facs2_reg2)$coeff[3,3]
Facs2_SMB_reg[2,5] = summary(Facs2_reg2)$coeff[4,3]
Facs2_SMB_reg[2,6] = summary(Facs2_reg2)$coeff[5,3]
Facs2_SMB_reg[3,1] = summary(Facs2_reg2)$coeff[1,4]
Facs2_SMB_reg[3,2] = summary(Facs2_reg2)$coeff[2,4]
Facs2_SMB_reg[3,4] = summary(Facs2_reg2)$coeff[3,4]
Facs2_SMB_reg[3,5] = summary(Facs2_reg2)$coeff[4,4]
Facs2_SMB_reg[3,6] = summary(Facs2_reg2)$coeff[5,4]


Facs2_HML_reg = matrix(NA,ncol=7,nrow=3)

Facs2_HML_reg[1,1] = Facs2_reg3$coeff[1]
Facs2_HML_reg[1,2] = Facs2_reg3$coeff[2]
Facs2_HML_reg[1,3] = Facs2_reg3$coeff[3]
Facs2_HML_reg[1,5] = Facs2_reg3$coeff[4]
Facs2_HML_reg[1,6] = Facs2_reg3$coeff[5]
Facs2_HML_reg[1,7] = summary(Facs2_reg3)$r.squared
Facs2_HML_reg[2,1] = summary(Facs2_reg3)$coeff[1,3]
Facs2_HML_reg[2,2] = summary(Facs2_reg3)$coeff[2,3]
Facs2_HML_reg[2,3] = summary(Facs2_reg3)$coeff[3,3]
Facs2_HML_reg[2,5] = summary(Facs2_reg3)$coeff[4,3]
Facs2_HML_reg[2,6] = summary(Facs2_reg3)$coeff[5,3]
Facs2_HML_reg[3,1] = summary(Facs2_reg3)$coeff[1,4]
Facs2_HML_reg[3,2] = summary(Facs2_reg3)$coeff[2,4]
Facs2_HML_reg[3,3] = summary(Facs2_reg3)$coeff[3,4]
Facs2_HML_reg[3,5] = summary(Facs2_reg3)$coeff[4,4]
Facs2_HML_reg[3,6] = summary(Facs2_reg3)$coeff[5,4]


Facs2_UMD_reg = matrix(NA,ncol=7,nrow=3)

Facs2_UMD_reg[1,1] = Facs2_reg4$coeff[1]
Facs2_UMD_reg[1,2] = Facs2_reg4$coeff[2]
Facs2_UMD_reg[1,3] = Facs2_reg4$coeff[3]
Facs2_UMD_reg[1,4] = Facs2_reg4$coeff[4]
Facs2_UMD_reg[1,6] = Facs2_reg4$coeff[5]
Facs2_UMD_reg[1,7] = summary(Facs2_reg4)$r.squared
Facs2_UMD_reg[2,1] = summary(Facs2_reg4)$coeff[1,3]
Facs2_UMD_reg[2,2] = summary(Facs2_reg4)$coeff[2,3]
Facs2_UMD_reg[2,3] = summary(Facs2_reg4)$coeff[3,3]
Facs2_UMD_reg[2,4] = summary(Facs2_reg4)$coeff[4,3]
Facs2_UMD_reg[2,6] = summary(Facs2_reg4)$coeff[5,3]
Facs2_UMD_reg[3,1] = summary(Facs2_reg4)$coeff[1,4]
Facs2_UMD_reg[3,2] = summary(Facs2_reg4)$coeff[2,4]
Facs2_UMD_reg[3,3] = summary(Facs2_reg4)$coeff[3,4]
Facs2_UMD_reg[3,4] = summary(Facs2_reg4)$coeff[4,4]
Facs2_UMD_reg[3,6] = summary(Facs2_reg4)$coeff[5,4]


Facs2_CMV_reg = matrix(NA,ncol=7,nrow=3)

Facs2_CMV_reg[1,1] = Facs2_reg5$coeff[1]
Facs2_CMV_reg[1,2] = Facs2_reg5$coeff[2]
Facs2_CMV_reg[1,3] = Facs2_reg5$coeff[3]
Facs2_CMV_reg[1,4] = Facs2_reg5$coeff[4]
Facs2_CMV_reg[1,5] = Facs2_reg5$coeff[5]
Facs2_CMV_reg[1,7] = summary(Facs2_reg5)$r.squared
Facs2_CMV_reg[2,1] = summary(Facs2_reg5)$coeff[1,3]
Facs2_CMV_reg[2,2] = summary(Facs2_reg5)$coeff[2,3]
Facs2_CMV_reg[2,3] = summary(Facs2_reg5)$coeff[3,3]
Facs2_CMV_reg[2,4] = summary(Facs2_reg5)$coeff[4,3]
Facs2_CMV_reg[2,5] = summary(Facs2_reg5)$coeff[5,3]
Facs2_CMV_reg[3,1] = summary(Facs2_reg5)$coeff[1,4]
Facs2_CMV_reg[3,2] = summary(Facs2_reg5)$coeff[2,4]
Facs2_CMV_reg[3,3] = summary(Facs2_reg5)$coeff[3,4]
Facs2_CMV_reg[3,4] = summary(Facs2_reg5)$coeff[4,4]
Facs2_CMV_reg[3,5] = summary(Facs2_reg5)$coeff[5,4]





