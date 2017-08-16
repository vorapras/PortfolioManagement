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

#For Loop: Start with defining i take value from 1 - no.row(sizes)
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
#----------------SORTED value BY percentiles--------------------
#///////////////////////////////////////////////////////////////

#Create "bm" from BM Data column 2 to last one
#take dates in the first column out
bm = BM[,2:ncol(BM)]

#Create lists for High, Neutral and Low
High_index = list(nrow(bm))
Neutral_index = list(nrow(bm))
Low_index = list(nrow(bm))

#For Loop: Start with defining i take value from 1 to no.row(bm)
for(j in 1: nrow(bm)){
  
  #Create row_bm for each rows
  #Take value out period by period
  row_bm = bm[j,]
  
  #Create break pionts from 30th and 70th percentile
  thrity_row  = quantile(as.numeric(row_bm), probs = 0.3, 
                         na.rm = TRUE)
  seventy_row  = quantile(as.numeric(row_bm), probs = 0.7, 
                         na.rm = TRUE) 
  
  #small_med match value which lower than median
  high_med = which(row_bm > seventy_row)
  neutral_med = which((row_bm <= seventy_row) 
                      & (row_bm >= thrity_row))
  low_med = which(row_bm < thrity_row)
  
  #Keep value by listing it out in the created lists
  High_index[[j]] =   high_med
  Neutral_index[[j]] =  neutral_med 
  Low_index[[j]] =  low_med
}

#///////////////////////////////////////////////////////////////
#-----------------------Intersect SH----------------------------
#///////////////////////////////////////////////////////////////

#Create list
SH_index = list(nrow(Return))

for(k in 1:nrow(Return)){
  
  SH_inter = intersect(Small_index[[k]],High_index[[k]]) 
  
  #Keep value by listing it out in the created lists
  SH_index[[k]] =  SH_inter
}

#---------------------Tarced back return------------------------

#Create lists to keep return value for Big and Small
SH_ret = list(nrow(ret))

#For loop to match ID back to return from each row in list
for(p in 1: nrow(ret)){
  
  #Temporarily create macthing value for each ID and return
  sh_ret = unlist(ret[p,SH_index[[p]]])
  
  #Keep value by listing it out in the created lists
  SH_ret[[p]] =   as.numeric(sh_ret)
}

#-------------------Mean return by period-----------------------

#Created Weights
SH_weight = list(nrow(ret))
for(p in 1: nrow(ret)){
  sh_weight = unlist(sizes[p,SH_index[[p]]])
  SH_weight[[p]] = as.numeric(sh_weight)
}

#Create lists to keep return value for Big and Small
SH_ret_t = list(nrow(ret))

for(p in 1: nrow(ret)){
  
  #Calculate the average reurn fot that period
  sh_ret_t = weighted.mean(as.numeric(SH_ret[[p]]),
                           SH_weight[[p]], na.rm = TRUE)
  
  #Keep value by listing it out in the created lists
  SH_ret_t[[p]] =   as.numeric(sh_ret_t)
}

#Chnage from NaN to zero for lists
SH_ret_t_z = rapply( SH_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )

#--------------------COMBINE AS MATRIX--------------------------

#Create the SH matrix
SH_Port = matrix(NA,ncol=1,nrow=nrow(ret))

#For loop to combine 
for(q in 1:nrow(SH_Port)){
  
  #create temporary value fro Small_ret_t
  temp = SH_ret_t_z[[q]]
  
  #put the value in matrix
  SH_Port[q] = temp
}

#Get overall portfolio return
SH_all_ret = mean(SH_Port)

#///////////////////////////////////////////////////////////////
#-----------------------Intersect SN----------------------------
#///////////////////////////////////////////////////////////////

#Create list
SN_index = list(nrow(Return))

for(k in 1:nrow(Return)){
  
  SN_inter = intersect(Small_index[[k]],Neutral_index[[k]]) 
  
  #Keep value by listing it out in the created lists
  SN_index[[k]] =  SN_inter
}

#---------------------Tarced back return------------------------

#Created Weights
SN_weight = list(nrow(ret))
for(p in 1: nrow(ret)){
  sn_weight = unlist(sizes[p,SN_index[[p]]])
  SN_weight[[p]] = as.numeric(sn_weight)
}

#Create lists to keep return value for Big and Small
SN_ret = list(nrow(ret))

#For loop to match ID back to return from each row in list
for(p in 1: nrow(ret)){
  
  #Temporarily create macthing value for each ID and return
  sn_ret = unlist(ret[p,SN_index[[p]]])
  
  #Keep value by listing it out in the created lists
  SN_ret[[p]] =   as.numeric(sn_ret)
}

#-------------------Mean return by period-----------------------

#Create lists to keep return value for Big and Small
SN_ret_t = list(nrow(ret))

for(p in 1: nrow(ret)){
  
  #Calculate the average reurn fot that period
  sn_ret_t = weighted.mean(as.numeric(SN_ret[[p]]),
                           SN_weight[[p]], na.rm = TRUE)
  
  #Keep value by listing it out in the created lists
  SN_ret_t[[p]] =   as.numeric(sn_ret_t)
}

#Chnage from NaN to zero for lists
SN_ret_t_z = rapply( SN_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )

#--------------------COMBINE AS MATRIX--------------------------

#Create the SH matrix
SN_Port = matrix(NA,ncol=1,nrow=nrow(ret))

#For loop to combine 
for(q in 1:nrow(SN_Port)){
  
  #create temporary value fro Small_ret_t
  temp = SN_ret_t_z[[q]]
  
  #put the value in matrix
  SN_Port[q] = temp
}

#Get overall portfolio return
SN_all_ret = mean(SN_Port)

#///////////////////////////////////////////////////////////////
#-----------------------Intersect SL----------------------------
#///////////////////////////////////////////////////////////////

#Create list
SL_index = list(nrow(Return))

for(k in 1:nrow(Return)){
  
  SL_inter = intersect(Small_index[[k]],Low_index[[k]]) 
  
  #Keep value by listing it out in the created lists
  SL_index[[k]] =  SL_inter
}

#---------------------Tarced back return------------------------

#Create lists to keep return value for Big and Small
SL_ret = list(nrow(ret))

#For loop to match ID back to return from each row in list
for(p in 1: nrow(ret)){
  
  #Temporarily create macthing value for each ID and return
  sl_ret = unlist(ret[p,SL_index[[p]]])
  
  #Keep value by listing it out in the created lists
  SL_ret[[p]] =   as.numeric(sl_ret)
}

#-------------------Mean return by period-----------------------

#Created Weights
SL_weight = list(nrow(ret))
for(p in 1: nrow(ret)){
  sl_weight = unlist(sizes[p,SL_index[[p]]])
  SL_weight[[p]] = as.numeric(sl_weight)
}

#Create lists to keep return value for Big and Small
SL_ret_t = list(nrow(ret))

for(p in 1: nrow(ret)){
  
  #Calculate the average reurn fot that period
  sl_ret_t = weighted.mean(as.numeric(SL_ret[[p]]),
                           SL_weight[[p]], na.rm = TRUE)
  
  #Keep value by listing it out in the created lists
  SL_ret_t[[p]] =   as.numeric(sl_ret_t)
}

#Chnage from NaN to zero for lists
SL_ret_t_z = rapply( SL_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )

#--------------------COMBINE AS MATRIX--------------------------

#Create the SH matrix
SL_Port = matrix(NA,ncol=1,nrow=nrow(ret))

#For loop to combine 
for(q in 1:nrow(SL_Port)){
  
  #create temporary value fro Small_ret_t
  temp = SL_ret_t_z[[q]]
  
  #put the value in matrix
  SL_Port[q] = temp
}

#Get overall portfolio return
SL_all_ret = mean(SL_Port)

#///////////////////////////////////////////////////////////////
#-----------------------Intersect BL----------------------------
#///////////////////////////////////////////////////////////////

#Create list
BL_index = list(nrow(Return))

for(k in 1:nrow(Return)){
  
  BL_inter = intersect(Big_index[[k]],Low_index[[k]]) 
  
  #Keep value by listing it out in the created lists
  BL_index[[k]] =  BL_inter
}

#---------------------Tarced back return------------------------

#Create lists to keep return value for Big and Small
BL_ret = list(nrow(ret))

#For loop to match ID back to return from each row in list
for(p in 1: nrow(ret)){
  
  #Temporarily create macthing value for each ID and return
  bl_ret = unlist(ret[p,BL_index[[p]]])
  
  #Keep value by listing it out in the created lists
  BL_ret[[p]] =   as.numeric(bl_ret)
}

#-------------------Mean return by period-----------------------

#Created Weights
BL_weight = list(nrow(ret))
for(p in 1: nrow(ret)){
  bl_weight = unlist(sizes[p,BL_index[[p]]])
  BL_weight[[p]] = as.numeric(bl_weight)
}

#Create lists to keep return value for Big and Small
BL_ret_t = list(nrow(ret))

for(p in 1: nrow(ret)){
  
  #Calculate the average reurn fot that period
  bl_ret_t = weighted.mean(as.numeric(BL_ret[[p]]),
                           BL_weight[[p]], na.rm = TRUE)
  
  #Keep value by listing it out in the created lists
  BL_ret_t[[p]] =   as.numeric(bl_ret_t)
}

#Chnage from NaN to zero for lists
BL_ret_t_z = rapply( BL_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )

#--------------------COMBINE AS MATRIX--------------------------

#Create the SH matrix
BL_Port = matrix(NA,ncol=1,nrow=nrow(ret))

#For loop to combine 
for(q in 1:nrow(BL_Port)){
  
  #create temporary value fro Small_ret_t
  temp = BL_ret_t_z[[q]]
  
  #put the value in matrix
  BL_Port[q] = temp
}

#Get overall portfolio return
BL_all_ret = mean(BL_Port)

#///////////////////////////////////////////////////////////////
#-----------------------Intersect BN----------------------------
#///////////////////////////////////////////////////////////////

#Create list
BN_index = list(nrow(Return))

for(k in 1:nrow(Return)){
  
  BN_inter = intersect(Big_index[[k]],Neutral_index[[k]]) 
  
  #Keep value by listing it out in the created lists
  BN_index[[k]] =  BN_inter
}

#---------------------Tarced back return------------------------

#Create lists to keep return value for Big and Small
BN_ret = list(nrow(ret))

#For loop to match ID back to return from each row in list
for(p in 1: nrow(ret)){
  
  #Temporarily create macthing value for each ID and return
  bn_ret = unlist(ret[p,BN_index[[p]]])
  
  #Keep value by listing it out in the created lists
  BN_ret[[p]] =   as.numeric(bn_ret)
}

#-------------------Mean return by period-----------------------

#Created Weights
BN_weight = list(nrow(ret))
for(p in 1: nrow(ret)){
  bn_weight = unlist(sizes[p,BN_index[[p]]])
  BN_weight[[p]] = as.numeric(bn_weight)
}

#Create lists to keep return value for Big and Small
BN_ret_t = list(nrow(ret))

for(p in 1: nrow(ret)){
  
  #Calculate the average reurn fot that period
  bn_ret_t = weighted.mean(as.numeric(BN_ret[[p]]),
                           BN_weight[[p]], na.rm = TRUE)
  
  #Keep value by listing it out in the created lists
  BN_ret_t[[p]] =   as.numeric(bn_ret_t)
}

#Chnage from NaN to zero for lists
BN_ret_t_z = rapply( BN_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )

#--------------------COMBINE AS MATRIX--------------------------

#Create the SH matrix
BN_Port = matrix(NA,ncol=1,nrow=nrow(ret))

#For loop to combine 
for(q in 1:nrow(BN_Port)){
  
  #create temporary value fro Small_ret_t
  temp = BN_ret_t_z[[q]]
  
  #put the value in matrix
  BN_Port[q] = temp
}

#Get overall portfolio return
BN_all_ret = mean(BN_Port)

#///////////////////////////////////////////////////////////////
#-----------------------Intersect BH----------------------------
#///////////////////////////////////////////////////////////////

#Create list
BH_index = list(nrow(Return))

for(k in 1:nrow(Return)){
  
  BH_inter = intersect(Big_index[[k]],High_index[[k]]) 
  
  #Keep value by listing it out in the created lists
  BH_index[[k]] =  BH_inter
}

#---------------------Tarced back return------------------------

#Create lists to keep return value for Big and Small
BH_ret = list(nrow(ret))

#For loop to match ID back to return from each row in list
for(p in 1: nrow(ret)){
  
  #Temporarily create macthing value for each ID and return
  bh_ret = unlist(ret[p,BH_index[[p]]])
  
  #Keep value by listing it out in the created lists
  BH_ret[[p]] =   as.numeric(bh_ret)
}

#-------------------Mean return by period-----------------------

#Created Weights
BH_weight = list(nrow(ret))
for(p in 1: nrow(ret)){
  bh_weight = unlist(sizes[p,BH_index[[p]]])
  BH_weight[[p]] = as.numeric(bh_weight)
}

#Create lists to keep return value for Big and Small
BH_ret_t = list(nrow(ret))

for(p in 1: nrow(ret)){
  
  #Calculate the average reurn fot that period
  bh_ret_t = weighted.mean(as.numeric(BH_ret[[p]]),
                           BH_weight[[p]], na.rm = TRUE)
  
  #Keep value by listing it out in the created lists
  BH_ret_t[[p]] =   as.numeric(bh_ret_t)
}

#Chnage from NaN to zero for lists
BH_ret_t_z = rapply( BH_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )

#--------------------COMBINE AS MATRIX--------------------------

#Create the SH matrix
BH_Port = matrix(NA,ncol=1,nrow=nrow(ret))

#For loop to combine 
for(q in 1:nrow(BH_Port)){
  
  #create temporary value fro Small_ret_t
  temp = BH_ret_t_z[[q]]
  
  #put the value in matrix
  BH_Port[q] = temp
}

#Get overall portfolio return
BH_all_ret = mean(BH_Port)

#///////////////////////////////////////////////////////////////
#---------------SORTED momentum BY percentiles------------------
#///////////////////////////////////////////////////////////////

#Create "mom" from MoM Data column 2 to last one
#take dates in the first column out
mom = MoM[,2:ncol(MoM)]

#Create lists
Up_index = list(nrow(mom))
NeutralM_index = list(nrow(mom))
Down_index = list(nrow(mom))

#For Loop
for(j in 1: nrow(mom)){
  
  #Create row_mom for each rows
  #Take value out period by period
  row_mom = mom[j,]
  
  #Create break pionts from 30th and 70th percentile
  thritym_row  = quantile(as.numeric(row_mom), probs = 0.3, 
                         na.rm = TRUE)
  seventym_row  = quantile(as.numeric(row_mom), probs = 0.7, 
                          na.rm = TRUE) 
  
  #small_med match value which lower than median
  up_med = which(row_mom > seventym_row)
  neutralm_med = which((row_mom <= seventym_row) 
                      & (row_mom >= thritym_row))
  down_med = which(row_mom < thritym_row)
  
  #Keep value by listing it out in the created lists
  Up_index[[j]] =   up_med
  NeutralM_index[[j]] =  neutralm_med 
  Down_index[[j]] =  down_med
}

#///////////////////////////////////////////////////////////////
#--------------------------Intersect----------------------------
#///////////////////////////////////////////////////////////////

#Create list
SU_index = list(nrow(Return))
SNm_index = list(nrow(Return))
SD_index = list(nrow(Return))

BU_index = list(nrow(Return))
BNm_index = list(nrow(Return))
BD_index = list(nrow(Return))

for(k in 1:nrow(Return)){
  
  SU_inter = intersect(Small_index[[k]],Up_index[[k]])
  SNm_inter = intersect(Small_index[[k]],NeutralM_index[[k]])
  SD_inter = intersect(Small_index[[k]],Down_index[[k]])
  
  BU_inter = intersect(Big_index[[k]],Up_index[[k]])
  BNm_inter = intersect(Big_index[[k]],NeutralM_index[[k]])
  BD_inter = intersect(Big_index[[k]],Down_index[[k]])
  
  #Keep value by listing it out in the created lists
  SU_index[[k]] =  SU_inter
  SNm_index[[k]] =  SNm_inter
  SD_index[[k]] =  SD_inter
  
  BU_index[[k]] =  BU_inter
  BNm_index[[k]] =  BNm_inter
  BD_index[[k]] =  BD_inter
}

#///////////////////////////////////////////////////////////////
#---------------------Tarced back return------------------------
#///////////////////////////////////////////////////////////////

#Create lists 
SU_ret = list(nrow(ret))
SNm_ret = list(nrow(ret))
SD_ret = list(nrow(ret))

BU_ret = list(nrow(ret))
BNm_ret = list(nrow(ret))
BD_ret = list(nrow(ret))

#For loop to match ID back to return from each row in list
for(p in 1: nrow(ret)){
  
  #Temporarily create macthing value for each ID and return
  su_ret = unlist(ret[p,SU_index[[p]]])
  snm_ret = unlist(ret[p,SNm_index[[p]]])
  sd_ret = unlist(ret[p,SD_index[[p]]])
  
  bu_ret = unlist(ret[p,BU_index[[p]]])
  bnm_ret = unlist(ret[p,BNm_index[[p]]])
  bd_ret = unlist(ret[p,BD_index[[p]]])
  
  #Keep value by listing it out in the created lists
  SU_ret[[p]] =   as.numeric(su_ret)
  SNm_ret[[p]] =   as.numeric(snm_ret)
  SD_ret[[p]] =   as.numeric(sd_ret)
  
  BU_ret[[p]] =   as.numeric(bu_ret)
  BNm_ret[[p]] =   as.numeric(bnm_ret)
  BD_ret[[p]] =   as.numeric(bd_ret)
}

#///////////////////////////////////////////////////////////////
#-------------------Mean return by period-----------------------
#///////////////////////////////////////////////////////////////

#Created Weights
SU_weight = list(nrow(ret))
SNm_weight = list(nrow(ret))
SD_weight = list(nrow(ret))

BU_weight = list(nrow(ret))
BNm_weight = list(nrow(ret))
BD_weight = list(nrow(ret))

for(p in 1: nrow(ret)){
  
  su_weight = unlist(sizes[p,SU_index[[p]]])
  snm_weight = unlist(sizes[p,SNm_index[[p]]])
  sd_weight = unlist(sizes[p,SD_index[[p]]])
  
  bu_weight = unlist(sizes[p,BU_index[[p]]])
  bnm_weight = unlist(sizes[p,BNm_index[[p]]])
  bd_weight = unlist(sizes[p,BD_index[[p]]])
  
  SU_weight[[p]] = as.numeric(su_weight)
  SNm_weight[[p]] = as.numeric(snm_weight)
  SD_weight[[p]] = as.numeric(sd_weight)
  
  BU_weight[[p]] = as.numeric(bu_weight)
  BNm_weight[[p]] = as.numeric(bnm_weight)
  BD_weight[[p]] = as.numeric(bd_weight)
}

#Create lists to keep return value for Big and Small
SU_ret_t = list(nrow(ret))
SNm_ret_t = list(nrow(ret))
SD_ret_t = list(nrow(ret))

BU_ret_t = list(nrow(ret))
BNm_ret_t = list(nrow(ret))
BD_ret_t = list(nrow(ret))

for(p in 1: nrow(ret)){
  
  #Calculate the average reurn fot that period
  su_ret_t = weighted.mean(as.numeric(SU_ret[[p]]),
                           SU_weight[[p]], na.rm = TRUE)
  snm_ret_t = weighted.mean(as.numeric(SNm_ret[[p]]),
                           SNm_weight[[p]], na.rm = TRUE)
  sd_ret_t = weighted.mean(as.numeric(SD_ret[[p]]),
                           SD_weight[[p]], na.rm = TRUE)
  
  bu_ret_t = weighted.mean(as.numeric(BU_ret[[p]]),
                           BU_weight[[p]], na.rm = TRUE)
  bnm_ret_t = weighted.mean(as.numeric(BNm_ret[[p]]),
                            BNm_weight[[p]], na.rm = TRUE)
  bd_ret_t = weighted.mean(as.numeric(BD_ret[[p]]),
                           BD_weight[[p]], na.rm = TRUE)
  
  #Keep value by listing it out in the created lists
  SU_ret_t[[p]] =   as.numeric(su_ret_t)
  SNm_ret_t[[p]] =   as.numeric(snm_ret_t)
  SD_ret_t[[p]] =   as.numeric(sd_ret_t)

  BU_ret_t[[p]] =   as.numeric(bu_ret_t)
  BNm_ret_t[[p]] =   as.numeric(bnm_ret_t)
  BD_ret_t[[p]] =   as.numeric(bd_ret_t)
}

#Chnage from NaN to zero for lists
SU_ret_t_z = rapply( SU_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
SNm_ret_t_z = rapply( SNm_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
SD_ret_t_z = rapply( SD_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )

BU_ret_t_z = rapply( BU_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BNm_ret_t_z = rapply( BNm_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BD_ret_t_z = rapply( BD_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )

#///////////////////////////////////////////////////////////////
#--------------------COMBINE AS MATRIX--------------------------
#///////////////////////////////////////////////////////////////

#Create the matrix
SU_Port = matrix(NA,ncol=1,nrow=nrow(ret))
SNm_Port = matrix(NA,ncol=1,nrow=nrow(ret))
SD_Port = matrix(NA,ncol=1,nrow=nrow(ret))

BU_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BNm_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BD_Port = matrix(NA,ncol=1,nrow=nrow(ret))

#For loop to combine 
for(q in 1:nrow(ret)){
  
  #create temporary value
  temp1 = SU_ret_t_z[[q]]
  temp2 = SNm_ret_t_z[[q]]
  temp3 = SD_ret_t_z[[q]]
  
  temp4 = BU_ret_t_z[[q]]
  temp5 = BNm_ret_t_z[[q]]
  temp6 = BD_ret_t_z[[q]]
  
  #put the value in matrix
  SU_Port[q] = temp1
  SNm_Port[q] = temp2
  SD_Port[q] = temp3
  
  BU_Port[q] = temp4
  BNm_Port[q] = temp5
  BD_Port[q] = temp6
}

#Get overall portfolio return
SU_all_ret = mean(SU_Port)
SNm_all_ret = mean(SNm_Port)
SD_all_ret = mean(SD_Port)

BU_all_ret = mean(BU_Port)
BNm_all_ret = mean(BNm_Port)
BD_all_ret = mean(BD_Port)

#///////////////////////////////////////////////////////////////
#---------------SORTED volatility BY percentiles----------------
#///////////////////////////////////////////////////////////////

#Create "mom" from MoM Data column 2 to last one
#take dates in the first column out
vol = Vol[,2:ncol(Vol)]

#Create lists
Calm_index = list(nrow(vol))
NeutralV_index = list(nrow(vol))
Vol_index = list(nrow(vol))

#For Loop
for(j in 1: nrow(vol)){
  
  #Create row_vol for each rows
  #Take value out period by period
  row_vol = vol[j,]
  
  #Create break pionts from 30th and 70th percentile
  thrityv_row  = quantile(as.numeric(row_vol), probs = 0.3, 
                          na.rm = TRUE)
  seventyv_row  = quantile(as.numeric(row_vol), probs = 0.7, 
                           na.rm = TRUE) 
  
  #small_med match value which lower than median
  vol_med = which(row_vol > seventyv_row)
  neutralv_med = which((row_vol <= seventyv_row) 
                       & (row_vol >= thrityv_row))
  calm_med = which(row_vol < thrityv_row)
  
  #Keep value by listing it out in the created lists
  Calm_index[[j]] =   calm_med
  NeutralV_index[[j]] =  neutralv_med 
  Vol_index[[j]] =  vol_med
}

#///////////////////////////////////////////////////////////////
#--------------------------Intersect----------------------------
#///////////////////////////////////////////////////////////////

#Create list
SC_index = list(nrow(Return))
SNv_index = list(nrow(Return))
SV_index = list(nrow(Return))

BC_index = list(nrow(Return))
BNv_index = list(nrow(Return))
BV_index = list(nrow(Return))

for(k in 1:nrow(Return)){
  
  SC_inter = intersect(Small_index[[k]],Calm_index[[k]])
  SNv_inter = intersect(Small_index[[k]],NeutralV_index[[k]])
  SV_inter = intersect(Small_index[[k]],Vol_index[[k]])
  
  BC_inter = intersect(Big_index[[k]],Calm_index[[k]])
  BNv_inter = intersect(Big_index[[k]],NeutralV_index[[k]])
  BV_inter = intersect(Big_index[[k]],Vol_index[[k]])
  
  #Keep value by listing it out in the created lists
  SC_index[[k]] =  SC_inter
  SNv_index[[k]] =  SNv_inter
  SV_index[[k]] =  SV_inter
  
  BC_index[[k]] =  BC_inter
  BNv_index[[k]] =  BNv_inter
  BV_index[[k]] =  BV_inter
}

#///////////////////////////////////////////////////////////////
#---------------------Tarced back return------------------------
#///////////////////////////////////////////////////////////////

#Create lists 
SC_ret = list(nrow(ret))
SNv_ret = list(nrow(ret))
SV_ret = list(nrow(ret))

BC_ret = list(nrow(ret))
BNv_ret = list(nrow(ret))
BV_ret = list(nrow(ret))

#For loop to match ID back to return from each row in list
for(p in 1: nrow(ret)){
  
  #Temporarily create macthing value for each ID and return
  sc_ret = unlist(ret[p,SC_index[[p]]])
  snv_ret = unlist(ret[p,SNv_index[[p]]])
  sv_ret = unlist(ret[p,SV_index[[p]]])
  
  bc_ret = unlist(ret[p,BC_index[[p]]])
  bnv_ret = unlist(ret[p,BNv_index[[p]]])
  bv_ret = unlist(ret[p,BV_index[[p]]])
  
  #Keep value by listing it out in the created lists
  SC_ret[[p]] =   as.numeric(sc_ret)
  SNv_ret[[p]] =   as.numeric(snv_ret)
  SV_ret[[p]] =   as.numeric(sv_ret)
  
  BC_ret[[p]] =   as.numeric(bc_ret)
  BNv_ret[[p]] =   as.numeric(bnv_ret)
  BV_ret[[p]] =   as.numeric(bv_ret)
}

#///////////////////////////////////////////////////////////////
#-------------------Mean return by period-----------------------
#///////////////////////////////////////////////////////////////

#Created Weights
SC_weight = list(nrow(ret))
SNv_weight = list(nrow(ret))
SV_weight = list(nrow(ret))

BC_weight = list(nrow(ret))
BNv_weight = list(nrow(ret))
BV_weight = list(nrow(ret))

for(p in 1: nrow(ret)){
  
  sc_weight = unlist(sizes[p,SC_index[[p]]])
  snv_weight = unlist(sizes[p,SNv_index[[p]]])
  sv_weight = unlist(sizes[p,SV_index[[p]]])
  
  bc_weight = unlist(sizes[p,BC_index[[p]]])
  bnv_weight = unlist(sizes[p,BNv_index[[p]]])
  bv_weight = unlist(sizes[p,BV_index[[p]]])
  
  SC_weight[[p]] = as.numeric(sc_weight)
  SNv_weight[[p]] = as.numeric(snv_weight)
  SV_weight[[p]] = as.numeric(sv_weight)
  
  BC_weight[[p]] = as.numeric(bc_weight)
  BNv_weight[[p]] = as.numeric(bnv_weight)
  BV_weight[[p]] = as.numeric(bv_weight)
}

#Create lists to keep return value for Big and Small
SC_ret_t = list(nrow(ret))
SNv_ret_t = list(nrow(ret))
SV_ret_t = list(nrow(ret))

BC_ret_t = list(nrow(ret))
BNv_ret_t = list(nrow(ret))
BV_ret_t = list(nrow(ret))

for(p in 1: nrow(ret)){
  
  #Calculate the average reurn fot that period
  sc_ret_t = weighted.mean(as.numeric(SC_ret[[p]]),
                           SC_weight[[p]], na.rm = TRUE)
  snv_ret_t = weighted.mean(as.numeric(SNv_ret[[p]]),
                            SNv_weight[[p]], na.rm = TRUE)
  sv_ret_t = weighted.mean(as.numeric(SV_ret[[p]]),
                           SV_weight[[p]], na.rm = TRUE)
  
  bc_ret_t = weighted.mean(as.numeric(BC_ret[[p]]),
                           BC_weight[[p]], na.rm = TRUE)
  bnv_ret_t = weighted.mean(as.numeric(BNv_ret[[p]]),
                            BNv_weight[[p]], na.rm = TRUE)
  bv_ret_t = weighted.mean(as.numeric(BV_ret[[p]]),
                           BV_weight[[p]], na.rm = TRUE)
  
  #Keep value by listing it out in the created lists
  SC_ret_t[[p]] =   as.numeric(sc_ret_t)
  SNv_ret_t[[p]] =   as.numeric(snv_ret_t)
  SV_ret_t[[p]] =   as.numeric(sv_ret_t)
  
  BC_ret_t[[p]] =   as.numeric(bc_ret_t)
  BNv_ret_t[[p]] =   as.numeric(bnv_ret_t)
  BV_ret_t[[p]] =   as.numeric(bv_ret_t)
}

#Chnage from NaN to zero for lists
SC_ret_t_z = rapply( SC_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
SNv_ret_t_z = rapply( SNv_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
SV_ret_t_z = rapply( SV_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )

BC_ret_t_z = rapply( BC_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BNv_ret_t_z = rapply( BNv_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BV_ret_t_z = rapply( BV_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )

#///////////////////////////////////////////////////////////////
#--------------------COMBINE AS MATRIX--------------------------
#///////////////////////////////////////////////////////////////

#Create the matrix
SC_Port = matrix(NA,ncol=1,nrow=nrow(ret))
SNv_Port = matrix(NA,ncol=1,nrow=nrow(ret))
SV_Port = matrix(NA,ncol=1,nrow=nrow(ret))

BC_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BNv_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BV_Port = matrix(NA,ncol=1,nrow=nrow(ret))

#For loop to combine 
for(q in 1:nrow(ret)){
  
  #create temporary value
  temp1 = SC_ret_t_z[[q]]
  temp2 = SNv_ret_t_z[[q]]
  temp3 = SV_ret_t_z[[q]]
  
  temp4 = BC_ret_t_z[[q]]
  temp5 = BNv_ret_t_z[[q]]
  temp6 = BV_ret_t_z[[q]]
  
  #put the value in matrix
  SC_Port[q] = temp1
  SNv_Port[q] = temp2
  SV_Port[q] = temp3
  
  BC_Port[q] = temp4
  BNv_Port[q] = temp5
  BV_Port[q] = temp6
}

#Get overall portfolio return
SC_all_ret = mean(SC_Port)
SNv_all_ret = mean(SNv_Port)
SV_all_ret = mean(SV_Port)

BC_all_ret = mean(BC_Port)
BNv_all_ret = mean(BNv_Port)
BV_all_ret = mean(BV_Port)


#///////////////////////////////////////////////////////////////
#----------------SMB, HML, UMD, CMV Port------------------------
#///////////////////////////////////////////////////////////////

SMB_BM_Port = ((SH_Port + SN_Port + SL_Port)/3 - 
  (BH_Port + BN_Port + BL_Port)/3)

SMB_MoM_Port = ((SU_Port + SNm_Port + SD_Port)/3 - 
  (BU_Port + BNm_Port + BD_Port)/3)

SMB_Vol_Port = ((SC_Port + SNv_Port + SV_Port)/3 - 
  (BC_Port + BNv_Port + BV_Port)/3)

SMB_Port = (SMB_BM_Port + SMB_MoM_Port + SMB_Vol_Port)/3

HML_Port = (SH_Port + BH_Port)/2 - (SL_Port + BL_Port)/2

UMD_Port = (SU_Port + BU_Port)/2 - (SD_Port + BD_Port)/2

CMV_Port = (SC_Port + BC_Port)/2 - (SV_Port + BV_Port)/2

#///////////////////////////////////////////////////////////////
#---------------------Summary Statistics------------------------
#///////////////////////////////////////////////////////////////

#Get overall portfolio return
SMB_BM_all_ret = mean(SMB_BM_Port)
SMB_MoM_all_ret = mean(SMB_MoM_Port)
SMB_Vol_all_ret = mean(SMB_Vol_Port)

SMB_Mean = mean(SMB_Port)
HML_Mean = mean(HML_Port)
UMD_Mean = mean(UMD_Port)
CMV_Mean = mean(CMV_Port)

SMB_SD = sd(SMB_Port)
HML_SD = sd(HML_Port)
UMD_SD = sd(UMD_Port)
CMV_SD = sd(CMV_Port)

SMB_t = t.test(as.vector(SMB_Port))
HML_t = t.test(as.vector(HML_Port))
UMD_t = t.test(as.vector(UMD_Port))
CMV_t = t.test(as.vector(CMV_Port))

#Create Matrix
STAT_sum = matrix(NA,ncol=4,nrow=3)

STAT_sum[1,1] = SMB_Mean*12
STAT_sum[2,1] = SMB_SD*sqrt(12)
STAT_sum[3,1] = SMB_t[[1]]

STAT_sum[1,2] = HML_Mean*12
STAT_sum[2,2] = HML_SD*sqrt(12)
STAT_sum[3,2] = HML_t[[1]]

STAT_sum[1,3] = UMD_Mean*12
STAT_sum[2,3] = UMD_SD*sqrt(12)
STAT_sum[3,3] = UMD_t[[1]]

STAT_sum[1,4] = CMV_Mean*12
STAT_sum[2,4] = CMV_SD*sqrt(12)
STAT_sum[3,4] = CMV_t[[1]]

#///////////////////////////////////////////////////////////////
#------------------Statistics S B Factors-----------------------
#///////////////////////////////////////////////////////////////

#Create the desired variables
HML_S_Port = SH_Port - SL_Port
HML_B_Port = BH_Port - BL_Port
HML_SB_Port = HML_S_Port - HML_B_Port

UMD_S_Port = SU_Port - SD_Port
UMD_B_Port = BU_Port - BD_Port
UMD_SB_Port = UMD_S_Port - UMD_B_Port

CMV_S_Port = SC_Port - SV_Port
CMV_B_Port = BC_Port - BV_Port
CMV_SB_Port = CMV_S_Port - CMV_B_Port

#Calculate their statistics
HML_S_Mean = mean(HML_S_Port)
UMD_S_Mean = mean(UMD_S_Port)
CMV_S_Mean = mean(CMV_S_Port)

HML_S_SD = sd(HML_S_Port)
UMD_S_SD = sd(UMD_S_Port)
CMV_S_SD = sd(CMV_S_Port)

HML_S_t = t.test(as.vector(HML_S_Port))
UMD_S_t = t.test(as.vector(UMD_S_Port))
CMV_S_t = t.test(as.vector(CMV_S_Port))


HML_B_Mean = mean(HML_B_Port)
UMD_B_Mean = mean(UMD_B_Port)
CMV_B_Mean = mean(CMV_B_Port)

HML_B_SD = sd(HML_B_Port)
UMD_B_SD = sd(UMD_B_Port)
CMV_B_SD = sd(CMV_B_Port)

HML_B_t = t.test(as.vector(HML_B_Port))
UMD_B_t = t.test(as.vector(UMD_B_Port))
CMV_B_t = t.test(as.vector(CMV_B_Port))


HML_SB_Mean = mean(HML_SB_Port)
UMD_SB_Mean = mean(UMD_SB_Port)
CMV_SB_Mean = mean(CMV_SB_Port)

HML_SB_SD = sd(HML_SB_Port)
UMD_SB_SD = sd(UMD_SB_Port)
CMV_SB_SD = sd(CMV_SB_Port)

HML_SB_t = t.test(as.vector(HML_SB_Port))
UMD_SB_t = t.test(as.vector(UMD_SB_Port))
CMV_SB_t = t.test(as.vector(CMV_SB_Port))

#Matrix for each vriables
#Create Matrix
STAT_HML_SB = matrix(NA,ncol=3,nrow=3)

STAT_HML_SB[1,1] = HML_S_Mean*12
STAT_HML_SB[2,1] = HML_S_SD*sqrt(12)
STAT_HML_SB[3,1] = HML_S_t[[1]]

STAT_HML_SB[1,2] = HML_B_Mean*12
STAT_HML_SB[2,2] = HML_B_SD*sqrt(12)
STAT_HML_SB[3,2] = HML_B_t[[1]]

STAT_HML_SB[1,3] = HML_SB_Mean*12
STAT_HML_SB[2,3] = HML_SB_SD*sqrt(12)
STAT_HML_SB[3,3] = HML_SB_t[[1]]


STAT_UMD_SB = matrix(NA,ncol=3,nrow=3)

STAT_UMD_SB[1,1] = UMD_S_Mean*12
STAT_UMD_SB[2,1] = UMD_S_SD*sqrt(12)
STAT_UMD_SB[3,1] = UMD_S_t[[1]]

STAT_UMD_SB[1,2] = UMD_B_Mean*12
STAT_UMD_SB[2,2] = UMD_B_SD*sqrt(12)
STAT_UMD_SB[3,2] = UMD_B_t[[1]]

STAT_UMD_SB[1,3] = UMD_SB_Mean*12
STAT_UMD_SB[2,3] = UMD_SB_SD*sqrt(12)
STAT_UMD_SB[3,3] = UMD_SB_t[[1]]


STAT_CMV_SB = matrix(NA,ncol=3,nrow=3)

STAT_CMV_SB[1,1] = CMV_S_Mean*12
STAT_CMV_SB[2,1] = CMV_S_SD*sqrt(12)
STAT_CMV_SB[3,1] = CMV_S_t[[1]]

STAT_CMV_SB[1,2] = CMV_B_Mean*12
STAT_CMV_SB[2,2] = CMV_B_SD*sqrt(12)
STAT_CMV_SB[3,2] = CMV_B_t[[1]]

STAT_CMV_SB[1,3] = CMV_SB_Mean*12
STAT_CMV_SB[2,3] = CMV_SB_SD*sqrt(12)
STAT_CMV_SB[3,3] = CMV_SB_t[[1]]

#///////////////////////////////////////////////////////////////
#-------------------Corr btw Diff Factors-----------------------
#///////////////////////////////////////////////////////////////

ExRm_ExRm = cor(ExcessRm,ExcessRm)
ExRm_SMB = cor(ExcessRm,SMB_Port)
ExRm_HML = cor(ExcessRm,HML_Port)
ExRm_UMD = cor(ExcessRm,UMD_Port)
ExRm_CMV = cor(ExcessRm,CMV_Port)

SMB_SMB = cor(SMB_Port,SMB_Port)
SMB_HML = cor(SMB_Port,HML_Port)
SMB_UMD = cor(SMB_Port,UMD_Port)
SMB_CMV = cor(SMB_Port,CMV_Port)

HML_HML = cor(HML_Port,HML_Port)
HML_UMD = cor(HML_Port,UMD_Port)
HML_CMV = cor(HML_Port,CMV_Port)

UMD_UMD = cor(UMD_Port,UMD_Port)
UMD_CMV = cor(UMD_Port,CMV_Port)

CMV_CMV = cor(CMV_Port,CMV_Port)

#Create Matrix
Facs_cor = matrix(NA,nrow=5,ncol=5)

Facs_cor[1,1] = ExRm_ExRm
Facs_cor[2,1] = ExRm_SMB
Facs_cor[3,1] = ExRm_HML
Facs_cor[4,1] = ExRm_UMD
Facs_cor[5,1] = ExRm_CMV

Facs_cor[1,2] = ExRm_SMB
Facs_cor[2,2] = SMB_SMB
Facs_cor[3,2] = SMB_HML
Facs_cor[4,2] = SMB_UMD
Facs_cor[5,2] = SMB_CMV

Facs_cor[1,3] = ExRm_HML
Facs_cor[2,3] = SMB_HML
Facs_cor[3,3] = HML_HML
Facs_cor[4,3] = HML_UMD
Facs_cor[5,3] = HML_CMV

Facs_cor[1,4] = ExRm_UMD
Facs_cor[2,4] = SMB_UMD
Facs_cor[3,4] = HML_UMD
Facs_cor[4,4] = UMD_UMD
Facs_cor[5,4] = UMD_CMV

Facs_cor[1,5] = ExRm_CMV
Facs_cor[2,5] = SMB_CMV
Facs_cor[3,5] = HML_CMV
Facs_cor[4,5] = UMD_CMV
Facs_cor[5,5] = CMV_CMV

#///////////////////////////////////////////////////////////////
#-------------------Reg four explain fifth----------------------
#///////////////////////////////////////////////////////////////

Facs_reg1 = lm(ExcessRm ~ 
                SMB_Port + HML_Port + UMD_Port + CMV_Port )
Facs_reg2 = lm(SMB_Port ~ 
                  ExcessRm + HML_Port + UMD_Port + CMV_Port )
Facs_reg3 = lm(HML_Port ~ 
                 ExcessRm + SMB_Port + UMD_Port + CMV_Port )
Facs_reg4 = lm(UMD_Port ~ 
                 ExcessRm + SMB_Port + HML_Port + CMV_Port )
Facs_reg5 = lm(CMV_Port ~ 
                 ExcessRm + SMB_Port + HML_Port + UMD_Port)

#Create Matrix

Facs_ExRm_reg = matrix(NA,ncol=7,nrow=3)

Facs_ExRm_reg[1,1] = Facs_reg1$coeff[1]
Facs_ExRm_reg[1,3] = Facs_reg1$coeff[2]
Facs_ExRm_reg[1,4] = Facs_reg1$coeff[3]
Facs_ExRm_reg[1,5] = Facs_reg1$coeff[4]
Facs_ExRm_reg[1,6] = Facs_reg1$coeff[5]
Facs_ExRm_reg[1,7] = summary(Facs_reg1)$r.squared
Facs_ExRm_reg[2,1] = summary(Facs_reg1)$coeff[1,3]
Facs_ExRm_reg[2,3] = summary(Facs_reg1)$coeff[2,3]
Facs_ExRm_reg[2,4] = summary(Facs_reg1)$coeff[3,3]
Facs_ExRm_reg[2,5] = summary(Facs_reg1)$coeff[4,3]
Facs_ExRm_reg[2,6] = summary(Facs_reg1)$coeff[5,3]
Facs_ExRm_reg[3,1] = summary(Facs_reg1)$coeff[1,4]
Facs_ExRm_reg[3,3] = summary(Facs_reg1)$coeff[2,4]
Facs_ExRm_reg[3,4] = summary(Facs_reg1)$coeff[3,4]
Facs_ExRm_reg[3,5] = summary(Facs_reg1)$coeff[4,4]
Facs_ExRm_reg[3,6] = summary(Facs_reg1)$coeff[5,4]


Facs_SMB_reg = matrix(NA,ncol=7,nrow=3)

Facs_SMB_reg[1,1] = Facs_reg2$coeff[1]
Facs_SMB_reg[1,2] = Facs_reg2$coeff[2]
Facs_SMB_reg[1,4] = Facs_reg2$coeff[3]
Facs_SMB_reg[1,5] = Facs_reg2$coeff[4]
Facs_SMB_reg[1,6] = Facs_reg2$coeff[5]
Facs_SMB_reg[1,7] = summary(Facs_reg2)$r.squared
Facs_SMB_reg[2,1] = summary(Facs_reg2)$coeff[1,3]
Facs_SMB_reg[2,2] = summary(Facs_reg2)$coeff[2,3]
Facs_SMB_reg[2,4] = summary(Facs_reg2)$coeff[3,3]
Facs_SMB_reg[2,5] = summary(Facs_reg2)$coeff[4,3]
Facs_SMB_reg[2,6] = summary(Facs_reg2)$coeff[5,3]
Facs_SMB_reg[3,1] = summary(Facs_reg2)$coeff[1,4]
Facs_SMB_reg[3,2] = summary(Facs_reg2)$coeff[2,4]
Facs_SMB_reg[3,4] = summary(Facs_reg2)$coeff[3,4]
Facs_SMB_reg[3,5] = summary(Facs_reg2)$coeff[4,4]
Facs_SMB_reg[3,6] = summary(Facs_reg2)$coeff[5,4]


Facs_HML_reg = matrix(NA,ncol=7,nrow=3)

Facs_HML_reg[1,1] = Facs_reg3$coeff[1]
Facs_HML_reg[1,2] = Facs_reg3$coeff[2]
Facs_HML_reg[1,3] = Facs_reg3$coeff[3]
Facs_HML_reg[1,5] = Facs_reg3$coeff[4]
Facs_HML_reg[1,6] = Facs_reg3$coeff[5]
Facs_HML_reg[1,7] = summary(Facs_reg3)$r.squared
Facs_HML_reg[2,1] = summary(Facs_reg3)$coeff[1,3]
Facs_HML_reg[2,2] = summary(Facs_reg3)$coeff[2,3]
Facs_HML_reg[2,3] = summary(Facs_reg3)$coeff[3,3]
Facs_HML_reg[2,5] = summary(Facs_reg3)$coeff[4,3]
Facs_HML_reg[2,6] = summary(Facs_reg3)$coeff[5,3]
Facs_HML_reg[3,1] = summary(Facs_reg3)$coeff[1,4]
Facs_HML_reg[3,2] = summary(Facs_reg3)$coeff[2,4]
Facs_HML_reg[3,3] = summary(Facs_reg3)$coeff[3,4]
Facs_HML_reg[3,5] = summary(Facs_reg3)$coeff[4,4]
Facs_HML_reg[3,6] = summary(Facs_reg3)$coeff[5,4]


Facs_UMD_reg = matrix(NA,ncol=7,nrow=3)

Facs_UMD_reg[1,1] = Facs_reg4$coeff[1]
Facs_UMD_reg[1,2] = Facs_reg4$coeff[2]
Facs_UMD_reg[1,3] = Facs_reg4$coeff[3]
Facs_UMD_reg[1,4] = Facs_reg4$coeff[4]
Facs_UMD_reg[1,6] = Facs_reg4$coeff[5]
Facs_UMD_reg[1,7] = summary(Facs_reg4)$r.squared
Facs_UMD_reg[2,1] = summary(Facs_reg4)$coeff[1,3]
Facs_UMD_reg[2,2] = summary(Facs_reg4)$coeff[2,3]
Facs_UMD_reg[2,3] = summary(Facs_reg4)$coeff[3,3]
Facs_UMD_reg[2,4] = summary(Facs_reg4)$coeff[4,3]
Facs_UMD_reg[2,6] = summary(Facs_reg4)$coeff[5,3]
Facs_UMD_reg[3,1] = summary(Facs_reg4)$coeff[1,4]
Facs_UMD_reg[3,2] = summary(Facs_reg4)$coeff[2,4]
Facs_UMD_reg[3,3] = summary(Facs_reg4)$coeff[3,4]
Facs_UMD_reg[3,4] = summary(Facs_reg4)$coeff[4,4]
Facs_UMD_reg[3,6] = summary(Facs_reg4)$coeff[5,4]


Facs_CMV_reg = matrix(NA,ncol=7,nrow=3)

Facs_CMV_reg[1,1] = Facs_reg5$coeff[1]
Facs_CMV_reg[1,2] = Facs_reg5$coeff[2]
Facs_CMV_reg[1,3] = Facs_reg5$coeff[3]
Facs_CMV_reg[1,4] = Facs_reg5$coeff[4]
Facs_CMV_reg[1,5] = Facs_reg5$coeff[5]
Facs_CMV_reg[1,7] = summary(Facs_reg5)$r.squared
Facs_CMV_reg[2,1] = summary(Facs_reg5)$coeff[1,3]
Facs_CMV_reg[2,2] = summary(Facs_reg5)$coeff[2,3]
Facs_CMV_reg[2,3] = summary(Facs_reg5)$coeff[3,3]
Facs_CMV_reg[2,4] = summary(Facs_reg5)$coeff[4,3]
Facs_CMV_reg[2,5] = summary(Facs_reg5)$coeff[5,3]
Facs_CMV_reg[3,1] = summary(Facs_reg5)$coeff[1,4]
Facs_CMV_reg[3,2] = summary(Facs_reg5)$coeff[2,4]
Facs_CMV_reg[3,3] = summary(Facs_reg5)$coeff[3,4]
Facs_CMV_reg[3,4] = summary(Facs_reg5)$coeff[4,4]
Facs_CMV_reg[3,5] = summary(Facs_reg5)$coeff[5,4]

