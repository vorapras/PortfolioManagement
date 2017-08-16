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
BH_index = list(nrow(Return))
BL_index = list(nrow(Return))
UC_index = list(nrow(Return))
UV_index = list(nrow(Return))
DC_index = list(nrow(Return))
DV_index = list(nrow(Return))

for(k in 1:nrow(Return)){
  
  SH_inter = intersect(Small_index[[k]],High_index[[k]])
  SL_inter = intersect(Small_index[[k]],Low_index[[k]])
  BH_inter = intersect(Big_index[[k]],High_index[[k]])
  BL_inter = intersect(Big_index[[k]],Low_index[[k]])
  UC_inter = intersect(Up_index[[k]],Calm_index[[k]])
  UV_inter = intersect(Up_index[[k]],Vol_index[[k]])
  DC_inter = intersect(Down_index[[k]],Calm_index[[k]])
  DV_inter = intersect(Down_index[[k]],Vol_index[[k]])
  
  #Keep value by listing it out in the created lists
  SH_index[[k]] =  SH_inter
  SL_index[[k]] =  SL_inter
  BH_index[[k]] =  BH_inter
  BL_index[[k]] =  BL_inter
  UC_index[[k]] =  UC_inter
  UV_index[[k]] =  UV_inter
  DC_index[[k]] =  DC_inter
  DV_index[[k]] =  DV_inter
}


#Create list
SHUC_index = list(nrow(Return))
SHUV_index = list(nrow(Return))
SHDC_index = list(nrow(Return))
SHDV_index = list(nrow(Return))
SLUC_index = list(nrow(Return))
SLUV_index = list(nrow(Return))
SLDC_index = list(nrow(Return))
SLDV_index = list(nrow(Return))

BHUC_index = list(nrow(Return))
BHUV_index = list(nrow(Return))
BHDC_index = list(nrow(Return))
BHDV_index = list(nrow(Return))
BLUC_index = list(nrow(Return))
BLUV_index = list(nrow(Return))
BLDC_index = list(nrow(Return))
BLDV_index = list(nrow(Return))


for(k in 1:nrow(Return)){
  
  SHUC_inter = intersect(SH_index[[k]],UC_index[[k]])
  SHUV_inter = intersect(SH_index[[k]],UV_index[[k]])
  SHDC_inter = intersect(SH_index[[k]],DC_index[[k]])
  SHDV_inter = intersect(SH_index[[k]],DV_index[[k]])
  SLUC_inter = intersect(SL_index[[k]],UC_index[[k]])
  SLUV_inter = intersect(SL_index[[k]],UV_index[[k]])
  SLDC_inter = intersect(SL_index[[k]],DC_index[[k]])
  SLDV_inter = intersect(SL_index[[k]],DV_index[[k]])
  
  BHUC_inter = intersect(BH_index[[k]],UC_index[[k]])
  BHUV_inter = intersect(BH_index[[k]],UV_index[[k]])
  BHDC_inter = intersect(BH_index[[k]],DC_index[[k]])
  BHDV_inter = intersect(BH_index[[k]],DV_index[[k]])
  BLUC_inter = intersect(BL_index[[k]],UC_index[[k]])
  BLUV_inter = intersect(BL_index[[k]],UV_index[[k]])
  BLDC_inter = intersect(BL_index[[k]],DC_index[[k]])
  BLDV_inter = intersect(BL_index[[k]],DV_index[[k]])
  
  
  #Keep value by listing it out in the created lists
  SHUC_index[[k]] =  SHUC_inter
  SHUV_index[[k]] =  SHUV_inter
  SHDC_index[[k]] =  SHDC_inter
  SHDV_index[[k]] =  SHDV_inter
  SLUC_index[[k]] =  SLUC_inter
  SLUV_index[[k]] =  SLUV_inter
  SLDC_index[[k]] =  SLDC_inter
  SLDV_index[[k]] =  SLDV_inter
  
  BHUC_index[[k]] =  BHUC_inter
  BHUV_index[[k]] =  BHUV_inter
  BHDC_index[[k]] =  BHDC_inter
  BHDV_index[[k]] =  BHDV_inter
  BLUC_index[[k]] =  BLUC_inter
  BLUV_index[[k]] =  BLUV_inter
  BLDC_index[[k]] =  BLDC_inter
  BLDV_index[[k]] =  BLDV_inter
}

#///////////////////////////////////////////////////////////////
#---------------------Tarced back return------------------------
#///////////////////////////////////////////////////////////////

#Create lists 
SHUC_ret = list(nrow(ret))
SHUV_ret = list(nrow(ret))
SHDC_ret = list(nrow(ret))
SHDV_ret = list(nrow(ret))
SLUC_ret = list(nrow(ret))
SLUV_ret = list(nrow(ret))
SLDC_ret = list(nrow(ret))
SLDV_ret = list(nrow(ret))

BHUC_ret = list(nrow(ret))
BHUV_ret = list(nrow(ret))
BHDC_ret = list(nrow(ret))
BHDV_ret = list(nrow(ret))
BLUC_ret = list(nrow(ret))
BLUV_ret = list(nrow(ret))
BLDC_ret = list(nrow(ret))
BLDV_ret = list(nrow(ret))


#For loop to match ID back to return from each row in list
for(p in 1: nrow(ret)){
  
  #Temporarily create macthing value for each ID and return
  shuc_ret = unlist(ret[p,SHUC_index[[p]]])
  shuv_ret = unlist(ret[p,SHUV_index[[p]]])
  shdc_ret = unlist(ret[p,SHDC_index[[p]]])
  shdv_ret = unlist(ret[p,SHDV_index[[p]]])
  sluc_ret = unlist(ret[p,SLUC_index[[p]]])
  sluv_ret = unlist(ret[p,SLUV_index[[p]]])
  sldc_ret = unlist(ret[p,SLDC_index[[p]]])
  sldv_ret = unlist(ret[p,SLDV_index[[p]]])
  
  bhuc_ret = unlist(ret[p,BHUC_index[[p]]])
  bhuv_ret = unlist(ret[p,BHUV_index[[p]]])
  bhdc_ret = unlist(ret[p,BHDC_index[[p]]])
  bhdv_ret = unlist(ret[p,BHDV_index[[p]]])
  bluc_ret = unlist(ret[p,BLUC_index[[p]]])
  bluv_ret = unlist(ret[p,BLUV_index[[p]]])
  bldc_ret = unlist(ret[p,BLDC_index[[p]]])
  bldv_ret = unlist(ret[p,BLDV_index[[p]]])
  
  #Keep value by listing it out in the created lists
  SHUC_ret[[p]] =   as.numeric(shuc_ret)
  SHUV_ret[[p]] =   as.numeric(shuv_ret)
  SHDC_ret[[p]] =   as.numeric(shdc_ret)
  SHDV_ret[[p]] =   as.numeric(shdv_ret)
  SLUC_ret[[p]] =   as.numeric(sluc_ret)
  SLUV_ret[[p]] =   as.numeric(sluv_ret)
  SLDC_ret[[p]] =   as.numeric(sldc_ret)
  SLDV_ret[[p]] =   as.numeric(sldv_ret)
  
  BHUC_ret[[p]] =   as.numeric(bhuc_ret)
  BHUV_ret[[p]] =   as.numeric(bhuv_ret)
  BHDC_ret[[p]] =   as.numeric(bhdc_ret)
  BHDV_ret[[p]] =   as.numeric(bhdv_ret)
  BLUC_ret[[p]] =   as.numeric(bluc_ret)
  BLUV_ret[[p]] =   as.numeric(bluv_ret)
  BLDC_ret[[p]] =   as.numeric(bldc_ret)
  BLDV_ret[[p]] =   as.numeric(bldv_ret)
}

#///////////////////////////////////////////////////////////////
#-------------------Mean return by period-----------------------
#///////////////////////////////////////////////////////////////

#Created Weights
SHUC_weight = list(nrow(ret))
SHUV_weight = list(nrow(ret))
SHDC_weight = list(nrow(ret))
SHDV_weight = list(nrow(ret))
SLUC_weight = list(nrow(ret))
SLUV_weight = list(nrow(ret))
SLDC_weight = list(nrow(ret))
SLDV_weight = list(nrow(ret))

BHUC_weight = list(nrow(ret))
BHUV_weight = list(nrow(ret))
BHDC_weight = list(nrow(ret))
BHDV_weight = list(nrow(ret))
BLUC_weight = list(nrow(ret))
BLUV_weight = list(nrow(ret))
BLDC_weight = list(nrow(ret))
BLDV_weight = list(nrow(ret))

for(p in 1: nrow(ret)){
  
  shuc_weight = unlist(sizes[p,SHUC_index[[p]]])
  shuv_weight = unlist(sizes[p,SHUV_index[[p]]])
  shdc_weight = unlist(sizes[p,SHDC_index[[p]]])
  shdv_weight = unlist(sizes[p,SHDV_index[[p]]])
  sluc_weight = unlist(sizes[p,SLUC_index[[p]]])
  sluv_weight = unlist(sizes[p,SLUV_index[[p]]])
  sldc_weight = unlist(sizes[p,SLDC_index[[p]]])
  sldv_weight = unlist(sizes[p,SLDV_index[[p]]])
  
  bhuc_weight = unlist(sizes[p,BHUC_index[[p]]])
  bhuv_weight = unlist(sizes[p,BHUV_index[[p]]])
  bhdc_weight = unlist(sizes[p,BHDC_index[[p]]])
  bhdv_weight = unlist(sizes[p,BHDV_index[[p]]])
  bluc_weight = unlist(sizes[p,BLUC_index[[p]]])
  bluv_weight = unlist(sizes[p,BLUV_index[[p]]])
  bldc_weight = unlist(sizes[p,BLDC_index[[p]]])
  bldv_weight = unlist(sizes[p,BLDV_index[[p]]])
  
  SHUC_weight[[p]] = as.numeric(shuc_weight)
  SHUV_weight[[p]] = as.numeric(shuv_weight)
  SHDC_weight[[p]] = as.numeric(shdc_weight)
  SHDV_weight[[p]] = as.numeric(shdv_weight)
  SLUC_weight[[p]] = as.numeric(sluc_weight)
  SLUV_weight[[p]] = as.numeric(sluv_weight)
  SLDC_weight[[p]] = as.numeric(sldc_weight)
  SLDV_weight[[p]] = as.numeric(sldv_weight)
  
  BHUC_weight[[p]] = as.numeric(bhuc_weight)
  BHUV_weight[[p]] = as.numeric(bhuv_weight)
  BHDC_weight[[p]] = as.numeric(bhdc_weight)
  BHDV_weight[[p]] = as.numeric(bhdv_weight)
  BLUC_weight[[p]] = as.numeric(bluc_weight)
  BLUV_weight[[p]] = as.numeric(bluv_weight)
  BLDC_weight[[p]] = as.numeric(bldc_weight)
  BLDV_weight[[p]] = as.numeric(bldv_weight)
}

#Create lists to keep return value
SHUC_ret_t = list(nrow(ret))
SHUV_ret_t = list(nrow(ret))
SHDC_ret_t = list(nrow(ret))
SHDV_ret_t = list(nrow(ret))
SLUC_ret_t = list(nrow(ret))
SLUV_ret_t = list(nrow(ret))
SLDC_ret_t = list(nrow(ret))
SLDV_ret_t = list(nrow(ret))

BHUC_ret_t = list(nrow(ret))
BHUV_ret_t = list(nrow(ret))
BHDC_ret_t = list(nrow(ret))
BHDV_ret_t = list(nrow(ret))
BLUC_ret_t = list(nrow(ret))
BLUV_ret_t = list(nrow(ret))
BLDC_ret_t = list(nrow(ret))
BLDV_ret_t = list(nrow(ret))

for(p in 1: nrow(ret)){
  
  #Calculate the average reurn fot that period
  shuc_ret_t = weighted.mean(as.numeric(SHUC_ret[[p]]),
                             SHUC_weight[[p]], na.rm = TRUE)
  shuv_ret_t = weighted.mean(as.numeric(SHUV_ret[[p]]),
                             SHUV_weight[[p]], na.rm = TRUE)
  shdc_ret_t = weighted.mean(as.numeric(SHDC_ret[[p]]),
                             SHDC_weight[[p]], na.rm = TRUE)
  shdv_ret_t = weighted.mean(as.numeric(SHDV_ret[[p]]),
                             SHDV_weight[[p]], na.rm = TRUE)
  sluc_ret_t = weighted.mean(as.numeric(SLUC_ret[[p]]),
                             SLUC_weight[[p]], na.rm = TRUE)
  sluv_ret_t = weighted.mean(as.numeric(SLUV_ret[[p]]),
                             SLUV_weight[[p]], na.rm = TRUE)
  sldc_ret_t = weighted.mean(as.numeric(SLDC_ret[[p]]),
                             SLDC_weight[[p]], na.rm = TRUE)
  sldv_ret_t = weighted.mean(as.numeric(SLDV_ret[[p]]),
                             SLDV_weight[[p]], na.rm = TRUE)
  
  bhuc_ret_t = weighted.mean(as.numeric(BHUC_ret[[p]]),
                             BHUC_weight[[p]], na.rm = TRUE)
  bhuv_ret_t = weighted.mean(as.numeric(BHUV_ret[[p]]),
                             BHUV_weight[[p]], na.rm = TRUE)
  bhdc_ret_t = weighted.mean(as.numeric(BHDC_ret[[p]]),
                             BHDC_weight[[p]], na.rm = TRUE)
  bhdv_ret_t = weighted.mean(as.numeric(BHDV_ret[[p]]),
                             BHDV_weight[[p]], na.rm = TRUE)
  bluc_ret_t = weighted.mean(as.numeric(BLUC_ret[[p]]),
                             BLUC_weight[[p]], na.rm = TRUE)
  bluv_ret_t = weighted.mean(as.numeric(BLUV_ret[[p]]),
                             BLUV_weight[[p]], na.rm = TRUE)
  bldc_ret_t = weighted.mean(as.numeric(BLDC_ret[[p]]),
                             BLDC_weight[[p]], na.rm = TRUE)
  bldv_ret_t = weighted.mean(as.numeric(BLDV_ret[[p]]),
                             BLDV_weight[[p]], na.rm = TRUE)
  
  #Keep value by listing it out in the created lists
  SHUC_ret_t[[p]] =   as.numeric(shuc_ret_t)
  SHUV_ret_t[[p]] =   as.numeric(shuv_ret_t)
  SHDC_ret_t[[p]] =   as.numeric(shdc_ret_t)
  SHDV_ret_t[[p]] =   as.numeric(shdv_ret_t)
  SLUC_ret_t[[p]] =   as.numeric(sluc_ret_t)
  SLUV_ret_t[[p]] =   as.numeric(sluv_ret_t)
  SLDC_ret_t[[p]] =   as.numeric(sldc_ret_t)
  SLDV_ret_t[[p]] =   as.numeric(sldv_ret_t)
  
  BHUC_ret_t[[p]] =   as.numeric(bhuc_ret_t)
  BHUV_ret_t[[p]] =   as.numeric(bhuv_ret_t)
  BHDC_ret_t[[p]] =   as.numeric(bhdc_ret_t)
  BHDV_ret_t[[p]] =   as.numeric(bhdv_ret_t)
  BLUC_ret_t[[p]] =   as.numeric(bluc_ret_t)
  BLUV_ret_t[[p]] =   as.numeric(bluv_ret_t)
  BLDC_ret_t[[p]] =   as.numeric(bldc_ret_t)
  BLDV_ret_t[[p]] =   as.numeric(bldv_ret_t)
}

#Chnage from NaN to zero for lists
SHUC_ret_t_z = rapply( SHUC_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
SHUV_ret_t_z = rapply( SHUV_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
SHDC_ret_t_z = rapply( SHDC_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
SHDV_ret_t_z = rapply( SHDV_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
SLUC_ret_t_z = rapply( SLUC_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
SLUV_ret_t_z = rapply( SLUV_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
SLDC_ret_t_z = rapply( SLDC_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
SLDV_ret_t_z = rapply( SLDV_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )

BHUC_ret_t_z = rapply( BHUC_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BHUV_ret_t_z = rapply( BHUV_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BHDC_ret_t_z = rapply( BHDC_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BHDV_ret_t_z = rapply( BHDV_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BLUC_ret_t_z = rapply( BLUC_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BLUV_ret_t_z = rapply( BLUV_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BLDC_ret_t_z = rapply( BLDC_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BLDV_ret_t_z = rapply( BLDV_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )


#///////////////////////////////////////////////////////////////
#--------------------COMBINE AS MATRIX--------------------------
#///////////////////////////////////////////////////////////////

#Create the matrix
SHUC_Port = matrix(NA,ncol=1,nrow=nrow(ret))
SHUV_Port = matrix(NA,ncol=1,nrow=nrow(ret))
SHDC_Port = matrix(NA,ncol=1,nrow=nrow(ret))
SHDV_Port = matrix(NA,ncol=1,nrow=nrow(ret))
SLUC_Port = matrix(NA,ncol=1,nrow=nrow(ret))
SLUV_Port = matrix(NA,ncol=1,nrow=nrow(ret))
SLDC_Port = matrix(NA,ncol=1,nrow=nrow(ret))
SLDV_Port = matrix(NA,ncol=1,nrow=nrow(ret))

BHUC_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BHUV_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BHDC_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BHDV_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BLUC_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BLUV_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BLDC_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BLDV_Port = matrix(NA,ncol=1,nrow=nrow(ret))


#For loop to combine 
for(q in 1:nrow(ret)){
  
  #create temporary value
  temp1 = SHUC_ret_t_z[[q]]
  temp2 = SHUV_ret_t_z[[q]]
  temp3 = SHDC_ret_t_z[[q]]
  temp4 = SHDV_ret_t_z[[q]]
  temp5 = SLUC_ret_t_z[[q]]
  temp6 = SLUV_ret_t_z[[q]]
  temp51 = SLDC_ret_t_z[[q]]
  temp61 = SLDV_ret_t_z[[q]]
  
  temp7 = BHUC_ret_t_z[[q]]
  temp8 = BHUV_ret_t_z[[q]]
  temp9 = BHDC_ret_t_z[[q]]
  temp10 = BHDV_ret_t_z[[q]]
  temp11 = BLUC_ret_t_z[[q]]
  temp12 = BLUV_ret_t_z[[q]]
  temp111 = BLDC_ret_t_z[[q]]
  temp121 = BLDV_ret_t_z[[q]]
  
  #put the value in matrix
  SHUC_Port[q] = temp1
  SHUV_Port[q] = temp2
  SHDC_Port[q] = temp3
  SHDV_Port[q] = temp4
  SLUC_Port[q] = temp5
  SLUV_Port[q] = temp6
  SLDC_Port[q] = temp51
  SLDV_Port[q] = temp61
  
  BHUC_Port[q] = temp7
  BHUV_Port[q] = temp8
  BHDC_Port[q] = temp9
  BHDV_Port[q] = temp10
  BLUC_Port[q] = temp11
  BLUV_Port[q] = temp12
  BLDC_Port[q] = temp111
  BLDV_Port[q] = temp121
}

#Get overall portfolio return
SHUC_all_ret = mean(SHUC_Port)
SHUV_all_ret = mean(SHUV_Port)
SHDC_all_ret = mean(SHDC_Port)
SHDV_all_ret = mean(SHDV_Port)
SLUC_all_ret = mean(SLUC_Port)
SLUV_all_ret = mean(SLUV_Port)
SLDC_all_ret = mean(SLDC_Port)
SLDV_all_ret = mean(SLDV_Port)

BHUC_all_ret = mean(BHUC_Port)
BHUV_all_ret = mean(BHUV_Port)
BHDC_all_ret = mean(BHDC_Port)
BHDV_all_ret = mean(BHDV_Port)
BLUC_all_ret = mean(BLUC_Port)
BLUV_all_ret = mean(BLUV_Port)
BLDC_all_ret = mean(BLDC_Port)
BLDV_all_ret = mean(BLDV_Port)

#///////////////////////////////////////////////////////////////
#----------------SMB, HML, UMD, CMV Port------------------------
#///////////////////////////////////////////////////////////////

SMB3_Port = ((SHUC_Port + SHUV_Port + SHDC_Port + SHDV_Port 
             + SLUC_Port + SLUV_Port + SLDC_Port + SLDV_Port)/8 
- (BHUC_Port + BHUV_Port + BHDC_Port + BHDV_Port + BLUC_Port 
   + BLUV_Port + BLDC_Port + BLDV_Port)/8) 

HML3_Port = ((SHUC_Port + SHUV_Port + SHDC_Port + SHDV_Port 
             + BHUC_Port + BHUV_Port + BHDC_Port + BHDV_Port)/8 
- (SLUC_Port + SLUV_Port + SLDC_Port + SLDV_Port + BLUC_Port 
   + BLUV_Port + BLDC_Port + BLDV_Port)/8) 

UMD3_Port = ((SHUC_Port + SHUV_Port + SLUC_Port + SLUV_Port 
             + BHUC_Port + BHUV_Port + BLUC_Port + BLUV_Port)/8 
- (SHDC_Port + SHDV_Port + SLDC_Port + SLDV_Port + BHDC_Port 
   + BHDV_Port + BLDC_Port + BLDV_Port)/8 )

CMV3_Port = ((SHUC_Port + SHDC_Port + SLUC_Port + SLDC_Port 
             + BHUC_Port + BHDC_Port + BLUC_Port + BLDC_Port)/8 
- (SHUV_Port + SHDV_Port + SLUV_Port + SLDV_Port + BHUV_Port 
   + BHDV_Port + BLUV_Port + BLDV_Port)/8 )

#///////////////////////////////////////////////////////////////
#---------------------Summary Statistics------------------------
#///////////////////////////////////////////////////////////////

#Get overall portfolio return
SMB3_Mean = mean(SMB3_Port)
HML3_Mean = mean(HML3_Port)
UMD3_Mean = mean(UMD3_Port)
CMV3_Mean = mean(CMV3_Port)

SMB3_SD = sd(SMB3_Port)
HML3_SD = sd(HML3_Port)
UMD3_SD = sd(UMD3_Port)
CMV3_SD = sd(CMV3_Port)

SMB3_t = t.test(as.vector(SMB3_Port))
HML3_t = t.test(as.vector(HML3_Port))
UMD3_t = t.test(as.vector(UMD3_Port))
CMV3_t = t.test(as.vector(CMV3_Port))

#Create Matrix
STAT3_sum = matrix(NA,ncol=4,nrow=3)

STAT3_sum[1,1] = SMB3_Mean*12
STAT3_sum[2,1] = SMB3_SD*sqrt(12)
STAT3_sum[3,1] = SMB3_t[[1]]

STAT3_sum[1,2] = HML3_Mean*12
STAT3_sum[2,2] = HML3_SD*sqrt(12)
STAT3_sum[3,2] = HML3_t[[1]]

STAT3_sum[1,3] = UMD3_Mean*12
STAT3_sum[2,3] = UMD3_SD*sqrt(12)
STAT3_sum[3,3] = UMD3_t[[1]]

STAT3_sum[1,4] = CMV3_Mean*12
STAT3_sum[2,4] = CMV3_SD*sqrt(12)
STAT3_sum[3,4] = CMV3_t[[1]]

#///////////////////////////////////////////////////////////////
#------------------Statistics S B Factors-----------------------
#///////////////////////////////////////////////////////////////

#Create the desired variables
HML3_S_Port = ((SHUC_Port + SHUV_Port + SHDC_Port + SHDV_Port)/4 
              - (SLUC_Port + SLUV_Port + SLDC_Port + SLDV_Port)/4) 
HML3_B_Port = ((BHUC_Port + BHUV_Port + BHDC_Port + BHDV_Port)/4 
              - (BLUC_Port + BLUV_Port + BLDC_Port + BLDV_Port)/4)
HML3_SB_Port = HML3_S_Port - HML3_B_Port

UMD3_S_Port = ((SHUC_Port + SHUV_Port + SLUC_Port + SLUV_Port)/4 
              - (SHDC_Port + SHDV_Port + SLDC_Port + SLDV_Port)/4)
UMD3_B_Port = ((BHUC_Port + BHUV_Port + BLUC_Port + BLUV_Port)/4 
              - (BHDC_Port + BHDV_Port + BLDC_Port + BLDV_Port)/4)
UMD3_SB_Port = UMD3_S_Port - UMD3_B_Port

CMV3_S_Port = ((SHUC_Port + SHDC_Port + SLUC_Port + SLDC_Port)/4 
              - (SHUV_Port + SHDV_Port + SLUV_Port + SLDV_Port)/4)
CMV3_B_Port = ((BHUC_Port + BHDC_Port + BLUC_Port + BLDC_Port)/4 
              - (BHUV_Port + BHDV_Port + BLUV_Port + BLDV_Port)/4)
CMV3_SB_Port = CMV3_S_Port - CMV3_B_Port

#Calculate their statistics
HML3_S_Mean = mean(HML3_S_Port)
UMD3_S_Mean = mean(UMD3_S_Port)
CMV3_S_Mean = mean(CMV3_S_Port)

HML3_S_SD = sd(HML3_S_Port)
UMD3_S_SD = sd(UMD3_S_Port)
CMV3_S_SD = sd(CMV3_S_Port)

HML3_S_t = t.test(as.vector(HML3_S_Port))
UMD3_S_t = t.test(as.vector(UMD3_S_Port))
CMV3_S_t = t.test(as.vector(CMV3_S_Port))


HML3_B_Mean = mean(HML3_B_Port)
UMD3_B_Mean = mean(UMD3_B_Port)
CMV3_B_Mean = mean(CMV3_B_Port)

HML3_B_SD = sd(HML3_B_Port)
UMD3_B_SD = sd(UMD3_B_Port)
CMV3_B_SD = sd(CMV3_B_Port)

HML3_B_t = t.test(as.vector(HML3_B_Port))
UMD3_B_t = t.test(as.vector(UMD3_B_Port))
CMV3_B_t = t.test(as.vector(CMV3_B_Port))


HML3_SB_Mean = mean(HML3_SB_Port)
UMD3_SB_Mean = mean(UMD3_SB_Port)
CMV3_SB_Mean = mean(CMV3_SB_Port)

HML3_SB_SD = sd(HML3_SB_Port)
UMD3_SB_SD = sd(UMD3_SB_Port)
CMV3_SB_SD = sd(CMV3_SB_Port)

HML3_SB_t = t.test(as.vector(HML3_SB_Port))
UMD3_SB_t = t.test(as.vector(UMD3_SB_Port))
CMV3_SB_t = t.test(as.vector(CMV3_SB_Port))

#Matrix for each vriables
#Create Matrix
STAT_HML3_SB = matrix(NA,ncol=3,nrow=3)

STAT_HML3_SB[1,1] = HML3_S_Mean*12
STAT_HML3_SB[2,1] = HML3_S_SD*sqrt(12)
STAT_HML3_SB[3,1] = HML3_S_t[[1]]

STAT_HML3_SB[1,2] = HML3_B_Mean*12
STAT_HML3_SB[2,2] = HML3_B_SD*sqrt(12)
STAT_HML3_SB[3,2] = HML3_B_t[[1]]

STAT_HML3_SB[1,3] = HML3_SB_Mean*12
STAT_HML3_SB[2,3] = HML3_SB_SD*sqrt(12)
STAT_HML3_SB[3,3] = HML3_SB_t[[1]]


STAT_UMD3_SB = matrix(NA,ncol=3,nrow=3)

STAT_UMD3_SB[1,1] = UMD3_S_Mean*12
STAT_UMD3_SB[2,1] = UMD3_S_SD*sqrt(12)
STAT_UMD3_SB[3,1] = UMD3_S_t[[1]]

STAT_UMD3_SB[1,2] = UMD3_B_Mean*12
STAT_UMD3_SB[2,2] = UMD3_B_SD*sqrt(12)
STAT_UMD3_SB[3,2] = UMD3_B_t[[1]]

STAT_UMD3_SB[1,3] = UMD3_SB_Mean*12
STAT_UMD3_SB[2,3] = UMD3_SB_SD*sqrt(12)
STAT_UMD3_SB[3,3] = UMD3_SB_t[[1]]


STAT_CMV3_SB = matrix(NA,ncol=3,nrow=3)

STAT_CMV3_SB[1,1] = CMV3_S_Mean*12
STAT_CMV3_SB[2,1] = CMV3_S_SD*sqrt(12)
STAT_CMV3_SB[3,1] = CMV3_S_t[[1]]

STAT_CMV3_SB[1,2] = CMV3_B_Mean*12
STAT_CMV3_SB[2,2] = CMV3_B_SD*sqrt(12)
STAT_CMV3_SB[3,2] = CMV3_B_t[[1]]

STAT_CMV3_SB[1,3] = CMV3_SB_Mean*12
STAT_CMV3_SB[2,3] = CMV3_SB_SD*sqrt(12)
STAT_CMV3_SB[3,3] = CMV3_SB_t[[1]]

#///////////////////////////////////////////////////////////////
#-------------------Corr btw Diff Factors-----------------------
#///////////////////////////////////////////////////////////////

ExRm_ExRm = cor(ExcessRm,ExcessRm)
ExRm_SMB = cor(ExcessRm,SMB3_Port)
ExRm_HML = cor(ExcessRm,HML3_Port)
ExRm_UMD = cor(ExcessRm,UMD3_Port)
ExRm_CMV = cor(ExcessRm,CMV3_Port)

SMB_SMB = cor(SMB3_Port,SMB3_Port)
SMB_HML = cor(SMB3_Port,HML3_Port)
SMB_UMD = cor(SMB3_Port,UMD3_Port)
SMB_CMV = cor(SMB3_Port,CMV3_Port)

HML_HML = cor(HML3_Port,HML3_Port)
HML_UMD = cor(HML3_Port,UMD3_Port)
HML_CMV = cor(HML3_Port,CMV3_Port)

UMD_UMD = cor(UMD3_Port,UMD3_Port)
UMD_CMV = cor(UMD3_Port,CMV3_Port)

CMV_CMV = cor(UMD3_Port,CMV3_Port)

#Create Matrix
Facs3_cor = matrix(NA,nrow=5,ncol=5)

Facs3_cor[1,1] = ExRm_ExRm
Facs3_cor[2,1] = ExRm_SMB
Facs3_cor[3,1] = ExRm_HML
Facs3_cor[4,1] = ExRm_UMD
Facs3_cor[5,1] = ExRm_CMV

Facs3_cor[1,2] = ExRm_SMB
Facs3_cor[2,2] = SMB_SMB
Facs3_cor[3,2] = SMB_HML
Facs3_cor[4,2] = SMB_UMD
Facs3_cor[5,2] = SMB_CMV

Facs3_cor[1,3] = ExRm_HML
Facs3_cor[2,3] = SMB_HML
Facs3_cor[3,3] = HML_HML
Facs3_cor[4,3] = HML_UMD
Facs3_cor[5,3] = HML_CMV

Facs3_cor[1,4] = ExRm_UMD
Facs3_cor[2,4] = SMB_UMD
Facs3_cor[3,4] = HML_UMD
Facs3_cor[4,4] = UMD_UMD
Facs3_cor[5,4] = UMD_CMV

Facs3_cor[1,5] = ExRm_CMV
Facs3_cor[2,5] = SMB_CMV
Facs3_cor[3,5] = HML_CMV
Facs3_cor[4,5] = UMD_CMV
Facs3_cor[5,5] = CMV_CMV

#///////////////////////////////////////////////////////////////
#-------------------Reg four explain fifth----------------------
#///////////////////////////////////////////////////////////////

Facs3_reg1 = lm(ExcessRm ~ 
                  SMB3_Port + HML3_Port + UMD3_Port + CMV3_Port )
Facs3_reg2 = lm(SMB3_Port ~ 
                  ExcessRm + HML3_Port + UMD3_Port + CMV3_Port )
Facs3_reg3 = lm(HML3_Port ~ 
                  ExcessRm + SMB3_Port + UMD3_Port + CMV3_Port )
Facs3_reg4 = lm(UMD3_Port ~ 
                  ExcessRm + SMB3_Port + HML3_Port + CMV3_Port )
Facs3_reg5 = lm(CMV3_Port ~ 
                  ExcessRm + SMB3_Port + HML3_Port + UMD3_Port)

#Create Matrix

Facs3_ExRm_reg = matrix(NA,ncol=7,nrow=3)

Facs3_ExRm_reg[1,1] = Facs3_reg1$coeff[1]
Facs3_ExRm_reg[1,3] = Facs3_reg1$coeff[2]
Facs3_ExRm_reg[1,4] = Facs3_reg1$coeff[3]
Facs3_ExRm_reg[1,5] = Facs3_reg1$coeff[4]
Facs3_ExRm_reg[1,6] = Facs3_reg1$coeff[5]
Facs3_ExRm_reg[1,7] = summary(Facs3_reg1)$r.squared
Facs3_ExRm_reg[2,1] = summary(Facs3_reg1)$coeff[1,3]
Facs3_ExRm_reg[2,3] = summary(Facs3_reg1)$coeff[2,3]
Facs3_ExRm_reg[2,4] = summary(Facs3_reg1)$coeff[3,3]
Facs3_ExRm_reg[2,5] = summary(Facs3_reg1)$coeff[4,3]
Facs3_ExRm_reg[2,6] = summary(Facs3_reg1)$coeff[5,3]
Facs3_ExRm_reg[3,1] = summary(Facs3_reg1)$coeff[1,4]
Facs3_ExRm_reg[3,3] = summary(Facs3_reg1)$coeff[2,4]
Facs3_ExRm_reg[3,4] = summary(Facs3_reg1)$coeff[3,4]
Facs3_ExRm_reg[3,5] = summary(Facs3_reg1)$coeff[4,4]
Facs3_ExRm_reg[3,6] = summary(Facs3_reg1)$coeff[5,4]


Facs3_SMB_reg = matrix(NA,ncol=7,nrow=3)

Facs3_SMB_reg[1,1] = Facs3_reg2$coeff[1]
Facs3_SMB_reg[1,2] = Facs3_reg2$coeff[2]
Facs3_SMB_reg[1,4] = Facs3_reg2$coeff[3]
Facs3_SMB_reg[1,5] = Facs3_reg2$coeff[4]
Facs3_SMB_reg[1,6] = Facs3_reg2$coeff[5]
Facs3_SMB_reg[1,7] = summary(Facs3_reg2)$r.squared
Facs3_SMB_reg[2,1] = summary(Facs3_reg2)$coeff[1,3]
Facs3_SMB_reg[2,2] = summary(Facs3_reg2)$coeff[2,3]
Facs3_SMB_reg[2,4] = summary(Facs3_reg2)$coeff[3,3]
Facs3_SMB_reg[2,5] = summary(Facs3_reg2)$coeff[4,3]
Facs3_SMB_reg[2,6] = summary(Facs3_reg2)$coeff[5,3]
Facs3_SMB_reg[3,1] = summary(Facs3_reg2)$coeff[1,4]
Facs3_SMB_reg[3,2] = summary(Facs3_reg2)$coeff[2,4]
Facs3_SMB_reg[3,4] = summary(Facs3_reg2)$coeff[3,4]
Facs3_SMB_reg[3,5] = summary(Facs3_reg2)$coeff[4,4]
Facs3_SMB_reg[3,6] = summary(Facs3_reg2)$coeff[5,4]


Facs3_HML_reg = matrix(NA,ncol=7,nrow=3)

Facs3_HML_reg[1,1] = Facs3_reg3$coeff[1]
Facs3_HML_reg[1,2] = Facs3_reg3$coeff[2]
Facs3_HML_reg[1,3] = Facs3_reg3$coeff[3]
Facs3_HML_reg[1,5] = Facs3_reg3$coeff[4]
Facs3_HML_reg[1,6] = Facs3_reg3$coeff[5]
Facs3_HML_reg[1,7] = summary(Facs3_reg3)$r.squared
Facs3_HML_reg[2,1] = summary(Facs3_reg3)$coeff[1,3]
Facs3_HML_reg[2,2] = summary(Facs3_reg3)$coeff[2,3]
Facs3_HML_reg[2,3] = summary(Facs3_reg3)$coeff[3,3]
Facs3_HML_reg[2,5] = summary(Facs3_reg3)$coeff[4,3]
Facs3_HML_reg[2,6] = summary(Facs3_reg3)$coeff[5,3]
Facs3_HML_reg[3,1] = summary(Facs3_reg3)$coeff[1,4]
Facs3_HML_reg[3,2] = summary(Facs3_reg3)$coeff[2,4]
Facs3_HML_reg[3,3] = summary(Facs3_reg3)$coeff[3,4]
Facs3_HML_reg[3,5] = summary(Facs3_reg3)$coeff[4,4]
Facs3_HML_reg[3,6] = summary(Facs3_reg3)$coeff[5,4]


Facs3_UMD_reg = matrix(NA,ncol=7,nrow=3)

Facs3_UMD_reg[1,1] = Facs3_reg4$coeff[1]
Facs3_UMD_reg[1,2] = Facs3_reg4$coeff[2]
Facs3_UMD_reg[1,3] = Facs3_reg4$coeff[3]
Facs3_UMD_reg[1,4] = Facs3_reg4$coeff[4]
Facs3_UMD_reg[1,6] = Facs3_reg4$coeff[5]
Facs3_UMD_reg[1,7] = summary(Facs3_reg4)$r.squared
Facs3_UMD_reg[2,1] = summary(Facs3_reg4)$coeff[1,3]
Facs3_UMD_reg[2,2] = summary(Facs3_reg4)$coeff[2,3]
Facs3_UMD_reg[2,3] = summary(Facs3_reg4)$coeff[3,3]
Facs3_UMD_reg[2,4] = summary(Facs3_reg4)$coeff[4,3]
Facs3_UMD_reg[2,6] = summary(Facs3_reg4)$coeff[5,3]
Facs3_UMD_reg[3,1] = summary(Facs3_reg4)$coeff[1,4]
Facs3_UMD_reg[3,2] = summary(Facs3_reg4)$coeff[2,4]
Facs3_UMD_reg[3,3] = summary(Facs3_reg4)$coeff[3,4]
Facs3_UMD_reg[3,4] = summary(Facs3_reg4)$coeff[4,4]
Facs3_UMD_reg[3,6] = summary(Facs3_reg4)$coeff[5,4]


Facs3_CMV_reg = matrix(NA,ncol=7,nrow=3)

Facs3_CMV_reg[1,1] = Facs3_reg5$coeff[1]
Facs3_CMV_reg[1,2] = Facs3_reg5$coeff[2]
Facs3_CMV_reg[1,3] = Facs3_reg5$coeff[3]
Facs3_CMV_reg[1,4] = Facs3_reg5$coeff[4]
Facs3_CMV_reg[1,5] = Facs3_reg5$coeff[5]
Facs3_CMV_reg[1,7] = summary(Facs3_reg5)$r.squared
Facs3_CMV_reg[2,1] = summary(Facs3_reg5)$coeff[1,3]
Facs3_CMV_reg[2,2] = summary(Facs3_reg5)$coeff[2,3]
Facs3_CMV_reg[2,3] = summary(Facs3_reg5)$coeff[3,3]
Facs3_CMV_reg[2,4] = summary(Facs3_reg5)$coeff[4,3]
Facs3_CMV_reg[2,5] = summary(Facs3_reg5)$coeff[5,3]
Facs3_CMV_reg[3,1] = summary(Facs3_reg5)$coeff[1,4]
Facs3_CMV_reg[3,2] = summary(Facs3_reg5)$coeff[2,4]
Facs3_CMV_reg[3,3] = summary(Facs3_reg5)$coeff[3,4]
Facs3_CMV_reg[3,4] = summary(Facs3_reg5)$coeff[4,4]
Facs3_CMV_reg[3,5] = summary(Facs3_reg5)$coeff[5,4]













