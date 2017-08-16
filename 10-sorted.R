#Call Library
library(readxl)
library(data.table)

#Import data
Return <- read_excel("~/Desktop/Data6M_CT.xlsx")
MoM <- read_excel("~/Desktop/Data6M_CT.xlsx", 
                  sheet = "Vol")
Rf<- read_excel("~/Desktop/Data6M_CT.xlsx", 
                sheet = "Rf")

#Create ret to macth back the return
#Take out dates and Rf ready to match by ID
ret = Return[,2:ncol(Return)] 

rf = Rf[,2]
Rf_ann = rf*(1/12)
Rf <- data.matrix(Rf_ann)

ExcessRm = Rm - Rf




#///////////////////////////////////////////////////////////////
#---------------SORTED momentum BY percentiles------------------
#///////////////////////////////////////////////////////////////

#Create "mom" from MoM Data column 2 to last one
#take dates in the first column out
mom = MoM[,2:ncol(MoM)]

#Create lists
M1_index = list(nrow(mom))
M2_index = list(nrow(mom))
M3_index = list(nrow(mom))
M4_index = list(nrow(mom))
M5_index = list(nrow(mom))
M6_index = list(nrow(mom))
M7_index = list(nrow(mom))
M8_index = list(nrow(mom))
M9_index = list(nrow(mom))
M10_index = list(nrow(mom))

#For Loop
for(j in 1: nrow(mom)){
  
  #Create row_mom for each rows
  #Take value out period by period
  row_mom = mom[j,]
  
  #Create break pionts from 30th and 70th percentile
  m1_row  = quantile(as.numeric(row_mom), probs = 0.1, 
                          na.rm = TRUE)
  m2_row  = quantile(as.numeric(row_mom), probs = 0.2, 
                           na.rm = TRUE) 
  m3_row  = quantile(as.numeric(row_mom), probs = 0.3, 
                     na.rm = TRUE) 
  m4_row  = quantile(as.numeric(row_mom), probs = 0.4, 
                     na.rm = TRUE) 
  m5_row  = quantile(as.numeric(row_mom), probs = 0.5, 
                     na.rm = TRUE) 
  m6_row  = quantile(as.numeric(row_mom), probs = 0.6, 
                     na.rm = TRUE) 
  m7_row  = quantile(as.numeric(row_mom), probs = 0.7, 
                     na.rm = TRUE) 
  m8_row  = quantile(as.numeric(row_mom), probs = 0.8, 
                     na.rm = TRUE) 
  m9_row  = quantile(as.numeric(row_mom), probs = 0.9, 
                     na.rm = TRUE) 
  
  #small_med match value which lower than median
  m1_med = which(row_mom < m1_row)
  m2_med = which((row_mom >= m1_row) 
                       & (row_mom < m2_row))
  m3_med = which((row_mom >= m2_row) 
                 & (row_mom < m3_row))
  m4_med = which((row_mom >= m3_row) 
                 & (row_mom < m4_row))
  m5_med = which((row_mom >= m4_row) 
                 & (row_mom < m5_row))
  m6_med = which((row_mom >= m5_row) 
                 & (row_mom <= m6_row))
  m7_med = which((row_mom > m6_row) 
                 & (row_mom <= m7_row))
  m8_med = which((row_mom > m7_row) 
                 & (row_mom <= m8_row))
  m9_med = which((row_mom > m8_row) 
                 & (row_mom <= m9_row))
  m10_med = which(row_mom > m9_row)
  
  #Keep value by listing it out in the created lists
  M1_index[[j]] =  m1_med
  M2_index[[j]] =  m2_med 
  M3_index[[j]] =  m3_med
  M4_index[[j]] =  m4_med
  M5_index[[j]] =  m5_med
  M6_index[[j]] =  m6_med
  M7_index[[j]] =  m7_med
  M8_index[[j]] =  m8_med
  M9_index[[j]] =  m9_med
  M10_index[[j]] =  m10_med
}

#///////////////////////////////////////////////////////////////
#---------------------Tarced back return------------------------
#///////////////////////////////////////////////////////////////

#Create lists 
M1_ret = list(nrow(ret))
M2_ret = list(nrow(ret))
M3_ret = list(nrow(ret))
M4_ret = list(nrow(ret))
M5_ret = list(nrow(ret))
M6_ret = list(nrow(ret))
M7_ret = list(nrow(ret))
M8_ret = list(nrow(ret))
M9_ret = list(nrow(ret))
M10_ret = list(nrow(ret))


#For loop to match ID back to return from each row in list
for(p in 1: nrow(ret)){
  
  #Temporarily create macthing value for each ID and return
  m_1_ret = unlist(ret[p,M1_index[[p]]])
  m_2_ret = unlist(ret[p,M2_index[[p]]])
  m_3_ret = unlist(ret[p,M3_index[[p]]])
  m_4_ret = unlist(ret[p,M4_index[[p]]])
  m_5_ret = unlist(ret[p,M5_index[[p]]])
  m_6_ret = unlist(ret[p,M6_index[[p]]])
  m_7_ret = unlist(ret[p,M7_index[[p]]])
  m_8_ret = unlist(ret[p,M8_index[[p]]])
  m_9_ret = unlist(ret[p,M9_index[[p]]])
  m_10_ret = unlist(ret[p,M10_index[[p]]])
  
  #Keep value by listing it out in the created lists
  M1_ret[[p]] =   as.numeric(m_1_ret)
  M2_ret[[p]] =   as.numeric(m_2_ret)
  M3_ret[[p]] =   as.numeric(m_3_ret)
  M4_ret[[p]] =   as.numeric(m_4_ret)
  M5_ret[[p]] =   as.numeric(m_5_ret)
  M6_ret[[p]] =   as.numeric(m_6_ret)
  M7_ret[[p]] =   as.numeric(m_7_ret)
  M8_ret[[p]] =   as.numeric(m_8_ret)
  M9_ret[[p]] =   as.numeric(m_9_ret)
  M10_ret[[p]] =   as.numeric(m_10_ret)
}

#///////////////////////////////////////////////////////////////
#-------------------Mean return by period-----------------------
#///////////////////////////////////////////////////////////////

#Create lists to keep return value for Big and Small
M1_ret_t = list(nrow(ret))
M2_ret_t = list(nrow(ret))
M3_ret_t = list(nrow(ret))
M4_ret_t = list(nrow(ret))
M5_ret_t = list(nrow(ret))
M6_ret_t = list(nrow(ret))
M7_ret_t = list(nrow(ret))
M8_ret_t = list(nrow(ret))
M9_ret_t = list(nrow(ret))
M10_ret_t = list(nrow(ret))

for(p in 1: nrow(ret)){
  
  #Calculate the average reurn fot that period
  m_1_ret_t = mean(as.numeric(M1_ret[[p]]), na.rm = TRUE)
  m_2_ret_t = mean(as.numeric(M2_ret[[p]]), na.rm = TRUE)
  m_3_ret_t = mean(as.numeric(M3_ret[[p]]), na.rm = TRUE)
  m_4_ret_t = mean(as.numeric(M4_ret[[p]]), na.rm = TRUE)
  m_5_ret_t = mean(as.numeric(M5_ret[[p]]), na.rm = TRUE)
  m_6_ret_t = mean(as.numeric(M6_ret[[p]]), na.rm = TRUE)
  m_7_ret_t = mean(as.numeric(M7_ret[[p]]), na.rm = TRUE)
  m_8_ret_t = mean(as.numeric(M8_ret[[p]]), na.rm = TRUE)
  m_9_ret_t = mean(as.numeric(M9_ret[[p]]), na.rm = TRUE)
  m_10_ret_t = mean(as.numeric(M10_ret[[p]]), na.rm = TRUE)
  
  #Keep value by listing it out in the created lists
  M1_ret_t[[p]] =   as.numeric(m_1_ret_t)
  M2_ret_t[[p]] =   as.numeric(m_2_ret_t)
  M3_ret_t[[p]] =   as.numeric(m_3_ret_t)
  M4_ret_t[[p]] =   as.numeric(m_4_ret_t)
  M5_ret_t[[p]] =   as.numeric(m_5_ret_t)
  M6_ret_t[[p]] =   as.numeric(m_6_ret_t)
  M7_ret_t[[p]] =   as.numeric(m_7_ret_t)
  M8_ret_t[[p]] =   as.numeric(m_8_ret_t)
  M9_ret_t[[p]] =   as.numeric(m_9_ret_t)
  M10_ret_t[[p]] =   as.numeric(m_10_ret_t)
}

#Chnage from NaN to zero for lists
M1_ret_t_z = rapply( M1_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
M2_ret_t_z = rapply( M2_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
M3_ret_t_z = rapply( M3_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
M4_ret_t_z = rapply( M4_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
M5_ret_t_z = rapply( M5_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
M6_ret_t_z = rapply( M6_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
M7_ret_t_z = rapply( M7_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
M8_ret_t_z = rapply( M8_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
M9_ret_t_z = rapply( M9_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
M10_ret_t_z = rapply( M10_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )


#///////////////////////////////////////////////////////////////
#--------------------COMBINE AS MATRIX--------------------------
#///////////////////////////////////////////////////////////////

#Create the matrix
M1_Port = matrix(NA,ncol=1,nrow=nrow(ret))
M2_Port = matrix(NA,ncol=1,nrow=nrow(ret))
M3_Port = matrix(NA,ncol=1,nrow=nrow(ret))
M4_Port = matrix(NA,ncol=1,nrow=nrow(ret))
M5_Port = matrix(NA,ncol=1,nrow=nrow(ret))
M6_Port = matrix(NA,ncol=1,nrow=nrow(ret))
M7_Port = matrix(NA,ncol=1,nrow=nrow(ret))
M8_Port = matrix(NA,ncol=1,nrow=nrow(ret))
M9_Port = matrix(NA,ncol=1,nrow=nrow(ret))
M10_Port = matrix(NA,ncol=1,nrow=nrow(ret))

#For loop to combine 
for(q in 1:nrow(ret)){
  
  #create temporary value
  temp1 = M1_ret_t_z[[q]]
  temp2 = M2_ret_t_z[[q]]
  temp3 = M3_ret_t_z[[q]]
  temp4 = M4_ret_t_z[[q]]
  temp5 = M5_ret_t_z[[q]]
  temp6 = M6_ret_t_z[[q]]
  temp7 = M7_ret_t_z[[q]]
  temp8 = M8_ret_t_z[[q]]
  temp9 = M9_ret_t_z[[q]]
  temp10 = M10_ret_t_z[[q]]

  #put the value in matrix
  M1_Port[q] = temp1
  M2_Port[q] = temp2
  M3_Port[q] = temp3
  M4_Port[q] = temp4
  M5_Port[q] = temp5
  M6_Port[q] = temp6
  M7_Port[q] = temp7
  M8_Port[q] = temp8
  M9_Port[q] = temp9
  M10_Port[q] = temp10
}

#Get overall portfolio return
ALL10_ret = matrix(NA,ncol=10,nrow=1)

ALL10_ret[1,1] = mean(M1_Port)*12
ALL10_ret[1,2] = mean(M2_Port)*12
ALL10_ret[1,3] = mean(M3_Port)*12
ALL10_ret[1,4] = mean(M4_Port)*12
ALL10_ret[1,5] = mean(M5_Port)*12
ALL10_ret[1,6] = mean(M6_Port)*12
ALL10_ret[1,7] = mean(M7_Port)*12
ALL10_ret[1,8] = mean(M8_Port)*12
ALL10_ret[1,9] = mean(M9_Port)*12
ALL10_ret[1,10] = mean(M10_Port)*12












