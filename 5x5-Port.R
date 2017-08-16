#Call Library
library(readxl)
library(data.table)

#Import data
Return <- read_excel("~/Desktop/Data61M_C.xlsx")
Size <- read_excel("~/Desktop/Data61M_C.xlsx", 
                   sheet = "MV")
BM <- read_excel("~/Desktop/Data61M_C.xlsx", 
                 sheet = "Vol")
Rf<- read_excel("~/Desktop/Data61M_C.xlsx", 
                sheet = "Rf")
rf = Rf[,2]
Rf_ann = rf*(1/12)
Rf <- data.matrix(Rf_ann)

#///////////////////////////////////////////////////////////////
#------------------SORTED size BY Quantile----------------------
#///////////////////////////////////////////////////////////////

#Create "sizes" from Size Data column 2 to last one
#take dates in the first column out
sizes = Size[,2:ncol(Size)]

#Create lists for Small to Big
BA_index = list(nrow(sizes))
BB_index = list(nrow(sizes))
BC_index = list(nrow(sizes))
BD_index = list(nrow(sizes))
BE_index = list(nrow(sizes))

#For Loop: Start with defining i take value from 1 to no.row(sizes)
for(i in 1: nrow(sizes)){

  #Create row_sizes for each rows
  #Take value out period by period
  row_sizes = sizes[i,]
  
  #Create break pionts from 30th and 70th percentile
  A_row  = quantile(as.numeric(row_sizes), probs = 0.2, 
                         na.rm = TRUE)
  B_row  = quantile(as.numeric(row_sizes), probs = 0.4, 
                          na.rm = TRUE)
  C_row  = quantile(as.numeric(row_sizes), probs = 0.6, 
                          na.rm = TRUE)
  D_row  = quantile(as.numeric(row_sizes), probs = 0.8, 
                          na.rm = TRUE)
  
  #match value by quantile small to large
  BA_med = which(row_sizes < A_row)
  BB_med = which((row_sizes < B_row) 
                      & (row_sizes >= A_row))
  BC_med = which((row_sizes <= C_row) 
                 & (row_sizes >= B_row))
  BD_med = which((row_sizes < D_row) 
                 & (row_sizes >= C_row))
  BE_med = which(row_sizes > D_row)
  
  #Keep value by listing it out in the created lists
  BA_index[[i]] = BA_med
  BB_index[[i]] = BB_med 
  BC_index[[i]] = BC_med
  BD_index[[i]] = BD_med 
  BE_index[[i]] = BE_med
}

#///////////////////////////////////////////////////////////////
#------------------SORTED value BY Quantile---------------------
#///////////////////////////////////////////////////////////////

#Create "bm" from BM Data column 2 to last one
#take dates in the first column out
bm = BM[,2:ncol(BM)]

#Create lists for low to high
VA_index = list(nrow(bm))
VB_index = list(nrow(bm))
VC_index = list(nrow(bm))
VD_index = list(nrow(bm))
VE_index = list(nrow(bm))

#For Loop: Start with defining i take value from 1 to no.row(sizes)
for(i in 1: nrow(bm)){
  
  #Create row_sizes for each rows
  #Take value out period by period
  row_bm = bm[i,]
  
  #Create break pionts from 30th and 70th percentile
  A_row  = quantile(as.numeric(row_bm), probs = 0.2, 
                    na.rm = TRUE)
  B_row  = quantile(as.numeric(row_bm), probs = 0.4, 
                    na.rm = TRUE)
  C_row  = quantile(as.numeric(row_bm), probs = 0.6, 
                    na.rm = TRUE)
  D_row  = quantile(as.numeric(row_bm), probs = 0.8, 
                    na.rm = TRUE)
  
  #match value by quantile
  VA_med = which(row_bm < A_row)
  VB_med = which((row_bm < B_row) 
                 & (row_bm >= A_row))
  VC_med = which((row_bm <= C_row) 
                 & (row_bm >= B_row))
  VD_med = which((row_bm <= D_row) 
                 & (row_bm > C_row))
  VE_med = which(row_bm > D_row)
  
  #Keep value by listing it out in the created lists
  VA_index[[i]] = VA_med
  VB_index[[i]] = VB_med 
  VC_index[[i]] = VC_med
  VD_index[[i]] = VD_med 
  VE_index[[i]] = VE_med
}

#///////////////////////////////////////////////////////////////
#-------------------------Intersect-----------------------------
#///////////////////////////////////////////////////////////////

#---------------------------BA VA------------------------------
#Create list
BAVA_index = list(nrow(Return))
for(k in 1:nrow(Return)){
  BAVA_inter = intersect(BA_index[[k]],VA_index[[k]]) 
  #Keep value by listing it out in the created lists
  BAVA_index[[k]] =  BAVA_inter
}
#---------------------------BB VA------------------------------
BBVA_index = list(nrow(Return))
for(k in 1:nrow(Return)){
  BBVA_inter = intersect(BB_index[[k]],VA_index[[k]]) 
  BBVA_index[[k]] =  BBVA_inter
}
#---------------------------BC VA------------------------------
BCVA_index = list(nrow(Return))
for(k in 1:nrow(Return)){
  BCVA_inter = intersect(BC_index[[k]],VA_index[[k]]) 
  BCVA_index[[k]] =  BCVA_inter
}
#---------------------------BD VA------------------------------
BDVA_index = list(nrow(Return))
for(k in 1:nrow(Return)){
  BDVA_inter = intersect(BD_index[[k]],VA_index[[k]]) 
  BDVA_index[[k]] =  BDVA_inter
}
#---------------------------BE VA------------------------------
BEVA_index = list(nrow(Return))
for(k in 1:nrow(Return)){
  BEVA_inter = intersect(BE_index[[k]],VA_index[[k]]) 
  BEVA_index[[k]] =  BEVA_inter
}
#---------------------------BA VB------------------------------
BAVB_index = list(nrow(Return))
for(k in 1:nrow(Return)){
  BAVB_inter = intersect(BA_index[[k]],VB_index[[k]]) 
  BAVB_index[[k]] =  BAVB_inter
}
#---------------------------BB VB------------------------------
BBVB_index = list(nrow(Return))
for(k in 1:nrow(Return)){
  BBVB_inter = intersect(BB_index[[k]],VB_index[[k]]) 
  BBVB_index[[k]] =  BBVB_inter
}
#---------------------------BC VB------------------------------
BCVB_index = list(nrow(Return))
for(k in 1:nrow(Return)){
  BCVB_inter = intersect(BC_index[[k]],VB_index[[k]]) 
  BCVB_index[[k]] =  BCVB_inter
}
#---------------------------BD VB------------------------------
BDVB_index = list(nrow(Return))
for(k in 1:nrow(Return)){
  BDVB_inter = intersect(BD_index[[k]],VB_index[[k]]) 
  BDVB_index[[k]] =  BDVB_inter
}
#---------------------------BE VB------------------------------
BEVB_index = list(nrow(Return))
for(k in 1:nrow(Return)){
  BEVB_inter = intersect(BE_index[[k]],VB_index[[k]]) 
  BEVB_index[[k]] =  BEVB_inter
}
#---------------------------BA VC------------------------------
BAVC_index = list(nrow(Return))
for(k in 1:nrow(Return)){
  BAVC_inter = intersect(BA_index[[k]],VC_index[[k]]) 
  BAVC_index[[k]] =  BAVC_inter
}
#---------------------------BB VC------------------------------
BBVC_index = list(nrow(Return))
for(k in 1:nrow(Return)){
  BBVC_inter = intersect(BB_index[[k]],VC_index[[k]]) 
  BBVC_index[[k]] =  BBVC_inter
}
#---------------------------BC VC------------------------------
BCVC_index = list(nrow(Return))
for(k in 1:nrow(Return)){
  BCVC_inter = intersect(BC_index[[k]],VC_index[[k]]) 
  BCVC_index[[k]] =  BCVC_inter
}
#---------------------------BD VC------------------------------
BDVC_index = list(nrow(Return))
for(k in 1:nrow(Return)){
  BDVC_inter = intersect(BD_index[[k]],VC_index[[k]]) 
  BDVC_index[[k]] =  BDVC_inter
}
#---------------------------BE VC------------------------------
BEVC_index = list(nrow(Return))
for(k in 1:nrow(Return)){
  BEVC_inter = intersect(BE_index[[k]],VC_index[[k]]) 
  BEVC_index[[k]] =  BEVC_inter
}
#---------------------------BA VD------------------------------
BAVD_index = list(nrow(Return))
for(k in 1:nrow(Return)){
  BAVD_inter = intersect(BA_index[[k]],VD_index[[k]]) 
  BAVD_index[[k]] =  BAVD_inter
}
#---------------------------BB VD------------------------------
BBVD_index = list(nrow(Return))
for(k in 1:nrow(Return)){
  BBVD_inter = intersect(BB_index[[k]],VD_index[[k]]) 
  BBVD_index[[k]] =  BBVD_inter
}
#---------------------------BC VD------------------------------
BCVD_index = list(nrow(Return))
for(k in 1:nrow(Return)){
  BCVD_inter = intersect(BC_index[[k]],VD_index[[k]]) 
  BCVD_index[[k]] =  BCVD_inter
}
#---------------------------BD VD------------------------------
BDVD_index = list(nrow(Return))
for(k in 1:nrow(Return)){
  BDVD_inter = intersect(BD_index[[k]],VD_index[[k]]) 
  BDVD_index[[k]] =  BDVD_inter
}
#---------------------------BE VD------------------------------
BEVD_index = list(nrow(Return))
for(k in 1:nrow(Return)){
  BEVD_inter = intersect(BE_index[[k]],VD_index[[k]]) 
  BEVD_index[[k]] =  BEVD_inter
}
#---------------------------BA VE------------------------------
BAVE_index = list(nrow(Return))
for(k in 1:nrow(Return)){
  BAVE_inter = intersect(BA_index[[k]],VE_index[[k]]) 
  BAVE_index[[k]] =  BAVE_inter
}
#---------------------------BB VE------------------------------
BBVE_index = list(nrow(Return))
for(k in 1:nrow(Return)){
  BBVE_inter = intersect(BB_index[[k]],VE_index[[k]]) 
  BBVE_index[[k]] =  BBVE_inter
}
#---------------------------BC VE------------------------------
BCVE_index = list(nrow(Return))
for(k in 1:nrow(Return)){
  BCVE_inter = intersect(BC_index[[k]],VE_index[[k]]) 
  BCVE_index[[k]] =  BCVE_inter
}
#---------------------------BD VE------------------------------
BDVE_index = list(nrow(Return))
for(k in 1:nrow(Return)){
  BDVE_inter = intersect(BD_index[[k]],VE_index[[k]]) 
  BDVE_index[[k]] =  BDVE_inter
}
#---------------------------BE VE------------------------------
BEVE_index = list(nrow(Return))
for(k in 1:nrow(Return)){
  BEVE_inter = intersect(BE_index[[k]],VE_index[[k]]) 
  BEVE_index[[k]] =  BEVE_inter
}

#///////////////////////////////////////////////////////////////
#---------------------Tarced back return------------------------
#///////////////////////////////////////////////////////////////

#Create ret to macth back the return
#Take out dates and Rf ready to match by ID
ret = Return[,2:ncol(Return)] 

#Create lists to keep return value 
BAVA_ret = list(nrow(ret))
BBVA_ret = list(nrow(ret))
BCVA_ret = list(nrow(ret))
BDVA_ret = list(nrow(ret))
BEVA_ret = list(nrow(ret))

BAVB_ret = list(nrow(ret))
BBVB_ret = list(nrow(ret))
BCVB_ret = list(nrow(ret))
BDVB_ret = list(nrow(ret))
BEVB_ret = list(nrow(ret))

BAVC_ret = list(nrow(ret))
BBVC_ret = list(nrow(ret))
BCVC_ret = list(nrow(ret))
BDVC_ret = list(nrow(ret))
BEVC_ret = list(nrow(ret))

BAVD_ret = list(nrow(ret))
BBVD_ret = list(nrow(ret))
BCVD_ret = list(nrow(ret))
BDVD_ret = list(nrow(ret))
BEVD_ret = list(nrow(ret))

BAVE_ret = list(nrow(ret))
BBVE_ret = list(nrow(ret))
BCVE_ret = list(nrow(ret))
BDVE_ret = list(nrow(ret))
BEVE_ret = list(nrow(ret))


#For loop to match ID back to return from each row in list
for(p in 1: nrow(ret)){
  
  #Temporarily create macthing value for each ID and return
  
  bava_ret = unlist(ret[p,BAVA_index[[p]]])
  bbva_ret = unlist(ret[p,BBVA_index[[p]]])
  bcva_ret = unlist(ret[p,BCVA_index[[p]]])
  bdva_ret = unlist(ret[p,BDVA_index[[p]]])
  beva_ret = unlist(ret[p,BEVA_index[[p]]])
  
  bavb_ret = unlist(ret[p,BAVB_index[[p]]])
  bbvb_ret = unlist(ret[p,BBVB_index[[p]]])
  bcvb_ret = unlist(ret[p,BCVB_index[[p]]])
  bdvb_ret = unlist(ret[p,BDVB_index[[p]]])
  bevb_ret = unlist(ret[p,BEVB_index[[p]]])
  
  bavc_ret = unlist(ret[p,BAVC_index[[p]]])
  bbvc_ret = unlist(ret[p,BBVC_index[[p]]])
  bcvc_ret = unlist(ret[p,BCVC_index[[p]]])
  bdvc_ret = unlist(ret[p,BDVC_index[[p]]])
  bevc_ret = unlist(ret[p,BEVC_index[[p]]])
  
  bavd_ret = unlist(ret[p,BAVD_index[[p]]])
  bbvd_ret = unlist(ret[p,BBVD_index[[p]]])
  bcvd_ret = unlist(ret[p,BCVD_index[[p]]])
  bdvd_ret = unlist(ret[p,BDVD_index[[p]]])
  bevd_ret = unlist(ret[p,BEVD_index[[p]]])
  
  bave_ret = unlist(ret[p,BAVE_index[[p]]])
  bbve_ret = unlist(ret[p,BBVE_index[[p]]])
  bcve_ret = unlist(ret[p,BCVE_index[[p]]])
  bdve_ret = unlist(ret[p,BDVE_index[[p]]])
  beve_ret = unlist(ret[p,BEVE_index[[p]]])
  
  #Keep value by listing it out in the created lists
  
  BAVA_ret[[p]] = as.numeric(bava_ret)
  BBVA_ret[[p]] = as.numeric(bbva_ret)
  BCVA_ret[[p]] = as.numeric(bcva_ret)
  BDVA_ret[[p]] = as.numeric(bdva_ret)
  BEVA_ret[[p]] = as.numeric(beva_ret)
  
  BAVB_ret[[p]] = as.numeric(bavb_ret)
  BBVB_ret[[p]] = as.numeric(bbvb_ret)
  BCVB_ret[[p]] = as.numeric(bcvb_ret)
  BDVB_ret[[p]] = as.numeric(bdvb_ret)
  BEVB_ret[[p]] = as.numeric(bevb_ret)
  
  BAVC_ret[[p]] = as.numeric(bavc_ret)
  BBVC_ret[[p]] = as.numeric(bbvc_ret)
  BCVC_ret[[p]] = as.numeric(bcvc_ret)
  BDVC_ret[[p]] = as.numeric(bdvc_ret)
  BEVC_ret[[p]] = as.numeric(bevc_ret)
  
  BAVD_ret[[p]] = as.numeric(bavd_ret)
  BBVD_ret[[p]] = as.numeric(bbvd_ret)
  BCVD_ret[[p]] = as.numeric(bcvd_ret)
  BDVD_ret[[p]] = as.numeric(bdvd_ret)
  BEVD_ret[[p]] = as.numeric(bevd_ret)
  
  BAVE_ret[[p]] = as.numeric(bave_ret)
  BBVE_ret[[p]] = as.numeric(bbve_ret)
  BCVE_ret[[p]] = as.numeric(bcve_ret)
  BDVE_ret[[p]] = as.numeric(bdve_ret)
  BEVE_ret[[p]] = as.numeric(beve_ret)
}

#///////////////////////////////////////////////////////////////
#-------------------Mean return by period-----------------------
#///////////////////////////////////////////////////////////////

#Created Weights
BAVA_weight = list(nrow(ret))
BBVA_weight = list(nrow(ret))
BCVA_weight = list(nrow(ret))
BDVA_weight = list(nrow(ret))
BEVA_weight = list(nrow(ret))

BAVB_weight = list(nrow(ret))
BBVB_weight = list(nrow(ret))
BCVB_weight = list(nrow(ret))
BDVB_weight = list(nrow(ret))
BEVB_weight = list(nrow(ret))

BAVC_weight = list(nrow(ret))
BBVC_weight = list(nrow(ret))
BCVC_weight = list(nrow(ret))
BDVC_weight = list(nrow(ret))
BEVC_weight = list(nrow(ret))

BAVD_weight = list(nrow(ret))
BBVD_weight = list(nrow(ret))
BCVD_weight = list(nrow(ret))
BDVD_weight = list(nrow(ret))
BEVD_weight = list(nrow(ret))

BAVE_weight = list(nrow(ret))
BBVE_weight = list(nrow(ret))
BCVE_weight = list(nrow(ret))
BDVE_weight = list(nrow(ret))
BEVE_weight = list(nrow(ret))

for(p in 1: nrow(ret)){
  
  bava_weight = unlist(sizes[p,BAVA_index[[p]]])
  bbva_weight = unlist(sizes[p,BBVA_index[[p]]])
  bcva_weight = unlist(sizes[p,BCVA_index[[p]]])
  bdva_weight = unlist(sizes[p,BDVA_index[[p]]])
  beva_weight = unlist(sizes[p,BEVA_index[[p]]])
  
  bavb_weight = unlist(sizes[p,BAVB_index[[p]]])
  bbvb_weight = unlist(sizes[p,BBVB_index[[p]]])
  bcvb_weight = unlist(sizes[p,BCVB_index[[p]]])
  bdvb_weight = unlist(sizes[p,BDVB_index[[p]]])
  bevb_weight = unlist(sizes[p,BEVB_index[[p]]])
  
  bavc_weight = unlist(sizes[p,BAVC_index[[p]]])
  bbvc_weight = unlist(sizes[p,BBVC_index[[p]]])
  bcvc_weight = unlist(sizes[p,BCVC_index[[p]]])
  bdvc_weight = unlist(sizes[p,BDVC_index[[p]]])
  bevc_weight = unlist(sizes[p,BEVC_index[[p]]])
  
  bavd_weight = unlist(sizes[p,BAVD_index[[p]]])
  bbvd_weight = unlist(sizes[p,BBVD_index[[p]]])
  bcvd_weight = unlist(sizes[p,BCVD_index[[p]]])
  bdvd_weight = unlist(sizes[p,BDVD_index[[p]]])
  bevd_weight = unlist(sizes[p,BEVD_index[[p]]])
  
  bave_weight = unlist(sizes[p,BAVE_index[[p]]])
  bbve_weight = unlist(sizes[p,BBVE_index[[p]]])
  bcve_weight = unlist(sizes[p,BCVE_index[[p]]])
  bdve_weight = unlist(sizes[p,BDVE_index[[p]]])
  beve_weight = unlist(sizes[p,BEVE_index[[p]]])
  
  BAVA_weight[[p]] = as.numeric(bava_weight)
  BBVA_weight[[p]] = as.numeric(bbva_weight)
  BCVA_weight[[p]] = as.numeric(bcva_weight)
  BDVA_weight[[p]] = as.numeric(bdva_weight)
  BEVA_weight[[p]] = as.numeric(beva_weight)
  
  BAVB_weight[[p]] = as.numeric(bavb_weight)
  BBVB_weight[[p]] = as.numeric(bbvb_weight)
  BCVB_weight[[p]] = as.numeric(bcvb_weight)
  BDVB_weight[[p]] = as.numeric(bdvb_weight)
  BEVB_weight[[p]] = as.numeric(bevb_weight)
  
  BAVC_weight[[p]] = as.numeric(bavc_weight)
  BBVC_weight[[p]] = as.numeric(bbvc_weight)
  BCVC_weight[[p]] = as.numeric(bcvc_weight)
  BDVC_weight[[p]] = as.numeric(bdvc_weight)
  BEVC_weight[[p]] = as.numeric(bevc_weight)
  
  BAVD_weight[[p]] = as.numeric(bavd_weight)
  BBVD_weight[[p]] = as.numeric(bbvd_weight)
  BCVD_weight[[p]] = as.numeric(bcvd_weight)
  BDVD_weight[[p]] = as.numeric(bdvd_weight)
  BEVD_weight[[p]] = as.numeric(bevd_weight)
  
  BAVE_weight[[p]] = as.numeric(bave_weight)
  BBVE_weight[[p]] = as.numeric(bbve_weight)
  BCVE_weight[[p]] = as.numeric(bcve_weight)
  BDVE_weight[[p]] = as.numeric(bdve_weight)
  BEVE_weight[[p]] = as.numeric(beve_weight)
}

#Create lists to keep return value for Big and Small
BAVA_ret_t = list(nrow(ret))
BBVA_ret_t = list(nrow(ret))
BCVA_ret_t = list(nrow(ret))
BDVA_ret_t = list(nrow(ret))
BEVA_ret_t = list(nrow(ret))

BAVB_ret_t = list(nrow(ret))
BBVB_ret_t = list(nrow(ret))
BCVB_ret_t = list(nrow(ret))
BDVB_ret_t = list(nrow(ret))
BEVB_ret_t = list(nrow(ret))

BAVC_ret_t = list(nrow(ret))
BBVC_ret_t = list(nrow(ret))
BCVC_ret_t = list(nrow(ret))
BDVC_ret_t = list(nrow(ret))
BEVC_ret_t = list(nrow(ret))

BAVD_ret_t = list(nrow(ret))
BBVD_ret_t = list(nrow(ret))
BCVD_ret_t = list(nrow(ret))
BDVD_ret_t = list(nrow(ret))
BEVD_ret_t = list(nrow(ret))

BAVE_ret_t = list(nrow(ret))
BBVE_ret_t = list(nrow(ret))
BCVE_ret_t = list(nrow(ret))
BDVE_ret_t = list(nrow(ret))
BEVE_ret_t = list(nrow(ret))

for(p in 1: nrow(ret)){
  
  #Calculate the average return for that period
  bava_ret_t = weighted.mean(as.numeric(BAVA_ret[[p]]),
                             BAVA_weight[[p]], na.rm = TRUE)
  bbva_ret_t = weighted.mean(as.numeric(BBVA_ret[[p]]),
                             BBVA_weight[[p]], na.rm = TRUE)
  bcva_ret_t = weighted.mean(as.numeric(BCVA_ret[[p]]),
                             BCVA_weight[[p]], na.rm = TRUE)
  bdva_ret_t = weighted.mean(as.numeric(BDVA_ret[[p]]),
                             BDVA_weight[[p]], na.rm = TRUE)
  beva_ret_t = weighted.mean(as.numeric(BEVA_ret[[p]]),
                             BEVA_weight[[p]], na.rm = TRUE)
  
  bavb_ret_t = weighted.mean(as.numeric(BAVB_ret[[p]]),
                             BAVB_weight[[p]], na.rm = TRUE)
  bbvb_ret_t = weighted.mean(as.numeric(BBVB_ret[[p]]),
                             BBVB_weight[[p]], na.rm = TRUE)
  bcvb_ret_t = weighted.mean(as.numeric(BCVB_ret[[p]]),
                             BCVB_weight[[p]], na.rm = TRUE)
  bdvb_ret_t = weighted.mean(as.numeric(BDVB_ret[[p]]),
                             BDVB_weight[[p]], na.rm = TRUE)
  bevb_ret_t = weighted.mean(as.numeric(BEVB_ret[[p]]),
                             BEVB_weight[[p]], na.rm = TRUE)
  
  bavc_ret_t = weighted.mean(as.numeric(BAVC_ret[[p]]),
                             BAVC_weight[[p]], na.rm = TRUE)
  bbvc_ret_t = weighted.mean(as.numeric(BBVC_ret[[p]]),
                             BBVC_weight[[p]], na.rm = TRUE)
  bcvc_ret_t = weighted.mean(as.numeric(BCVC_ret[[p]]),
                             BCVC_weight[[p]], na.rm = TRUE)
  bdvc_ret_t = weighted.mean(as.numeric(BDVC_ret[[p]]),
                             BDVC_weight[[p]], na.rm = TRUE)
  bevc_ret_t = weighted.mean(as.numeric(BEVC_ret[[p]]),
                             BEVC_weight[[p]], na.rm = TRUE)
  
  bavd_ret_t = weighted.mean(as.numeric(BAVD_ret[[p]]),
                             BAVD_weight[[p]], na.rm = TRUE)
  bbvd_ret_t = weighted.mean(as.numeric(BBVD_ret[[p]]),
                             BBVD_weight[[p]], na.rm = TRUE)
  bcvd_ret_t = weighted.mean(as.numeric(BCVD_ret[[p]]),
                             BCVD_weight[[p]], na.rm = TRUE)
  bdvd_ret_t = weighted.mean(as.numeric(BDVD_ret[[p]]),
                             BDVD_weight[[p]], na.rm = TRUE)
  bevd_ret_t = weighted.mean(as.numeric(BEVD_ret[[p]]),
                             BEVD_weight[[p]], na.rm = TRUE)
  
  bave_ret_t = weighted.mean(as.numeric(BAVE_ret[[p]]),
                             BAVE_weight[[p]], na.rm = TRUE)
  bbve_ret_t = weighted.mean(as.numeric(BBVE_ret[[p]]),
                             BBVE_weight[[p]], na.rm = TRUE)
  bcve_ret_t = weighted.mean(as.numeric(BCVE_ret[[p]]),
                             BCVE_weight[[p]], na.rm = TRUE)
  bdve_ret_t = weighted.mean(as.numeric(BDVE_ret[[p]]),
                             BDVE_weight[[p]], na.rm = TRUE)
  beve_ret_t = weighted.mean(as.numeric(BEVE_ret[[p]]),
                             BEVE_weight[[p]], na.rm = TRUE)
  
  #Keep value by listing it out in the created lists
  BAVA_ret_t[[p]] =   as.numeric(bava_ret_t)
  BBVA_ret_t[[p]] =   as.numeric(bbva_ret_t)
  BCVA_ret_t[[p]] =   as.numeric(bcva_ret_t)
  BDVA_ret_t[[p]] =   as.numeric(bdva_ret_t)
  BEVA_ret_t[[p]] =   as.numeric(beva_ret_t)
  
  BAVB_ret_t[[p]] =   as.numeric(bavb_ret_t)
  BBVB_ret_t[[p]] =   as.numeric(bbvb_ret_t)
  BCVB_ret_t[[p]] =   as.numeric(bcvb_ret_t)
  BDVB_ret_t[[p]] =   as.numeric(bdvb_ret_t)
  BEVB_ret_t[[p]] =   as.numeric(bevb_ret_t)
  
  BAVC_ret_t[[p]] =   as.numeric(bavc_ret_t)
  BBVC_ret_t[[p]] =   as.numeric(bbvc_ret_t)
  BCVC_ret_t[[p]] =   as.numeric(bcvc_ret_t)
  BDVC_ret_t[[p]] =   as.numeric(bdvc_ret_t)
  BEVC_ret_t[[p]] =   as.numeric(bevc_ret_t)
  
  BAVD_ret_t[[p]] =   as.numeric(bavd_ret_t)
  BBVD_ret_t[[p]] =   as.numeric(bbvd_ret_t)
  BCVD_ret_t[[p]] =   as.numeric(bcvd_ret_t)
  BDVD_ret_t[[p]] =   as.numeric(bdvd_ret_t)
  BEVD_ret_t[[p]] =   as.numeric(bevd_ret_t)
  
  BAVE_ret_t[[p]] =   as.numeric(bave_ret_t)
  BBVE_ret_t[[p]] =   as.numeric(bbve_ret_t)
  BCVE_ret_t[[p]] =   as.numeric(bcve_ret_t)
  BDVE_ret_t[[p]] =   as.numeric(bdve_ret_t)
  BEVE_ret_t[[p]] =   as.numeric(beve_ret_t)
}

#Chnage from NaN to zero for lists
BAVA_ret_t_z = rapply( BAVA_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BBVA_ret_t_z = rapply( BBVA_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BCVA_ret_t_z = rapply( BCVA_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BDVA_ret_t_z = rapply( BDVA_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BEVA_ret_t_z = rapply( BEVA_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )

BAVB_ret_t_z = rapply( BAVB_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BBVB_ret_t_z = rapply( BBVB_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BCVB_ret_t_z = rapply( BCVB_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BDVB_ret_t_z = rapply( BDVB_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BEVB_ret_t_z = rapply( BEVB_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )

BAVC_ret_t_z = rapply( BAVC_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BBVC_ret_t_z = rapply( BBVC_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BCVC_ret_t_z = rapply( BCVC_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BDVC_ret_t_z = rapply( BDVC_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BEVC_ret_t_z = rapply( BEVC_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )

BAVD_ret_t_z = rapply( BAVD_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BBVD_ret_t_z = rapply( BBVD_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BCVD_ret_t_z = rapply( BCVD_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BDVD_ret_t_z = rapply( BDVD_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BEVD_ret_t_z = rapply( BEVD_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )

BAVE_ret_t_z = rapply( BAVE_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BBVE_ret_t_z = rapply( BBVE_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BCVE_ret_t_z = rapply( BCVE_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BDVE_ret_t_z = rapply( BDVE_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )
BEVE_ret_t_z = rapply( BEVE_ret_t, f=function(x) 
  ifelse(is.nan(x),0,x), how="replace" )

#///////////////////////////////////////////////////////////////
#--------------------COMBINE AS MATRIX--------------------------
#///////////////////////////////////////////////////////////////

#Create the matrix
BAVA_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BBVA_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BCVA_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BDVA_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BEVA_Port = matrix(NA,ncol=1,nrow=nrow(ret))

BAVB_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BBVB_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BCVB_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BDVB_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BEVB_Port = matrix(NA,ncol=1,nrow=nrow(ret))

BAVC_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BBVC_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BCVC_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BDVC_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BEVC_Port = matrix(NA,ncol=1,nrow=nrow(ret))

BAVD_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BBVD_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BCVD_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BDVD_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BEVD_Port = matrix(NA,ncol=1,nrow=nrow(ret))

BAVE_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BBVE_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BCVE_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BDVE_Port = matrix(NA,ncol=1,nrow=nrow(ret))
BEVE_Port = matrix(NA,ncol=1,nrow=nrow(ret))

#For loop to combine 
for(q in 1:nrow(ret)){
  
  #create temporary value fro Small_ret_t
  temp11 = BAVA_ret_t_z[[q]]
  temp21 = BBVA_ret_t_z[[q]]
  temp31 = BCVA_ret_t_z[[q]]
  temp41 = BDVA_ret_t_z[[q]]
  temp51 = BEVA_ret_t_z[[q]]
  
  temp12 = BAVB_ret_t_z[[q]]
  temp22 = BBVB_ret_t_z[[q]]
  temp32 = BCVB_ret_t_z[[q]]
  temp42 = BDVB_ret_t_z[[q]]
  temp52 = BEVB_ret_t_z[[q]]
  
  temp13 = BAVC_ret_t_z[[q]]
  temp23 = BBVC_ret_t_z[[q]]
  temp33 = BCVC_ret_t_z[[q]]
  temp43 = BDVC_ret_t_z[[q]]
  temp53 = BEVC_ret_t_z[[q]]
  
  temp14 = BAVD_ret_t_z[[q]]
  temp24 = BBVD_ret_t_z[[q]]
  temp34 = BCVD_ret_t_z[[q]]
  temp44 = BDVD_ret_t_z[[q]]
  temp54 = BEVD_ret_t_z[[q]]
  
  temp15 = BAVE_ret_t_z[[q]]
  temp25 = BBVE_ret_t_z[[q]]
  temp35 = BCVE_ret_t_z[[q]]
  temp45 = BDVE_ret_t_z[[q]]
  temp55 = BEVE_ret_t_z[[q]]
  
  #put the value in matrix
  BAVA_Port[q] = temp11
  BBVA_Port[q] = temp21
  BCVA_Port[q] = temp31
  BDVA_Port[q] = temp41
  BEVA_Port[q] = temp51
  
  BAVB_Port[q] = temp12
  BBVB_Port[q] = temp22
  BCVB_Port[q] = temp32
  BDVB_Port[q] = temp42
  BEVB_Port[q] = temp52
  
  BAVC_Port[q] = temp13
  BBVC_Port[q] = temp23
  BCVC_Port[q] = temp33
  BDVC_Port[q] = temp43
  BEVC_Port[q] = temp53
  
  BAVD_Port[q] = temp14
  BBVD_Port[q] = temp24
  BCVD_Port[q] = temp34
  BDVD_Port[q] = temp44
  BEVD_Port[q] = temp54
  
  BAVE_Port[q] = temp15
  BBVE_Port[q] = temp25
  BCVE_Port[q] = temp35
  BDVE_Port[q] = temp45
  BEVE_Port[q] = temp55
}

#Get overall portfolio return
BAVA_all_ret = mean(BAVA_Port - Rf)
BBVA_all_ret = mean(BBVA_Port - Rf)
BCVA_all_ret = mean(BCVA_Port - Rf)
BDVA_all_ret = mean(BDVA_Port - Rf)
BEVA_all_ret = mean(BEVA_Port - Rf)

BAVB_all_ret = mean(BAVB_Port - Rf)
BBVB_all_ret = mean(BBVB_Port - Rf)
BCVB_all_ret = mean(BCVB_Port - Rf)
BDVB_all_ret = mean(BDVB_Port - Rf)
BEVB_all_ret = mean(BEVB_Port - Rf)

BAVC_all_ret = mean(BAVC_Port - Rf)
BBVC_all_ret = mean(BBVC_Port - Rf)
BCVC_all_ret = mean(BCVC_Port - Rf)
BDVC_all_ret = mean(BDVC_Port - Rf)
BEVC_all_ret = mean(BEVC_Port - Rf)

BAVD_all_ret = mean(BAVD_Port - Rf)
BBVD_all_ret = mean(BBVD_Port - Rf)
BCVD_all_ret = mean(BCVD_Port - Rf)
BDVD_all_ret = mean(BDVD_Port - Rf)
BEVD_all_ret = mean(BEVD_Port - Rf)

BAVE_all_ret = mean(BAVE_Port - Rf)
BBVE_all_ret = mean(BBVE_Port - Rf)
BCVE_all_ret = mean(BCVE_Port - Rf)
BDVE_all_ret = mean(BDVE_Port - Rf)
BEVE_all_ret = mean(BEVE_Port - Rf)

#///////////////////////////////////////////////////////////////
#------------------Get matrix for 5x5 port----------------------
#///////////////////////////////////////////////////////////////

ALL_Port = matrix(NA,ncol=5,nrow=5)

ALL_Port[1,1] = BAVA_all_ret*12
ALL_Port[2,1] = BBVA_all_ret*12
ALL_Port[3,1] = BCVA_all_ret*12
ALL_Port[4,1] = BDVA_all_ret*12
ALL_Port[5,1] = BEVA_all_ret*12

ALL_Port[1,2] = BAVB_all_ret*12
ALL_Port[2,2] = BBVB_all_ret*12
ALL_Port[3,2] = BCVB_all_ret*12
ALL_Port[4,2] = BDVB_all_ret*12
ALL_Port[5,2] = BEVB_all_ret*12

ALL_Port[1,3] = BAVC_all_ret*12
ALL_Port[2,3] = BBVC_all_ret*12
ALL_Port[3,3] = BCVC_all_ret*12
ALL_Port[4,3] = BDVC_all_ret*12
ALL_Port[5,3] = BEVC_all_ret*12

ALL_Port[1,4] = BAVD_all_ret*12
ALL_Port[2,4] = BBVD_all_ret*12
ALL_Port[3,4] = BCVD_all_ret*12
ALL_Port[4,4] = BDVD_all_ret*12
ALL_Port[5,4] = BEVD_all_ret*12

ALL_Port[1,5] = BAVE_all_ret*12
ALL_Port[2,5] = BBVE_all_ret*12
ALL_Port[3,5] = BCVE_all_ret*12
ALL_Port[4,5] = BDVE_all_ret*12
ALL_Port[5,5] = BEVE_all_ret*12


