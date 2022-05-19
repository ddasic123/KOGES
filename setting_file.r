rm(list = ls())
options(stringsAsFactors = F)
library(plyr)
setwd("d:/data/MetS/")

#
dat1 = read.table("Koges_ansan_visit1_MetS_cleaned.txt", sep = "\t", header = T)
dat2 = read.table("Koges_ansan_visit2_MetS.txt", sep = "\t", header = T)
dat3 = read.table("Koges_ansan_visit3_MetS.txt", sep = "\t", header = T)
dat4 = read.table("Koges_ansan_visit4_MetS.txt", sep = "\t", header = T)
dat5 = read.table("Koges_ansan_visit5_MetS.txt", sep = "\t", header = T)
dat6 = read.table("Koges_ansan_visit6_MetS.txt", sep = "\t", header = T)
dat7 = read.table("Koges_ansan_visit7_MetS.txt", sep = "\t", header = T)

#
dat = join(dat1, dat2, by = "NIHID")
dat = join(dat, dat3, by = "NIHID")
dat = join(dat, dat4, by = "NIHID")
dat = join(dat, dat5, by = "NIHID")
dat = join(dat, dat6, by = "NIHID")
dat = join(dat, dat7, by = "NIHID")

#F1_BP1
idx1 = which(dat$AS1_PDHT == 2)
idx2 = which(dat$AS1_BPSIT1RS >= 130)
idx3 = which(dat$AS1_BPSIT1RD >= 85)
idx = union(idx1, idx2)
idx = union(idx, idx3)

dat$F1_BP1 = 0
dat$F1_BP1[idx] = 1

#F2_WC1
idx1 = which(dat$AS1_WAIST1 >=90 & dat$sex == 1)
idx2 = which(dat$AS1_WAIST1 >=85 & dat$sex == 2)
idx = union(idx1, idx2)

dat$F2_WC1 = 0
dat$F2_WC1[idx] = 1

#F3_GLU1
idx1 = which(dat$AS1_GLU0_TR >= 100)
idx2 = which(dat$AS1_PDDM == 2)
idx = union(idx1, idx2)

dat$F3_GLU1 = 0
dat$F3_GLU1[idx] = 1

#F4_HDL1
idx1 = which(dat$AS1_HDL_TR < 40 & dat$sex == 1)
idx2 = which(dat$AS1_HDL_TR < 50 & dat$sex == 2)
idx = union(idx1, idx2)

dat$F4_HDL1 = 0
dat$F4_HDL1[idx] = 1

#F5_TG1
idx = which(dat$AS1_TG_TR >= 150)

dat$F5_TG1 = 0
dat$F5_TG1[idx] = 1

#dm1
idx1 = which(dat$AS1_PDDM == 2)
idx2 = which(dat$AS1_GLU0_TR >= 126 & dat$AS1_GLU0_TR < 1000 & dat$AS1_GLU0_TR < 2000)
#297
idx3 = which(dat$AS1_GLU120_TR >= 200 & dat$AS1_GLU120_TR < 2000)
#547
idx4 = which(dat$AS1_HBA1C >= 6.5 & dat$AS1_HBA1C < 1000)
#660
idx = Reduce(union, x = list(idx1, idx2, idx3, idx4))

dat$dm1 = 0
dat$dm1[idx] = 1

#dm2
idx1 = which(dat$AS2_PDDM == 2)
idx2 = which(dat$AS2_GLU0 >= 126 & dat$AS2_GLU0 < 2000)
idx3 = which(dat$AS2_GLU120 >= 200 & dat$AS2_GLU120 < 2000)
idx4 = which(dat$AS2_HBA1C >= 6.5 & dat$AS2_HBA1C < 2000)
idx = Reduce(union, x = list(idx1, idx2, idx3, idx4))
#792

dat$dm2 = 0
dat$dm2[idx] = 1

#dm3
idx1 = which(dat$AS3_PDFDM == 2)
idx2 = which(dat$AS3_GLU0 >= 126 & dat$AS3_GLU0 < 2000)
idx3 = which(dat$AS3_GLU120 >= 200 & dat$AS3_GLU120 < 2000)
idx4 = which(dat$AS3_HBA1C >= 6.5 & dat$AS3_HBA1C < 2000)
idx = Reduce(union, x = list(idx1, idx2, idx3, idx4))
#952
dat$dm3 = 0
dat$dm3[idx] = 1

#dm4
idx1 = which(dat$AS4_DM == 2)
idx2 = which(dat$AS4_GLU0 >= 126 & dat$AS4_GLU0 < 2000)
idx3 = which(dat$AS4_GLU120 >= 200 & dat$AS4_GLU120 < 2000)
idx4 = which(dat$AS4_HBA1C >= 6.5 & dat$AS4_HBA1C < 2000)
idx = Reduce(union, x = list(idx1, idx2, idx3, idx4))
#997
dat$dm4 = 0
dat$dm4[idx] = 1

#dm5
idx1 = which(dat$AS5_DM == 2)
idx2 = which(dat$AS5_GLU0 >= 126 & dat$AS5_GLU0 < 2000)
idx3 = which(dat$AS5_GLU120 >= 200 & dat$AS5_GLU120 < 2000)
idx4 = which(dat$AS5_HBA1C >= 6.5 & dat$AS5_HBA1C < 2000)
idx = Reduce(union, x = list(idx1, idx2, idx3, idx4))
#1111
dat$dm5 = 0
dat$dm5[idx] = 1

#dm6
idx1 = which(dat$AS6_DM == 2)
idx2 = which(dat$AS6_GLU0_TR >= 126 & dat$AS6_GLU0_TR < 2000)
idx3 = which(dat$AS6_GLU120_TR >= 200 & dat$AS6_GLU120_TR < 2000)
idx4 = which(dat$AS6_HBA1C >= 6.5 & dat$AS6_HBA1C < 2000)
idx = Reduce(union, x = list(idx1, idx2, idx3, idx4))
#984
dat$dm6 = 0
dat$dm6[idx] = 1

#dm7
idx1 = which(dat$AS7_DM == 2)
idx2 = which(dat$AS7_GLU0_TR >= 126 & dat$AS7_GLU0_TR < 2000)
idx3 = which(dat$AS7_GLU120_TR >= 200 & dat$AS7_GLU120_TR < 2000)
idx4 = which(dat$AS7_HBA1C >= 6.5 & dat$AS7_HBA1C < 2000)
idx = Reduce(union, x = list(idx1, idx2, idx3, idx4))
#1011
dat$dm7 = 0
dat$dm7[idx] = 1

save(file = "Koges_ansan_dm_annotation.rdata", dat)

