rm(list = ls()); options(stringsAsFactors = F)
library(plyr)
setwd("d:/data/MetS/")

#
load("Koges_ansan_dm_annotation.rdata")

#
storage1 = "koges_cox_dm.txt"
storage2 = "koges_cox_dm_flow.txt"

#duration
var = names(dat)
idx1 = grep("EDATE", var)
var = var[idx1]

temp = dat[, var]

for(i in var){
  temp = dat[, i]*100 + 1
  dat[, i] = as.Date(as.character(temp), format="%Y%m%d")
}

base = var[1]
var = var[-1]
for(i in var){
  baseline = dat[, base]
  visit = dat[, i]
  dur_months = round(as.numeric(difftime(visit, baseline, units = "days"))/365, 2)
  dat[, i] = dur_months
}

#
flow_chart = c("All", nrow(dat))

#
temp = c("SES", nrow(dat))
flow_chart = rbind(flow_chart, temp)

#
dx = "dm"
dat[, dx] = 0
var = names(dat)
idx1 = grep("dm", var)
#var[idx1]
date = grep("EDATE", var)
dat.date = dat[, date]

#1
dx1 = which(dat[, idx1[1]] == 1)
dat = dat[-dx1, ]
dat.date = dat.date[-dx1, ]
temp = c("T2D, baseline", nrow(dat))
flow_chart = rbind(flow_chart, temp)
#7433

#2
dx2 = which(dat[, idx1[2]] == 1)
dat[dx2, "duration"] = dat.date[dx2, 2]
dat[dx2, dx] = 1
all.dx = dx2

#3
dx3 = which(dat[, idx1[3]] == 1)
dx3 = setdiff(dx3, all.dx)
dat[dx3, "duration"] = dat.date[dx3, 3]
dat[dx3, dx] = 1
all.dx = union(all.dx, dx3)

#4
dx4 = which(dat[, idx1[4]] == 1)
dx4 = setdiff(dx4, all.dx)
dat[dx4, "duration"] = dat.date[dx4, 4]
dat[dx4, dx] = 1
all.dx = union(all.dx, dx4)

#5
dx5 = which(dat[, idx1[5]] == 1)
dx5 = setdiff(dx5, all.dx)
dat[dx5, "duration"] = dat.date[dx5, 5]
dat[dx5, dx] = 1
all.dx = union(all.dx, dx5)

#6
dx6 = which(dat[, idx1[6]] == 1)
dx6 = setdiff(dx6, all.dx)
dat[dx6, "duration"] = dat.date[dx6, 6]
dat[dx6, dx] = 1
all.dx = union(all.dx, dx6)

#7
dx7 = which(dat[, idx1[7]] == 1)
dx7 = setdiff(dx7, all.dx)
dat[dx7, "duration"] = dat.date[dx7, 7]
dat[dx7, dx] = 1

#
idx1 = which(is.na(dat$duration))
sub_dat1 = dat[-idx1, ]
sub_dat2 = dat[idx1, ]
dat.date = dat.date[idx1, ]
for(i in 1:nrow(dat.date)){
  temp_duration = unlist(dat.date[i, 2:ncol(dat.date)])
  temp_duration = temp_duration[!is.na(temp_duration)]
  if(length(temp_duration) > 0){
    max = max(temp_duration)
    sub_dat2[i, "duration"] = max
  } 
}

dat = rbind(sub_dat1, sub_dat2)
dat = dat[!is.na(dat$duration), ]
temp = c("F/U loss", nrow(dat))
flow_chart = rbind(flow_chart, temp)
#7433

write.table(dat, storage1, sep = "\t", row.names = F)
write.table(flow_chart, storage2, sep = "\t", row.names = F)
