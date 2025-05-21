# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# PROJECT TITLE:  PRAGMA - RQ3
# CODE AUTHOR:    Jakob Manthe / Carolin Kilian
# DATE STARTED:   09/02/2024
# LAST MODIFIED:  01/10/2024

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 0) ESSENTIALS
# ______________________________________________________________________________________________________________________

# clean workspace
rm(list=ls())

# input path
#inpath <- paste0("C:/Data.work/PRAGMA/Daten/PRAGMA_input/output/")
inpath <- paste0("/Users/carolinkilian/Desktop/Pragma/Data/preprocessed data/")

# output path
outpath <- paste0("/Users/carolinkilian/Desktop/Pragma/Data/raw data/")

# load libraries
library( data.table )
library( ggplot2 )
library( ggthemes )
library( tidyr )
library( stringr )
library( lubridate )

# themes and options
theme_set( theme_gdocs() )
options(scipen = 999)

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 1) LOAD DATA
# ______________________________________________________________________________________________________________________

DATE <- "2024-07-09"

##  1) PRAGMA-ID
# -------------------------------------------------------

filename <- paste0(outpath,"0_pragma_id_GKV with Stammdata_",DATE,".rds")
id.dat <- readRDS(filename)
rm(filename)

##  2) Insurance periods
# -------------------------------------------------------

filename <- paste0(outpath,"1_data_insurance periods_",DATE,".rds")
ins.dat <- readRDS(filename)
rm(filename)

##  3) QWT Data
# -------------------------------------------------------

filename <- paste0(outpath,"2_QWT data_",DATE,".rds")
qwt.dat <- readRDS(filename)
rm(filename)

##  4) INPAT Data
# -------------------------------------------------------

filename <- paste0(outpath,"2_INPAT data_",DATE,".rds")
inpat.dat <- readRDS(filename)
rm(filename)

##  5) REHA Data
# -------------------------------------------------------

filename <- paste0(outpath,"2_REHA-DRV and GKV data_",DATE,".rds")
reha.dat <- readRDS(filename)
rm(filename)

##  6) AUD and ANY DIAGNOSES
# -------------------------------------------------------

filename <- paste0(outpath,"1_data_all diagnoses_",DATE,".rds")
diag.dat <- readRDS(filename)
rm(filename)

filename <- paste0(outpath,"1_data_alcohol diagnoses_",DATE,".rds")
aud.dat <- readRDS(filename)
rm(filename)

##  7) INCOME
# -------------------------------------------------------
            
filename <- paste0(outpath,"1_data_employment periods_",DATE,".rds")
emp.dat <- readRDS(filename)
rm(filename)

filename <- paste0(outpath,"1_data_income data_",DATE,".rds")
inc.dat <- readRDS(filename)
rm(filename)


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 2) Prepare data
# ______________________________________________________________________________________________________________________

##  DEFINE SAMPLE
#   The sample will comprise persons insured with one of the two SHI with an F10.2 diagnosis in outpatient settings, 
#   who did not have this diagnosis in outpatient settings in the 12 months prior 
#   and who also did not enter withdrawal treatment in that preceding period.

##  1) F10.2/F10.3/F10.4 diagnosis in outpatient settings
#   .............................................

data <- copy(aud.dat[,.(pragmaid,gkv,setting,case.id,icd,icd_type,date.aud = date.diag.start)])
nrow(data) # 360630
length(unique(data$pragmaid)) # 25293

# dates
data$date.aud <- as.Date(data$date.aud)
#data$date.diag.median <- as.Date(data$date.diag.median)
#data$date.diag.end <- as.Date(data$date.diag.end)

# only F10.2, F10.3 or F10.4 in outpatient settings
data <- data[icd %like% "F10.2|F10.3|F10.4" & setting == "outpatient"]
nrow(data) # 122366
length(unique(data$pragmaid)) # 11342

# % confirmed?
data[, prop.table(table(icd_type))] # 91% confirmed
data <- data[icd_type == "confirmed"]
nrow(data) # 111798
length(unique(data$pragmaid)) # 10723

# remove info
data$setting <- NULL
data$icd <- NULL
data$icd_type <- NULL

##  2) M2BF
#   .............................................

# M2BF = mindestens zwei Diagnosen in unterschiedlichen Behandlungsfällen

add <- copy(diag.dat[,.(pragmaid,gkv,setting,case.id,icd,icd_type,date.diag.start,date.diag.end)])

# keep only F10.2
add <- add[icd %like% "F10.2|F10.3|F10.4"]

# keep only relevant types
add[, table(icd_type, useNA = "always")]
add <- add[icd_type %like% "confirmed|primary"]

# dates
add$date.diag.start <- as.Date(add$date.diag.start)
add$date.diag.end <- as.Date(add$date.diag.end)

add <- add[,.(pragmaid,gkv,case.id2 = case.id,date.aud2 = date.diag.start)]

data <- merge(data,
              add, 
              by = c("pragmaid","gkv"), 
              allow.cartesian = T,
              all.x = T)

data <- data[!is.na(date.aud2)]

nrow(data) # 2606169
length(unique(data$pragmaid)) # 10723

# diagnosis2 maximum one year apart?
data <- data[order(pragmaid,date.aud,date.aud2)]
data[, interval := as.numeric(date.aud2) - as.numeric(date.aud)]
data[interval > 365]
data <- data[!(interval > 365 | interval < 0)]

nrow(data) # 587505
length(unique(data$pragmaid)) # 10723 (none removed because the same diagnoses is also considered - next step)

# remove rows with same case IDs
data <- data[!case.id == case.id2]

nrow(data) # 471595
length(unique(data$pragmaid)) # 8587

# keep only info on first AUD diagnosis
data <- unique(data[,.(pragmaid,gkv,case.id,date.aud)])

nrow(data) # 98769
length(unique(data$pragmaid)) # 8587

##  3) Insurance period
#   .............................................

add <- copy(ins.dat[,.(pragmaid,gkv,sex,yob,date.ins.start,date.ins.end)])
add$date.ins.start <- as.Date(add$date.ins.start)
add$date.ins.end <- as.Date(add$date.ins.end)

data <- merge(data,
              add,
              by = c("gkv","pragmaid"), all.x = T)
rm(add)

data <- data[!is.na(date.ins.start) | !is.na(date.ins.end)]

nrow(data) # 105867
length(unique(data$pragmaid)) # 8585

##  keep only rows where the first AUD date falls into insurance periods
data <- data[date.aud %between% list(date.ins.start,date.ins.end)]

nrow(data) # 97942
length(unique(data$pragmaid)) # 8554

##  keep only rows where the first AUD date is at least 12 months after insurance start
data[, lag.days := difftime(date.aud, date.ins.start, units = "days")]
data[, table(lag.days)] ## should be >=365
data <- data[lag.days >= 365,.(gkv,pragmaid,date.aud,sex,yob,date.ins.start,date.ins.end)]

##  keep only rows where the first AUD date is at least 12 months before insurance end
data[, lag.days := difftime(date.ins.end, date.aud, units = "days")]
data[, table(lag.days)] ## should be >=365
data <- data[lag.days >= 365,.(gkv,pragmaid,date.aud,sex,yob,date.ins.start,date.ins.end)]

nrow(data) # 88933
length(unique(data$pragmaid)) # 7933

##  first year at least 2017
data[year(date.aud) >= 2017]
data <- data[year(date.aud) >= 2017]

nrow(data) # 73233
length(unique(data$pragmaid)) # 7248

##  keep first diagnosis only!
data[, mindate := min(date.aud), by = pragmaid]
data <- data[date.aud == mindate]
data$mindate <- NULL

nrow(data) # 8293
length(unique(data$pragmaid)) # 7248

##  4) QWT
#   .............................................

add <- copy(qwt.dat[,.(gkv,pragmaid,KH_FALL_ID,ward,date.kkh.start,date.kkh.end,ENTL301)])

# define QWT dates based on days in hospital (instead of based on OPS codes)
add$date.qwt.start <- as.Date(add$date.kkh.start)
add$date.qwt.end <- as.Date(add$date.kkh.end)
add$date.kkh.start <- NULL
add$date.kkh.end <- NULL

nrow(add) # 3859
length(unique(add$pragmaid)) # 1924

data_qwt <- merge(data,
                  add,
                  by = c("gkv","pragmaid"), all.x = T)
rm(add)

data_qwt <- data_qwt[!is.na(date.qwt.start) | !is.na(date.qwt.end)]

nrow(data_qwt) # 4140
length(unique(data_qwt$pragmaid)) # 1425

##  remove people who had any QWT before the AUD diagnosis
data_qwt[,lag.days := difftime(date.qwt.end, date.aud, units = "days")]
data_qwt[, table(lag.days)] ## should be >=0 (negative numbers = before the AUD diagnosis)
data_qwt <- data_qwt[lag.days >= 0]
data_qwt[, table(lag.days %between% c(-365,-1))] ## none

nrow(data_qwt) # 3052
length(unique(data_qwt$pragmaid)) # 1184

##  show lag time
data_qwt[, .N, by = pragmaid][, summary(N)]
data_qwt[, summary(as.numeric(lag.days))] # overall median = 394
data_qwt[lag.days <= 365, length(unique(pragmaid))] # 799
data_qwt$lag.days <- NULL

##  re-make qwt_id
data_qwt <- data_qwt[order(pragmaid, date.qwt.start)]
data_qwt[, qwt_id := 1:.N, by = pragmaid]

##  5) INPAT
#   .............................................

add <- copy(inpat.dat[,.(gkv,pragmaid,KH_FALL_ID,date.inpat.start,date.inpat.end,ENTL301)])
add$date.inpat.start <- as.Date(add$date.inpat.start)
add$date.inpat.end <- as.Date(add$date.inpat.end)

nrow(add) # 3887
length(unique(add$pragmaid)) # 1793

data_inpat <- merge(data,
                    add,
                    by = c("gkv","pragmaid"), all.x = T)
rm(add)

data_inpat <- data_inpat[!is.na(date.inpat.start) | !is.na(date.inpat.end)]

nrow(data_inpat) # 4164
length(unique(data_inpat$pragmaid)) # 1298

##  remove people who had any inpat before the AUD diagnosis
data_inpat[,lag.days := difftime(date.inpat.end, date.aud, units = "days")]
data_inpat[, table(lag.days)] ## should be >=0 (negative numbers = before the AUD diagnosis)
data_inpat <- data_inpat[lag.days >= 0]
data_inpat[, table(lag.days %between% c(-365,-1))] ## none

nrow(data_inpat) # 3226
length(unique(data_inpat$pragmaid)) # 1069

##  show lag time
data_inpat[, .N, by = pragmaid][, summary(N)]
data_inpat[, summary(as.numeric(lag.days))] # overall median = 628
data_inpat[lag.days <= 365, length(unique(pragmaid))] # 600
data_inpat$lag.days <- NULL

##  re-make inpat_id
data_inpat <- data_inpat[order(pragmaid,date.inpat.start)]
data_inpat[, inpat_id := 1:.N, by = pragmaid]

sum(unique(data_inpat$pragmaid) %in% unique(data_qwt$pragmaid)) # 539


##  6) REHA
#   .............................................

add <- copy(reha.dat[,.(pragmaid,date.reha.start,date.reha.end)])
add$date.reha.start <- as.Date(add$date.reha.start)
add$date.reha.end <- as.Date(add$date.reha.end)

nrow(add) # 4178
length(unique(add$pragmaid)) # 3097

data_reha <- merge(data,
                    add,
                    by = c("pragmaid"), all.x = T)
rm(add)

data_reha <- data_reha[!is.na(date.reha.start) | !is.na(date.reha.end)]

nrow(data_reha) # 1222
length(unique(data_reha$pragmaid)) # 731

##  remove people who had reh 12 months before the AUD diagnosis
data_reha[,lag.days := difftime(date.reha.end, date.aud, units = "days")]
data_reha[, table(lag.days)] ## should be >=0 (negative numbers = before the AUD diagnosis)
data_reha <- data_reha[lag.days >= 0]
data_reha[, table(lag.days %between% c(-365,-1))] ## none

nrow(data_reha) # 912
length(unique(data_reha$pragmaid)) # 573

##  show lag time
data_reha[, .N, by = pragmaid][, summary(N)]
data_reha[, summary(as.numeric(lag.days))] # overall median = 412
data_reha[lag.days <= 365, length(unique(pragmaid))] # 288
data_reha$lag.days <- NULL

##  re-make reha_id
data_reha <- data_reha[order(pragmaid,date.reha.start)]
data_reha[, reha_id := 1:.N, by = pragmaid]

sum(unique(data_reha$pragmaid) %in% unique(data_qwt$pragmaid)) # 362


##  6) CONTROL
#   .............................................

# remove IDs included in any treatment group
toremove <- c(unique(qwt.dat$pragmaid), unique(inpat.dat$pragmaid), unique(reha.dat$pragmaid))
data_control <- data %>% filter(!pragmaid %in% toremove) %>% distinct()

# check whether only one observation per pragmaid
data_control[, .N, by = pragmaid][, summary(N)]

sum(unique(data_control$pragmaid) %in% unique(data_control$pragmaid)) # 5074


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 3) Save data
# ______________________________________________________________________________________________________________________

write.csv(data %>% dplyr::select(-gkv), paste0(outpath,"data_all_",Sys.Date(),".csv"), row.names = F)
write.csv(data_qwt %>% dplyr::select(-gkv), paste0(outpath,"data_qwt_",Sys.Date(),".csv"), row.names = F)
write.csv(data_inpat %>% dplyr::select(-gkv), paste0(outpath,"data_inpat_",Sys.Date(),".csv"), row.names = F)
write.csv(data_reha %>% dplyr::select(-gkv), paste0(outpath,"data_reha_",Sys.Date(),".csv"), row.names = F)
#write.csv(data_control, paste0(outpath,"data_control_",Sys.Date(),".csv"), row.names = F)

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 4) For Umwidmung:
# ______________________________________________________________________________________________________________________

dat <- copy(diag.dat[icd.alc == T,])
dat <- unique(dat[icd == "F10.2" & setting == "inpatient" & icd_type == "primary",.(gkv,pragmaid,date.diag.end)])
dat[, mindate := min(date.diag.end), by = pragmaid]
dat <- dat[date.diag.end == mindate,.(gkv,pragmaid,date.diag.end)]
nrow(dat) # 2932

# add emp.type
add <- unique(emp.dat[,.(gkv,pragmaid,emp.type,start = date.emp.start, end = date.emp.end)])
dat <- merge(dat, add, by = c("gkv","pragmaid"), all.x = T)
dat <- dat[date.diag.end %between% list(start,end)]
nrow(dat) # 2883

dat[, .N, by = pragmaid][N>1] # none
dat <- dat[,.(gkv,pragmaid,year = year(date.diag.end),emp.type)]
rm(add)

# add income
add <- unique(inc.dat[,.(gkv,pragmaid,year = income.year,income)])
dat <- merge(dat, add, by = c("gkv","pragmaid", "year"), all.x = T)
nrow(dat) # 2883

# report
dat[, table(emp.type)] # 957 employed
dat[, prop.table(table(emp.type))]
dat[emp.type == "employed", summary(income)]
dat[!is.na(income) & emp.type == "employed", .N]
dat[!is.na(income) & emp.type == "employed", summary(income)]
dat[!is.na(income) & emp.type == "employed", mean(income<=10000)]



