
# Capstone Project

############### Library ############### 
library(mvtnorm)
library(MASS)
library(pastecs)
library(foreign)
library(plyr)
library(dplyr)
library(doBy)
library(ggplot2)
library(reshape2)
library(robustbase)
library(shiny)
library(arules)
library(arulesViz)
library(Hmisc)
library(rpart)
library(devtools)
library(e1071) 
library(stargazer)
library(RobStatTM)
library(doParallel)
library(solitude)
library(readr)
library(data.table)
library(dbscan)
library(forecast)
library(gridExtra)

options(scipen=999)
rm(list=ls())


############### Preprocessing ############### 

setwd("/BDA.Full/Capstone Project/files/MEFfiles/R")

full <- read_csv2("MV_KIOSHAMDATAGUNLUKTEKIL_v1.csv")

#musteriler <- read_csv2("CMV_T_MUSTERILER.csv")

#str(full) # 890118 obs. of  122 variables

# Total Missing Values
NAs.full <- as.data.frame(sapply(full, function(x) sum(is.na(x))/length(x))*100)


### Feature Elimination
# Features consist of more than 40% NA values are omitted. - 29 Features, 
drop.cols1 <- c("KSD_SONUC", "KSD_KARARFIRMASEGMENT", "KSD_KARARSONUC", "STC_DUYUM", "STC_KARARSONUC", "STC_HAMCEKID",
                "STC_HAMMEMZUCID", "ISTISNA/MAHSUP", "ISTISNA/CEK TUTARI KUCUK", "ISTISNA/GUCLU SATICI", 
                "ISTISNA/BUYUK KESIDECI",
                "ISTISNA/DIGER", "ISTISNA/GUCLU CIRANTA", "DUZENLEME/KULLANICI TALEBI-RNK", "STC_SUBE", 
                "ISTISNA/GENEL",
                "ISTISNA/CEK<=2000", "E_CEKHESAPNO", "E_CEKBANKA", "E_CEKSUBE", "E_CEKNO", "E_CEKRECNO", "E_CEKSTATU",
                "E_VKN", "FB_FIRMAADI", "E_ISTONAY", "E_ONAYLAYANAD", "E_ISTUZMANIAD", "E_ISTSONONAY")
full <- full %>% select(-one_of(drop.cols1))


# Duplicate columns identified and omitted >> STC_KTUTAR = STC_MUSTERILIMITI, CEKID = MaxID
#dupps <- full[ , which(!duplicated(t(full)))]
#setdiff(full,dupps)
drop.cols2 <- c("CEKID2", "STC_KAYNAKNO", "STC_KISIID", "CEK_ID", "STC_STATU", "STC_MUSTERILIMITI")
full <- full %>% select(-one_of(drop.cols2))



# Features with contact info or ID eliminated.
drop.cols3 <- c("KSD_SATICI_KISIID","YENI_VKN", "YENI_CEKNO", "CEK_RECNO", "CEK_CEKNO","CEK_CEKHESAPNO",
                "CEK_KARARCEKID",
                "KSD_KISIID", "KSD_VKN","KSD_FIRMANO", "KSD_FIRMAADI", "KSD_HAMMEMZUCID", "STC_VKN", "STC_FIRMANO",
                "STC_FIRMAADI", "STC_MT", "STC_ANASEKTOR", "STC_MTKOD", "KSD_HAMCEKID", "KSD_HAMRISKID",
                "KSD_KAYNAKNO",
                "STC_HAMRISKID")
full <- full %>% select(-one_of(drop.cols3))


# E_SUBE omitted from dataset, categorical feature which shows where the transaction held, not much informative
# full$E_SUBE <- NULL, keep it
full$KSD_SUBE <- NULL 


# ISLEM_TIPI consist of 20% of NA values and not very much informative. 
full$ISLEM_TIPI <- NULL # NEEDS IMPUTATION, hard to impute, may distort data


# KSD_TAKIPKODU is also omitted, more than 20% NA 
full$KSD_TAKIPKODU <- NULL


# STC_FRMSGMNTGUNCELLEMETARIH omitted, not informative
full$STC_FRMSGMNTGUNCELLEMETARIH <- NULL


# CEK_CEKSUBE and CEK_CEKBANKA features represent characters but encoded as number, should be omitted
full$CEK_CEKSUBE <- NULL
full$CEK_CEKBANKA <- NULL


# KSD_KAYNAK omitted, only 1 value in column
full$KSD_KAYNAK <- NULL
full$KSD_KISI_TIP <- NULL
full$STC_KAYNAK <- NULL
full$STC_KISI_TIP <- NULL
full$KSD_STATU <- NULL


# not very informative, only information 4483 row = KIOS
full$KSD_CREATEUSR <- NULL


# E_ISTDURUM not very informative, system message contains information about system process
full$E_ISTDURUM <- NULL
full$E_DETAYSTATU <- NULL
#full$E_ANASTATU <- NULL


# KSD_CREATEDAY = KSD_CREATEDATE omitted
full$KSD_CREATEDAY <- NULL


# CEK_FAIZORAN omitted from data since rejected checks automatically got 0.0 interest rate in this feature.
full$CEK_FAIZORAN <- NULL

# CEK_CUTOFF_CEKRENK is omitted because CEK_PRE_CEKRENK is the updated form of CUTOFF feature, CUTOFF is initial 
# color of check. Same statement is also valid for CEK_KIOSCEKRENK = CEK_CEKRENK
full$CEK_CUTOFF_CEKRENK <- NULL
full$CEK_KIOSCEKRENK <- NULL
# CEK_PRE_CEKRENK is previous version of CEK_CEKRENK, pre cekrenk should be omitted
full$CEK_PRE_CEKRENK <- NULL


# CEK_PRE_CEKSKOR is identical with CEK_CEKSKOR, ===> EK_PRE_CEKSKOR omitted
full$CEK_PRE_CEKSKOR <- NULL


# "KSD_KESIDECIGUNADET" is feature that shows kesideci's day count in the system "KSD_KESIDECIHAFTAADET" = weekly(same)
full$KSD_KESIDECIHAFTAADET <- NULL


# STC_FIRMASEGMENT represents work secgment of check seller but it is encoded as number which represent category
# should be omitted or encoded as factor. STC_KARARFIRMASEGMENT is better
full$STC_FIRMASEGMENT <- NULL


# CEK_VADE - CEK_ISLEMTARIHI = CEKORTVADE
full$CEK_CEKVADE <- NULL
full$E_ISLEMTARIHI <- NULL


# STC_KARARFIRMASEGMENTTXT, omitted identical with E_ISLEMSEGMENT
full$STC_KARARFIRMASEGMENTTXT <- NULL



# Categorical feature transformation  
# CEK_KARARSONUC shows 
full$CEK_KARARSONUC[full$CEK_KARARSONUC=="EVET"] <- 1
full$CEK_KARARSONUC[full$CEK_KARARSONUC=="HAYR"] <- 0
# CEK_KARARSTATU shows 
full$CEK_KARARSTATU[full$CEK_KARARSTATU=="Tamamlandi"] <- 1
full$CEK_KARARSTATU[full$CEK_KARARSTATU=="Manuel"] <- 0
# if data about seller is obtained = 1
full$STC_SONUC[full$STC_SONUC=="EVET"] <- 1
full$STC_SONUC[full$STC_SONUC=="HAYR"] <- 0
# if its the first transaction of seller = 1
full$STC_STCILKISLEM[full$STC_STCILKISLEM=="E"] <- 1
full$STC_STCILKISLEM[full$STC_STCILKISLEM=="H"] <- 0




# Target feature is CEK_ISTIHBARATSONUC,  contains 6 labels > iade, onay, oto-onay, oto-red, red, SF(system failure)
full$CEK_ISTIHBARATSONUC[full$CEK_ISTIHBARATSONUC=="OTO-ONAY"] <-"ONAY"
full$CEK_ISTIHBARATSONUC[full$CEK_ISTIHBARATSONUC=="OTO-RED"] <- "RED"
full$CEK_ISTIHBARATSONUC[full$CEK_ISTIHBARATSONUC=="iADE"] <- "RED"



# 3 features about interest rate not in percentage form, they are divided by 100
full$CEK_ISLEMFAIZI <- full$CEK_ISLEMFAIZI/100
full$CEK_HESAPLANANFAIZORAN <- full$CEK_HESAPLANANFAIZORAN/100
full$E_FAIZORAN <- full$E_FAIZORAN/100




# KSD_KESIDECITIP transformation
full$KSD_KESIDECITIP[full$KSD_KESIDECITIP=="<B1>"] <- "B1"
full$KSD_KESIDECITIP[full$KSD_KESIDECITIP=="<B2>"] <- "B2"
full$KSD_KESIDECITIP[full$KSD_KESIDECITIP=="<K1>"] <- "K1"
full$KSD_KESIDECITIP[full$KSD_KESIDECITIP=="<K2>"] <- "K2"


# KSD_GERCEKTUZEL & STC_GERCEKTUZEL changed 
colnames(full)[colnames(full)=="KSD_GERCEKTUZEL"] <- "KSD_TUZEL"
full$KSD_TUZEL[full$KSD_TUZEL=="T"] <- 1
full$KSD_TUZEL[full$KSD_TUZEL=="G"] <- 0

colnames(full)[colnames(full)=="STC_GERCEKTUZEL"] <- "STC_TUZEL"
full$STC_TUZEL[full$STC_TUZEL=="T"] <- 1
full$STC_TUZEL[full$STC_TUZEL=="G"] <- 0



# E_ANASTATU <- fiyatlaniyor, kesinlesti and limitTahsis omitted because they don't mean anything about anomaly
full<-full[!(full$E_ANASTATU=="Fiyatlaniyor" | full$E_ANASTATU=='Kesinlesti' | full$E_ANASTATU=='Limit Tahsis' ),]



### Missing value Replacement

# E_ISLEMSEGMENT contains 202 missing values, these rows are omitted from data, since changing them with mode or median
# not a good option. That feature contains information about classification of a customer.
full <- full[!is.na(full$E_ISLEMSEGMENT),]



# CEK_CEKORTVADEGUNSAYISI is equal to CEK_CEKVADEGUNSAYISI, thus omitted from data.
full$CEK_CEKORTVADEGUNSAYISI <- NULL


# STC_RISK is now omitted, but it's a question to ask, what to do with these missing features ?????????????????
full$STC_RISK <- NULL


# STC_TAKiPKODU = 50k missing value and categorical feature, NAs replaced with "Normal", question to ask ??
#full$STC_TAKiPKODU[is.na(full$STC_TAKiPKODU)] <- "Normal"
full$STC_TAKiPKODU <- NULL








# Numeric Features - features
numeric <- full[, names(full[, sapply(full, is.numeric)])]
drop.cols.num <- c("MAXID")
numeric <- numeric %>% select(-one_of(drop.cols.num))
numeric <- as.data.frame(numeric)


# Categorical Features - 18 features 
categorical <- full[, names(full[, sapply(full, is.character)])]
# Remove whitespace
categorical_v1 <- as.data.frame(apply(categorical,2,function(x)gsub('\\s+', '',x)))
categorical_v1$KSD_CREATEDATE <- NULL
cat_names <- names(categorical_v1)
full[, cat_names] <- categorical_v1
full <- as.data.frame(full)





### Descriptive statistics
stargazer(numeric, type="text",digits=0, out="table4.txt")
summary(numeric)
df.stats <- numeric
df.stats$CEK_HESAPLANANFAIZORAN <- NULL # omitted distorting the model
stargazer(as.data.frame(df.stats), type = "html", title="Descriptive Statistics", median = T, digits = 1,
          align = T, out="table1.htm", nobs = F)



### LAST FEATURE ELIMINATION
# 
#describe(full$KSD_KESIDECIGUNADET)
full$KSD_KESIDECIGUNADET <- NULL # 99.5% of values equal to zero

# KSD_MUSTERILIMITI  >>> 96% of values equal to zero, 
quantile(full$KSD_MUSTERILIMITI, probs = c(.96,.99)) 

quantile(full$KSD_ALICILIMITI, probs = c(.975,.98)) # 97.5% equal to zero

quantile(full$KSD_MUSTERIRISKI, probs = c(.98,.99)) # 98% equal to zero


### Correlation Matrix
options(digits = 1)
correlation.matrix <- cor(numeric)
corrs <- setDT(melt(correlation.matrix))[order(value)]
corrs <- subset(corrs, value != 1)


# final dataset stored
full_ready <- full
#write.csv(full,'full_data_last.csv')



############### Preprocessing ENDS ############### 



############### EDA ###############

# Data has been scaled for better visualization
full_sc <- full # raw data stored

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

for (var in 1:ncol(full_sc)) {
  if (class(full_sc[,var]) %in% "numeric") {
    full_sc[, var] <- normalize(full_sc[, var])
  }
}



## Visualization
# 1) count of cekrenk with istihbaratsonuc feature, clearly shows that company only accepts sari and yesil checks and
# mostly works with green checks 
quartz()
g1 <- ggplot(data=full,aes(x=CEK_CEKRENK))
g1 + geom_histogram(aes(fill=E_ANASTATU),color="Black", stat="count") +
  scale_y_continuous(limits = c(0, 375000), breaks = seq(0, 375000, 25000)) +
  labs(title="Countplot of Check Colors with Final Status", y="Count", x = "Check Color") + 
  theme_light() + theme(legend.title=element_blank()) + 
  scale_fill_manual(values = c("#990000", "#339900"))



# 2) vade gun say?s? vs cekskor, sonuc colored
# company mostly accepts checks that have score higher than 650 and 
# maturity days between 0 and 200
quartz()
g2 <- ggplot(data=full,aes(x=CEK_CEKVADEGUNSAYISI,y=CEK_CEKSKOR))
g2 +  geom_point(alpha = 0.2, aes(color=E_ANASTATU)) +
  scale_x_continuous(limits = c(0, 500), breaks = seq(0, 500, 50)) +
  scale_y_continuous(limits = c(500, 700), breaks = seq(500, 700, 25)) +
  labs(title="Scatterplot of Remaining Days Until Maturity and Check Scores", y="Check Score", x = "Remaining Days") +
   theme_light() + theme(legend.title=element_blank(),
                        legend.key.size = unit(1.5, "cm"),
                        legend.key.width = unit(1.5,"cm")) + 
  scale_color_manual(values = c("#990000", "#339900"))




# 3) Histogram of CEK_TUTAR
describe(subset(full$CEK_TUTAR, CEK_TUTAR > 150000)) # only 6607 observation is higher than 150k

quartz()
g3 <- ggplot(data=subset(full, CEK_TUTAR < 150000), aes(x=CEK_TUTAR))
g3 + geom_histogram(binwidth = 20000,aes(fill=E_ANASTATU),color="Black") +
  scale_x_continuous(limits = c(0, 150000), breaks = seq(0, 150000, 20000)) +
  scale_y_continuous(limits = c(0, 400000), breaks = seq(0, 400000, 50000)) +
  labs(title="Distribution of Check Amounts", y="Count", x = "Check Amount") +
  theme_light() + theme(legend.title=element_blank()) + 
  scale_fill_manual(values = c("#990000", "#339900")) #+ 
#facet_grid(CEK_ISTIHBARATSONUC~., scales = "free")



# 4) # look for outliers with box plot, y= what we searching, x = CEK_ISTIHBARATSONUC
quartz()
u <- ggplot(data=full, aes(x=CEK_ISTIHBARATSONUC,y=CEK_TUTAR,color=CEK_ISTIHBARATSONUC))
u + geom_boxplot() +
  labs(title="Boxplot of Check Scores", y="Check Score", x = "Remaining Days") + 
  theme_light() + theme(legend.title=element_blank(),
                        legend.key.size = unit(1.5, "cm"),
                        legend.key.width = unit(1.5,"cm")) + 
  scale_color_manual(values = c("#339900", "#990000", "#5ab4ac"))


# 5) interest rates examination 
quartz()
boxint1 <- ggplot(data=full, aes(x=CEK_ISTIHBARATSONUC,y=CEK_ISLEMFAIZI,color=CEK_ISTIHBARATSONUC))
boxint1 + geom_boxplot() +
  labs(title="Boxplot of Interest Rates of Checks", y="Check Score", x = "Remaining Days") + 
  theme_light() + theme(legend.title=element_blank(),
                        legend.key.size = unit(1.5, "cm"),
                        legend.key.width = unit(1.5,"cm")) + 
  scale_color_manual(values = c("#339900", "#990000", "#5ab4ac"))


# Calculated interest rate boxplot without 3 outliers
quartz()
boxint2 <- ggplot(data=subset(full, CEK_HESAPLANANFAIZORAN < 1), aes(x=E_ANASTATU,
                                                                     y=CEK_HESAPLANANFAIZORAN,
                                                                     color=E_ANASTATU))
boxint2 + geom_boxplot() +
  labs(title="Boxplot of Calculated Interest Rates", y="Calculated Interest Rate", x = "Check Status") + 
  theme_light() + theme(legend.title=element_blank(),
                        legend.key.size = unit(1.5, "cm"),
                        legend.key.width = unit(1.5,"cm")) + 
                  scale_color_manual(values = c("#990000", "#339900")) +
                 scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.05)) + coord_flip()



# 
boxint3 <- ggplot(data=full, aes(x=CEK_ISTIHBARATSONUC,y=CEK_ISLEMFAIZI,color=CEK_ISTIHBARATSONUC))
boxint3 + geom_boxplot() +
  labs(title="Boxplot of Check Scores", y="Check Score", x = "Remaining Days") + 
  theme_light() + theme(legend.title=element_blank(),
                        legend.key.size = unit(1.5, "cm"),
                        legend.key.width = unit(1.5,"cm")) + 
  scale_color_manual(values = c("#339900", "#990000", "#5ab4ac"))




# Another graph
quartz()
q <- ggplot(data=full,aes(x=CEK_CEKSKOR,y=log(CEK_TUTAR),
                          colour=CEK_KIOSCEKRENK))
q + geom_jitter()


# cek score density graph with final cek renk
s <- ggplot(data=full,aes(x=CEK_CEKSKOR))
s + geom_density(aes(fill=CEK_KIOSCEKRENK))


# cek score density graph with final cek renk
s <- ggplot(data=full,aes(x=CEK_ISTIHBARATSONUC))
s + geom_histogram(aes(fill=CEK_ISTIHBARATSONUC),color="Black", stat="count")


# seller segment with result
quartz()
s1 <- ggplot(data=full,aes(x=STC_KARARFIRMASEGMENTTXT))
s1 + geom_histogram(aes(fill=CEK_ISTIHBARATSONUC),color="Black", stat="count") +
  theme(axis.text.x = element_text(angle = 90))



############### MODELS ###############

# Parallelization
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
stopCluster(cl)

# Final dataset for analysis

# DATE creation
full$DATE=as.Date(substr(full$KSD_CREATEDATE,1,10),"%d.%m.%Y")
full$KSD_CREATEDATE <- NULL
full$DATE <- as.Date(full$DATE)

drop.cols4 <- c("MAXID", "E_SUBE", "DATE")
final.full <- full %>% select(-one_of(drop.cols4))



# Final Numeric features
df.num <- final.full[, names(final.full[, sapply(final.full, is.numeric)])]

# One Hot Encoding
df.mix <- model.matrix(~.+0,data = final.full)


# Scaling with z-score
zscore= function(x) { (x - mean(x)) / sd(x) }


normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

for (var in 1:ncol(df.num)) {
  if (class(df.num[,var]) %in% "numeric") {
    df.num[, var] <- normalize(df.num[, var])
  }
}

df.num <- as.data.table(df.num)
df.num <- df.num[, lapply(.SD,zscore)]


# random sampling 100k
set.seed(15)
df.num <- final.full[, names(final.full[, sapply(final.full, is.numeric)])]
df.num <- as.data.table(df.num)
df.num <- df.num[, lapply(.SD,zscore)]

index <- sample(nrow(full), 100000, replace = F)
df.num <- df.num[index,]
df.full <- full[index,]



# Mahalanobis distances are calculated with numeric features

# noise for MCD calculations
e=mvrnorm(n = 100000, mu=rep(0,20), Sigma=diag(rep(0.05,20)))

# Mahalanobis Distance
MAH <- sqrt(mahalanobis(df.num,colMeans(df.num),cov(df.num)))
df.MAH <- as.data.frame(MAH)
# MCD Mahalanobis
MCD <- sqrt(covMcd(df.num+e)$mah)

# Evaluation of both methods
compare.distances <- cbind(MAH, MCD, df.full$E_ANASTATU)
compare.distances <- as.data.frame(compare.distances)
compare.distances$MAH <- as.numeric(as.character(compare.distances$MAH))
compare.distances$MCD <- as.numeric(as.character(compare.distances$MCD))


# top 10% should be highly probable anomalous cases
MCD_comp <- compare.distances[compare.distances$MCD > quantile(compare.distances$MCD,prob=0.9),]
MAH_comp <- compare.distances[compare.distances$MAH > quantile(compare.distances$MAH,prob=0.9),]

table(MCD_comp$V3) # mcd = 84.93%
table(MAH_comp$V3) # mah = 81.30%

df.plotMAH <- as.data.frame(cbind(MAH, MCD))
colnames(df.plotMAH)[1] <- "MAH"
colnames(df.plotMAH)[2] <- "MCD"
df.plotMAH$Date <- as.Date(df.full$DATE)

# Visualization of both models
mcd.plot <- ggplot(data=df.plotMAH, aes(x=Date,y=MCD)) + geom_line()
mah.plot <- ggplot(data=df.plotMAH, aes(x=Date,y=MAH)) + geom_line()
grid.arrange(mcd.plot, mah.plot)


############### LOF ###############
# LOF with k=10
set.seed(1)
lof10 <- lof(df.num, k=10)
#hist(lof10, breaks=10)

lof10.scores <- as.data.frame(lof10)

test_lof10 <- cbind(df.full$E_ANASTATU, lof10.scores)
test_lof_top10 <- test_lof10[test_lof10$lof10 > quantile(test_lof10$lof10,prob=0.9),]
table(test_lof_top10$`df.full$E_ANASTATU`)


# LOF with k=20
set.seed(1)
lof20 <- lof(df.num, k=20)

lof20.scores <- as.data.frame(lof20)

test_lof20 <- cbind(df.full$E_ANASTATU, lof20.scores)
test_lof_top20 <- test_lof20[test_lof20$lof20 > quantile(test_lof20$lof20,prob=0.9),]
table(test_lof_top20$`df.full$E_ANASTATU`)


# LOF with k=30
set.seed(1)
lof30 <- lof(df.num, k=30)

lof30.scores <- as.data.frame(lof30)

test_lof30 <- cbind(df.full$E_ANASTATU, lof30.scores)
test_lof_top30 <- test_lof30[test_lof30$lof30 > quantile(test_lof30$lof30,prob=0.9),]
table(test_lof_top30$`df.full$E_ANASTATU`)


# LOF with k=40
set.seed(1)
lof40 <- lof(df.num, k=40)

lof40.scores <- as.data.frame(lof40)

test_lof40 <- cbind(df.full$E_ANASTATU, lof40.scores)
test_lof_top40 <- test_lof40[test_lof40$lof40 > quantile(test_lof40$lof40,prob=0.9),]
table(test_lof_top40$`df.full$E_ANASTATU`)


# LOF with k=50
set.seed(1)
lof50 <- lof(df.num, k=50)

lof50.scores <- as.data.frame(lof50)

test_lof50 <- cbind(df.full$E_ANASTATU, lof50.scores)
test_lof_top50 <- test_lof50[test_lof50$lof50 > quantile(test_lof50$lof50,prob=0.9),]
table(test_lof_top50$`df.full$E_ANASTATU`)


############### Isolation Forest ###############
# 300 trees = 81.05, 0.7 fraction = 82.71
# 250 trees = 80.95
# 150 trees = 80.91, 0.7 fraction = 83.25, 0.6 fraction = 83.1, 0.1 fraction = 86.65
# 100 trees = 80.81, 0.6 fraction = 82.96, 0.7 fraction = 83.04, 0.3 fraction = 84.69, 0.2 fraction = 84.98, 0.1 frac=86.63
set.seed(1)
isf = isolationForest$new(num_trees = 150, sample_fraction = 0.1)  
isf$fit(df.num)  
if_scores <- isf$scores$anomaly_score                 
if_scores <- as.data.frame(if_scores)


test_if <- cbind(df.full$E_ANASTATU, if_scores)
test_if_top <- test_if[test_if$if_scores > quantile(test_if$if_scores,prob=0.9),]
table(test_if_top$`df.full$E_ANASTATU`)

plot(density(isf$scores$anomaly_score))
