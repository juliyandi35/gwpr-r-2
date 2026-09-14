library(kableExtra)
library(stats)
library(graphics)
library(grDevices)
library(utils)
library(datasets)
library(methods)
library(base)
library(readxl)
library(dplyr)
library(ggplot2)
library(gplots)
library(corrplot)
library(car)
library(broom)
library(lmtest)
library(sf)

# Import dataset
Dataset <- read_excel("Data-data/DATA GABUNG.xlsx",sheet = "Data Gabung (Tahun)")
names(Dataset)
colnames(Dataset) <- c("KAB","Tahun","Y","X1","X2","X3","X4","X5","X6")
Dataset$KAB <- as.factor(Dataset$KAB)
Dataset$Tahun <- as.factor(Dataset$Tahun)
str(Dataset)

# Pre-processing
colSums(is.na(Dataset))

# Analisis deskriptif
# Keragaman waktu
ggplot(data=Dataset,aes(x=Tahun,y=Y))+
  geom_line()+
  labs(x="Tahun",y="Y")+
  theme(legend.position = "none")+
  theme_bw()

# Keragaman antar individu
plotmeans(Y~KAB,main="Keragaman Y antar lokasi",data=Dataset,n.label = F,xlab = "a")

# Correlation Plot
corrplot(cor(Dataset[-c(1:2)]),method = "color",type = "upper",tl.pos = 'tp')
corrplot(cor(Dataset[-c(1:2)]),method = "number",diag = F,add = T, type = "lower",tl.pos = 'n',cl.pos = 'n')

# Time series plot
ggplot(data=Dataset, aes(x=Tahun, y=Y, group = KAB, colour = KAB))+ theme_bw()+
  geom_line(size=1.2) +
  geom_point(size=3, shape=19, fill="red") + 
  labs(colour="KAB", title = "Y", subtitle = "Tahun 2018-2022") +
  theme(plot.title = element_text(face = "bold"))

# Plot peta
Ind_map = read_sf('Peta Kabupaten/BATAS KABUPATEN KOTA DESEMBER 2019 DUKCAPIL.shp')
head(Ind_map)
Kab_Kalbar <- c("SAMBAS","BENGKAYANG","LANDAK","MEMPAWAH","SANGGAU",
                "KETAPANG","SINTANG","KAPUAS HULU","SEKADAU","MELAWI",
                "KAYONG UTARA","KUBU RAYA","KOTA PONTIANAK","KOTA SINGKAWANG")

Kalbar_map <- subset(Ind_map, KAB_KOTA %in% Kab_Kalbar)
colnames(Kalbar_map) <- c("KAB","geometry")
Kalbar_map

# Membuat plot peta
ggplot() +
  geom_sf(data = Kalbar_map) +
  labs(title = "Peta Kabupaten/Kota di Kalimantan Barat")

Dataset <- merge(Kalbar_map,Dataset,by="KAB")
Dataset

# Export coordinates dari peta
library(spdep)
library(sp)
coords<-data.frame(x = coordinates(as(Dataset,"Spatial"))[,1], y = coordinates(as(Dataset,"Spatial"))[,2])
# writexl::write_xlsx(coords,"Long & Lat.xlsx")

# Uji Multikolinieritas
check_model <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6,data=Dataset)
data.frame(t(vif(check_model)))

# Model Common Effect
library(plm)
cem <- plm(Y ~ X1 + X2 + X3 + X4 + X5 + X6,data=Dataset, model = "pooling")
summary(cem)

# Model Fixed Effect
fem <- plm(Y ~ X1 + X2 + X3 + X4 + X5 + X6,data=Dataset, index = c("KAB", "Tahun"), model = "within", effect= "individual")
summary(fem)
summary(fixef(fem, effect="individual"))

# Model FEM dengan waktu
fem_time <- plm(Y ~ X1 + X2 + X3 + X4 + X5 + X6,data=Dataset, index = c("KAB", "Tahun"), model = "within", effect= "time")
summary(fem_time)
summary(fixef(fem_time, effect="time"))

# Model FEM 2 arah
fem_twoways <- plm(Y ~ X1 + X2 + X3 + X4 + X5 + X6,data=Dataset, index = c("KAB", "Tahun"), model = "within", effect= "twoways")
summary(fem_twoways)
data.frame(summary(fixef(fem_twoways, effect="twoways")))

# Uji SignifX1si Pengaruh Individu / Waktu / Two ways
# Uji pengaruh individu
plmtest(fem_twoways, type = "bp", effect = "individual")

# Uji pengaruh waktu
plmtest(fem_twoways, type = "bp", effect = "time")

# Uji pengaruh twoways
plmtest(fem_twoways, type = "bp", effect = "twoways")

# Nilai Kebaikan Model
# Sum Squared Error
dsse <- data.frame(Individu=sum(fem$residuals^2),Time=sum(fem_time$residuals^2),Twoways=sum(fem_twoways$residuals^2))

# AIC
lsdv_ind <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + KAB,data=Dataset)
lsdv_time <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + Tahun,data=Dataset)
lsdv_twoways <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + KAB + Tahun,data=Dataset)

daic <- data.frame(Individu=AIC(lsdv_ind),Time=AIC(lsdv_time),Twoways=AIC(lsdv_twoways))

# MAPE
mape <- function(actual, forecast) {
  mean(abs((actual - forecast) / actual)) * 100
}
dmape <- data.frame(Individu=mape(Dataset$Y,predict(fem)),
                    Time=mape(Dataset$Y,predict(fem_time)),
                    Twoways=mape(Dataset$Y,predict(fem_twoways)))
# BIC
dbic <- data.frame(Individu=BIC(lsdv_ind),Time=BIC(lsdv_time),Twoways=BIC(lsdv_twoways))

# R-squared
ind <- summary(fem)
time <- summary(fem_time)
tways <- summary(fem_twoways)
drsq <- data.frame(Individu=ind$r.squared,Time=time$r.squared,Twoways=tways$r.squared)

# Perbandingan
compare <- t(rbind(dsse,daic,dbic,dmape,drsq))
colnames(compare) <- c("SSE","AIC","BIC", "MAPE","R-Squared","Adj R-Squared")
compare

# FEM VS CEM
pooltest(cem, fem_twoways) # Keputusan pilih FEM

# REM dengan Generalized Least Square
rem_gls <- plm(Y ~ X1 + X2 + X3 + X4 + X5 + X6, data = Dataset, 
               index = c("KAB", "Tahun"), 
               effect = "twoways", model = "random", random.method = "nerlove")
summary(rem_gls)

#efek individu
plmtest(rem_gls,type = "bp", effect="individu")

#efek waktu 
plmtest(rem_gls,type = "bp", effect="time")

#efek twoways 
plmtest(rem_gls,type = "bp", effect="twoways")

tidy_ranef_ind <- tidy(ranef(rem_gls, effect="individual"))
colnames(tidy_ranef_ind) <- c("KAB", "Pengaruh Acak Individu")
tidy_ranef_ind

tidy_ranef_time <- tidy(ranef(rem_gls, effect="time"))
colnames(tidy_ranef_time) <- c("Tahun", "Pengaruh Acak Waktu")
tidy_ranef_time

# FEM VS REM
# Uji Haussman
phtest(fem_twoways, rem_gls) # Pilih model FEM

# Uji diagnostik residu
# Uji normalitas
ks.test(fem_twoways$residuals, "pnorm", 
        mean=mean(fem_twoways$residuals), 
        sd=sd(fem_twoways$residuals))

# Histogram
ggplot(as.data.frame(fem_twoways$residuals), aes(x = fem_twoways$residuals)) +
  geom_histogram(aes(y = after_stat(density)), color = "white", fill = "steelblue") +
  geom_density(color = "red", linewidth = 1) +
  theme_minimal()

# Uji Autokorelasi
pbgtest(fem_twoways)

# Uji Heteroskedastisitas
bptest(fem_twoways)

# Modeling GWPR
library(GWmodel)
library(sp)
Dataset$ID <- match(Dataset$KAB,unique(Dataset$KAB))

Dataset.df <- read_excel("Data-data/DATA GABUNG.xlsx",sheet = "Data Gabung (Tahun)")
Dataset.df <- data.frame(Dataset.df)
colnames(Dataset.df) <- c("KAB","Tahun","Y","X1","X2","X3","X4","X5","X6")
Dataset.df$ID <- match(Dataset.df$KAB,unique(Dataset.df$KAB))
Dataset.df$KAB <- as.factor(Dataset.df$KAB)
Dataset.df$Tahun <- as.factor(Dataset.df$Tahun)
Dataset.df$Y1 <- Dataset.df$Y

Dataset.sdf <- Kalbar_map
names(Dataset.sdf)
Dataset.sdf$ID <- match(Dataset.sdf$KAB,unique(Dataset.sdf$KAB))
Dataset.sdf <- as(Dataset.sdf,"Spatial")
class(Dataset.sdf)

# Menentukan fungsi pembobot spasial terbaik
#adaptive bisquare
bw.Adaptive <- bw.GWPR(Y1 ~ X1 + X2 + X3 + X4 + X5 + X6, data = Dataset.df,
                    index = c("ID", "Tahun"), SDF = Dataset.sdf, adaptive = TRUE,
                    effect = "twoways", model = "within", approach = "AIC",
                    kernel = "bisquare", longlat = FALSE) # Adaptive lebih baik

bw.Fixed <- bw.GWPR(Y1 ~ X1 + X2 + X3 + X4 + X5 + X6, data = Dataset.df,
                    index = c("ID", "Tahun"), SDF = Dataset.sdf, adaptive = FALSE,
                    effect = "twoways", model = "within", approach = "AIC",
                    kernel = "bisquare", longlat = FALSE) 
str(Dataset)

#install.packages("devtools")
#devtools::install_github(repo = "https://github.com/MichaelChaoLi-cpu/GWPR.light")

library(GWPR.light)
result.Bisquare.Adaptive <- GWPR(bw = bw.Adaptive,Y1 ~ X1 + X2 + X3 + X4 + X5 + X6, data = Dataset.df,
                        index = c("ID", "Tahun"), SDF = Dataset.sdf, adaptive = TRUE,
                        effect = "twoways", model = "within",
                        kernel = "bisquare", longlat = FALSE)
result.Gaussian.Adaptive <- GWPR(bw = bw.Adaptive,Y1 ~ X1 + X2 + X3 + X4 + X5 + X6, data = Dataset.df,
                        index = c("ID", "Tahun"), SDF = Dataset.sdf, adaptive = TRUE,
                        effect = "twoways", model = "within",
                        kernel = "gaussian", longlat = FALSE)
result.Tricube.Adaptive <- GWPR(bw = bw.Adaptive,Y1 ~ X1 + X2 + X3 + X4 + X5 + X6, data = Dataset.df,
                        index = c("ID", "Tahun"), SDF = Dataset.sdf, adaptive = TRUE,
                        effect = "twoways", model = "within",
                        kernel = "tricube", longlat = FALSE)
result.Bisquare.Fixed <- GWPR(bw = bw.Adaptive,Y1 ~ X1 + X2 + X3 + X4 + X5 + X6, data = Dataset.df,
                        index = c("ID", "Tahun"), SDF = Dataset.sdf, adaptive = FALSE,
                        effect = "twoways", model = "within",
                        kernel = "bisquare", longlat = FALSE)
result.Gaussian.Fixed <- GWPR(bw = bw.Adaptive,Y1 ~ X1 + X2 + X3 + X4 + X5 + X6, data = Dataset.df,
                        index = c("ID", "Tahun"), SDF = Dataset.sdf, adaptive = FALSE,
                        effect = "twoways", model = "within",
                        kernel = "gaussian", longlat = FALSE)
result.Tricube.Fixed <- GWPR(bw = bw.Adaptive,Y1 ~ X1 + X2 + X3 + X4 + X5 + X6, data = Dataset.df,
                        index = c("ID", "Tahun"), SDF = Dataset.sdf, adaptive = FALSE,
                        effect = "twoways", model = "within",
                        kernel = "tricube", longlat = FALSE)

SSE <- function(residual){
  sum((residual)^2)
}

Bisquare.Adaptive.SSE <- SSE(result.Bisquare.Adaptive$GWPR.residuals$resid)
Gaussian.Adaptive.SSE <- SSE(result.Gaussian.Adaptive$GWPR.residuals$resid)
Tricube.Adaptive.SSE <- SSE(result.Tricube.Adaptive$GWPR.residuals$resid)
Bisquare.Fixed.SSE <- SSE(result.Bisquare.Fixed$GWPR.residuals$resid)
Gaussian.Fixed.SSE <- SSE(result.Gaussian.Fixed$GWPR.residuals$resid)
Tricube.Fixed.SSE <- SSE(result.Tricube.Fixed$GWPR.residuals$resid)

AIC_value <- function(n,SSE,k){
  n*log(SSE/n) + 2*k
} 

n <- length(result.Bisquare.Adaptive$GWPR.residuals$id) # Jumlah observasi
k <- 6 # Jumlah variabel bebas/Jumlah koefisien yang diestimasi

Bisquare.Adaptive.AIC <- AIC_value(n=n,SSE=Bisquare.Adaptive.SSE,k=k)
Gaussian.Adaptive.AIC <- AIC_value(n=n,SSE=Gaussian.Adaptive.SSE,k=k)
Tricube.Adaptive.AIC <- AIC_value(n=n,SSE=Tricube.Adaptive.SSE,k=k)
Bisquare.Fixed.AIC <- AIC_value(n=n,SSE=Bisquare.Fixed.SSE,k=k)
Gaussian.Fixed.AIC <- AIC_value(n=n,SSE=Gaussian.Fixed.SSE,k=k)
Tricube.Fixed.AIC <- AIC_value(n=n,SSE=Tricube.Fixed.SSE,k=k)

# Perbandingan hasil
Compare2 <- data.frame(R.Squared = c(result.Bisquare.Adaptive$R2,result.Gaussian.Adaptive$R2,
                                     result.Tricube.Adaptive$R2,result.Bisquare.Fixed$R2,
                                     result.Gaussian.Fixed$R2,result.Tricube.Fixed$R2),
                       AIC = c(Bisquare.Adaptive.AIC,Gaussian.Adaptive.AIC,
                               Tricube.Adaptive.AIC,Bisquare.Fixed.AIC,
                               Gaussian.Fixed.AIC,Tricube.Fixed.AIC))

rownames(Compare2) <- c("Bisquare Adaptive","Gaussian Adaptive",
                        "Tricube Adaptive","Bisquare Fixed",
                        "Gaussian Fixed","Tricube Fixed")
Compare2 

# Model terbaik adalah model GWPR pada bisquare adaptive

GWPR.Result <- st_as_sf(result.Bisquare.Adaptive$SDF) # Koefisien pada model terbaik

Dataset$tval.X1 = GWPR.Result$X1_TVa
Dataset$tval.X2 = GWPR.Result$X2_TVa
Dataset$tval.X3 = GWPR.Result$X3_TVa
Dataset$tval.X4 = GWPR.Result$X4_TVa
Dataset$tval.X5 = GWPR.Result$X5_TVa
Dataset$tval.X6 = GWPR.Result$X6_TVa

#---------------------------------------------------------------#
#    signfikansi variabel (variabel X1)
#---------------------------------------------------------------#
Dataset$signfikansi_X1 <- NA
# Signifikan
Dataset[(Dataset$tval.X1 <= -1.998340543 | Dataset$tval.X1 >= 1.998340543), "signfikansi_X1"] <- "Signifikan"

# Tidak Signifikan
Dataset[(Dataset$tval.X1 > -1.998340543 & Dataset$tval.X1 < 1.998340543), "signfikansi_X1"] <- "Tidak Signifikan"

#------------------------------------------------
ggplot(data=Dataset) +
  geom_sf(mapping=aes(fill =signfikansi_X1)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")+ggtitle("Signifikansi X1")

#---------------------------------------------------------------#
#    signfikansi variabel (misal variabel X2)
#---------------------------------------------------------------#
Dataset$signfikansi_X2 <- NA

# Signifikan
Dataset[(Dataset$tval.X2 <= -1.998340543 | Dataset$tval.X2 >= 1.998340543), "signfikansi_X2"] <- "Signifikan"

# Tidak Signifikan
Dataset[(Dataset$tval.X2 > -1.998340543 & Dataset$tval.X2 < 1.998340543), "signfikansi_X2"] <- "Tidak Signifikan"

#------------------------------------------------
ggplot(data=Dataset) +
  geom_sf(mapping=aes(fill =signfikansi_X2)) +
  scale_fill_manual(values = c("#DF536B"))+
  labs(fill="signfikansi")+ggtitle("Signifikansi X2")

#---------------------------------------------------------------#
#    signfikansi variabel (misal variabel X3)
#---------------------------------------------------------------#
Dataset$signfikansi_X3 <- NA

# Signifikan
Dataset[(Dataset$tval.X3 <= -1.998340543 | Dataset$tval.X3 >= 1.998340543), "signfikansi_X3"] <- "Signifikan"

# Tidak Signifikan
Dataset[(Dataset$tval.X3 > -1.998340543 & Dataset$tval.X3 < 1.998340543), "signfikansi_X3"] <- "Tidak Signifikan"

#------------------------------------------------
ggplot(data=Dataset) +
  geom_sf(mapping=aes(fill =signfikansi_X3)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")+ggtitle("Signifikansi X3")

#---------------------------------------------------------------#
#    signfikansi variabel (misal variabel X4)
#---------------------------------------------------------------#
Dataset$signfikansi_X4 <- NA

# Signifikan
Dataset[(Dataset$tval.X4 <= -1.998340543 | Dataset$tval.X4 >= 1.998340543), "signfikansi_X4"] <- "Signifikan"

# Tidak Signifikan
Dataset[(Dataset$tval.X4 > -1.998340543 & Dataset$tval.X4 < 1.998340543), "signfikansi_X4"] <- "Tidak Signifikan"

#------------------------------------------------
ggplot(data=Dataset) +
  geom_sf(mapping=aes(fill =signfikansi_X4)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")+ggtitle("Signifikansi X4")

#---------------------------------------------------------------#
#    signfikansi variabel (misal variabel X5)
#---------------------------------------------------------------#
Dataset$signfikansi_X5 <- NA

# Signifikan
Dataset[(Dataset$tval.X5 <= -1.998340543 | Dataset$tval.X5 >= 1.998340543), "signfikansi_X5"] <- "Signifikan"

# Tidak Signifikan
Dataset[(Dataset$tval.X5 > -1.998340543 & Dataset$tval.X5 < 1.998340543), "signfikansi_X5"] <- "Tidak Signifikan"

#------------------------------------------------
ggplot(data=Dataset) +
  geom_sf(mapping=aes(fill =signfikansi_X5)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")+ggtitle("Signifikansi X5")

#---------------------------------------------------------------#
#    signfikansi variabel (misal variabel X6)
#---------------------------------------------------------------#
Dataset$signfikansi_X6 <- NA

# Signifikan
Dataset[(Dataset$tval.X6 <= -1.998340543 | Dataset$tval.X6 >= 1.998340543), "signfikansi_X6"] <- "Signifikan"

# Tidak Signifikan
Dataset[(Dataset$tval.X6 > -1.998340543 & Dataset$tval.X6 < 1.998340543), "signfikansi_X6"] <- "Tidak Signifikan"

#------------------------------------------------
ggplot(data=Dataset) +
  geom_sf(mapping=aes(fill =signfikansi_X6)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")+ggtitle("Signifikansi X6")

#---------------------------------------------------------------#
#    Plotting variabel signifikan
#---------------------------------------------------------------#
Significant_Map <- Dataset

# Buat kolom baru untuk kombinasi signfikansi
Significant_Map <- Significant_Map %>%
  mutate(Variabel_Signifikan = case_when(
    # Utuh
    signfikansi_X1 == "Signifikan" & signfikansi_X2 == "Signifikan" &
      signfikansi_X3 == "Signifikan" & signfikansi_X4 == "Signifikan" &
      signfikansi_X5 == "Signifikan" & signfikansi_X6 == "Signifikan" ~ "X1,X2,X3,X4,X5,X6",
    
    # Eliminasi 1
    signfikansi_X1 == "Signifikan" & signfikansi_X2 == "Signifikan" &
      signfikansi_X3 == "Signifikan" & signfikansi_X4 == "Signifikan" &
      signfikansi_X5 == "Signifikan" ~ "X1,X2,X3,X4,X5",
    signfikansi_X1 == "Signifikan" & signfikansi_X2 == "Signifikan" &
      signfikansi_X3 == "Signifikan" & signfikansi_X4 == "Signifikan" &
      signfikansi_X6 == "Signifikan" ~ "X1,X2,X3,X4,X6",
    signfikansi_X1 == "Signifikan" & signfikansi_X2 == "Signifikan" &
      signfikansi_X3 == "Signifikan" &
      signfikansi_X5 == "Signifikan" & signfikansi_X6 == "Signifikan" ~ "X1,X2,X3,X5,X6",
    signfikansi_X1 == "Signifikan" & signfikansi_X2 == "Signifikan" &
      signfikansi_X4 == "Signifikan" &
      signfikansi_X5 == "Signifikan" & signfikansi_X6 == "Signifikan" ~ "X1,X2,X4,X5,X6",
    signfikansi_X1 == "Signifikan" &
      signfikansi_X3 == "Signifikan" & signfikansi_X4 == "Signifikan" &
      signfikansi_X5 == "Signifikan" & signfikansi_X6 == "Signifikan" ~ "X1,X3,X4,X5,X6",
    signfikansi_X2 == "Signifikan" &
      signfikansi_X3 == "Signifikan" & signfikansi_X4 == "Signifikan" &
      signfikansi_X5 == "Signifikan" & signfikansi_X6 == "Signifikan" ~ "X2,X3,X4,X5,X6",
    
    # Eliminasi 2
    signfikansi_X1 == "Signifikan" & signfikansi_X2 == "Signifikan" &
      signfikansi_X3 == "Signifikan" & signfikansi_X4 == "Signifikan" ~ "X1,X2,X3,X4",
    signfikansi_X1 == "Signifikan" & signfikansi_X2 == "Signifikan" &
      signfikansi_X3 == "Signifikan" & signfikansi_X6 == "Signifikan" ~ "X1,X2,X3,X6",
    signfikansi_X1 == "Signifikan" & signfikansi_X2 == "Signifikan" &
      signfikansi_X5 == "Signifikan" & signfikansi_X6 == "Signifikan" ~ "X1,X2,X5,X6",
    signfikansi_X1 == "Signifikan" & signfikansi_X4 == "Signifikan" &
      signfikansi_X5 == "Signifikan" & signfikansi_X6 == "Signifikan" ~ "X1,X4,X5,X6",
    signfikansi_X3 == "Signifikan" & signfikansi_X4 == "Signifikan" &
      signfikansi_X5 == "Signifikan" & signfikansi_X6 == "Signifikan" ~ "X3,X4,X5,X6",
    signfikansi_X2 == "Signifikan" & signfikansi_X3 == "Signifikan" & 
      signfikansi_X4 == "Signifikan" & signfikansi_X5 == "Signifikan" ~ "X2,X3,X4,X5",
    
    # Eliminasi 3
    signfikansi_X1 == "Signifikan" & signfikansi_X2 == "Signifikan" &
      signfikansi_X3 == "Signifikan" ~ "X1,X2,X3",
    signfikansi_X1 == "Signifikan" & signfikansi_X2 == "Signifikan" &
      signfikansi_X6 == "Signifikan" ~ "X1,X2,X6",
    signfikansi_X1 == "Signifikan" & signfikansi_X5 == "Signifikan" & 
      signfikansi_X6 == "Signifikan" ~ "X1,X5,X6",
    signfikansi_X4 == "Signifikan" & signfikansi_X5 == "Signifikan" &
      signfikansi_X6 == "Signifikan" ~ "X4,X5,X6",
    signfikansi_X3 == "Signifikan" & signfikansi_X4 == "Signifikan" &
      signfikansi_X5 == "Signifikan" ~ "X3,X4,X5",
    signfikansi_X2 == "Signifikan" & signfikansi_X3 == "Signifikan" & 
      signfikansi_X4 == "Signifikan" ~ "X2,X3,X4",
    
    # Eliminasi 4
    signfikansi_X1 == "Signifikan" & signfikansi_X2 == "Signifikan" ~ "X1,X2",
    signfikansi_X1 == "Signifikan" & signfikansi_X6 == "Signifikan" ~ "X1,X6",
    signfikansi_X5 == "Signifikan" & signfikansi_X6 == "Signifikan" ~ "X5,X6",
    signfikansi_X4 == "Signifikan" & signfikansi_X5 == "Signifikan" ~ "X4,X5,",
    signfikansi_X3 == "Signifikan" & signfikansi_X4 == "Signifikan" ~ "X3,X4",
    signfikansi_X2 == "Signifikan" & signfikansi_X3 == "Signifikan" ~ "X2,X3",
    
    # Eliminasi 5
    signfikansi_X1 == "Signifikan" ~ "X1",
    signfikansi_X6 == "Signifikan" ~ "X6",
    signfikansi_X5 == "Signifikan" ~ "X5",
    signfikansi_X4 == "Signifikan" ~ "X4",
    signfikansi_X3 == "Signifikan" ~ "X3",
    signfikansi_X2 == "Signifikan" ~ "X2",
    
    TRUE ~ "Tidak Signifikan"
  ))

# Buat skema warna kustom
warna_custom <- c(
  "X1,X2,X3,X4,X5,X6" = "black",
  "X1,X2,X3,X4,X5" = "red",
  "X1,X2,X3,X4,X6" = "green",
  "X1,X2,X3,X5,X6" = "blue",
  "X1,X2,X4,X5,X6" = "cyan",
  "X1,X3,X4,X5,X6" = "magenta",
  "X2,X3,X4,X5,X6" = "yellow",
  "X1,X2,X3,X4" = "gray",
  "X1,X2,X3,X6" = "darkgray",
  "X1,X2,X5,X6" = "lightgray",
  "X1,X4,X5,X6" = "orange",
  "X3,X4,X5,X6" = "brown",
  "X2,X3,X4,X5" = "pink",
  "X1,X2,X3" = "violet",
  "X1,X2,X6" = "purple",
  "X1,X5,X6" = "orchid",
  "X4,X5,X6" = "lavender",
  "X3,X4,X5" = "plum",
  "X2,X3,X4" = "maroon",
  "X1,X2" = "firebrick",
  "X1,X6" = "tomato",
  "X5,X6" = "aquamarine",
  "X4,X5" = "turquoise",
  "X3,X4" = "skyblue",
  "X2,X3" = "dodgerblue",
  "X1" = "steelblue",
  "X6" = "royalblue",
  "X5" = "navyblue",
  "X4" = "midnightblue",
  "X3" = "cornflowerblue",
  "X2" = "darkslateblue",
  "Tidak Signifikan" = "white"
)

ggplot(data = Significant_Map) +
  geom_sf(mapping=aes(geometry = geometry,fill = Variabel_Signifikan)) +
  scale_fill_manual(values = warna_custom)+
  labs(fill="Variabel Signifikan")
