rm(list=ls())

# Exporting From 02 -------------------------------------------------------
source("02 ARIMAX 800 data per Lex.R")

# Export mydata.raw
write.csv(mydata.raw,"04 Data Agregasi Sentimen Harian.csv")

# Export Preliminary DR Model Coefficients
write.csv(t(M.1.DF), "04 Data Koefisien Model Awal DR.csv")

# Export Final DR Model Coefficients
write.csv(t(M.3.DF), "04 Data Koefisien Model Akhir DR.csv")

# Export Ljung-Box Test and Shapiro-Wilk Test
write.csv(cbind(BT,ST), "04 Hasil Uji Ljung-Box dan Shapiro-Wilk.csv")

# Exporting From 03 -------------------------------------------------------
source("03 Sliding Windows ARIMA vs ARIMAX.R")

# Export rsq.df.10
write.csv(rsq.df.10, "04 R-Squared Model Sliding Windows Periode Fitting satu tahun.csv")

# Export rsq.df.15
write.csv(rsq.df.15, "04 R-Squared Model Sliding Windows Periode Fitting satu setengah tahun.csv")

# Export rsq.df.20
write.csv(rsq.df.20, "04 R-Squared Model Sliding Windows Periode Fitting dua tahun.csv")

# Export mse.df.10
write.csv(mse.df.10, "04 MSE Model Sliding Windows Periode Fitting satu tahun.csv")

# Export mse.df.15
write.csv(mse.df.15, "04 MSE Model Sliding Windows Periode Fitting satu setengah tahun.csv")

# Export mse.df.20
write.csv(mse.df.20, "04 MSE Model Sliding Windows Periode Fitting dua tahun.csv")

# Export aic.df.10
write.csv(aic.df.10, "04 AIC Model Sliding Windows Periode Fitting satu tahun.csv")

# Export aic.df.15
write.csv(aic.df.15, "04 AIC Model Sliding Windows Periode Fitting satu setengah tahun.csv")

# Export aic.df.20
write.csv(aic.df.20, "04 AIC Model Sliding Windows Periode Fitting dua tahun.csv")

# Export p.arimax.10
write.csv(p.arimax.10, "04 Persentase Perbandingan MSE Periode Fiting satu tahun.csv")

# Export p.arimax.15
write.csv(p.arimax.15, "04 Persentase Perbandingan MSE Periode Fiting satu setengah tahun.csv")

# Export p.arimax.20
write.csv(p.arimax.20, "04 Persentase Perbandingan MSE Periode Fiting dua tahun.csv")
