## Perbandingan Performa Pemodelan Indeks DXY antara Model Autoregressive Integrated Moving Average (ARIMA) dan Model Dynamic Regression (DR) dengan Data Sentimen Berita Ekonomi Amerika Serikat

#### Penjelasan setiap berkas data yang disediakan pada *directory* utama:
1. "00 MPQA Subjectivity Lexicon - Raw Data.zip" : Leksikon MPQA yang diunduh dari laman http://mpqa.cs.pitt.edu/
2. "00 MPQA Subjectivity Lexicon.csv" : Leksikon MPQA yang telah diolah dengan menghilangkan beberapa kolom yang tidak digunakan dalam skripsi
3. "00 UnSupLab 10-22.csv" : Data berita yang diperoleh dari laman https://www.marketwatch.com/ dengan bantuan *web scraper* yang disediakan oleh https://webscraper.io/
4. "00 SentiWord.csv" : Data sentimen *SentiWordNet* yang diperoleh dengan melakukan *run* pada berkas "00 Analisis Sentimen Unsupervised.R"
5. "00 HuLiu.csv" : Data sentimen *SentiWordNet* yang diperoleh dengan melakukan *run* pada berkas "00 Analisis Sentimen Unsupervised.R"
6. "00 Jockers.csv" : Data sentimen *SentiWordNet* yang diperoleh dengan melakukan *run* pada berkas "00 Analisis Sentimen Unsupervised.R"
7. "00 SenticNet.csv" : Data sentimen *SentiWordNet* yang diperoleh dengan melakukan *run* pada berkas "00 Analisis Sentimen Unsupervised.R"
8. "00 MPQA Subjectivity.csv" : Data sentimen *SentiWordNet* yang diperoleh dengan melakukan *run* pada berkas "00 Analisis Sentimen Unsupervised.R"
9. "01 HistoricalPrices 10-22.csv" : Data indeks DXY yang diperoleh dari laman https://www.wsj.com/market-data/quotes/index/DXY
10. "02 Source Run.R" : Penggalan *coding* dari "02 ARIMAX 800 data per Lex.R" terkait dengan agregasi sentimen
11. "04 Data Agregasi Sentimen Harian.csv" : Data sentimen harian
12. "04 Data Koefisien Model Awal DR.csv" : Data koefisien model awal DR beserta dengan AIC model
13. "04 Data Koefisien Model Akhir DR.csv" : Data koefisien model akhir DR beserta dengan AIC dan spesifikasinya
14. "04 Hasil Uji Ljung-Box dan Shapiro-Wilk.csv" : Hasil uji Ljung-Box dan Shapiro-Wilk
15. "04 R-Squared Model Sliding Windows Periode Fitting satu tahun.csv" : Nilai R-Squared *fitting* Model periode *fitting* 1 tahun
16. "04 R-Squared Model Sliding Windows Periode Fitting satu setengah tahun.csv" : Nilai R-Squared *fitting* Model periode *fitting* 1,5 tahun
17. "04 R-Squared Model Sliding Windows Periode Fitting dua tahun.csv" : Nilai R-Squared *fitting* Model periode *fitting* 2 tahun
18. "04 MSE Model Sliding Windows Periode Fitting satu tahun.csv" : Nilai MSE Ramalan Model periode *fitting* 1 tahun
19. "04 MSE Model Sliding Windows Periode Fitting satu setengah tahun.csv" : Nilai MSE Ramalan Model periode *fitting* 1,5 tahun
20. "04 MSE Model Sliding Windows Periode Fitting dua tahun.csv" : Nilai MSE Ramalan Model periode *fitting* 2 tahun
21. "04 AIC Model Sliding Windows Periode Fitting satu tahun.csv" : Nilai AIC *fitting* Model periode *fitting* 1 tahun
22. "04 AIC Model Sliding Windows Periode Fitting satu setengah tahun.csv" : Nilai AIC *fitting* Model periode *fitting* 1,5 tahun
23. "04 AIC Model Sliding Windows Periode Fitting dua tahun.csv" : Nilai AIC *fitting* Model periode *fitting* 2 tahun
24. "04 Persentase Perbandingan MSE Periode Fiting satu tahun.csv" : Nilai Perbandingan MSE Ramalan Model periode *fitting* 1 tahun
25. "04 Persentase Perbandingan MSE Periode Fiting satu setengah tahun.csv" : Nilai Perbandingan MSE Ramalan Model periode *fitting* 1,5 tahun
26. "04 Persentase Perbandingan MSE Periode Fiting dua tahun.csv" : Nilai Perbandingan MSE Ramalan Model periode *fitting* 2 tahun

#### Pengerjaan skripsi dilakukan dengan melakukan *run* pada berkas *coding* berikut:
1. "00 Analisis Sentimen Unsupervised.R" : Analisis sentimen dengan lima leksikon
2. "01 ARIMA 800 data.R" : Analisis data Indeks dan pembentukan model ARIMA
3. "02 ARIMAX 800 data per Lex.R" : Agregasi sentimen dan pembentukan model DR
4. "03 Sliding Windows ARIMA vs ARIMAX.R" : Perbandingan performa *fitting* dan *forecasting* model ARIMA dan model DR
5. "04 Export Data.R" : *Export* data hasil pengolahan
