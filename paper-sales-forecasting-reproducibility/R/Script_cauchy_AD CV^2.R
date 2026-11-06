#Memasukkan Data ke R
#Data dari file excel dnegan nama file "Data penjualan kertas Fix.xlsx"
setwd("D:/Dosen/Riset/Luaran/Cauchy Paper sales")
getwd()
library(readxl)
data_penjualan <- read_excel(
  "Data penjualan kertas Fix.xlsx",
)
head(data_penjualan)
#ubah menjadi data time series
library(dplyr)
library(lubridate)

data_penjualan$Tanggal <- as.Date(data_penjualan$Tanggal)

data_penjualan <- data_penjualan %>%
  mutate(
    Tahun = year(Tanggal),
    Minggu = isoweek(Tanggal),
    YearWeek = paste0(Tahun,"-W",sprintf("%02d", Minggu))
  )
#penjualan per minggu
weekly_sales <- data_penjualan %>%
  group_by(YearWeek, `Jenis Produk`) %>%
  summarise(
    Demand = sum(Total, na.rm = TRUE),
    .groups = "drop"
  )
#ubah jadi format wide
library(tidyr)
weekly_wide <- weekly_sales %>%
  pivot_wider(
    names_from = `Jenis Produk`,
    values_from = Demand,
    values_fill = 0
  )
#screening
sales_data <- weekly_wide[, -1]

screening <- data.frame(
  SKU = names(sales_data),
  
  Total_Demand = sapply(sales_data, sum),
  
  NonZero_Periods = sapply(
    sales_data,
    function(x) sum(x > 0)
  ),
  
  Zero_Periods = sapply(
    sales_data,
    function(x) sum(x == 0)
  )
)

screening$Percent_Zero <-
  100 * screening$Zero_Periods /
  (screening$Zero_Periods + screening$NonZero_Periods)
screening$Percent_Zero
data_penjualan$`Jenis Produk` <- toupper(data_penjualan$`Jenis Produk`)

length(unique(data_penjualan$`Jenis Produk`))
produk <- sort(unique(data_penjualan$`Jenis Produk`))

produk
screening$NonZero_Periods >= 5
screening[
  screening$NonZero_Periods >= 5,
  c("SKU","NonZero_Periods")
]
screening
sku_layak <- screening$SKU[screening$NonZero_Periods >= 5]
weekly_layak <- weekly_wide[, c("YearWeek", sku_layak)]
weekly_layak
ncol(weekly_layak) - 1 #jumlah SKU tersisa
names(weekly_layak) #judul kolom pada data weekly_layak
#menyimpan data screening layak di excel
library(writexl)

write_xlsx(
  weekly_layak,
  "Data_Kertas_Lolos_Screening.xlsx"
)

# ==============================================================================
# KLASIFIKASI POLA PERMINTAAN ADI - CV^2 (TANPA LOOK-AHEAD LEAKAGE)
# Input  : Data_Kertas_Lolos_Screening.xlsx
# Output : Tabel Klasifikasi (Excel), Statistik Deskriptif, & Scatter Plot (PNG)
# ==============================================================================

# 1. Load Package yang Dibutuhkan
library(readxl)
library(dplyr)
library(ggplot2)
library(writexl)

# 2. Impor Data Hasil Screening
weekly_layak <- read_excel("Data_Kertas_Lolos_Screening.xlsx")

# 3. Tetapkan Data Latih (Initial Training Period: Minggu 1-50)
# Catatan: Perhitungan ADI & CV^2 hanya menggunakan data latih (50 observasi awal)
# agar tidak terjadikebocoran data (look-ahead leakage) dari data uji (minggu 51-63).
n_train <- 50
sales_train <- weekly_layak[1:n_train, -1] # Mengabaikan kolom YearWeek

# 4. Hitung Average Demand Interval (ADI) pada Data Latih
ADI <- sapply(sales_train, function(x) {
  x[is.na(x)] <- 0
  n_total <- length(x)
  n_nonzero <- sum(x > 0)
  
  if (n_nonzero == 0) return(NA)
  return(n_total / n_nonzero)
})

# 5. Hitung Squared Coefficient of Variation (CV^2) pada Data Latih
CV2 <- sapply(sales_train, function(x) {
  demand <- x[x > 0]
  
  # Minimal butuh 2 observasi permintaan positif untuk menghitung SD
  if (length(demand) < 2) {
    return(NA)
  }
  
  mean_d <- mean(demand)
  sd_d   <- sd(demand)
  
  return((sd_d / mean_d)^2)
})

# 6. Susun Data Frame Karakteristik SKU
karakteristik <- data.frame(
  SKU = names(sales_train),
  ADI = round(ADI, 3),
  CV2 = round(CV2, 3)
)

# 7. Klasifikasi Kategori Berdasarkan Ambang Syntetos & Boylan
# Cut-off Boundary: ADI = 1.32 dan CV^2 = 0.49
karakteristik$Category <- with(
  karakteristik,
  ifelse(
    ADI <= 1.32 & CV2 <= 0.49, "Smooth",
    ifelse(
      ADI <= 1.32 & CV2 > 0.49, "Erratic",
      ifelse(
        ADI > 1.32 & CV2 <= 0.49, "Intermittent",
        "Lumpy"
      )
    )
  )
)

# 8. Tampilkan Ringkasan Hasil Klasifikasi di Console
cat("\n=== FREKUENSI KATEGORI PERMINTAAN ===\n")
print(table(karakteristik$Category))

cat("\n=== HASIL KLASIFIKASI 10 SKU PERTAMA ===\n")
print(head(karakteristik[order(karakteristik$ADI), ], 10))

# 9. Hitung Statistik Deskriptif Tiap Kategori
stat_deskriptif <- karakteristik %>%
  group_by(Category) %>%
  summarise(
    Jumlah_SKU = n(),
    Mean_ADI   = round(mean(ADI, na.rm = TRUE), 2),
    Median_ADI = round(median(ADI, na.rm = TRUE), 2),
    Mean_CV2   = round(mean(CV2, na.rm = TRUE), 2),
    Median_CV2 = round(median(CV2, na.rm = TRUE), 2),
    .groups    = "drop"
  )

cat("\n=== STATISTIK DESKRIPTIF PER KATEGORI ===\n")
print(stat_deskriptif)

# 10. Ekspor Hasil Klasifikasi ke File Excel
write_xlsx(karakteristik, "Hasil_Klasifikasi_ADI_CV2_Train50.xlsx")
write_xlsx(stat_deskriptif, "Statistik_Deskriptif_Kategori_Train50.xlsx")

# 11. Visualisasi Scatter Plot ADI vs CV^2 dengan Threshold Line
plot_klasifikasi <- ggplot(karakteristik, aes(x = ADI, y = CV2, color = Category)) +
  geom_point(size = 2, alpha = 0.85) +
  geom_vline(xintercept = 1.32, linetype = "dashed", color = "gray30", linewidth = 0.5) +
  geom_hline(yintercept = 0.49, linetype = "dashed", color = "gray30", linewidth = 0.5) +
  annotate("text", x = 1.0, y = 0.1, label = "Smooth", fontface = "bold", color = "#2ecc71") +
  annotate("text", x = 1.0, y = max(karakteristik$CV2, na.rm=T)*0.9, label = "Erratic", fontface = "bold", color = "#e67e22") +
  annotate("text", x = max(karakteristik$ADI, na.rm=T)*0.8, y = 0.1, label = "Intermittent", fontface = "bold", color = "#3498db") +
  annotate("text", x = max(karakteristik$ADI, na.rm=T)*0.8, y = max(karakteristik$CV2, na.rm=T)*0.9, label = "Lumpy", fontface = "bold", color = "#e74c3c") +
  scale_color_manual(values = c(
    "Smooth"       = "#2ecc71",
    "Erratic"      = "#e67e22",
    "Intermittent" = "#3498db",
    "Lumpy"        = "#e74c3c"
  )) +
  labs(
    title = "ADI – CV² Demand Classification",
    subtitle = "Evaluated on Initial Training Period (Weeks 1–50)",
    x = "Average Demand Interval (ADI)",
    y = expression(CV^2),
    color = "Demand Category"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

# Tampilkan Plot
print(plot_klasifikasi)

# Simpan Gambar Grafik High-Resolution (300 DPI)
ggsave("Scatterplot_ADI_CV2.png", plot = plot_klasifikasi, width = 8, height = 6, dpi = 300)

# ==============================================================================
# KONVERSI TIME SERIES DAN VISUALISASI 50 OBSERVASI PERTAMA DUPLEKS310
# ==============================================================================

# 1. Load Package
library(readxl)
library(ggplot2)

# 2. Impor Data Hasil Screening
weekly_layak <- read_excel("Data_Kertas_Lolos_Screening.xlsx")

# 3. Ekstrak 50 Observasi Pertama SKU Dupleks310 (Initial Training Data)
n_train <- 50
dupleks310_train <- weekly_layak$Dupleks310[1:n_train]

# 4. Ubah Menjadi Objek Time Series (ts) di R
# Catatan: Objek 'ts' digunakan sebagai input untuk fungsi auto.arima, ets, crost, dll.
dupleks310_ts <- ts(dupleks310_train)

# Tampilkan ringkasan data time series
print(summary(dupleks310_ts))

# 5. Buat Data Frame Khusus Visualisasi
plot_df <- data.frame(
  Minggu = 1:n_train,
  Sales  = dupleks310_train
)

# 6. Plot Time Series 50 Observasi Pertama
plot_dupleks50 <- ggplot(plot_df, aes(x = Minggu, y = Sales)) +
  geom_line(color = "#2b5c8f", linewidth = 0.8) +
  geom_point(color = "#2b5c8f", size = 2) +
  scale_y_continuous(labels = scales::comma) + # Format angka Rupiah tanpa notasi ilmiah
  labs(
    title = "Weekly Sales of Dupleks310 (First 50 Observations)",
    subtitle = "Initial Training Period for Rolling-Origin Evaluation",
    x = "Week",
    y = "Sales (Rp)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray30"),
    panel.grid.minor = element_blank()
  )

# Tampilkan Plot
print(plot_dupleks50)

# Simpan Grafik ke File
ggsave("Plot_Dupleks310_50Weeks.png", plot = plot_dupleks50, width = 8, height = 4.5, dpi = 300)

# ==============================================================================
# SKRIP R AUTOMATIS: GENERASI TABEL EVALUASI ROLLING ORIGIN PER SKU & MODEL
# Input  : Data_Kertas_Lolos_Screening.xlsx
# Output : 1. Tabel_Evaluasi_Semua_Model.tex (Kode LaTeX Siap Copy-Paste)
#          2. Tabel_Evaluasi_Per_SKU_Model.xlsx (Workbook Excel Per SKU)
# ==============================================================================

# 1. Load Package
library(readxl)
library(forecast)
library(tsintermittent)
library(openxlsx)
library(dplyr)

# 2. Impor Data & Parameter Evaluasi
weekly_layak <- read_excel("Data_Kertas_Lolos_Screening.xlsx")
sku_list <- colnames(weekly_layak)[-1]

h <- 4                  # Horison 4 minggu
initial <- 50           # Sampel data latih awal 50 minggu
n_total <- nrow(weekly_layak)
n_origins <- n_total - initial - h + 1 # 10 Origin

# Penampung File LaTeX dan Workbook Excel
latex_output <- c("% ==========================================================",
                  "% KODE TABEL LATEX EVALUASI ROLLING ORIGIN PER SKU & MODEL",
                  "% ==========================================================\n")
wb <- createWorkbook()

table_counter <- 2 # Dimulai dari Table 2 sesuai dengan draf naskah

cat("Membuat tabel untuk 23 SKU x 4 Model...\n")

# 3. Loop Utama: Setiap SKU
for (sku in sku_list) {
  y <- weekly_layak[[sku]]
  
  # Matriks Penampung Per Model untuk SKU Terkait
  res_arima   <- data.frame(Origin = 1:n_origins, Model = character(n_origins), MAE = numeric(n_origins), MASE = numeric(n_origins))
  res_ets     <- data.frame(Origin = 1:n_origins, Model = character(n_origins), MAE = numeric(n_origins), MASE = numeric(n_origins))
  res_croston <- data.frame(Origin = 1:n_origins, Model = character(n_origins), MAE = numeric(n_origins), MASE = numeric(n_origins))
  res_sba     <- data.frame(Origin = 1:n_origins, Model = character(n_origins), MAE = numeric(n_origins), MASE = numeric(n_origins))
  
  # Loop 10 Origin
  for (origin in 1:n_origins) {
    train_end <- initial + origin - 1
    train <- y[1:train_end]
    test  <- y[(train_end + 1):(train_end + h)]
    
    scale_in_sample <- mean(abs(diff(train)))
    if (is.na(scale_in_sample) || scale_in_sample == 0) scale_in_sample <- 1e-6
    
    # ------------------ 1. ARIMA ------------------
    fit_arima <- tryCatch(auto.arima(train, stepwise = TRUE, approximation = FALSE), error = function(e) NULL)
    if (!is.null(fit_arima)) {
      fc_arima <- as.numeric(forecast(fit_arima, h = h)$mean)
      ord <- arimaorder(fit_arima)
      res_arima$Model[origin] <- paste0("ARIMA(", ord[1], ",", ord[2], ",", ord[3], ")")
    } else {
      fc_arima <- rep(mean(train), h)
      res_arima$Model[origin] <- "ARIMA(0,0,0)"
    }
    res_arima$MAE[origin]  <- mean(abs(test - fc_arima))
    res_arima$MASE[origin] <- res_arima$MAE[origin] / scale_in_sample
    
    # ------------------ 2. ETS ------------------
    fit_ets <- tryCatch(ets(train), error = function(e) NULL)
    if (!is.null(fit_ets)) {
      fc_ets <- as.numeric(forecast(fit_ets, h = h)$mean)
      res_ets$Model[origin] <- fit_ets$method
    } else {
      fc_ets <- rep(mean(train), h)
      res_ets$Model[origin] <- "ETS(A,N,N)"
    }
    res_ets$MAE[origin]  <- mean(abs(test - fc_ets))
    res_ets$MASE[origin] <- res_ets$MAE[origin] / scale_in_sample
    
    # ------------------ 3. Croston ------------------
    fit_croston <- tryCatch(crost(train, h = h, type = "croston", outplot = FALSE), error = function(e) NULL)
    if (!is.null(fit_croston) && !is.null(fit_croston$forecast)) {
      fc_croston <- as.numeric(fit_croston$forecast)
    } else {
      fc_croston <- rep(mean(train), h)
    }
    res_croston$Model[origin] <- "Croston"
    res_croston$MAE[origin]  <- mean(abs(test - fc_croston))
    res_croston$MASE[origin] <- res_croston$MAE[origin] / scale_in_sample
    
    # ------------------ 4. SBA ------------------
    fit_sba <- tryCatch(crost(train, h = h, type = "sba", outplot = FALSE), error = function(e) NULL)
    if (!is.null(fit_sba) && !is.null(fit_sba$forecast)) {
      fc_sba <- as.numeric(fit_sba$forecast)
    } else {
      fc_sba <- rep(mean(train), h)
    }
    res_sba$Model[origin] <- "SBA"
    res_sba$MAE[origin]  <- mean(abs(test - fc_sba))
    res_sba$MASE[origin] <- res_sba$MAE[origin] / scale_in_sample
  }
  
  # Daftar Model per SKU
  models_list <- list(
    "ARIMA"   = res_arima,
    "ETS"     = res_ets,
    "Croston" = res_croston,
    "SBA"     = res_sba
  )
  
  # Tambahkan Sheet Excel Baru per SKU
  sheet_name <- substr(sku, 1, 31)
  addWorksheet(wb, sheet_name)
  start_row <- 1
  
  # Loop Pemformatan Tabel per Model
  for (m_name in names(models_list)) {
    df_model <- models_list[[m_name]]
    
    # Format Nama Kolom Model (misal: "ARIMA Model", "ETS Model", dll)
    model_col_name <- paste(m_name, "Model")
    colnames(df_model) <- c("Origin", model_col_name, "MAE", "MASE")
    
    # --- A. FORMATTING KODE LATEX ---
    caption_title <- paste0("Table ", table_counter, ": ", m_name, " Model of Ten Origins from ", sku)
    
    latex_tab <- c(
      "\\begin{table}[htbp]",
      "\\centering",
      paste0("\\caption{", caption_title, "}"),
      "\\begin{tabular}{cccc}",
      "\\hline",
      paste0("\\textbf{Origin} & \\textbf{", model_col_name, "} & \\textbf{MAE} & \\textbf{MASE} \\\\"),
      "\\hline"
    )
    
    for (r in 1:nrow(df_model)) {
      # Format MAE (bulat/desimal) dan MASE (8 desimal seperti di naskah)
      mae_val  <- ifelse(df_model$MAE[r] %% 1 == 0, 
                         as.character(df_model$MAE[r]), 
                         sprintf("%.3f", df_model$MAE[r]))
      mase_val <- sprintf("%.8f", df_model$MASE[r])
      
      line_str <- paste0(df_model$Origin[r], " & ", 
                         df_model[[model_col_name]][r], " & ", 
                         mae_val, " & ", 
                         mase_val, " \\\\")
      latex_tab <- c(latex_tab, line_str)
    }
    
    latex_tab <- c(latex_tab, "\\hline", "\\end{tabular}", "\\end{table}\n")
    latex_output <- c(latex_output, latex_tab)
    
    # --- B. WRITE KE EXCEL SHEET ---
    writeData(wb, sheet = sheet_name, x = caption_title, startRow = start_row, startCol = 1)
    writeData(wb, sheet = sheet_name, x = df_model, startRow = start_row + 1, startCol = 1)
    
    start_row <- start_row + nrow(df_model) + 4 # Jarak antar tabel model di Excel
    table_counter <- table_counter + 1
  }
}

# 4. Simpan Output File
writeLines(latex_output, "Tabel_Evaluasi_Semua_Model.tex")
saveWorkbook(wb, "Tabel_Evaluasi_Per_SKU_Model.xlsx", overwrite = TRUE)

# ==============================================================================
# SKRIP R: EVALUASI PERAMALAN ROLLING ORIGIN (TABEL 1, TABEL 2, & KONTINGENSI)
# Input  : Data_Kertas_Lolos_Screening.xlsx
# Output : 1. Tabel 1 (Mean MAE) tercetak di Console
#          2. Tabel 2 (Mean MASE) tercetak di Console
#          3. Tabel Kontingensi MAE & MASE + Uji Fisher di Console
#          4. File Excel: Hasil_Evaluasi_Peramalan_SKU.xlsx
# ==============================================================================

# 1. Load Package
library(readxl)
library(forecast)
library(tsintermittent)
library(dplyr)
library(tidyr)
library(writexl)

# 2. Impor Data & Parameter Evaluasi
weekly_layak <- read_excel("Data_Kertas_Lolos_Screening.xlsx")
sku_list     <- colnames(weekly_layak)[-1]

h         <- 4   # Horison 4 minggu
initial   <- 50  # Data latih awal 50 minggu
n_origins <- 10  # 10 Rolling Origins

# ------------------------------------------------------------------------------
# TAHAP 1: KLASIFIKASI ADI - CV^2 (DATA LATIH 50 MINGGU AWAL)
# ------------------------------------------------------------------------------
sales_train_50 <- weekly_layak[1:initial, -1]

karakteristik <- data.frame(
  SKU = names(sales_train_50),
  ADI = sapply(sales_train_50, function(x) {
    x[is.na(x)] <- 0
    n_nonzero <- sum(x > 0)
    if (n_nonzero == 0) return(NA)
    return(length(x) / n_nonzero)
  }),
  CV2 = sapply(sales_train_50, function(x) {
    d <- x[x > 0]
    if (length(d) < 2) return(NA)
    return((sd(d) / mean(d))^2)
  })
)

# Ambang batas Syntetos & Boylan (ADI = 1.32, CV^2 = 0.49)
karakteristik <- karakteristik %>%
  mutate(
    kategori = case_when(
      ADI <= 1.32 & CV2 <= 0.49 ~ "Smooth",
      ADI <= 1.32 & CV2 >  0.49 ~ "Erratic",
      ADI >  1.32 & CV2 <= 0.49 ~ "Intermittent",
      ADI >  1.32 & CV2 >  0.49 ~ "Lumpy",
      TRUE                      ~ "Unknown"
    )
  )

# ------------------------------------------------------------------------------
# TAHAP 2: SIMULASI ROLLING ORIGIN (10 ORIGIN x SKU x 4 MODEL)
# ------------------------------------------------------------------------------
raw_results <- list()
idx <- 1

cat("Menjalankan Simulasi 10 Rolling Origins...\n")

for (sku in sku_list) {
  y <- weekly_layak[[sku]]
  
  for (origin in 1:n_origins) {
    train_end <- initial + origin - 1
    train     <- y[1:train_end]
    test      <- y[(train_end + 1):(train_end + h)]
    
    # In-Sample Baseline Scale (Diff Naïve)
    scale_in_sample <- mean(abs(diff(train)))
    if (is.na(scale_in_sample) || scale_in_sample == 0) scale_in_sample <- 1e-6
    
    # --- Model 1: ARIMA ---
    fit_arima <- tryCatch(auto.arima(train, stepwise = TRUE, approximation = FALSE), error = function(e) NULL)
    fc_arima  <- if (!is.null(fit_arima)) as.numeric(forecast(fit_arima, h = h)$mean) else rep(mean(train), h)
    mae_arima <- mean(abs(test - fc_arima))
    raw_results[[idx]] <- data.frame(SKU = sku, Origin = origin, Model_Family = "ARIMA", MAE = mae_arima, MASE = mae_arima / scale_in_sample)
    idx <- idx + 1
    
    # --- Model 2: ETS ---
    fit_ets <- tryCatch(ets(train), error = function(e) NULL)
    fc_ets  <- if (!is.null(fit_ets)) as.numeric(forecast(fit_ets, h = h)$mean) else rep(mean(train), h)
    mae_ets <- mean(abs(test - fc_ets))
    raw_results[[idx]] <- data.frame(SKU = sku, Origin = origin, Model_Family = "ETS", MAE = mae_ets, MASE = mae_ets / scale_in_sample)
    idx <- idx + 1
    
    # --- Model 3: Croston ---
    fit_croston <- tryCatch(crost(train, h = h, type = "croston", outplot = FALSE), error = function(e) NULL)
    fc_croston  <- if (!is.null(fit_croston) && !is.null(fit_croston$forecast)) as.numeric(fit_croston$forecast) else rep(mean(train), h)
    mae_croston <- mean(abs(test - fc_croston))
    raw_results[[idx]] <- data.frame(SKU = sku, Origin = origin, Model_Family = "Croston", MAE = mae_croston, MASE = mae_croston / scale_in_sample)
    idx <- idx + 1
    
    # --- Model 4: SBA ---
    fit_sba <- tryCatch(crost(train, h = h, type = "sba", outplot = FALSE), error = function(e) NULL)
    fc_sba  <- if (!is.null(fit_sba) && !is.null(fit_sba$forecast)) as.numeric(fit_sba$forecast) else rep(mean(train), h)
    mae_sba <- mean(abs(test - fc_sba))
    raw_results[[idx]] <- data.frame(SKU = sku, Origin = origin, Model_Family = "SBA", MAE = mae_sba, MASE = mae_sba / scale_in_sample)
    idx <- idx + 1
  }
}

df_all <- bind_rows(raw_results)

# ------------------------------------------------------------------------------
# TAHAP 3: PEMBENTUKAN TABEL 1 (BERDASARKAN MEAN MAE)
# ------------------------------------------------------------------------------
summary_mae_wide <- df_all %>%
  group_by(SKU, Model_Family) %>%
  summarise(Mean_MAE = round(mean(MAE), 3), .groups = "drop") %>%
  pivot_wider(names_from = Model_Family, values_from = Mean_MAE) %>%
  rename(
    `mean MAE ARIMA`   = ARIMA,
    `mean MAE ETS`     = ETS,
    `mean MAE Croston` = Croston,
    `mean MAE SBA`     = SBA
  )

winning_mae <- df_all %>%
  group_by(SKU, Model_Family) %>%
  summarise(Mean_MAE = mean(MAE), .groups = "drop") %>%
  group_by(SKU) %>%
  slice_min(order_by = Mean_MAE, n = 1, with_ties = FALSE) %>%
  select(SKU, `model terbaik` = Model_Family)

tabel_1_mae <- karakteristik %>%
  select(SKU, kategori) %>%
  left_join(summary_mae_wide, by = "SKU") %>%
  left_join(winning_mae, by = "SKU") %>%
  select(SKU, kategori, `mean MAE ARIMA`, `mean MAE ETS`, `mean MAE Croston`, `mean MAE SBA`, `model terbaik`)

# ------------------------------------------------------------------------------
# TAHAP 4: PEMBENTUKAN TABEL 2 (BERDASARKAN MEAN MASE)
# ------------------------------------------------------------------------------
summary_mase_wide <- df_all %>%
  group_by(SKU, Model_Family) %>%
  summarise(Mean_MASE = round(mean(MASE), 6), .groups = "drop") %>%
  pivot_wider(names_from = Model_Family, values_from = Mean_MASE) %>%
  rename(
    `mean MASE ARIMA`   = ARIMA,
    `mean MASE ETS`     = ETS,
    `mean MASE Croston` = Croston,
    `mean MASE SBA`     = SBA
  )

winning_mase <- df_all %>%
  group_by(SKU, Model_Family) %>%
  summarise(Mean_MASE = mean(MASE), .groups = "drop") %>%
  group_by(SKU) %>%
  slice_min(order_by = Mean_MASE, n = 1, with_ties = FALSE) %>%
  select(SKU, `model terbaik` = Model_Family)

tabel_2_mase <- karakteristik %>%
  select(SKU, kategori) %>%
  left_join(summary_mase_wide, by = "SKU") %>%
  left_join(winning_mase, by = "SKU") %>%
  select(SKU, kategori, `mean MASE ARIMA`, `mean MASE ETS`, `mean MASE Croston`, `mean MASE SBA`, `model terbaik`)

# ------------------------------------------------------------------------------
# TAHAP 5: FUNGSI TABEL KONTINGENSI DAN UJI FISHER
# ------------------------------------------------------------------------------
buat_analisis_kontingensi <- function(tabel_input, nama_metrik) {
  cat(paste0("\n================ TABEL KONTINGENSI (BERDASARKAN ", nama_metrik, ") ================\n"))
  
  vec_model <- tabel_input[["model terbaik"]]
  vec_kat   <- tabel_input[["kategori"]]
  
  raw_tab <- table(
    Kategori_ADI_CV2 = factor(vec_kat, levels = c("Smooth", "Erratic", "Intermittent", "Lumpy")),
    Model_Terbaik    = factor(vec_model, levels = c("ARIMA", "ETS", "Croston", "SBA"))
  )
  
  df_tab <- as.data.frame.matrix(raw_tab)
  df_tab$Kategori_ADI_CV2 <- rownames(df_tab)
  df_tab$Total_SKU <- rowSums(raw_tab)
  df_tab <- df_tab[, c("Kategori_ADI_CV2", "ARIMA", "ETS", "Croston", "SBA", "Total_SKU")]
  rownames(df_tab) <- NULL
  
  print(df_tab)
  
  fisher_res <- fisher.test(raw_tab, simulate.p.value = TRUE, B = 10000)
  cat("\n--- Uji Fisher's Exact (Asosiasi Kategori vs Model Terbaik) ---\n")
  cat("p-value:", fisher_res$p.value, "\n")
  if (fisher_res$p.value < 0.05) {
    cat("Kesimpulan: Terdapat hubungan signifikan antara Kategori ADI-CV^2 dan Model Terbaik.\n")
  } else {
    cat("Kesimpulan: Pemilihan Model Terbaik INDEPENDEN dari Kategori ADI-CV^2 (p >= 0.05).\n")
  }
  
  return(df_tab)
}

# ------------------------------------------------------------------------------
# TAHAP 6: EKSEKUSI CETAK CONSOLE & EXPORT FILE EXCEL
# ------------------------------------------------------------------------------

# 1. Cetak Tabel 1 ke Console
cat("\n================ TABEL 1: EVALUASI BERDASARKAN MEAN MAE ================\n")
print(as.data.frame(tabel_1_mae))

# 2. Cetak Tabel 2 ke Console
cat("\n================ TABEL 2: EVALUASI BERDASARKAN MEAN MASE ================\n")
print(as.data.frame(tabel_2_mase))

# 3. Eksekusi Fungsi & Cetak Tabel Kontingensi ke Console
tabel_kontingensi_mae  <- buat_analisis_kontingensi(tabel_1_mae, "MAE")
tabel_kontingensi_mase <- buat_analisis_kontingensi(tabel_2_mase, "MASE")

# 4. Ekspor Seluruh Tabel ke File Excel
list_of_sheets <- list(
  "Tabel_1_MAE"        = tabel_1_mae,
  "Tabel_2_MASE"       = tabel_2_mase,
  "Kontingensi_MAE"    = tabel_kontingensi_mae,
  "Kontingensi_MASE"   = tabel_kontingensi_mase
)

write_xlsx(list_of_sheets, "Hasil_Evaluasi_Peramalan_SKU.xlsx")

# ==============================================================================
# UJI FRIEDMAN & POST-HOC NEMENYI (FORMAT LONG DATA)
# # ==============================================================================

# 1. Load Package PMCMRplus
if (!require(PMCMRplus)) install.packages("PMCMRplus")
library(PMCMRplus)

# 2. Fungsi Uji Friedman & Post-Hoc Nemenyi (Format Long Data)
jalankan_friedman_dan_nemenyi <- function(tabel_input, nama_metrik) {
  cat(paste0("\n================ UJI FRIEDMAN & POST-HOC NEMENYI (BERDASARKAN ", nama_metrik, ") ================\n"))
  
  # A. Transformasi Data ke Format Long (Format Wajib untuk frdAllPairsNemenyiTest)
  prefix_kolom <- paste0("mean ", nama_metrik, " ")
  
  df_long <- tabel_input %>%
    select(SKU, starts_with(paste0("mean ", nama_metrik))) %>%
    pivot_longer(
      cols = starts_with(paste0("mean ", nama_metrik)),
      names_to = "Model",
      values_to = "Nilai_Error"
    ) %>%
    mutate(
      Model = gsub(prefix_kolom, "", Model),
      Model = factor(Model, levels = c("ARIMA", "ETS", "Croston", "SBA")),
      SKU   = factor(SKU)
    )
  
  # B. Hitung Mean Rank Tiap Model (Peringkat 1 = Error Terkecil / Terbaik)
  mat_data <- tabel_input %>%
    select(starts_with(paste0("mean ", nama_metrik))) %>%
    as.matrix()
  colnames(mat_data) <- c("ARIMA", "ETS", "Croston", "SBA")
  
  ranks <- t(apply(mat_data, 1, rank))
  mean_ranks <- round(colMeans(ranks), 4)
  
  cat("--- Mean Rank Model (Peringkat 1 = Terbaik) ---\n")
  print(mean_ranks)
  
  # C. Uji Friedman
  friedman_res <- friedman.test(mat_data)
  cat("\n--- Hasil Uji Friedman ---\n")
  cat("Chi-squared:", round(as.numeric(friedman_res$statistic), 4), 
      "| df:", as.numeric(friedman_res$parameter), 
      "| p-value:", round(friedman_res$p.value, 5), "\n")
  
  # D. Uji Post-Hoc Nemenyi (Sintaks Formula: Nilai_Error ~ Model | SKU)
  cat("\n--- Hasil Uji Post-Hoc Nemenyi (Pairwise Comparisons p-values) ---\n")
  nemenyi_res <- frdAllPairsNemenyiTest(Nilai_Error ~ Model | SKU, data = df_long)
  p_mat <- nemenyi_res$p.value
  print(round(p_mat, 4))
  
  # E. Kesimpulan Otomatis
  if (friedman_res$p.value < 0.05) {
    cat("\nKesimpulan: Perbedaan performa antar-model SIGNIFIKAN secara statistik (p < 0.05).\n")
  } else {
    cat("\nKesimpulan: Perbedaan performa antar-model TIDAK SIGNIFIKAN secara statistik (p >= 0.05).\n")
  }
  
  # F. Penyusunan Ringkasan Data untuk Ekspor Excel
  res_summary <- data.frame(
    Metrik            = nama_metrik,
    Mean_Rank_ARIMA   = mean_ranks["ARIMA"],
    Mean_Rank_ETS     = mean_ranks["ETS"],
    Mean_Rank_Croston = mean_ranks["Croston"],
    Mean_Rank_SBA     = mean_ranks["SBA"],
    Friedman_ChiSq    = round(as.numeric(friedman_res$statistic), 4),
    Friedman_df       = as.numeric(friedman_res$parameter),
    Friedman_p_value  = round(friedman_res$p.value, 5)
  )
  
  # Konversi Matriks Nemenyi ke Data Frame Bersih
  df_nemenyi <- as.data.frame(matrix(round(as.numeric(p_mat), 4), 
                                     nrow = nrow(p_mat), 
                                     ncol = ncol(p_mat)))
  colnames(df_nemenyi) <- colnames(p_mat)
  df_nemenyi$Model <- rownames(p_mat)
  df_nemenyi <- df_nemenyi[, c("Model", colnames(p_mat))]
  
  return(list(summary = res_summary, nemenyi = df_nemenyi))
}

# 3. Eksekusi Uji Friedman & Nemenyi (MAE dan MASE)
stat_mae  <- jalankan_friedman_dan_nemenyi(tabel_1_mae, "MAE")
stat_mase <- jalankan_friedman_dan_nemenyi(tabel_2_mase, "MASE")

# 4. Ekspor Seluruh Hasil Lengkap ke Excel Utama
list_of_sheets_lengkap <- list(
  "Tabel_1_MAE"          = tabel_1_mae,
  "Tabel_2_MASE"         = tabel_2_mase,
  "Kontingensi_MAE"      = tabel_kontingensi_mae,
  "Kontingensi_MASE"     = tabel_kontingensi_mase,
  "Ringkasan_Friedman"   = bind_rows(stat_mae$summary, stat_mase$summary),
  "Nemenyi_pVal_MAE"     = stat_mae$nemenyi,
  "Nemenyi_pVal_MASE"    = stat_mase$nemenyi
)

write_xlsx(list_of_sheets_lengkap, "Hasil_Evaluasi_Peramalan_SKU_Lengkap.xlsx")

# ==============================================================================
# SKRIP R: TABEL PERANKINGAN MODEL PER SKU (MAE & MASE) & RATA-RATA RANKING
# Input  : tabel_1_mae & tabel_2_mase (dari skrip sebelumnya)
# Output : 1. Tabel Perankingan Individual (23 SKU x 4 Model) MAE & MASE
#          2. Tabel Rata-Rata Ranking (Mean Rank) Keseluruhan & Per Kategori
#          3. File Excel: Tabel_Perankingan_Per_SKU_Lengkap.xlsx
# ==============================================================================

library(dplyr)
library(writexl)

# ------------------------------------------------------------------------------
# 1. FUNGSI MEMBUAT TABEL RANKING PER SKU & MENGHITUNG MEAN RANK
# ------------------------------------------------------------------------------
buat_tabel_perankingan_sku <- function(tabel_input, nama_metrik) {
  cols_model   <- c("ARIMA", "ETS", "Croston", "SBA")
  prefix_kolom <- paste0("mean ", nama_metrik, " ")
  cols_target  <- paste0(prefix_kolom, cols_model)
  
  # A. Ambil Matriks Error (23 SKU x 4 Model)
  mat_error <- tabel_input %>%
    select(all_of(cols_target)) %>%
    as.matrix()
  
  # B. Hitung Peringkat Tiap SKU (Baris demi Baris)
  # Peringkat 1 = Error Terkecil (Performa Terbaik), Peringkat 4 = Error Terbesar
  mat_rank <- t(apply(mat_error, 1, rank))
  colnames(mat_rank) <- paste0("Rank_", cols_model)
  
  # C. Susun Tabel Perankingan Individual per SKU
  tabel_rank_per_sku <- cbind(
    tabel_input %>% select(SKU, kategori),
    as.data.frame(mat_rank)
  )
  
  # D. Hitung Rata-Rata Ranking (Mean Rank) dari Tabel Peringkat di Atas
  mean_rank_overall <- colMeans(mat_rank)
  
  df_mean_overall <- data.frame(
    Metrik   = nama_metrik,
    Kategori = "Overall (23 SKU)",
    ARIMA    = round(mean_rank_overall["Rank_ARIMA"], 4),
    ETS      = round(mean_rank_overall["Rank_ETS"], 4),
    Croston  = round(mean_rank_overall["Rank_Croston"], 4),
    SBA      = round(mean_rank_overall["Rank_SBA"], 4)
  )
  
  # E. Hitung Rata-Rata Ranking per Kategori ADI-CV^2
  df_mean_by_kat <- tabel_rank_per_sku %>%
    group_by(kategori) %>%
    summarise(
      ARIMA   = round(mean(Rank_ARIMA), 4),
      ETS     = round(mean(Rank_ETS), 4),
      Croston = round(mean(Rank_Croston), 4),
      SBA     = round(mean(Rank_SBA), 4),
      .groups = "drop"
    ) %>%
    rename(Kategori = kategori) %>%
    mutate(Metrik = nama_metrik, .before = Kategori)
  
  tabel_ringkasan_mean_rank <- bind_rows(df_mean_overall, df_mean_by_kat)
  
  return(list(
    detail_per_sku = tabel_rank_per_sku,
    ringkasan_mean_rank = tabel_ringkasan_mean_rank
  ))
}

# ------------------------------------------------------------------------------
# 2. EKSEKUSI UNTUK MAE DAN MASE
# ------------------------------------------------------------------------------
hasil_rank_mae  <- buat_tabel_perankingan_sku(tabel_1_mae, "MAE")
hasil_rank_mase <- buat_tabel_perankingan_sku(tabel_2_mase, "MASE")

# ------------------------------------------------------------------------------
# 3. TAMPILKAN HASIL CETAK DI CONSOLE RSTUDIO
# ------------------------------------------------------------------------------
cat("\n================ 1. TABEL PERANKINGAN INDIVIDUAL PER SKU (MAE) ================\n")
print(as.data.frame(hasil_rank_mae$detail_per_sku))

cat("\n--- Rata-Rata Ranking (Mean Rank) Hasil Rata-Rata Kolom MAE di Atas ---\n")
print(as.data.frame(hasil_rank_mae$ringkasan_mean_rank))

cat("\n================ 2. TABEL PERANKINGAN INDIVIDUAL PER SKU (MASE) ================\n")
print(as.data.frame(hasil_rank_mase$detail_per_sku))

cat("\n--- Rata-Rata Ranking (Mean Rank) Hasil Rata-Rata Kolom MASE di Atas ---\n")
print(as.data.frame(hasil_rank_mase$ringkasan_mean_rank))

# ------------------------------------------------------------------------------
# 4. EKSPOR KE EXCEL UNTUK VERIFIKASI / AUDIT MANUAL
# ------------------------------------------------------------------------------
list_of_sheets_per_sku <- list(
  "Rank_Per_SKU_MAE"      = hasil_rank_mae$detail_per_sku,
  "Mean_Rank_MAE_Summary" = hasil_rank_mae$ringkasan_mean_rank,
  "Rank_Per_SKU_MASE"     = hasil_rank_mase$detail_per_sku,
  "Mean_Rank_MASE_Summary"= hasil_rank_mase$ringkasan_mean_rank
)

write_xlsx(list_of_sheets_per_sku, "Tabel_Perankingan_Per_SKU_Lengkap.xlsx")

# ==============================================================================
# SKRIP R GABUNGAN: TABEL KONTINGENSI TERBOBOT (FRACTIONAL) & UJI FISHER MONTE CARLO
# Input  : tabel_1_mae & tabel_2_mase (dari skrip sebelumnya)
# Output : 1. Tabel Kontingensi Terbobot (4 Kategori x 4 Model) MAE & MASE
#          2. Uji Fisher's Exact Test via Monte Carlo (N = 23 SKU Asli)
#          3. File Excel: Tabel_Kontingensi_Terbobot_Fisher.xlsx
# ==============================================================================

library(dplyr)
library(tidyr)
library(writexl)

# ------------------------------------------------------------------------------
# 1. FUNGSI UTAMA: PEMBOBOTAN PROPORSIONAL & UJI FISHER'S EXACT MONTE CARLO
# ------------------------------------------------------------------------------
jalankan_analisis_kontingensi_fisher <- function(tabel_input, nama_metrik) {
  cols_model   <- c("ARIMA", "ETS", "Croston", "SBA")
  prefix_kolom <- paste0("mean ", nama_metrik, " ")
  cols_target  <- paste0(prefix_kolom, cols_model)
  
  # A. Ekstrak Data Error per Model
  df_error <- tabel_input %>% 
    select(SKU, kategori, all_of(cols_target))
  colnames(df_error) <- c("SKU", "kategori", cols_model)
  
  # B. Hitung Bobot Proporsional (1/m) per SKU untuk Menangani Ties
  df_weights <- df_error %>%
    rowwise() %>%
    mutate(
      min_val   = min(c_across(all_of(cols_model))),
      m_ties    = sum(c_across(all_of(cols_model)) == min_val), # Jumlah model pemenang seri
      w_ARIMA   = ifelse(ARIMA == min_val, 1 / m_ties, 0),
      w_ETS     = ifelse(ETS == min_val, 1 / m_ties, 0),
      w_Croston = ifelse(Croston == min_val, 1 / m_ties, 0),
      w_SBA     = ifelse(SBA == min_val, 1 / m_ties, 0)
    ) %>%
    ungroup()
  
  # C. Agregasi Matriks Kontingensi Terbobot per Kategori ADI-CV^2
  tabel_kontingensi <- df_weights %>%
    group_by(kategori) %>%
    summarise(
      ARIMA   = round(sum(w_ARIMA), 4),
      ETS     = round(sum(w_ETS), 4),
      Croston = round(sum(w_Croston), 4),
      SBA     = round(sum(w_SBA), 4),
      .groups = "drop"
    )
  
  # D. Lengkapi Seluruh 4 Kategori Taksonomi (Smooth, Erratic, Intermittent, Lumpy)
  semua_kategori    <- data.frame(kategori = c("Intermittent", "Lumpy", "Erratic", "Smooth"))
  tabel_4x4_lengkap <- semua_kategori %>%
    left_join(tabel_kontingensi, by = "kategori") %>%
    mutate(across(all_of(cols_model), ~ replace_na(., 0)))
  
  # E. Tambahkan Kolom Total Frekuensi SKU per Kategori
  tabel_4x4_dengan_total <- tabel_4x4_lengkap %>%
    rowwise() %>%
    mutate(Total_SKU = sum(c_across(all_of(cols_model)))) %>%
    ungroup()
  
  # F. Persiapan Matriks untuk Uji Fisher's Exact
  mat_raw <- as.matrix(tabel_4x4_lengkap[, cols_model])
  rownames(mat_raw) <- tabel_4x4_lengkap$kategori
  
  # Clean: Hapus baris dan kolom yang bernilai 0 total (seperti kategori Smooth)
  mat_bersih <- mat_raw[rowSums(mat_raw) > 0, colSums(mat_raw) > 0]
  
  # Pembulatan ke integer terdekat pada skala N = 23 SKU murni (tanpa pembesaran buatan)
  mat_integer <- round(mat_bersih)
  
  # G. Eksekusi Uji Fisher's Exact via Monte Carlo Simulation (B = 100.000)
  set.seed(42)
  ujifisher <- fisher.test(mat_integer, simulate.p.value = TRUE, B = 100000)
  
  return(list(
    tabel_fraksional = tabel_4x4_dengan_total,
    matriks_fisher   = mat_integer,
    hasil_fisher     = ujifisher
  ))
}

# ------------------------------------------------------------------------------
# 2. EKSEKUSI ANALISIS UNTUK MAE DAN MASE
# ------------------------------------------------------------------------------
hasil_kontingensi_mae  <- jalankan_analisis_kontingensi_fisher(tabel_1_mae, "MAE")
hasil_kontingensi_mase <- jalankan_analisis_kontingensi_fisher(tabel_2_mase, "MASE")

# ------------------------------------------------------------------------------
# 3. CETAK HASIL KE CONSOLE RSTUDIO
# ------------------------------------------------------------------------------
cat("\n================ 1. TABEL KONTINGENSI TERBOBOT (BERDASARKAN MAE) ================\n")
print(as.data.frame(hasil_kontingensi_mae$tabel_fraksional))

cat("\n--- HASIL UJI FISHER'S EXACT MONTE CARLO (MAE - N = 23 SKU) ---\n")
print(hasil_kontingensi_mae$hasil_fisher)

cat("\n================ 2. TABEL KONTINGENSI TERBOBOT (BERDASARKAN MASE) ================\n")
print(as.data.frame(hasil_kontingensi_mase$tabel_fraksional))

cat("\n--- HASIL UJI FISHER'S EXACT MONTE CARLO (MASE - N = 23 SKU) ---\n")
print(hasil_kontingensi_mase$hasil_fisher)

# ------------------------------------------------------------------------------
# 4. EKSPOR HASIL TABEL KE FILE EXCEL
# ------------------------------------------------------------------------------
list_of_sheets_kontingensi <- list(
  "Kontingensi_MAE"  = hasil_kontingensi_mae$tabel_fraksional,
  "Kontingensi_MASE" = hasil_kontingensi_mase$tabel_fraksional
)

write_xlsx(list_of_sheets_kontingensi, "Tabel_Kontingensi_Terbobot_Fisher.xlsx")


