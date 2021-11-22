library(httr)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(lubridate)
library(tidyr)
library(plotly)
# Memperoleh data dari API
resp_jabar <- GET("https://data.covid19.go.id/public/api/prov_detail_JAWA_BARAT.json")
cov_jabar_raw <- content(resp_jabar, as = "parsed", simplifyVector = TRUE)
names(cov_jabar_raw)

# Karena List Perkembangan merupakan array lagi jadi kita masukan ke variable baru 
cov_jabar <- cov_jabar_raw$list_perkembangan

# Menjinakan Data
new_cov_jabar <-
  cov_jabar %>%
  #Menghapus Kolom Dirawat Or Isolasi
  select(-contains("DIRAWAT OR ISOLASI")) %>%
  # #Menghapus Kolom AKUMULASI Dirawat Or Isolasi
  select(-starts_with("AKUMULASI")) %>%
  # Mengganti nama kolom menjadi huruf kecil
  rename(
    kasus_baru = KASUS,
    meninggal = MENINGGAL,
    sembuh = SEMBUH
  ) %>%
  # Merubah Format Tanggal soalnya format nya ngaco
  mutate(
    tanggal = as.POSIXct(tanggal / 1000, origin = "1970-01-01"),
    tanggal = as.Date(tanggal)
  )
# Format sebelum convert
str(cov_jabar$tanggal)
# Format Setelah COnvert
str(new_cov_jabar$tanggal)
str(new_cov_jabar)

# Akumulasi data
cov_jabar_akumulasi <- 
  new_cov_jabar %>% 
  transmute(
    tanggal,
    akumulasi_aktif = cumsum(kasus_baru) - cumsum(sembuh) - cumsum(meninggal),
    akumulasi_sembuh = cumsum(sembuh),
    akumulasi_meninggal = cumsum(meninggal)
  )

# Disini kita akan menampilkan 3 data(kasus, sembuh dan meninggal) dalam 1 graph
# Langkah pertama membuat pivot dulu dari data akumulasi menggunakan library(tidyr)
dim(cov_jabar_akumulasi)
tail(cov_jabar_akumulasi$akumulasi_sembuh)

cov_jabar_akumulasi_pivot <- 
  cov_jabar_akumulasi %>% 
  gather(
    key = "kategori",
    value = "jumlah",
    -tanggal
  ) %>% 
  mutate(
    kategori = sub(pattern = "akumulasi_", replacement = "", kategori)
  )

dim(cov_jabar_akumulasi_pivot)

glimpse(cov_jabar_akumulasi_pivot)

# Convert Ke dalam Bentuk Grafik
plot <- ggplot(cov_jabar_akumulasi_pivot, aes(tanggal, jumlah, colour = (kategori))) +
  geom_point(size = 0.9) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL), limits = c(0, 400000), labels = scales::number) +
  scale_color_manual(
    values = c(
      "aktif" = "salmon",
      "meninggal" = "darkslategray4",
      "sembuh" = "olivedrab2"
    ),
    labels = c("Aktif", "Meninggal", "Sembuh")
  ) +
  labs(
    x = NULL,
    y = "Jumlah kasus akumulasi",
    colour = NULL,
    title = "Dinamika Kasus COVID-19 di Jawa Barat",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )
plot