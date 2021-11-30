library(httr)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(lubridate)
# Memperoleh data dari API
resp_jabar <- GET("https://data.covid19.go.id/public/api/prov_detail_JAWA_BARAT.json")
cov_jabar_raw <- content(resp_jabar, as = "parsed", simplifyVector = TRUE)
names(cov_jabar_raw)
str(cov_jabar_raw)

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
cov_jabar_akumulasi <- 
  new_cov_jabar %>% 
  transmute(
    tanggal,
    akumulasi_aktif = cumsum(kasus_baru) - cumsum(sembuh) - cumsum(meninggal),
    akumulasi_sembuh = cumsum(sembuh),
    akumulasi_meninggal = cumsum(meninggal)
  )

tail(cov_jabar_akumulasi)