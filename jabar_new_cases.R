library(httr)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
# Memperoleh data dari API
resp_jabar <- GET("https://data.covid19.go.id/public/api/prov_detail_JAWA_BARAT.json")
cov_jabar_raw <- content(resp_jabar, as = "parsed", simplifyVector = TRUE)
names(cov_jabar_raw)


# Karena List Perkembangan merupakan array lagi jadi kita masukan ke variable baru
cov_jabar <- cov_jabar_raw$list_perkembangan

# Menjinakan Data
new_cov_jabar <-
  cov_jabar %>%
  # Menghapus Kolom Dirawat Or Isolasi
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
ggplot(new_cov_jabar, aes(tanggal, kasus_baru)) +
  geom_col(fill = "salmon") +
  labs(
    x = NULL,
    y = "Jumlah Kasus",
    title = "Kasus Harian Positif Covid-19 di Jawa Barat",
    subtitle = "Terjadi pelonjakan kasus di awal bulan Juni akibat klaster Secapa AD Bandung",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")