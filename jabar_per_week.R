library(httr)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(lubridate)
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

# Membuat data menjadi format per week dimulai dari tanggal 02-03-2020
cov_jabar_pekanan <- new_cov_jabar %>% 
  count(
    tahun = year(tanggal),
    pekan_ke = week(tanggal),
    wt = kasus_baru,
    name = "jumlah"
  )

glimpse(cov_jabar_pekanan)

# Membuat format hasil komparasi ketika jumlah pekan sekarang lebih rendah dari pada pekan lalu maka lebih_baik bernilai true
cov_jabar_pekanan <-
  cov_jabar_pekanan %>% 
  mutate(
    jumlah_pekanlalu = dplyr::lag(jumlah, 1),
    jumlah_pekanlalu = ifelse(is.na(jumlah_pekanlalu), 0, jumlah_pekanlalu),
    lebih_baik = jumlah < jumlah_pekanlalu
  )
glimpse(cov_jabar_pekanan)

# Membuat plot
ggplot(cov_jabar_pekanan, aes(pekan_ke, jumlah, fill = lebih_baik)) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("TRUE" = "seagreen3", "FALSE" = "salmon")) +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Pekanan Positif COVID-19 di Jawa Barat",
    subtitle = "Kolom hijau menunjukan penambahan kasus baru lebih sedikit dibandingkan satu pekan sebelumnya\ndata Start Tanggal 03 Maret 2020",
    caption = "Sumber data: covid.19.go.id"
  ) + 
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")