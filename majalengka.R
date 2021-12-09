library(httr)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(lubridate)
library(tidyr)
library(plotly)
# Memperoleh data dari API
resp_prov <- GET("https://covid19-public.digitalservice.id/api/v1/rekapitulasi_v2/jabar/kumulatif?level=kab")
cov_prov_raw <- content(resp_prov, as = "parsed", simplifyVector = TRUE)
names(cov_prov_raw)


# Karena List Perkembangan merupakan array lagi jadi kita masukan ke variable baru 
cov_prov_jabar <- cov_prov_raw$data$content
names(cov_prov_jabar)

# Select data apa aja yang akan ditampilkan
majalengka <-
  cov_prov_jabar %>%
  filter(
    nama_kab %in% c("Kabupaten Majalengka")
  ) %>%
  rename(
    confirmation_kasus = CONFIRMATION,
  ) %>%
  select(tanggal, confirmation_kasus, confirmation_selesai, confirmation_meninggal) %>%
  mutate(
    dates = as.POSIXct(tanggal, origin = "1970-01-01"),
    dates = as.Date(dates)
  )

str(majalengka)
# Akumulasi data
majalengka_akumulasi <- 
  majalengka %>% 
  transmute(
    dates,
    confirmation_aktif = confirmation_kasus,
    confirmation_sembuh = confirmation_selesai,
    confirmation_meninggal = confirmation_meninggal
  )

# Disini kita akan menampilkan 3 data(kasus, sembuh dan meninggal) dalam 1 graph
# Langkah pertama membuat pivot dulu dari data akumulasi menggunakan library(tidyr)
dim(majalengka_akumulasi)

majalengka_akumulasi_pivot <- 
  majalengka_akumulasi %>% 
  gather(
    key = "kategori",
    value = "jumlah",
    -dates
  ) %>% 
  mutate(
    kategori = sub(pattern = "confirmation_", replacement = "", kategori)
  )

dim(majalengka_akumulasi_pivot)

glimpse(majalengka_akumulasi_pivot)


# Convert Ke dalam Bentuk Grafik
plot <- ggplot(majalengka_akumulasi_pivot, aes(dates, jumlah, colour = (kategori))) +
  geom_line(size = 0.9) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL), limits = c(0,8000)) +
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
    title = "Dinamika Kasus COVID-19 di Majalengka",
    caption = "Sumber data: covid19-public.digitalservice.id"
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