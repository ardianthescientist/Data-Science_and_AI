library(shinydashboard)
library(shiny)
library(bslib)

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(glue)
library(plotly)
library(scales)
library(DT)

gojek <- read.csv("gojek.csv")

# hapus kolom-kolom tidak berguna
gofood <- gojek %>% 
  filter(mode == "FOOD") %>% 
  select(
    -mode,
    -from_lat,
    -from_lng,
    -to_lat,
    -to_lng,
    -driver_id,
    -driver_name,
    -driver_gender,
    -driver_birthdate,
    -kendaraan_jenis,
    -kendaraan_merk,
    -customer_gender
  ) %>% 
  # ubah tipe data dan membuat kolom order_hour
  mutate(
    date_order = dmy_hm(date_order),
    date_finished = dmy_hm(date_finished),
    distance = as.numeric(gsub(",", ".", distance)),
    customer_birthdate = date(ymd_hms(customer_birthdate)),
    customer_age = (as.integer(floor(difftime(Sys.Date(), customer_birthdate, units = "days")) / 365.25)),
    merchant_category = as.factor(str_to_title(merchant_category)),
    from_kelurahan = as.factor(str_to_title(from_kelurahan)),
    from_kecamatan = as.factor(str_to_title(from_kecamatan)),
    to_kelurahan = as.factor(str_to_title(to_kelurahan)),
    to_kecamatan = as.factor(str_to_title(to_kecamatan)),
    order_hour = hour(date_order)
  )

# ubah nama kolom
colnames(gofood)[c(1, 2, 5, 6, 7, 8, 9, 10, 12, 13)] <- c("id", "order_id", "alamat_merchant", "kelurahan_merchant", "kecamatan_merchant", "alamat_customer", "kelurahan_customer", "kecamatan_customer", "biaya_ongkir", "harga_produk")


