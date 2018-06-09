#' ---
#' title: "Mi reporte"
#' output: 
#'   flexdashboard::flex_dashboard:
#'     orientation: rows
#'     df_print: kable
#' ---


#' Row
#' -------------------------------------
#' 
#' ### Distrubcion de Ventas

library(tidyverse)
library(readxl)


transactions <- read_excel("~/transactions.xlsx", sheet = "transactions")
# transactions

transactions_date <- read_excel("~/transactions.xlsx", sheet = "transactions_date")
# transactions_date


products <- read_excel("~/transactions.xlsx", sheet = "products")
# products


#' Ventas mas fome
distribucion_compras <- transactions %>% 
  group_by(id) %>% 
  summarise(n = n()) %>% 
  arrange(n) %>% 
  count(n) %>% 
  rename(cantidad_de_prodctos = n, cantidad_de_compras = nn) %>% 
  mutate(porcentaje = 100 * cantidad_de_compras/sum(cantidad_de_compras))

ggplot(distribucion_compras) +
  geom_col(aes(cantidad_de_prodctos, cantidad_de_compras))

plotly::ggplotly()

#' Row
#' -------------------------------------
#' 
#' ### Ventas por día
#' 
data_total <- transactions %>% 
  left_join(products) %>% 
  left_join(transactions_date)

venta_diaria <- data_total %>% 
  group_by(date) %>% 
  summarise(venta_diaria = sum(price))


library(lubridate)

venta_diaria <- venta_diaria %>% 
  mutate(date = ymd(date))


ggplot(venta_diaria) +
  geom_line(aes(x = date, y = venta_diaria), group = 1) +
  geom_smooth(aes(x = date, y = venta_diaria))


plotly::ggplotly()
