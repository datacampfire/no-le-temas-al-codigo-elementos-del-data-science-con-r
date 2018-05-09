#' ---
#' title: "Mi reporte"
#' output: 
#'   flexdashboard::flex_dashboard:
#'     orientation: rows
#'     df_print: kable
#' ---


#' Row
#' -------------------------------------
   
#' ### Chart 1
library(tidyverse)
ggplot(mtcars) +
  geom_point(aes(mpg, cyl, color = gear))
plotly::ggplotly()


#' ### Chart 2
head(mtcars)


#' Row
#'-------------------------------------

#' ### Chart 3
library(tidyverse)

ggplot(mtcars) +
  geom_point(aes(mpg, cyl, color = gear))


#' ### Chart 4
head(mtcars)


