library("devtools")
library("roxygen2")
devtools::document()
library("pguIMP")
pguIMP::IMPgui()


library("tidyverse")

data_df <- readxl::read_xlsx("/Users/malkusch/PowerFolders/pharmacology/Daten/Gurke/data_paper_2020/66-14_semi-targeted_Zeitpunkt1.xlsx",
                  sheet = 1)


norm_sigmoid = function(x){
  y <- 1.0 / (1.0 + exp(-1.0 * x))
  return(y)
}

data_df %>%
  dplyr::select("TG_42.1") %>%
  tidyr::drop_na() %>%
  dplyr::mutate(TG_42.1 = norm_sigmoid(TG_42.1)) %>%
  ggplot2::ggplot() +
  ggplot2::geom_density(mapping = ggplot2::aes(x= TG_42.1))
