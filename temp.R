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


one_hot = function(data_df = "tbl_df"){
  if(!tibble::is_tibble(data_df)){
    print("Warning: data_df needs to by of type tibble.")
    return(NULL)
  }
  data_df %>%
    dplyr::select_if(is.numeric) %>%
    dplyr::transmute_all(list(miss = ~ as.integer(is.na(.)))) %>%
    return()
}

v <- sequence(nvec = 10, from = 1, by = 1)
v1 <- v
v2 <- v
v1[c(2,4,6,8,10)] <- NA
v2[c(1,3,5,7,9)] <- NA

data_df <- tibble::tibble(f1 = v1,
                          f2 = v2)


data_df %>%
  one_hot()


  dplyr::select_if(is.numeric) %>%
  dplyr::mutate_all(list(miss = ~ as.inteis.na(.)))





