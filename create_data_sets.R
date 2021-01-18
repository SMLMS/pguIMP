library("tidyverse")
library("datasets")
library("compositions")

# create dataframe
iris_data = function(){
  datasets::iris %>%
    tibble::as_tibble() %>%
    dplyr::rename(label = Species) %>%
    return()
}

# define missings
insert_missings = function(data_df = "tbl_df", car_rate = 0.1, nar_rate = 0.0){
  if(!tibble::is_tibble(data_df)){
    message_str <- sprintf("\nWarning(insert_missing): data_df needs to be of type tibble. Was of type %s\n", typeof(data_df))
    cat(message_str)
    return(data_df)
  }
  data_df %>%
    dplyr::select(-c("label")) %>%
    compositions::simulateMissings(MARprob=car_rate, MNARprob = nar_rate) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(label = data_df$label) %>%
    return()
}

shuffle_data = function(data_df = "tbl_df"){
  if(!tibble::is_tibble(data_df)){
    message_str <- sprintf("\nWarning(insert_missing): data_df needs to be of type tibble. Was of type %s\n", typeof(data_df))
    cat(message_str)
    return(data_df)
  }
  data_df %>%
    dplyr::mutate_if(is.numeric, sample) %>%
    return()
}

random_state <- 42
data_type_vec <- c("iris")
car_vec <- c(0.0, 0.1, 0.0)
nar_vec <- c(0.0, 0.0, 0.1)
shuffle_vec <- c(TRUE, FALSE)

set.seed(random_state)
data_df <- iris_data()
imperfect_df <- insert_missings(data_df = data_df,
                                car_rate = car_rate,
                                nar_rate = nar_rate)

folder_str <- sprintf("/Users/malkusch/PowerFolders/pharmacology/Daten/imputation/datasets/")
for (i in seq(length(data_type_vec))){
  data_type <- data_type_vec[i]
  for (j in seq(length(car_vec))){
    data_df <- switch (data_type,
                       "iris" = {iris_data()},
                       "lipids" = {print("under construction")}
    )
    data_df <- insert_missings(data_df = data_df,
                               car_rate = car_vec[j],
                               nar_rate = nar_vec[j])

    for (k in seq(length(shuffle_vec))){
      if(shuffle_vec[k]){
        sample_df <- shuffle_data(data_df)
      }
      else{
        sample_df <- data_df
      }
      file_str <- sprintf("%s_car%i_nar%i_shuffle_%s.csv", data_type_vec[i], as.integer(car_vec[j]*100), as.integer(nar_vec[j]*100), as.character(shuffle_vec[k]))
      outfile_name <- paste(folder_str, file_str, sep = "")
      sample_df %>%
        readr::write_csv(outfile_name)
    }
  }
}
