library("tidyverse")
library("mice")

fileName <- "/Users/malkusch/PowerFolders/pharmacology/Daten/Gurke/data_paper_2020/66-14_semi-targeted_Zeitpunkt1.xlsx"

data_df <- readxl::read_xlsx(path = fileName,
                             sheet = 1)

data_transformed_df <- data_df %>%
  dplyr::select(-c("Sample Name")) %>%
  dplyr::mutate_all(log)

n_row <- nrow(data_transformed_df)
n_column <- ncol(data_transformed_df)

data_transformed_df %>%
  mice::fluxplot()

seed <- 42
set.seed(seed)
fx1_df <- data_transformed_df %>%
  mice::flux() %>%
  tibble::rownames_to_column() %>%
  tibble::as_tibble()

outlist1 <- fx1_df %>%
  dplyr::filter(outflux < 0.5) %>%
  dplyr::select(rowname) %>%
  dplyr::pull()

fx2_df <- data_transformed_df %>%
  dplyr::select(-dplyr::all_of(outlist1)) %>%
  mice::flux() %>%
  tibble::rownames_to_column() %>%
  tibble::as_tibble()

outlist2 <- fx2_df %>%
  dplyr::filter(outflux < 0.5) %>%
  dplyr::select(rowname) %>%
  dplyr::pull()

outlist <- unique(c(outlist1, outlist2))

data_filtered_df <- data_transformed_df %>%
  dplyr::select(-dplyr::all_of(outlist))

mean_number_of_predictors = function(data_df, minpuc, mincor){
  pred <- data_df %>%
    mice::quickpred(minpuc = minpuc, mincor = mincor)

  pred_dist <- table(rowSums(pred))

  sum(as.numeric(names(pred_dist)) * as.numeric(pred_dist)) / sum(as.numeric(pred_dist)[2:length(pred_dist)]) %>%
    return()
}

quickpred_df <- tidyr::expand_grid(minpuc = seq(from=0.1, to=0.9, by=0.1),
                                   mincor = seq(from=0.1, to=0.9, by=0.1))


quickpred_df <- quickpred_df %>%
  dplyr::rowwise() %>%
  dplyr::mutate(mean = mean_number_of_predictors(data_filtered_df, minpuc, mincor)) %>%
  dplyr::ungroup()

quickpred_para <- quickpred_df %>%
  tidyr::drop_na() %>%
  dplyr::slice(which.min(abs(quickpred_df$mean - 15)))


minpuc <- quickpred_para$minpuc
mincor <- quickpred_para$mincor


pred <- data_transformed_df %>%
  mice::quickpred(minpuc = minpuc, mincor = mincor, exclude = outlist)

# mice_model <- data_transformed_df %>%
#   mice::mice(method = "cart", pred = pred, seed = seed, printFlag = FALSE)
#
# data_imputed_df <- mice_model %>%
#   mice::complete()
#
# data_imputed_df %>%
#   tibble::as_tibble()

# iterations <- 2
# stats0 <- data_transformed_df %>%
#   dplyr::select(c("TG_44.0")) %>%
#   unlist() %>%
#   psych::describe()
# print(stats0)
#
# stats_mat <- matrix(NA, ncol= iterations, nrow = 13)
# stats_mat
#
# for (i in seq(1, iterations)){
#   mice_model <- data_transformed_df %>%
#     mice::mice(method = "cart", pred = pred, seed = seed+i, printFlag = FALSE)
#
#   stats_mat[,i] <- mice_model %>%
#     mice::complete() %>%
#     dplyr::select(c("TG_44.0")) %>%
#     unlist() %>%
#     psych::describe() %>%
#     unlist()
# }
# stats_mat
#
# diffMat <- stats_mat %>%
#   sweep(MARGIN = 1, STATS = unlist(stats0), FUN = "-") %>%
#   abs()
#
# diffMat
#
# ranks <- diffMat %>%
#   apply(MARGIN = 1, FUN = function(x)rank(x, ties.method = "min"))
#
# ranks
#
# ranks %>%
#   rowSums() %>%
#   which.min()

seed <- 42
iterations <- 10

# Calculate Errors
stats0_mat <- data_transformed_df %>%
  psych::describe(na.rm=TRUE) %>%
  as.matrix()

stats_mat_list = list()
diff_mat_list = list()
for (i in seq(1, iterations)){
  mice_model <- data_transformed_df %>%
    mice::mice(method = "cart", pred = pred, seed = seed+i, printFlag = FALSE)

  stats_mat_list[[i]] <- mice_model %>%
    mice::complete() %>%
    psych::describe(na.rm=TRUE) %>%
    as.matrix()

  diff_mat_list[[i]] <- stats0_mat - stats_mat_list[[i]]
}

# Calculate Ranks
cumulative_diff_df <- tibble::tibble(statistics = colnames(diff_mat_list[[1]]))
for(i in seq(iterations)){
  diff_sum <- rep(0.0, times=13)
  for(j in seq(1, ncol(data_transformed_df))){
    diff_sum <- diff_sum + (diff_mat_list[[i]][j,])^2
  }
  feature_name <- sprintf("iter_%i", i)
  cumulative_diff_df <- cumulative_diff_df %>%
    dplyr::mutate(!!feature_name := sqrt(diff_sum))
}
cumulative_diff_df

ranks_mat <- cumulative_diff_df %>%
  dplyr::select(-c("statistics")) %>%
  apply(MARGIN = 1, FUN = function(x)rank(x, ties.method = "min"))

# determine optimal seed
seed_additive <- ranks_mat %>%
  rowSums() %>%
  which.min() %>%
  as.integer()

ranks_mat

seed_additive

# mc
# Calculate Errors
stats0_mat <- data_transformed_df %>%
  psych::describe(na.rm=TRUE) %>%
  as.matrix()

stats_mat_list = list()
diff_mat_list = list()
for(i in seq(iterations)){
  set.seed(self$seed + i)
  for(feature in colnames(data_transformed_df)){
    mu <- data_transformed_df %>%
      dplyr::select(feature) %>%
      tidyr::drop_na() %>%
      dplyr::pull(feature) %>%
      as.double() %>%
      mean()

    sigma <- data_transformed_df %>%
      dplyr::select(feature) %>%
      tidyr::drop_na() %>%
      dplyr::pull(feature) %>%
      as.double() %>%
      sd()


  }
}


# M5p tree
imputed_df <- data_transformed_df
for(feature in colnames(data_transformed_df)){
  na_idx <- data_transformed_df %>%
    dplyr::pull(feature) %>%
    as.numeric() %>%
    is.na() %>%
    which()

  if((length(na_idx)<1) | length(na_idx) == nrow(data_transformed_df)){
    next
  }#if

  predictor_idx <- pred[feature,] %>%
    as.logical()

  predictor_names <- colnames(pred)[predictor_idx]

  #split in train and prediction data
  train_df <- data_transformed_df %>%
    dplyr::select(dplyr::all_of(c(feature, predictor_names))) %>%
    dplyr::slice(-na_idx)

  na_df <- data_transformed_df %>%
    dplyr::select(dplyr::all_of(c(feature, predictor_names))) %>%
    dplyr::slice(na_idx)

  # if predictor selection fails, take all features as predictors
  if(ncol(na_df) <2){
    print(feature)
    train_df <- data_transformed_df %>%
      dplyr::slice(-na_idx)

    na_df <- data_transformed_df %>%
      dplyr::slice(na_idx)
  }

  print(feature)
  m5p_model <- sprintf("%s ~ .", feature) %>%
    stats::as.formula() %>%
    RWeka::M5P(data=train_df)

  imputed_values <- predict(m5p_model, newdata = na_df)

  print(feature)
  print(imputed_values)

  for (j in 1:length(na_idx)){
    imputed_df[[na_idx[j], feature]] <- imputed_values[j]
  }#for
}


pred %>%
  rowSums()
