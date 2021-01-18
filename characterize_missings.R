library("tidyverse")
library("datasets")
library("compositions")
library("pguIMP")

# create dataframe
data_df <- datasets::iris %>%
  tibble::as_tibble() %>%
  dplyr::select_if(is.numeric)

feature_names <- colnames(data_df)

##################
# transformation #
##################
# init transformator
transformator <- pguIMP::pgu.transformator$new(data_df)
for (feature in feature_names){
  transformator$setTrafoType(feature = feature,
                             type = "boxCox")
  transformator$setMirrorLogic(feature = feature,
                               logic = FALSE)
}
# estimate transformation parameter
transformator$estimateTrafoParameter(data_df)
# perform transformation
transformed_df <- transformator$mutateData(data_df)


#################
# normalization #
#################
normalizer <- pguIMP::pgu.normalizer$new()
normalizer$setNormAgent <- "min-max"
normalizer$detectNormParameter(transformed_df)
scaled_df <- normalizer$scale_data(transformed_df)

#####################
# define parameters #
#####################
max_runs <- 10
car_seq <- seq(from =0.00, to = 0.2,  by = 0.01)
nar <- 0.2
missings <- pguIMP::pgu.missings$new()
missingsCharacterizer <- pguIMP::pgu.missingsCharacterizer$new()
##################
# run experiment #
##################
dependency_df <- tibble::tibble()
# car durch i ersetzten
for (car in car_seq){
  progress_bar <- sprintf("progress: %.2f\r", car/max(car_seq)) %>%
    cat()
  dependency_condition_df <- tibble::tibble()
  for (i in 1:max_runs){
    #####################
    # simulate missings #
    #####################
    set.seed(42*car+i)
    imperfect_df <- scaled_df %>%
      compositions::simulateMissings(MARprob=car, MNARprob = nar) %>%
      tibble::as_tibble()

    ####################
    # analyze missings #
    ####################
    # detect missings
    missings$resetImputationParameter(imperfect_df)
    imputation_site_df <- missings$one_hot_df

    # characterize type of missings
    missingsCharacterizer$analyze(imperfect_df)
    mCharacteristics_df <- missingsCharacterizer$missingsCharacteristics_df %>%
      dplyr::mutate(pValue = stringr::str_replace(pValue, "<", "")) %>%
      dplyr::mutate(pValue = as.numeric(pValue))

    # Bonferroni correction
    n_tests <- nrow(mCharacteristics_df)
    mCharacteristics_df <- mCharacteristics_df %>%
      dplyr::mutate(pValue_corrected = pValue * n_tests)

    # wie hoch ist die Fraktion an Signifikanten pValues?
    thr <- 0.05
    dependency_iteration_df <- mCharacteristics_df %>%
      dplyr::select(pValue_corrected) %>%
      dplyr::summarise(
        count = n(),
        mean = mean(pValue_corrected),
        sd = sd(pValue_corrected),
        dependent = sum(pValue_corrected<thr)/n(),
        independent = sum(!pValue_corrected<thr)/n()
      )

    dependency_condition_df <- rbind(dependency_condition_df, dependency_iteration_df)
  }
  dependency_df <- dependency_df %>%
    rbind(
      dependency_condition_df %>%
        dplyr::select(dependent) %>%
        dplyr::mutate(condition = rep(car, max_runs))
    )
}
cat("\ndone!\n")

dependency_df %>%
  dplyr::filter(dependent != 0) %>%
  tail()
###############
# plot result #
###############
dependency_df %>%
  dplyr::arrange(condition) %>%
  dplyr::mutate(condition = as.character(condition)) %>%
  ggplot2::ggplot() +
  ggplot2::geom_boxplot(mapping = ggplot2::aes(x=condition, y = dependent), outlier.shape = NA)
