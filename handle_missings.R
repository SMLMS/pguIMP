library("tidyverse")
library("datasets")
library("compositions")
library("DataVisualizations")
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
# simulate missings #
#####################
set.seed(42)
imperfect_df <- scaled_df %>%
  compositions::simulateMissings(MNARprob = 0.05) %>%
  tibble::as_tibble()

####################
# analyze missings #
####################
# detect missings
missings <- pguIMP::pgu.missings$new()
missings$resetImputationParameter(imperfect_df)
missings_mat <- as.matrix(missings$one_hot_df)

missings_mat[missings_mat < 1] <- NA

# get values of simulated missings
missings_vector <- (missings_mat * as.matrix(scaled_df)) %>%
  tibble::as_tibble() %>%
  tidyr::gather(key = "key", value = "value") %>%
  tidyr::drop_na(value) %>%
  dplyr::pull(value)

###################
# impute missings #
###################
imputer <- pguIMP::pgu.imputation$new(seed = 42,
                                      iterations = 4,
                                      imputationAgent = "none",
                                      nNeighbors = 3,
                                      pred_frac = 1.0,
                                      outflux_thr = 0.5)

imputer$gatherImputationSites(missings_df = missings$imputationSites)

imputer$analyzeImputationSites(data_df = imperfect_df)

imputer$detectPredictors(data_df = imperfect_df)

imputer$imputationSiteHeatMap()

imputer$setImputationAgent <- "mean"
imputer$setSeed <- 42

imputer$fluxPlot()

imputer$pred_mat

imputed_df <- imputer$handleImputationSites(data_df = imperfect_df)

# get values of imputed missings
imputed_vector <- (missings_mat * as.matrix(imputed_df)) %>%
  tibble::as_tibble() %>%
  tidyr::gather(key = "key", value = "value") %>%
  tidyr::drop_na(value) %>%
  dplyr::pull(value)


###############################
# relative mean squared error #
###############################

error_df <- tibble::tibble(original = missings_vector,
               imputed = imputed_vector) %>%
  dplyr::mutate(square_error = (original-imputed)^2)

sqrt(sum(error_df$square_error)/nrow(error_df))
