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
missings$imputationSiteHeatMap()
imputation_site_df <- missings$one_hot_df

# characterize type of missings
missingsCharacterizer <- pguIMP::pgu.missingsCharacterizer$new()
missingsCharacterizer$analyze(imperfect_df)
missingsCharacterizer$plot_pair_dist(imperfect_df)
mCharacteristics_df <- missingsCharacterizer$missingsCharacteristics_df %>%
  dplyr::mutate(pValue = stringr::str_replace(pValue, "<0.001", "0.001")) %>%
  dplyr::mutate(pValue = as.numeric(pValue))

# wie hoch ist die Fraktion an Signifikanten pValues?
thr <- 0.05
mCharacteristics_df %>%
  dplyr::select(pValue) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(pValue),
    sd = sd(pValue),
    dependent = sum(pValue<thr)/n(),
    independent = sum(!pValue<thr)/n()
  )

#############
# plot data #
#############
scaled_df %>%
  tidyr::gather(key = "feature", value = "value") %>%
  ggplot2::ggplot() +
  ggplot2::geom_boxplot(mapping = ggplot2::aes(x=feature, y=value))
