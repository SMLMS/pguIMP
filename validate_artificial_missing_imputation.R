library("tidyverse")
library("ggthemes")
library("compositions")
#library("DataVisualizations")
library("forcats")
library("pguIMP")


##################
# initialization #
##################
# init required pguIMP objects
verbose <- FALSE
shuffle_vec <- c(FALSE, TRUE)
random_state <- 42
nar <- 0.2
car <- 0.0
file_name_obj <- pguIMP::pgu.file$new()
loq_obj <- pguIMP::pgu.limitsOfQuantification$new()
importer_obj <- pguIMP::pgu.importer$new()
transformator_obj <- pguIMP::pgu.transformator$new()
normalizer_obj <- pguIMP::pgu.normalizer$new()
missings_obj <- pguIMP::pgu.missings$new()
outliers_obj <- pguIMP::pgu.outliers$new()
imputer_obj <- pguIMP::pgu.imputation$new()
missingsCharacterizer_obj <- pguIMP::pgu.missingsCharacterizer$new()
set.seed(random_state)
##########
# import #
##########
# define filename
folder_str <- sprintf("/Users/malkusch/PowerFolders/pharmacology/Daten/imputation/datasets/")
file_str <- sprintf("lipids.xlsx")
infile_name <- paste(folder_str, file_str, sep = "")

file_name_obj$setFileName <- file_str
file_name_obj$setFolderName <- folder_str
file_name_obj$setUploadFileName <- infile_name
file_name_obj$splitFileName()

# import data
data_df <- importer_obj$importData(file_name_obj) %>%
  dplyr::select(-c("Sample Name", "dCytidin"))

metadata_df <- importer_obj$importMetadata(file_name_obj)
loq_obj$setLoq <- importer_obj$importLoq(file_name_obj)
feature_names <- colnames(data_df)


##########################
# loq outliers detection #
##########################
# detect outliers
loq_obj$setNaHandlingAgent <- "keep"
loq_obj$findOutliers(data_df)
loq_obj$collectStatistics(data_df)

if (verbose){
  print("loq statistics:")
  loq_obj$loqStatistics %>%
    dplyr::filter(loqOutliers>0) %>%
    print()
  loq_obj$plotLoqDistribution()
}

# mutate outliers to NA
loq_obj$setLloqSubstituteAgent <- "keep" # choices are: c("keep","NA", "LLOQ", "0.5 LLOQ")
loq_obj$setUloqSubstituteAgent <- "keep" # choices are: c("keep", "NA", "ULOQ")
loq_data_df <- loq_obj$mutateLoqOutliers(data_df)

##################
# transformation #
##################
# init transformator
transformator_obj$resetTrafoParameter(loq_data_df)
for (feature in feature_names){
  transformator_obj$setTrafoType(feature = feature,
                                 type = "boxCox")
  transformator_obj$setMirrorLogic(feature = feature,
                                   logic = FALSE)
}
# estimate transformation parameter
transformator_obj$estimateTrafoParameter(loq_data_df)
if(verbose){
  transformator_obj$trafoParameter %>%
    print()
}
# perform transformation
transformed_df <- transformator_obj$mutateData(loq_data_df)


#################
# normalization #
#################
normalizer_obj$setNormAgent <- "min-max" # choices are: c("none", "min-max", "mean", "z-score")
normalizer_obj$detectNormParameter(transformed_df)
if(verbose){
  normalizer_obj$normParameter %>%
    print()
}
scaled_df <- normalizer_obj$scale_data(transformed_df)


###############################
# get information on outliers #
###############################
# detect outlies
outliers_obj$setOutliersAgent <- "none"
outliers_obj$setAlpha <- 0.05
outliers_obj$detectOutliers(scaled_df)
if(verbose){
  outliers_obj$outliersStatistics %>%
    dplyr::filter(outlierFraction > 0) %>%
    print()
}
# replace outliers by NA
# imperfect_df <- outliers_obj$setImputationSites(scaled_df)

#####################
# simulate missings #
#####################
set.seed(random_state)
imperfect_df <- scaled_df %>%
  compositions::simulateMissings(MARprob=car, MNARprob = nar) %>%
  tibble::as_tibble()



########################################
# get information on  imputation sites #
########################################
# detect missings
missings_obj$resetImputationParameter(imperfect_df)
missings_mat <- as.matrix(missings_obj$one_hot_df)
na_missings_mat <- missings_mat
na_missings_mat[na_missings_mat < 1] <- NA
original_vector <- (na_missings_mat * as.matrix(scaled_df)) %>%
  tibble::as_tibble() %>%
  tidyr::gather(key = "key", value = "value") %>%
  tidyr::drop_na() %>%
  dplyr::pull(value)


if(verbose){
  missings_obj$imputationSiteDistribution(imperfect_df) %>%
    print()
}

############################
# analyze imputation sites #
############################
missingsCharacterizer_obj$analyze(imperfect_df)
if(verbose){
  missingsCharacterizer_obj$missingsCharacteristics_df %>%
    dplyr::select(c("dependent", "explanatory", "pValue")) %>%
    dplyr::mutate(pValue = as.numeric(pValue)) %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = dependent, y = explanatory, fill = pValue)) +
    geom_tile() +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
}




##############
# imputation #
##############
# init imputer object
imputer_obj$setSeed <- random_state
imputer_obj$setIterations <- 5
imputer_obj$setImputationAgent <- "none"
imputer_obj$setNNeighbors <- 3
imputer_obj$setPred_frac <- 0.5
imputer_obj$setOutflux_thr <- 0.5
imputation_agent_alphabet <- imputer_obj$imputationAgentAlphabet[2:length(imputer_obj$imputationAgentAlphabet)]
# analyze imputation sites
imputer_obj$gatherImputationSites(missings_df = missings_obj$imputationSites,
                                  outliers_df = outliers_obj$outliers)
imputer_obj$analyzeImputationSites(data_df = imperfect_df)
imputer_obj$detectPredictors(data_df = imperfect_df)


if(verbose){
  imputer_obj$imputationSiteDistribution %>%
    print()
  imputer_obj$imputationSiteHeatMap()
  imputer_obj$fluxPlot()
}



#################
# Lumpensammler #
#################
validation_df = tibble::tibble(method = character(0),
                               org_value = numeric(0),
                               imp_value = numeric(0),
                               delta = numeric(0))

###################
# impute missings #
###################
imputation_agent_alphabet
for (imputation_agent in imputation_agent_alphabet){
  print(imputation_agent)
  imputer_obj$setImputationAgent <- imputation_agent
  tryCatch({
    imputed_df <- imputer_obj$handleImputationSites(data_df = imperfect_df)
    imputed_df
    # get values of imputed missings
    na_missings_mat <- missings_mat
    na_missings_mat[na_missings_mat < 1] <- NA
    cleaned_vector <- (na_missings_mat * as.matrix(imputed_df)) %>%
      tibble::as_tibble() %>%
      tidyr::gather(key = "key", value = "value") %>%
      tidyr::drop_na() %>%
      dplyr::pull(value)
    validation_iteration_df <- tibble::tibble(method = rep(imputation_agent, length(cleaned_vector)),
                                              org_value = original_vector,
                                              imp_value = cleaned_vector) %>%
      dplyr::mutate(delta = org_value - imp_value)

    validation_df <- rbind(validation_df, validation_iteration_df)
  },
  error = function(e) {
    error_string <- sprintf("\n Error in iteration %i while analyzing using method %s\n", i, imputation_agent)
    cat(error_string)
    print(e)
  })
}
validation_df %>%
  dplyr::group_by(method) %>%
  dplyr::summarise(mu = mean(delta),
                   median = median(delta),
                   sigma = sd(delta),
                   qs_25 = quantile(delta, c(0.25)),
                   qs_75 = quantile(delta, c(0.75)))
