library("tidyverse")
library("ggthemes")
library("compositions")
#library("DataVisualizations")
library("forcats")
library("pguIMP")

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

##################
# initialization #
##################
# init required pguIMP objects
verbose <- TRUE
shuffle_vec <- c(FALSE, TRUE)
shuffle_request <- shuffle_vec[1]
random_state <- 42

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

if (shuffle_request){
  set.seed(random_state)
  data_df <- shuffle_data(data_df)
}

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
loq_obj$setLloqSubstituteAgent <- "NA" # choices are: c("keep","NA", "LLOQ", "0.5 LLOQ")
loq_obj$setUloqSubstituteAgent <- "NA" # choices are: c("keep", "NA", "ULOQ")
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
imperfect_df <- outliers_obj$setImputationSites(scaled_df)

############################
# analyze imputation sites #
############################

# Lumpensammler
dependency_df <- tibble::tibble(dependent = character(),
                                explanatory = character(),
                                p_apparent = numeric(),
                                p_corrected = numeric(),
                                random_state = integer())

######################
# calculate p values #
######################
missingsCharacterizer_obj$reset(imperfect_df)
missingsCharacterizer_obj$analyze(imperfect_df)
mCharacteristics_df <- missingsCharacterizer_obj$missingsCharacteristics_df %>%
  dplyr::mutate(p_apparent = stringr::str_replace(pValue, "<", "")) %>%
  dplyr::mutate(p_apparent = as.numeric(p_apparent))

# Bonferroni correction
n_tests <- nrow(mCharacteristics_df)
mCharacteristics_df <- mCharacteristics_df %>%
  dplyr::mutate(p_corrected = p_apparent * n_tests)

dependency_df <- mCharacteristics_df %>%
  dplyr::select(c("dependent", "explanatory", "p_apparent", "p_corrected" ))

#######################
# plot p_value Matrix #
#######################

significance_alphabet <-c("not significant", "significant")

dependency_df %>%
  dplyr::select(c("dependent", "explanatory", "p_corrected")) %>%
  dplyr::mutate(p_value = ifelse(p_corrected < 0.05, "significant", "not significant")) %>%
  dplyr::mutate(p_value = factor(p_value, levels = significance_alphabet)) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(x = dependent, y = explanatory, fill = p_value)) +
  ggplot2::geom_tile() +
  scale_fill_manual(values = c(gdocs_pal()(2))) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))


##############################
# plot missings distribution #
##############################
set.seed(random_state)
imperfect_df <- scaled_df %>%
  dplyr::select(c("C16Cer",
                  "C16GluCer",
                  "C16LacCer",
                  "C16Sphinganin",
                  "C20Cer",
                  "C24Cer",
                  "C24_1Cer",
                  "cGMP",
                  "S1P",))

missingsCharacterizer_obj$reset(imperfect_df)
missingsCharacterizer_obj$analyze(imperfect_df)
missingsCharacterizer_obj$plot_pair_dist(imperfect_df)
