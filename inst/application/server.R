#' @title server
#' @description Run shiny based gui of pguIMP.
#'
#' @import tidyverse
#' @import shiny
#' @import shinydashboard
#' @import shinyjs
#'
#' @include pguDelegate.R
#'
#' @param input Pointer to shiny input
#' @param output Pointer to shiny output
#' @param session Pointer to shiny session
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'

server <- function(input, output, session) {
  delegate <- pgu.delegate$new()
  analysisFinished <- shiny::reactiveVal(FALSE)

  shiny::observeEvent(input$fi.import,{
    delegate$queryExcel(input, output, session)
    delegate$hideOutdatedResults(input, output, session)
    analysisFinished(FALSE)
  })

  shiny::observeEvent(input$ab.import,{
    delegate$importData(input, output, session)
    delegate$updateRawDataInfo(input, output, session)
    delegate$importLoq(input, output, session)
    delegate$updateLoqInfo(input, output, session)
    delegate$importMetadata(input, output, session)
    delegate$updateMetadataInfo(input, output, session)
    delegate$updateFilterTbl(input, output, session)
    delegate$resetFilter(input, output, session)
    delegate$filterData(input, output, session)
    delegate$updateFilterStatisticsTbl(input, output, session)
    delegate$updateFilterMissingsTbl(input, output, session)
    delegate$updateLoqDetectGui(input, output, session)
    delegate$hideOutdatedResults(input, output, session)
    analysisFinished(FALSE)
  })

  shiny::observeEvent(input$ab.filterSet,{
    delegate$updateFilter(input, output, session)
    delegate$filterData(input, output, session)
    delegate$updateFilterStatisticsTbl(input, output, session)
    delegate$updateFilterMissingsTbl(input, output, session)
    delegate$updateLoqDetectGui(input, output, session)
    delegate$hideOutdatedResults(input, output, session)
    analysisFinished(FALSE)
  })

  shiny::observeEvent(input$ab.filterInvSet,{
    delegate$updateFilterInverse(input, output, session)
    delegate$filterData(input, output, session)
    delegate$updateFilterStatisticsTbl(input, output, session)
    delegate$updateFilterMissingsTbl(input, output, session)
    delegate$updateLoqDetectGui(input, output, session)
    delegate$hideOutdatedResults(input, output, session)
    analysisFinished(FALSE)
  })

  shiny::observeEvent(input$ab.filterReset,{
    delegate$resetFilter(input, output, session)
    delegate$filterData(input, output, session)
    delegate$updateFilterStatisticsTbl(input, output, session)
    delegate$updateFilterMissingsTbl(input, output, session)
    delegate$updateLoqDetectGui(input, output, session)
    delegate$hideOutdatedResults(input, output, session)
    analysisFinished(FALSE)
  })

  shiny::observeEvent(input$si.exploreAbs, {
    delegate$updateExplorationAbscissa(input, output, session)
    delegate$updateExplorationGraphic(input, output, session)
    delegate$updateExplorationAbscissaGraphic(input, output, session)
    delegate$updateExplorationAbscissaTable(input, output, session)
  })

  shiny::observeEvent(input$si.exploreOrd, {
    delegate$updateExplorationOrdinate(input, output, session)
    delegate$updateExplorationGraphic(input, output, session)
    delegate$updateExplorationOrdinateGraphic(input, output, session)
    delegate$updateExplorationOrdinateTable(input, output, session)
  })

  shiny::observeEvent(input$ab.detectLoq, {
    delegate$detectLoq(input, output, session)
    delegate$updateLoqDetectStatisticsGraphic(input, output, session)
    delegate$updateLoqDetectFeatureGraphic(input, output, session)
    delegate$updateLoqDetectFeatureTbl(input, output, session)
    delegate$updateLoqDetectStatisticsTbl(input, output, session)
    delegate$updateLoqDetectOutlierTbl(input, output, session)
    delegate$updateLoqDetectDataTbl(input, output, session)
    delegate$updateLoqMutateGui(input, output, session)
    delegate$hideOutdatedResults(input, output, session)
    analysisFinished(FALSE)
  })

  shiny::observeEvent(input$si.loqDetectFeature, {
    delegate$updateLoqDetectFeatureGraphic(input, output, session)
    delegate$updateLoqDetectFeatureTbl(input, output, session)
  })

  shiny::observeEvent(input$ab.resetDetectLoq, {
    delegate$updateLoqDetectGui(input, output, session)
  })

  shiny::observeEvent(input$ab.mutateLoq, {
    delegate$mutateLoq(input, output, session)
    delegate$updateLoqMutateStatisticsGraphic(input, output, session)
    delegate$updateLoqMutateFeatureGraphic(input, output, session)
    delegate$updateLoqMutateFeatureTbl(input, output, session)
    delegate$updateLoqMutateStatisticsTbl(input, output, session)
    delegate$updateLoqMutateOutlierTbl(input, output, session)
    delegate$updateLoqMutateDataTbl(input, output, session)
    delegate$updateTrafoMutateGui(input, output, session)
    delegate$hideOutdatedResults(input, output, session)
    analysisFinished(FALSE)
  })

  shiny::observeEvent(input$si.loqMutateFeature, {
    delegate$updateLoqMutateFeatureGraphic(input, output, session)
    delegate$updateLoqMutateFeatureTbl(input, output, session)
  })

  shiny::observeEvent(input$ab.resetMutateLoq, {
    delegate$updateLoqMutateGui(input, output, session)
  })

  # shiny::observeEvent(input$ab.wizardOptimize, {
  #   delegate$optimizeTrafoParameter(input, output, session)
  #   delegate$updateDetectedTrafoTypes(input, output, session)
  #   delegate$updateDetectedTrafoParameter(input, output, session)
  #   delegate$hideOutdatedResults(input, output, session)
  #   analysisFinished(FALSE)
  # })
  #
  # shiny::observeEvent(input$ab.wizardReset, {
  #   delegate$updateTrafoDetectGui(input, output, session)
  # })

  shiny::observeEvent(input$ab.trafoMutateSetGlobal, {
    delegate$trafoMutateGlobal(input, output, session)
    delegate$updateTrafoMutateFeatureGraphic(input, output, session)
    delegate$updateTrafoMutateFeatureParameterTbl(input, output, session)
    delegate$updateTrafoMutateFeatureQualityTbl(input, output, session)
    delegate$updateTrafoMutateGlobalParameterTbl(input, output, session)
    delegate$updateTrafoMutateGlobalModelTbl(input, output, session)
    delegate$updateTrafoMutateGlobalQualityTbl(input, output, session)
    delegate$updateTrafoMutateGlobalTestsTbl(input, output, session)
    delegate$updateTrafoMutateGlobalDataTbl(input, output, session)
    delegate$updateImputeNormGui(input, output, session)
    delegate$hideOutdatedResults(input, output, session)
    analysisFinished(FALSE)
  })

  shiny::observeEvent(input$ab.trafoMutateFeature, {
    delegate$trafoMutateFeature(input, output, session)
    delegate$updateTrafoMutateFeatureGraphic(input, output, session)
    delegate$updateTrafoMutateFeatureParameterTbl(input, output, session)
    delegate$updateTrafoMutateFeatureQualityTbl(input, output, session)
    delegate$updateTrafoMutateGlobalParameterTbl(input, output, session)
    delegate$updateTrafoMutateGlobalModelTbl(input, output, session)
    delegate$updateTrafoMutateGlobalQualityTbl(input, output, session)
    delegate$updateTrafoMutateGlobalTestsTbl(input, output, session)
    delegate$updateTrafoMutateGlobalDataTbl(input, output, session)
    delegate$updateImputeNormGui(input, output, session)
    delegate$hideOutdatedResults(input, output, session)
    analysisFinished(FALSE)
  })

  shiny::observeEvent(input$si.trafoMutateFeature, {
    delegate$updateTrafoMutateFeatureGraphic(input, output, session)
    delegate$updateTrafoMutateFeatureParameterTbl(input, output, session)
    delegate$updateTrafoMutateFeatureQualityTbl(input, output, session)
    delegate$resetTrafoMutateGui(input,output, session)
  })

  shiny::observeEvent(input$ab.trafoMutateReset, {
    delegate$resetTrafoMutateGui(input, output, session)
  })

  shiny::observeEvent(input$ab.imputeNormMutate,{
    delegate$imputeNormMutate(input, output, session)
    delegate$updateImputeNormFeatureGraphic(input, output, session)
    delegate$updateImputeNormFeatureStatisticsTbl(input, output, session)
    delegate$updateImputeNormParameterTbl(input, output, session)
    delegate$updateImputeNormStatisticsTbl(input, output, session)
    delegate$updateImputeNormDataTbl(input, output, session)
    delegate$hideOutdatedResults(input, output, session)
    analysisFinished(FALSE)
  })

  shiny::observeEvent(input$si.imputeNormFeature, {
    delegate$updateImputeNormFeatureGraphic(input, output, session)
    delegate$updateImputeNormFeatureStatisticsTbl(input, output, session)
    delegate$resetImputeNormGui(input,output, session)
  })

  shiny::observeEvent(input$ab.imputeNormReset, {
    delegate$resetImputeNormGui(input, output, session)
  })

  shiny::observeEvent(input$ab.imputeMissingsDetect, {
    delegate$imputeMissingsDetect(input, output, session)
    delegate$updateImputeMissingsGraphic(input, output, session)
    delegate$updateImputeMissingsStatisticsTbl(input, output, session)
    delegate$updateImputeMissingsDistributionTbl(input, output, session)
    delegate$updateImputeMissingsDetailTbl(input, output, session)
    delegate$updateImputeMissingsDataTbl(input, output, session)
    delegate$updateImputeOutliersGui(input, output, session)
    delegate$hideOutdatedResults(input, output, session)
    analysisFinished(FALSE)
  })

  shiny::observeEvent(input$ab.imputeOutliersReset, {
    delegate$resetImputeOutliersGui(input, output, session)
  })

  shiny::observeEvent(input$ab.imputeOutliersDetect, {
    delegate$imputeOutliersDetect(input, output, session)
    delegate$updateImputeOutliersGraphic(input, output, session)
    delegate$updateImputeOutliersFeatureGraphic(input, output, session)
    delegate$updateImputeOutliersFeatureTbl(input, output, session)
    delegate$updateImputeOutliersStatisticsTbl(input, output, session)
    delegate$updateImputeOutliersDetailTbl(input, output, session)
    delegate$updateImputeOutliersDataTbl(input, output, session)
    delegate$updateImputeMutateGui(input, output, session)
    delegate$hideOutdatedResults(input, output, session)
    analysisFinished(FALSE)
  })

  shiny::observeEvent(input$si.imputeOutliersFeature, {
    delegate$updateImputeOutliersFeatureGraphic(input, output, session)
    delegate$updateImputeOutliersFeatureTbl(input, output, session)
  })


  shiny::observeEvent(input$ab.imputeMutateReset, {
    delegate$resetImputeMutateGui(input, output, session)
  })

  shiny::observeEvent(input$ab.imputeMutateMutate, {
    delegate$imputeMutateMutate(input, output, session)
    delegate$updateImputeMutateSeed(input, output, session)
    delegate$updateImputeMutateGraphic(input, output, session)
    delegate$updateImputeMutateStatisticsTbl(input, output, session)
    delegate$updateImputeMutateDistributionTbl(input, output, session)
    delegate$updateImputeMutateFeatureDetailGraphic(input, output, session)
    delegate$updateImputeMutateFeatureDetailTbl(input, output, session)
    delegate$updateImputeMutateDetailTbl(input, output, session)
    delegate$updateImputeMutateDataTbl(input, output, session)
    delegate$hideOutdatedResults(input, output, session)
    analysisFinished(FALSE)
  })

  shiny::observeEvent(input$ab.validation, {
    delegate$validate(input, output, session)
    delegate$updateAnalysisValidationGui(input, output, session)
    delegate$updateanalysisValidationGraphic(input, output, session)
    delegate$updateAnalysisValidationTestTbl(input, output, session)
    delegate$updateCentralMomentsOrgTbl(input, output, session)
    delegate$updateCentralMomentsImpTbl(input, output, session)
    delegate$updateCentralMomentsDeltaTbl(input, output, session)
    analysisFinished(FALSE)
  })

  shiny::observeEvent(input$si.analysisValidationFeature, {
    delegate$updateanalysisValidationGraphic(input, output, session)
  })

  shiny::observeEvent(input$ab.correlation, {
    delegate$correlate(input, output, session)
    delegate$updateCorrelationMatrixPPearsonTbl(input, output, session)
    delegate$updateCorrelationMatrixRTbl(input, output, session)
    delegate$updateCorrelationMatrixPKendallTbl(input, output, session)
    delegate$updateCorrelationMatrixTauTbl(input, output, session)
    delegate$updateCorrelationMatrixPSpearmanTbl(input, output, session)
    delegate$updateCorrelationMatrixRhoTbl(input, output, session)
    delegate$hideOutdatedResults(input, output, session)
  })

  shiny::observeEvent(input$ab.regression, {
    delegate$regression(input, output, session)
    delegate$updateRegressionGui(input, output, session)
    delegate$updateRegressionGraphic(input, output, session)
    delegate$updateRegressionModelTbl(input, output, session)
    delegate$updateRegressionInterceptTbl(input, output, session)
    delegate$updateRegressionPInterceptTbl(input, output, session)
    delegate$updateRegressionSlopeTbl(input, output, session)
    delegate$updateRegressionPSlopeTbl(input, output, session)
    delegate$hideOutdatedResults(input, output, session)
  })

  shiny::observeEvent(input$si.regressionAbs, {
    delegate$updateRegressionOrdinate(input, output, session)
    delegate$updateRegressionGraphic(input, output, session)
    delegate$updateRegressionModelTbl(input, output, session)
  })

  shiny::observeEvent(input$si.regressionOrd, {
    delegate$updateRegressionGraphic(input, output, session)
    delegate$updateRegressionModelTbl(input, output, session)
  })

  shiny::observe({
    if (analysisFinished()) {
      shinyjs::enable("dbExport")
      shinyjs::enable("dbReport")
    }
    else {
      shinyjs::disable("dbExport")
      shinyjs::disable("dbReport")
    }
  })

  output$dbExport <- shiny::downloadHandler(
    filename = function(){
      delegate$exportFileName(input, output, session)
    },
    content = function(file){
      delegate$exportData(file)
    }
  )

  output$dbReport <- shiny::downloadHandler(
    filename = function(){
      delegate$reportFileName(input, output, session)
    },
    content = function(file){
      delegate$writeReport(file)
    }
  )

  output$helpMenu <- shinydashboard::renderMenu({
    shinydashboard::dropdownMenu(type = "notifications",
                                 icon = icon("question-circle"),
                                 badgeStatus = NULL,
                                 headerText = "See also:",
                                 tags$li(
                                   a(href = "pguHelp.html",
                                     target = "_blank",
                                     tagAppendAttributes(icon("users"),
                                                         class = "text-info"),
                                     "Help"
                                     )
                                 )
    )
  })
}




