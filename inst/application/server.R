#' @title server
#' @description Run shiny based gui of pguIMP.
#'
#' @importFrom shiny downloadHandler observe observeEvent reactiveVal
#' @importFrom shinydashboard renderMenu dropdownMenu
#' @importFrom shinyjs disable enable
#' @importFrom pguIMP pgu.delegate
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
  delegate <- pguIMP::pgu.delegate$new()
  analysisFinished <- shiny::reactiveVal(FALSE)

  shiny::observeEvent(input$fi.import,{
    delegate$query_data(input, output, session)
    delegate$import_data(input, output, session)
    delegate$update_import_data_Types_tbl(input, output, session)
    delegate$update_import_data_statistics_tbl(input, output, session)
    delegate$update_import_missings_statistics_tbl(input, output, session)
    delegate$update_filter_select_tbl(input, output, session)
    delegate$hide_outdated_results(input, output, session)
    analysisFinished(FALSE)
  })

  shiny::observeEvent(input$ab.importReset, {
    delegate$update_import_gui(input, output, session)
  })



  shiny::observeEvent(input$ab.filterSet,{
    delegate$update_filter(input, output, session)
    delegate$filter_data(input, output, session)
    delegate$update_filter_statistics_tbl(input, output, session)
    delegate$update_filter_missings_tbl(input, output, session)
    delegate$update_exploration_gui(input, output, session)
    delegate$reset_loq_values(input, output, session)
    delegate$init_detect_loq(input, output, session)
    delegate$update_loq_upload_gui(input, output, session)
    delegate$update_loq_define_gui(input, output, session)
    delegate$update_loq_detect_gui(input, output, session)
    delegate$update_loq_detect_statistics_graphic(input, output, session)
    delegate$update_loq_detect_attribute_graphic(input, output, session)
    delegate$update_loq_detect_attribute_tbl(input, output, session)
    delegate$update_loq_detect_statistics_tbl(input, output, session)
    delegate$update_loq_detect_outlier_tbl(input, output, session)
    delegate$init_mutate_loq(input, output, session)
    delegate$update_loq_mutate_gui(input, output, session)
    delegate$update_loq_mutate_statistics_graphic(input, output, session)
    delegate$update_loq_mutate_attribute_graphic(input, output, session)
    delegate$init_loq_mutate_attribute_tbl(input, output, session)
    delegate$update_loq_mutate_data_tbl(input, output, session)
    delegate$resetTrafoMutateGui(input, output, session)
    delegate$hide_outdated_results(input, output, session)
    analysisFinished(FALSE)
  })

  shiny::observeEvent(input$ab.filterInvSet,{
    delegate$update_filter_inverse(input, output, session)
    delegate$filter_data(input, output, session)
    delegate$update_filter_statistics_tbl(input, output, session)
    delegate$update_filter_missings_tbl(input, output, session)
    delegate$update_exploration_gui(input, output, session)
    delegate$reset_loq_values(input, output, session)
    delegate$update_loq_upload_gui(input, output, session)
    delegate$update_loq_define_gui(input, output, session)
    delegate$update_loq_detect_gui(input, output, session)
    delegate$hide_outdated_results(input, output, session)
    analysisFinished(FALSE)
  })

  shiny::observeEvent(input$ab.filterReset,{
    delegate$update_filter_select_tbl(input, output, session)
    delegate$reset_filter(input, output, session)
    delegate$update_filter(input, output, session)
    delegate$filter_data(input, output, session)
    delegate$update_filter_statistics_tbl(input, output, session)
    delegate$update_filter_missings_tbl(input, output, session)
    delegate$update_exploration_gui(input, output, session)
    delegate$reset_loq_values(input, output, session)
    delegate$update_loq_upload_gui(input, output, session)
    delegate$update_loq_define_gui(input, output, session)
    delegate$hide_outdated_results(input, output, session)
    analysisFinished(FALSE)
  })

  shiny::observeEvent(input$si.exploreAbs, {
    delegate$update_exploration_abscissa(input, output, session)
    delegate$update_exploration_graphic(input, output, session)
    delegate$update_exploration_abscissa_graphic(input, output, session)
    delegate$update_exploration_abscissa_table(input, output, session)
  })

  shiny::observeEvent(input$si.exploreOrd, {
    delegate$update_exploration_ordinate(input, output, session)
    delegate$update_exploration_graphic(input, output, session)
    delegate$update_exploration_ordinate_graphic(input, output, session)
    delegate$update_exploration_ordinate_table(input, output, session)
  })

  shiny::observeEvent(input$fi.LoqImport, {
    delegate$query_loq(input, output, session)
    delegate$import_loq(input, output, session)
    delegate$update_loq_define_gui(input, output, session)
    delegate$hide_outdated_results(input, output, session)
    analysisFinished(FALSE)
  })

  shiny::observeEvent(input$ab.LoqUploadReset, {
    delegate$reset_loq_values(input, output, session)
    delegate$update_loq_upload_gui(input, output, session)
    delegate$update_loq_define_gui(input, output, session)
    delegate$hide_outdated_results(input, output, session)
    analysisFinished(FALSE)
  })

  shiny::observeEvent(input$si.LoqDefineFeature, {
    delegate$update_loq_define_menu(input, output, session)
  })

  shiny::observeEvent(input$ab.LoqDefineSet, {
    delegate$set_loq_define_values(input, output, session)
    delegate$update_loq_upload_gui(input, output, session)
    delegate$update_loq_define_table(input, output, session)
    delegate$hide_outdated_results(input, output, session)
    analysisFinished(FALSE)
  })

  shiny::observeEvent(input$ab.LoqDefineSetGlobally, {
    delegate$set_loq_define_values_globally(input, output, session)
    delegate$update_loq_upload_gui(input, output, session)
    delegate$update_loq_define_table(input, output, session)
    delegate$hide_outdated_results(input, output, session)
    analysisFinished(FALSE)
  })

  shiny::observeEvent(input$ab.LoqDefineReset, {
    delegate$reset_loq_values(input, output, session)
    delegate$update_loq_upload_gui(input, output, session)
    delegate$update_loq_define_gui(input, output, session)
    delegate$hide_outdated_results(input, output, session)
    analysisFinished(FALSE)
  })

  shiny::observeEvent(input$ab.detectLoq, {
    delegate$detect_loq(input, output, session)
    delegate$update_loq_detect_statistics_graphic(input, output, session)
    delegate$update_loq_detect_attribute_graphic(input, output, session)
    delegate$update_loq_detect_attribute_tbl(input, output, session)
    delegate$update_loq_detect_statistics_tbl(input, output, session)
    delegate$update_loq_detect_outlier_tbl(input, output, session)
    delegate$update_loq_mutate_gui(input, output, session)
    delegate$hide_outdated_results(input, output, session)
    analysisFinished(FALSE)
  })

  shiny::observeEvent(input$si.loqDetectFeature, {
    delegate$update_loq_detect_attribute_graphic(input, output, session)
    delegate$update_loq_detect_attribute_tbl(input, output, session)
  })

  shiny::observeEvent(input$ab.resetDetectLoq, {
    delegate$update_loq_detect_gui(input, output, session)
  })

  shiny::observeEvent(input$ab.mutateLoq, {
    delegate$mutate_loq(input, output, session)
    delegate$update_loq_mutate_statistics_graphic(input, output, session)
    delegate$update_loq_mutate_attribute_graphic(input, output, session)
    delegate$update_loq_mutate_attribute_tbl(input, output, session)
    delegate$update_loq_mutate_data_tbl(input, output, session)
    delegate$resetTrafoMutateGui(input, output, session)
    delegate$hide_outdated_results(input, output, session)
    analysisFinished(FALSE)
  })

  shiny::observeEvent(input$si.loqMutateFeature, {
    delegate$update_loq_mutate_attribute_graphic(input, output, session)
    delegate$update_loq_mutate_attribute_tbl(input, output, session)
  })

  shiny::observeEvent(input$ab.resetMutateLoq, {
    delegate$update_loq_mutate_gui(input, output, session)
  })

  # shiny::observeEvent(input$ab.trafoMutateFit,{
  #   delegate$trafoMutateFit(input, output, session)
  #   delegate$updateTrafoMutateGui(input, output, session)
  #   delegate$updateTrafoMutateFeatureGraphic(input, output, session)
  #   delegate$updateTrafoMutateFeatureParameterTbl(input, output, session)
  #   delegate$updateTrafoMutateFeatureQualityTbl(input, output, session)
  #   delegate$updateTrafoMutateGlobalParameterTbl(input, output, session)
  #   delegate$updateTrafoMutateGlobalModelTbl(input, output, session)
  #   delegate$updateTrafoMutateGlobalQualityTbl(input, output, session)
  #   delegate$updateTrafoMutateGlobalTestsTbl(input, output, session)
  #   delegate$updateTrafoMutateGlobalDataTbl(input, output, session)
  #   delegate$hide_outdated_results(input, output, session)
  #   analysisFinished(FALSE)
  # })

  shiny::observeEvent(input$ab.trafoMutateSetGlobal, {
    delegate$trafoMutateFit(input, output, session)
    delegate$trafoMutateGlobal(input, output, session)
    delegate$updateTrafoMutateGui(input, output, session)
    delegate$updateTrafoMutateFeatureGraphic(input, output, session)
    delegate$updateTrafoMutateFeatureParameterTbl(input, output, session)
    delegate$updateTrafoMutateFeatureQualityTbl(input, output, session)
    delegate$updateTrafoMutateGlobalParameterTbl(input, output, session)
    delegate$updateTrafoMutateGlobalModelTbl(input, output, session)
    delegate$updateTrafoMutateGlobalQualityTbl(input, output, session)
    delegate$updateTrafoMutateGlobalTestsTbl(input, output, session)
    delegate$updateTrafoMutateGlobalDataTbl(input, output, session)
    delegate$updateTrafoNormGui(input, output, session)
    delegate$hide_outdated_results(input, output, session)
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
    delegate$updateTrafoNormGui(input, output, session)
    delegate$hide_outdated_results(input, output, session)
    analysisFinished(FALSE)
  })

  shiny::observeEvent(input$si.trafoMutateFeature, {
    delegate$updateTrafoMutateFeatureGraphic(input, output, session)
    delegate$updateTrafoMutateFeatureParameterTbl(input, output, session)
    delegate$updateTrafoMutateFeatureQualityTbl(input, output, session)
    delegate$updateTrafoMutateGui(input,output, session)
  })

  shiny::observeEvent(input$ab.trafoMutateReset, {
    delegate$updateTrafoMutateGui(input, output, session)
  })

  shiny::observeEvent(input$ab.trafoNormMutate,{
    delegate$trafoNormMutate(input, output, session)
    delegate$updateTrafoNormFeatureGraphic(input, output, session)
    delegate$updateTrafoNormFeatureStatisticsTbl(input, output, session)
    delegate$updateTrafoNormParameterTbl(input, output, session)
    delegate$updateTrafoNormStatisticsTbl(input, output, session)
    delegate$updateTrafoNormDataTbl(input, output, session)
    delegate$hide_outdated_results(input, output, session)
    analysisFinished(FALSE)
  })

  shiny::observeEvent(input$si.imputeNormFeature, {
    delegate$updateTrafoNormFeatureGraphic(input, output, session)
    delegate$updateTrafoNormFeatureStatisticsTbl(input, output, session)
    delegate$resetTrafoNormGui(input,output, session)
  })

  shiny::observeEvent(input$ab.imputeNormReset, {
    delegate$resetTrafoNormGui(input, output, session)
  })

  shiny::observeEvent(input$ab.imputeMissingsDetect, {
    delegate$imputeMissingsAnalyze(input, output, session)
    delegate$updateImputeMissingsGraphic(input, output, session)
    delegate$updateImputeMissingsStatisticsTbl(input, output, session)
    delegate$updateImputeMissingsDistributionTbl(input, output, session)
    delegate$updateImputeMissingCharacteristicsGraphic(input, output, session)
    delegate$updateImputeMissingsCharacteristicsTbl(input, output, session)
    delegate$updateImputeMissingsDetailTbl(input, output, session)
    # delegate$updateImputeMissingsDataTbl(input, output, session)
    delegate$updateImputeOutliersGui(input, output, session)
    delegate$hide_outdated_results(input, output, session)
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
    # delegate$updateImputeOutliersDataTbl(input, output, session)
    delegate$updateImputeMutateGui(input, output, session)
    delegate$hide_outdated_results(input, output, session)
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
    delegate$resetImputeMutateGui(input, output, session)
    delegate$updateImputeFluxGraphic(input, output, session)
    delegate$updateImputeMutateGraphic(input, output, session)
    delegate$updateImputeMutateStatisticsTbl(input, output, session)
    delegate$updateImputeMutateDistributionTbl(input, output, session)
    delegate$updateImputeMutateFeatureDetailGraphic(input, output, session)
    delegate$updateImputeMutateFeatureDetailTbl(input, output, session)
    delegate$updateImputeMutateDetailTbl(input, output, session)
    delegate$updateImputeMutateDataTbl(input, output, session)
    delegate$hide_outdated_results(input, output, session)
    analysisFinished(FALSE)
  })

  shiny::observeEvent(input$ab.validation, {
    delegate$validate(input, output, session)
    delegate$updateAnalysisValidationGui(input, output, session)
    delegate$updateAnalysisValidationGraphic(input, output, session)
    delegate$updateAnalysisValidationTestTbl(input, output, session)
    delegate$updateCentralMomentsOrgTbl(input, output, session)
    delegate$updateCentralMomentsImpTbl(input, output, session)
    delegate$updateCentralMomentsDeltaTbl(input, output, session)
    delegate$updateCorrelationValidationScatterGraphic(input, output, session)
    delegate$updateCorrelationValidationBoxPlotGraphic(input, output, session)
    delegate$updateCorrelationValidationDeviationTbl(input, output, session)
    delegate$updateCorrelationValidationDataTbl(input, output, session)
    delegate$hide_outdated_results(input, output, session)
    analysisFinished(TRUE)
  })

  shiny::observeEvent(input$si.analysisValidationFeature, {
    delegate$updateAnalysisValidationGraphic(input, output, session)
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
      delegate$exportData(input, file)
    }
  )

  output$dbReport <- shiny::downloadHandler(
    filename = function(){
      delegate$reportFileName(input, output, session)
    },
    content = function(file){
      delegate$writeReport(input, file)
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

  shiny::observeEvent(input$switch.help, {
    delegate$update_help_html(input, output, session)
  })
}




