library(shiny);library(shinydashboard);
library(ggplot2);library(ggthemes);
library(reshape2);library(dplyr);
library(stringr);library(tidyr);
library(oro.dicom);library(viridis)
library(Cubist); library(RCurl)
library(rCharts)
source("scripts/dicomDataExtraction.r")
source("scripts/combinePlannedAndDelivered.r")
source("scripts/calculateFeatures.r")
load("models/cubistModel.RData")


server <- function(input, output, session) {
  
  
  #-------DICOM UPLOAD FUNCTIONS-------------#
  PlannedDicom <- reactive({
    planneddicom_in <- input$planneddicom
    if (is.null(planneddicom_in)){
      return(NULL)
    } else {
      planneddicom_in <- readDICOMFile(planneddicom_in$datapath,pixelData=FALSE)
      planneddicom_data <- dicom_data(planneddicom_in)
    }
    planneddicom_data
  })
  DeliveredDicom <- reactive({
    delivereddicom_in <- input$delivereddicom
    if (is.null(delivereddicom_in)){
      return(NULL)
    } else {
      delivereddicom_in <- readDICOMFile(delivereddicom_in$datapath,pixelData=FALSE)
      delivereddicom_data <- dicom_data(delivereddicom_in)
    }
    delivereddicom_data
  })

  #--------=========TAB 1===========---------#
  #---/-/-/-/START INFOBOXES\-\-\-\-\-------------#
  
  output$plannedidbox <- renderInfoBox({
    dat <- PlannedDicom()
    if (is.null(dat)){
      infoBox(
        "Plan ID", " ", icon = icon("list"),
        color = "light-blue")
    } else {
      infoBox(
        "Plan ID", dat[1,1], icon = icon("list"),
        color = "light-blue")
    }
  })
  output$plannedenergybox <- renderInfoBox({
    dat <- PlannedDicom()
    if (is.null(dat)){
      infoBox(
        "Beam Energy", " ", icon = icon("list"),
        color = "light-blue")
    } else {
      infoBox(
        "Beam Energy", dat[1,2], icon = icon("list"),
        color = "light-blue")
    }
  })
  output$planneddosebox <- renderInfoBox({
    dat <- PlannedDicom()
    if (is.null(dat)){
      infoBox(
        "Dose (Gy)", " ", icon = icon("list"),
        color = "light-blue")
    } else {
      infoBox(
        "Dose (Gy)", round(dat[1,5] + dat[nrow(dat), 5],5), icon = icon("list"),
        color = "light-blue")
    }
  })
  output$deliveredidbox <- renderInfoBox({
    dat <- DeliveredDicom()
    if (is.null(dat)){
      infoBox(
        "Plan ID", " ", icon = icon("list"),
        color = "yellow")
    } else {
      infoBox(
        "Plan ID", dat[1,1], icon = icon("list"),
        color = "yellow")
    }
  })
  output$deliveredenergybox <- renderInfoBox({
    dat <- DeliveredDicom()
    if (is.null(dat)){
      infoBox(
        "Beam Energy", " ", icon = icon("list"),
        color = "yellow")
    } else {
      infoBox(
        "Beam Energy", dat[1,2], icon = icon("list"),
        color = "yellow")
    }
  })
  output$delivereddosebox <- renderInfoBox({
    dat <- DeliveredDicom()
    if (is.null(dat)){
      infoBox(
        "Dose (Gy)", " ", icon = icon("list"),
        color = "yellow")
    } else {
      infoBox( 
        "Dose (Gy)", round(dat[1,5] + dat[nrow(dat), 5], 5), icon = icon("list"),
        color = "yellow")
    }
  })
  
  #------\-\-\-\-\END INFOBOXES/-/-/-/-/------#
  
  
  #--------=========TAB 2===========---------#
  
  #------PLAN ERROR FUNCTION--------#
  ErrorDF <- reactive({
    planned <- PlannedDicom()
    delivered <- DeliveredDicom()
    if(is.null(planned) || is.null(delivered)){
      return(NULL)
    } else {
      combined <- combinePlannedAndDelivered(planned, delivered)
      features <- calculateFeatures(combined)
    }
    features
  })
  
  
  output$tab2_plot_errorhist <- renderPlot({
    
    error_data <- ErrorDF()
    if (is.null(error_data)){
      return(NULL)
    } else {
      dat <- filter(error_data, RestStop %in% input$tab2_checkbox_hist)
      if(nrow(dat) == 0){
        return(NULL)
      }
      #glcm_data <- melt(glcm(input$radiomicdata, verbose=FALSE))
      ggplot(data=dat, aes(x=Error, fill=RestStop)) + 
        geom_histogram(binwidth=input$tab2_slider_errorhist_binwidth, position="identity", alpha=0.7) +
        theme_fivethirtyeight() + 
        theme(axis.title=element_text(), axis.title.y=element_blank()) +
        xlab("Leaf Position Error (mm)")+
        scale_fill_viridis(discrete=TRUE, guide = guide_legend(title = NULL))
      
    }
  })
  output$tab2_plot_errordensity <- renderPlot({
    
    error_data <- ErrorDF()
    if (is.null(error_data)){
      return(NULL)
    } else {
      dat <- filter(error_data, RestStop %in% input$tab2_checkbox_hist)
      if(nrow(dat) == 0){
        return(NULL)
      }
      #glcm_data <- melt(glcm(input$radiomicdata, verbose=FALSE))
      ggplot(data=dat, aes(x=abs(Error), fill=RestStop)) + 
        geom_density(alpha=0.65) +
        theme_fivethirtyeight() + 
        theme(axis.title=element_text(), axis.title.y=element_blank()) +
        xlab("Leaf Position Error (mm)") +
        scale_fill_viridis(discrete=TRUE, guide = guide_legend(title = NULL))
      
    }
  })
  #---/-/-/-/START INFOBOXES\-\-\-\-\-------------#
  output$absmeanerror <- renderInfoBox({
    error_data <- ErrorDF()
    if (is.null(error_data)){
      infoBox(
        "Mean Absolute Error (mm)", " ", icon = icon("list"),
        color = "light-blue")
    } else {
      dat <- filter(error_data, RestStop %in% input$tab2_checkbox_hist)
      infoBox(
        "Mean Absolute Error (mm)", round(mean(abs(dat$Error)), 5), icon = icon("list"),
        color = "light-blue")
    }
  })
  output$rmserror <- renderInfoBox({
    error_data <- ErrorDF()
    if (is.null(error_data)){
      infoBox(
        "Root Mean Squared Error (mm)", " ", icon = icon("list"),
        color = "light-blue")
    } else {
      dat <- filter(error_data, RestStop %in% input$tab2_checkbox_hist)
      infoBox(
        "Root Mean Squared Error (mm)", round(sd(dat$Error), 5), icon = icon("list"),
        color = "light-blue")
    }
  })
  output$maxerror <- renderInfoBox({
    error_data <- ErrorDF()
    if (is.null(error_data)){
      infoBox(
        "Maximum Error (mm)", " ", icon = icon("list"),
        color = "light-blue")
    } else {
      dat <- filter(error_data, RestStop %in% input$tab2_checkbox_hist)
      infoBox(
        "Maximum Error (mm)", round(max(abs(dat$Error)), 5), icon = icon("list"),
        color = "light-blue")
    }
  })
  
  
  #--------=========TAB 3===========---------#
  
  pretrainedpreds <- eventReactive(input$pretrainedpredict, {
    dat <- ErrorDF()
    dat$preds <- predict(cubistModel, dat)
    dat$Predicted <- dat$Planned - dat$preds
    dat$PredictionError <- dat$Delivered - dat$Predicted
    dat
  })
  
  output$tab3_plot_mlcpositions <- renderChart2({
    
    error_data <- ErrorDF()
    prediction_data <- try(pretrainedpreds(), silent=TRUE)
    
    #Data selection
    #Goes to prediction data if the predict action button has been clicked
    if (is.null(error_data) ){
      blank_chart <- Highcharts$new()
      blank_chart$set(width = session$clientData$output_plot1_width)
      return(blank_chart)
    } else if (class(prediction_data) != "try-error"){
      error_data <- melt(select(prediction_data, Bank, MLCIndex, CP, Planned, Delivered, Predicted, Error, PredictionError), id.vars = c("Bank", "MLCIndex", "CP"))
      
    } else {
      error_data <- melt(select(error_data, Bank, MLCIndex, CP, Planned, Delivered, Error), id.vars = c("Bank", "MLCIndex", "CP"))
    }
    
    #IF plot type is "Position data"
    if(input$tab3_radio_plottype > 0){
      #settings for prediction data
      if (class(prediction_data) != "try-error"){
        choose_vars <- c("Error", "PredictionError")
        legend_labels <- c("Planned", "Delivered", "Predicted")
        #Non-prediction data  
      } else {
        choose_vars <- c("Error")
        legend_labels <- c("Planned", "Delivered")
      }
      filt_error_data <- filter(error_data, !variable %in% choose_vars)
      ytitle <- "Leaf Position (mm)"
      
      #If plot type is error data
    } else {
      if (class(prediction_data) != "try-error"){
        choose_vars <- c("Error", "PredictionError")
        
        legend_labels <- c("Delivered - Planned", "Delivered - Predicted")
      } else {
        choose_vars <- c("Error")
        legend_labels <- c("Delivered - Planned")
      }
      filt_error_data <- filter(error_data, variable %in% choose_vars)
      filt_error_data$value <- abs(filt_error_data$value)
      ytitle <- "Leaf Error Magnitude (mm)"
      
    }
    
    cplow <- input$tab3_slider_cps[1]
    cphigh <- input$tab3_slider_cps[2]
    if(input$tab3_radio_arc == 2){
      cplow <- cplow+178
      cphigh <- cphigh+178
    }
    
    #ggplot(data=filter(filt_error_data, CP >= cplow & CP <= cphigh,
    #                   MLCIndex == as.character(input$tab3_slider_mlcindex), Bank == input$tab3_radio_bank)) + 
    #  geom_point(aes(x=CP, y=value, col=variable)) +  
    #  theme_fivethirtyeight() + 
    #  theme(axis.title=element_text(), axis.title.y=element_text(angle=90)) +
    #  labs(x="Control Point", y=ytitle) +
    #  scale_color_viridis(discrete=TRUE, guide = guide_legend(title = NULL), labels=legend_labels)
    
    #Alt plot
    chart <- hPlot(value ~ CP, data=filter(filt_error_data, CP >= cplow & CP <= cphigh, 
                                            MLCIndex ==as.character(input$tab3_slider_mlcindex), Bank == input$tab3_radio_bank),
                   group="variable", type="scatter")
    chart$yAxis(title=list(text=ytitle))
    chart$xAxis(title=list(text="Control Point"))
    chart$set(width = session$clientData$output_plot1_width)
    
    return(chart)
    
  })
  
  
  
  output$tab2_plot_diffhist <- renderPlot({
    
    velhist_data <- PlannedDicom()
    if (is.null(velhist_data)){
      return(NULL)
    } else {
      return(NULL)
    }
  })
  
  
  
}