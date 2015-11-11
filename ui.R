
library(shinydashboard)


#==================HEADER==================#
header <- dashboardHeader(title="MLC Prediction")



#=================SIDEBAR==================#
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Plan Upload and Info", tabName = "dashboard", icon = icon("upload")),
    menuItem("Error Visualization", tabName = "errorviz",  icon=icon("bar-chart")),
    menuItem("Predictive Models", icon = icon("magic"),
             menuSubItem("Pre-trained Model", tabName = "pretrainedmodel"),
             menuSubItem("Custom Model", tabName = "custommodel")
    )
  )
)



#==================BODY====================#
body <- dashboardBody(
  tabItems(
    
    #-----------------PLAN UPLOAD AND INFO TAB------------------#
    tabItem(tabName = "dashboard",
            
            fluidRow(
              column(width=12,
                  h3("MLC Positional Error Prediction"),
                  h6("You may upload your planned and delivered DICOM-RT QA files on this page.
                     The upload process will complete automatically, and plan statistics will be 
                     displayed after a slight delay due to calculation time. Navigate to
                     other tabs to see error predictions and exploratory plots."),
                  h6("If you don't have an appropriate pair of DICOM-RT files, but still wish
                     to explore the app, you can download a sample pair of files ", a("here (Planned)", href="https://github.com/joelcarlson/MLCPredsShinyDash/raw/master/trainPlanned.dcm"), "and ", a("here (Delivered).", href="https://github.com/joelcarlson/MLCPredsShinyDash/raw/master/trainDelivered.dcm"))
                
              )
            ),
            #UPLOAD SLOTS
            fluidRow(
              box(status= "primary", fileInput("planneddicom", label=h6("Select Planned DICOM-RT (*.dcm)"), multiple = FALSE, accept = NULL)),
              box(status= "warning", fileInput("delivereddicom", label=h6("Select Delivered DICOM-RT (*.dcm)"), multiple = FALSE, accept = NULL))
            ),
            
            #INFO BOXES
            fluidRow(
              #PLANNED BOXES
              infoBoxOutput("plannedidbox", width=6),
              #DELIVERED BOXES
              infoBoxOutput("deliveredidbox", width=6)
            ),
            fluidRow(
              #PLANNED BOXES
              infoBoxOutput("plannedenergybox", width=6),
              #DELIVERED BOXES
              infoBoxOutput("deliveredenergybox", width=6)
            ),
            fluidRow(
              #PLANNED BOXES
              infoBoxOutput("planneddosebox", width=6),
              #DELIVERED BOXES
              infoBoxOutput("delivereddosebox", width=6)
            )
            
            ),
    
    
    
    #---------------ERROR VIZ TAB---------------#
    tabItem(tabName = "errorviz",
            
            h3("Visualizations of the Errors in your plan"),
            h6("If there is nothing displayed on this page make sure you have uploaded both a planned and delivered DICOM-RT."),
            
            #---PLOT COLUMN---#
            column(width=6,
                   fluidRow(
                     tabBox(title = "Plots", id = "tab2plots", width=12,
                            tabPanel("Histogram of Errors",plotOutput("tab2_plot_errorhist", height=400), width = 12),
                            tabPanel("Error Density",  plotOutput("tab2_plot_errordensity", height=400), width = 12)
                            
                     )),
                   fluidRow(
                     box(width=12,
                         checkboxGroupInput("tab2_checkbox_hist", "MLC Movement Category:", inline=TRUE,
                                            c("Rest"="Rest", "Moving"="Moving" , "Single CP"="Single CP",
                                              "Starting"="Starting",  "Stopping"="Stopping" ),
                                            selected = c("Moving", "Single CP", "Starting", "Stopping")),
                         sliderInput("tab2_slider_errorhist_binwidth", "Histogram Binwidth:", 0.005, 0.20, 0.15)
                     )
                   )
            ),
            #---STATS COLUMN---#
            column(width=6,
                   fluidRow(
                     #MEAN ERROR
                     infoBoxOutput("absmeanerror", width=12),
                     
                     infoBoxOutput("rmserror", width=12),
                     
                     infoBoxOutput("maxerror", width=12)
                   )
            )
    ),
    
    
    
    
    #---------------MODEL BUILDING TAB---------------#
    tabItem(tabName = "pretrainedmodel",
            fluidRow(
              box(width=9,
                  
                  h3("Model Building with a Pre-trained Model"),
                  h6("Included in this dashboard is a model trained on the example data provided in the 'Plan Upload and Info'
                     tab. By clicking the 'Predict' button, the model will take position data from the 'planned' DICOM,
                     and use this information to predict the 'delivered' positions.")
                  ),
              box(width=3,
                  h6("Pressing this button will initiate the pre-trained model and make predictions on the uploaded 'planned' DICOM."),
                  actionButton("pretrainedpredict", "Predict")
              )
                  ),
            
            column(width=7,
                   fluidRow(
                     box(width = 12, height = 450, solidHeader = FALSE, status = "primary",
                         #This mess is to make sure that the plot doesn't run over the box width. ugly.
                         plotOutput("plot1", height = "1px"),
                         showOutput("tab3_plot_mlcpositions","highcharts")
                     )
                     #box(title = "MLC Positions by Control Point", showOutput("tab3_plot_mlcpositions", "highcharts"), width = 12)
                   )
            ),
            column(width=5,
                   fluidRow(
                     box(title = "Plot Controls", width=12,
                         sliderInput("tab3_slider_cps", "Control Point Range", min = 1, max = 178, value = c(120, 130)),
                         sliderInput("tab3_slider_mlcindex", "MLC Index:", 1, 60, 30)
                     )
                   ),
                   fluidRow(
                     box(width=6,
                         radioButtons("tab3_radio_bank", "Leaf Bank", choices = list("A" = "A", "B" = "B"), inline=TRUE)
                     ),
                     box(width=6,
                         radioButtons("tab3_radio_arc", "Arc", choices = list("1"=1, "2"=2), inline=TRUE)
                     )
                   ),
                   fluidRow(
                     box(width=12,
                         radioButtons("tab3_radio_plottype", "Plot Type", choices = list("Position" = 1, "Error Magnitude"=0), inline=TRUE)
                     )
                   )
            )
            
            
    ),
    tabItem(tabName = "custommodel",
            h3("Model Building"),
            h6("On this page we will train a predictive model."),
            fluidRow(
              
              fileInput("planneddicom", label=h6("Select File (DICOM-RT)"), multiple = FALSE, accept = NULL)
            ),
            fluidRow(
              h6("This function not yet implemented. Check back later!")
            )
    )
  )
  )


##############################
######### BUILD UI ###########       
##############################
ui <- dashboardPage( 
  skin = "black",
  header,                    #
  sidebar,                   #
  body                       #
)                            #
##############################