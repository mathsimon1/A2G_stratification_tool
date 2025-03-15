library(shinydashboard)
library(shiny)
library(shinythemes)
library(data.table)
library(RCurl)
library(shinyWidgets)
library(magick)
library(shinybusy)
library(shiny)
library(shinyFiles)
library(fs)
library(shinyAce)
library(mailR)
library(rmarkdown)
library(officer) 


##### UI
ui <- fluidPage(theme = shinytheme("slate"),
                add_busy_spinner(spin = "fading-circle"),
                navbarPage("AllTogether Stratification Algorythm:",
                            
                           tabPanel("Global stratification",
                                    # Input values
                                    
                                    sidebarPanel(
                                      HTML("<h3>Input parameters</h3>"),
                                      selectInput("ALL_subtype", label = "ALL subtype:", 
                                                  choices = list("B-ALL" = "B-ALL", "T-ALL" = "T-ALL"), selected = "B-ALL"),
                                    
                                     
                                      
                                      # SelectInput for Oncogenetic classification
                                      selectInput(
                                        inputId = "Oncogenetic",
                                        label = "Oncogenetic",
                                        choices = list(
                                          "ETV6-RUNX1" = "ETV6-RUNX1", 
                                          "High Hyperdiploidy (51-67 chromosomes)" = "High Hyperdiploidy",
                                          "B-other" = "B-other",
                                          "ABL class fusions" = "ABL_class_fusions",
                                          "Hypodiploidy (30-39 chromosomes)" = "Hypodiploidy",
                                          "Near Haploidy (<30 chromosomes)" = "near Haploidy",
                                          "iAMP21" = "iAMP21",
                                          "KMT2Ar" = "KMT2A",
                                          "t(17;19)" = "t(17;19)"
                                        ),
                                        width = '400px'
                                      ),
                                      
                                      # Radio buttons for HR cytogenetics
                                      # Radio buttons for HR cytogenetics (cannot be changed by user)

                                    
                                      
                                      sliderInput("Age_at_diagnosis", "Age at diagnosis:",
                                                  min = 1, max = 18,
                                                  value = 5),
                                     
                                      conditionalPanel(
                                        condition = "input.ALL_subtype == 'B-ALL'",
                                        selectInput("WBC_at_diagnosis", label = "WBC at diagnosis:", 
                                                    choices = list("≥50 G/L" = "≥50 G/L", "<50 G/L" = "<50 G/L"),selected = "<50 G/L")), 
                                      
                                      selectInput("CNS_status", label = "CNS status:", 
                                                  choices = list("CNS1 or TLP-" = "CNS1 or TLP-",
                                                                 "CNS2" = "CNS2",
                                                                 "CNS3 or TLP+WBC>5" = "CNS3 or TLP+WBC>5",
                                                                 "TLP+WBC≤5" = "TLP+WBC≤5"
                                                                 ), 
                                                  selected = "CNS1 or TLP-"),
                                      
                            
                                      
                                      
                                      conditionalPanel(
                                        condition = "input.Oncogenetic == 'B-other'",
                                      awesomeRadio( "IKZF1", "IKZF1:",
                                                     choices = c("del" = "IKZF1_del", "not_del" = "IKZF1_not_del"),inline = TRUE,selected = "IKZF1_not_del",    status = "danger"), 
                                      awesomeRadio( "ETV6", "ETV6:",
                                                    choices = c("del" = "ETV6_del", "not_del" = "ETV6_not_del"),inline = TRUE,  selected = "ETV6_not_del",  status = "danger"), 
                                      awesomeRadio( "BTG1", "BTG1:",
                                                    choices = c("del" = "BTG1_del", "not_del" = "BTG1_not_del"),inline = TRUE, selected = "BTG1_not_del",   status = "danger"),      
                                      awesomeRadio( "PAX5", "PAX5:",
                                                    choices = c("del" = "PAX5_del", "not_del" = "PAX5_not_del"),inline = TRUE, selected = "PAX5_not_del",   status = "danger"), 
                                      awesomeRadio( "CDKN2A/B", "CDKN2A/B:",
                                                    choices = c("del" = "CDKN2A/B_del", "not_del" = "CDKN2A/B_not_del"),inline = TRUE,  selected = "CDKN2A/B_not_del",  status = "danger"), 
                                      awesomeRadio( "EBF1", "EBF1:",
                                                    choices = c("del" = "EBF1_del", "not_del" = "EBF1_not_del"),inline = TRUE, selected = "EBF1_not_del",   status = "danger"), 
                                      awesomeRadio( "RB1", "RB1:",
                                                    choices = c("del" = "RB1_del", "not_del" = "RB1_not_del"),inline = TRUE, selected = "RB1_not_del",   status = "danger"), 
                                      awesomeRadio( "PAR1", "PAR1:",
                                                    choices = c("del" = "PAR1_del", "not_del" = "PAR1_not_del"),inline = TRUE,  selected = "PAR1_not_del",  status = "danger")), 
                                      
                                
                                      conditionalPanel(
                                        condition = "input.Oncogenetic == 'B-other'",
                                      radioGroupButtons(
                                        inputId = "CNA_profile",
                                        label = "CNA_profile (conclusion)",
                                        choices = c("CNA good risk", "CNA poor risk"),
                                        status = "primary",
                                        checkIcon = list(
                                          yes = icon("ok", 
                                                     lib = "glyphicon"),
                                          no = icon("remove",
                                                    lib = "glyphicon"))
                                      )),
                                      
                                      
                                      
                                      numericInput("MRD TP1", "MRD TP1 en % (10-4 = 0.01%)", 0.05, min = 0, max = 10, step = 0.005,
                                                   width = NULL),
                                
                
                                      
                                      radioButtons(
                                        inputId = "MRD TP2",
                                        label = "MRD TP2", 
                                        choices = c("undetectable", "pos ≥0.01% and <0.05%", "≥0.05%" )
                                      ),
                                      
                                      
                                      actionButton("submitbutton", 
                                                   "Submit", 
                                                   class = "btn btn-primary"),
                                      
                                     
                                    
                                      br(),
                                      br(),
                                    checkboxGroupInput("selected_images", 
                                                       "Select Protocol phase to download:",
                                                       choices = c("Induction A" = "www/induction_A_v2.png",
                                                                   "Induction B" = "www/induction_B.png", 
                                                                   "Induction C" = "www/induction_C.png",
                                                                   "Induction F" = "www/induction_F.png",
                                                                   "Consolidation SR" ="www/conso_SR.png", 
                                                                   "Consolidation IR low" = "www/IR_low_conso.png",
                                                                   "Consolidation IR High" = "www/IR_high_conso.png",
                                                                   "Delayed Intensification SR" = "www/di_SR.png",
                                                                   "Delayed Intensification IR Low" = "www/di_IRlow.png",
                                                                   "Delayed Intensification IR High" = "www/di_IRhigh.png",
                                                                   "Consolidation 3 IR Low" = "www/conso3_IR_Low.png",
                                                                   "Consolidation 3 IR High" = "www/conso3_IR_High.png",
                                                                   "Maintenance SR" = "www/maintenance_SR.png",
                                                                   "Maintenance IR Low" = "www/maintenance_IR_Low.png",
                                                                   "Maintenance IR High" = "www/maintenance_IR_High.png")),
                                                       
                                                       
                                                       
                                                     
                                    
                                     
                                    # Button to trigger download
                                    downloadButton("download_ppt", "Download PowerPoint")),
                                    
                                    mainPanel(
                                      img(src = "trousseau.png", height = 100, width = 200),
                                      br(),
                                      tags$label(h4('Version dec 2024')),
                                      br(),
                                      radioGroupButtons(
                                        inputId = "HR_cytogenetics",
                                        label = "Cytogenetics (HR or not HR)",
                                        choices = c("Not HR cytogenetics"),  # Default choice
                                        selected = "Not HR cytogenetics",
                                        status = "primary",
                                        checkIcon = list(
                                          yes = icon("ok", lib = "glyphicon"),
                                          no = icon("remove", lib = "glyphicon")
                                        )
                                      ),
                                      
                                      
                                      verbatimTextOutput("result"),
                                      tags$label(h4('Induction Stratification')), # Status/Output Text Box
                                      verbatimTextOutput('induction_result'),
                                      br(),
                                      conditionalPanel(condition = "input.Oncogenetic == 'B-other'",
                                      tags$label(h4('CNA stratification')), 
                                      verbatimTextOutput('CNA_stratification')),
                                      br(),
                                      tags$label(h4('MRD TP1 Stratification')),
                                      verbatimTextOutput('contents_TP1'),
                                      br(),
                                      tags$label(h4('MRD TP2 Stratification')),
                                      verbatimTextOutput('contents_TP2'),

                                    
    
                                      ) # mainPanel()
                                    
                           ),
                           
                           tabPanel("Overview protocol",
                                    # Input values
                                    mainPanel(
                                      tags$label(h4('Schema')),
                                      br(),
                                      box( tags$img(height = 500, width = 1000,src ="schema.png")), 
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),   
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                    tags$label(h4('BCP-ALL')),
                                      br(),
                                      box( tags$img(height = 500, width = 1000,src ="oveview_B.png")),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      tags$label(h4('T-ALL')),
                                      br(),
                                      box( tags$img(height = 500, width = 1000,src ="oveview_T.png")),
                                    
                                    tags$label(h4('T-ALL')),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    tags$label(h4('T-ALL')),
                                    br(),
                                    br(),
                                    box( tags$img(height =700, width = 650,src ="T_ALL_v2.png"))                                 
                                    
                                    
                                    )),
                           
                      
                           
                           tabPanel("CNS stratification",
                                    # Input values
                                    mainPanel(
                                      tags$label(h4('CNS stratification')),
                                      br(),
                                      box( tags$img(height = 500, width = 800,src ="CNS.png")))), 
                           
                           tabPanel("CNA stratification",
                                    # Input values
                                    mainPanel(
                                      tags$label(h4('CNA stratification')),
                                      br(),
                                      box( tags$img(height = 500, width = 800,src ="CNA_profile.png")))),
                           
                           tabPanel("!! Warning !!",
                                    # Input values
                                    mainPanel(
                                      tags$label(h4("Ce site est un outil d'aide à la stratification thérapeutique dans le protocole ALLTogether, en aucun cas 
                                                    il ne peut se substituer au protocole original 
                                                    ALLTogether qui devra être consulté avant toute décision.")),
                                      tags$label(h5("Dr. Mathieu Simonin - Service d'hématologie - Hôpital Trousseau")),
                                      tags$label(h6("- mathieu.simonin@aphp.fr")))),
                           #tabPanel(), About
                           
                ) # navbarPage()
) # fluidPage()


####################################
# Server                           #
####################################
server <- function(input, output, session) {
  
  # Observe changes in selectInput and update radioGroupButtons dynamically
  observeEvent(input$Oncogenetic, {
    
    # Define oncogenetic subtypes that are HR cytogenetics
    HR_subtypes <- c("Hypodiploidy", "near Haploidy", "iAMP21", "KMT2A", "t(17;19)")
    
    if (input$Oncogenetic %in% HR_subtypes) {
      # Force "HR cytogenetics" and disable user modification
      updateRadioGroupButtons(session, "HR_cytogenetics", choices = c("HR cytogenetics"), selected = "HR cytogenetics")
    } else {
      # Force "Not HR cytogenetics" and disable user modification
      updateRadioGroupButtons(session, "HR_cytogenetics", choices = c("Not HR cytogenetics"), selected = "Not HR cytogenetics")
    }
  })
  
  output$result <- renderPrint({
    paste(input$Oncogenetic, ": ", input$HR_cytogenetics)
  })

  
  
  
  # Image Induction
  selected_image_induction <- reactive({
    if (input$Oncogenetic == "Hypodiploidy (30-39 chromosomes)"& input$submitbutton>0) { return("www/induction_C.png") }
    else if (input$Oncogenetic  == "ABL_class_fusions" & input$submitbutton>0)     {    return("www/induction_C.png") }
    else if (input$Oncogenetic  == "near Haploidy " & input$submitbutton>0)     {    return("www/induction_C.png") }   
    else if (input$Oncogenetic  == "iAMP21" & input$submitbutton>0)     {    return("www/induction_C.png") }   
    else if (input$Oncogenetic  == "KMT2Ar" & input$submitbutton>0)     {    return("www/induction_C.png") } 
    else if (input$Oncogenetic  == "t(17;19)" & input$submitbutton>0)     {    return("www/induction_C.png") } 
    else if (input$Oncogenetic  == "Hypodiploidy" & input$submitbutton>0)     {    return("www/induction_C.png") }  
    
    else if (input$Oncogenetic  == "ABL_class_fusions" & input$submitbutton>0) {return("www/induction_F.png") }
    else if (input$ALL_subtype == "B-ALL" & input$Age_at_diagnosis <10 & input$WBC_at_diagnosis == "<50 G/L"  & input$submitbutton>0) { 
      return("www/induction_A_v2.png") } 
    else if (input$ALL_subtype == "B-ALL" & input$Age_at_diagnosis >=10 | input$WBC_at_diagnosis == "≥50 G/L"& input$submitbutton>0) { 
      return("www/induction_B.png") } 
    else if (input$ALL_subtype == "T-ALL"& input$submitbutton>0) {return("www/induction_B.png") }
   
    else {return ("www/induction_A_v2.png") } 
  })

  # Render the image
  output$output_image_induction <- renderImage({
    if (!is.null(selected_image_induction())) {
      list(
        src = selected_image_induction(),
        contentType = "png",  # Adjust the content type based on your image format
        width = "100%"  # Adjust the width as needed
      )
    }  
    else {
      return(NULL)  # Return NULL if no category is selected
    }
  }, deleteFile = FALSE)
 
  
  # Image post TP2
  selected_image_consolidation <- reactive({
    
    if (input$`MRD TP1` >= 5) {isolate("")}   
    else if (input$Oncogenetic == "t(17;19)") {isolate("www/conso_HR.png")}
    else if (input$`MRD TP2` == "≥0.05%") { isolate("www/conso_HR.png")}     
    else if (input$HR_cytogenetics == "HR cytogenetics") { isolate("www/IR_high_conso.png")}
    else if (input$CNS_status == "CNS3 or TLP+WBC>5") { isolate("www/IR_high_conso.png")} 
    else if (input$HR_cytogenetics == "Not HR cytogenetics" & input$`MRD TP1` == 0 & input$ALL_subtype == "B-ALL" & input$CNS_status != "TLP+WBC≤5") { return("www/conso_SR.png")}
    
    else if (input$Oncogenetic == "ETV6-RUNX1" & input$`MRD TP1` < 0.1& input$Age_at_diagnosis <16) {return("www/IR_low_conso.png") }
    else if (input$Oncogenetic == "ETV6-RUNX1" & input$`MRD TP1` >= 0.1) {return("www/IR_high_conso.png") }
    
    else if (input$Oncogenetic == "High Hyperdiploidy" & input$`MRD TP1` < 0.03& input$Age_at_diagnosis <16) {return("www/IR_low_conso.png") }    
    else if (input$Oncogenetic == "High Hyperdiploidy" & input$`MRD TP1` >= 0.03) {return("www/IR_high_conso.png") } 
    
    else if (input$ALL_subtype == "T-ALL" &  input$`MRD TP2` != "undetectable") { return("www/IR_high_conso.png") } 
    else if (input$ALL_subtype == "T-ALL" &  input$`MRD TP2` == "undetectable"& input$Age_at_diagnosis <16) { return("www/IR_low_conso.png") }
    
    else if (input$Oncogenetic == "B-other" & input$CNA_profile == "CNA good risk" & input$`MRD TP1` < 0.05 & input$Age_at_diagnosis <16) { return("www/IR_low_conso.png") }  
    else if (input$Oncogenetic == "B-other" & input$CNA_profile == "CNA good risk" & input$`MRD TP1` >= 0.05 ) { return("www/IR_high_conso.png") }    
    
    else if (input$Oncogenetic == "B-other" & input$CNA_profile == "CNA poor risk" & input$`MRD TP1` >= 0 ) { return("www/IR_high_conso.png") }      
    
    else {return("www/IR_high_conso.png")}
  })
  
  # Render the image
  output$output_image_consolidation <- renderImage({
    if (!is.null(selected_image_consolidation())) {
      list(
        src = selected_image_consolidation(),
        contentType = "png",  # Adjust the content type based on your image format
        width = "100%"  # Adjust the width as needed
      )
    }  
    else {
      return(NULL)  # Return NULL if no category is selected
    }
  }, deleteFile = FALSE) 
  
  # Image DI
  selected_image_DI <- reactive({
    
    if (input$`MRD TP1` >= 5) {isolate("www/di_IRhigh.png")}   
    else if (input$Oncogenetic == "t(17;19)") {isolate("www/di_HR.png")}
    else if (input$`MRD TP2` == "≥0.05%") { isolate("www/di_HR.png")}     
    else if (input$HR_cytogenetics == "HR cytogenetics") { isolate("www/di_IRhigh.png")} 
    else if (input$CNS_status == "CNS3 or TLP+WBC>5") { isolate("www/di_IRhigh.png")} 
    else if (input$HR_cytogenetics == "Not HR cytogenetics" & input$`MRD TP1` == 0 & input$ALL_subtype == "B-ALL" & input$CNS_status != "TLP+WBC≤5") { return("www/di_SR.png")}
    
    else if (input$Oncogenetic == "ETV6-RUNX1" & input$`MRD TP1` < 0.1& input$Age_at_diagnosis <16) {return("www/di_IRlow.png") }
    else if (input$Oncogenetic == "ETV6-RUNX1" & input$`MRD TP1` >= 0.1) {return("www/di_IRhigh.png") }
    
    else if (input$Oncogenetic == "High Hyperdiploidy" & input$`MRD TP1` < 0.03& input$Age_at_diagnosis <16) {return("www/di_IRlow.png") }    
    else if (input$Oncogenetic == "High Hyperdiploidy" & input$`MRD TP1` >= 0.03) {return("www/di_IRhigh.png") } 
    
    else if (input$ALL_subtype == "T-ALL" &  input$`MRD TP2` != "undetectable") { return("www/di_IRhigh.png") } 
    else if (input$ALL_subtype == "T-ALL" &  input$`MRD TP2` == "undetectable"& input$Age_at_diagnosis <16) { return("www/di_IRlow.png") }
    
    
    else if (input$Oncogenetic == "B-other" & input$CNA_profile == "CNA good risk" & input$`MRD TP1` < 0.05 & input$Age_at_diagnosis <16) { return("www/di_IRlow.png") }  
    else if (input$Oncogenetic == "B-other" & input$CNA_profile == "CNA good risk" & input$`MRD TP1` >= 0.05 ) { return("www/di_IRhigh.png") }    
    
    else if (input$Oncogenetic == "B-other" & input$CNA_profile == "CNA poor risk" & input$`MRD TP1` >= 0 ) { return("www/di_IRhigh.png") }      
    
    else {return("www/di_IRhigh.png")}
  })
  
  # Render the image
  output$output_image_DI <- renderImage({
    if (!is.null(selected_image_DI())) {
      list(
        src = selected_image_DI(),
        contentType = "png",  # Adjust the content type based on your image format
        width = "100%"  # Adjust the width as needed
      )
    }  
    else {
      return(NULL)  # Return NULL if no category is selected
    }
  }, deleteFile = FALSE) 
  
 
  # Image conso3
  selected_image_conso3 <- reactive({
    
    if (input$`MRD TP1` >= 5) {isolate("www/conso3_IR_High.png")}   
    else if (input$Oncogenetic == "t(17;19)") {isolate("www/conso3_IR_High.png")}
    else if (input$`MRD TP2` == "≥0.05%") { isolate("www/conso3_IR_High.png")}     
    else if (input$HR_cytogenetics == "HR cytogenetics") { isolate("www/conso3_IR_High.png")} 
    else if (input$CNS_status == "CNS3 or TLP+WBC>5") { isolate("www/conso3_IR_High.png")} 
    else if (input$HR_cytogenetics == "Not HR cytogenetics" & input$`MRD TP1` == 0 & input$ALL_subtype == "B-ALL" & input$CNS_status != "TLP+WBC≤5") { return("www/conso3_SR.png")}
    else if (input$Oncogenetic == "ETV6-RUNX1" & input$`MRD TP1` < 0.1& input$Age_at_diagnosis <16) {return("www/conso3_IR_Low.png") }
    else if (input$Oncogenetic == "ETV6-RUNX1" & input$`MRD TP1` >= 0.1) {return("www/conso3_IR_High.png") }
    
    else if (input$Oncogenetic == "High Hyperdiploidy" & input$`MRD TP1` < 0.03& input$Age_at_diagnosis <16) {return("www/conso3_IR_Low.png") }    
    else if (input$Oncogenetic == "High Hyperdiploidy" & input$`MRD TP1` >= 0.03) {return("www/conso3_IR_High.png") } 
    
    else if (input$ALL_subtype == "T-ALL" &  input$`MRD TP2` != "undetectable") { return("www/conso3_IR_High.png") } 
    else if (input$ALL_subtype == "T-ALL" &  input$`MRD TP2` == "undetectable"& input$Age_at_diagnosis <16) { return("www/conso3_IR_Low.png") }
    
    
    else if (input$Oncogenetic == "B-other" & input$CNA_profile == "CNA good risk" & input$`MRD TP1` < 0.05 & input$Age_at_diagnosis <16) { return("www/conso3_IR_Low.png") }  
    else if (input$Oncogenetic == "B-other" & input$CNA_profile == "CNA good risk" & input$`MRD TP1` >= 0.05 ) { return("www/conso3_IR_High.png") }    
    
    else if (input$Oncogenetic == "B-other" & input$CNA_profile == "CNA poor risk" & input$`MRD TP1` >= 0 ) { return("www/conso3_IR_High.png") }      
    
    else {return("www/conso3_IR_High.png")}
  })
  
  
  # Render the image
  output$output_image_conso3 <- renderImage({
    if (!is.null( selected_image_conso3())) {
      list(
        src = selected_image_conso3(),
        contentType = "png",  # Adjust the content type based on your image format
        width = "100%"  # Adjust the width as needed
      )
    }  
    else {
      return(NULL)  # Return NULL if no category is selected
    }
  }, deleteFile = FALSE) 
  
  

  # Image Maintenance
  selected_image_maintenance <- reactive({
    
    if (input$`MRD TP1` >= 5) {isolate("www/maintenance_IR_High.png")}   
    else if (input$Oncogenetic == "t(17;19)") {isolate("www/maintenance_IR_High.png")}
    else if (input$`MRD TP2` == "≥0.05%") { isolate("www/maintenance_IR_High.png")}     
    else if (input$HR_cytogenetics == "HR cytogenetics") { isolate("www/maintenance_IR_High.png")} 
    else if (input$CNS_status == "CNS3 or TLP+WBC>5") { isolate("www/maintenance_IR_High.png")} 
    else if (input$HR_cytogenetics == "Not HR cytogenetics" & input$`MRD TP1` == 0 & input$ALL_subtype == "B-ALL" & input$CNS_status != "TLP+WBC≤5") { return("www/maintenance_SR.png")}
    else if (input$Oncogenetic == "ETV6-RUNX1" & input$`MRD TP1` < 0.1& input$Age_at_diagnosis <16) {return("www/maintenance_IR_Low.png") }
    else if (input$Oncogenetic == "ETV6-RUNX1" & input$`MRD TP1` >= 0.1) {return("www/maintenance_IR_High.png") }
    
    else if (input$Oncogenetic == "High Hyperdiploidy" & input$`MRD TP1` < 0.03& input$Age_at_diagnosis <16) {return("www/maintenance_IR_Low.png") }    
    else if (input$Oncogenetic == "High Hyperdiploidy" & input$`MRD TP1` >= 0.03) {return("www/maintenance_IR_High.png") } 
    
    else if (input$ALL_subtype == "T-ALL" &  input$`MRD TP2` != "undetectable") { return("www/maintenance_IR_High.png") } 
    else if (input$ALL_subtype == "T-ALL" &  input$`MRD TP2` == "undetectable"& input$Age_at_diagnosis <16) { return("www/maintenance_IR_Low.png") }
    
    
    else if (input$Oncogenetic == "B-other" & input$CNA_profile == "CNA good risk" & input$`MRD TP1` < 0.05 & input$Age_at_diagnosis <16) { return("www/maintenance_IR_Low.png") }  
    else if (input$Oncogenetic == "B-other" & input$CNA_profile == "CNA good risk" & input$`MRD TP1` >= 0.05 ) { return("www/maintenance_IR_High.png") }    
    
    else if (input$Oncogenetic == "B-other" & input$CNA_profile == "CNA poor risk" & input$`MRD TP1` >= 0 ) { return("www/maintenance_IR_High.png") }      
    
    else {return("www/maintenance_IR_High.png")}
  })
  
  
  # Render the image
  output$output_image_maintenance <- renderImage({
    if (!is.null( selected_image_maintenance())) {
      list(
        src = selected_image_maintenance (),
        contentType = "png",  # Adjust the content type based on your image format
        width = "100%"  # Adjust the width as needed
      )
    }  
    else {
      return(NULL)  # Return NULL if no category is selected
    }
  }, deleteFile = FALSE) 
   
  # Input Data
  datasetInput <- 

    # cytogenetic_result
    output$cytogenetic_result <- renderPrint({
      
      if (input$submitbutton == 0) { 
        isolate("en attente") }
      else
        if (input$Oncogenetic == ""& input$submitbutton>0) { 
          isolate("Induction C (HR cytogenetic)") }
      else if (input$ALL_subtype == "B-ALL" & input$Age_at_diagnosis <10 & input$WBC_at_diagnosis == "<50 G/L" & input$HR_cytogenetics == "Not HR cytogenetics" & input$submitbutton>0) { 
        isolate("Induction A (B-ALL NCI-SR: <10y and <50G/L)") 
      }
      else 
        if (input$ALL_subtype == "B-ALL" & input$Age_at_diagnosis >=10 | input$WBC_at_diagnosis == "≥50 G/L"& input$submitbutton>0) { 
          isolate("Induction B (B-ALL and NCI HR: ≥10y or ≥50G/L)") 
        }
      else if (input$ALL_subtype == "T-ALL"& input$submitbutton>0) { 
        isolate("Induction B (T-ALL)") }
      else {return("en attente")
      }
    })
    
    
        
    # induction stratification 
    output$induction_result <- renderPrint({

      if (input$submitbutton == 0) { 
        isolate("en attente") }
      else if (input$HR_cytogenetics == "HR cytogenetics"& input$submitbutton>0) {  isolate("Induction C (HR cytogenetic)") }
      else if (input$Oncogenetic  == "near Haploidy " & input$submitbutton>0)    {  isolate("Induction C (HR cytogenetic)") }  
      else if (input$Oncogenetic  == "iAMP21" & input$submitbutton>0)    {  isolate("Induction C (HR cytogenetic)") }
      else if (input$Oncogenetic  == "KMT2Ar" & input$submitbutton>0)    {  isolate("Induction C (HR cytogenetic)") }
      else if (input$Oncogenetic  == "t(17;19)" & input$submitbutton>0)    {  isolate("Induction C (HR cytogenetic)") }
      else if (input$Oncogenetic  == "Hypodiploidy" & input$submitbutton>0)    {  isolate("Induction C (HR cytogenetic)") }
      
      else if (input$Oncogenetic  == "ABL_class_fusions" & input$submitbutton>0) {isolate("Induction F (ABL class fusion)") }
      else if (input$ALL_subtype == "B-ALL" & input$Age_at_diagnosis <10 & input$WBC_at_diagnosis == "<50 G/L" & input$HR_cytogenetics == "Not HR cytogenetics" & input$submitbutton>0) { 
        isolate("Induction A (B-ALL NCI-SR: <10y and <50G/L)") 
      }
      else 
        if (input$ALL_subtype == "B-ALL" & input$Age_at_diagnosis >=10 | input$WBC_at_diagnosis == "≥50 G/L"& input$submitbutton>0) { 
          isolate("Induction B (B-ALL and NCI HR: ≥10y or ≥50G/L)") 
        }
      else if (input$ALL_subtype == "T-ALL"& input$submitbutton>0) { 
          isolate("Induction B (T-ALL)") }
      else {return("en attente")
      }
    })
  
  # TP1 stratification   
  output$contents_TP1 <- renderPrint({
    
    if (input$submitbutton == 0) { 
      isolate("en attente") }
    else if (input$`MRD TP1` >= 5) {isolate("High Risk (HR) MRD TP1 ≥5% Check protocol")}   
    else if (input$Oncogenetic == "t(17;19)") {isolate("High Risk (HR) t(17;19) Check protocol")} 
    else if (input$CNS_status == "CNS3 or TLP+WBC>5") { isolate("Intermediate Risk (IR) (CNS3 or TLP+WBC>5)")} 
else if (input$HR_cytogenetics == "Not HR cytogenetics" & input$`MRD TP1` == 0 & input$ALL_subtype == "B-ALL" & input$CNS_status != "TLP+WBC≤5" ) { 
      isolate("Standard Risk (SR) B-ALL - MRD TP1 undetectable - not HR cytogenetics - CNS1/CNS2/TLP-")}
else {
      return("Intermediate Risk (IR)")
    }
  })   
  
  # CNA stratification
  output$CNA_stratification <- renderPrint({
    
    if (input$submitbutton == 0) {isolate("en attente") }
    
    else if (  input$submitbutton>0 
               & input$ETV6 == "ETV6_not_del" 
               & input$`CDKN2A/B` == "CDKN2A/B_not_del"
               & input$RB1 == "RB1_not_del"
               & input$EBF1 == "EBF1_not_del"
               & input$PAR1 == "PAR1_not_del"
               & input$IKZF1 == "IKZF1_not_del"
               & input$PAX5 == "PAX5_not_del"
               & input$BTG1 == "BTG1_not_del") {isolate("CNA Good risk (no deletions)") }
    
    else if (  input$submitbutton>0 
               & input$ETV6 == "ETV6_del" 
               & input$`CDKN2A/B` == "CDKN2A/B_not_del"
               & input$RB1 == "RB1_not_del"
               & input$EBF1 == "EBF1_not_del"
               & input$PAR1 == "PAR1_not_del"
               & input$IKZF1 == "IKZF1_not_del"
               & input$PAX5 == "PAX5_not_del"
               & input$BTG1 == "BTG1_not_del") {isolate("CNA Good risk (isolated ETV6)") }
    
    else if (  input$submitbutton>0 
               & input$ETV6 == "ETV6_not_del" 
               & input$`CDKN2A/B` == "CDKN2A/B_not_del"
               & input$RB1 == "RB1_not_del"
               & input$EBF1 == "EBF1_not_del"
               & input$PAR1 == "PAR1_not_del"
               & input$IKZF1 == "IKZF1_not_del"
               & input$PAX5 == "PAX5_not_del"
               & input$BTG1 == "BTG1_del") {isolate("CNA Good risk (isolated BTG1)") }
    
    else if (  input$submitbutton>0 
               & input$ETV6 == "ETV6_not_del" 
               & input$`CDKN2A/B` == "CDKN2A/B_not_del"
               & input$RB1 == "RB1_not_del"
               & input$EBF1 == "EBF1_not_del"
               & input$PAR1 == "PAR1_not_del"
               & input$IKZF1 == "IKZF1_not_del"
               & input$PAX5 == "PAX5_del"
               & input$BTG1 == "BTG1_not_del") {isolate("CNA Good risk (isolated PAX5)") }
    
    else if (  input$submitbutton>0 
             & input$ETV6 == "ETV6_del" 
             & input$`CDKN2A/B` == "CDKN2A/B_del"
             & input$RB1 == "RB1_not_del"
             & input$EBF1 == "EBF1_not_del"
             & input$PAR1 == "PAR1_not_del"
             & input$IKZF1 == "IKZF1_not_del"
             & input$PAX5 == "PAX5_not_del"
             & input$BTG1 == "BTG1_not_del") {isolate("CNA Good risk (ETV6 and CDKN2A/B)") }
    
    else if (  input$submitbutton>0 
               & input$ETV6 == "ETV6_del" 
               & input$`CDKN2A/B` == "CDKN2A/B_not_del"
               & input$RB1 == "RB1_not_del"
               & input$EBF1 == "EBF1_not_del"
               & input$PAR1 == "PAR1_not_del"
               & input$IKZF1 == "IKZF1_not_del"
               & input$PAX5 == "PAX5_del"
               & input$BTG1 == "BTG1_not_del") {isolate("CNA Good risk (ETV6 and PAX5)") }
    
    else if (  input$submitbutton>0 
               & input$ETV6 == "ETV6_del" 
               & input$`CDKN2A/B` == "CDKN2A/B_not_del"
               & input$RB1 == "RB1_not_del"
               & input$EBF1 == "EBF1_not_del"
               & input$PAR1 == "PAR1_not_del"
               & input$IKZF1 == "IKZF1_not_del"
               & input$PAX5 == "PAX5_not_del"
               & input$BTG1 == "BTG1_del") {isolate("CNA Good risk (ETV6 and BTG1)") }
 
    else {return("CNA Poor risk")}
  })
  
  # CNA conslusion 
  CNA_stratification_conclusion <- renderPrint({
    
    if (input$submitbutton == 0) {isolate("en attente") }
    
    else if (  input$submitbutton>0 
               & output$CNA_stratification == "CNA Good risk (no deletions)"
               & output$CNA_stratification == "CNA Good risk (isolated ETV6)"
               & output$CNA_stratification == "CNA Good risk (isolated BTG1)" 
               & output$CNA_stratification == "CNA Good risk (isolated PAX5)" 
               & output$CNA_stratification == "CNA Good risk (ETV6 and CDKN2A/B)"
               & output$CNA_stratification == "CNA Good risk (ETV6 and PAX5)"
               & output$CNA_stratification == "CNA Good risk (ETV6 and BTG1)")
                   {isolate("CNA Good risk") }
    
    else {return("CNA Poor risk")}  
  })
  
  # TP2 stratification 
  output$contents_TP2 <- renderPrint({
    
    if (input$submitbutton == 0) { 
      isolate("en attente")}
    
    else if (input$`MRD TP1` >= 5) {isolate("High Risk (HR) MRD TP1 ≥5% Check protocol")}   
    
    else if (input$Oncogenetic == "t(17;19)") {isolate("High Risk (HR) t(17;19) Check protocol")} 
    
    else if (input$CNS_status == "CNS3 or TLP+WBC>5") { isolate("IR High (CNS3 or TLP+WBC>5)")} 
    
    else if (input$HR_cytogenetics == "Not HR cytogenetics" & input$`MRD TP1` == 0 & input$ALL_subtype == "B-ALL" & input$CNS_status != "TLP+WBC≤5" ) { 
      isolate("Standard Risk (SR) B-ALL - MRD TP1 undetectable - not HR cytogenetics - CNS1/CNS2/TLP-")}
    
    else if (input$Age_at_diagnosis >= 16 & input$`MRD TP1` >= 0) { isolate("IR high (≥16y and IR)")} 
    
    else if (input$`MRD TP2` == "≥0.05%") { isolate("High Risk (HR) MRD TP2 ≥0.05%")} 
    
    else if (input$HR_cytogenetics == "HR cytogenetics") { isolate("IR High (HR cytogenetics)")} 
    
    else if (input$HR_cytogenetics == "Not HR cytogenetics" & input$`MRD TP1` == 0 & input$ALL_subtype == "B-ALL" & input$CNS_status != "TLP+WBC≤5") { isolate("Standard Risk (SR)")}
    
    else if (input$Oncogenetic == "ETV6-RUNX1" & input$`MRD TP1` < 0.1 & input$Age_at_diagnosis <16) {isolate("IR Low (ETV6-RUNX1 with MRDTP1 <0.1% or <10-3)") }
    else if (input$Oncogenetic == "ETV6-RUNX1" & input$`MRD TP1` >= 0.1 ) {isolate("IR High (ETV6-RUNX1 with MRDTP1 ≥0.1% or ≥10-3)") }
    
    else if (input$Oncogenetic == "High Hyperdiploidy" & input$`MRD TP1` < 0.03 & input$Age_at_diagnosis <16) {isolate("IR Low (HeH with MRD PT1 <0.03% or <3.10-4)") }    
    else if (input$Oncogenetic == "High Hyperdiploidy" & input$`MRD TP1` >= 0.03) {isolate("IR High (HeH with MRD PT1 ≥0.03% or ≥3.10-4)") } 
    
    else if (input$ALL_subtype == "T-ALL" &  input$`MRD TP2` != "undetectable") { isolate("IR High (T-ALL with MRD TP2 detectable)") } 
    else if (input$ALL_subtype == "T-ALL" &  input$`MRD TP2` == "undetectable"& input$Age_at_diagnosis <16) { isolate("IR Low (T-ALL with MRD TP2 undetectable and age <16years)") }
    
    else if (input$Oncogenetic == "B-other" & input$CNA_profile == "CNA good risk" & input$`MRD TP1` < 0.05 & input$Age_at_diagnosis <16) { isolate("IR Low (B-other with CNA Good risk and MRD TP1 <0.05% and age <16y)") }  
    else if (input$Oncogenetic == "B-other" & input$CNA_profile == "CNA good risk" & input$`MRD TP1` >= 0.05 & input$Age_at_diagnosis <16) { isolate("IR High (B-other with CNA Good risk and MRD TP1 ≥ 0.05%)") }    
    
    else if (input$Oncogenetic == "B-other" & input$CNA_profile  == "CNA poor risk" & input$`MRD TP1` >= 0 ) { isolate("IR High (B-other with CNA Poor risk and MRD TP1 detectable)") }      
    
    else {
      return("IR high") }
    
    
  })
  
  
  selected_images <- reactive({
    input$selected_images
  })
  
  # Dynamically render selected images in the UI
  output$image_display <- renderUI({
    req(selected_images())
    image_tags <- lapply(selected_images(), function(img) {
      tags$img(src = img, style = "width:45%; margin:5px;")
    })
    do.call(tagList, image_tags)
  })
  
  # Download handler for PowerPoint
  output$download_ppt <- downloadHandler(
    filename = function() {
      "customized_images.pptx"  # Name of the PowerPoint file
    },
    content = function(file) {
      req(selected_images())  # Ensure at least one image is selected
      
      # Create a new PowerPoint presentation
      ppt <- read_pptx()
      
      # Customize format for images
      image_height <- 1  # Height of the image on the slide
      image_width <- 1   # Width of the image on the slide
      image_left <- 0.2    # Left margin of the image on the slide
      image_top <-  0.2    # Top margin of the image on the slide
      
      # Add a slide for each selected image with custom formatting
      for (image_path in selected_images()) {
        # Add a new slide
        ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
        # Add the image with specified size and position
        ppt <- ph_with(
          ppt,
          external_img(image_path, height = image_height, width = image_width),
          location = ph_location(left = 1, top = 1, width = 8, height = 6)
        )
      }
      
      # Save the PowerPoint file
      print(ppt, target = file)
    }
  )
} 
####################################
# Create Shiny App                 #
####################################
shinyApp(ui = ui, server = server)

