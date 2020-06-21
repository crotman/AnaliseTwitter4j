
library(shiny)
library(shinydashboardPlus)

library(xml2)
library(tidyverse)
library(gt)
library(knitr)
library(kableExtra)
library(tidygraph)
library(ggraph)
library(patchwork)
library(magrittr)
library(scales)
library(magrittr)


map_rule_small <- tribble(
    
    ~rule,                              ~small_rule,
    "class_or_interface_body",          "class_body",                   
    "class_or_interface_declaration",   "class_decl",
    "class_or_interface_type",          "class_type",
    "compilation_unit",                 "unit",
    "extends_list",                     "extends",
    "implements_list",                  "implements",
    "import_declaration",               "import",
    "method",                           "method",
    "name",                             "name",
    "package",                          "package",
    "type_declaration",                 "type_decl",
    "constructor_declaration",          "constructor",
    "field_declaration",                "field",
    "variable_id",                      "var_id",
    "formal_parameter",                 "param",
    "formal_parameters",                "params",
    "annotation",                       "annotation",
    "block",                            "block",
    "statement",                        "statement",
    "if_statement",                     "if"
    
)



size_line_of_code <- 160

length_alert_name <- 35

length_alert_name_side_by_side <- 20

size_line_of_code_side_by_side <- 110


pmd_path <- "pmd/bin/pmd.bat"

rule_path <- "rulesets/java/quickstart.xml"

output_path <-  ""  

default_code_new <- read_file("default_new/code.java")
default_code_old <- read_file("default_old/code.java")


source("R/functions.R")




# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Match alerts"),
    splitLayout(
        accordion(        
            accordionItem(
                id = "old",
                title = "Old Code",
                textAreaInput(
                    inputId = "input_old_code",
                    label = "Old code",
                    rows = 70,
                    width = "250%",
                    value = default_code_old
                )
            ),
            accordionItem(
                id = "new",
                title = "New Code",
                textAreaInput(
                    inputId = "input_new_code",
                    label = "New code",
                    width = "250%",
                    rows = 70,
                    value = default_code_new
                )
            )
        )
        ,
        gt_output(
            outputId = "tabela"
        )
    )
        
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    saida_algoritmo <- reactive({
        
        saida <- calculate_features_from_versions(code_new = input$input_new_code, code_old = input$input_old_code )
        
    }) 
    
    features <- reactive({
     
        saida_algoritmo <- saida_algoritmo()$features 
        saida_algoritmo
    })
    
    output$tabela <- render_gt({
        
        gt(features())
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
