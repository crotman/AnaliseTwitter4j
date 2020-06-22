
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
library(codeModules)

decorate_code_shiny <-  function(code){
    
    linhas <- str_split(code, "\n") %>% 
        enframe(name = "id", value = "string_linha") %>% 
        unnest(string_linha) %>% 
        mutate(linha = row_number()) %>% 
        mutate(linha = str_pad(linha, width = 3)) %>% 
        mutate(string = str_glue("{linha}:   {string_linha}")) %>% 
        pull(string) %>% 
        str_flatten(collapse = "\n") 
    
    
}


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

if (Sys.info() == "Windows"){
    pmd_path <- "pmd/bin/pmd.bat"
}else{
    pmd_path <-"$HOME/pmd-bin-6.24.0/bin/run.sh pmd"    
}

rule_path <- "rulesets/java/quickstart.xml"

output_path <-  ""  

default_code_new <- read_file("default_new/code.java")
default_code_old <- read_file("default_old/code.java")


source("R/functions.R")

input_old_code <- textAreaInput(
    inputId = "input_old_code",
    label = "Old code",
    rows = 20,
    width = "250%",
    value = default_code_old
)

input_new_code <- textAreaInput(
    inputId = "input_new_code",
    label = "New code",
    rows = 20,
    width = "250%",
    value = default_code_new
)



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Match alerts"),
    actionButton(inputId = "go_button",label = "Run!"),
    splitLayout(cellWidths = c("40%","60%"),
        accordion(        
            accordionItem(
                id = "old",
                title = "Old Code",
                input_old_code
                
            ),
            accordionItem(
                id = "new",
                title = "New Code",
                input_new_code
            )
        )
        ,
        verticalLayout(
            splitLayout(
                codeOutput(outputId = "old_code_out"),
                codeOutput(outputId = "new_code_out")
            )
            ,
            gt_output(
                outputId = "tabela"
            )
        )
    )
        
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    saida_algoritmo <- reactive({
        
        saida <- calculate_features_from_versions(code_new = input$input_new_code, code_old = input$input_old_code )
        
    }) 
    

    output$tabela <- render_gt({
        
        if(input$go_button == 0){
            return()
        }
        
        dados <- isolate(saida_algoritmo())
            
        alerts_old <- dados$graph_old_with_alert %>% 
            activate(nodes) %>% 
            as_tibble() %>% 
            filter(!is.na(id_alert_alert)) %>% 
            select(
                begin_line_old = beginline,
                id_alert_old = id_alert,
                rule_alert_old = rule_alert
            )
        

        alerts_new <- dados$graph_new_with_alert %>% 
            activate(nodes) %>% 
            as_tibble() %>% 
            filter(!is.na(id_alert_alert)) %>% 
            select(
                begin_line_new = beginline,
                id_alert_new = id_alert,
                rule_alert_new = rule_alert
            )
        
                
        features <- dados$features %>% 
            select(-c(graph_new, graph_old)) %>% 
            unnest(features) %>%
            left_join(
                alerts_old,
                by = c("id_alert_old")
            ) %>% 
            left_join(
                alerts_new,
                by = c("id_alert_new")
            ) 

        features %>% 
            select(
                begin_line_old,
                begin_line_new,
                rule_alert_old,
                rule_alert_new,
                same_rule,
                same_id_group,
                same_method,
                same_block,
                dist_line,
                dist_line_normalized_block,
                dist_line_normalized_method,
                dist_line_normalized_unit
            ) %>% 
            arrange(
                begin_line_old,
                begin_line_new
            ) %>% 
            gt() 
            
            
        
    })
    
    output$old_code_out <- renderCode({
        saida <- decorate_code_shiny(input$input_old_code)
        saida
    })

    
    output$new_code_out <- renderCode({
        saida <- decorate_code_shiny(input$input_new_code)
        saida
    })
    
        
}

# Run the application 
shinyApp(ui = ui, server = server)
