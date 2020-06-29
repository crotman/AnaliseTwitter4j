
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
library(waiter)

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
    pmd_path <- "$HOME/pmd-bin-6.24.0/bin/run.sh pmd"    
}

rule_path <- "rulesets/java/quickstart.xml"

output_path <-  ""  

default_code_new <- read_file("default_new/code.java")
default_code_old <- read_file("default_old/code.java")


source("R/functions.R")

input_old_code <- textAreaInput(
    inputId = "input_old_code",
    label = "Old code",
    rows = 40,
    width = "250%",
    value = default_code_old
)
 
input_new_code <- textAreaInput(
    inputId = "input_new_code",
    label = "New code",
    rows = 40,
    width = "250%",  
    value = default_code_new
)



# Define UI for application that draws a histogram
ui <- fluidPage(
    use_waiter(),  
    # Application title
    titlePanel("Match alerts"),
    splitLayout(cellWidths = c("40%","60%"),
        wellPanel(
            actionButton(inputId = "go_button",label = "Run!"),
            actionButton(inputId = "save_button",label = "Save Files"),
            textInput(inputId = "path_to_save", label = "Path to Save" ),
            textInput(inputId = "nodes_old", label = "Highlight nodes (old)", value = "10, 42, 41, 15, 16, 43"),
            textInput(inputId = "nodes_new", label =  "Highlight nodes (new)", value = "10, 43, 17, 15, 18, 16, 45, 44"),
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
        )
        ,
        verticalLayout(
            splitLayout(
                codeOutput(outputId = "old_code_out"),
                codeOutput(outputId = "new_code_out")
            ),
            plotOutput("grafico1", height = 1200),
            plotOutput("grafico2", height = 1200),
            plotOutput("grafico3", height = 1200),
            plotOutput("grafico4", height = 1200)
            ,
            gt_output(
                outputId = "tabela"
            )
                
        )
    )
          
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    string_to_vector <- function(x){
        x %>% 
            str_split(pattern = ",", simplify = TRUE) %>% 
            str_trim() %>% 
            as.numeric()        
    }
    
    
    saida_algoritmo <- reactive({
        waiter_show()
        saida <- calculate_features_from_versions(
            code_new = input$input_new_code, 
            code_old = input$input_old_code,
            mostra_new = string_to_vector(input$nodes_new),
            mostra_old = string_to_vector(input$nodes_old)
                
        )
        
    }) 
    
    observeEvent(
        eventExpr =  input$save_button,
        handlerExpr = {
            dir_new <- str_glue("{input$path_to_save}/new")
            dir_old <- str_glue("{input$path_to_save}/old")
            dir.create(dir_new, recursive = TRUE) 
            dir.create(dir_old, recursive = TRUE) 
            write_lines(input$input_new_code, path = str_glue("{dir_new}/code.java") )
            write_lines(input$input_old_code, path = str_glue("{dir_old}/code.java") )
        }
        
        
    )

    
    output$grafico1 <- renderPlot({

        if(input$go_button == 0){
            return()
        }
        
        dados <- isolate(saida_algoritmo())
        

        chart_graph_new <- show_ast(
            dados$graph_new_with_alert,
            size_label = 5,
            show_label = TRUE,
            alpha_label = 1,
            name_field = "text_alert_id_node"
            
        )
        
        chart_graph_old <- show_ast(
            dados$graph_old_with_alert,
            size_label = 5,
            show_label = TRUE,
            alpha_label = 1,
            name_field = "text_alert_id_node"
            
        )
        
        
        
        chart_graph_old + chart_graph_new
        
                
    })

    output$grafico2 <- renderPlot({
        
        if(input$go_button == 0){
            return()
        }
        
        dados <- isolate(saida_algoritmo())
        
        
        chart_graph_old <- show_ast(
            dados$graph_old_with_group, 
            size_label = 4, 
            show_label = TRUE, 
            alpha_label = "mostra", 
            title = "Old version"
        ) 
        
        chart_graph_new <- show_ast(
            dados$graph_new_with_group, 
            size_label = 4, 
            show_label = TRUE, 
            alpha_label = "mostra",
            title = "New version"
        )
        
        
        chart_graph_old + chart_graph_new
        
        
        
    })
    

    
    output$grafico3 <- renderPlot({
        
        if(input$go_button == 0){
            return()
        }
        
        dados <- isolate(saida_algoritmo())
        
        
        chart_graph_old <- show_ast(
            dados$graph_old_with_group, 
            size_label = 4, 
            show_label = TRUE, 
            alpha_label = 0, 
            node_text_field = "id_group" 
        )
        
        chart_graph_new <- show_ast(
            dados$graph_new_with_group, 
            size_label = 4, 
            show_label = TRUE, 
            alpha_label = 0, 
            node_text_field = "id_group" 
        )
        
        
        chart_graph_old + chart_graph_new
        
        
        
    })
    
    
    
    output$grafico4 <- renderPlot({
        
        if(input$go_button == 0){
            return()
        }
        
        dados <- isolate(saida_algoritmo())
        
        
        chart_graph_old <- show_ast(
            dados$graph_old_with_alert, 
            size_label = 5, 
            show_label = TRUE, 
            alpha_label = 1, 
            node_text_field = "id_group",
            name_field = "text_alert"
            
        )
        
        
        chart_graph_new <- show_ast(
            dados$graph_new_with_alert, 
            size_label = 5, 
            show_label = TRUE, 
            alpha_label = 1, 
            node_text_field = "id_group",
            name_field = "text_alert"
            
        )
        
        
        chart_graph_old + chart_graph_new
        
        
        
        
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

        output <- features %>% 
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
            gt() %>% 
            tab_options(
                table.font.size = "80%"
            ) %>% 
            cols_label(
                begin_line_old = "Old line" %>%  html(),
                begin_line_new = "New line" %>% html(),
                rule_alert_old = "Old rule" %>%  html(),
                rule_alert_new = "New rule" %>% html(),
                same_rule = "Same rule" %>% html()  ,
                same_id_group = "Same group" %>%  html(),
                same_method = "Same method" %>%  html(),
                same_block = "Same block" %>%  html(),
                dist_line = "Distance" %>%  html(),
                dist_line_normalized_block = "Distance<br>(norm. block)" %>%  html(),
                dist_line_normalized_method = "Distance<br>(norm. method)" %>%  html(),
                dist_line_normalized_unit = "Distance<br>(norm. unit)" %>%  html()
            ) %>% 
            fmt_number(
                columns = starts_with("dist_line_"),
                decimals = 2
            )
            
        waiter_hide()
        
        output
        
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
