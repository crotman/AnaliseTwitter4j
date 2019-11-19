
library(tidyverse)
library(xml2)

content_xml <- read_xml("testexml.xml")

alerts <- content_xml %>% 
    xml_children() %>% 
    xml_children() %>% 
    xml_attrs() %>% 
    map_df(.f = ~enframe(.x)) %>% 
    mutate(primeiro_campo = if_else(name == "beginline", 1, 0)  ) %>% 
    mutate(linha = cumsum(primeiro_campo) ) %>% 
    select(-primeiro_campo) %>% 
    pivot_wider(names_from = name, values_from = value) 

files <- content_xml %>% 
    xml_children() %>% 
    xml_attrs() %>% 
    map_df(.f = as_tibble) %>% 
    mutate(file = row_number())

counts <- content_xml %>% 
    xml_children() %>% 
    as_list() %>% 
    map(.f = length ) %>% 
    enframe() %>% 
    unnest(value) %>% 
    mutate(list = map(.x = value, .f = function(x){tibble(alert = 1:x)} ) ) %>% 
    select(-value) %>% 
    unnest(list) %>% 
    transmute(file = name)
    
alerts_with_features <- bind_cols(alerts,counts) %>% 
    inner_join(files, by = c("file"))










