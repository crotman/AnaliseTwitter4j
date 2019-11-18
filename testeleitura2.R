
library(git2r)
library(tidyverse)
library(magrittr)
library(furrr)
library(uuid)
library(lubridate)
library(gridExtra)
library(extrafont)
library(patchwork)
library(xml2)


iteracao <- 0

check_out_and_pmd_xml <- function(commit){
    
    iteracao <<- iteracao + 1
    print(iteracao)
    temp <- (paste0(UUIDgenerate(),".xml"))
    print(commit$sha)    
    checkout(commit, force = TRUE)    
    shell(paste0("pmd/bin/pmd.bat -d repository/Twitter4J -f xml -R rulesets/java/quickstart.xml -cache cache -reportfile ", temp), shell = "PowerShell")
    
    tibble(sha = commit$sha, xml = temp  )    


}


read_pmd_xml <- function(arquivo){
    

    
    content_xml <- read_xml(arquivo)
    
    
    alerts <- content_xml %>% 
        xml_children() %>% 
        xml_children() %>% 
        xml_attrs() %>% 
        map_df(.f = ~enframe(.x)) %>% 
        mutate(primeiro_campo = if_else(name == "beginline", 1, 0)  ) %>% 
        mutate(linha = cumsum(primeiro_campo) ) %>% 
        select(-primeiro_campo) %>% 
        pivot_wider(names_from = name, values_from = value) %>% 
        mutate(id_alert = row_number())
    
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
        transmute(file = name) %>% 
        mutate(id_alert = row_number())
            
    

    
    alerts_with_features <- alerts %>% 
        inner_join(counts, by = c("id_alert")) %>% 
        inner_join(files, by = c("file"))
    
    conteudo <- alerts_with_features
    
        
    conteudo <- alerts_with_features 
    
    # file.remove(temp)
    
    conteudo
    
}
    
# clone("https://github.com/Twitter4J/Twitter4J.git","repository/Twitter4J")
# 
# repository <- repository("repository/Twitter4j")


# check_out_and_pmd_possibly <- possibly(check_out_and_pmd_xml, otherwise = tibble(sha = "erro"))
# 
# 
# repository <- repository("repository/Twitter4j")
# 
# commits <-  commits(repository) %>%
#     map_df(as_tibble)
# 
# commits_alerts <-  commits(repository) %>%
#     map_dfr(check_out_and_pmd_possibly)
# 
# write_rds(commits_alerts,"xmls_alerts")
# 

read_pmd_xml_possibly <- possibly(read_pmd_xml, otherwise = tibble(sha = "erro"))


plan(multiprocess)

alertas <- read_rds("xmls_alerts") %>% 
    rename(sha_in = sha) %>% 
    mutate(data = future_map(.x = xml, .f = read_pmd_xml_possibly, .progress = TRUE )) %>% 
    unnest(data) %>% 
    mutate(value = str_replace(value,"C:\\\\AnaliseTwitter4j\\\\repository\\\\Twitter4J\\\\", ""))



write_rds(alertas, "alertas_xml")



