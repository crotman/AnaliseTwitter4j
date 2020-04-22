
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

diretorio_corrente <- getwd() 



iteracao <- 0

check_out_and_pmd_xml <- function(commit){
    
    iteracao <<- iteracao + 1
    print(iteracao)
    temp <- paste0(diretorio_corrente, "/xml/",UUIDgenerate(),".xml")
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


create_diffs <- function(sha, sha_ant){


    temp <- paste0(diretorio_corrente, "/diffs/",UUIDgenerate(),".git")
        
    shell(paste0("git -C repository/Twitter4J diff ", sha_ant, " ", sha, "  -U0 --patience --numstat --summary --output=", temp))
    
    temp
}

create_diff_possibly <- possibly(create_diffs, "erro")


create_diffs_single <- function(sha){

    temp <- paste0(diretorio_corrente, "/diffs/",UUIDgenerate(),".git")
    
    shell(paste0("git -C repository/Twitter4J diff ", sha, " 4b825dc642cb6eb9a060e54bf8d69288fbee4904  --numstat --output=", temp))
    
    temp
    
}

create_diffs_single_possibly <- possibly(create_diffs_single, "erro")

    

###CLONA REPOSITÓRIO PROJETO####

clone("https://github.com/Twitter4J/Twitter4J.git","repository/Twitter4J")
 
repository <- repository("repository/Twitter4j")


check_out_and_pmd_possibly <- possibly(check_out_and_pmd_xml, otherwise = tibble(sha = "erro"))


repository <- repository("repository/Twitter4j")


###CRIA DIFFS VERSÃO ANTERIOR####


plan(multiprocess)


commits <-  commits(repository) %>%
    map_df(as_tibble) %>%
    mutate(sha_ant = lead(sha)) %>%
    mutate(files = future_map2(.x = sha, .y = sha_ant, .f =  create_diff_possibly, .progress = TRUE) )


write_rds(commits, "diffs/diffs.rds")

teste <- read_rds("diffs/diffs.rds")



###CRIA DIFFS EM RELAÇÃO AOS VAZIOS####


commits <-  commits(repository) %>%
    map_df(as_tibble) %>%
    mutate(files = future_map(.x = sha, .f =  create_diffs_single_possibly, .progress = TRUE) )

write_rds(commits, "diffs/diffs_single.rds")


####CRIA ALERTAS EM XML####


commits_alerts <-  commits(repository) %>%
    map_dfr(check_out_and_pmd_possibly)

write_rds(commits_alerts,"xmls_alerts")


read_pmd_xml_possibly <- possibly(read_pmd_xml, otherwise = tibble(sha = "erro"))


####JUNTA ALERTAS####

plan(multiprocess)

alertas <- read_rds("xmls_alerts") %>%
    rename(sha_in = sha) %>%
    mutate(data = future_map(.x = xml, .f = read_pmd_xml_possibly, .progress = TRUE )) %>%
    unnest(data) %>%
    mutate(value = str_replace(value,"C:\\\\AnaliseTwitter4j\\\\repository\\\\Twitter4J\\\\", ""))



write_rds(alertas, "xml/alertas_xml")






