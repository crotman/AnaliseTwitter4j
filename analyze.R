library(git2r)
library(tidyverse)
library(magrittr)
library(furrr)
library(uuid)

repository <- repository("repository/Twitter4j")

iteracao <- 0

check_out_and_pmd <- function(commit, id){

    iteracao <<- iteracao + 1
    print(iteracao)
    temp <- (paste0(UUIDgenerate(),".csv"))
    print(commit$sha)    
    checkout(commit, force = TRUE)    
    shell(paste0("pmd/bin/pmd.bat -d repository/Twitter4J -f csv -R rulesets/java/quickstart.xml -cache cache -reportfile ", temp), shell = "PowerShell")
    conteudo <- read_csv(temp) %>% 
        mutate(sha = commit$sha)
    file.remove(temp)
    conteudo

}


commits <-  commits(repository) %>% 
    map_df(as_tibble) %>% 
    head(100) 

saveRDS(commits, "commits.rds")

commits_alerts <-  commits(repository) %>% 
    head(100) %>% 
    map_dfr(check_out_and_pmd)
    
saveRDS(commits_alerts, "commits_alerts.rds")















