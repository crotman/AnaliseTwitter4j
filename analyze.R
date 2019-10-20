library(git2r)
library(tidyverse)
library(magrittr)
library(furrr)
library(uuid)


#clone("https://github.com/Twitter4J/Twitter4J.git","repository/Twitter4J")

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

check_out_and_pmd_possibly <- possibly(check_out_and_pmd, otherwise = tibble(sha = "erro"))

commits <-  commits(repository) %>% 
    map_df(as_tibble)

saveRDS(commits, file =  "commits.rds")

commits_alerts <-  commits(repository) %>%
    map_dfr(check_out_and_pmd_possibly) 
    
saveRDS(commits_alerts, file =  "commits_alerts.rds")















