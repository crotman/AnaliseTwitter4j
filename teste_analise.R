library(tidyverse)

alertas <- read_rds("alertas_xml")

commits_rank <- commits %>% 
    mutate(commit = nrow(commits) - row_number() +1)


alertas_commits <- alertas %>% 
    inner_join(commits_rank, by = c("sha_in" = "sha")) %>% 
    group_by(sha_in) %>% 
    mutate(id_alert = row_number()) %>% 
    ungroup() %>% 
    select(
        commit, 
        id_alert, 
        beginline,
        endline,
        begincolumn,
        endcolumn,
        rule,
        package,
        class,
        method,
        variable
    ) %>% 
    mutate(
        beginline = as.integer(beginline),
        endline  = as.integer(endline),
        begincolumn  = as.integer(begincolumn),
        endcolumn = as.integer(endcolumn),
        rule = as_factor(rule),
        package = as_factor(package),
        class = as_factor(class),
        method = as_factor(method),
        variable = as_factor(variable)
        
    ) %>% 
    mutate(commit_ant = commit - 1) 


alertas_commits <- alertas_commits %>% 
    inner_join(alertas_commits, by = c("commit_ant" = "commit", "rule", "package"))

str(alertas_commits)
