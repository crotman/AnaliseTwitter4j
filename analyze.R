library(git2r)
library(tidyverse)

repository <- repository("repository/Twitter4j")



#check_out_and_pmd <- function()


commits <-  commits(repository) %>% 
    map_df(as_tibble) %>% 
    head()








