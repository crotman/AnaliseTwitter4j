library(tidyverse)

lines_prev <- read_table("diff_a.diff",  col_names = c("tudo")) %>% 
    separate(tudo, into = c("nothing","lines_prev","file"), sep = "\t") %>% 
    select(-nothing) 


lines_post <- read_table("diff_b.diff",  col_names = c("tudo")) %>% 
    separate(tudo, into = c("nothing","lines_post","file"), sep = "\t") %>% 
    select(-nothing) 


diff_marks <- read_table("testegit.git", col_names = FALSE) %>% 
    rename(text = 1) %>% 
    mutate(
        marca_inicio_diff = str_detect(text, "diff --git"),
        id_diff = cumsum(marca_inicio_diff),
        diff_title = if_else(marca_inicio_diff, text, NA_character_)
    ) %>% 
    select(-marca_inicio_diff) %>% 
    fill(diff_title, .direction = "down") %>% 
    filter(str_starts(text, "@@")) %>% 
    separate(text, sep = " ", into = c("mark", "minus", "plus"), extra = "drop" ) %>% 
    select(-mark) %>% 
    separate(minus, into = c("line_remove", "n_remove"), sep = ",") %>% 
    separate(plus, into = c("line_add", "n_add"), sep = ",") %>% 
    mutate(
        n_remove = if_else(is.na(n_remove),"1",n_remove),
        n_add = if_else(is.na(n_add),"1",n_add)
    ) %>% 
    mutate(
        line_remove = str_remove(line_remove,"\\-") %>% str_trim(),
        line_add = str_remove(line_add,"\\+" %>% str_trim())
    ) %>% 
    separate(
        diff_title, 
        sep = " ", 
        into = 
            c(
                "diff", 
                "git", 
                "file_prev",
                "file_post"
            ), 
        extra = "drop" 
        ) %>% 
    select(c(-diff,-git)) %>% 
    mutate(
        file_prev = str_replace(file_prev, "a/",""),
        file_post = str_replace(file_prev, "b/","")
    ) %>% 
    left_join(lines_prev, by = c("file_prev" = "file")) %>% 
    left_join(lines_post, by = c("file_post" = "file")) %>% 
    mutate(
        lines_prev = as.integer(lines_prev),
        lines_post = as.integer(lines_post)
    ) %>%    
    mutate_at(
        vars(ends_with("_add")),
        as.integer
    ) %>% 
    mutate_at(
        vars(ends_with("_remove")),
        as.integer
    ) %>% 
    mutate(
        line_add = if_else(n_add == 0, line_add + 1L, line_add)
    ) %>% 
    mutate(
        end_remove = line_remove + n_remove - 1L,
        end_add = line_add + n_add - 1L
    ) %>% 
    group_by(id_diff) %>% 
    mutate(
        id_diff_id = row_number(),
        n_diff = n()
    ) %>% 
    ungroup() 

last_diff <- diff_marked %>% 
    group_by(id_diff) %>% 
    summarise(
        line_remove = first(lines_prev) + 1L ,
        n_remove = NA,
        line_add = first(lines_post) + 1L ,
        n_add = NA,
        file_prev = first(file_prev),
        file_post = first(file_post),
        lines_prev = first(lines_prev),
        lines_post = first(lines_post),
        end_remove = NA,
        end_add = NA,
        id_diff_id = last(id_diff_id) + 1L,
        n_diff = first(n_diff)
    ) %>% 
    ungroup()


diff_marks %>% 
    bind_rows(last_diff) %>% 
    arrange(id_diff, id_diff_id) %>% 
    mutate(
        end_remove_prev = lag(end_remove),
        end_add_prev = lag(end_add)
    ) %>% 
    mutate(
        end_remove_prev = if_else(is.na(end_remove_prev),0L, end_remove_prev),
        end_add_prev = if_else(is.na(end_add_prev),0L, end_add_prev)
    ) %>% 
    mutate(
        line_add = if_else(is.na(line_add),0L, line_add)
    ) %>%    
    mutate(
        prev_interval_remove = map2(.x = (end_remove_prev + 1L), .y = (line_remove-1L),.f = function(x, y) x:y),
        prev_interval_add = map2(.x = (end_add_prev+1L), .y = (line_add-1L),.f = function(x, y) x:y)
    ) %>% 
    View()





