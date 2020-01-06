library(tidyverse)


# alerts <- read_rds("xml/alertas_xml")

# alerts_commits <- alerts %>% 
#     filter(str_detect(sha_in, "8376f") )


diffs = read_rds("diffs/diffs_single.rds")

sha <- "8376fade8d557896bb9319fb46e39a55b134b166"
    
sha_ant <- "e8ac51da7f0b052e70b1b953a5b89680d5962231" 

file_diff_single <- "diffs/07039a34-0f09-11ea-bb95-95fbb242a52c.git"

file_diff_single_ant <- "diffs/0716c6f4-0f09-11ea-bb95-95fbb242a52c.git"

file_diff <- "diffs/5d7aa310-0e66-11ea-9334-35935f559940.git"
    

lines_prev <- read_table(file_diff_single_ant,  col_names = c("tudo") ) %>% 
    separate(tudo, into = c("nothing","lines_prev","file"), sep = "\t") %>% 
    select(-nothing) 


lines_post <- read_table(file_diff_single,  col_names = c("tudo")) %>% 
    separate(tudo, into = c("nothing","lines_post","file"), sep = "\t") %>% 
    select(-nothing) 


diff_marks <- read_table(file_diff, col_names = FALSE ) %>% 
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
        file_post = str_replace(file_post, "b/",""),
        file_prev = str_replace(file_prev, "a/","")
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
    mutate(
        line_remove = if_else(n_remove == 0 | is.na(n_remove) , line_remove+1L, line_remove ),
        end_remove = if_else(n_remove == 0 | is.na(n_remove) , end_remove+1L, end_remove )
    ) %>%
    group_by(id_diff) %>% 
    mutate(
        id_diff_id = row_number(),
        n_diff = n()
    ) %>% 
    ungroup() 



last_diff <- diff_marks %>% 
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



map <- diff_marks %>% 
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
    filter(!is.na(line_remove )) %>% 
    mutate(
        map_remove = map2(.x = (end_remove_prev + 1L), .y = (line_remove - 1L),.f = function(x, y) x:y),
        map_add = map2(.x = (end_add_prev+1L), .y = (line_add - 1L),.f = function(x, y) x:y)
    ) %>%
    filter(!is.na(lines_post)) %>%
    unnest(cols = c(map_remove, map_add )) %>% 
    select(
        lines_post,
        lines_prev,
        file_prev,
        file_post,
        map_remove,
        map_add
    ) 


post_sem_prev <- diff_marks %>% 
    select(lines_post, file_post, file_prev) %>% 
    distinct() %>% 
    replace_na(list(lines_post = 1)) %>% 
    mutate( lines =  map(.x = lines_post, .f = function(x){tibble(map_add = 1:x)} )) %>% 
    unnest(lines) %>% 
    anti_join(map, by = c("file_post","map_add" )) 

prev_sem_post <- diff_marks %>% 
    select(lines_prev, file_prev, file_post) %>% 
    distinct() %>% 
    replace_na(list(lines_prev = 1)) %>% 
    mutate( lines =  map(.x = lines_prev, .f = function(x){tibble(map_remove = 1:x)} )) %>% 
    unnest(lines) %>% 
    anti_join(map, by = c("file_post","map_remove" )) 



final_map <- map %>%
    bind_rows(post_sem_prev) %>%
    bind_rows(prev_sem_post) %>% 
    mutate(
        changed = sum((is.na(map_remove) | is.na(map_add) ))
    ) 
    



final_map    

# 
# 
# 
# 
# 
# final_map_direct_impl <- final_map %>% 
#     filter(file_post == "twitter4j-core/src/internal-json/java/twitter4j/DirectMessageJSONImpl.java") %T>%
#     View()
#     
# 
# 
# post_sem_prev <- diff_marks %>% 
#     filter(file_post == "twitter4j-core/src/internal-json/java/twitter4j/DirectMessageJSONImpl.java") %>% 
#     select(lines_post, file_post, file_prev) %>% 
#     distinct() %>% 
#     mutate( lines =  map(.x = lines_post, .f = function(x){tibble(map_add = 1:lines_post)} )) %>% 
#     unnest(lines) %>%
#     anti_join(final_map, by = c("file_post","map_add" )) %T>%
#     View()
# 
# 
# 
# 



# final_map %>% 
#     filter(file_post == "twitter4j-core/src/internal-json/java/twitter4j/DirectMessageJSONImpl.java") %T>%
#     View()
    








