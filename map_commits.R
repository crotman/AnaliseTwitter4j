
library(tidyverse)
library(furrr)
library(magrittr)

plan(multiprocess)

map_commits <- function(file_diff_single, file_diff_single_ant, file_diff, sha, summary, message, author, email, when, sha_ant ){


    
    
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
        bind_rows(prev_sem_post)
    
    
    
    final_map    
    
    
}


map_commits_possibly <- possibly(map_commits, "erro")


diff_single <- read_rds("diffs/diffs_single.rds") %>% 
    unnest(files) %>% 
    select(sha, files) %>% 
    mutate(
        files = str_remove(files,fixed("c:/temp/"))
    ) 


diffs <- read_rds("diffs/diffs.rds") %>% 
    unnest(files) %>% 
    rename(
        file_diff = files
    ) %>% 
    left_join(diff_single, by = c("sha" = "sha" )) %>%
    rename(
        file_diff_single = files
    ) %>% 
    left_join(diff_single, by = c("sha_ant" = "sha" )) %>%
    rename(
        file_diff_single_ant = files
    ) %>% 
    mutate_at(
        vars(starts_with("file_")),
        function(x) paste0("diffs/", x)    ) %>% 
    # slice(1:500) %>%
    mutate(maps = future_pmap(.l = ., .f = map_commits_possibly, .progress = TRUE )) %>% 
    unnest(maps)
    

alerts <- read_rds("xml/alertas_xml")

diffs_summary <- 
    diffs %>% 
    select(
        sha_post = sha, 
        sha_prev = sha_ant, 
        file_prev,
        file_post,
        map_prev = map_remove, 
        map_post = map_add
        
    ) %>% 
    # mutate(
    #     sha_prev = as_factor(sha_prev),
    #     sha_post = as_factor(sha_post),
    #     file_post = as_factor(file_post),
    #     file_prev = as_factor(file_prev)
    # ) %>% 
    identity()
        
# alerts_summary <-  alerts %>% 
#     transmute(
#         sha = as_factor(sha_in),
#         beginline = as.integer(beginline),
#         rule = as_factor(rule),
#         ruleset = as_factor(ruleset),
#         package = as_factor(package),
#         class = as_factor(class),
#         priority = as.integer(priority),
#         method = as_factor(method),
#         variable = variable,
#         file = value %>% str_replace_all("\\\\", "/") %>%  as_factor(),
#         alert_id = row_number()
#     )

alerts_summary <-  alerts %>% 
    transmute(
        sha = sha_in,
        beginline = as.integer(beginline),
        rule = rule,
        ruleset = ruleset,
        package = package,
        class = class,
        priority = priority,
        method = method,
        variable = variable,
        file = value %>% str_replace_all("\\\\", "/") ,
        alert_id = row_number()
    )


# diffs_summary %>% 
#     group_by(file_post, map_post) %>% 
#     count() %>% 
#     arrange(desc(n))


diffs_with_alerts_post <- diffs_summary %>% 
    left_join(alerts_summary, by = c("sha_post" = "sha", "file_post" = "file", "map_post" = "beginline" ))


diffs_with_alerts_prev <- diffs_summary %>% 
    left_join(alerts_summary, by = c("sha_prev" = "sha", "file_prev" = "file", "map_prev" = "beginline" ))


diffs_useful <-  diffs_with_alerts_post %>% 
    filter(!is.na(rule)) %>% 
    bind_rows(diffs_with_alerts_prev) %>% 
    filter(!is.na(rule)) %>% 
    select(sha_post,  sha_prev,  file_prev, file_post, map_prev,  map_post) %>% 
    distinct()


diffs_with_alerts_post %<>% 
    semi_join(diffs_useful, by = c("sha_post",  "sha_prev",  "file_prev", "file_post", "map_prev",  "map_post" )) %>% 
    group_by(sha_post,  sha_prev,  file_prev, file_post, map_prev,  map_post) %>%
    nest() %>% 
    rename(post = data) %>% 
    ungroup() 


diffs_with_alerts_prev %<>% 
    semi_join(diffs_useful, by = c("sha_post",  "sha_prev",  "file_prev", "file_post", "map_prev",  "map_post" )) %>% 
    group_by(sha_post,  sha_prev,  file_prev, file_post, map_prev,  map_post) %>%
    nest() %>% 
    rename(prev = data) %>% 
    ungroup() 



count_alerts <- function(df){
    
    df %>% 
        dplyr::filter(!is.na(alert_id)) %>% 
        dplyr::count() %>% 
        pull(n)
    
}


diff_alerts_same_line <-  function(prev, post){
    

    t_join <- prev %>% 
        full_join(post,
                  by =
                    c(
                        "rule",     
                        "ruleset",
                        "package",
                        "class",
                        "priority",
                        "method",
                        "variable"
                    )
                  ,
                  suffix = c("_prev", "_post")
        ) %>% 
        filter(
            xor(!is.na(alert_id_prev), !is.na(alert_id_post))
        ) %>% 
        mutate(
            prev_or_post = case_when(
                !is.na(alert_id_prev) ~ "prev",
                !is.na(alert_id_post) ~ "post"
            )
        )
        
    
    t_join
    
}





diffs_with_alerts <- diffs_with_alerts_post %>% 
    full_join(diffs_with_alerts_prev,
               by = c("sha_post",  "sha_prev",  "file_prev", "file_post", "map_prev",  "map_post")
    ) %>% 
    dplyr::mutate(count_prev = future_map_int(.x = prev, .f = count_alerts, .progress = TRUE)) %>% 
    dplyr::mutate(count_post = future_map_int(.x = post, .f = count_alerts, .progress = TRUE)) %>%
    filter(count_prev != 0 | count_post != 0 ) 




diff_alerts_prev_post <- diffs_with_alerts %>% 
    mutate(id = row_number()) %>% 
    mutate(diff = future_map2(.x = prev, .y = post, .f = diff_alerts_same_line, .progress = TRUE  )) %>% 
    unnest(diff)

    
# 
# 
# 
# # 
# # diffs_with_alerts %>% 
# #     group_by(file_post, map_post) %>% 
# #     count() %>% 
# #     arrange(desc(n))
# # 
# # 
# # 
# # diffs_with_only_alerts <- diffs_with_alerts %>% 
# #     filter(!is.na(rule_post)) %T>% 
# #     View()
# # 
# # 
# # diffs_with_only_alerts %>% distinct(alert_id_post) 
# # 
# # diffs_with_only_alerts %>% distinct(alert_id_prev) 
# # 
# # 
# alerts_post <- alerts_summary %>%
#     filter(sha %in% diffs_summary$sha_post ) 
# # 
# # alerts_post %>% distinct(alert_id)
# # 
# alerts_prev <- alerts_summary %>%
#     filter(sha %in% diffs_summary$sha_prev )
# 
# 
# alerts_post_direct_impl <- alerts_post %>% 
#     filter(file == "twitter4j-core/src/main/java/twitter4j/api/DirectMessagesResources.java") %T>% 
#     View()
# 
# 
# alerts_prev_direct_impl <- alerts_prev %>% 
#     filter(file == "twitter4j-core/src/main/java/twitter4j/api/DirectMessagesResources.java") %T>%
#     View()
# 
# 
# diffs_with_alerts_direct_impl <-  diffs_with_alerts %>% 
#     filter(file_prev == "twitter4j-core/src/main/java/twitter4j/api/DirectMessagesResources.java") %T>%
#     View()
# 
# 
# diffs_summary_direct_impl <- diffs_summary %>% 
#     filter(file_prev == "twitter4j-core/src/main/java/twitter4j/api/DirectMessagesResources.java") %T>%
#     View()
# 
# # 
# # 
# # diffs_with_alerts_prev <- diffs_with_alerts %>% 
# #     filter(!is.na(rule_prev) & is.na(map_post)) %T>% 
# #     View()
# # 
# # 
# # diffs_with_alerts_post <- diffs_with_alerts %>% 
# #     filter(!is.na(rule_post) & is.na(rule_prev)) %T>%
# #     View()
# # 
# # 
# alerts_post_by_file <- alerts_post %>%
#     count(file)
# 
# alerts_prev_by_file <- alerts_prev %>%
#     count(file)
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# comp_alerts_by_file <- alerts_prev_by_file  %>%
#     full_join(alerts_post_by_file, by =c("file"))
# 
# comp_alerts_by_file_diff <- comp_alerts_by_file %>%
#     replace_na(list(n.x = 0, n.y = 0)) %>%
#     filter(n.x != n.y ) 


write_rds(diff_alerts_prev_post, "diff_alerts_prev_post.rds")










