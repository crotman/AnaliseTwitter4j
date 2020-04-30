assemble_pmd_command <- function(pmd_path, code_path, rule_path, output_path, output){
    command <- str_glue("{pmd_path} -d {code_path} -f xml -R {rule_path} -reportfile {output_path}{output}.xml")
}

read_pmd_xml <- function(file){
    
    content_xml <- read_xml(file)
    
    
    alerts <- content_xml %>% 
        xml_children() %>% 
        xml_children() %>% 
        xml_attrs() %>% 
        map_df(.f = ~enframe(x = .x )) %>% 
        mutate(primeiro_campo = if_else(name == "beginline", 1, 0)  ) %>% 
        mutate(linha = cumsum(primeiro_campo) ) %>% 
        select(-primeiro_campo) %>% 
        pivot_wider(names_from = name, values_from = value) %>% 
        mutate(id_alert = row_number())
    
}


assemble_diff_command <- function(code_path_left, code_path_right, output_path, output_left, output_right){
    str_glue("git diff -U0 --patience --numstat --summary --output={output_path}{output_left}_{output_right}.diff --no-index {code_path_left} {code_path_right}")
}

read_number_of_lines <- function(dir){
    
    files <- list.files(dir)
    
    read_table(file = str_glue("{dir}\\{files}"), col_names = FALSE, skip_empty_rows = FALSE) %>% 
        nrow() 
}

map_lines <- function(file, lines_prev_param, lines_post_param){
    
    file <- "C:\\doutorado\\AnaliseTwitter4j\\match_algorithm_description\\old_original_new_1.diff"
    
    lines_prev_param <- as.integer(lines_prev_param)
    
    lines_post_param <- as.integer(lines_post_param)
    
    diff_marks <- read_table(file, col_names = FALSE ) %>% 
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
        mutate(
            lines_prev = lines_prev_param,
            lines_post = lines_post_param
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
        ) %>% 
        rowwise() %>% 
        mutate(
            min_map = max(c(map_remove, map_add), na.rm = TRUE)
        ) %>%
        arrange(min_map)        
    
    
    
    
}


categorise_alerts <- function(map, alerts_old, alerts_new){
    
    map  = examples_crossed$lines_map[[1]]
    
    alerts_old = examples_executed$pmd_output[[1]] %>% 
        select(
            id_alert,
            line = beginline,
            rule,
            ruleset,
            package,
            class,
            method,
            variable
        ) %>% 
        rename_all(
            .funs = ~str_glue("{.x}_old")
        ) %>% 
        mutate(
            line_old = as.integer(line_old)
        )
    
    
    alerts_new = examples_executed$pmd_output[[2]] %>% 
        select(
            id_alert,
            line = beginline,
            rule,
            ruleset,
            package,
            class,
            method,
            variable
        ) %>% 
        rename_all(
            .funs = ~str_glue("{.x}_new")
        ) %>% 
        mutate(
            line_new = as.integer(line_new)
        )
    
    
    classification_old <- map %>% 
        select(
            line_old = map_remove,
            line_new = map_add,
        ) %>%
        mutate_all(
            as.integer
        ) %>% 
        left_join(
            alerts_old,
            by = c("line_old")
        ) %>% 
        left_join(
            alerts_new,
            by = c("line_new")
        ) %>% 
        filter(
            !is.na(rule_old)
        ) %>% 
        replace_na(
            list(
                rule_old = "No rule",
                rule_new = "No rule",
                class_new = "No class",
                class_old = "No class",
                ruleset_new = "No ruleset",
                ruleset_old = "no ruleset",
                class_old = "No class",
                class_new = "No class",
                method_old = "No method",
                method_new = "No method",
                variable_old = "No variable",
                variable_new = "No variable"
            )
        ) %>% 
        mutate(
            classification = if_else(
                rule_old == rule_new & 
                    class_new == class_old & 
                    method_old == method_new &
                    variable_new == variable_old
                ,
                "Not fixed",
                "Fixed"
            )
        ) %>% 
        group_by(
            id_alert_old
        ) %>% 
        mutate(
            n_not_fixed = sum(classification == "Not fixed")
        ) %>% 
        filter(
            (n_not_fixed == 0) | classification == "Not fixed"
        ) 
    
    
    classification_new <- map %>% 
        select(
            line_old = map_remove,
            line_new = map_add,
        ) %>%
        mutate_all(
            as.integer
        ) %>% 
        left_join(
            alerts_old,
            by = c("line_old")
        ) %>% 
        left_join(
            alerts_new,
            by = c("line_new")
        ) %>% 
        filter(
            !is.na(rule_new)
        ) %>% 
        replace_na(
            list(
                rule_old = "No rule",
                rule_new = "No rule",
                class_new = "No class",
                class_old = "No class",
                ruleset_new = "No ruleset",
                ruleset_old = "no ruleset",
                class_old = "No class",
                class_new = "No class",
                method_old = "No method",
                method_new = "No method",
                variable_old = "No variable",
                variable_new = "No variable"
            )
        ) %>% 
        mutate(
            classification = if_else(
                rule_old == rule_new & 
                    class_new == class_old & 
                    method_old == method_new &
                    variable_new == variable_old
                ,
                "Not fixed",
                "New"
            )
        ) %>% 
        group_by(
            id_alert_new
        ) %>% 
        mutate(
            n_not_fixed = sum(classification == "Not fixed")
        ) %>% 
        filter(
            (n_not_fixed == 0) | classification == "Not fixed"
        ) 
    
    classification <- bind_rows(
        classification_old %>% mutate(version = "Old"),
        classification_new %>% mutate(version = "New"),
    )
    
    classification
    
}



decorate_code <- function(strings) {
    strings %>%
        enframe(name = "line", value = "code") %>%
        mutate(
            line = as.character(line) %>%  str_pad(width = 3, side = "left"),
            code = code %>%  str_trunc(width = size_line_of_code, ellipsis = "..."),
            final_code = str_glue("/*{line}*/{code}")
        ) %>%
        pull(final_code)
}


aggregate_alerts_by_line <-
    function(alerts, trunc_rule_length_param = NA_real_) {
        trunc_rule_length <-
            if_else(is.na(trunc_rule_length_param),
                    10,
                    trunc_rule_length_param)
        
        alerts %>%
            mutate(beginline = as.integer(beginline)) %>%
            group_by(beginline,
                     rule) %>%
            summarise(n = n()) %>%
            mutate(rule = if_else(
                is.na(trunc_rule_length_param),
                rule,
                str_trunc(
                    rule,
                    width = trunc_rule_length,
                    side = "right",
                    ellipsis = ""
                ) %>% as.character()
            )) %>%
            mutate(rule = if_else(n == 1, rule, str_glue("{rule}({n})") %>% as.character()))
        
    }

decorate_code_and_alerts <-
    function(strings,
             alerts,
             region_only = FALSE,
             region_size = 3) {
        
        # for debug
        # strings <- read_lines("old/code.java")
        # alerts <- examples_executed$pmd_output[[1]]
        # region_only = TRUE
        # region_size = 3
        
        alert <- alerts %>%
            as_tibble() %>%
            select(beginline, rule) %>%
            aggregate_alerts_by_line()
        
        
        
        max_rule <- alert %>%
            pull(rule) %>%
            str_length() %>%
            max(na.rm = TRUE)
        
        
        strings %>%
            enframe(name = "line", value = "code") %>%
            left_join(alert,
                      by = c("line" = "beginline")) %>%
            mutate(
                alert_mark_up = if_else(is.na(rule), NA_integer_, line ),  
                alert_mark_down = if_else(is.na(rule), NA_integer_, line )  
            ) %>%
            fill(
                alert_mark_up,
                .direction = "up"
            ) %>% 
            fill(
                alert_mark_down,
                .direction = "down"
            ) %>% 
            replace_na(
                list(
                    alert_mark_down = 0,
                    alert_mark_up = length(strings)
                )
            ) %>% 
            mutate(
                dist_up = line - alert_mark_down,
                dist_down = alert_mark_up - line
            ) %>% 
            rowwise() %>%
            mutate(
                min_dist = min(dist_up, dist_down)
            ) %>% 
            mutate(
                code = if_else(min_dist == region_size + 1, "/* ...  */", code )
            ) %>% 
            filter(
                min_dist <= region_size + 1
            ) %>% 
            ungroup() %>% 
            replace_na(list(rule = "")) %>%
            mutate(
                line = as.character(line) %>%  str_pad(width = 3, side = "left"),
                code = code %>%  str_trunc(width = size_line_of_code - length_alert_name, ellipsis = "..."),
                rule = rule %>% str_pad(width = length_alert_name, side = "right"),
                final_code = str_glue("/*{line}-{rule}*/{code}")
            ) %>%
            pull(final_code)
    }

decorate_code_alerts_mapped <-
    function(strings_old_param,
             alerts_old_param,
             strings_new_param,
             alerts_new_param,
             map_param,
             region_only = FALSE,
             region_size = 3) {
        # for debug
        strings_old_param <-  read_lines("old/code.java")
        strings_new_param <-  read_lines("new/code.java")
        alerts_old_param <-  examples_executed$pmd_output[[1]]
        alerts_new_param <-  examples_executed$pmd_output[[2]]
        map_param <-  examples_crossed$lines_map[[1]]
        region_only = FALSE
        region_size = 3      
        
        
        map <- map_param %>%
            select(line_old = map_remove,
                   line_new = map_add)
        
        strings_old <-
            strings_old_param %>% enframe(name = "line_old", value = "code_old") %>% replace_na(list(code_old = ""))
        
        alerts_old <- alerts_old_param %>%
            aggregate_alerts_by_line(trunc_rule_length = length_alert_name_side_by_side) %>%
            select(line_old = beginline, rule_old = rule)
        
        strings_new <-
            strings_new_param %>% enframe(name = "line_new", value = "code_new") %>% replace_na(list(code_new = ""))
        
        alerts_new <- alerts_new_param %>%
            aggregate_alerts_by_line(trunc_rule_length = length_alert_name_side_by_side) %>%
            select(line_new = beginline, rule_new = rule)
        
        
        saida <- map %>%
            left_join(strings_old,
                      by = c("line_old")) %>%
            left_join(alerts_old,
                      by = c("line_old")) %>%
            left_join(strings_new,
                      by = c("line_new")) %>%
            left_join(alerts_new,
                      by = c("line_new")) %>%
            ungroup() %>% 
            mutate(
                id_line = row_number(), 
                alert_mark_up = if_else(is.na(rule_new) & is.na(rule_old) , NA_integer_, id_line ),  
                alert_mark_down = if_else(is.na(rule_new) & is.na(rule_old), NA_integer_, id_line )  
            ) %>%
            fill(
                alert_mark_up,
                .direction = "up"
            ) %>% 
            fill(
                alert_mark_down,
                .direction = "down"
            ) %>% 
            replace_na(
                list(
                    alert_mark_down = 0,
                    alert_mark_up = nrow(map)
                )
            ) %>% 
            mutate(
                dist_up = id_line - alert_mark_down,
                dist_down = alert_mark_up - id_line
            ) %>% 
            rowwise() %>%
            mutate(
                min_dist = min(dist_up, dist_down)
            ) %>% 
            mutate(
                code_new = if_else(min_dist == region_size + 1, "/* ...  */", code_new ),
                code_old = if_else(min_dist == region_size + 1, "/* ...  */", code_old )
            ) %>% 
            filter(
                min_dist <= region_size + 1
            ) %>% 
            ungroup() %>% 
            select(
                -id_line
            ) %>% 
            mutate(line_old = as.character(line_old),
                   line_new = as.character(line_new)) %>%
            replace_na(
                list(
                    code_new = "/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*/",
                    code_old = "/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*/",
                    line_old = "",
                    line_new = "",
                    code_old = "",
                    code_new = "",
                    rule_old = "",
                    rule_new = ""
                )
            ) %>%
            mutate(
                line_old = line_old %>%  str_pad(width = 3, side = "left"),
                line_new = line_new %>%  str_pad(width = 3, side = "left"),
                code_old = code_old %>%
                    str_trunc(
                        width = size_line_of_code_side_by_side - length_alert_name_side_by_side,
                        ellipsis = ""
                    ) %>%
                    str_pad(
                        width = size_line_of_code_side_by_side - length_alert_name_side_by_side,
                        side = "right"
                    ),
                
                code_new = code_new %>%
                    str_trunc(
                        width = size_line_of_code_side_by_side - length_alert_name_side_by_side,
                        ellipsis = ""
                    ) %>%
                    str_pad(
                        width = size_line_of_code_side_by_side - length_alert_name_side_by_side,
                        side = "right"
                    ),
                
                rule_old = rule_old %>% str_pad(width = length_alert_name_side_by_side + 3, side = "right"),
                rule_new = rule_new %>% str_pad(width = length_alert_name_side_by_side + 3, side = "right"),
                
                
                
                final_code = str_glue(
                    "/*{line_old}-{rule_old}*/{code_old}/*{line_new}-{rule_new}*/{code_new}"
                )
            ) %>%
            pull(final_code) %>% 
            view()
        
        
        
    }

read_and_decorate_code <-  function(file) {
    read_lines(file,) %>%
        decorate_code() %>%
        as.character()
}

read_and_decorate_code_and_alerts <-  function(file, alerts,  region_only = FALSE,
                                               region_size = 3) {
    #for debug
    # file <-  "old/code.java"
    # alerts <- examples_executed$pmd_output[[1]]
    
    read_lines(file) %>%
        decorate_code_and_alerts(alerts = alerts, region_only = region_only, region_size = region_size) %>%
        as.character()
}

read_and_decorate_code_and_alerts_mapped <-
    function(file_old,
             alerts_old,
             file_new,
             alerts_new,
             map,             
             region_only = FALSE,
             region_size = 3) {
        # For debug
        # file_old <- "old/code.java"
        # alerts_old <- examples_executed$pmd_output[[1]]
        # file_new <-  "new/code.java"
        # alerts_new <- examples_executed$pmd_output[[2]]
        # map <- examples_crossed$lines_map[[1]]
        
        strings_new <- read_lines(file_new)
        strings_old <- read_lines(file_old)
        
        decorate_code_alerts_mapped(
            strings_old_param = strings_old,
            strings_new_param = strings_new,
            alerts_old_param = alerts_old,
            alerts_new_param = alerts_new,
            map_param = map,
            region_only = region_only,
            region_size = region_size            
        ) %>%
            as.character()
        
    }

