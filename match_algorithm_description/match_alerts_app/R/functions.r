assemble_pmd_command <- function(pmd_path, code_path, rule_path, output_path, output){
    command <- str_glue("{pmd_path} -d {code_path} -f xml -R {rule_path} -reportfile {output_path}{output}.xml")
}

read_pmd_xml <- function(file){
    
    # file <- "new_1.xml"
    
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
        mutate(id_alert = row_number()) %>% 
        mutate_at(
            vars(one_of(c("beginline", "endline", "begincolumn", "endcolumn", "priority"))),
            as.integer
        )
    
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
    
    # file <- "C:\\doutorado\\AnaliseTwitter4j\\match_algorithm_description\\old_original_new_1.diff"
    
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


categorise_alerts <- function(map, alerts_old_param, alerts_new_param){
    
    map  = examples_crossed$lines_map[[1]]

    alerts_old_param <- examples_executed$pmd_output[[1]]

    alerts_new_param <- examples_executed$pmd_output[[2]]
    
    
    
    alerts_old = alerts_old_param %>%
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
    # 
    # 
    alerts_new = alerts_new_param %>%
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
                "Open",
                "Fixed"
            )
        ) %>% 
        group_by(
            id_alert_old
        ) %>% 
        mutate(
            n_not_fixed = sum(classification == "Open")
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
                "Open",
                "New"
            )
        ) %>% 
        group_by(
            id_alert_new
        ) %>% 
        mutate(
            n_not_fixed = sum(classification == "Open")
        ) %>% 
        filter(
            (n_not_fixed == 0) | classification == "Open"
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
        # strings_old_param <-  read_lines("old/code.java")
        # strings_new_param <-  read_lines("new/code.java")
        # alerts_old_param <-  examples_executed$pmd_output[[1]]
        # alerts_new_param <-  examples_executed$pmd_output[[2]]
        # map_param <-  examples_crossed$lines_map[[1]]
        # region_only = FALSE
        # region_size = 3      
        
        
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
            pull(final_code) 
        
        
        
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



extract_piece_of_code <-  function(strings_param, begin_line, end_line, begin_column, end_column){
    
    
    #for debug
    #strings_param <- read_lines("little-tree/code.java") %>% str_flatten("\n")
    # begin_line <- 33
    # end_line <- 43
    # begin_column <- 5
    # end_column <- 5
    
    strings <- str_split(strings_param, pattern = "\n") %>% unlist()
    
    piece <- strings %>% 
        enframe(name = "line", value = "code") %>% 
        filter(
            between(line, begin_line, end_line)
        ) %>% 
        mutate(
            code = case_when(
                line == begin_line & line == end_line ~ str_sub(code, start = begin_column , end_column),
                line == begin_line ~  str_sub(code, start = begin_column),
                line == end_line ~str_sub(code, end = end_column),
                TRUE ~ code
            )   
        ) %>% 
        pull(code) %>% 
        str_flatten(collapse = "\n") 
    
    
    piece
    
    
}


read_raw_ast_nodes <-  function(code_location, output_location ){

    # code_location <- code_file_old
    # output_location <- output_old
    
    system(str_glue("pmd/bin/pmd.bat -d {code_location} -f xml -R blockrules/blockrules.xml -reportfile {output_location}"))
    
    code_all_lines <- read_lines(code_location)
    
    returned_value <- read_pmd_xml(output_location) %>% 
        replace_na(
            list(
                method = "No method"
            )
        ) %>% 
        left_join(
            map_rule_small,
            by = c("rule")
        ) %>% 
        mutate(
            code = pmap(
                .l =  list(
                    strings_param = str_flatten(code_all_lines, collapse = "\n"), 
                    begin_line = beginline, 
                    end_line = endline, 
                    begin_column = begincolumn, 
                    end_column = endcolumn                
                ),
                .f = extract_piece_of_code 
            )
        )
    
    file.remove(output_location)
    
    write_rds(returned_value, "ast.rds")
    
    returned_value
}


show_latex_raw_ast_nodes <- function(nodes){
    
    nodes %>%
        select(
            -c(linha, ruleset, package, class, priority, variable, id_alert, small_rule)
        ) %>% 
        rename(
            line = beginline,
            endline = endline,
            col = begincolumn,
            endcol = endcolumn
            
        ) %>% 
        mutate(
            code = str_trunc(code,width = 30, ellipsis = "...")
        ) %>% 
        arrange(
            line, col
        ) %>% 
        kable(
            format = "latex",
            caption = "Elements captured in code\\label{elements_captured}",
            escape = TRUE
        ) %>%
        kable_styling(
            font_size = 8,
            latex_options = c("hold_position")
        )
    
    
}


generate_ast_tree_from_raw_nodes <-  function(nodes){
    

    max_column <- max(nodes$endcolumn)
    
    nodes_from <- nodes %>%  rename_all(.funs = ~str_glue("{.x}_from"))
    
    nodes_to <- nodes %>%  rename_all(.funs = ~str_glue("{.x}_to"))
    
    all_edges <- nodes_from %>% 
        crossing(nodes_to) %>% 
        mutate(
            location_begin_from = beginline_from * max_column + begincolumn_from,
            location_begin_to = beginline_to * max_column + begincolumn_to,
            location_end_from = endline_from * max_column + endcolumn_from,
            location_end_to = endline_to * max_column + endcolumn_to
        ) %>% 
        filter(id_alert_from != id_alert_to) %>% 
        filter(
            location_begin_from <= location_begin_to & location_end_from >= location_end_to
        ) %>% 
        select(
            from = id_alert_from,
            to =id_alert_to
        ) 
    
    
    descendents <- all_edges %>% 
        group_by(from) %>% 
        summarise(n_descendents = n()) 
    
    nodes_sorted <- nodes %>% 
        left_join(
            descendents,
            by = c("id_alert" = "from")
        ) %>% 
        replace_na(
            list(n_descendents = 0 )
        ) %>% 
        arrange(
            desc(n_descendents)
        ) %>% 
        mutate(
            id_alert_old = id_alert,
            id_alert = row_number()
        ) %>%  
        mutate(
            name = case_when(
                small_rule %in% c("name", "class_type","var_id" ) ~ str_glue('{id_alert}:line:{beginline},{small_rule}:{code}'),
                TRUE ~ str_glue("{id_alert}:line:{beginline},{small_rule}")
            )
        )
    
    map_new_id_alert <- nodes_sorted %>% 
        select(
            id_alert_old,
            id_alert
        )
    
    all_edges_new_id <-  all_edges %>% 
        left_join(
            map_new_id_alert,
            c("from" = "id_alert_old")
        ) %>% 
        mutate(
            from = id_alert
        ) %>% 
        select(-id_alert) %>% 
        left_join(
            map_new_id_alert,
            c("to" = "id_alert_old")
        ) %>% 
        mutate(
            to = id_alert
        ) %>% 
        select(-id_alert) 
    
    nodes_sorted <-  nodes_sorted %>% 
        select(-id_alert_old)
    
    
    complete_graph <- create_empty(n = 0, directed = TRUE) %>% 
        bind_nodes(nodes_sorted ) %>% 
        bind_edges(all_edges_new_id) 
    
    
    complete_graph %>% 
        convert(to_dfs_tree , root = 1, mode = "out" )
    
}


show_ast <-  function(
    graph_dfs_tree, 
    size_label = 5.5, 
    alpha_label = 1,
    node_text_field = "id_alert", 
    name_field = "name", 
    show_label = TRUE,
    title = ""
){

    # graph_dfs_tree <- graph_old
    
    # graph_selected <- graph_dfs_tree %>% 
    #     activate(nodes) %>% 
    #     filter(
    #         id_alert %in% c(9, 10, 42, 41, 15, 16)
    #     ) %>% 
    #     as_tibble()
    
    


    if(show_label){
        
        if(is.numeric(alpha_label)){
            layer <- geom_node_label(
                aes(label = .data[[name_field]]),
                label.size = 0.3,
                repel = TRUE,
                size = size_label,
                label.padding = 0.4,
                alpha = alpha_label,
                stroke = 4,
                hjust = "left"
            )
        }
        else{
            layer <- geom_node_label(
                aes(
                    label = .data[[name_field]],
                    alpha = .data[[alpha_label]]
                ),
                label.size = 0.3,
                repel = TRUE,
                size = size_label,
                label.padding = 0.3,
                stroke = 4
            )
        }
        
    }
    else{
        layer <- NULL
    }
    
    
        
    ggraph(graph_dfs_tree, layout = "tree" ) +
        geom_edge_link(arrow = arrow(length = unit(2, 'mm')), 
                       end_cap = circle(3, 'mm'), start_cap = circle(3, 'mm')) +    
        layer +
        geom_node_point(
            aes(color = method),
            size = 8,
            shape = 21
        ) +
        geom_node_text(
            aes(label = .data[[node_text_field]]),
            size = 5
        ) +
        coord_flip() +
        scale_x_reverse(expand =c(-1.2,1.2)) +
        scale_y_continuous(expand =c(-1.2,1.2)) +
        theme_void() +
        theme(
            aspect.ratio = 1.3  ,
            legend.position = "top" 
        ) +
        guides(
            alpha = FALSE
        ) +
        ggtitle(title)

    
}




cross_versions <- function(examples_executed){
    
    # examples_executed <- examples_sec2_executed
    
    examples_executed_selected_fields_left <-
        examples_executed %>% select(id, name, path, output) %>%
        rename_all(
            .funs = function(x) {
                str_glue("{x}_left")
            }
        )
    
    examples_executed_selected_fields_right <-
        examples_executed %>% select(id, name, path, output) %>%
        rename_all(
            .funs = function(x) {
                str_glue("{x}_right")
            }
        )
    
    
    saida <- examples_executed_selected_fields_left %>%
        crossing(examples_executed_selected_fields_right) %>%
        filter(id_left < id_right) %>%
        mutate(diff_command =
                   map2(
                       .x = path_left,
                       .y = path_right,
                       ~ assemble_diff_command(
                           code_path_left = .x,
                           code_path_right = .y,
                           output_path = output_path,
                           output_left = output_left,
                           output_right = output_right
                       )
                   )) %>%
        mutate(lines_left = read_number_of_lines(path_left),
               lines_right = read_number_of_lines(path_right)) %>%
        mutate(
            output_diff_command = map(
                .x = diff_command,
                .f = ~ system(command =  .x)
            ),
            file_diff = str_glue("{output_path}{output_left}_{output_right}.diff")
        ) %>%
        mutate(lines_map = pmap(
            .l = list(
                file = file_diff  ,
                lines_prev_param = lines_left,
                lines_post_param = lines_right
            ),
            .f = map_lines
        ))
    
        saida

}


calculate_features <-  function(graph_old, graph_new, coordinates){
    
    # graph_new <- graphs_from_alerts_new$graph_new[[2]]
    # graph_old <- graphs_from_alerts_old$graph_old[[2]]

    alert_old <- graph_old %>% 
        activate(nodes) %>% 
        as_tibble() %T>%
        write_rds("alert_old.rds") %>% 
        select(
            beginline,
            endline,
            rule,
            id_group,
            method,
            rule_alert,
            code
        ) %>% 
        rowwise() %>% 
        mutate( code = str_flatten(code, collapse = "\n") ) %>%
        ungroup() %>% 
        left_join(
            coordinates %>% select(-new),
            by = c("beginline" = "old")
        ) %>% 
        rename(
            begin_common_line = common_line
        ) %>% 
        left_join(
            coordinates %>%  select(-new),
            by = c("endline" = "old")
        ) %>% 
        rename(
            end_common_line = common_line
        ) %>% 
        mutate(
            node = row_number()
        ) %>% 
        rename_all(
            .funs = ~str_glue("{.x}_old")
        )
    
    alert_new <- graph_new %>% 
        activate(nodes) %>% 
        as_tibble() %>%
        select(
            beginline,
            endline,
            rule,
            id_group,
            method,
            rule_alert,
            code
        ) %>% 
        rowwise() %>% 
        mutate( code = str_flatten(code, collapse = "\n") ) %>%
        ungroup() %>% 
        left_join(
            coordinates %>% select(-old),
            by = c("beginline" = "new")
        ) %>% 
        rename(
            begin_common_line = common_line
        ) %>% 
        left_join(
            coordinates %>%  select(-old),
            by = c("endline" = "new")
        ) %>% 
        rename(
            end_common_line = common_line
        ) %>% 
        mutate(
            node = row_number()
        ) %>% 
        rename_all(
            .funs = ~str_glue("{.x}_new")
        )
    
    
    match_path <- alert_old %>% 
        full_join(
            alert_new,
            by = c("node_old" = "node_new")
        ) 
    
    
    features_match_path <- match_path %>% 
        mutate(
            last_method_id_old = if_else(
                rule_old %in% c(
                    "compilation_unit", 
                    "constructor_declaration", 
                    "method"
                ), 
                id_group_old, 
                NA_integer_
            ),
            last_method_id_new = if_else(rule_new %in% c(
                "compilation_unit", 
                "constructor_declaration", 
                "method"
            ), 
            id_group_new, 
            NA_integer_
            ),
            last_method_begin_line_old = if_else(
                rule_old %in% c(
                    "compilation_unit", 
                    "constructor_declaration", 
                    "method"
                ), 
                begin_common_line_old, 
                NA_integer_
            ),
            last_method_begin_line_new = if_else(rule_new %in% c(
                "compilation_unit", 
                "constructor_declaration", 
                "method"
            ), 
            begin_common_line_new, 
            NA_integer_
            ),
            last_method_end_line_old = if_else(
                rule_old %in% c(
                    "compilation_unit", 
                    "constructor_declaration", 
                    "method"
                ), 
                end_common_line_old, 
                NA_integer_
            ),
            last_method_end_line_new = if_else(rule_new %in% c(
                "compilation_unit", 
                "constructor_declaration", 
                "method"
            ),    
            end_common_line_new, 
            NA_integer_
            ),

            last_method_code_old = if_else(
                rule_old %in% c(
                    "compilation_unit", 
                    "constructor_declaration", 
                    "method"
                ), 
                code_old, 
                NA_character_
            ),
            
            
            last_method_code_new = if_else(
                rule_new %in% c(
                    "compilation_unit", 
                    "constructor_declaration", 
                    "method"
                ), 
                code_new, 
                NA_character_
            ),
            
            last_code_new = code_new,
            last_code_old = code_old,
            
                        
            last_block_id_old = if_else(rule_old %in% c("compilation_unit","block"), id_group_old, NA_integer_),
            last_block_id_new = if_else(rule_new %in% c("compilation_unit","block"), id_group_new, NA_integer_),
            
            last_block_begin_line_old = if_else(rule_old %in% c("compilation_unit","block"), begin_common_line_old, NA_integer_),
            last_block_begin_line_new = if_else(rule_new %in% c("compilation_unit","block"), begin_common_line_new, NA_integer_),
            
            last_block_end_line_old = if_else(rule_old %in% c("compilation_unit","block"), end_common_line_old, NA_integer_),
            last_block_end_line_new = if_else(rule_new %in% c("compilation_unit","block"), end_common_line_new, NA_integer_),
            
            last_class_begin_line_old = if_else(rule_old %in% c("compilation_unit"), begin_common_line_old, NA_integer_),
            last_class_begin_line_new = if_else(rule_new %in% c("compilation_unit"), begin_common_line_new, NA_integer_),
            
            last_class_end_line_old = if_else(rule_old %in% c("compilation_unit"), end_common_line_old, NA_integer_),
            last_class_end_line_new = if_else(rule_new %in% c("compilation_unit"), end_common_line_new, NA_integer_),
            
            last_block_begin_line_old = if_else(rule_old %in% c("compilation_unit","block"), begin_common_line_old, NA_integer_),
            last_block_begin_line_new = if_else(rule_new %in% c("compilation_unit","block"), begin_common_line_new, NA_integer_),
            
            
            last_id_group_old = id_group_old,
            last_id_group_new = id_group_new,
            
            last_common_group_begin_line = if_else(
                id_group_new == id_group_old, 
                begin_common_line_old, 
                NA_integer_
            ),
            
            last_common_group_end_line = if_else(
                id_group_new == id_group_old, 
                end_common_line_old, 
                NA_integer_
            ),
            
            last_method_name_old = method_old,
            
            last_method_name_new = method_new
            
            
        ) %T>%
        write_rds(path = "features.rds") %>% 
        fill(
            last_method_id_old,
            last_method_id_new,
            last_method_begin_line_new,
            last_method_begin_line_old,
            last_method_end_line_new,
            last_method_end_line_old,
            last_id_group_old,
            last_id_group_new,
            last_block_id_old,
            last_block_id_new,
            last_block_begin_line_old,
            last_block_begin_line_new,
            last_block_end_line_old,
            last_block_end_line_new,
            begin_common_line_new,
            begin_common_line_old,
            end_common_line_new,
            end_common_line_old,
            last_common_group_begin_line,
            last_common_group_end_line,
            id_group_new,
            id_group_old,
            last_class_begin_line_old,
            last_class_begin_line_new,
            last_class_end_line_old,
            last_class_end_line_new,
            rule_alert_new, 
            rule_alert_old,
            last_method_name_old,
            last_method_name_new,
            last_method_code_new,
            last_method_code_old,
            last_code_new,
            last_code_old
            
        ) %>%
        mutate(
            same_rule = rule_alert_new == rule_alert_old,
            same_id_group = id_group_new == id_group_old,  
            same_method_group = last_method_id_new == last_method_id_old,
            same_method_name = last_method_name_old == last_method_name_new,
            same_block = last_block_id_new == last_block_id_old,
            last_common_group_mean_line = (last_common_group_begin_line + last_common_group_end_line)/2,
            mean_line_new = (begin_common_line_new + end_common_line_new)/2,
            mean_line_old = (begin_common_line_old + end_common_line_old)/2,
            mean_line_last_common_group = (last_common_group_begin_line + last_common_group_end_line)/2,
            dist_line = abs(mean_line_new - mean_line_old),
            size_last_block = last_common_group_end_line - last_common_group_begin_line,
            dist_line_normalized_block = dist_line/if_else(size_last_block == 0, 1L , size_last_block ),
            size_unit = last_class_end_line_new - last_class_begin_line_new,
            size_method = if_else(
                same_method_group,
                last_method_end_line_new - last_method_begin_line_new,
                size_unit
            ),  
            dist_line_normalized_method = dist_line/size_method,
            dist_line_normalized_unit = dist_line/size_unit,
            same_code = str_trim(last_code_old) == str_trim(last_code_new),
            same_method_code = str_trim(last_method_code_old) == str_trim(last_method_code_new)
        ) %>% 
        select(
            same_rule,
            same_id_group,
            same_method_group,
            same_method_name,
            same_block,
            same_code,
            same_method_code,
            dist_line,
            dist_line_normalized_block,
            dist_line_normalized_method,
            dist_line_normalized_unit
        ) %>% 
        slice_tail(n = 1) 

}



nodes_path_from_alert <- function(graph, id_node){
    output <- graph %>% 
        convert(to_shortest_path , from = id_node, to = 1  , mode = "out" ) %>%
        activate(nodes) %>% 
        as_tibble() %>% 
        select(id_group)
}

graph_path_from_alert <- function(graph, id_node){
    output <- graph %>% 
        convert(to_shortest_path , from = id_node, to = 1  , mode = "out" ) 
}




calculate_features_from_versions <- function(
    code_file_new = "", 
    code_file_old = "", 
    code_new = "", 
    code_old = "",
    mostra_new = c(10, 43, 17, 15, 18, 16, 45, 44),
    mostra_old = c(10, 42, 41, 15, 16, 43),
    glue_string = ""
    
    ){
    
    # code_file_old <- "C:/doutorado/AnaliseTwitter4j/match_algorithm_description/little-tree/code.java"
    # code_file_new <- "C:/doutorado/AnaliseTwitter4j/match_algorithm_description/little-tree-new/code.java"
    # 
    # code_new <- default_code_new
    # code_old <- default_code_old
    

    if(code_new != ""){
        
        write_lines(code_new, "code_files_new/new.java")
        write_lines(code_old, "code_files_old/old.java")
        code_file_new <- "code_files_new/new.java"
        code_file_old <- "code_files_old/old.java"
    }
    

    path_code_file_old <- code_file_old %>% 
        str_remove("/[^/]*$") 
    
    path_code_file_new <- code_file_new %>% 
        str_remove("/[^/]*$") 

    output_code_file_old <- path_code_file_old %>% 
        str_match("[^/]*$") 
    
    output_code_file_new <- path_code_file_new %>% 
        str_match("[^/]*$") 

    
    examples_sec2 <- tribble(
        
        ~name,                  ~path,      ~output,          
        "Simple old",  path_code_file_old ,  output_code_file_old %>% as.character(),
        "Simple new",  path_code_file_new ,  output_code_file_new %>%  as.character(),
        
    ) %>% 
        mutate(id = row_number()) 
    
    
    examples_sec2_executed <- examples_sec2 %>%
        mutate(pmd_command =
                   map2(
                       .x = path,
                       .y = output,
                       ~ assemble_pmd_command(
                           pmd_path = pmd_path,
                           code_path = .x ,
                           rule_path = rule_path,
                           output_path = output_path,
                           output = .y
                       )
                   )) %>%
        mutate(pmd_command_output = map(
            .x = pmd_command,
            .f =  ~ system(command =  .x)
        )) %>%
        mutate(pmd_output = map(.x = str_glue("{output_path}{output}.xml"), .f = read_pmd_xml))
    
    examples_sec2_crossed <- cross_versions(examples_sec2_executed) 
    
    map <- examples_sec2_crossed$lines_map[[1]] %>% 
        select(   
            old = map_remove,
            new = map_add
        )
    
    
    output_old <-  code_file_old %>% 
        str_replace(".java", ".xml") 
    
    output_new <-  code_file_new %>% 
        str_replace(".java", ".xml") 
    
    nodes_old <- read_raw_ast_nodes(
        code_location = code_file_old,
        output_location =  output_old
    )
    
    graph_old <- generate_ast_tree_from_raw_nodes(nodes_old)
    
    nodes_new <- read_raw_ast_nodes(
        code_location = code_file_new,
        output_location <-  output_new
    )
    
    graph_new <- generate_ast_tree_from_raw_nodes(nodes_new)
    
    nodes_new <- graph_new %>% 
        activate(nodes) %>% 
        as_tibble() %>% 
        rename_all(
            ~str_glue("{.x}_new")
        )
    
    nodes_old <- graph_old %>% 
        activate(nodes) %>% 
        as_tibble() %>% 
        rename_all(
            ~str_glue("{.x}_old")
        ) 
    
    map <- examples_sec2_crossed$lines_map[[1]] %>% 
        select(   
            old = map_remove,
            new = map_add
        )
    
    write_rds(map, "map.rds")
    
    map_begin <- map %>% 
        rename_all(
            ~str_glue("{.x}_begin")
        )
    
    
    map_end <- map %>% 
        rename_all(
            ~str_glue("{.x}_end")
        )
    
    
    match_nodes <- nodes_old %>% 
        left_join(
            map_begin,
            by = c("beginline_old" = "old_begin")
        ) %>% 
        left_join(
            map_end,
            by = c("endline_old" = "old_end")
        ) %>% 
        left_join(
            nodes_new,
            by = c( 
                "new_begin" = "beginline_new",
                "new_end" = "endline_new",
                "rule_old" = "rule_new"
            )
        ) %>%
        group_by(
            id_alert_old
        ) %>% 
        mutate(
            n_old = n()
        ) %>%   
        group_by(
            id_alert_new
        ) %>% 
        mutate(
            n_new = n()
        ) %>% 
        ungroup() %>% 
        filter(
            (n_old == 1 & n_new == 1) | (begincolumn_new == begincolumn_old & begincolumn_old == begincolumn_new)
        ) %>% 
        group_by(
            id_alert_old
        ) %>% 
        mutate(
            n_old = n()
        ) %>% 
        group_by(
            id_alert_new
        ) %>% 
        mutate(
            n_new = n()
        ) %>% 
        ungroup() %>% 
        select(
            id_alert_new,
            id_alert_old
        ) %>% 
        mutate(
            id_group = row_number()
        )
    
    write_rds(match_nodes, "match_nodes.rds")
    
    offset_id_group_na <- 0L
    
    graph_new_with_group <- graph_new %>% 
        activate(nodes) %>% 
        left_join(
            match_nodes,
            by = c("id_alert" = "id_alert_new" )
        ) %>% 
        select(
            -id_alert_old 
        ) %>% 
        mutate(
            mostra = case_when(
                id_alert %in% mostra_new ~ 1,
                TRUE ~ -1 
            )
        ) %>% 
        mutate(
            id_group = if_else(is.na(id_group), -row_number()-offset_id_group_na, id_group)
        )
    
    offset_id_group_na <- nrow(graph_new_with_group %>% activate(nodes) %>%  as_tibble())
    
    graph_old_with_group <- graph_old %>% 
        activate(nodes) %>% 
        left_join(
            match_nodes,
            by = c("id_alert" = "id_alert_old" )
        ) %>% 
        select(
            -id_alert_new 
        ) %>% 
        mutate(
            mostra = case_when(
                id_alert %in% mostra_old ~ 1,
                TRUE ~ -1 
            )
        ) %>% 
        mutate(
            id_group = if_else(is.na(id_group), -row_number()-offset_id_group_na, id_group)
        )        
    
    
    alerts_old <- examples_sec2_executed$pmd_output[[1]] %>% 
        rename_all(
            ~str_glue("{.x}_alert")
        ) %>% 
        mutate(
            one = 1
        )
    
    alerts_new <- examples_sec2_executed$pmd_output[[2]] %>% 
        rename_all(
            ~str_glue("{.x}_alert")
        ) %>% 
        mutate(
            one = 1
        )
    
    

    graph_old_with_alert <- graph_old_with_group %>% 
        activate(nodes) %>% 
        mutate(
            one = 1
        ) %>% 
        left_join(
            alerts_old,
            by = c(
                "beginline" = "beginline_alert", 
                "endline" = "endline_alert", 
                "begincolumn" = "begincolumn_alert", 
                "endcolumn" = "endcolumn_alert")
        ) %>% 
        mutate(
            text_alert = if_else(is.na(id_alert_alert),
                                 "",
                                 str_glue("{id_group}-{rule_alert}") %>%  as.character()
            ),
            
            text_alert_id_node = if_else(is.na(id_alert_alert),
                                         "",
                                         str_glue("{id_alert}-{rule_alert}") %>%  as.character()
            ),
            
            text_line_rule = if_else(is.na(id_alert_alert),
                                     "",
                                     str_glue("{id_alert}-{rule_alert}") %>%  as.character()
                                     
            ),
            
            glue = str_glue(glue_string)
            
            
        ) 
    
    

    
    graph_new_with_alert <- graph_new_with_group %>% 
        activate(nodes) %>% 
        mutate(
            one = 1
        ) %>% 
        left_join(
            alerts_new,
            by = c(
                "beginline" = "beginline_alert", 
                "endline" = "endline_alert", 
                "begincolumn" = "begincolumn_alert", 
                "endcolumn" = "endcolumn_alert")
        ) %>% 
        mutate(
            text_alert = if_else(is.na(id_alert_alert),
                                 "",
                                 str_glue("{id_group}-{rule_alert}") %>%  as.character()
            ),
            
            text_alert_id_node = if_else(is.na(id_alert_alert),
                                         "",
                                         str_glue("{id_alert}-{rule_alert}") %>%  as.character()
            ),
            
            glue = str_glue(glue_string)

        ) 
    

    graph_old_reverted <- graph_old_with_alert %>% 
        activate(edges) %>% 
        reroute(from = to, to = from)
    
    nodes_alerts_old <- graph_old_reverted %>% 
        activate(nodes) %>% 
        filter(!is.na(id_alert_alert)) %>% 
        select(id_alert) %>% 
        as_tibble()
    
    graphs_from_alerts_old <- nodes_alerts_old %>% 
        mutate(graph = map(.x = id_alert, .f = ~graph_path_from_alert(graph = graph_old_reverted, id_node = .x )   )) 
    
    
    graphs_from_alerts_old %<>% rename(
        id_alert_old = id_alert,
        graph_old = graph
    ) 
    
    
    graph_new_reverted <- graph_new_with_alert %>% 
        activate(edges) %>% 
        reroute(from = to, to = from)
    
    nodes_alerts_new <- graph_new_reverted %>% 
        activate(nodes) %>% 
        filter(!is.na(id_alert_alert)) %>% 
        select(id_alert) %>% 
        as_tibble()
    
    graphs_from_alerts_new <- nodes_alerts_new %>% 
        mutate(graph = map(.x = id_alert, .f = ~graph_path_from_alert(graph = graph_new_reverted, id_node = .x )   )) 
    
    
    graphs_from_alerts_new %<>% rename(
        id_alert_new = id_alert,
        graph_new = graph
    ) 
    
    coordinates <- map %>% 
        ungroup() %>% 
        mutate(common_line = row_number()) 
    
    match_alerts_alg2 <- graphs_from_alerts_new %>% 
        crossing(graphs_from_alerts_old) %>% 
        rowwise() %>% 
        mutate(
            features = calculate_features(graph_old = graph_old, graph_new = graph_new, coordinates = coordinates) %>% list()
        ) 
    
    
    saida <- list(
        versions_executed = examples_sec2_executed,
        versions_crossed = examples_sec2_crossed,
        graph_old_with_alert =  graph_old_with_alert,
        graph_new_with_alert = graph_new_with_alert,
        graph_old_with_group = graph_old_with_group,
        graph_new_with_group = graph_new_with_group,
        graphs_from_alerts_old = graphs_from_alerts_old,
        graphs_from_alerts_new = graphs_from_alerts_new,
        features = match_alerts_alg2
    )
    
    saida

}



report_features <- function(features_df, caption){
    

    old_lines <- features_df$graph_old_with_alert %>%
        activate(nodes) %>% 
        as_tibble() %>% 
        filter(!is.na(id_alert_alert)) %>% 
        select(id_alert, beginline) %>% 
        rename_with( ~str_glue("{.x}_old")) 
    
    new_lines <- features_df$graph_new_with_alert %>%
        activate(nodes) %>% 
        as_tibble() %>% 
        filter(!is.na(id_alert_alert)) %>% 
        select(id_alert, beginline) %>% 
        rename_with( ~str_glue("{.x}_new")) 
    
    
    feature_names_translation <- tribble(
        ~feature,                       ~feature_display,
        "same_rule",                   "Same Rule",
        "same_id_group",               "Same Group ID",
        "same_method_group",           "Same Method Group ID",
        "same_method_name" ,           "Same Method Name",
        "same_block",                  "Same Block",
        "same_code",                   "Same Code",
        "same_method_code",            "Same Method Code",
        "dist_line",                   "Line Distance",
        "dist_line_normalized_block"  ,"Line Distance Normalized by Block Size",
        "dist_line_normalized_method" ,"Line Distance Normalized by Method Size",
        "dist_line_normalized_unit",   "Line Distance Normalized by Compilation Unit Size"
        
        
    )
    
    
    saida_tabela <- features_df$features %>%
        unnest(features, .sep = ".") %>%
        select(
            starts_with("id_alert") | starts_with("features")
        ) %>%
        left_join(
            old_lines,
            by = c("id_alert_old" = "id_alert_old")
        ) %>%
        left_join(
            new_lines,
            by = c("id_alert_new" = "id_alert_new")
        ) %>%
        select(
            -c(id_alert_new, id_alert_old)
        ) %>%
        mutate(
            across(
                where(is.numeric) & !starts_with("beginline_"),
                ~number(.x, accuracy = 0.01)
            )
        ) %>%
        mutate(
            across(
                where(is.logical),
                as.character
            )
        ) %>%
        relocate(
            beginline_old, beginline_new
        ) %>%
        pivot_longer(
            cols = c(-beginline_old, -beginline_new),
            names_to = "feature",
            values_to = "value"
        ) %>% 
        mutate(
            line_old_line_new = str_glue("Line (Old version):{beginline_old}, Line (New version):{beginline_new}")
        ) %>%
        select(
            c(-beginline_new, -beginline_old)
        ) %>% 
        relocate(
            line_old_line_new
        ) %>% 
        mutate(
            feature = str_remove(feature, "feature.") %>% str_remove("\\.")
        ) %>% 
        left_join(
            feature_names_translation,
            by = c("feature")
        ) %>% 
        select(
            line_old_line_new,
            feature = feature_display,
            value
        )
    
    
    
    kable(saida_tabela,
          format = "latex",
          caption = caption,
          escape = TRUE,
          # booktabs = TRUE,
          # align = "r",
          # linesep = "",
          col.names = c(
              "Alert combination",
              "Feature",
              "Value"
          )
    ) %>%
        collapse_rows(columns = 1, latex_hline = "major", valign =  "top") %>% 
        kable_styling(
            latex_options = c("hold_position", "striped")
        )
    

}















