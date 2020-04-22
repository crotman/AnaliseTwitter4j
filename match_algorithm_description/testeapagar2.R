testeReadLines <- readLines("old/code.java")



read_and_decorate_code(file){
    
    read_lines()
    
}

decorate_code <- function(strings){

    string %>% 
       enframe(name = "line", value = "code") %>% 
       mutate(
           line = as.character(line) %>%  str_pad(width = 4, side = "left"),
           code = code %>%  str_trunc(width = 73, ellipsis = "..."),
           final_code = str_glue("{line} - {code}") 
       ) %>% 
       view()
}