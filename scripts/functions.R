load_data <- function(polls_files) {
  polls_files %>% 
    map(read_sav) %>% 
    map(as_factor)
}
