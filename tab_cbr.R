tab_cbr <- function(df, vars_indicators, vars_demo, ..., value = c("n", "prop"), na_rm_demo = TRUE, na_rm_resp = TRUE, resp_values = NULL) {
  
  #make sure value is correct, and in the order desired
  value <- match.arg(value, several.ok = TRUE)
  value <- sort(value)
  
  #filter, if applicable
  exprs <- quos(...)
  if (length(exprs)>0) {
    df <- df %>% 
      filter(!!!exprs)
  }
  
  #convert to long format
  df <- df %>% 
    mutate_at(vars(vars_indicators), funs(as.numeric(.))) %>% 
    select(c(vars_demo, vars_indicators)) %>% 
    gather(key = "indicator", value = "resp", !!!rlang::syms(vars_indicators))
  
  #initialize table
  tab <- df %>% 
    group_by_at(c(vars_demo, "indicator", "resp")) %>%
    summarize(n=n()) 
  if ("prop" %in% value) tab <- tab %>% mutate(prop = round(n/sum(n)*100,1))
  if (identical(value, "prop")) tab <- tab %>% select(-n)
  
  #filter NAs
  if (na_rm_resp) tab <- tab %>% filter(!is.na(resp))
  if (na_rm_demo) tab <- tab %>% filter_at(vars_demo, all_vars(!is.na(.)))
  
  #unite vars_demo columns
  tab <- tab %>% unite("demo", !!!rlang::syms(vars_demo))
  
  #unite n and prop, if applicable
  if (all(c("n", "prop") %in% value)) tab <- tab %>% unite("n_prop", n, prop)
  
  #spread by vars_demo (united)
  tab <- tab %>% spread(demo, !!rlang::sym(paste(value,collapse = "_")))
  
  #separate n and pct, if applicable
  if (all(c("n", "prop") %in% value)) {
    vars_to_separate <- names(tab)[which(!(names(tab) %in% c("indicator", "resp")))]
    for (var in vars_to_separate) {
      tab <- tab %>% separate(col = !!rlang::sym(var), 
                              into = paste(c("n", "prop"), var, sep = "---"), 
                              sep = "_")
    }
  }
  
  
  #filter resp (eg keep just 4+5)
  if (!is.null(resp_values)) {
    tab <- tab %>%
      filter(resp %in% resp_values)
  }
  
  #sum resp (eg combine 4+5)
  if (!is.null(resp_values)) {
    tab <- tab %>% 
      group_by(indicator) %>% 
      mutate_at(vars(c(starts_with("n"), starts_with("prop"))), funs(as.numeric(.))) %>% 
      summarize_all(funs(sum(.,na.rm = TRUE))) %>% 
      select(-resp)
  }
  
  
  #FIX: ordering of indicator column
  #FIX: ordering of demographic columns
  #FIX: total columns and rows
  #FIX: all possible response options kept
  #FIX: keep response option labels
  
  return(tab)
}