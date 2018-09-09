#' Create table for CBR report
#'
#' @param df a tibble
#' @param vars_indicators a character vector with names of indicator columns
#' @param vars_demo a character vector with demographic variables to disaggregate by
#' @param ... expressions to pass to dplyr::filter()
#' @param value indicates whether to include N and/or proportion values
#' @param na_rm_demo logical indicating whether to remove missing values for the demographic variables
#' @param na_rm_resp logical indicating whether to remove missing values for indicator variables
#' @param resp_values vectors of indicator values to include in the table. Default is NULL, to include all available values
#' @param total_row logical indicating whether to include a row of totals
#' @param total_col logical indicating whether to include a column of totals
#'
#' @return a tibble
#' @export
tab_cbr <- function(df, vars_indicators, vars_demo, ..., value = c("n", "prop"), na_rm_demo = TRUE, na_rm_resp = TRUE, resp_values = NULL, total_row = FALSE, total_col = FALSE) {
  
  #make sure value is correct, and in the order desired
  value <- match.arg(value, several.ok = TRUE)
  value <- sort(value)
  
  #filter, if applicable
  exprs <- quos(...)
  if (length(exprs)>0) {
    df <- df %>% 
      filter(!!!exprs)
  }
  
  #check vars_indicators
  indicators_are_factors <- df %>% select(vars_indicators) %>% map_lgl(~ "factor" %in% class(.))
  indicators_are_logicals <- df %>% select(vars_indicators) %>% map_lgl(~ "logical" %in% class(.)) 
  
  if (all(indicators_are_factors)) {
    
    #check levels are equal
    unique_levels <- df %>% select(vars_indicators) %>% map(~ levels(.)) %>% unique() 
    
    if (length(unique_levels) != 1) stop("All vars_indicators need to have the same levels.")
    
    resp_original <- unlist(unique_levels)
    resp_numeric <- 1:length(resp_original)
    
  } else if (indicators_are_logicals) {
    resp_original <- c(TRUE, FALSE)
    resp_numeric <- c(1,0)
  } else stop("All vars_indicators need to be factors or all vars_indicators need to be logicals.")
  
  #check vars_demo
  demo_are_factors <- df %>% select(vars_demo) %>% map_lgl(~ "factor" %in% class(.))
  
  if (all(demo_are_factors)) {
    vars_demo_levels <- df %>% select(vars_demo) %>% map(levels)
  } else stop("All vars_demo must be factors.")
  
  #if resp_values is numeric, convert resp to numeric, save current levels
  if (is.numeric(resp_values)) {
    df <- df %>% 
      mutate_at(vars(vars_indicators), funs(as.numeric(.)))
    resp_current_levels <- resp_numeric
  } else resp_current_levels <- resp_original
  
  
  #convert to long format
  df <- df %>% 
    select(c(vars_demo, vars_indicators)) %>% 
    gather(key = "indicator", value = "resp", !!!rlang::syms(vars_indicators)) %>% 
    mutate(indicator = factor(indicator, levels = vars_indicators),
           resp = factor(resp, levels = resp_current_levels))

  
  #initialize table
  tab <- df %>% 
    group_by_at(c(vars_demo, "indicator", "resp")) %>%
    summarize(n=n())
  if ("prop" %in% value) tab <- tab %>% mutate(prop = round(n/sum(n)*100,1))
  if (identical(value, "prop")) tab <- tab %>% select(-n)
  
  #filter NAs
  if (na_rm_resp) tab <- tab %>% filter(!is.na(resp))
  if (na_rm_demo) tab <- tab %>% filter_at(vars_demo, all_vars(!is.na(.)))
  
  #complete missing cases
  if (all(c("n", "prop") %in% value)) {
    tab <- tab %>% 
      ungroup() %>%
      complete(!!!rlang::syms(vars_demo), indicator, resp, 
               fill = list(n = 0, prop = 0))
  } else if (value == "n") {
    tab <- tab %>% 
      ungroup() %>%
      complete(!!!rlang::syms(vars_demo), indicator, resp, 
               fill = list(n = 0))
  } else if (value == "prop") {
    tab <- tab %>% 
      ungroup() %>%
      complete(!!!rlang::syms(vars_demo), indicator, resp, 
               fill = list(prop = 0))
  }

  

  #unite vars_demo columns
  tab <- tab %>% unite("demo", !!!rlang::syms(vars_demo))
  
  #unite n and prop, if applicable
  if (all(c("n", "prop") %in% value)) tab <- tab %>% unite("n_prop", n, prop)
  
  #spread by vars_demo (united)
  tab <- tab %>% spread(demo, !!rlang::sym(paste(value, collapse = "_")))
  
  #order vars_demo columns (united)
  demo_col_order <- vars_demo_levels %>% reduce(
    .f = function(x, y) {
      paste(rep(x, each = length(y)),
            rep(y, length(x)), sep = "_")
    }
  )
  col_order <- c("indicator", 
                 "resp", 
                 demo_col_order)
  tab <- tab %>% select(col_order)
  
  
  
  
  #separate n and pct, if applicable
  if (all(c("n", "prop") %in% value)) {
    vars_to_separate <- names(tab)[which(!(names(tab) %in% c("indicator", "resp")))]
    for (var in vars_to_separate) {
      tab <- tab %>% separate(col = !!rlang::sym(var), 
                              into = paste(c("n", "prop"), var, sep = "---"), 
                              sep = "_")
    }
  } else {
    tab <- tab %>% 
      ungroup() %>% 
      rename_at(.vars = vars(demo_col_order), .funs = funs(paste(value, ., sep = "---")))
  }
  
  #filter resp and sum resp (eg keep just 4+5)
  if (!is.null(resp_values)) {
    tab <- tab %>%
      filter(resp %in% resp_values) %>% 
      group_by(indicator) %>% 
      mutate_at(vars(-one_of(c("resp"))), funs(as.numeric(.))) %>%
      summarize_at(vars(-one_of(c("resp"))), funs(sum(., na.rm = TRUE))) 
  }
  
  #arrange indicator and resp (arranging resp only relevant if resp_values is NULL)
  tab <- tab %>% 
    arrange(match(indicator, vars_indicators))
  if (is.null(resp_values)) {
    tab <- tab %>% 
      arrange(match(resp, resp_original))
  }
  
  #FIX: total columns and rows
  
  
  return(tab)
}
