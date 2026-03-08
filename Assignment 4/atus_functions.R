# R functions:
#
make_labels <- function(x, values, labels) {
  factor(x, levels = values, labels = labels)
}
#

#
label_from_dict <- function(v_name, data, dict, variable_var  = "variable", value_var = "value", label_var = "label") {
  
  d_sub <- dict %>% filter(!!sym(variable_var) == v_name)
  
  make_labels(data %>% pull(v_name),
              d_sub %>% pull(!!sym(value_var)), 
              d_sub %>% pull(!!sym(label_var)))
}

#
label_all_from_dict <- function(data, dict) {
  cat_vars <- dict %>% filter(type == "categorical") %>% 
    distinct(variable) %>% pull(variable)
  
  M <- map_dfc(cat_vars, .f = label_from_dict, data = data, dict = dict)
  colnames(M) <- cat_vars
  
  full_data <- M %>% bind_cols(data %>% select(-all_of(cat_vars))) %>%
    select(all_of(colnames(data)))
  
  return(full_data)
}

summary_plot <- function(data, v_name, dict) {
  # Get the variable type
  # We have to use rlang::as_name(v_name) to convert it to a string
  v_name_str <- rlang::as_name(enquo(v_name))
  
  var_type <- dict %>% filter(variable == v_name_str) %>% pull(type) %>% first()
  
  # !!sym(vname) converts the string v_name into a variable name that
  # ggplot knows how to deal with.
  # For example, if v_name is the string "GENHEALTH", then
  # aes(x = !!sym(v_name)) is like typing aes(x = GENHEALTH)
  base_plot <- data %>% ggplot(aes(x = !!sym(v_name_str)) )
  
  if(var_type == "categorical") {
    # For categorical variables, make a bar plot
    final_plot <- base_plot + geom_bar() 
  } else {
    # For numeric variables, make a density plot
    final_plot <- base_plot + geom_density() 
  }
  
  return(final_plot + ggtitle(paste("Summary plot of", v_name_str)))
  
}
