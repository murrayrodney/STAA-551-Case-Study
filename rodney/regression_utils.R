track_model_perf <- function(model, new_model_name, prev_model_perf=NULL, add_formula=TRUE) {
  sum_obj <- summary(model)
  new_formula <- toString(sum_obj$terms)
  
  model_perf <- glance(model)
  model_perf$formula = new_formula
  if (add_formula) {model_perf$model_name = new_model_name}
  
  if (!is.null(prev_model_perf)) {
    prev_model_perf <- filter(prev_model_perf, model_name != new_model_name)
    model_perf <-  bind_rows(prev_model_perf, model_perf)
    
  }
  
  return(model_perf)
}


#' Get formula combinations 
#'
#' Given a vector of columns, return a dataframe with formulas for all combinations of a specified length
#' @param columns A vector of columns
#' @param n_var Number of variables to construct combinations for 
#' @param base_formula Base formula as a string for combinations to be attached to
#' @param interactions Degree of interactions to specify
#'
#' @return Returns a dataframe with all of the variables and formulas for a set of combinations
#' @export
#'
#' @examples
get_formulas <- function(columns, n_var=2, base_formula='rfft ~ 1', interactions=NULL) {
  # Get all of the possible combinations
  n_cols = length(columns)
  combs <- combinations(n=n_cols, r=n_var, v=columns)
  df <- data.frame(combs)
  
  # combine variables into one formula
  df_form <- data.frame(base_formula=base_formula)
  df <- bind_cols(df_form, df)
  df_cols <- names(df)
  
  df <- unite(df, 'formula', sep=' + ', remove=FALSE)
  if (!is.null(interactions)) {
    df <- mutate(df,
                 formula = str_replace(formula, '~ ', '~ ( '),
                 formula = paste(formula, ')^', as.character(interactions), sep=''),
                 interaction = interactions
    )
  } else {df <- mutate(df, interaction=1)}
  
  return (df)
}


#' Get all possible combinations
#'
#' Given a vector of column names, create a dataframe with all possible combinations of any length
#' @param columns Vector of columns to create combinations for
#' @param base_formula Base formula as a string for combinations to be attached to
#' @param interactions Degree of interactions
#'
#' @return Returns a dataframe with all of the variables and formulas for a set of combinations
#' @export
#'
#' @examples
get_all_formula_combs <- function(columns, base_formula='rfft ~ 1', interactions=NULL) {
  formulas <- data.frame()
  n_vars <- 1:length(columns)
  for (i in n_vars) {
    formulas <- bind_rows(formulas, 
                          get_formulas(columns, 
                                       n_var=i, 
                                       base_formula=base_formula, 
                                       interactions=interactions
                                       )
                          )
  }
  return(formulas)
}


#' Fit all models in a dataframe
#'
#' @param formulas A dataframe with all variables and formulas
#' @param data A dataframe to with the data for fitting the model
#'
#' @return A tibble with model performance for all of the specified models, uses broom::tidy
#' @export
#'
#' @examples
fit_all_models <- function(formulas, data, print_freq=1e6) {
  print('Starting loop')
  print_freq = as.integer(print_freq)
  for (i in 1:nrow(formulas)) {
    if (i %% print_freq  == 0) {print(i)}
    
    formula <- formulas$formula[i]
    model <- lm(formula, data=data)
    
    if (i == 1) {
      model_perf <- track_model_perf(model, as.character(i))
    } else {
      model_perf <- track_model_perf(model, as.character(i), model_perf)
    }
  }
  print('Finished')
  model_perf$interaction <- factor(formulas$interaction)
  
  return(model_perf)
}

calc_if_used <- function(df, columns) {
  for (col in columns) {
    df <- mutate(df, !!col := grepl(col, formula))
  }
  return(df)
}

fit_all_models_parallel <- function(formulas, data, type='FORK') {
  # Setup for parallel computing
  n_cores <- parallel::detectCores() - 1
  my_cluster <- parallel::makeCluster(n_cores, type='FORK')
  doParallel::registerDoParallel(cl=my_cluster)
  
  # Fit all the models
  model_perf <- foreach(i=1:nrow(formulas), .combine=bind_rows) %dopar% {
    formula <- formulas$formula[i]
    model <- lm(formula, data)
    track_model_perf(model, as.character(i))
  }
  
  parallel::stopCluster(my_cluster)
  return(model_perf)
}