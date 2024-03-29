require(dplyr)

write_formula <- function(output, variables, pairs = FALSE, quote_wrap = FALSE) {
  # Function to produce formula with all first-order marginal effects
  # e.g. output = A + B + C + A*B + A*C + B*C

  if (quote_wrap) {
    variables = paste("`", variables, "`", sep = "")
  }
  
  if (pairs) {
    # Get list of all possible pairs of variables
    all_pairs <- expand.grid(variables, variables) %>%
      # Don't include matching pairs
      filter(Var1 != Var2) %>%
      mutate(
        # Interpret values as characters
        Var1 = as.character(Var1),
        Var2 = as.character(Var2),
        # Interpret put them in alphabetical order in order to 
        # remove doubles e.g. AB and BA 
        Nvar1 = case_when(
          Var1 < Var2 ~ Var1,
          Var1 > Var2 ~ Var2,
          TRUE ~ "ERROR"
        ),
        Nvar2 = case_when(
          Var1 < Var2 ~ Var2,
          Var1 > Var2 ~ Var1,
          TRUE ~ "ERROR"
        ), 
        # Combine pair in one string E.g. A, B -> A*B
        comb = paste(Nvar1, Nvar2, sep = "*")
      ) %>%
      select(comb) %>%
      distinct()
  } else {
    all_pairs <- data.frame(matrix(ncol = 1, nrow = 0))
    colnames(all_pairs) <- c('comb')
  }

  # Create right hand side of formula
  formula_rhs <- stringr::str_replace_all(toString(c(variables, all_pairs$comb)), ", ", " + ")
  
  # Create left hand side of formula
  formula_lhs <- paste(output, "~")
  
  # Combine both sides
  formula_string = paste(formula_lhs, formula_rhs, sep = "")
  
  # interpret as a formula
  final_formula <- as.formula(eval(parse(text = formula_string)))
  return(final_formula)
}

load_with_ref <- function(file, 
                          data_sheet = 1, 
                          ref_sheet = 2,
                          seperate_ref_sheet = NULL,
                          add_colon = TRUE,
                          return_ref = FALSE
                          ) {
  # Function for loading data and defining the reference group
  # ready for logistic regression
  
  # Load data
  file_type = str_split(file, "[.]",2)[[1]][2]
  if (file_type == "xlsx") {
    data <- readxl::read_excel(file, sheet = data_sheet)
  } else if (file_type == "parquet") {
    library(arrow)
    data <- read_parquet(file)
  }
  
  if (is.null(seperate_ref_sheet)) {
    # Define reference group
    ref_group <- readxl::read_excel(file,
                                    sheet = ref_sheet)    
  } else {
    ref_group <- readxl::read_excel(seperate_ref_sheet,
                                    sheet = ref_sheet)  
  }
  
  for (column_name in colnames(ref_group)){
    ref = ref_group[column_name][[1]]
    cat(column_name, ":", ref,"\n")
    data[[column_name]] <- relevel(factor(data[[column_name]]), ref = ref)
  }
  
  if (add_colon) {
    # Add colon to variable names to make processing regression results easier
    colnames(data) <- paste(colnames(data), ":", sep = "")
  } 
  
  if (return_ref) {
    # reshape reference group data.frame into 
    return_ref <- reshape2::melt(ref_group, id = 0, value.name = "Reference Value", 
                                 variable.name = "Variable")
    
    return(list(data, return_ref))
  } else {
    return(data)
  }
}

var_imp <- function(model, dp = 2) {
  # Get sorted variable importance of a model
  
  varImp <- caret::varImp(model) %>% 
    arrange(desc(Overall)) %>% 
    mutate(Rank = row_number(),
           Importance = round(Overall, dp))
  
  varImp <- cbind(Variable = rownames(varImp), 
                  varImp)
  varImp <- varImp %>%
    select(c("Variable", "Importance", "Rank"))
  
  # Reset index
  rownames(varImp) <- NULL
  
  return(varImp)
}

odds_ratios <- function(model, 
                        ref = NULL, 
                        rm_na = FALSE) {
  # Get tidy odds ratio results with confidence intervals
  
  # calculate odds ratio and CI
  odds_vals <- round(exp(coef(model))[-1],3)
  conf_ints <- exp(confint(model))[-1, ]
  
  # Create variable base
  varnames <- rownames(data.frame(exp(coef(model))[-1]))
  
  variable_base <- data.frame(
    FullVar = varnames,
    `Variable.group.1` = str_split(varnames, ':', simplify = TRUE)[,1],
    `Variable.group.2` = str_split(varnames, ':', simplify = TRUE)[,3],
    `Variable.1` = str_split(varnames, ':', simplify = TRUE)[,2],
    `Variable.2` = str_split(varnames, ':', simplify = TRUE)[,4]) %>%
    mutate(
      join = case_when(
        `Variable.group.2` == "" ~ "",
        TRUE ~ ":"
        ),
      `Variable.group` = paste(`Variable.group.1`, join, `Variable.group.2`, sep = ""),
      `Variable` = paste(`Variable.1`, join, `Variable.2`, sep = ""),
    ) %>%
    select(c("FullVar", "Variable.group", "Variable"))
  print(variable_base)
  # %>%
  # mutate(
  #   `Variable group` = paste(`Variable group 1`, `Variable group 2`, sep = ":"),
  #   `Variable` = paste(`Variable 1`, `Variable 2`, sep = ":")
  # )
  variable_base$`Variable.group` <- gsub("`", "",as.character(variable_base$`Variable.group`))
  variable_base$Variable <- gsub("`","",as.character(variable_base$Variable))
  
  odds_ratios <- variable_base %>%
    full_join(
      data.frame(
        FullVar = rownames(data.frame(odds_vals)),
        `Odds Ratio` = odds_vals
      )
    ) %>%
    full_join(
      data.frame(
        FullVar = rownames(data.frame(conf_ints)),
        `Conf int lower` = round(conf_ints[,1],3),
        `Conf int upper` = round(conf_ints[,2],3)
      )
    ) %>%
    select(c("Variable.group", "Variable",  "Odds.Ratio", 
             "Conf.int.lower", "Conf.int.upper"))
  
  colnames(odds_ratios) = c("Variable group", "Variable", "Odds Ratio", 
                         "Conf int lower", "Conf int upper")
  if (rm_na) {
    # Remove rows with NAs
    odds_ratios <- na.omit(odds_ratios)
  }
  
  if (!is.null(ref)) {
    
    used_ref <- ref %>%
      # filter to just relevant reference values
      filter(`Variable` %in% odds_ratios$`Variable group`) %>%
      # Update column names and refine reference ratio values
      mutate(
        `Variable group` = Variable,
        Variable = `Reference Value`,
        `Odds Ratio` = 1,
        `Conf int lower` = "Ref",
        `Conf int upper` = "Ref"
        ) %>%
      select(c("Variable group", "Variable", "Odds Ratio", 
               "Conf int lower", "Conf int upper"))
    
    # Join to calculated risk ratios
    odds_ratios <- rbind(odds_ratios, used_ref)
  }
  
  # Sort results alphabetically by group then outcome
  odds_ratios_sorted <- odds_ratios %>% 
    arrange(`Variable group`) %>% 
    group_by(`Variable group`) %>% 
    arrange(`Variable`, .by_group = TRUE)
  
  return(odds_ratios_sorted)
}

# Test run if this is the main script
if (interactive()) {
  variables <- c("A", "B", "C", "D")
  my_output = "Output"
  write_formula(my_output, variables)
}