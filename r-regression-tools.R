require(dplyr)

all_pairs <- function(output, variables) {
  # Function to produce formula with all first-order marginal effects
  # e.g. output = A + B + C + A*B + A*C + B*C

  # Get list of all possible pairs of variables
  pairs <- expand.grid(variables, variables) %>%
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

  # Create right hand side of formula
  formula_rhs <- stringr::str_replace_all(toString(c(variables, pairs$comb)), ", ", " + ")
  
  # Create left hand side of formula
  formula_lhs <- paste(output, "~")
  
  # Combine both sides
  formula_string = paste(formula_lhs, formula_rhs, sep = "")
  
  # interpret as a formula
  final_formula <- as.formula(eval(parse(text = formula_string)))
  return(final_formula)
}

load_with_ref <- function(file, data_sheet = 1, ref_sheet = 2) {
  # Function for loading data and defining the reference group
  # ready for logistic regression
  
  # Load data
  data <- readxl::read_excel(file, sheet = data_sheet)
  
  # Define reference group
  ref_group <- readxl::read_excel(file,
                                  sheet = "reference")
  
  for (col in colnames(ref_group)){
    ref = ref_group[col][[1]]
    ##cat(col, ":", ref,"\n")
    data[[col]] <- relevel(factor(data[[col]]), ref = ref)
  }
  return(data)
}

# Test run if this is the main script
if (interactive()) {
  variables <- c("A", "B", "C", "D")
  my_output = "Output"
  all_pairs(my_output, variables)
}