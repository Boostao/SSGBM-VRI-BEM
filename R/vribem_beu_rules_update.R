vribem_beu_rules_update <- function(conn, vri_bem, rules_tbl = "beu_update_rules") {
  
  #SPEC_PCT columns need to be numeric with 0 instead of NA
  update_tbl(conn, tbl_name = vri_bem, where_expr = NULL,
             set_expr = "SPEC_PCT_1 = IFNULL(TRY_CAST(SPEC_PCT_1 AS DOUBLE), 0), 
                         SPEC_PCT_2 = IFNULL(TRY_CAST(SPEC_PCT_2 AS DOUBLE), 0), 
                         SPEC_PCT_3 = IFNULL(TRY_CAST(SPEC_PCT_3 AS DOUBLE), 0), 
                         SPEC_PCT_4 = IFNULL(TRY_CAST(SPEC_PCT_4 AS DOUBLE), 0), 
                         SPEC_PCT_5 = IFNULL(TRY_CAST(SPEC_PCT_5 AS DOUBLE), 0), 
                         SPEC_PCT_6 = IFNULL(TRY_CAST(SPEC_PCT_6 AS DOUBLE), 0)")
  
  rules_colnames <- DBI::dbGetQuery(conn, sprintf("PRAGMA table_info('%s')", rules_tbl))$name

  tree_list_var <- grep("^TREE_RL_SP_CD_[0-9]$", rules_colnames)
  tree_pct_var <- grep("^TREE_RL_SP_PCT_[0-9]$", rules_colnames)
  rule_columns <- setdiff((which(rules_colnames == "INPUTS") + 1):(which(rules_colnames == "OUTPUTS") - 1), 
                          c(tree_list_var, tree_pct_var))
  
  rules_output_cols <- rules_colnames[(which(rules_colnames == "OUTPUTS")+1):length(rules_colnames)]

  rules_dt <- setDT(DBI::dbGetQuery(conn, sprintf("SELECT * FROM %s", rules_tbl)))

  # all rule column must appear in the vri_bem
  rule_not_in_vri <- setdiff(rules_colnames[rule_columns], DBI::dbGetQuery(conn, sprintf("PRAGMA table_info('%s')", vri_bem))$name)
  if (length(rule_not_in_vri) > 0) {
    stop(paste0("Following rules are not a feature in vri-bem : ", rule_not_in_vri))
  }

  # create expession for all rule column except for tree rules
  
  # for non tree rules there is 4 possibilities
  # the CONTAINS keyword creates a rule that check if the variable contains the specified value
  # the DOES NOT CONTAINS keyword creates a rule that check if the variable does not contains the specified value
  # a list of values separate by commas creates a rule that check if the variable is in the list of values
  # a single value creates a rule that check if the variable is equal to the value

  for (column in rule_columns) {
    column_name <- rules_colnames[column]
    column_name_expr <-  parse_expr(column_name)
    rules_dt[, paste0(rules_colnames[column], "_expr") := fcase(grepl("^CONTAINS", eval(column_name_expr)), sql_contains_rule(var_name = column_name, rule = eval(column_name_expr)),
                                                                grepl("^DOES NOT CONTAIN", eval(column_name_expr)), sql_does_not_contains_rule(var_name = column_name, rule = eval(column_name_expr)),
                                                                grepl(",", eval(column_name_expr)), sql_in_list_rule(var_name = column_name, rule_list = eval(column_name_expr)),
                                                                !is.na(eval(column_name_expr)), sql_equal_value_rule(var_name = column_name, rule_value = eval(column_name_expr)),
                                                                default = "TRUE")]

  }
  rules_dt[,SLOPE_MOD_expr:=fcase(SLOPE_MOD == "BLANK", "SLOPE_MOD NOT IN ('k','q','j','w','z')",
                                  default = SLOPE_MOD_expr)]
  
  # create expression for each pairs of tree list and percentage

  # for tree rules there is two possibilities
  # a list of values creates a rule that check if the total sum of percentage of any species in the list across all the pairs of species/pct variables is within the range in the corresponding pct rule column
  # a list of values separated by a > or a < sign creates a rule that check of the total sum of percentage of the species in the left-hand-side of the sign across all the pairs of species/pct variables is greater or less than the percentage for the species in the right-hand-side

  tree_list_var <- rules_colnames[tree_list_var]
  tree_pct_var <- rules_colnames[tree_pct_var]

  vri_bem_colnames <- DBI::dbGetQuery(conn, sprintf("PRAGMA table_info('%s')", vri_bem))$name
  vri_bem_species_var <- grep("^SPEC_CD_[0-9]$", vri_bem_colnames, value = T)
  vri_bem_pct_var <- grep("^SPEC_PCT_[0-9]$", vri_bem_colnames, value = T)

  for (i in seq_along(tree_list_var)) {
    tree_list_var_parse <- rlang::parse_expr(tree_list_var[i])
    tree_pct_var_parse <- rlang::parse_expr(tree_pct_var[i])
    rules_dt[, paste0(tree_list_var[i], "_expr") := fcase(grepl("<|>", eval(tree_list_var_parse)), sql_compare_pct_of_species_in_list(tree_list = eval(tree_list_var_parse), species_var = vri_bem_species_var, pct_var = vri_bem_pct_var),
                                                          !is.na(eval(tree_list_var_parse)), sql_sum_pct_of_species_in_list_is_within_range(tree_list = eval(tree_list_var_parse), tree_range = eval(tree_pct_var_parse), species_var = vri_bem_species_var, pct_var = vri_bem_pct_var),
                                                          default = "TRUE")]

  }

  # create total expression
  rules_dt[ , total_where_expr := do.call(paste, c(.SD, list(sep = " AND "))), .SDcols = c(paste0(rules_colnames[rule_columns], "_expr"), paste0(tree_list_var, "_expr"))]

  rules_dt[, total_set_expr := ""]
  for (output_col in rules_output_cols) {
    if (output_col == "BEUMC"){
      set(rules_dt, j = "total_set_expr", 
          value = fifelse(rules_dt[["total_set_expr"]] == "", 
                    paste0("BEUMC_S1 = '", rules_dt[[output_col]], "'"), 
                    paste0(rules_dt[["total_set_expr"]], ", BEUMC_S1 = '", rules_dt[[output_col]], "'")))
    } else {
      set(rules_dt, j = "total_set_expr", 
          value = fifelse(rules_dt[["total_set_expr"]] == "", 
                    paste0(output_col, " = '", rules_dt[[output_col]], "'"), 
                    paste0(rules_dt[["total_set_expr"]], ", ", output_col, " = '", rules_dt[[output_col]], "'")))
    }
  }

  # apply total expr on vri_bem and update output columns based on rules result
  for (i in seq_len(nrow(rules_dt))) {
    update_tbl(conn, tbl_name = vri_bem, 
              set_expr = rules_dt[["total_set_expr"]][i],
              where_expr = rules_dt[["total_where_expr"]][i])
  }
  
  #Make correction for cases where BEUMC_S1 now equals BEUMC_S2 (or BEUMC_S3)
  update_tbl(conn, tbl_name = vri_bem, where_expr = NULL,
             set_expr = "SDEC_1 = IFNULL(SDEC_1, 0),
                         SDEC_2 = IFNULL(SDEC_2, 0),
                         SDEC_3 = IFNULL(SDEC_3, 0)")
  
  update_tbl(conn, tbl_name = vri_bem, where_expr = NULL,
             set_expr = "SDEC_1 = CASE WHEN BEUMC_S1 = BEUMC_S2 THEN SDEC_1 + SDEC_2 
                                       WHEN BEUMC_S1 = BEUMC_S3 THEN SDEC_1 + SDEC_3
                                       ELSE SDEC_1 END,
                         BEUMC_S2 = CASE WHEN BEUMC_S1 = BEUMC_S2 THEN BEUMC_S3
                                         ELSE BEUMC_S2 END,
                         SDEC_2 = CASE WHEN BEUMC_S1 = BEUMC_S2 AND BEUMC_S3 IS NULL THEN 0 
                                       WHEN BEUMC_S1 = BEUMC_S2 AND BEUMC_S3 IS NOT NULL THEN SDEC_3
                                       ELSE SDEC_2 END,
                         BEUMC_S3 = CASE WHEN BEUMC_S1 = BEUMC_S2 OR BEUMC_S1 = BEUMC_S3 THEN NULL
                                         ELSE BEUMC_S3 END,
                         SDEC_3 = CASE WHEN BEUMC_S1 = BEUMC_S2 OR BEUMC_S1 = BEUMC_S3 THEN 0  
                                       ELSE SDEC_3 END")
}


import_rules_to_duckdb <- function(conn, rules_xl, tbl_name = "beu_update_rules") {
  # read in rules
  stopifnot(file.exists(rules_xl))
  sht <- readxl::excel_sheets(rules_xl) |> grep(pattern = "^comb.+script", ignore.case = TRUE, value = TRUE)
  rules <- readxl::read_excel(rules_xl, sht) 
  
  if (length(which(names(rules) == "INPUTS")) != 1L) {
    stop("One empty column named 'INPUTS' is expected in the rule file to mark the beginning of the columns use to create rules")
  }
  if (length(which(names(rules) == "OUTPUTS")) != 1L) {
    stop("One empty column named 'OUTPUTS' is expected in the rule file to mark the end of the columns use to create rules and the beginning of the columns use to create outputs")
  }

  duckdb::dbWriteTable(conn, name = tbl_name, value = rules, temporary = TRUE, overwrite = TRUE)
}


sql_contains_rule <- function(var_name, rule) {
  sprintf(" %s LIKE '%%%s%%' ", 
    var_name, 
    sub("^CONTAINS ", "", rule))
}

sql_does_not_contains_rule <- function(var_name, rule) {
 sprintf(" %s NOT LIKE '%%%s%%' ", 
    var_name, 
    sub("^DOES NOT CONTAIN ", "", rule))
}

sql_in_list_rule <- function(var_name, rule_list) {
  paste0(var_name, " IN ('", sapply(strsplit(gsub(" ", "", rule_list), split = ",|>|<"), \(x) paste0(x, collapse = "','")), "')")
}

sql_equal_value_rule <- function(var_name, rule_value) {
  paste0(var_name," = '", rule_value, "'")
}

sql_sum_pct_for_species_in_list <-  function(tree_list, vri_bem_colnames = NULL, species_var = NULL, pct_var = NULL) {
  if (is.null(species_var)) {
    species_var <- grep("^SPEC_CD_", vri_bem_colnames, value = T)
  }
  if (is.null(pct_var)) {
    pct_var <- grep("^SPEC_PCT_", vri_bem_colnames, value = T)
  }

  species_list <- sapply(strsplit(gsub(" ", "", tree_list), split = ",|>|<"), \(x) paste0(x, collapse = "','"))

  for (i in seq_along(species_var)) {
    if (i == 1) {
      string_expression <- paste0("TRY_CAST((", species_var[i], " IN ('", species_list, "')) AS DOUBLE) * ", pct_var[i], " ")
    } else {
      string_expression <- paste0(string_expression, " + ", paste0("TRY_CAST((", species_var[i], " IN ('", species_list, "')) AS DOUBLE) * ", pct_var[i], " "))
    }
  }

  return(string_expression)
}


sql_sum_pct_of_species_in_list_is_within_range <-  function(tree_list, tree_range, vri_bem_colnames = NULL , species_var = NULL, pct_var = NULL) {
  sum_pct_string_expression <-  sql_sum_pct_for_species_in_list(tree_list = tree_list, vri_bem_colnames = vri_bem_colnames, species_var = species_var, pct_var = pct_var)
  range_pct <- strsplit(tree_range, split = "-")

  string_expression <- character(length(range_pct))
  for (i in seq_along(range_pct)) {
    string_expression[i] <- paste0("(", sum_pct_string_expression[i], " BETWEEN ", range_pct[[i]][1], " AND ", range_pct[[i]][2], ")")
  }

  return(string_expression)
}

sql_compare_pct_of_species_in_list <- function(tree_list, vri_bem_colnames = NULL, species_var = NULL, pct_var = NULL) {
  tree_list_groups <- strsplit(tree_list, split = "<|>")
  tree_list_group_a <- sapply(tree_list_groups, function(x) x[1])
  tree_list_group_b <- sapply(tree_list_groups, function(x) x[2])

  sum_pct_group_a <-  sql_sum_pct_for_species_in_list(tree_list = tree_list_group_a, vri_bem_colnames = vri_bem_colnames, species_var = species_var, pct_var = pct_var)
  sum_pct_group_b <-  sql_sum_pct_for_species_in_list(tree_list = tree_list_group_b, vri_bem_colnames = vri_bem_colnames, species_var = species_var, pct_var = pct_var)

  return(paste0("(", sum_pct_group_a, ") ", fifelse(grepl("<", tree_list), "< ", "> "), "(", sum_pct_group_b, ")"))

}
  