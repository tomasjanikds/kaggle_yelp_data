#' Prepare Correlation Matrix for Table Graph
#'
#' This function prepares a correlation matrix for creating a table graph by modifying the matrix based on a specified edge limit.
#'
#' @param correlation_matrix The correlation matrix to be prepared for the table graph.
#' @param edge_limit The threshold value for filtering the correlation matrix. Default is 0.9999.
#'
#' @return A tibble containing the prepared correlation matrix for creating a table graph.
#'
#' @details
#' The \code{prep_corr_matrix_for_tbl_graph} function modifies the provided correlation matrix to prepare it for creating a table graph.
#' It first sets the diagonal elements of the correlation matrix to zero to remove self-correlations.
#' Then, it sets the upper triangular elements of the correlation matrix to zero, as they are symmetrical duplicates.
#' Next, it sets the elements below the specified \code{edge_limit} threshold to zero to filter out weak correlations.
#' After these modifications, it removes rows and columns that have all zero values to reduce redundancy and optimize the matrix.
#' Finally, the function converts the modified correlation matrix to a tibble format and reshapes it to a long format with three columns: "from", "to", and "weight".
#' The resulting tibble contains the pairs of variables with non-zero weights, indicating the correlated relationships.
#'
#' @examples
#' # Prepare correlation matrix for table graph
#' prepared_matrix <- prep_corr_matrix_for_tbl_graph(correlation_matrix = my_cor_matrix, edge_limit = 0.95)
#'
#' @import tidyr
#' @export
prep_corr_matrix_for_tbl_graph <- function(correlation_matrix, edge_limit = 0.9999) {
  diag(correlation_matrix) <- 0
  correlation_matrix[upper.tri(correlation_matrix)] <- 0
  correlation_matrix[correlation_matrix < edge_limit] <- 0
  correlation_matrix <- correlation_matrix[rowSums(correlation_matrix) > 0, colSums(correlation_matrix) > 0] 
  correlation_matrix %>%
    as_tibble(rownames = "from") %>%
    gather(key = "to", value = "weight", -from) %>%
    filter(weight > 0)
}


#' Plot Density by Group
#'
#' This function generates density plots of a specified column in the provided data, grouped by a focus variable.
#'
#' @param data The data frame containing the data to be plotted.
#' @param col The column name or expression representing the variable to be plotted.
#' @param group_focus The specific group value to focus on for grouping the data. Default is 1.
#' @param ncol The number of columns in the facet grid. Default is 1.
#'
#' @details
#' The \code{plot_density_by} function uses \code{ggplot2} to create density plots of a specified column (\code{col}) in the provided data frame (\code{data}).
#' The data is grouped by a focus variable, with a specific group value (\code{group_focus}) being highlighted in the plots.
#' By default, the first group value is used as the focus.
#' The plots are displayed in a facet grid, with the number of columns in the grid specified by \code{ncol}.
#' The colors of the density plots are set using the \code{tq_palette} function from the \code{tidyquant} package.
#' The theme is customized using the \code{tq_theme} function.
#'
#' @examples
#' # Plot density by group, focusing on group 1
#' plot_density_by(data = my_data, col = "column_name", group_focus = 1, ncol = 2)
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom tidyquant tq_palette
#' @importFrom tidyquant tq_theme
#' @export
plot_density_by <- function(data, col, group_focus = 1, ncol = 1) {
  
  col_expr <- enquo(col)
  
  data %>%
    mutate(focus = as.character(group_lump)) %>%
    select(focus, everything()) %>%
    mutate(focus = ifelse(as.character(focus) == as.character(group_focus), 
                          as.character(group_focus), 
                          "Other")) %>%
    mutate(focus = as.factor(focus)) %>%
    
    ggplot(aes(!! col_expr, fill = focus)) +
    geom_density(alpha = 0.4) +
    facet_wrap(~ focus, ncol = ncol) +
    scale_fill_tq() +
    theme_tq()
}

#' Explain Customer Results
#'
#' This function explains the results for a specific customer position using the LIME (Local Interpretable Model-agnostic Explanations) method.
#'
#' @param cust_pos The position of the customer to explain within the user group table.
#'
#' @details
#' The \code{explain_customer} function utilizes the LIME package to generate explanations for the results of a specific customer.
#' It takes the position of the customer (\code{cust_pos}) within the user group table and extracts the corresponding row from the table.
#' The extracted row is then passed to the \code{explain} function from the LIME package, along with a pre-trained explainer object (\code{lime_result}).
#' The number of labels to consider for explanation is set to 1, and the number of features to include in the explanation is set to 4.
#' Additionally, the function performs 5000 permutations to compute feature importance scores.
#' The resulting explanation is plotted using the \code{plot_features} function from the LIME package.
#'
#' @examples
#' # Explain results for customer at position 3
#' explain_customer(cust_pos = 3)
#'
#' @import dplyr
#' @importFrom lime explain
#' @importFrom lime plot_features
#' @export
explain_customer <- function(cust_pos) {
  
  explain_result <- lime::explain(
    x = user_group_tbl %>% slice(cust_pos) %>% select(-group_lump, -neighbors, -group, -user_id),
    explainer  = lime_result,
    n_labels   = 1,
    n_features = 4, 
    n_permutations = 5000)
  
  lime::plot_features(explain_result)
}
