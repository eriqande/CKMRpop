

#' @importFrom dplyr anti_join arrange bind_cols bind_rows ends_with everything filter group_by inner_join left_join mutate n pull recode rename rename_all select slice ungroup
#' @importFrom ggplot2 element_blank facet_grid geom_hline geom_segment geom_vline ggplot theme theme_bw
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom readr cols read_delim read_lines read_table2
#' @importFrom stats runif
#' @importFrom stringr str_detect str_replace str_split str_split_fixed
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyr extract gather pivot_longer unnest
#' @useDynLib CKMRpop
NULL




# quiets concerns of R CMD check re: the . and other column names
# that appear in dplyr chains
if (getRversion() >= "2.15.1")  {
  utils::globalVariables(
    c(
      ".",
      "ID",
      "ID_1",
      "ID_2",
      "X2",
      "X3",
      "aes",
      "age",
      "age_1",
      "age_2",
      "born_year_1",
      "born_year_2",
      "extract",
      "ma",
      "ma_1",
      "ma_2",
      "nojit_age1",
      "nojit_age2",
      "pa",
      "pa_1",
      "pa_2",
      "pair_type",
      "sampling_year",
      "sampling_year_1",
      "sampling_year_2",
      "sex",
      "sex_1",
      "sex_2",
      "sy_1",
      "sy_2",
      "syears",
      "tag",
      "xjit",
      "xlab",
      "year",
      "yjit",
      "ylab"
    )
  )
}
