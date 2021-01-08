library(tidyverse)

# slurp up all the results
res_files <- dir(path = "cluster_runs", pattern = "cz_.*rds", full.names = TRUE)
names(res_files) <- basename(res_files)

res_list <- lapply(res_files, read_rds)

# get the annual census sizes each year for all the simulations
ann_pop_size_list <- lapply(res_list, function(x) {
  x$pop_sizes %>%
    mutate(cz = x$cohort_size)
})
names(ann_pop_size_list) <- 1:60

ann_pop_size_tib <- bind_rows(ann_pop_size_list, .id = "sim") %>%
  mutate(
    expected_pop_size = case_when(
      cz == 1110000 ~ "1.5 million",
      cz == 740000 ~ "1 million",
      cz == 370000 ~ "0.5 million"
    )
  )

sizes <- tibble(
  expected_pop_size = c("1.5 million", "1 million", "0.5 million"),
  size = c(1.5e6, 1e6, 0.5e6)
)

# Now, just plot the annual total population sizes and overlay
# the expected values:
gann <- ggplot(
  ann_pop_size_tib,
  aes(
    x = year,
    y = tot_pop,
    colour = expected_pop_size,
    group = sim
  )
) +
  geom_line(size = 0.4) +
  facet_wrap(~expected_pop_size, scales = "free_y", ncol = 1) +
  geom_hline(data = sizes, aes(yintercept = size))


ggsave(gann, filename = "cluster_runs/schrodingers-snappers.pdf", width = 6, height = 8)

relat_sum_tib <- tibble(
  cz = map_int(res_list, function(x) x$cohort_size),
  mean_pop = map_dbl(res_list, function(x) mean(x$pop_sizes$tot_pop)),
  tot_samp_size = map_int(res_list, function(x) nrow(x$ds_samples)),
  relat_list = map(res_list, function(x) x$ds_relat_counts$highly_summarised)
)

relat_counts <- relat_sum_tib %>%
  unnest(cols = c(relat_list)) %>%
  mutate(
    relat_cat = str_c(dom_relat, max_hit, sep = "-"),
    pop_size = case_when(
      cz == 1110000 ~ "1.5 million",
      cz == 740000 ~ "1 million",
      cz == 370000 ~ "0.5 million"
    )
  )

set.seed(15)
g <- ggplot(relat_counts, aes(x = n, fill = pop_size)) +
  #geom_histogram(binwidth = 10, alpha = 0.4, colour = "black") +
  geom_jitter(
    aes(y = 1),
    width = 0,
    height = 0.5,
    shape = 21,
    stroke = 0.2,
    colour = "black",
    size = 2
  ) +
  facet_wrap(~ relat_cat, scales = "free")


ggsave(g, filename = "cluster_runs/relat-category-counts.pdf", width = 10, height = 8)


#### Now look specifically at half-sibs born in different years ####

only_half_sibs <- relat_sum_tib %>%
  mutate(
    sib_diff_borns = map_int(res_list, function(x) x$ds_pairs %>% filter(dom_relat == "Si", max_hit == 1, born_year_1 != born_year_2) %>% nrow(.))
  ) %>%
  unnest(cols = c(sib_diff_borns)) %>%
  mutate(
    pop_size = case_when(
      cz == 1110000 ~ "1.5 million",
      cz == 740000 ~ "1 million",
      cz == 370000 ~ "0.5 million"
    )
  )


g2 <- ggplot(only_half_sibs, aes(x = sib_diff_borns, fill = pop_size)) +
  geom_histogram(binwidth = 3, colour = "black")

ggsave(g2, filename = "cluster_runs/diff-born-year-half-sibs.pdf", width = 5, height = 4)


#### Make a table of relationship counts ####

relat_counts %>%
  select(
    pop_size,
    mean_pop,
    relat_cat,
    n
  ) %>%
  pivot_wider(
    names_from = relat_cat,
    values_from = n,
    values_fill = 0
  ) %>%
  write_csv("all-pairs-counts.csv")

#### Make another of the Si-1, A-2, and GP-1's with different born years ####


AllPairs <- relat_sum_tib %>%
  mutate(
    sim_num = 1:n(),
    all_pairs = map(res_list, function(x) x$ds_pairs)
  ) %>%
  unnest(cols = c(all_pairs)) %>%
  mutate(
    pop_size = case_when(
      cz == 1110000 ~ "1.5 million",
      cz == 740000 ~ "1 million",
      cz == 370000 ~ "0.5 million"
    )
  )

# now summarise that
tmp <- AllPairs %>%
  mutate(
    relat_cat = str_c(dom_relat, max_hit, sep = "-")
  ) %>%
  filter(relat_cat %in% c("Si-1", "A-2", "GP-1")) %>%
  select(sim_num, pop_size, mean_pop, relat_cat, born_year_1, born_year_2, everything()) %>%
  filter(born_year_2 != born_year_1) %>%
  count(sim_num, pop_size, mean_pop, relat_cat) %>%
  pivot_wider(
    names_from = relat_cat,
    values_from = n,
    values_fill = 0
  )

write_csv(tmp, "different-born-year-Si1-A2-GP1-counts.csv")
