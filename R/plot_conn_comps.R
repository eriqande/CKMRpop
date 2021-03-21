#' plot the graph showing the connected components
#'
#' Simple a wrapper for some tidygraph/ggraph stuff
#' @param Pairs the tibble that comes out of `compile_related_pairs()`.
#' For this function it must have, at least the
#' columns `id_1`, `id_2`, and `dom_relat`.
#' @export
plot_conn_comps <- function(Pairs) {
  fi_graph <- Pairs %>%
    mutate(`dom_relat-max_hit` = paste(dom_relat, max_hit, sep = "-")) %>%
    rename(from = id_1, to = id_2) %>%
    igraph::graph_from_data_frame(directed = FALSE) %>%
    tidygraph::as_tbl_graph()

  conn_comps <- fi_graph %>%
    igraph::components() %>%
    .$membership %>%
    enframe() %>%
    rename(cluster = value) %>%
    arrange(cluster, name) %>%
    group_by(cluster) %>%
    mutate(cluster_size = n()) %>%
    ungroup()

  fi_graph2 <- fi_graph %>%
    tidygraph::activate(nodes) %>%
    tidygraph::left_join(conn_comps, by = "name")

  # now, let's plot that thing
  pofs_clusters_plot <- ggraph::ggraph(fi_graph2, layout = "kk") +
    ggraph::geom_edge_link(
      aes(
        colour = `dom_relat-max_hit`
      )
    ) +
    ggraph::geom_node_point()



  pofs_clusters_plot
}
