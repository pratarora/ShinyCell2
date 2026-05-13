#' Write DEG readRDS lines for ui.R
#'
#' @rdname wrUIloadDEG
#' @export wrUIloadDEG
#'
wrUIloadDEG <- function() {
  glue::glue(
    'deg         = readRDS("./deg.rds")\n',
    'deg_summary = readRDS("./deg_summary.rds")\n',
    '\n'
  )
}

#' Write DEG readRDS lines for server.R
#'
#' @rdname wrSVloadDEG
#' @export wrSVloadDEG
#'
wrSVloadDEG <- function() {
  glue::glue(
    'deg         = readRDS("./deg.rds")\n',
    'deg_summary = readRDS("./deg_summary.rds")\n',
    '\n'
  )
}

#' Write LIANA readRDS line for ui.R
#'
#' @rdname wrUIloadLR
#' @export wrUIloadLR
#'
wrUIloadLR <- function() {
  glue::glue(
    'liana = readRDS("./liana.rds")\n',
    '\n'
  )
}

#' Write LIANA readRDS line for server.R
#'
#' @rdname wrSVloadLR
#' @export wrSVloadLR
#'
wrSVloadLR <- function() {
  glue::glue(
    'liana = readRDS("./liana.rds")\n',
    '\n'
  )
}

#' Write DEG tab UI code for ui.R
#'
#' Expects deg.rds (columns: comparison, sub_cell_type, log2FoldChange, ...)
#' and deg_summary.rds to be present in the shiny app directory.
#'
#' @param default.comparison Default comparison to select on load. NULL means
#'   the first comparison in alphabetical order.
#' @param default.celltype Default sub cell type to select on load. NULL means
#'   the first cell type in alphabetical order.
#'
#' @rdname wrUImainDEG
#' @export wrUImainDEG
#'
wrUImainDEG <- function(default.comparison = NULL, default.celltype = NULL) {
  sel_comp <- if (is.null(default.comparison))
    'sort(unique(deg$comparison))[1]'
  else
    paste0('"', default.comparison, '"')

  sel_ct <- if (is.null(default.celltype))
    'sort(unique(deg$sub_cell_type))[1]'
  else
    paste0('"', default.celltype, '"')

  glue::glue(
    '\n\n',
    '### Tab: Differentially Expressed Genes\n',
    'tabPanel(\n',
    '  HTML("Differentially Expressed Genes"),\n',
    '  h4("Differentially Expressed Genes"),\n',
    '  p("Explore genes differentially expressed between conditions."),\n',
    '  br(),\n',
    '  h5("DE Results by Comparison"),\n',
    '  fluidRow(\n',
    '    column(4, selectInput("deg_comparison", "Comparison:",\n',
    '                          choices  = sort(unique(deg$comparison)),\n',
    '                          selected = {sel_comp})),\n',
    '    column(4, selectInput("deg_celltype", "Sub Cell Type:",\n',
    '                          choices  = sort(unique(deg$sub_cell_type)),\n',
    '                          selected = {sel_ct})),\n',
    '    column(4, numericInput("deg_lfc", "Min |log2 Fold Change|:",\n',
    '                           value = 0, min = 0, max = 10, step = 0.1))\n',
    '  ),\n',
    '  fluidRow(\n',
    '    column(12, DT::dataTableOutput("deg_table"))\n',
    '  ),\n',
    '  br(), hr(),\n',
    '  h4("DE Summary (all comparisons & sub-cell types)"),\n',
    '  fluidRow(\n',
    '    column(12, DT::dataTableOutput("deg_summary_table"))\n',
    '  ),\n',
    '  br()\n',
    ')      # End of DEG tab\n',
    '  \n'
  )
}

#' Write DEG server-side code for server.R
#'
#' @rdname wrSVmainDEG
#' @export wrSVmainDEG
#'
wrSVmainDEG <- function() {
  glue::glue(
    '\n\n\n',
    '### Functions for DEG tab\n',
    'output$deg_table <- DT::renderDataTable(server = FALSE, {{\n',
    '  dt <- deg[deg$comparison == input$deg_comparison &\n',
    '              deg$sub_cell_type == input$deg_celltype, ]\n',
    '  dt <- dt[abs(dt$log2FoldChange) >= input$deg_lfc, ]\n',
    '  DT::datatable(\n',
    '    dt, rownames = FALSE, extensions = "Buttons",\n',
    '    filter  = list(position = "top", clear = TRUE),\n',
    '    options = list(\n',
    '      pageLength = 15,\n',
    '      dom        = "Bfrtip",\n',
    '      buttons    = c("copy", "csv", "excel", "pdf"),\n',
    '      scrollX    = TRUE\n',
    '    )\n',
    '  )\n',
    '}})\n',
    '\n',
    'output$deg_summary_table <- DT::renderDataTable(server = FALSE, {{\n',
    '  DT::datatable(\n',
    '    deg_summary, rownames = FALSE, extensions = "Buttons",\n',
    '    filter  = list(position = "top", clear = TRUE),\n',
    '    options = list(\n',
    '      pageLength = 25,\n',
    '      dom        = "Bfrtip",\n',
    '      buttons    = c("copy", "csv", "excel", "pdf"),\n',
    '      scrollX    = TRUE\n',
    '    )\n',
    '  )\n',
    '}})\n',
    '\n\n'
  )
}

#' Write LR Analysis tab UI code for ui.R
#'
#' Expects liana.rds (columns: group, source, target, ligand_complex,
#' receptor_complex, magnitude_rank, spec_weight, ...) to be present in
#' the shiny app directory.
#'
#' @param default.groups Character vector of groups (conditions) to pre-select.
#'   NULL means all groups are selected.
#' @param default.sources Character vector of source cell types to pre-select.
#'   NULL means no pre-selection (show all).
#' @param default.targets Character vector of target cell types to pre-select.
#'   NULL means no pre-selection (show all).
#' @param default.rank Default max magnitude_rank cutoff (0–1). Default 0.05.
#' @param default.top_n Default number of top LR pairs to plot. Default 25.
#'
#' @rdname wrUImainLR
#' @export wrUImainLR
#'
wrUImainLR <- function(default.groups  = NULL,
                       default.sources = NULL,
                       default.targets = NULL,
                       default.rank    = 0.05,
                       default.top_n   = 25) {
  # choices_groups drives both the display order in the checkboxes AND
  # (via input$lr_groups) the facet column order in the dotplot.
  choices_groups <- if (is.null(default.groups))
    'sort(unique(liana$group))'
  else
    paste0('c("', paste(default.groups, collapse = '", "'), '")')

  sel_groups <- choices_groups   # pre-select same set in the same order

  sel_sources <- if (is.null(default.sources))
    'NULL'
  else
    paste0('c("', paste(default.sources, collapse = '", "'), '")')

  sel_targets <- if (is.null(default.targets))
    'NULL'
  else
    paste0('c("', paste(default.targets, collapse = '", "'), '")')

  glue::glue(
    '\n\n',
    '### Tab: LR Analysis (LIANA)\n',
    'tabPanel(\n',
    '  HTML("LR Analysis"),\n',
    '  h4("Ligand-Receptor Analysis (LIANA+)"),\n',
    '  p("Explore ligand-receptor interactions computed with LIANA+. ",\n',
    '    "Lower ", strong("magnitude_rank"), " = more significant. ",\n',
    '    "Higher ", strong("spec_weight"), " = more cell-type specific."),\n',
    '  br(),\n',
    '  wellPanel(\n',
    '    h5("Filters"),\n',
    '    fluidRow(\n',
    '      column(3,\n',
    '        checkboxGroupInput("lr_groups", "Condition(s):",\n',
    '          choices  = {choices_groups},\n',
    '          selected = {sel_groups})\n',
    '      ),\n',
    '      column(3,\n',
    '        selectizeInput("lr_sources", "Source cell type(s):",\n',
    '          choices  = sort(unique(liana$source)),\n',
    '          selected = {sel_sources},\n',
    '          multiple = TRUE,\n',
    '          options  = list(placeholder = "All sources"))\n',
    '      ),\n',
    '      column(3,\n',
    '        selectizeInput("lr_targets", "Target cell type(s):",\n',
    '          choices  = sort(unique(liana$target)),\n',
    '          selected = {sel_targets},\n',
    '          multiple = TRUE,\n',
    '          options  = list(placeholder = "All targets"))\n',
    '      ),\n',
    '      column(3,\n',
    '        textInput("lr_gene", "Gene (ligand or receptor):",\n',
    '                  value = "", placeholder = "e.g. GeneA, GeneB"),\n',
    '        sliderInput("lr_rank", "Max magnitude_rank:",\n',
    '                    min = 0, max = 1, value = {default.rank}, step = 0.01)\n',
    '      )\n',
    '    )\n',
    '  ),\n',
    '  wellPanel(\n',
    '    h5("Dotplot controls"),\n',
    '    fluidRow(\n',
    '      column(3,\n',
    '        numericInput("lr_top_n", "Top N LR pairs to plot:",\n',
    '                     value = {default.top_n}, min = 5, max = 100, step = 5)\n',
    '      ),\n',
    '      column(3,\n',
    '        checkboxInput("lr_facet", "Facet by condition", value = TRUE)\n',
    '      ),\n',
    '      column(2,\n',
    '        radioButtons("lr_plot_fmt", "Download format:",\n',
    '                     choices = c("pdf", "png"), selected = "pdf", inline = TRUE)\n',
    '      ),\n',
    '      column(2,\n',
    '        numericInput("lr_plot_w", "Width (in):",  value = 14, min = 4, max = 40, step = 1),\n',
    '        numericInput("lr_plot_h", "Height (in):", value = 10, min = 4, max = 40, step = 1)\n',
    '      ),\n',
    '      column(2,\n',
    '        br(),\n',
    '        downloadButton("lr_dotplot.dl", "Download plot")\n',
    '      )\n',
    '    )\n',
    '  ),\n',
    '  h5("Dotplot"),\n',
    '  uiOutput("lr_dotplot.ui"),\n',
    '  br(), hr(),\n',
    '  h5("Interaction table (filtered)"),\n',
    '  p(em("Use the column-search boxes to further filter by any field.")),\n',
    '  fluidRow(\n',
    '    column(12, DT::dataTableOutput("lr_table"))\n',
    '  ),\n',
    '  br()\n',
    ')      # End of LR tab\n',
    '  \n'
  )
}

#' Write LR Analysis server-side code for server.R
#'
#' Requires dplyr and tidyr to be installed in the Shiny app environment.
#' The pipe operator %>% is supplied by magrittr (loaded via shinyFunc.R).
#'
#' @rdname wrSVmainLR
#' @export wrSVmainLR
#'
wrSVmainLR <- function() {
  glue::glue(
    '\n\n\n',
    '### Functions for LR Analysis tab\n',
    '\n',
    '# Reactive: filtered LIANA table\n',
    'liana_filtered <- reactive({{\n',
    '  df <- as.data.frame(liana)\n',
    '  if (!is.null(input$lr_groups) && length(input$lr_groups) > 0)\n',
    '    df <- df[df$group %in% input$lr_groups, , drop = FALSE]\n',
    '  if (!is.null(input$lr_sources) && length(input$lr_sources) > 0)\n',
    '    df <- df[df$source %in% input$lr_sources, , drop = FALSE]\n',
    '  if (!is.null(input$lr_targets) && length(input$lr_targets) > 0)\n',
    '    df <- df[df$target %in% input$lr_targets, , drop = FALSE]\n',
    '  gene_q <- trimws(input$lr_gene)\n',
    '  if (nchar(gene_q) > 0) {{\n',
    '    tokens <- strsplit(gene_q, "[,[:space:]]+")[[1]]\n',
    '    tokens <- tokens[nchar(tokens) > 0]\n',
    '    pat    <- paste(tokens, collapse = "|")\n',
    '    df     <- df[grepl(pat, df$ligand_complex,   ignore.case = TRUE) |\n',
    '                 grepl(pat, df$receptor_complex, ignore.case = TRUE), , drop = FALSE]\n',
    '  }}\n',
    '  df <- df[!is.na(df$magnitude_rank) & df$magnitude_rank <= input$lr_rank, , drop = FALSE]\n',
    '  df[order(df$magnitude_rank), ]\n',
    '}})\n',
    '\n',
    '# DT table\n',
    'output$lr_table <- DT::renderDataTable(server = TRUE, {{\n',
    '  df <- liana_filtered()\n',
    '  num_cols <- intersect(c("lr_means","expr_prod","scaled_weight","lr_logfc",\n',
    '                          "spec_weight","lrscore","magnitude_rank"), names(df))\n',
    '  df[num_cols] <- lapply(df[num_cols], function(x) round(x, 5))\n',
    '  DT::datatable(\n',
    '    df, rownames = FALSE, extensions = "Buttons",\n',
    '    filter  = list(position = "top", clear = TRUE),\n',
    '    options = list(\n',
    '      pageLength = 20,\n',
    '      dom        = "Bfrtip",\n',
    '      buttons    = c("copy", "csv", "excel"),\n',
    '      scrollX    = TRUE\n',
    '    )\n',
    '  )\n',
    '}})\n',
    '\n',
    '# Dotplot helper (shared by renderPlot and downloadHandler)\n',
    'make_lr_dotplot <- function(df, top_n) {{\n',
    '  req(nrow(df) > 0)\n',
    '  # Respect the checkbox order (set by choices=) rather than re-sorting\n',
    '  grp_levels <- unique(c(input$lr_groups, as.character(df$group)))\n',
    '  df$group   <- factor(df$group, levels = grp_levels)\n',
    '  df$lr_pair <- paste0(df$ligand_complex, " \u2192 ", df$receptor_complex)\n',
    '  top_pairs <- df %>%\n',
    '    dplyr::group_by(lr_pair) %>%\n',
    '    dplyr::summarise(mean_rank = mean(magnitude_rank, na.rm = TRUE), .groups = "drop") %>%\n',
    '    dplyr::slice_min(mean_rank, n = top_n, with_ties = FALSE) %>%\n',
    '    dplyr::pull(lr_pair)\n',
    '  df <- df[df$lr_pair %in% top_pairs, ]\n',
    '  req(nrow(df) > 0)\n',
    '  pair_order <- df %>%\n',
    '    dplyr::group_by(lr_pair) %>%\n',
    '    dplyr::summarise(m = mean(magnitude_rank, na.rm = TRUE), .groups = "drop") %>%\n',
    '    dplyr::arrange(m) %>%\n',
    '    dplyr::pull(lr_pair)\n',
    '  df$lr_pair <- factor(df$lr_pair, levels = rev(pair_order))\n',
    '  df$source  <- factor(df$source, levels = sort(unique(as.character(df$source))))\n',
    '  df$target  <- factor(df$target, levels = sort(unique(as.character(df$target))))\n',
    '  df <- tidyr::complete(df, group, source, target, lr_pair)\n',
    '  df$neg_log_rank <- -log10(pmax(df$magnitude_rank, 1e-10))\n',
    '  ggplot(df, aes(x = source, y = lr_pair,\n',
    '                 colour = neg_log_rank, size = spec_weight)) +\n',
    '    geom_point(na.rm = TRUE) +\n',
    '    scale_colour_viridis_c(\n',
    '      name      = expression(-log[10](magnitude~rank)),\n',
    '      option    = "viridis",\n',
    '      direction = -1,\n',
    '      na.value  = NA\n',
    '    ) +\n',
    '    scale_size_continuous(\n',
    '      name  = "Specificity\\nweight",\n',
    '      range = c(1, 7)\n',
    '    ) +\n',
    '    facet_grid(rows = vars(target), cols = vars(group),\n',
    '               scales = "free", space = "free") +\n',
    '    theme_bw(base_size = 11) +\n',
    '    theme(\n',
    '      axis.text.x      = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),\n',
    '      axis.text.y      = element_text(size = 8),\n',
    '      axis.title.x     = element_text(size = 10),\n',
    '      axis.title.y     = element_text(size = 10),\n',
    '      strip.text.x     = element_text(size = 8, face = "bold", colour = "gray20"),\n',
    '      strip.text.y     = element_text(size = 7, angle = 0, hjust = 0),\n',
    '      strip.background = element_rect(fill = NA, colour = "grey70"),\n',
    '      panel.spacing    = unit(0.08, "lines"),\n',
    '      legend.position  = "right",\n',
    '      legend.title     = element_text(size = 9),\n',
    '      legend.text      = element_text(size = 8),\n',
    '      plot.title       = element_text(hjust = 0.5, size = 12, colour = "gray20"),\n',
    '      plot.subtitle    = element_text(hjust = 0.5, size = 9, colour = "gray40")\n',
    '    ) +\n',
    '    labs(\n',
    '      x        = "Source",\n',
    '      y        = "Interactions (Ligand \u2192 Receptor)",\n',
    '      subtitle = "Row facets = Target  |  Column facets = Condition"\n',
    '    )\n',
    '}}\n',
    '\n',
    'output$lr_dotplot <- renderPlot({{\n',
    '  make_lr_dotplot(liana_filtered(), top_n = input$lr_top_n)\n',
    '}})\n',
    '\n',
    'output$lr_dotplot.ui <- renderUI({{\n',
    '  df        <- liana_filtered()\n',
    '  if (nrow(df) == 0) return(plotOutput("lr_dotplot", height = "400px"))\n',
    '  top_n     <- max(1, min(input$lr_top_n,\n',
    '                          length(unique(paste0(df$ligand_complex, df$receptor_complex)))))\n',
    '  n_targets <- max(1, length(unique(df$target[!is.na(df$target)])))\n',
    '  h <- max(400, min(n_targets * (top_n * 20 + 30) + 180, 15000))\n',
    '  plotOutput("lr_dotplot", height = paste0(h, "px"))\n',
    '}})\n',
    '\n',
    'output$lr_dotplot.dl <- downloadHandler(\n',
    '  filename = function() {{ paste0("lr_dotplot.", input$lr_plot_fmt) }},\n',
    '  content  = function(file) {{\n',
    '    ggsave(file,\n',
    '           plot   = make_lr_dotplot(liana_filtered(), top_n = input$lr_top_n),\n',
    '           width  = input$lr_plot_w,\n',
    '           height = input$lr_plot_h,\n',
    '           units  = "in", dpi = 300)\n',
    '  }}\n',
    ')\n',
    '\n\n'
  )
}
