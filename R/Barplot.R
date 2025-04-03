relative_abundance_plot <-
  function(tabla,
           metadata,
           col_x,
           facet_col = NULL,
           num_grupos,
           label = "Taxonomy") {
    library(ggplot2)
    library(ggthemes)
    library(patchwork)
    library(ggthemr)
    library(readxl)
    library(ggtext)
    
    
    tabla[, -1] <-
      sweep(tabla[, -1], 2, colSums(tabla[, -1], na.rm = TRUE), FUN = "/") * 100
    
    tabla_ordenada <- tabla[order(tabla[, 2], decreasing = TRUE),]
    tabla_ordenada <- tabla_ordenada[1:num_grupos, ]
    rownames(tabla_ordenada) <- NULL
    tabla_ordenada <- column_to_rownames(tabla_ordenada, "Group")
    tabla_ordenada <- data.frame(t(tabla_ordenada))
    tabla_ordenada <- rownames_to_column(tabla_ordenada, "SAMPLEID")
    
    # Seleccionar din?micamente las columnas a unir
    columnas_a_unir <- c("SAMPLEID", col_x)
    if (!is.null(facet_col)) {
      columnas_a_unir <- c(columnas_a_unir, facet_col)
    }
    
    tabla_ordenada <- tabla_ordenada %>%
      left_join(metadata %>% select(all_of(columnas_a_unir)), by = "SAMPLEID")
    
    # Determinar columnas a excluir din?micamente en pivot_longer
    columnas_excluir <- c("SAMPLEID", col_x)
    if (!is.null(facet_col)) {
      columnas_excluir <- c(columnas_excluir, facet_col)
    }
    
    long_data <- tabla_ordenada %>%
      pivot_longer(
        cols = -all_of(columnas_excluir),
        names_to = "Group",
        values_to = "RelativeAbundance"
      )
    
    # Asegurar el orden de los facets en el mismo orden que aparecen en la tabla
    if (!is.null(facet_col)) {
      niveles_facet <- unique(tabla_ordenada[[facet_col]])
      long_data[[facet_col]] <-
        factor(long_data[[facet_col]], levels = niveles_facet)
    }
    
    # Determinar variables de agrupaci?n din?micamente
    grupo_vars <- c(col_x, "Group")
    if (!is.null(facet_col)) {
      grupo_vars <- c(grupo_vars, facet_col)
    }
    
    promedio_tratamiento <- long_data %>%
      group_by(across(all_of(grupo_vars))) %>%
      summarise(
        PromedioAbundancia = mean(RelativeAbundance, na.rm = TRUE),
        .groups = "drop"
      )
    
    cbPalette <- c(
      "#999999",
      "#0099CC",
      "#ff6600",
      "#FF0066",
      "#99FF33",
      "#CC00cc",
      "#009E73",
      "#F0E442",
      "#0072B2",
      "#ff9900",
      "#56B4E9",
      "#FFFFFF",
      "#99ff90",
      "#ffff00",
      "#FF0000"
    )
    num_colores <- length(unique(promedio_tratamiento$Group))
    cbPalette <- colorRampPalette(cbPalette)(num_colores)
    
    
    promedio_tratamiento <- promedio_tratamiento %>%
      mutate(Group = fct_reorder(Group, PromedioAbundancia))
    
    p <-
      ggplot(promedio_tratamiento,
             aes(
               fill = Group,
               y = PromedioAbundancia,
               x = !!sym(col_x)
             )) +
      geom_bar(
        position = "stack",
        stat = "identity",
        width = 0.5,
        color = "#000000"
      ) +
      scale_fill_manual(name = label, values = cbPalette) +
      theme_bw() +
      theme(
        panel.grid = element_blank(),
        legend.title = element_text(size = 17),
        legend.text = element_markdown(size = 12)
      ) +
      ylim(0, 100) +
      ylab("Relative abundance (%)") +
      xlab("Sample")
    
    # Agregar facet_wrap() si facet_col no es NULL
    if (!is.null(facet_col)) {
      p <- p + facet_wrap(vars(!!sym(facet_col)), scales = "free_x")
    }
    
    return(p)
  }
