relative_abundance_plot <-
  function(tabla,
           metadata,
           col_x,
           facet_col = NULL,
           group_var,
           label = "Taxonomy",
           num_grupos = 15) {
    
    library(ggplot2)
    library(ggthemes)
    library(patchwork)
    library(readxl)
    library(ggtext)
    library(dplyr)
    library(tidyr)
    library(forcats)
    
    # Calcular abundancia relativa
    tabla[, -1] <- sweep(tabla[, -1], 2, colSums(tabla[, -1], na.rm = TRUE), FUN = "/") * 100
    
    # Transformar tabla para unir con metadatos
    tabla_long <- tabla %>%
      pivot_longer(cols = -Group, names_to = "SAMPLEID", values_to = "RelativeAbundance")
    
    # Unir con metadatos
    columnas_a_unir <- c("SAMPLEID", col_x, facet_col, group_var)
    columnas_a_unir <- columnas_a_unir[!is.na(columnas_a_unir) & columnas_a_unir != "NULL"]
    
    tabla_long <- tabla_long %>%
      left_join(metadata %>% select(all_of(columnas_a_unir)), by = "SAMPLEID")
    
    # Promediar por grupo y posibles facetas
    grupo_vars <- c(group_var, "Group")
    if (!is.null(facet_col)) {
      grupo_vars <- c(grupo_vars, facet_col)
    }
    
    promedio_tratamiento <- tabla_long %>%
      group_by(across(all_of(grupo_vars))) %>%
      summarise(PromedioAbundancia = mean(RelativeAbundance, na.rm = TRUE), .groups = "drop")
    
    # Obtener los grupos con mayor abundancia promedio
    promedio_general <- promedio_tratamiento %>%
      group_by(Group) %>%
      summarise(MeanAbundance = mean(PromedioAbundancia, na.rm = TRUE))
    
    top_groups <- promedio_general %>%
      arrange(desc(MeanAbundance)) %>%
      slice_head(n = num_grupos) %>%
      pull(Group)
    
    # Filtrar los top N grupos
    promedio_tratamiento <- promedio_tratamiento %>%
      filter(Group %in% top_groups)
    
    # Simplificar nombres de grupos
    promedio_tratamiento <- promedio_tratamiento %>%
      mutate(Group = case_when(
        grepl("g__", Group) ~ sub(".*g__", "", Group),
        grepl(".__", Group) ~ sub(".*f__(.*)\\..*", "other \\1", Group),
        TRUE ~ Group
      ))
    
    # Ordenar facetas si hay
    if (!is.null(facet_col)) {
      niveles_facet <- unique(promedio_tratamiento[[facet_col]])
      promedio_tratamiento[[facet_col]] <-
        factor(promedio_tratamiento[[facet_col]], levels = niveles_facet)
    }
    
    # Ordenar el eje x según el orden original en metadata
    niveles_x <- unique(metadata[[group_var]])
    promedio_tratamiento[[group_var]] <- factor(promedio_tratamiento[[group_var]],
                                                levels = niveles_x)
    
    # Reordenar los niveles de Group según su valor máximo individual (y luego invertir)
    orden_grupos <- promedio_tratamiento %>%
      group_by(Group) %>%
      summarise(max_abund = max(PromedioAbundancia, na.rm = TRUE)) %>%
      arrange(desc(max_abund)) %>%
      pull(Group)
    
    promedio_tratamiento$Group <- factor(promedio_tratamiento$Group, levels = rev(orden_grupos))
    
    # Paleta de colores
    cbPalette <- colorRampPalette(c(
      "#999999", "#0099CC", "#ff6600", "#FF0066", "#99FF33", 
      "#CC00cc", "#009E73", "#F0E442", "#0072B2", "#ff9900", 
      "#56B4E9", "#FFFFFF", "#99ff90", "#ffff00", "#FF0000"
    ))(length(unique(promedio_tratamiento$Group)))
    
    # Crear gráfico
    p <- ggplot(promedio_tratamiento, aes(
      fill = Group,
      y = PromedioAbundancia,
      x = !!sym(group_var)
    )) +
      geom_bar(position = "stack", stat = "identity", width = 0.5, color = "#000000") +
      scale_fill_manual(name = label, values = cbPalette) +
      theme_bw() +
      theme(panel.grid = element_blank(),
            legend.title = element_text(size = 12),
            legend.text = element_markdown(size = 10)) +
      ylim(0, 100) +
      ylab("Relative abundance (%)") +
      xlab("Sample")
    
    if (!is.null(facet_col)) {
      p <- p + facet_wrap(vars(!!sym(facet_col)), scales = "free_x")
    }
    
    return(p)
  }
