relative_abundance_plot <-
  function(tabla,
           metadata,
           col_x,
           facet_col = NULL,
           group_var,
           label = "Taxonomy",
           num_grupos=15) {
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
    columnas_a_unir <- columnas_a_unir[!is.na(columnas_a_unir) & columnas_a_unir != "NULL"]  # Eliminar NULLs
    
    tabla_long <- tabla_long %>%
      left_join(metadata %>% select(all_of(columnas_a_unir)), by = "SAMPLEID")
    
    # Promediar por la variable especificada (group_var) y facet_col si está presente
    grupo_vars <- c(group_var, "Group")
    if (!is.null(facet_col)) {
      grupo_vars <- c(grupo_vars, facet_col)
    }
    
    promedio_tratamiento <- tabla_long %>%
      group_by(across(all_of(grupo_vars))) %>%
      summarise(PromedioAbundancia = mean(RelativeAbundance, na.rm = TRUE), .groups = "drop")
    
    # Calcular el promedio general por grupo
    promedio_general <- promedio_tratamiento %>%
      group_by(Group) %>%
      summarise(MeanAbundance = mean(PromedioAbundancia, na.rm = TRUE))
    
    # Seleccionar los 15 grupos más abundantes según el promedio general
    top_groups <- promedio_general %>%
      arrange(desc(MeanAbundance)) %>%
      slice_head(n = num_grupos) %>%
      pull(Group)
    
    # Filtrar solo los top 15 grupos
    promedio_tratamiento <- promedio_tratamiento %>%
      filter(Group %in% top_groups)
    
    # Modificar los nombres de los grupos
    promedio_tratamiento <- promedio_tratamiento %>%
      mutate(Group = case_when(
        grepl("g__", Group) ~ sub(".*g__", "", Group),  # Si contiene "g__"
        grepl(".__", Group) ~ sub(".*f__(.*)\\..*", "other \\1", Group),  # Si contiene ".__", extraer lo que está después de "f__" y antes de "."
        TRUE ~ Group  # Si no contiene ninguno, mantener el nombre original
      ))
    
    # Asegurar el orden de los facets en el mismo orden que aparecen en la tabla
    if (!is.null(facet_col)) {
      niveles_facet <- unique(promedio_tratamiento[[facet_col]])
      promedio_tratamiento[[facet_col]] <-
        factor(promedio_tratamiento[[facet_col]], levels = niveles_facet)
    }
    
    # Paleta de colores
    cbPalette <- colorRampPalette(c("#999999", "#0099CC", "#ff6600", "#FF0066", "#99FF33", 
                                    "#CC00cc", "#009E73", "#F0E442", "#0072B2", "#ff9900", 
                                    "#56B4E9", "#FFFFFF", "#99ff90", "#ffff00", "#FF0000"))(length(unique(promedio_tratamiento$Group)))
    
    # Reordenar factores
    promedio_tratamiento <- promedio_tratamiento %>%
      mutate(Group = fct_reorder(Group, PromedioAbundancia, .desc = FALSE))
    
    # Crear gráfico
    p <- ggplot(promedio_tratamiento, aes(fill = Group, y = PromedioAbundancia, x = fct_reorder(!!sym(group_var), PromedioAbundancia, .desc = FALSE))) +
      geom_bar(position = "stack", stat = "identity", width = 0.5, color = "#000000") +
      scale_fill_manual(name = label, values = cbPalette) +
      theme_bw() +
      theme(panel.grid = element_blank(),
            legend.title = element_text(size = 12),
            legend.text = element_markdown(size = 10)) +
      ylim(0, 100) +
      ylab("Relative abundance (%)") +
      xlab("Sample")
    
    # Agregar facet_wrap() si facet_col no es NULL
    if (!is.null(facet_col)) {
      p <- p + facet_wrap(vars(!!sym(facet_col)), scales = "free_x")
    }
    
    return(p)
  }
