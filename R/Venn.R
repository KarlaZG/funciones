venn_diagram <- function(tabla, metadata, merge_by = "Sample_type") {
  library(micro)
  
  # Asegurar que los datos son data frames
  tabla <- as.data.frame(tabla)
  metadata <- as.data.frame(metadata)
  
  # Crear el objeto microtable
  dataset <- microtable$new(
    otu_table = tabla,
    sample_table = metadata,
    auto_tidy = TRUE
  )
  
  # Combinar muestras según el grupo deseado
  dataset_merged <- dataset$merge_samples(merge_by)
  
  # Crear y graficar el diagrama de Venn
  t1 <- trans_venn$new(dataset_merged, ratio = NULL)
  t1$plot_venn()
}
