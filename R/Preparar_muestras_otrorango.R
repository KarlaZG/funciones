library(readxl)
library(tidyverse)
setwd("d:/trigo-16S/repeticion/")
tabla <-
  data.frame(read_excel("GENERO_CONTEOS_ORDENADO_TODO.xlsx",), row.names = 1)

tabla<- rownames_to_column(tabla,"Group")

tabla<- tabla %>% separate(
  col = "Group",                    # nombre de la columna a separar
  into = c("D", "P","C","O","F","G"),  # nombres de las columnas a crear
  sep = "\\." ,                   # patron a buscar, en este caso se usa \\ por ser un caracter especial
  remove = T                      # sirve para conservar la variable a separar
)

tabla<- tabla[,c(2,7:118)] 
tabla<- tabla[,c(1:58)] 
tabla<- tabla %>% rename("Group" = P)

tabla<- tabla %>%
  group_by(Group) %>%  # Agrupar por la primera columna
  summarize(across(everything(), sum, na.rm = TRUE))




# Definir niveles comunes para TRATAMIENTO_SAMPLE_DAY
tratamiento_day_levels <- c("Soil_m_Soil_Jul", "Soil_Soil_Jul", "No.compost_Soil_Ago", "No.compost_Soil_Sep", 
                            "Compost_Soil_Ago", "Compost_Soil_Sep", "Sterile.compost_Soil_Ago", "Sterile.compost_Soil_Sep",
                            "No.compost_Rhizosphere_Ago", "No.compost_Rhizosphere_Sep", 
                            "Compost_Rhizosphere_Ago", "Compost_Rhizosphere_Sep", 
                            "Sterile.compost_Rhizosphere_Ago", "Sterile.compost_Rhizosphere_Sep",
                            "No.compost_Root_Ago", "No.compost_Root_Sep", "Compost_Root_Ago", "Compost_Root_Sep",
                            "Sterile.compost_Root_Ago", "Sterile.compost_Root_Sep")

tratamiento_day_labels <- c("CM_0", "S_0", "Un_S_31", "Un_S_162", "C_S_31", "C_S_162", "SC_S_31", "SC_S_162",
                            "Un_R_31", "Un_R_162", "C_R_31", "C_R_162", "SC_R_31", "SC_R_162",
                            "Un_Ro_31", "Un_Ro_162", "C_Ro_31", "C_Ro_162", "SC_Ro_31", "SC_Ro_162")

# Cargar y procesar los datos
metadata <- read.delim("sample_metadata_TODO.tsv", row.names = NULL)[1:57, ] %>%
  mutate(
    TRATAMIENTO_SAMPLE_DAY = paste(Treatment, Sample_type, Time.days, sep = "_"),
    TRATAMIENTO_SAMPLE_DAY = factor(TRATAMIENTO_SAMPLE_DAY, levels = tratamiento_day_levels, labels = tratamiento_day_labels)
  ) %>%
  arrange(TRATAMIENTO_SAMPLE_DAY)


