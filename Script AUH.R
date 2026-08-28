library(tidyverse) 
library(here)
library(openxlsx)
library(ggplot2)
library(patchwork)
library(scales)
library(stargazer)
library(readxl)

# Unzip todos los datos
unzip(here("Data", "EPH_usu_1_Trim_2025_xls.zip"), exdir = here("Data"))
unzip(here("Data", "EPH_usu_2_Trim_2025_xls.zip"), exdir = here("Data"))
unzip(here("Data", "EPH_usu_3_Trim_2025_xls.zip"), exdir = here("Data"))
unzip(here("Data", "EPH_usu_4_Trim_2025_xls.zip"), exdir = here("Data"))

# Leer bases individuales
EPH_individual_T1 <- read_excel(here("Data", "EPH_usu_1er_Trim_2025_xlsx", "usu_individual_T125.xlsx"))
EPH_individual_T2 <- read_excel(here("Data", "usu_individual_T225.xlsx"))
EPH_individual_T3 <- read_excel(here("Data", "usu_individual_T325.xlsx"))
EPH_individual_T4 <- read_excel(here("Data", "usu_individual_T425.xlsx"))

# Leer bases de hogar
EPH_hogar_T1 <- read_excel(here("Data", "EPH_usu_1er_Trim_2025_xlsx", "usu_hogar_T125.xlsx"))
EPH_hogar_T2 <- read_excel(here("Data", "usu_hogar_T225.xlsx"))
EPH_hogar_T3 <- read_excel(here("Data", "usu_hogar_T325.xlsx"))
EPH_hogar_T4 <- read_excel(here("Data", "usu_hogar_T425.xlsx"))

## Unir las bases de datos
trimestre1 <- EPH_individual_T1 %>%
  left_join(
    EPH_hogar_T1,
    by = c("CODUSU", "NRO_HOGAR")
  ) %>%
  mutate(trimestre = 1)

trimestre2 <- EPH_individual_T2 %>%
  left_join(
    EPH_hogar_T2,
    by = c("CODUSU", "NRO_HOGAR")
  ) %>%
  mutate(trimestre = 2)

trimestre3 <- EPH_individual_T3 %>%
  left_join(
    EPH_hogar_T3,
    by = c("CODUSU", "NRO_HOGAR")
  ) %>%
  mutate(trimestre = 3)

trimestre4 <- EPH_individual_T4 %>%
  left_join(
    EPH_hogar_T4,
    by = c("CODUSU", "NRO_HOGAR")
  ) %>%
  mutate(trimestre = 4)

# Uniformo la interpretación de la variable CH05 para poder unir las tablas
trimestre1 <- trimestre1 %>% mutate(CH05 = as.character(CH05))
trimestre2 <- trimestre2 %>% mutate(CH05 = as.character(CH05))
trimestre3 <- trimestre3 %>% mutate(CH05 = as.character(CH05))
trimestre4 <- trimestre4 %>% mutate(CH05 = as.character(CH05))

datos <- bind_rows(
  trimestre1,
  trimestre2,
  trimestre3,
  trimestre4
)

## Filtramos la muestra por individuos elegibles
# Variable para determinar si es menor
datos <- datos %>%
  mutate(menor = ifelse(as.numeric(CH06) < 18, 1, 0))

# Variable para determinar si es un hijo discapacitado
datos <- datos %>%
  mutate(discapacitado = ifelse((as.numeric(CAT_INAC) == 6) 
                                & (CH03 == 3), 1, 0))

# Variable para determinar si es menor y no trabaja y otra variable para 
# determinar si es jefe de hogar
datos <- datos %>%
  mutate(menor_no_trabaja = ifelse(menor == 1 & as.numeric(ESTADO) %in% c(2, 3, 4), 1, 0),
         jefe = ifelse(CH03 == 1, 1, 0))

# Variable para determinar si el menor es elegibe
datos <- datos %>% mutate(
  menor_elegible = ifelse(
    menor_no_trabaja == 1 & CH03 == 3, 1, 0))

# Variable para determinar si es un jefe de hogar elegible
datos <- datos %>%
  mutate(jefe_elegible = ifelse(jefe == 1 & (
    ESTADO == 2 | # Desempleado
      EMPLEO == 2 | # Informal
      SECTOR == 3 | # Trabajador doméstico
      PP05I == 2 # Monotributista social
  ), 1, 0),
  jefe_elegible = ifelse(is.na(jefe_elegible), 0, jefe_elegible) # Que tome los NAs como FALSE
  )

# Variable para determinar si alguien recibe jubilación (si reciben monto de jubilación mayor a 0)
datos <- datos %>% mutate(
  recibe_jubilacion_pension = ifelse(
    coalesce(as.numeric(V2_01_M), 0) > 0 |
      coalesce(as.numeric(V2_02_M), 0) > 0 |
      coalesce(as.numeric(V2_03_M), 0) > 0, 1, 0))

# Variable para determinar si el jefe de hogar elegible no recibe jubilación
datos <- datos %>% mutate(
  jefe_no_jubilado = ifelse((jefe_elegible == 1) & (recibe_jubilacion_pension == 0), 1, 0)
)

# Variable para determinar que el jefe no jubilado también es nativo
datos <- datos %>%
  mutate(
    jefe_nativo = ifelse(
      jefe_no_jubilado == 1 & as.numeric(CH15) %in% c(1, 2, 3),
      1, 0
    )
  )

# Variable binaria para recepción de AUH
datos <- datos %>% mutate(
  V5_01 = case_when(V5_01 == 1 ~ 1,
                    V5_01 == 2 ~ 0,
                    TRUE ~ NA_real_)) 

# Creación de dataframe con hogares elegibles: hogares con menores que no trabajan o hijos discapacitados
# y jefes elegibles
hogares <- datos %>%
  group_by(CODUSU, NRO_HOGAR, trimestre) %>%
  summarise(
    # El hogar tiene al menos un hijo o hijastro menor que no trabaja
    menor_elegible_hogar = as.integer(
      any(menor_elegible == 1, na.rm = TRUE)),
    # El hogar tiene al menos un hijo o hijastro con discapacidad
    discapacitado_hogar = as.integer(
      any(discapacitado == 1, na.rm = TRUE)),
    # El jefe cumple todas las condiciones
    jefe_elegible_hogar = as.integer(
      any(jefe_nativo == 1, na.rm = TRUE)),
    # Al menos un integrante del hogar reporta cobrar AUH
    recibe_auh_hogar = as.integer(
      any(V5_01 == 1, na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  mutate(
    # El hogar es potencialmente elegible
    elegible = ifelse(
      (menor_elegible_hogar == 1 | discapacitado_hogar == 1) &
        jefe_elegible_hogar == 1, 1, 0))

# Le agrego a los hogares correctos (por trimestre, código de usuario y 
# número de hogar) su "elegibilidad" correspondiente
datos <- datos %>%
  left_join(hogares, by = c("CODUSU", "NRO_HOGAR", "trimestre"))

# Creo una muestra con una fila por hogar potencialmente elegible
datos_elegibles <- datos %>%
  filter(
    elegible == 1,
    jefe == 1
  )

########## Estadistica descriptiva ###########
# Jefes extranjeros que reportan recibir la AUH
datos %>%
  filter(jefe == 1, V5_01 == 1) %>%
  mutate(
    extranjero = ifelse(as.numeric(CH15) %in% c(4, 5), 1, 0)
  ) %>%
  count(extranjero) %>%
  mutate(
    porcentaje = n / sum(n) * 100
  )

# Discapacitados que reportan recibir la AUH
datos %>%
  filter(discapacitado == 1) %>%
  count(V5_01) %>%
  mutate(
    porcentaje = n / sum(n) * 100
  )
