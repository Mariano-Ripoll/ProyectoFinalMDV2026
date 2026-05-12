# ============================================================
# Calculo de adulto equivalente
# Proyecto Grupal MVD 2026
# ============================================================

# Se busca asignar a cada persona un coeficiente de "adulto equivalente" segun sexo y edad para medir
# Es la base para poder construir una aproximación a cuanto consume cada persona respecto de un adulto
# de referencia. 

# Ex post se suman los coefs individuales a nivel de hogar para asignar a este su cantidad total de 
# "adultos equivalentes".. 
# Deriva de acá el uso de esta cantidad  para ajustar ingresos familiares por tamaño/composición del hogar. 


library(tidyverse)
library(readr)
library(here)

message("Calculando adulto equivalente....")

# ============================================================
# 1. Lectura de bases limpias
# ============================================================


individuos_eph_gba <- read_rds(
  here("data_processed", "individuos_eph_gba.rds")
)

# Aquí uso único para veridicar su consistencia vs n_declarado miembros del hogar
hogares_eph_gba <- read_rds(
  here("data_processed", "hogares_eph_gba.rds")
)

# ============================================================
# 2. Tabla de adulto equivalente
# ============================================================

# Ajusta manualmente  tabla de equivalencias por sexo y tramo etario
tabla_adulto_equivalente <- tribble(
  ~sexo,    ~edad_desde, ~edad_hasta, ~adulto_equiv,
  "Varon", 0,           0,           0.35,
  "Varon", 1,           1,           0.37,
  "Varon", 2,           2,           0.46,
  "Varon", 3,           3,           0.51,
  "Varon", 4,           4,           0.55,
  "Varon", 5,           5,           0.60,
  "Varon", 6,           6,           0.64,
  "Varon", 7,           7,           0.66,
  "Varon", 8,           8,           0.68,
  "Varon", 9,           9,           0.69,
  "Varon", 10,          10,          0.79,
  "Varon", 11,          11,          0.82,
  "Varon", 12,          12,          0.85,
  "Varon", 13,          13,          0.90,
  "Varon", 14,          14,          0.96,
  "Varon", 15,          15,          1.00,
  "Varon", 16,          16,          1.03,
  "Varon", 17,          17,          1.04,
  "Varon", 18,          29,          1.02,
  "Varon", 30,          45,          1.00,
  "Varon", 46,          60,          1.00,
  "Varon", 61,          75,          0.83,
  "Varon", 76,          120,         0.74,
  "Mujer", 0,           0,           0.35,
  "Mujer", 1,           1,           0.37,
  "Mujer", 2,           2,           0.46,
  "Mujer", 3,           3,           0.51,
  "Mujer", 4,           4,           0.55,
  "Mujer", 5,           5,           0.60,
  "Mujer", 6,           6,           0.64,
  "Mujer", 7,           7,           0.66,
  "Mujer", 8,           8,           0.68,
  "Mujer", 9,           9,           0.69,
  "Mujer", 10,          10,          0.70,
  "Mujer", 11,          11,          0.72,
  "Mujer", 12,          12,          0.74,
  "Mujer", 13,          13,          0.76,
  "Mujer", 14,          14,          0.76,
  "Mujer", 15,          15,          0.77,
  "Mujer", 16,          16,          0.77,
  "Mujer", 17,          17,          0.77,
  "Mujer", 18,          29,          0.76,
  "Mujer", 30,          45,          0.77,
  "Mujer", 46,          60,          0.76,
  "Mujer", 61,          75,          0.67,
  "Mujer", 76,          120,         0.63
)
#Cada fila indica el coeficiente de consumo equivalente correspondiente al grupo específico dado.

write_csv(
  tabla_adulto_equivalente,
  here("output", "tablas", "tabla_adulto_equivalente.csv")
)

# ============================================================
# 3. Asignación de adulto equivalente
# ============================================================

# En lugar de calcular persona por persona, unimos la base de personas con la tabla de adulto equiv
# por  sexo y rango de edad.Se crea un identificador único por persona para poder recuperar luego los casos que no logren emparejarse
# con ningún tramo válido de la tabla de adulto equivalente
# ============================================================
# 3. Asignación de adulto equivalente
# ============================================================

# En lugar de unir cada persona con todos los tramos posibles de su sexo,
# expandimos primero la tabla de adulto equivalente para que haya una fila por sexo y edad.
# Así, cada persona se une directamente con un único valor posible de adulto equivalente.

tabla_adulto_equivalente_expandida <- tabla_adulto_equivalente |>
  rowwise() |>
  mutate(
    edad = list(seq(edad_desde, edad_hasta))
  ) |>
  unnest(edad) |>
  ungroup() |>
  select(
    sexo,
    edad,
    adulto_equiv
  )

individuos_adulto_equiv <- individuos_eph_gba |>
  mutate(
    id_persona = row_number(),
    edad = ch06
  ) |>
  left_join(
    tabla_adulto_equivalente_expandida,
    by = c("sexo", "edad")
  ) |>
  mutate(
    adulto_equiv_faltante = case_when(
      is.na(adulto_equiv) ~ 1,
      TRUE ~ 0
    )
  )
# ============================================================
# 4. Adulto equivalente por hogar
# ============================================================

# Agrega los coeficientes individuales a nivel hogar.
# La suma de adulto_equiv representa la cantidad total de adultos equivalentes del hogar.
# Si al menos una persona del hogar no tiene adulto equivalente válido, se invalida el total del hogar,
#(logicamente, sino la suma quedaría incompleta y subestimaría)

adulto_equiv_hogar <- individuos_adulto_equiv |>
  group_by(anio, ano4, trimestre, periodo, codusu, nro_hogar, aglomerado, gba) |>
  summarise( # Cantidad de personas observadas en la base individual para cada hogar.
    miembros_hogar = n(),
    personas_sin_adulto_equiv = sum(adulto_equiv_faltante == 1, na.rm = TRUE),
    adulto_equiv_hogar = case_when(
      personas_sin_adulto_equiv == 0 ~ sum(adulto_equiv, na.rm = TRUE),
      TRUE ~ NA_real_
    ),
    adulto_equiv_valido = case_when(
      personas_sin_adulto_equiv == 0 ~ 1,
      TRUE ~ 0
    ),
    .groups = "drop"
  )

# ============================================================
# 5. Informacion del jefe de hogar
# ============================================================
# Se extraen características básicas del jefe de hogar.
# Despues nos va a dejar caracterizar hogares por sus atributos sociodemográficos y laborales de esta persona. 
jefatura_hogar <- individuos_adulto_equiv |>
  filter(jefe_hogar == 1) |>
  transmute(
    anio,
    trimestre,
    periodo,
    codusu,
    nro_hogar,
    sexo_jefe = sexo,
    edad_jefe = edad,
    estado_jefe = estado,
    condicion_actividad_jefe = condicion_actividad,
    cat_ocup_jefe = cat_ocup
  )
# Incorpora la información de jefe a la base agregada por hogar;
# unión se realiza por los identificadores temporales y del hogar:
adulto_equiv_hogar <- adulto_equiv_hogar |>
  left_join(
    jefatura_hogar,
    by = c("anio", "trimestre", "periodo", "codusu", "nro_hogar")
  )

# ============================================================
# 6. Chequeo general
# ============================================================

chequeo_adulto_equivalente <- adulto_equiv_hogar |>
  left_join(
    hogares_eph_gba |>
      select(anio, trimestre, periodo, codusu, nro_hogar, ix_tot),
    by = c("anio", "trimestre", "periodo", "codusu", "nro_hogar")
  ) |>
  mutate(
    diferencia_miembros = miembros_hogar - ix_tot
  ) |>
  group_by(anio, trimestre, periodo) |>
  summarise(
    hogares = n(),
    hogares_con_diferencia_miembros = sum(diferencia_miembros != 0, na.rm = TRUE),
    hogares_con_adulto_equiv_valido = sum(adulto_equiv_valido == 1, na.rm = TRUE),
    hogares_con_adulto_equiv_invalido = sum(adulto_equiv_valido == 0, na.rm = TRUE),
    adulto_equiv_media = mean(adulto_equiv_hogar, na.rm = TRUE),
    adulto_equiv_mediana = median(adulto_equiv_hogar, na.rm = TRUE),
    adulto_equiv_minimo = min(adulto_equiv_hogar, na.rm = TRUE),
    adulto_equiv_maximo = max(adulto_equiv_hogar, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(anio, trimestre)

print(chequeo_adulto_equivalente, n = Inf)

# ============================================================
# 7. Guardado de resultados
# ============================================================

# guarda las bases resultantes para los siguientes scripts
# Base individual conserva el adulto equivalente asignado a cada persona
# La base por hogar contiene la suma de adultos equivalentes y variables extra para jefes hogar

write_rds(
  individuos_adulto_equiv,
  here("data_processed", "individuos_adulto_equiv.rds")
)

write_rds(
  adulto_equiv_hogar,
  here("data_processed", "adulto_equiv_hogar.rds")
)

write_csv(
  chequeo_adulto_equivalente,
  here("output", "tablas", "chequeo_adulto_equivalente.csv")
)

message("Calculo de adulto equivalente finalizado.")