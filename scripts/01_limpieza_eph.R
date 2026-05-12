# ============================================================
# Inventario y limpieza de bases EPH
# Proyecto grupal MVD 2026
# ============================================================

# Objetivo:
# revisar las bases disponibles, leer todos los años y trimestres del proyecto,
# filtrar GBA y construir bases limpias de hogares e individuos.

library(tidyverse)
library(haven)
library(readr)
library(janitor)
library(here)

message("Iniciando busqueda y limpieza de bases EPH...")

# ============================================================
# 1. Periodos del proyecto
# ============================================================

periodos_proyecto <- expand_grid(
  anio = c(2007, 2008, 2010, 2011, 2018, 2019, 2021, 2022),
  trimestre = 1:4
) |>
  filter(!(anio == 2007 & trimestre == 3)) |>
  arrange(anio, trimestre)

# ============================================================
# 2. Rutas
# ============================================================

carpeta_hogar <- here("data_raw", "eph", "hogar")
carpeta_individual <- here("data_raw", "eph", "individual")

# ============================================================
# 3. Funciones
# ============================================================

buscar_archivo_eph <- function(carpeta, anio, trimestre) {
  
  archivos <- list.files(
    carpeta,
    pattern = "\\.(dta|txt|csv)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(archivos) == 0) {
    return(NA_character_)
  }
  
  nombres <- basename(archivos) |>
    str_to_lower()
  
  anio_dos_digitos <- str_sub(as.character(anio), 3, 4)
  codigo_indec <- paste0("t", trimestre, anio_dos_digitos)
  
  patron_anio_trimestre_1 <- paste0(anio, "_t", trimestre)
  patron_anio_trimestre_2 <- paste0(anio, "-t", trimestre)
  patron_anio_trimestre_3 <- paste0(anio, "t", trimestre)
  
  candidatos <- archivos[
    str_detect(nombres, fixed(codigo_indec)) |
      str_detect(nombres, fixed(patron_anio_trimestre_1)) |
      str_detect(nombres, fixed(patron_anio_trimestre_2)) |
      str_detect(nombres, fixed(patron_anio_trimestre_3))
  ]
  
  if (length(candidatos) == 0) {
    return(NA_character_)
  }
  
  candidatos[1]
}

convertir_numero_eph <- function(x) {
  
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  
  x_chr <- as.character(x) |>
    str_trim()
  
  x_chr[x_chr %in% c("", "NA", "NaN", "///", "-", "--", ".", "s/d", "S/D")] <- NA_character_
  
  tiene_coma <- str_detect(x_chr, ",")
  tiene_coma[is.na(tiene_coma)] <- FALSE
  
  resultado <- rep(NA_real_, length(x_chr))
  
  posiciones_con_coma <- which(tiene_coma & !is.na(x_chr))
  posiciones_sin_coma <- which(!tiene_coma & !is.na(x_chr))
  
  if (length(posiciones_con_coma) > 0) {
    resultado[posiciones_con_coma] <- parse_number(
      x_chr[posiciones_con_coma],
      locale = locale(decimal_mark = ",", grouping_mark = ".")
    )
  }
  
  if (length(posiciones_sin_coma) > 0) {
    resultado[posiciones_sin_coma] <- parse_number(
      x_chr[posiciones_sin_coma],
      locale = locale(decimal_mark = ".", grouping_mark = ",")
    )
  }
  
  resultado
}

leer_base_eph <- function(path) {
  
  extension <- tools::file_ext(path) |>
    str_to_lower()
  
  if (extension == "dta") {
    
    base <- read_dta(path) |>
      mutate(across(everything(), as.character)) |>
      clean_names()
    
  } else if (extension == "txt") {
    
    base <- read_delim(
      file = path,
      delim = ";",
      col_types = cols(.default = col_character()),
      show_col_types = FALSE,
      locale = locale(decimal_mark = ",")
    ) |>
      clean_names()
    
  } else if (extension == "csv") {
    
    base <- read_csv(
      file = path,
      col_types = cols(.default = col_character()),
      show_col_types = FALSE
    ) |>
      clean_names()
    
  } else {
    
    stop("Formato no reconocido: ", path)
  }
  
  base
}

convertir_variables_numericas <- function(base) {
  
  variables_numericas <- c(
    "ano4",
    "trimestre",
    "nro_hogar",
    "realizada",
    "region",
    "aglomerado",
    "pondera",
    "pondih",
    "itf",
    "ipcf",
    "ix_tot",
    "ix_men10",
    "ix_mayeq10",
    "componente",
    "ch03",
    "ch04",
    "ch06",
    "estado",
    "cat_ocup"
  )
  
  base |>
    mutate(
      across(
        any_of(variables_numericas),
        convertir_numero_eph
      )
    )
}

limpiar_hogar <- function(base, anio_base, trimestre_base) {
  
  if (!"ano4" %in% names(base)) {
    base$ano4 <- anio_base
  }
  
  if (!"trimestre" %in% names(base)) {
    base$trimestre <- trimestre_base
  }
  
  if (!"pondih" %in% names(base)) {
    base$pondih <- NA
  }
  
  base |>
    convertir_variables_numericas() |>
    filter(aglomerado %in% c(32, 33)) |>
    mutate(
      anio = anio_base,
      trimestre = trimestre_base,
      periodo = paste0(anio, "_t", trimestre),
      gba = case_when(
        aglomerado == 32 ~ "CABA",
        aglomerado == 33 ~ "Partidos del GBA",
        TRUE ~ NA_character_
      ),
      ponderador_ingreso = case_when(
        !is.na(pondih) ~ pondih,
        TRUE ~ pondera
      ),
      decifr = str_trim(as.character(decifr)),
      deccfr = str_trim(as.character(deccfr)),
      decifr_codigo = convertir_numero_eph(decifr),
      deccfr_codigo = convertir_numero_eph(deccfr),
      no_respuesta_ingresos = case_when(
        decifr_codigo == 12 | deccfr_codigo == 12 ~ 1,
        TRUE ~ 0
      ),
      ingreso_cero = case_when(
        itf == 0 & no_respuesta_ingresos == 0 ~ 1,
        TRUE ~ 0
      )
    ) |>
    select(
      codusu,
      nro_hogar,
      anio,
      ano4,
      trimestre,
      periodo,
      region,
      aglomerado,
      gba,
      realizada,
      pondera,
      pondih,
      ponderador_ingreso,
      itf,
      ipcf,
      decifr,
      deccfr,
      no_respuesta_ingresos,
      ingreso_cero,
      ix_tot,
      ix_men10,
      ix_mayeq10,
      everything()
    )
}

limpiar_individual <- function(base, anio_base, trimestre_base) {
  
  if (!"ano4" %in% names(base)) {
    base$ano4 <- anio_base
  }
  
  if (!"trimestre" %in% names(base)) {
    base$trimestre <- trimestre_base
  }
  
  base |>
    convertir_variables_numericas() |>
    filter(aglomerado %in% c(32, 33)) |>
    mutate(
      anio = anio_base,
      trimestre = trimestre_base,
      periodo = paste0(anio, "_t", trimestre),
      gba = case_when(
        aglomerado == 32 ~ "CABA",
        aglomerado == 33 ~ "Partidos del GBA",
        TRUE ~ NA_character_
      ),
      sexo = case_when(
        ch04 == 1 ~ "Varon",
        ch04 == 2 ~ "Mujer",
        TRUE ~ NA_character_
      ),
      jefe_hogar = case_when(
        ch03 == 1 ~ 1,
        TRUE ~ 0
      ),
      condicion_actividad = case_when(
        estado == 1 ~ "Ocupado",
        estado == 2 ~ "Desocupado",
        estado == 3 ~ "Inactivo",
        estado == 4 ~ "Menor de 10",
        estado == 0 ~ "Entrevista no realizada",
        TRUE ~ NA_character_
      )
    ) |>
    select(
      codusu,
      nro_hogar,
      componente,
      anio,
      ano4,
      trimestre,
      periodo,
      region,
      aglomerado,
      gba,
      pondera,
      ch03,
      jefe_hogar,
      ch04,
      sexo,
      ch06,
      estado,
      condicion_actividad,
      cat_ocup,
      itf,
      ipcf,
      decifr,
      deccfr,
      everything()
    )
}

# ============================================================
# 4. Inventario de archivos
# ============================================================

inventario_bases_eph <- periodos_proyecto |>
  mutate(
    path_hogar = map2_chr(anio, trimestre, ~ buscar_archivo_eph(carpeta_hogar, .x, .y)),
    path_individual = map2_chr(anio, trimestre, ~ buscar_archivo_eph(carpeta_individual, .x, .y)),
    hogar_disponible = case_when(!is.na(path_hogar) ~ 1, TRUE ~ 0),
    individual_disponible = case_when(!is.na(path_individual) ~ 1, TRUE ~ 0)
  )

print(inventario_bases_eph, n = Inf)

write_csv(
  inventario_bases_eph,
  here("output", "tablas", "inventario_bases_eph.csv")
)

faltantes <- inventario_bases_eph |>
  filter(hogar_disponible == 0 | individual_disponible == 0)

if (nrow(faltantes) > 0) {
  print(faltantes, n = Inf)
  stop("Faltan bases esperadas. Revisar nombres o ubicacion de archivos.")
}

# ============================================================
# 5. Lectura y limpieza de todos los periodos
# ============================================================

lista_hogares <- list()
lista_individuos <- list()

for (i in seq_len(nrow(inventario_bases_eph))) {
  
  anio_i <- inventario_bases_eph$anio[i]
  trimestre_i <- inventario_bases_eph$trimestre[i]
  
  message("Procesando hogar ", anio_i, " trimestre ", trimestre_i)
  
  lista_hogares[[i]] <- leer_base_eph(inventario_bases_eph$path_hogar[i]) |>
    limpiar_hogar(anio_i, trimestre_i)
  
  message("Procesando individual ", anio_i, " trimestre ", trimestre_i)
  
  lista_individuos[[i]] <- leer_base_eph(inventario_bases_eph$path_individual[i]) |>
    limpiar_individual(anio_i, trimestre_i)
}

hogares_eph_gba <- bind_rows(lista_hogares)

individuos_eph_gba <- bind_rows(lista_individuos)

# ============================================================
# 6. Chqueos de limpieza
# ============================================================

chequeo_hogares_eph <- hogares_eph_gba |>
  group_by(anio, trimestre, periodo) |>
  summarise(
    hogares = n(),
    hogares_unicos = n_distinct(paste(codusu, nro_hogar)),
    hogares_sin_respuesta_ingresos = sum(no_respuesta_ingresos == 1, na.rm = TRUE),
    hogares_ingreso_cero = sum(ingreso_cero == 1, na.rm = TRUE),
    itf_mediana = median(itf, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(anio, trimestre)

chequeo_individuos_eph <- individuos_eph_gba |>
  group_by(anio, trimestre, periodo) |>
  summarise(
    personas = n(),
    hogares_unicos = n_distinct(paste(codusu, nro_hogar)),
    jefes_hogar = sum(jefe_hogar == 1, na.rm = TRUE),
    ocupados = sum(estado == 1, na.rm = TRUE),
    desocupados = sum(estado == 2, na.rm = TRUE),
    inactivos = sum(estado == 3, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(anio, trimestre)

chequeo_union_hogar_individual <- chequeo_hogares_eph |>
  select(anio, trimestre, periodo, hogares_hogar = hogares_unicos) |>
  left_join(
    chequeo_individuos_eph |>
      select(anio, trimestre, periodo, hogares_individual = hogares_unicos, personas, jefes_hogar),
    by = c("anio", "trimestre", "periodo")
  ) |>
  mutate(
    diferencia_hogares = hogares_hogar - hogares_individual,
    diferencia_jefes = hogares_hogar - jefes_hogar
  )

# ============================================================
# 7. Guardado de resultados
# ============================================================

write_rds(
  hogares_eph_gba,
  here("data_processed", "hogares_eph_gba.rds")
)

write_rds(
  individuos_eph_gba,
  here("data_processed", "individuos_eph_gba.rds")
)

write_csv(
  chequeo_hogares_eph,
  here("output", "tablas", "chequeo_hogares_eph.csv")
)

write_csv(
  chequeo_individuos_eph,
  here("output", "tablas", "chequeo_individuos_eph.csv")
)

write_csv(
  chequeo_union_hogar_individual,
  here("output", "tablas", "chequeo_union_hogar_individual.csv")
)

message("Busqueda y limpieza de bases EPH finalizados.")