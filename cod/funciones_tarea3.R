# ============================================================
#  Tarea 3 · CA-0204 Herramientas de Ciencia de Datos I
#  Alonso Alpízar Brenes · C20274
#  Funciones de los ejercicios 1, 2 y 3
# ============================================================

library(tidyverse)


# ------------------------------------------------------------
#  Ejercicio 1 · Alertas por ventana móvil
# ------------------------------------------------------------
#  alerta.movil(df, ventana, k.alerta)
#
#  Recibe:
#    df       data frame de transacciones (columnas id_tx y fecha)
#    ventana  número de días anteriores que definen lo "normal"
#    k.alerta cuántas desviaciones sobre la media activan la alerta
#
#  Devuelve:
#    data frame con fecha, n.tx, media, desv y alerta.
#    Los primeros `ventana` días quedan en NA (no se evalúan).
# ------------------------------------------------------------

alerta.movil <- function(df, ventana, k.alerta) {
  
  # 1. Transacciones por día
  diario <- df |>
    count(fecha, name = "n.tx") |>
    mutate(fecha = as.Date(fecha))
  
  # 2. Media y desviación de los `ventana` días anteriores
  n     <- nrow(diario)
  media <- rep(NA, n)
  desv  <- rep(NA, n)
  
  for (t in (ventana + 1):n) {
    previos  <- diario$n.tx[(t - ventana):(t - 1)]   # sin incluir el día t
    media[t] <- mean(previos)
    desv[t]  <- sd(previos)
  }
  
  diario$media <- media
  diario$desv  <- desv
  
  # 3. Alerta: el día supera media + k desviaciones
  diario$alerta <- diario$n.tx > diario$media + k.alerta * diario$desv
  
  diario
}

# ------------------------------------------------------------
#  Ejercicio 2 · Ley de Benford por cantón
# ------------------------------------------------------------
#  benford.mad(x)
#
#  Recibe:
#    x  vector de montos (positivos)
#
#  Devuelve:
#    la MAD (desviación absoluta media) entre la proporción
#    observada de cada primer dígito y la proporción de Benford.
# ------------------------------------------------------------

benford.mad <- function(x) {

  # 1. Primer dígito de cada monto
  x <- x[!is.na(x) & x > 0]
  d <- x %/% 10^floor(log10(x))

  # 2. Proporción observada de cada dígito (1 a 9)
  p <- as.numeric(table(factor(d, levels = 1:9))) / length(x)

  # 3. Proporción teórica de Benford
  b <- log10(1 + 1 / (1:9))

  # 4. MAD
  sum(abs(p - b)) / 9
}


# ------------------------------------------------------------
#  benford.canton(df, canal, cantones)
#
#  Recibe:
#    df        data frame de transacciones (cod_canton, canal, monto_crc)
#    canal     canal a auditar (por ejemplo "Web")
#    cantones  catálogo con cod_canton y canton
#
#  Devuelve:
#    data frame por cantón con cod_canton, canton, n, mad y clase
#    (Nigrini), ordenado de mayor a menor MAD.
# ------------------------------------------------------------

benford.canton <- function(df, canal, cantones) {

  # 1. Solo las transacciones del canal pedido
  sub <- df[df$canal == canal, ]

  # 2. MAD por cantón (usa benford.mad)
  sub |>
    group_by(cod_canton) |>
    summarise(n = n(), mad = benford.mad(monto_crc)) |>

    # 3. Nombre del cantón y clase de Nigrini
    left_join(select(cantones, cod_canton, canton), by = "cod_canton") |>
    mutate(clase = case_when(
      mad <= 0.006 ~ "conformidad cercana",
      mad <= 0.012 ~ "conformidad aceptable",
      mad <= 0.015 ~ "conformidad marginal",
      TRUE         ~ "no conformidad"
    )) |>

    # 4. Orden y columnas finales
    arrange(desc(mad)) |>
    select(cod_canton, canton, n, mad, clase) |>
    as.data.frame()
}
