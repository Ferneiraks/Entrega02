
# =============================================================================
# ANÁLISIS COMPLETO DE FONDOS MUTUOS CMF - CON GENERACIÓN PDF
# =============================================================================

# 1. CARGAR LIBRERÍAS
library(dplyr)
library(ggplot2)
library(readr)
library(knitr)
library(kableExtra)
library(scales)
library(rmarkdown)
library(broom)
library(tidyr)
library(gridExtra)
library(grid)

# 2. PARÁMETRO PARA GENERAR PDF
GENERAR_PDF <- TRUE  # Cambiar a FALSE si no quieres generar PDF

# 3. PRIMERO: LEER Y EXPLORAR LOS DATOS
archivo_datos <- "C:/Users/PC/OneDrive/Entrega02/ffmm_todos_20250501_20250531.txt"

# Verificar si el archivo existe
if (!file.exists(archivo_datos)) {
  stop("❌ ARCHIVO NO ENCONTRADO: ", archivo_datos)
} else {
  cat("✅ Archivo encontrado:", archivo_datos, "\n")
}

# Leer los datos con read_delim (más robusto que read.csv para este caso)
cat("📂 LEYENDO DATOS...\n")
datos_cmf_raw <- read_delim(
  archivo_datos, 
  delim = ";", 
  locale = locale(encoding = "UTF-8"),
  col_types = cols(.default = col_character())
)

# Explorar la estructura inicial
cat("📋 ESTRUCTURA DE DATOS:\n")
cat("• Dimensiones:", nrow(datos_cmf_raw), "filas x", ncol(datos_cmf_raw), "columnas\n")
cat("• Columnas disponibles:\n")
print(names(datos_cmf_raw))

# Mostrar primeras filas para verificar
cat("\n🔍 PRIMERAS 3 FILAS:\n")
print(head(datos_cmf_raw, 3))

# 4. FUNCIÓN PRINCIPAL MODIFICADA SIN BETA Y RETORNO ACUMULADO
analizar_fondos_cmf_completo_modificado <- function(datos_input, generar_pdf = FALSE) {
  
  cat("\n🔍 INICIANDO ANÁLISIS COMPLETO DE FONDOS CMF \n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  # Definir TPM
  TPM_ANUAL <- 0.05
  TPM_MENSUAL <- (1 + TPM_ANUAL)^(1/12) - 1
  
  # Limpiar y preparar datos con nombres correctos
  cat("🧹 LIMPIANDO Y PREPARANDO DATOS...\n")
  datos_cmf <- datos_input %>%
    mutate(
      # Corregir conversiones con nombres reales de columnas
      FECHA_INF = as.Date(FECHA_INF, format = "%Y%m%d"),
      VALOR_CUOTA = as.numeric(gsub("[^0-9.-]", "", VALOR_CUOTA)),
      PATRIMONIO_NETO = as.numeric(gsub("[^0-9.-]", "", PATRIMONIO_NETO)),
      REM_VARIABLE = as.numeric(gsub("[^0-9.-]", "", REM_VARIABLE)),
      REM_FIJA = as.numeric(gsub("[^0-9.-]", "", REM_FIJA)),
      ACTIVO_TOT = as.numeric(gsub("[^0-9.-]", "", ACTIVO_TOT)),
      NUM_PARTICIPES = as.numeric(gsub("[^0-9.-]", "", NUM_PARTICIPES)),
      CUOTAS_EN_CIRCULACION = as.numeric(gsub("[^0-9.-]", "", CUOTAS_EN_CIRCULACION)),
      NOM_ADM = trimws(NOM_ADM),
      SERIE = trimws(SERIE),
      # Crear identificador único para cada fondo-serie
      FONDO_ID = paste(NOM_ADM, SERIE, sep = "_")
    ) %>%
    filter(!is.na(FECHA_INF), !is.na(VALOR_CUOTA), VALOR_CUOTA > 0) %>%
    arrange(FONDO_ID, FECHA_INF)
  
  cat("✅ Datos limpiados:", nrow(datos_cmf), "registros válidos\n")
  
  # Calcular retorno mensual
  cat("📈 CALCULANDO RETORNOS MENSUALES...\n")
  datos_cmf <- datos_cmf %>%
    group_by(FONDO_ID) %>%
    arrange(FECHA_INF) %>%
    mutate(retorno_mensual = VALOR_CUOTA / lag(VALOR_CUOTA) - 1) %>%
    ungroup() %>%
    filter(!is.na(retorno_mensual), retorno_mensual > -0.5, retorno_mensual < 5) # Filtrar outliers extremos
  
  cat("✅ Retornos calculados:", nrow(datos_cmf), "observaciones\n")
  
  # Retorno promedio por serie
  retorno_promedio_serie <- datos_cmf %>%
    group_by(SERIE) %>%
    summarise(
      retorno_promedio = mean(retorno_mensual, na.rm = TRUE),
      n_observaciones = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(retorno_promedio)) %>%
    mutate(retorno_promedio_pct = percent(retorno_promedio, accuracy = 0.01))
  
  # Calcular exceso de retorno
  datos_cmf <- datos_cmf %>% 
    mutate(exceso_retorno = retorno_mensual - TPM_MENSUAL)
  
  # Retorno del mercado (promedio ponderado por patrimonio)
  retorno_mercado <- datos_cmf %>%
    group_by(FECHA_INF) %>%
    summarise(
      retorno_mercado_simple = mean(retorno_mensual, na.rm = TRUE),
      retorno_mercado_ponderado = weighted.mean(retorno_mensual, PATRIMONIO_NETO, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(retorno_mercado = retorno_mercado_ponderado) # Usar el ponderado como principal
  
  # CAPM datos (solo para Alpha, sin Beta)
  datos_capm <- datos_cmf %>%
    inner_join(retorno_mercado, by = "FECHA_INF") %>%
    filter(!is.na(retorno_mensual), !is.na(retorno_mercado))
  
  cat("📊 CALCULANDO MÉTRICAS CAPM (SOLO ALPHA)...\n")
  # CAPM: Solo Alpha y R2, sin Beta
  camp_resultados <- datos_capm %>%
    group_by(FONDO_ID, NOM_ADM, SERIE) %>%
    filter(n() >= 5) %>% # Mínimo 5 observaciones para regresión confiable
    do({
      modelo <- try(lm(exceso_retorno ~ I(retorno_mercado - !!TPM_MENSUAL), data = .), silent = TRUE)
      if(inherits(modelo, "try-error") || nrow(.) < 5) {
        tibble(alpha = NA_real_, r2 = NA_real_, n_obs = nrow(.))
      } else {
        sumario <- glance(modelo)
        coef <- tidy(modelo)
        tibble(
          alpha = coef$estimate[coef$term == "(Intercept)"],
          r2 = sumario$r.squared,
          n_obs = nrow(.)
        )
      }
    }) %>%
    ungroup()
  
  # Estadísticas globales por fondo (SIN retorno acumulado)
  cat("📋 GENERANDO ESTADÍSTICAS GLOBALES...\n")
  resumen_general <- datos_cmf %>%
    group_by(FONDO_ID, NOM_ADM, SERIE) %>%
    summarise(
      patrimonio_inicial = first(PATRIMONIO_NETO, order_by = FECHA_INF),
      patrimonio_final = last(PATRIMONIO_NETO, order_by = FECHA_INF),
      activo_total_final = last(ACTIVO_TOT, order_by = FECHA_INF),
      participes_final = last(NUM_PARTICIPES, order_by = FECHA_INF),
      retorno_medio = mean(retorno_mensual, na.rm = TRUE),
      retorno_mediano = median(retorno_mensual, na.rm = TRUE),
      volatilidad = sd(retorno_mensual, na.rm = TRUE),
      sharpe_ratio = (mean(retorno_mensual, na.rm = TRUE) - TPM_MENSUAL) / sd(retorno_mensual, na.rm = TRUE),
      supera_tpm = mean(retorno_mensual > TPM_MENSUAL, na.rm = TRUE),
      n_observaciones = n(),
      fecha_inicio = min(FECHA_INF),
      fecha_fin = max(FECHA_INF),
      .groups = "drop"
    ) %>%
    filter(n_observaciones >= 3) # Filtrar fondos con muy pocas observaciones
  
  # Combinar con resultados CAPM
  resumen_general <- resumen_general %>%
    left_join(camp_resultados, by = c("FONDO_ID", "NOM_ADM", "SERIE"))
  
  # Métricas principales del mercado
  fondos_validos <- nrow(resumen_general)
  admin_unicas <- n_distinct(datos_cmf$NOM_ADM)
  patrimonio_total <- sum(resumen_general$patrimonio_final, na.rm = TRUE)
  activo_total <- sum(resumen_general$activo_total_final, na.rm = TRUE)
  participes_total <- sum(resumen_general$participes_final, na.rm = TRUE)
  rend_promedio <- mean(resumen_general$retorno_medio, na.rm = TRUE)
  rend_mediano <- median(resumen_general$retorno_medio, na.rm = TRUE)
  superan_tpm <- sum(resumen_general$retorno_medio > TPM_MENSUAL, na.rm = TRUE)
  
  # Resumen por serie
  resumen_por_serie <- resumen_general %>%
    group_by(SERIE) %>%
    summarise(
      n_fondos = n(),
      patrimonio_total = sum(patrimonio_final, na.rm = TRUE),
      activo_total = sum(activo_total_final, na.rm = TRUE),
      retorno_promedio = mean(retorno_medio, na.rm = TRUE),
      volatilidad_promedio = mean(volatilidad, na.rm = TRUE),
      sharpe_promedio = mean(sharpe_ratio, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(patrimonio_total)) %>%
    mutate(
      resumen = paste0("• ", SERIE, ": ", n_fondos, " fondos, $",
                       round(patrimonio_total / 1e9, 1), "B, ",
                       sprintf("%+.2f%%", retorno_promedio * 100), " mens.")
    )
  
  # Resumen por administradora
  resumen_por_admin <- resumen_general %>%
    group_by(NOM_ADM) %>%
    summarise(
      n_fondos = n(),
      patrimonio_total = sum(patrimonio_final, na.rm = TRUE),
      retorno_promedio = mean(retorno_medio, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(patrimonio_total))
  
  # Texto resumen
  resumen_texto <- paste0(
    "📈 **ESTADÍSTICAS GENERALES:**\n",
    "• Fondos válidos: ", format(fondos_validos, big.mark = ","), "\n",
    "• Administradoras: ", admin_unicas, "\n",
    "• Patrimonio Total: $", round(patrimonio_total / 1e9, 1), "B\n",
    "• Activos Totales: $", round(activo_total / 1e9, 1), "B\n",
    "• Partícipes Totales: ", format(participes_total, big.mark = ","), "\n",
    "• Rend. promedio: ", sprintf("%+.2f%%", rend_promedio * 100), " mensual\n",
    "• Rend. mediano: ", sprintf("%+.2f%%", rend_mediano * 100), " mensual\n",
    "• Superan TPM: ", superan_tpm, "/", fondos_validos, " (",
    sprintf("%.1f", superan_tpm / fondos_validos * 100), "%)\n\n",
    "📋 **TOP 5 SERIES POR PATRIMONIO:**\n",
    paste(head(resumen_por_serie$resumen, 5), collapse = "\n"), "\n\n",
    "🏢 **TOP 5 ADMINISTRADORAS:**\n",
    paste(head(paste0("• ", resumen_por_admin$NOM_ADM, ": ", resumen_por_admin$n_fondos, 
                      " fondos, $", round(resumen_por_admin$patrimonio_total/1e9, 1), "B"), 5), 
          collapse = "\n")
  )
  
  # Gráficos
  cat("📊 GENERANDO GRÁFICOS...\n")
  
  # Gráfico CAPM (Solo Alpha vs R2) - SIN BETA
  capm_grafico <- camp_resultados %>%
    filter(!is.na(alpha), abs(alpha) < 0.05, r2 > 0.1) %>%
    ggplot(aes(x = r2, y = alpha, color = SERIE)) +
    geom_point(alpha = 0.7, size = 3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title = "Alpha vs R² (Modelo CAPM) de Fondos Mutuos",
      subtitle = "Línea roja: Alpha=0 (sin retorno anormal)",
      x = "R² (Calidad del Ajuste)",
      y = "Alpha (Retorno Anormal Mensual)"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Gráfico de rentabilidad temporal
  retorno_mensual_promedio <- datos_cmf %>%
    group_by(FECHA_INF) %>%
    summarise(
      retorno_promedio = mean(retorno_mensual, na.rm = TRUE),
      retorno_mediano = median(retorno_mensual, na.rm = TRUE),
      retorno_ponderado = weighted.mean(retorno_mensual, PATRIMONIO_NETO, na.rm = TRUE),
      .groups = "drop"
    )
  
  g_retorno_mensual <- ggplot(retorno_mensual_promedio, aes(x = FECHA_INF)) +
    geom_line(aes(y = retorno_promedio, color = "Promedio Simple"), size = 1, alpha = 0.8) +
    geom_line(aes(y = retorno_ponderado, color = "Promedio Ponderado"), size = 1, alpha = 0.8) +
    geom_hline(yintercept = TPM_MENSUAL, linetype = "dashed", color = "red", alpha = 0.7) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    scale_color_manual(values = c("Promedio Simple" = "darkgreen", "Promedio Ponderado" = "darkblue")) +
    labs(
      title = "Rentabilidad Mensual de Fondos Mutuos",
      subtitle = "Línea roja: TPM mensual (~5% anual)",
      x = "Fecha", 
      y = "Retorno mensual",
      color = "Tipo de Promedio"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # NUEVO GRÁFICO: TOP 20 FONDOS POR ALPHA - RIESGO VS GENERACIÓN DE VALOR
  top_alpha_data <- camp_resultados %>%
    filter(!is.na(alpha), r2 > 0.1) %>%
    left_join(resumen_general %>% select(FONDO_ID, volatilidad, patrimonio_final), by = "FONDO_ID") %>%
    arrange(desc(alpha)) %>%
    slice_head(n = 20) %>%
    mutate(
      fondo_label = paste(substr(NOM_ADM, 1, 12), SERIE, sep = "-"),
      alpha_positivo = alpha > 0,
      tamano_patrimonio = case_when(
        patrimonio_final >= 1e9 ~ "Grande (>$1B)",
        patrimonio_final >= 100e6 ~ "Mediano ($100M-$1B)",
        TRUE ~ "Pequeño (<$100M)"
      )
    )
  
  g_top_alpha_riesgo <- top_alpha_data %>%
    ggplot(aes(x = volatilidad, y = alpha)) +
    geom_point(aes(color = SERIE, size = patrimonio_final), alpha = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
    geom_vline(xintercept = mean(top_alpha_data$volatilidad, na.rm = TRUE), 
               linetype = "dotted", color = "blue", alpha = 0.5) +
    geom_text(aes(label = fondo_label), size = 2.5, vjust = -0.5, hjust = 0.5, alpha = 0.8) +
    scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
    scale_y_continuous(labels = percent_format(accuracy = 0.01)) +
    scale_size_continuous(
      name = "Patrimonio",
      labels = function(x) paste0("$", round(x/1e6, 0), "M"),
      range = c(2, 8)
    ) +
    labs(
      title = "TOP 20 Fondos por Alpha: Riesgo vs Generación de Valor",
      subtitle = "Línea roja: Alpha=0 | Línea azul punteada: Volatilidad promedio del top 20",
      x = "Volatilidad (Riesgo)",
      y = "Alpha (Generación de Valor Mensual)",
      color = "Serie"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "gray50")
    ) +
    guides(
      color = guide_legend(override.aes = list(size = 4)),
      size = guide_legend(override.aes = list(alpha = 1))
    )
  
  # GRÁFICO: TOP FONDOS VS TPM (mantener el existente)
  top_fondos_data <- resumen_general %>%
    arrange(desc(retorno_medio)) %>%
    slice_head(n = 20) %>%
    mutate(
      fondo_label = paste(substr(NOM_ADM, 1, 15), SERIE, sep = "\n"),
      supera_tpm = retorno_medio > TPM_MENSUAL
    )
  
  g_top_vs_tpm <- top_fondos_data %>%
    ggplot(aes(x = reorder(fondo_label, retorno_medio), y = retorno_medio, fill = supera_tpm)) +
    geom_col(alpha = 0.8) +
    geom_hline(yintercept = TPM_MENSUAL, linetype = "dashed", color = "red", size = 1) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    scale_fill_manual(
      values = c("TRUE" = "darkgreen", "FALSE" = "darkred"),
      labels = c("Bajo TPM", "Sobre TPM"),
      name = "Desempeño"
    ) +
    labs(
      title = "TOP 20 Fondos: Retorno Mensual vs TPM",
      subtitle = paste("Línea roja: TPM mensual =", percent(TPM_MENSUAL, 0.02)),
      x = "Fondo (Administradora + Serie)",
      y = "Retorno Mensual Promedio"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      legend.position = "bottom"
    ) +
    coord_flip()
  
  # Tablas
  cat("📋 GENERANDO TABLAS...\n")
  
  # Top 15 fondos por rentabilidad (SIN retorno acumulado)
  tabla_top15_fondos <- resumen_general %>%
    arrange(desc(retorno_medio)) %>%
    slice_head(n = 15) %>%
    mutate(
      `Retorno Mensual` = percent(retorno_medio, 0.02),
      `Volatilidad` = percent(volatilidad, 0.02),
      `Sharpe Ratio` = round(sharpe_ratio, 3),
      `Patrimonio (MM$)` = round(patrimonio_final / 1e6, 1),
      `Partícipes` = format(participes_final, big.mark = ",")
    ) %>%
    select(
      Administradora = NOM_ADM,
      Serie = SERIE,
      `Retorno Mensual`,
      Volatilidad,
      `Sharpe Ratio`,
      `Patrimonio (MM$)`,
      Partícipes
    )
  
  # Top 15 fondos por Alpha (SIN BETA)
  tabla_top_alpha <- camp_resultados %>%
    filter(!is.na(alpha), r2 > 0.1) %>%
    arrange(desc(alpha)) %>%
    slice_head(n = 15) %>%
    mutate(
      `Alpha (%)` = round(alpha * 100, 3),
      `R²` = round(r2, 3),
      `N Obs` = n_obs
    ) %>%
    left_join(resumen_general %>% select(FONDO_ID, patrimonio_final), by = "FONDO_ID") %>%
    mutate(`Patrimonio (MM$)` = round(patrimonio_final / 1e6, 1)) %>%
    select(
      Administradora = NOM_ADM,
      Serie = SERIE,
      `Alpha (%)`,
      `R²`,
      `N Obs`,
      `Patrimonio (MM$)`
    )
  
  # Tabla resumen por administradora
  tabla_administradoras <- resumen_por_admin %>%
    slice_head(n = 10) %>%
    mutate(
      `N° Fondos` = n_fondos,
      `Patrimonio (B$)` = round(patrimonio_total / 1e9, 2),
      `Retorno Prom (%)` = round(retorno_promedio * 100, 2)
    ) %>%
    select(
      Administradora = NOM_ADM,
      `N° Fondos`,
      `Patrimonio (B$)`,
      `Retorno Prom (%)`
    )
  
  # Mostrar resumen en consola
  cat("\n", resumen_texto, "\n")
  
  # Mostrar gráficos en consola
  print(capm_grafico)
  print(g_retorno_mensual)
  print(g_top_alpha_riesgo)  # NUEVO GRÁFICO
  print(g_top_vs_tpm)
  
  # GENERAR PDF SI ESTÁ HABILITADO
  if(generar_pdf) {
    cat("\n📄 GENERANDO PDF...\n")
    
    # Nombre del archivo PDF
    nombre_pdf <- "Resumen_Fondos_CMF_R_Finanzas.pdf"
    
    # Abrir dispositivo PDF
    pdf(nombre_pdf, width = 16, height = 11, paper = "a4r")
    
    # PÁGINA 1: PORTADA CON RESUMEN EJECUTIVO
    grid.newpage()
    
    # Título principal
    grid.text("ANÁLISIS DE FONDOS MUTUOS CMF ", 
              x = 0.5, y = 0.9, 
              gp = gpar(fontsize = 24, fontface = "bold", col = "darkblue"))
    
    # Subtítulo
    grid.text(paste("Período: Mayo 2025 | Fecha de Análisis:", Sys.Date()), 
              x = 0.5, y = 0.85, 
              gp = gpar(fontsize = 14, col = "gray50"))
    
    # Resumen ejecutivo
    resumen_limpio <- gsub("📈|📋|🏢|•", "", resumen_texto)
    resumen_limpio <- gsub("\\*\\*|\\*", "", resumen_limpio)
    
    grid.text(resumen_limpio, 
              x = 0.1, y = 0.7, 
              just = c("left", "top"),
              gp = gpar(fontsize = 11, lineheight = 1.2))
    
    # PÁGINA 2: GRÁFICO CAPM (SIN BETA)
    print(capm_grafico)
    
    # PÁGINA 3: RENTABILIDAD TEMPORAL
    print(g_retorno_mensual)
    
    # PÁGINA 4: NUEVO GRÁFICO TOP 20 ALPHA - RIESGO VS VALOR
    print(g_top_alpha_riesgo)
    
    # PÁGINA 5: GRÁFICO TOP VS TPM
    print(g_top_vs_tpm)
    
    # PÁGINA 6: TABLA TOP FONDOS (SIN RETORNO ACUMULADO)
    grid.newpage()
    grid.text("TOP 15 FONDOS POR RENTABILIDAD", 
              x = 0.5, y = 0.95, 
              gp = gpar(fontsize = 18, fontface = "bold"))
    
    # Convertir tabla a grid table
    tabla_grid <- tableGrob(tabla_top15_fondos, rows = NULL, theme = ttheme_default())
    grid.draw(tabla_grid)
    
    # PÁGINA 7: TABLA ALPHA (SIN BETA)
    grid.newpage()
    grid.text("TOP 15 FONDOS POR ALPHA (SIN BETA)", 
              x = 0.5, y = 0.95, 
              gp = gpar(fontsize = 18, fontface = "bold"))
    
    tabla_alpha_grid <- tableGrob(tabla_top_alpha, rows = NULL, theme = ttheme_default())
    grid.draw(tabla_alpha_grid)
    
    # PÁGINA 8: TABLA ADMINISTRADORAS
    grid.newpage()
    grid.text("TOP 10 ADMINISTRADORAS", 
              x = 0.5, y = 0.95, 
              gp = gpar(fontsize = 18, fontface = "bold"))
    
    tabla_admin_grid <- tableGrob(tabla_administradoras, rows = NULL, theme = ttheme_default())
    grid.draw(tabla_admin_grid)
    
    # CÓDIGO PARA AÑADIR PÁGINA DE RECOMENDACIONES AL PDF
    # Insertar este código DESPUÉS de la página 8 (tablas administradoras) y ANTES del dev.off()
    
    # CÓDIGO PARA AÑADIR PÁGINA DE RECOMENDACIONES AL PDF
    # Insertar este código DESPUÉS de la página 8 (tablas administradoras) y ANTES del dev.off()
    
    # PÁGINA 9: RECOMENDACIONES ESTRATÉGICAS
    grid.newpage()
    grid.text("RECOMENDACIONES ESTRATÉGICAS", 
              x = 0.5, y = 0.95, 
              gp = gpar(fontsize = 20, fontface = "bold", col = "darkblue"))
    
    grid.text("Basadas en el Análisis de Fondos Mutuos CMF", 
              x = 0.5, y = 0.90, 
              gp = gpar(fontsize = 12, col = "gray50"))
    
    # Generar recomendaciones dinámicas basadas en los datos
    top_performer <- resultado$tablas$top_fondos[1, "Administradora"]
    mejor_alpha <- resultado$tablas$top_alpha[1, "Administradora"]
    fondos_sobre_tpm <- sum(resultado$metricas$retorno_medio > TPM_MENSUAL, na.rm = TRUE)
    total_fondos <- nrow(resultado$metricas)
    pct_sobre_tpm <- round(fondos_sobre_tpm / total_fondos * 100, 1)
    
    # Crear texto de recomendaciones
    recomendaciones_texto <- paste0(
      "PARA INVERSIONISTAS CONSERVADORES:\n",
      "• Considerar fondos serie A con Sharpe Ratio > 0.5 y volatilidad < 3%\n",
      "• Priorizar administradoras con historial consistente sobre TPM\n",
      "• Diversificar entre 3-5 fondos de diferentes administradoras\n\n",
      
      "PARA INVERSIONISTAS MODERADOS:\n",
      "• Evaluar fondos con Alpha positivo y R² > 30% para mejor predictibilidad\n",
      "• Balancear entre series A (conservadoras) y B (crecimiento)\n",
      "• Monitorear fondos que consistentemente superan el mercado ponderado\n\n",
      
      "PARA INVERSIONISTAS AGRESIVOS:\n",
      "• Analizar fondos del top 20 por Alpha con patrimonio > $100M\n",
      "• Considerar fondos con volatilidad alta pero Alpha consistentemente positivo\n",
      "• Evaluar oportunidades en administradoras emergentes con buen desempeño\n\n",
      
      "HALLAZGOS CLAVE DEL ANÁLISIS:\n",
      "• Solo ", pct_sobre_tpm, "% de los fondos superan consistentemente la TPM\n",
      "• La administradora ", top_performer, " lidera en rentabilidad promedio\n",
      "• ", mejor_alpha, " muestra el mejor Alpha (generación de valor ajustado)\n",
      "• La correlación con el mercado varía significativamente entre fondos\n\n",
      
      "RECOMENDACIONES OPERATIVAS:\n",
      "• Revisar portafolio trimestralmente, rebalancear semestralmente\n",
      "• Establecer stop-loss en -15% para fondos individuales\n",
      "• Mantener reserva de liquidez del 10-20% para oportunidades\n",
      "• Considerar costos de administración en decisiones finales"
    )
    
    # Mostrar texto de recomendaciones
    grid.text(recomendaciones_texto, 
              x = 0.05, y = 0.82, 
              just = c("left", "top"),
              gp = gpar(fontsize = 10, lineheight = 1.3))
    
   

    
    # Cerrar PDF
    dev.off()
    
    cat("✅ PDF generado exitosamente:", nombre_pdf, "\n")
    cat("📁 Ubicación:", getwd(), "/", nombre_pdf, "\n")
  }
  
  # Retornar resultados
  return(list(
    datos = datos_cmf,
    resumen = resumen_texto,
    graficos = list(
      capm = capm_grafico, 
      rentabilidad = g_retorno_mensual,
      alpha_riesgo = g_top_alpha_riesgo,  # NUEVO GRÁFICO
      top_vs_tpm = g_top_vs_tpm
    ),
    tablas = list(
      top_fondos = tabla_top15_fondos, 
      top_alpha = tabla_top_alpha,
      administradoras = tabla_administradoras
    ),
    metricas = resumen_general,
    series = retorno_promedio_serie,
    capm = camp_resultados
  ))
}

# 5. EJECUTAR ANÁLISIS CON PDF 
cat("\n🚀 INICIANDO ANÁLISIS ...\n")

# Ejecutar el análisis con generación de PDF
resultado <- analizar_fondos_cmf_completo_modificado(datos_cmf_raw, generar_pdf = GENERAR_PDF)

# Guardar resultados en variables globales para fácil acceso
datos_finales <- resultado$datos
top_fondos <- resultado$tablas$top_fondos
tabla_alpha <- resultado$tablas$top_alpha
tabla_administradoras <- resultado$tablas$administradoras
metricas_fondos <- resultado$metricas

#renv::init
#renv::snapshot()
