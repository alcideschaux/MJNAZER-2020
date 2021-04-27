# Función para plotear gráficos de barras univariables
plot_bar <- function(data, x, xlab = NULL) {
    x = enquo(x)
    
    data %>% 
    freq_table(!! x) %>% 
        ggplot(aes(x = !! x, y = n, fill = !! x)) +
        geom_bar(stat = "identity", color = "black") +
        geom_text(aes(label = paste0("(n = ", n, ")")), y = 0, vjust = 5) +
        geom_label(aes(label = paste0(round(prop), "%"), y = n/2), fill = "white") +
        labs(x = xlab, y = "Núm. casos") +
        scale_fill_brewer(palette = "Dark2") +
        coord_cartesian(clip = "off") +
        theme(
            legend.position = "bottom",
            legend.title = element_blank(),
            axis.title = element_text(face = "bold"),
            axis.title.x = element_text(margin = margin(10, unit = "pt")),
            axis.title.y = element_text(margin = margin(10, unit = "pt")),
            axis.text.x = element_text(margin = margin(b = 25, unit = "pt"))
        )
}

# Función para plotear gráficos de barras multivariables, con estadísticos
plot_barstats <- function(data, x, y, xlab = NULL, legend = NULL) {
    x = enquo(x)
    y = enquo(y)
    ggbarstats(
    data, y = !! x, x = !! y,
    bf.message = FALSE,
    proportion.test = FALSE,
    xlab = xlab,
    legend.title = legend,
    ggtheme = ggplot2::theme_gray()
) + 
    theme(
        legend.position = "bottom"
    )
}

# Función para plotear histogramas univariables
plot_histogram <- function(data, x, xlab = NULL, bins = 12, digits = 1, yaxis = 15) {
    x <- enquo(x)
    x_summary <- data %>% 
        summarize(
            prom = mean(!! x, na.rm = TRUE),
            de = sd(!! x, na.rm = TRUE),
            min = min(!! x, na.rm = TRUE),
            max = max(!! x, na.rm = TRUE)
        )
    prom <- x_summary %>% pull(prom)
    de <- x_summary %>% pull(de)
    min <- x_summary %>% pull(min)
    max <- x_summary %>% pull(max)
    
    ggplot(data, aes(x = !! x)) +
    geom_histogram(bins = bins, fill = "#7570b3", color = "black", alpha = .8) +
    geom_vline(xintercept = prom, size = 1.5, linetype = 5) +
    geom_label(
        label = paste0("Promedio: ", round(prom, digits), " +/- ", round(de, digits)),
        x = prom,
        y = yaxis
    ) +
    geom_label(label = paste0("Min: ", min), x = min, y = 0) +
    geom_label(label = paste0("Max: ", max), x = max, y = 0) +
    labs(x = xlab, y = "Núm. casos") +
    theme(axis.title = element_text(face = "bold"))
}