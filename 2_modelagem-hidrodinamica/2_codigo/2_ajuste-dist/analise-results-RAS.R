#Código para geração de gráficos para resultados de modelagem hidrodinâmica
#e para ajuste de distribuições de probabilidades
#Gabriel Fonseca Cunha e Chat GPT 4.0, 2024

# ---------------- BIBLIOTECAS ---------------
library(ggplot2)
library(tidyverse)
library(stats)


# ---------------- LEITURA DE DADOS ---------------
#Abre arquivo de dados e constrói dataframe
dat <- openxlsx::read.xlsx(xlsxFile = 'D:/output/RAS_results_df.xlsx',
                           sheet    = 'Database',
                           rows     = 1:1501,
                           cols     = 1:13)

class(dat)
head(dat)

dat_rejeito <- dat %>% filter(Reologia != "agua")

# ---------------- CONSTRUÇÃO DE GRÁFICOS ---------------
#Função para criar gráfico genérico
rotulo <- data.frame(
  var = c('Decl_long', 'Decl_trans', 'Prob_exc', '1_P_nao_exc',  'tau_y', 'K', 'n', 'Vel_max', 'Prof_max'),
  rot = c('Declividade longitudinal (m/m)',
          'Declividade transversal (m/m)',
          'Probabilidade acumulada',
          'Probabilidade acumulada',
          'Tensão Limite de Escoamento (Pa)',
          'Índice de Consistência',
          'Índice de Escoamento',
          'Velocidade máxima (m/s)',
          'Profundidade máxima (m)'))

plot_custom1 <- function(data, XS_filtro, x_col, y_col, color_col, shape_col) {
  
  data_filtro <- data %>% filter(XS == XS_filtro)
  Ncolor <- n_distinct(data_filtro$color_col)
  Nshape <- n_distinct(data_filtro$shape_col)
  x_lab <- rotulo$rot[which(rotulo$var == x_col)]
  y_lab <- rotulo$rot[which(rotulo$var == y_col)]
  color_lab <- rotulo$rot[which(rotulo$var == color_col)]
  shape_lab <- rotulo$rot[which(rotulo$var == shape_col)]
  
  ggplot(data_filtro, aes(x = .data[[x_col]], y = .data[[y_col]], 
                          color = factor(.data[[color_col]]), 
                          shape = factor(.data[[shape_col]]))) +
  geom_point() +
  theme_minimal() +
  labs(x = x_lab, y = y_lab, color = color_lab, shape = shape_lab) +
  theme(legend.position = "bottom") +
  ggtitle(paste(y_lab, " vs ", x_lab, ", ", XS_filtro))
}

plot_custom2 <- function(data, XS_filtro, x_col, y_col, color_col, shape_col) {
  
  data_filtro <- data %>% filter(XS == XS_filtro)
  Ncolor <- n_distinct(data_filtro$color_col)
  Nshape <- n_distinct(data_filtro$shape_col)
  x_lab <- rotulo$rot[which(rotulo$var == x_col)]
  y_lab <- rotulo$rot[which(rotulo$var == y_col)]
  color_lab <- rotulo$rot[which(rotulo$var == color_col)]
  shape_lab <- rotulo$rot[which(rotulo$var == shape_col)]
  
  ggplot(data_filtro, aes(x = .data[[x_col]], y = .data[[y_col]], 
                          color = factor(.data[[color_col]]), 
                          shape = factor(.data[[shape_col]]))) +
    geom_point() +
    ylim(c(0.01,0.99)) +
    theme_minimal() +
    labs(x = x_lab, y = y_lab, color = color_lab, shape = shape_lab) +
    theme(legend.position = "bottom") +
    ggtitle(paste(y_lab, " vs ", x_lab, ", ", XS_filtro))
}

#Função para criar o gráfico
plot_XS1_TLE_prof <- plot_custom1(dat_rejeito, 'XS1', 'tau_y', 'Prof_max', 'Decl_long', 'Decl_trans')
plot_XS1_K_prof  <-  plot_custom1(dat_rejeito, 'XS1', 'K',     'Prof_max', 'Decl_long', 'Decl_trans')
plot_XS1_n_prof  <-  plot_custom1(dat_rejeito, 'XS1', 'n',     'Prof_max', 'Decl_long', 'Decl_trans')

plot_XS1_TLE_vel <- plot_custom1(dat_rejeito, 'XS1', 'tau_y', 'Vel_max', 'Decl_long', 'Decl_trans')
plot_XS1_K_vel  <-  plot_custom1(dat_rejeito, 'XS1', 'K',     'Vel_max', 'Decl_long', 'Decl_trans')
plot_XS1_n_vel  <-  plot_custom1(dat_rejeito, 'XS1', 'n',     'Vel_max', 'Decl_long', 'Decl_trans')

plot_XS2_TLE_prof <- plot_custom1(dat_rejeito, 'XS2', 'tau_y', 'Prof_max', 'Decl_long', 'Decl_trans')
plot_XS2_K_prof  <-  plot_custom1(dat_rejeito, 'XS2', 'K',     'Prof_max', 'Decl_long', 'Decl_trans')
plot_XS2_n_prof  <-  plot_custom1(dat_rejeito, 'XS2', 'n',     'Prof_max', 'Decl_long', 'Decl_trans')

plot_XS2_TLE_vel <- plot_custom1(dat_rejeito, 'XS2', 'tau_y', 'Vel_max',  'Decl_long', 'Decl_trans')
plot_XS2_K_vel  <-  plot_custom1(dat_rejeito, 'XS2', 'K',     'Vel_max',  'Decl_long', 'Decl_trans')
plot_XS2_n_vel  <-  plot_custom1(dat_rejeito, 'XS2', 'n',     'Vel_max',  'Decl_long', 'Decl_trans')

plot_XS1_vel  <- plot_custom2(dat_rejeito, 'XS1', 'Vel_max',  '1_P_nao_exc',  'Decl_long', 'Decl_trans')
plot_XS1_prof <- plot_custom2(dat_rejeito, 'XS1', 'Prof_max', 'Prob_exc', 'Decl_long', 'Decl_trans')

plot_XS2_vel  <- plot_custom2(dat_rejeito, 'XS2', 'Vel_max',  '1_P_nao_exc',  'Decl_long', 'Decl_trans')
plot_XS2_prof <- plot_custom2(dat_rejeito, 'XS2', 'Prof_max', 'Prob_exc', 'Decl_long', 'Decl_trans')

plots <- list(plot_XS1_TLE_prof = plot_XS1_TLE_prof,
              plot_XS1_K_prof = plot_XS1_K_prof,
              plot_XS1_n_prof = plot_XS1_n_prof,
              plot_XS1_TLE_vel = plot_XS1_TLE_vel,
              plot_XS1_K_vel = plot_XS1_K_vel,
              plot_XS1_n_vel = plot_XS1_n_vel,
              plot_XS2_TLE_prof = plot_XS2_TLE_prof,
              plot_XS2_K_prof = plot_XS2_K_prof,
              plot_XS2_n_prof = plot_XS2_n_prof,
              plot_XS2_TLE_vel = plot_XS2_TLE_vel,
              plot_XS2_K_vel = plot_XS2_K_vel,
              plot_XS2_n_vel = plot_XS2_n_vel,
              plot_XS1_vel = plot_XS1_vel,
              plot_XS1_prof = plot_XS1_prof,
              plot_XS2_vel = plot_XS2_vel,
              plot_XS2_prof = plot_XS2_prof)

for (name in names(plots)) {
  ggsave(
   filename = paste0("D:/", name, ".jpg"),
   plot = plots[[name]],
   device = "jpg",
   scale = 1,
   units = "px",
   width = 1200,
   height = 550,
   dpi = 120)
}

rm(plot_XS1_vel, plot_XS1_prof, plot_XS2_vel, plot_XS2_prof, plots)
gc()

# ---------------- AJUSTE FAP PROFUNDIDADE ---------------
cXS <- c('XS2', 'XS1')
cDecl_long <- c(0.001, 0.005, 0.01)
cDecl_trans <- c(0.02, 0.05, 0.1, 0.2, 0.3)


param_norm <- data.frame(XS=character(),
                         Decl_long=numeric(),
                         Decl_trans=numeric(),
                         mean=numeric(),
                         sd=numeric(),
                         SSE=numeric())

param_lnorm <- data.frame(XS=character(),
                          Decl_long=numeric(),
                          Decl_trans=numeric(),
                          meanlog=numeric(),
                          sdlog=numeric(),
                          SSE=numeric())

param_logis <- data.frame(XS=character(),
                          Decl_long=numeric(),
                          Decl_trans=numeric(),
                          location=numeric(),
                          scale=numeric(),
                          SSE=numeric())

rotlong <- data.frame(
  long = cDecl_long,
  i = c('i001', 'i005', 'i01'))

rottrans <- data.frame(
  trans = cDecl_trans,
  Z = c('Z02', 'Z05', 'Z1', 'Z2', 'Z3'))

for (XSi in cXS) {
  for (Decl_longi in cDecl_long) {
    for (Decl_transi in cDecl_trans) {
      geom <- paste0(rotlong$i[which(rotlong$long == Decl_longi)],
                     rottrans$Z[which(rottrans$trans == Decl_transi)])
      
      data_filtro <- dat_rejeito %>% filter(XS == XSi,
                                            Decl_long == Decl_longi,
                                            Decl_trans == Decl_transi)
      
      df <- select(data_filtro, Prof_max, Prob_nao_exc)
      
      # Function to calculate the sum of squared errors (SSE) between empirical and theoretical CDFs
      sse_normal <- function(params) {
        mu <- params[1]
        sigma <- params[2]
        predicted <- pnorm(df$Prof_max, mean = mu, sd = sigma)
        sum((df$Prob_nao_exc - predicted)^2)
      }
      
      sse_lognormal <- function(params) {
        meanlog <- params[1]
        sdlog <- params[2]
        predicted <- plnorm(df$Prof_max, meanlog = meanlog, sdlog = sdlog)
        sum((df$Prob_nao_exc - predicted)^2)
      }
      
      sse_logistic <- function(params) {
        location <- params[1]
        scale <- params[2]
        predicted <- plogis(df$Prof_max, location = location, scale = scale)
        sum((df$Prob_nao_exc - predicted)^2)
      }
      
      # Optimization to find parameters that minimize the SSE
      optim_normal <- optim(par = c(mu = mean(df$Prof_max), sigma = 1), fn = sse_normal)
      optim_lognormal <- optim(par = c(meanlog = 0, sdlog = 1), fn = sse_lognormal)
      optim_logistic <- optim(par = c(location = mean(df$Prof_max), scale = 1), fn = sse_logistic)
      
      # Results
      param_norm <- param_norm %>% add_row(XS = XSi,
                                           Decl_long = Decl_longi,
                                           Decl_trans = Decl_transi,
                                           mean = optim_normal$par[1],
                                           sd = optim_normal$par[2],
                                           SSE = optim_normal$value)
      
      param_lnorm <- param_lnorm %>% add_row(XS = XSi,
                                             Decl_long = Decl_longi,
                                             Decl_trans = Decl_transi,
                                             meanlog = optim_lognormal$par[1],
                                             sdlog = optim_lognormal$par[2],
                                             SSE = optim_lognormal$value)
      
      param_logis <- param_logis %>% add_row(XS = XSi,
                                             Decl_long = Decl_longi,
                                             Decl_trans = Decl_transi,
                                             location = optim_logistic$par[1],
                                             scale = optim_logistic$par[2],
                                             SSE = optim_logistic$value)
      
      
      # ---------------- Geração de gráfico FAP ---------------
      # Generate a sequence of x-values covering the range of your data for plotting the CDFs
      x_range <- seq(0.7*min(df$Prof_max), 1.3*max(df$Prof_max), length.out = 100)
      
      # Calculate the theoretical CDFs using the optimized parameters
      cdf_normal <- pnorm(x_range, mean = optim_normal$par[1], sd = optim_normal$par[2])
      cdf_lognormal <- plnorm(x_range, meanlog = optim_lognormal$par[1], sdlog = optim_lognormal$par[2])
      cdf_logistic <- plogis(x_range, location = optim_logistic$par[1], scale = optim_logistic$par[2])
      
      # Prepare the data for plotting
      plot_data <- data.frame(Prof_max = x_range,
                              Normal = cdf_normal,
                              Lognormal = cdf_lognormal,
                              Logistic = cdf_logistic)
      
      # Plot
      p1 <- ggplot(df, aes(x = Prof_max, y = Prob_nao_exc)) +
        geom_line(data = plot_data, aes(x = Prof_max, y = Normal, colour = "Normal"),
                  linewidth  = 0.7) +
        geom_line(data = plot_data, aes(x = Prof_max, y = Lognormal, colour = "Lognormal"),
                  linewidth  = 0.7) +
        geom_line(data = plot_data, aes(x = Prof_max, y = Logistic, colour = "Logística"),
                  linewidth  = 0.7) +
        geom_point(aes(y = Prob_nao_exc), colour = "black", size = 1.5) +
        ggtitle(paste0("Dados vs. FAP ajustada, ", XSi, ", i=", Decl_longi, ", Z=", Decl_transi)) +
        labs(x = "Profundidade Máxima (m)", y = "FAP") +
        scale_colour_manual(values = c("Normal" = "blue", "Lognormal" = "green", "Logística" = "red")) +
        theme_minimal()
      
      ggsave(filename = paste0("D:/prof_", geom, "_", XSi, "_FAP.jpg"),
             plot = p1,
             device = "jpg",
             scale = 1,
             units = "px",
             width = 650,
             height = 400,
             dpi = 120)
      
      # ---------------- Geração de gráfico FDP ---------------
      # Generate a sequence of x-values covering the range of your data for plotting the CDFs
      x_range <- seq(0.7*min(df$Prof_max), 1.3*max(df$Prof_max), length.out = 100)
      
      # Calculate the theoretical CDFs using the optimized parameters
      pdf_normal <- dnorm(x_range, mean = optim_normal$par[1], sd = optim_normal$par[2])
      pdf_lognormal <- dlnorm(x_range, meanlog = optim_lognormal$par[1], sdlog = optim_lognormal$par[2])
      pdf_logistic <- dlogis(x_range, location = optim_logistic$par[1], scale = optim_logistic$par[2])
      
      # Prepare the data for plotting
      plot_data_pdf <- data.frame(Prof_max = x_range,
                                  Normal = pdf_normal,
                                  Lognormal = pdf_lognormal,
                                  Logistic = pdf_logistic)
      
      # Plot
      p2 <- ggplot(data = plot_data_pdf) + 
        geom_line(aes(x = Prof_max, y = Normal, colour = "Normal"), linewidth = 1) +
        geom_line(aes(x = Prof_max, y = Lognormal, colour = "Lognormal"), linewidth = 1) +
        geom_line(aes(x = Prof_max, y = Logistic, colour = "Logistic"), linewidth = 1) +
        labs(title = paste0("FDP ajustada, ", XSi, ", i=", Decl_longi, ", Z=", Decl_transi),
             x = "Profundidade Máxima (m)", y = "FDP") +
        scale_colour_manual(values = c("Normal" = "blue", "Lognormal" = "green", "Logistic" = "red")) +
        theme_minimal()
      
      ggsave(filename = paste0("D:/prof_", geom, "_", XSi, "_FDP.jpg"),
             plot = p2,
             device = "jpg",
             scale = 1,
             units = "px",
             width = 650,
             height = 400,
             dpi = 120)
    }
  }
}


# ---------------- AJUSTE FAP PROFUNDIDADE - SOMENTE LOGNORMAL ---------------
cXS <- c('XS1', 'XS2')
cDecl_long <- c(0.005, 0.01) # Removida 0.001
cDecl_trans <- c(0.02, 0.05, 0.1, 0.2, 0.3)

param_lnorm <- data.frame(XS=character(),
                          Decl_long=numeric(),
                          Decl_trans=numeric(),
                          meanlog=numeric(),
                          sdlog=numeric(),
                          SSE=numeric())

rotlong <- data.frame(
  long = cDecl_long,
  i = c('i005', 'i01'))

rottrans <- data.frame(
  trans = cDecl_trans,
  Z = c('Z02', 'Z05', 'Z1', 'Z2', 'Z3'))

for (XSi in cXS) {
  for (Decl_longi in cDecl_long) {
    for (Decl_transi in cDecl_trans) {
      geom <- paste0(rotlong$i[which(rotlong$long == Decl_longi)],
                     rottrans$Z[which(rottrans$trans == Decl_transi)])
      
      data_filtro <- dat_rejeito %>% filter(XS == XSi,
                                            Decl_long == Decl_longi,
                                            Decl_trans == Decl_transi)
      
      df <- select(data_filtro, Prof_max, Prob_nao_exc)
      
      # Function to calculate the sum of squared errors (SSE) between empirical and theoretical CDFs
      sse_lognormal <- function(params) {
        meanlog <- params[1]
        sdlog <- params[2]
        predicted <- plnorm(df$Prof_max, meanlog = meanlog, sdlog = sdlog)
        sum((df$Prob_nao_exc - predicted)^2)
      }
      
      # Optimization to find parameters that minimize the SSE
      optim_lognormal <- optim(par = c(meanlog = 0, sdlog = 1), fn = sse_lognormal)
      
      # Results
      param_lnorm <- param_lnorm %>% add_row(XS = XSi,
                                             Decl_long = Decl_longi,
                                             Decl_trans = Decl_transi,
                                             meanlog = optim_lognormal$par[1],
                                             sdlog = optim_lognormal$par[2],
                                             SSE = optim_lognormal$value)
      
      # ---------------- Geração de gráfico FAP ---------------
      # Generate a sequence of x-values covering the range of your data for plotting the CDFs
      x_range <- seq(0.7*min(df$Prof_max), 1.3*max(df$Prof_max), length.out = 100)
      
      # Calculate the theoretical CDFs using the optimized parameters
      cdf_lognormal <- plnorm(x_range, meanlog = optim_lognormal$par[1], sdlog = optim_lognormal$par[2])
      
      # Prepare the data for plotting
      plot_data <- data.frame(Prof_max = x_range,
                              Lognormal = cdf_lognormal)
      
      # Plot
      p1 <- ggplot(df, aes(x = Prof_max, y = Prob_nao_exc)) +
        geom_point(colour = "black", size = 1.5) +
        geom_line(data = plot_data, aes(x = Prof_max, y = Lognormal), linewidth = 0.7, colour = "red") +
        ggtitle(paste0("Dados vs. FAP ajustada, ", XSi, ", i=", Decl_longi, ", Z=", Decl_transi)) +
        labs(x = "Profundidade Máxima (m)", y = "Probabilidade Acumulada") +
        theme_minimal() +
        theme(legend.position = "none")
      
      ggsave(filename = paste0("D:/prof_", geom, "_", XSi, "_FAP_lnorm.jpg"),
             plot = p1,
             device = "jpg",
             scale = 1,
             units = "px",
             width = 650,
             height = 400,
             dpi = 120)

      # ---------------- Geração de gráfico FDP ---------------
      # Generate a sequence of x-values covering the range of your data for plotting the CDFs
      x_range <- seq(0.7*min(df$Prof_max), 1.3*max(df$Prof_max), length.out = 100)
      
      # Calculate the theoretical CDFs using the optimized parameters
      pdf_lognormal <- dlnorm(x_range, meanlog = optim_lognormal$par[1], sdlog = optim_lognormal$par[2])
      
      # Prepare the data for plotting
      plot_data_pdf <- data.frame(Prof_max = x_range,
                                  Lognormal = pdf_lognormal)
      
      # Plot
      p2 <- ggplot(data = plot_data_pdf) +
        geom_line(aes(x = Prof_max, y = Lognormal), linewidth = 1, colour = "red") +
        labs(title = paste0("FDP ajustada, ", XSi, ", i=", Decl_longi, ", Z=", Decl_transi),
             x = "Profundidade Máxima (m)", y = "FDP Lognormal") +
        theme_minimal() +
        theme(legend.position = "none")
      
      ggsave(filename = paste0("D:/prof_", geom, "_", XSi, "_FDP_lnorm.jpg"),
             plot = p2,
             device = "jpg",
             scale = 1,
             units = "px",
             width = 650,
             height = 400,
             dpi = 120)
    }
  }
}

# ---------------- AJUSTE FAP VELOCIDADE ---------------
cXS <- c('XS2', 'XS1')
cDecl_long <- c(0.001, 0.005, 0.01)
cDecl_trans <- c(0.02, 0.05, 0.1, 0.2, 0.3)


param_norm <- data.frame(XS=character(),
                         Decl_long=numeric(),
                         Decl_trans=numeric(),
                         mean=numeric(),
                         sd=numeric(),
                         SSE=numeric())

param_lnorm <- data.frame(XS=character(),
                          Decl_long=numeric(),
                          Decl_trans=numeric(),
                          meanlog=numeric(),
                          sdlog=numeric(),
                          SSE=numeric())

param_logis <- data.frame(XS=character(),
                          Decl_long=numeric(),
                          Decl_trans=numeric(),
                          location=numeric(),
                          scale=numeric(),
                          SSE=numeric())

rotlong <- data.frame(
  long = cDecl_long,
  i = c('i001', 'i005', 'i01'))

rottrans <- data.frame(
  trans = cDecl_trans,
  Z = c('Z02', 'Z05', 'Z1', 'Z2', 'Z3'))

for (XSi in cXS) {
  for (Decl_longi in cDecl_long) {
    for (Decl_transi in cDecl_trans) {
      geom <- paste0(rotlong$i[which(rotlong$long == Decl_longi)],
                     rottrans$Z[which(rottrans$trans == Decl_transi)])
      
      data_filtro <- dat_rejeito %>% filter(XS == XSi,
                                            Decl_long == Decl_longi,
                                            Decl_trans == Decl_transi)
      
      df <- select(data_filtro, Vel_max, Prob_exc)
      
      # Function to calculate the sum of squared errors (SSE) between empirical and theoretical CDFs
      sse_normal <- function(params) {
        mu <- params[1]
        sigma <- params[2]
        predicted <- pnorm(df$Vel_max, mean = mu, sd = sigma)
        sum((df$Prob_exc - predicted)^2)
      }
      
      sse_lognormal <- function(params) {
        meanlog <- params[1]
        sdlog <- params[2]
        predicted <- plnorm(df$Vel_max, meanlog = meanlog, sdlog = sdlog)
        sum((df$Prob_exc - predicted)^2)
      }
      
      sse_logistic <- function(params) {
        location <- params[1]
        scale <- params[2]
        predicted <- plogis(df$Vel_max, location = location, scale = scale)
        sum((df$Prob_exc - predicted)^2)
      }
      
      # Optimization to find parameters that minimize the SSE
      optim_normal <- optim(par = c(mu = mean(df$Vel_max), sigma = 1), fn = sse_normal)
      optim_lognormal <- optim(par = c(meanlog = 0, sdlog = 1), fn = sse_lognormal)
      optim_logistic <- optim(par = c(location = mean(df$Vel_max), scale = 1), fn = sse_logistic)
      
      # Results
      param_norm <- param_norm %>% add_row(XS = XSi,
                                           Decl_long = Decl_longi,
                                           Decl_trans = Decl_transi,
                                           mean = optim_normal$par[1],
                                           sd = optim_normal$par[2],
                                           SSE = optim_normal$value)
      
      param_lnorm <- param_lnorm %>% add_row(XS = XSi,
                                             Decl_long = Decl_longi,
                                             Decl_trans = Decl_transi,
                                             meanlog = optim_lognormal$par[1],
                                             sdlog = optim_lognormal$par[2],
                                             SSE = optim_lognormal$value)
      
      param_logis <- param_logis %>% add_row(XS = XSi,
                                             Decl_long = Decl_longi,
                                             Decl_trans = Decl_transi,
                                             location = optim_logistic$par[1],
                                             scale = optim_logistic$par[2],
                                             SSE = optim_logistic$value)
      
      
      # ---------------- Geração de gráfico FAP ---------------
      # Generate a sequence of x-values covering the range of your data for plotting the CDFs
      x_range <- seq(0.7*min(df$Vel_max), 1.3*max(df$Vel_max), length.out = 100)
      
      # Calculate the theoretical CDFs using the optimized parameters
      cdf_normal <- pnorm(x_range, mean = optim_normal$par[1], sd = optim_normal$par[2])
      cdf_lognormal <- plnorm(x_range, meanlog = optim_lognormal$par[1], sdlog = optim_lognormal$par[2])
      cdf_logistic <- plogis(x_range, location = optim_logistic$par[1], scale = optim_logistic$par[2])
      
      # Prepare the data for plotting
      plot_data <- data.frame(Vel_max = x_range,
                              Normal = cdf_normal,
                              Lognormal = cdf_lognormal,
                              Logistic = cdf_logistic)
      
      # Plot
      p1 <- ggplot(df, aes(x = Vel_max, y = Prob_exc)) +
        geom_line(data = plot_data, aes(x = Vel_max, y = Normal, colour = "Normal"),
                  linewidth  = 0.7) +
        geom_line(data = plot_data, aes(x = Vel_max, y = Lognormal, colour = "Lognormal"),
                  linewidth  = 0.7) +
        geom_line(data = plot_data, aes(x = Vel_max, y = Logistic, colour = "Logística"),
                  linewidth  = 0.7) +
        geom_point(aes(y = Prob_exc), colour = "black", size = 1.5) +
        ggtitle(paste0("Dados vs. FAP ajustada, ", XSi, ", i=", Decl_longi, ", Z=", Decl_transi)) +
        labs(x = "Velocidade Máxima (m)", y = "FAP") +
        scale_colour_manual(values = c("Normal" = "blue", "Lognormal" = "green", "Logística" = "red")) +
        theme_minimal()
      
      ggsave(filename = paste0("D:/vel_", geom, "_", XSi, "_FAP.jpg"),
             plot = p1,
             device = "jpg",
             scale = 1,
             units = "px",
             width = 650,
             height = 400,
             dpi = 120)
      
      # ---------------- Geração de gráfico FDP ---------------
      # Generate a sequence of x-values covering the range of your data for plotting the CDFs
      x_range <- seq(0.7*min(df$Vel_max), 1.3*max(df$Vel_max), length.out = 100)
      
      # Calculate the theoretical CDFs using the optimized parameters
      pdf_normal <- dnorm(x_range, mean = optim_normal$par[1], sd = optim_normal$par[2])
      pdf_lognormal <- dlnorm(x_range, meanlog = optim_lognormal$par[1], sdlog = optim_lognormal$par[2])
      pdf_logistic <- dlogis(x_range, location = optim_logistic$par[1], scale = optim_logistic$par[2])
      
      # Prepare the data for plotting
      plot_data_pdf <- data.frame(Vel_max = x_range,
                                  Normal = pdf_normal,
                                  Lognormal = pdf_lognormal,
                                  Logistic = pdf_logistic)
      
      # Plot
      p2 <- ggplot(data = plot_data_pdf) + 
        geom_line(aes(x = Vel_max, y = Normal, colour = "Normal"), linewidth = 1) +
        geom_line(aes(x = Vel_max, y = Lognormal, colour = "Lognormal"), linewidth = 1) +
        geom_line(aes(x = Vel_max, y = Logistic, colour = "Logistic"), linewidth = 1) +
        labs(title = paste0("FDP ajustada, ", XSi, ", i=", Decl_longi, ", Z=", Decl_transi),
             x = "Velocidade Máxima (m)", y = "FDP") +
        scale_colour_manual(values = c("Normal" = "blue", "Lognormal" = "green", "Logística" = "red")) +
        theme_minimal()
      
      ggsave(filename = paste0("D:/vel_", geom, "_", XSi, "_FDP.jpg"),
             plot = p2,
             device = "jpg",
             scale = 1,
             units = "px",
             width = 650,
             height = 400,
             dpi = 120)
    }
  }
}

# ---------------- AJUSTE FAP VELOCIDADE - SOMENTE LOGNORMAL ---------------
cXS <- c('XS2', 'XS1')
cDecl_long <- c(0.001, 0.005, 0.01)
cDecl_trans <- c(0.02, 0.05, 0.1, 0.2, 0.3)

param_lnorm <- data.frame(XS=character(),
                          Decl_long=numeric(),
                          Decl_trans=numeric(),
                          meanlog=numeric(),
                          sdlog=numeric(),
                          SSE=numeric())

rotlong <- data.frame(
  long = cDecl_long,
  i = c('i001', 'i005', 'i01'))

rottrans <- data.frame(
  trans = cDecl_trans,
  Z = c('Z02', 'Z05', 'Z1', 'Z2', 'Z3'))

for (XSi in cXS) {
  for (Decl_longi in cDecl_long) {
    for (Decl_transi in cDecl_trans) {
      geom <- paste0(rotlong$i[which(rotlong$long == Decl_longi)],
                     rottrans$Z[which(rottrans$trans == Decl_transi)])
      
      data_filtro <- dat_rejeito %>% filter(XS == XSi,
                                            Decl_long == Decl_longi,
                                            Decl_trans == Decl_transi)
      
      df <- select(data_filtro, Vel_max, Prob_exc)
      
      # Function to calculate the sum of squared errors (SSE) between empirical and theoretical CDFs
      sse_lognormal <- function(params) {
        meanlog <- params[1]
        sdlog <- params[2]
        predicted <- plnorm(df$Vel_max, meanlog = meanlog, sdlog = sdlog)
        sum((df$Prob_exc - predicted)^2)
      }
      
      # Optimization to find parameters that minimize the SSE
      optim_lognormal <- optim(par = c(meanlog = 0, sdlog = 1), fn = sse_lognormal)
      
      # Results
      param_lnorm <- param_lnorm %>% add_row(XS = XSi,
                                             Decl_long = Decl_longi,
                                             Decl_trans = Decl_transi,
                                             meanlog = optim_lognormal$par[1],
                                             sdlog = optim_lognormal$par[2],
                                             SSE = optim_lognormal$value)
      
      
      # ---------------- Geração de gráfico FAP ---------------
      # Generate a sequence of x-values covering the range of your data for plotting the CDFs
      x_range <- seq(0.7*min(df$Vel_max), 1.3*max(df$Vel_max), length.out = 100)
      
      # Calculate the theoretical CDFs using the optimized parameters
      cdf_lognormal <- plnorm(x_range, meanlog = optim_lognormal$par[1], sdlog = optim_lognormal$par[2])
      
      # Prepare the data for plotting
      plot_data <- data.frame(Vel_max = x_range,
                              Lognormal = cdf_lognormal)
      
      # Plot
      p1 <- ggplot(df, aes(x = Vel_max, y = Prob_exc)) +
        geom_point(colour = "black", size = 1.5) +
        geom_line(data = plot_data, aes(x = Vel_max, y = Lognormal), linewidth = 0.7, colour = "red") +
        ggtitle(paste0("Dados vs. FAP ajustada, ", XSi, ", i=", Decl_longi, ", Z=", Decl_transi)) +
        labs(x = "Velocidade Máxima (m)", y = "Probabilidade Acumulada") +
        theme_minimal()
        theme(legend.position = "none")
      
      ggsave(filename = paste0("D:/vel_", geom, "_", XSi, "_FAP.jpg"),
             plot = p1,
             device = "jpg",
             scale = 1,
             units = "px",
             width = 650,
             height = 400,
             dpi = 120)
      
      # ---------------- Geração de gráfico FDP ---------------
      # Generate a sequence of x-values covering the range of your data for plotting the CDFs
      x_range <- seq(0.7*min(df$Vel_max), 1.3*max(df$Vel_max), length.out = 100)
      
      # Calculate the theoretical CDFs using the optimized parameters
      pdf_lognormal <- dlnorm(x_range, meanlog = optim_lognormal$par[1], sdlog = optim_lognormal$par[2])
      
      # Prepare the data for plotting
      plot_data_pdf <- data.frame(Vel_max = x_range,
                                  Lognormal = pdf_lognormal)
      
      # Plot
      p2 <- ggplot(data = plot_data_pdf) +
        geom_line(aes(x = Vel_max, y = Lognormal), linewidth = 1, colour = "red") +
        labs(title = paste0("FDP ajustada, ", XSi, ", i=", Decl_longi, ", Z=", Decl_transi),
             x = "Velocidade Máxima (m)", y = "FDP Lognormal") +
        theme_minimal() +
        theme(legend.position = "none")
      
      ggsave(filename = paste0("D:/vel_", geom, "_", XSi, "_FDP.jpg"),
             plot = p2,
             device = "jpg",
             scale = 1,
             units = "px",
             width = 650,
             height = 400,
             dpi = 120)
    }
  }
}
