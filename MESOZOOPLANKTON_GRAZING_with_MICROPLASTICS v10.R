
# MESOZOOPLANKTON GRAZING with MICROPLASTICS 
# version V10

# R script for the paper: "EFFECTS OF MICROPLASTICS AND MESOZOOPLANKTON GRAZING ON MICRO- AND NANOPLANKTON SIZE SPECTRA IN TROPICAL COASTAL WATERS"

# Authors:  Taynara Bezerra dos Santos, Marius Nils Muller, 
#     Cynthia Dayanne Mello de Lima, Morgana Brito Lolaia, 
#     Wesley de Oliveira Neves, Ralf Schwamborn

# June 21, 2026


# 0. Preparations --------------

# clean memory (caution)

# rm(list = ls())
# gc()

# Load Packages -----------

library(tidyverse)
# library(janitor)
# library(mclust)
# library(coin)
# library(rstatix)
# library(e1071) 
library(vioplot)

# Save original graphic parameter settings --------

# dev.off()  # if a device is open
# dev.new()  # opens a new device (platform dependent)

par(no.readonly = TRUE)
 old_par <- par(no.readonly = TRUE)


# Change plotting settings
par(mar = c(2, 2, 1, 1))
plot(1:10)

# Restore original settings ----------------
par(old_par)


# 1. Import complete FLOWCAM dataset from github --------------

# setwd("~/Alunos Recife/Taynara Mestrado")

# dados_completos <- readRDS("dados_completos.rds")

# dados_completos <- readRDS("complete_FLOWCAM_data.rds")

url <- "https://raw.githubusercontent.com/rschwamborn/grazing_microplastics/main/complete_FLOWCAM_data.rds"

tmp <- tempfile(fileext = ".rds")
download.file(url, tmp, mode = "wb")

dados_completos <- readRDS(tmp)

# View (dados_completos)

dados_ESD <- dados_completos[, c("id_global", "tratamento", "replica", "diameter_esd", "volume_esd")]

dados_ESD


# primeiras plotagens e checagens ...  o numero de copep. influeciou na densidade de particlas?

# primeira checagem: zoo vs particulas (mais zoo devera dar menso particls, no final)

# Quando tem mais zoo, tem MAIS particulas no final! Deveria ser ao contrário!!!


tabela_z <- data.frame(
  amostra = c("T0A","T0B","T0C","T0D", 
              "ControleA","ControleB","ControleC","ControleD", 
              "ZooA","ZooB","ZooC","ZooD",
              "MPA","MPB","MPC","MPD"),
  Z_individuos = c(13, 27, 37, 22,
                   0, 0, 0, 0,
                   6, 21, 13, 17,
                   3, 17, 12, 4),
  tratamento = as.factor( c (  "T0","T0","T0","T0", 
                               "Controle","Controle","Controle","Controle", 
                               "Zoo","Zoo","Zoo","Zoo", 
                               "MP","MP","MP","MP" ) )  )

dados_brutos_flowcam <- data.frame(
  amostra_id = c("ControleA", "ControleB", "ControleC", "ControleD",
                 "MPA", "MPB", "MPC", "MPD", 
                 "T0A", "T0B", "T0C", "T0D",
                 "ZooA", "ZooB", "ZooC", "ZooD"),
  count = c(2067, 2528, 2938, 2531,
            491, 1247, 422, 560,
            800, 1061, 877, 851,
            548, 1504, 869, 1488),
  part_ml = c(17119, 27443, 32501, 29186,
              5512, 14425, 4684, 6262,
              9238,  12155, 10202, 9892,
              6054, 16354, 9146, 16499),
   tratamento = as.factor( c ( "Control","Control","Control","Control", 
                               "MP","MP","MP","MP",
                               "T0","T0","T0","T0", 
                               "Zoo","Zoo","Zoo","Zoo"  ) )  )



# View (dados_brutos_flowcam)
 
# checagem simples ------  
aggregate(
  part_ml ~ tratamento,
  data = dados_brutos_flowcam,
  FUN = median
)


# checagem simples ------  
aggregate(
  part_ml ~ tratamento,
  data = dados_brutos_flowcam,
  FUN = mean
)


medianZ_MP = median (3, 17, 12, 4)# N = 3 : MEDIAN cop number per treatment MP
medianZ_zoo = median (6, 21, 13, 17)# N  = 6:  MEDIAN cop number per treatment Zoo



# Vioplot of the results (total particle abundance, L-1) obtained from in situ grazing experiments in the Tamandaré coastal region,  Brazil. 


# 2. VIOPLOTS and points and 95% CI plot (fig. 2) -----------

# totais por grupo 

# Restore original plotting settings
par(old_par)

library(vioplot)

vioplot(
  dados_brutos_flowcam$part_ml ~ dados_brutos_flowcam$tratamento,
  col = "lightblue",
  main = "Total particle abundance (ml-1)\nTamandaré coastal region, Brazil",
  xlab = "Tratamento",
  ylab = "Partículas totais (ml-1)"
)


####### Fig 2 -----------
# points and mean + 95% CI -------------

# Restore original settings
par(old_par)


# Data
y <- dados_brutos_flowcam$part_ml
grp <- dados_brutos_flowcam$tratamento

# Convert group to numeric positions
g <- as.numeric(grp)

# Compute summary stats
means <- tapply(y, grp, mean)
sds   <- tapply(y, grp, sd)
ns    <- tapply(y, grp, length)
se    <- sds / sqrt(ns)

# 95% CI using t distribution
tval <- qt(0.975, df = ns - 1)
ci_upper <- means + tval * se
ci_lower <- means - tval * se

# Base plot: raw data points
plot( ylim = c(0, 40000), 
  jitter(g, amount = 0.1),
  y,
  pch = 16,
  xaxt = "n",
  xlab = "Treatment",
  ylab = "Total Particles (ml-1)"
  
)

axis(1, at = seq_along(levels(grp)), labels = levels(grp))

col_ci <- adjustcolor("steelblue", alpha.f = 0.6)

# Add mean (red dot)
points(seq_along(means), means, pch = 16, col = col_ci, cex = 1.5)

# Add 95% CI (vertical lines)
segments(seq_along(means), ci_lower,
         seq_along(means), ci_upper,
         col = col_ci, lwd = 2)

# Add small horizontal caps for CI
segments(seq_along(means) - 0.1, ci_lower,
         seq_along(means) + 0.1, ci_lower,
         col = col_ci, lwd = 2)

segments(seq_along(means) - 0.1, ci_upper,
         seq_along(means) + 0.1, ci_upper,
         col = col_ci, lwd = 2)


# PAIRWISE testing - not ANOVA

#install.packages("coin")
library(coin)

y <- dados_brutos_flowcam$part_ml
grp <- dados_brutos_flowcam$tratamento

pair_perm_coin <- function(data, group, g1, g2) {
  
  sub <- data[group %in% c(g1, g2)]
  grp_sub <- factor(group[group %in% c(g1, g2)])
  
  test <- coin::independence_test(
    sub ~ grp_sub,
    distribution = approximate(B = 10000)  # permutation = 10000
  )
  
  pval <- coin::pvalue(test)
  
  return(data.frame(
    group1 = g1,
    group2 = g2,
    p_value = pval
  ))
}

groups <- levels(grp)

res <- do.call(rbind, lapply(1:(length(groups)-1), function(i) {
  do.call(rbind, lapply((i+1):length(groups), function(j) {
    pair_perm_coin(y, grp, groups[i], groups[j])
  }))
}))

res




####### Fig 3 -----------

# Vioplot e testes do tamnho ESD - mediana por tratamento



attach(dados_ESD)

dados_ESD$tratamento          <- as.factor(dados_ESD$tratamento )

summary(dados_ESD)

ggplot(dados_ESD, aes(x = tratamento,
                      y = diameter_esd,
                      fill = tratamento)) +
  geom_violin(trim = FALSE, alpha = 0.8, color = "black") +
  geom_boxplot(width = 0.12,
               fill = "white",
               outlier.shape = NA,
               alpha = 0.8) +
  scale_fill_manual(values = c(
    "Controle" = "#A8DADC",
    "MP"       = "#FFD6A5",
    "T0"       = "#CAFFBF",
    "Zoo"      = "#FFCAD4"
  )) +
  labs(
    x = "Treatment",
    y = "ESD Diameter (µm)",
    title = "Diameter ESD by Treatment"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )



#########################
# test for diffrences in median size 

dados_ESD$tratamento <-  as.factor(dados_ESD$tratamento)

par(old_par)

plot( dados_ESD$diameter_esd ~ dados_ESD$tratamento)

library(coin)
independence_test(dados_ESD$diameter_esd ~ dados_ESD$tratamento)#  p-value < 2.2e-16

# post-hoc test
library(PMCMRplus)

PMCMRplus::kwAllPairsNemenyiTest(dados_ESD$diameter_esd ~ dados_ESD$tratamento)
# 
#      Controle   MP      T0     
#   MP  < 2e-16  -       -      
#   T0  < 2e-16  0.97    -      
#   Zoo 3.9e-14  1.1e-05 1.9e-05

# All are signiffcanty diffent amoing each other, except MP and T0



# 3. Histogramas  (figs 4 e 5 ) ----------------

# Histograma geral (todos dados) ----------------



attach(dados_ESD)

dados_ESD$tratamento          <- as.factor(dados_ESD$tratamento )

summary(dados_ESD)

hist(diameter_esd )

breaks_used <- 1:80 # definiiuçãoo das classes (limites das classes de tamanho, de 1 em 1 micrometro)
breaks_used

h1 <- hist(diameter_esd , breaks = breaks_used)

class(h1)

plot(h1$counts ~ h1$mids)

plot(log10(h1$counts) ~ log10(h1$mids)) # não é Pareto!

# Subsets 

dados_controle <- subset(dados_ESD, tratamento == "Controle")
dados_MP        <- subset(dados_ESD, tratamento == "MP")
dados_T0        <- subset(dados_ESD, tratamento == "T0")
dados_Zoo       <- subset(dados_ESD, tratamento == "Zoo")



# Store subsets in a list
subsets <- list(
  Controle = dados_controle,
  MP = dados_MP,
  T0 = dados_T0,
  Zoo = dados_Zoo
)





# Histogramas por tratamento (Figuras 4 e 5 ) ----------------


# -----------------------------
# Histograms and log10-log10 plots
# -----------------------------

par(mfrow = c(2,2))

for(nome in names(subsets)) {
  
  dados <- subsets[[nome]]
  
  hist(dados$diameter_esd,
       breaks = 1:80,
       main = paste("Histogram -", nome),
       xlab = "Diameter ESD",
       col = "lightgray")
}

# Counts vs mids
par(mfrow = c(2,2))

for(nome in names(subsets)) {
  
  dados <- subsets[[nome]]
  
  h <- hist(dados$diameter_esd,
            breaks = 1:80,
            plot = FALSE)
  
  plot(h$counts ~ h$mids,
       pch = 16,
       main = paste("Counts vs Diameter -", nome),
       xlab = "Midpoints",
       ylab = "Counts")
}

# log10-log10 plots
par(mfrow = c(2,2))

for(nome in names(subsets)) {
  
  dados <- subsets[[nome]]
  
  h <- hist(dados$diameter_esd,
            breaks = 1:80,
            plot = FALSE)
  
  # remove zeros to avoid log10(0)
  valid <- h$counts > 0
  
  plot(log10(h$counts[valid]) ~ log10(h$mids[valid]),
       pch = 16, ylim = c(0,3.4),xlim = c(0.4, 1.8),
       main = paste("log10-log10 Plot -", nome),
       xlab = "log10(midpoints)",
       ylab = "log10(counts)")
}

# inserir linhas de regressoa linear na figura 4 -------------


# Figura 4 - log10-log10 com regressão usando apenas ~50% dos dados centrais

par(mfrow = c(2,2))

for(nome in names(subsets)) {
  
  dados <- subsets[[nome]]
  
  h <- hist(dados$diameter_esd,
            breaks = 1:80,
            plot = FALSE)
  
  # remover zeros
  valid <- h$counts > 0
  
  x <- log10(h$mids[valid])
  y <- log10(h$counts[valid])
  
  # selecionar aproximadamente 50% central dos dados
  # lim.inf <- quantile(x, 0.25)
  # lim.sup <- quantile(x, 0.75)
 
   # selecionar microplancton (20 a 40 microm.)
  
  lim.inf1 <- log10(20)
   lim.sup1 <- log10(40)
  
   # selecionar nanoplancton (20 a 40 microm.)
   
   lim.inf2 <- log10(4)
   lim.sup2 <- log10(19.99)
   
   
  central1 <- x >= lim.inf1 & x <= lim.sup1
  central2 <- x >= lim.inf2 & x <= lim.sup2
  
  
  # ajuste linear 
  mod1 <- lm(y[central1] ~ x[central1])
  
  mod2 <- lm(y[central2] ~ x[central2])
  
  
  
  
  # gráfico completo
  plot(x, y,
       pch = 16,
       ylim = c(0,3.4), xlim = c(0.4, 1.8),
       main = paste("log10-log10 Plot -", nome),
       xlab = expression(log[10](Diameter)),
       ylab = expression(log[10](Counts)))
  
  # destacar pontos usados na regressão
  points(x[central1], y[central1],
         pch = 16,
         col = "steelblue")
  
  points(x[central2], y[central2],
         pch = 16,
         col = "olivedrab")
  
  
  # reta de regressão
  abline(mod1,
         col = "blue",
         lwd = 3)

  # reta de regressão
  abline(mod2,
         col = "darkgreen",
         lwd = 3)
  
    
  # R² e inclinação
  eq1 <- paste0("slope = ",
               round(coef(mod1)[2], 3),
               "\nR² = ",
               round(summary(mod1)$r.squared, 3))
 
  eq2 <- paste0("slope = ",
                round(coef(mod2)[2], 3),
                "\nR² = ",
                round(summary(mod2)$r.squared, 3))
  
   
  legend("topright",
         legend = eq1,
         bty = "n")
  
  legend("bottomleft",
         legend = eq2,
         bty = "n")
  
  
  
abline (  v=  log10(20), col = "grey") # insere linha vertical - separating nanoplankton vs microplankton (20 micrometros ESD)

  }


########## Frost equations, 15 size classes ########## 


# Equacoes de Frost por classes de tamanhos (nanoplâncton, 20 classes) -----------

# =====================================================
# FROST (1972) - CORRECT IMPLEMENTATION
# =====================================================

# =====================================================
# PARAMETERS
# =====================================================

tempo_h <- 5
vol_garrafa_ml <- 1500

# median copepod abundance
N_MP  <- 3
N_Zoo <- 6

# =====================================================
# SIZE CLASSES - bin width = 4 microns
# =====================================================

# breaks_used <- 1:80

breaks_used <- seq(0.0001 , 80, by = 4)
length(breaks_used)


# breaks_size <- breaks_used[2:17]
breaks_size <- breaks_used[1:20]

breaks_size
length(breaks_size)

max_size <- max(breaks_size)


breaks_size


midpoints <- (breaks_size[-1] + breaks_size[-length(breaks_size)]) / 2

midpoints


# =====================================================
# SUBSETS
# =====================================================

dados_controle <- subset(
  dados_ESD,
  tratamento == "Controle"
)

dados_T0 <- subset(
  dados_ESD,
  tratamento == "T0"
)

dados_Zoo <- subset(
  dados_ESD,
  tratamento == "Zoo"
)

dados_MP <- subset(
  dados_ESD,
  tratamento == "MP"
)

# =====================================================
# CROP DATA
# =====================================================

dados_controle <- subset(
  dados_controle,
  diameter_esd < max_size
)

dados_T0 <- subset(
  dados_T0,
  diameter_esd < max_size
)

dados_Zoo <- subset(
  dados_Zoo,
  diameter_esd < max_size
)

dados_MP <- subset(
  dados_MP,
  diameter_esd < max_size
)

# =====================================================
# HISTOGRAMS
# =====================================================

h_controle <- hist(
  dados_controle$diameter_esd,
  breaks = breaks_size,
  plot = FALSE
)

h_T0 <- hist(
  dados_T0$diameter_esd,
  breaks = breaks_size,
  plot = FALSE
)

h_Zoo <- hist(
  dados_Zoo$diameter_esd,
  breaks = breaks_size,
  plot = FALSE
)

h_MP <- hist(
  dados_MP$diameter_esd,
  breaks = breaks_size,
  plot = FALSE
)

# =====================================================
# DATAFRAME
# =====================================================

frost_df <- data.frame(
  size_class = h_controle$mids,
  
  CT0 = h_T0$counts,
  CC  = h_controle$counts,
  CZ  = h_Zoo$counts,
  CMP = h_MP$counts
)

# =====================================================
# REMOVE ZEROS
# =====================================================

frost_df$CT0[frost_df$CT0 <= 0] <- NA
frost_df$CC[frost_df$CC <= 0] <- NA
frost_df$CZ[frost_df$CZ <= 0] <- NA
frost_df$CMP[frost_df$CMP <= 0] <- NA

# =====================================================
# 1. PHYTOPLANKTON GROWTH RATE (k)
#
# k = (ln(CC) - ln(CT0)) / t
# =====================================================

frost_df$k <-
  (log10(frost_df$CC) - log10(frost_df$CT0)) /
  tempo_h

# =====================================================
# 2. APPARENT GROWTH RATE (d)
#
# d = (ln(CZ) - ln(CT0)) / t
# =====================================================

frost_df$d_Zoo <-
  (log10(frost_df$CZ) - log10(frost_df$CT0)) /
  tempo_h

frost_df$d_MP <-
  (log10(frost_df$CMP) - log10(frost_df$CT0)) /
  tempo_h

# =====================================================
# 3. GRAZING COEFFICIENT
#
# g = k - d
# =====================================================

frost_df$g_Zoo <-
  frost_df$k - frost_df$d_Zoo

frost_df$g_MP <-
  frost_df$k - frost_df$d_MP

# prevent negative grazing
frost_df$g_Zoo <-
  ifelse(frost_df$g_Zoo < 0, 0, frost_df$g_Zoo)

frost_df$g_MP <-
  ifelse(frost_df$g_MP < 0, 0, frost_df$g_MP)

# =====================================================
# 4. MEAN FOOD CONCENTRATION
#
# Cmean = CT0 * ((exp(d*t)-1)/(d*t))
# =====================================================

frost_df$Cmean_Zoo <-
  ifelse(
    abs(frost_df$d_Zoo) < 1e-10,
    frost_df$CT0,
    frost_df$CT0 *
      ((exp(frost_df$d_Zoo * tempo_h) - 1) /
         (frost_df$d_Zoo * tempo_h))
  )

frost_df$Cmean_MP <-
  ifelse(
    abs(frost_df$d_MP) < 1e-10,
    frost_df$CT0,
    frost_df$CT0 *
      ((exp(frost_df$d_MP * tempo_h) - 1) /
         (frost_df$d_MP * tempo_h))
  )

# =====================================================
# 5. FILTRATION / CLEARANCE RATE
#
# F = (g * V) / Z
# =====================================================

frost_df$F_Zoo <-
  (frost_df$g_Zoo * vol_garrafa_ml) /
  N_Zoo

frost_df$F_MP <-
  (frost_df$g_MP * vol_garrafa_ml) /
  N_MP

# =====================================================
# 6. INGESTION RATE
#
# I = F * Cmean
# =====================================================

frost_df$I_Zoo <-
  frost_df$F_Zoo *
  frost_df$Cmean_Zoo

frost_df$I_MP <-
  frost_df$F_MP *
  frost_df$Cmean_MP

# =====================================================
# BARPLOTS
# =====================================================

# Divide a área gráfica em 2 linhas x 2 colunas
par(mfrow = c(2, 2))

# -----------------------------------------------------
# GRAZING COEFFICIENT
# -----------------------------------------------------

barplot(
  frost_df$g_Zoo, col = "lightblue",
  ylim = c(0, 0.18),
  names.arg = round(frost_df$size_class,1),
  las = 2,
  main = "Grazing Coefficient - Zoo",
  xlab = "Size class midpoint (µm)",
  ylab = "g (h-1)"
)

barplot(
  frost_df$g_MP, col = "lightblue",
  ylim = c(0, 0.18),
  names.arg = round(frost_df$size_class,1),
  las = 2,
  main = "Grazing Coefficient - MP",
  xlab = "Size class midpoint (µm)",
  ylab = "g (h-1)"
)

# -----
# FILTRATION RATE ---------------------
# -----

barplot(
  frost_df$F_Zoo,col = "lightblue",
  ylim = c(0, 80),
  names.arg = round(frost_df$size_class,1),
  las = 2,
  main = "Filtration Rate - Zoo",
  xlab = "Size class midpoint (µm)",
  ylab = "mL ind-1 h-1"
)

barplot(
  frost_df$F_MP,col = "lightblue",
  ylim = c(0, 80),
  names.arg = round(frost_df$size_class,1),
  las = 2,
  main = "Filtration Rate - MP",
  xlab = "Size class midpoint (µm)",
  ylab = "mL ind-1 h-1"
)

# ---
# INGESTION RATE ------------------
# ---

barplot(
  frost_df$I_Zoo,
  ylim = c(0, 90000),col = "lightblue",
  names.arg = round(frost_df$size_class,1),
  las = 2,
  main = "Ingestion Rate - Zoo",
  xlab = "Size class midpoint (µm)",
  ylab = "cells ind-1 h-1"
)

barplot(
  frost_df$I_MP,
  ylim = c(0, 90000),col = "lightblue",
  names.arg = round(frost_df$size_class,1),
  las = 2,
  main = "Ingestion Rate - MP",
  xlab = "Size class midpoint (µm)",
  ylab = "cells ind-1 h-1"
)

# ==
# BARPLOT CMean ----------------
# ===


barplot(
  frost_df$Cmean_Zoo,col = "lightblue",
  ylim = c(0, 1700),
  names.arg = round(frost_df$size_class,1),
  las = 2,
  main = "Mean Concentration - Zoo",
  xlab = "Size class midpoint (µm)",
  ylab = "cells ml-1"
)

barplot(
  frost_df$Cmean_MP,col = "lightblue",
  ylim = c(0, 1700),
  names.arg = round(frost_df$size_class,1),
  las = 2,
  main = "Mean Concentration - MP",
  xlab = "Size class midpoint (µm)",
  ylab = "cells ml-1"
)




# Volta ao padrão (opcional)
par(mfrow = c(1, 1))

# ==
# OUTPUT ----------------
# ===

frost_df

# ===
# EXPORT ----------
# ====

write.csv(
  frost_df,
  "frost_1972_correct_results_OK_b.csv",
  row.names = FALSE
)



#### SELECTIVITY -------------------------------

# define function "selectivity_manly_chesson()" ----------


selectivity_manly_chesson <- function(offer, ingestion, food_names = NULL) {
  
  # Check inputs
  if(length(offer) != length(ingestion)) {
    stop("offer and ingestion must have same length")
  }
  
  # Number of food types
  m <- length(offer)
  
  # Proportions
  p_offer <- offer / sum(offer)
  p_ing <- ingestion / sum(ingestion)
  
  # Step 1: Manly's alpha
  alpha <- (p_ing / p_offer) / sum(p_ing / p_offer)
  
  # Step 2: Chesson's electivity (e)
  e <- (m * alpha - 1) / ((m - 2) * alpha + 1)
  
  # Create result table
  result <- data.frame(
    Food = if(is.null(food_names)) paste0("Food_", 1:m) else food_names,
    Offer = offer,
    Ingestion = ingestion,
    Alpha = alpha,
    Electivity_e = e
  )
  
  return(result)
}

#



# ingestion vs Cmean vs selectivity



# Zoo -------

# mean  concentration (zoo) for each size class
frost_df$Cmean_Zoo

# [1]  524.587385 1510.440987  906.370694  419.792201  202.849793   87.629935
# [7]   54.454580   24.898515   15.909578    9.589756    2.822066    7.520424
# [13]          NA    1.371751          NA          NA          NA          NA
# [19]          NA

# ingestion rate  (zoo) for each size class
frost_df$I_Zoo

# [1] 11097.88104 29767.13298 15471.28891  6903.50121  2465.01090   771.54328
# [7]   229.58292   173.10318     0.00000    21.94016     0.00000     0.00000
# [13]          NA     0.00000          NA          NA          NA          NA
# [19]          NA

# calculate Manly (alpha) and chesson (epsilon) indices 
 #for each size class


# Available food (offer)
offer <- frost_df$Cmean_Zoo[1:12]

# Ingested food
ingestion <- frost_df$I_Zoo[1:12]

# Size class labels (optional)
food_names <- paste0("Size_", seq_along(offer))

# Remove NA size classes
keep <- !is.na(offer) & !is.na(ingestion)

offer2 <- offer[keep]
ingestion2 <- ingestion[keep]
food_names2 <- food_names[keep]

# Calculate selectivity
sel <- selectivity_manly_chesson(
  offer = offer2,
  ingestion = ingestion2,
  food_names = food_names2
)

sel



# barplots plot for zoo  ---------


ESDsize <- round( midpoints[1:12],0)
# ESDsize <- c(6,10,14,18,22,26,30,34,38,42,46,50)


# ESD midpoints
#ESD <- c(6,10,14,18,22,26,30,34,38,42,46,50)
ESD <-round( midpoints[1:12],0)
attach(sel)

# Four barplots in one window

# ESD midpoints
#ESD <- c(6,10,14,18,22,26,30,34,38,42,46,50)

Offer <- c(
  3.195463,521.368109,455.455396,389.672468,
  339.927066,323.786358,283.813806,246.297460,
  201.125231,174.999426,115.487999,117.673080
)

Ingestion <- c(
  81.78522,11017.45397,9656.15751,7550.23973,
  6525.79466,5799.50967,4922.35740,3968.08969,
  3487.38566,3086.92784,2174.68011,2099.60946
)

Alpha <- c(
  0.11151229,0.09207008,0.09237197,0.08441950,
  0.08364296,0.07803950,0.07556512,0.07019453,
  0.07554664,0.07685490,0.08204276,0.07773975
)

Electivity <- c(
  0.159871335,0.054584736,0.056382268,0.007067552,
  0.002023227,-0.035680865,-0.053096309,-0.092638477,
  -0.053228200,-0.043957586,-0.008507278,-0.037764773
)


par(mfrow=c(2,2),
    mar=c(4,4,2,1),
    oma=c(0,0,1,0))

barplot(
  Offer,
  names.arg=ESD,
  col="lightblue",
  main="Offer",
  xlab="ESD (µm)",
  ylab="Offer"
)

barplot(
  Ingestion,
  names.arg=ESD,
  col="lightblue",
  main="Ingestion",
  xlab="ESD (µm)",
  ylab="Ingestion"
)

barplot(
  Alpha,
  names.arg=ESD,
  col="lightblue",
  main="Manly alpha",
  xlab="ESD (µm)",
  ylab=expression(alpha)
)
abline(h=1/12, lty=2)

barplot(
  Electivity,
  names.arg=ESD,
  col="lightblue",
  main="Chesson epsilon",
  xlab="ESD (µm)",
  ylab=expression(epsilon)
)
abline(h=0, lty=2)



# MP --------------

# mean  concentration (zoo) for each size class
frost_df$Cmean_MP
# [1]  449.100961 1364.432493  851.685057  377.308550  185.952143   84.123541
# [7]   49.633124   23.202317   13.963490    8.400471    3.026257    7.236350
# [13]          NA    1.166827          NA          NA          NA          NA
# [19]          NA

# ingestion rate  (MP) for each size class
frost_df$I_MP
# [1] 32708.2421 81721.8351 39863.2316 20590.8153  7730.9523  2147.4427  1303.5326
# [8]   631.7022   348.9162   252.8794     0.0000     0.0000         NA     0.0000
# [15]         NA         NA         NA         NA         NA



# calculate Manly (alpha) and chesson (epsilon) indices 
#for each size class


# Available food (offer)
offer <- frost_df$Cmean_MP[1:12]

# Ingested food
ingestion <- frost_df$I_MP[1:12]

# Size class labels (optional)
food_names <- paste0("Size_", seq_along(offer))


# Calculate selectivity
sel_MP <- selectivity_manly_chesson(
  offer = offer,
  ingestion = ingestion,
  food_names = food_names
)

sel_MP

attach(sel_MP)
barplot(
  sel_MP$Offer,
  names.arg=ESD,
  col="lightblue",
  main="Offer",
  xlab="ESD (µm)",
  ylab="Offer"
)

barplot(
  sel_MP$Ingestion,
  names.arg=ESD,
  col="lightblue",
  main="Ingestion",
  xlab="ESD (µm)",
  ylab="Ingestion"
)

barplot(
  sel_MP$Alpha,
  names.arg=ESD,
  col="lightblue",
  main="Manly alpha",
  xlab="ESD (µm)",
  ylab=expression(alpha)
)
abline(h=1/12, lty=2)

barplot(
  sel_MP$Electivity,
  names.arg=ESD,
  col="lightblue",
  main="Chesson epsilon",
  xlab="ESD (µm)",
  ylab=expression(epsilon)
)
abline(h=0, lty=2)


# MP vs ZOO (  fig 7 ) --------------------------------


summary( frost_df)

# filtration ratios MP / Zoo, 
# ratio 2 , MP has twice filtraton rate

 ratio_filtr_MPvsZoo <-  frost_df$F_MP[1:8] / frost_df$F_Zoo [1:8]


# ingestion ratios MP / Zoo, 
# ratio 2 , MP has twice ingestion rate

  ratio_ingest_MPvsZoo <-  frost_df$I_MP [1:8] / frost_df$I_Zoo [1:8]

  
  
  par(mfrow=c(2,2),
      mar=c(4,4,2,1),
      oma=c(0,0,1,0))
  
  
  barplot(
    ratio_ingest_MPvsZoo,
    names.arg=ESD[1:8], ylim = c(0,6),
    col="lightblue",
    main="ratio ingestion MP/Zoo",
    xlab="ESD (µm)",
    ylab= "ratio"
  )
 
  
  barplot(
    ratio_filtr_MPvsZoo[1:8],
    names.arg=ESD[1:8],ylim = c(0,6),
    col="lightblue",
    main="ratio filtration rate, MP/Zoo",
    xlab="ESD (µm)",
    ylab= "ratio" )
 
  # differcne final conntration zoo vs MP
  
  # final concetrations (counts)
  C_Zoo <- frost_df$CZ[1:12]
  C_MP <- frost_df$CMP[1:12]
  C_control <- frost_df$CC[1:12]
  
   
  ratio_final_concMP_Zoo <- C_MP /  C_Zoo

  ratio_final_concZoo_MP <- C_Zoo /  C_MP
  
  diff_final_concZoo_MP <- C_Zoo -  C_MP

  diff_final_concZoo_Control <- C_Zoo -  C_control
  
  
  ratio_final_concControl_Zoo <-   C_control [1:12] / C_Zoo [1:12]
  
    
  barplot(
    ratio_final_concZoo_MP[1:8],
    names.arg=ESD[1:8],ylim = c(0,2.5),
    col="lightblue",
    main="ratio final concentration, Zoo/MP",
    xlab="ESD (µm)",
    ylab= "ratio" )
  
  
  
    