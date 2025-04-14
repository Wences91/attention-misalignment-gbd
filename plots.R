library(reshape2)
library(ggtern)
library(dplyr)
library(scales)
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(GGally)

df <- read.delim('data/mental_disorders_metrics.tsv')
df <- df[which(!df$Cause.ID %in% c(558, 567, 572)),]

df_des <- df[,c('Cause.Name', 'works', 'page_views', 'total')]

df_norm <- as.data.frame(lapply(df_des[,-1], function(x) (x - min(x)) / (max(x)-min(x))))
df_norm$Cause.Name <- df_des$Cause.Name

df_melt_val <- melt(df_des, id.vars = 'Cause.Name')
df_melt_pct <- melt(df_norm, id.vars = 'Cause.Name')

df_melt_val$pct <- df_melt_pct$value

df_melt_val$variable <- as.character(df_melt_val$variable)

df_melt_val$variable[which(df_melt_val$variable=='works')] <- 'Publications'
df_melt_val$variable[which(df_melt_val$variable=='page_views')] <- 'Wikipedia\npageviews'
df_melt_val$variable[which(df_melt_val$variable=='total')] <- 'Incidence'

df_melt_val$Cause.Name <- factor(df_melt_val$Cause.Name, levels = sort(unique(df_melt_val$Cause.Name), decreasing = TRUE))

ggplot(df_melt_val, aes(x = variable, y = Cause.Name, fill=pct)) +
  geom_tile(color = 'white', linewidth = 1) +
  geom_text(aes(label = scales::comma(value)), color = ifelse(df_melt_val$pct>0.7, 'white', 'black'), size = 3) +
  scale_fill_gradient(low = 'white', high = 'red', name = '% relativo') +
  theme_minimal() +
  labs(x = 'Variable', y = 'Cause') +
  theme(panel.grid = element_blank(),
        axis.text = element_text(color = 'black'),
        axis.title = element_text(face = 'bold'),
        axis.text.x = element_text(face = 'bold'),
        axis.text.y = element_text(face = 'italic'),
        legend.position = 'none')

df_corr <- df %>% 
  select(works, page_views, total)

names(df_corr) <- c('Publications', 'Wikipedia\npageviews', 'Incidence')

p <- ggpairs(
  df_corr,
  upper = list(continuous = wrap('cor', size = 5, color = 'black')),
  lower = list(continuous = wrap('smooth', color = '#FF5733', alpha = 0.6, size = 0.8, fill = '#FF573330')),
  diag  = list(continuous = wrap('densityDiag', color = '#FF5733', fill = '#FF573330'))
)

p <- p + 
  theme_light(base_size = 12) +
  theme(
    strip.background = element_rect(fill = 'black', color = NA),
    strip.text = element_text(color = 'white', size=15, face = 'bold'),
    axis.text = element_text(color = 'black', size = 12),
    axis.title = element_text(color = 'white'),
    panel.border = element_rect(color = 'black'),
    legend.position = 'none'
  )

no_k <- c('Publications')

formato_k <- label_number(scale = 1e-3, suffix = 'K', big.mark = ',')
formato_k2 <- label_number(big.mark = ',')
formato_m <- label_number(scale = 1e-6, suffix = 'M', big.mark = ',')

for (i in 1:p$nrow) {
  for (j in 1:p$ncol) {
    x_var <- colnames(df_corr)[j]
    y_var <- colnames(df_corr)[i]
    
    this_plot <- p[i, j]
    
    x_scale <- switch(x_var,
                      'Publications' = scale_x_continuous(labels = formato_k2),
                      'Wikipedia\npageviews' = scale_x_continuous(labels = formato_m),
                      'Incidence' = scale_x_continuous(labels = formato_m)
    )
    y_scale <- switch(y_var,
                      'Publications' = scale_y_continuous(labels = formato_k2),
                      'Wikipedia\npageviews' = scale_y_continuous(labels = formato_m),
                      'Incidence' = scale_y_continuous(labels = formato_m)
    )
    
    p[i, j] <- this_plot + x_scale + y_scale
  }
}

p

df <- read.delim('data/mental_disorders_metrics.tsv')
df <- df[which(!df$Cause.ID %in% c(558, 567, 572)),]
names(df)[26] <- 'val'

normalize <- function(x) (x - min(x)) / (max(x) - min(x))

df_norm <- df %>%
  mutate(
    works_norm = normalize(works),
    page_views_norm = normalize(page_views),
    val_norm = normalize(val)
  )

df_norm <- df_norm %>%
  rowwise() %>%
  mutate(
    total = works_norm + page_views_norm + val_norm,
    w = works_norm / total,
    p = page_views_norm / total,
    v = val_norm / total
  ) %>%
  ungroup()

df_norm <- df_norm %>%
  rowwise() %>%
  mutate(
    zone = case_when(
      w >= 0.2 & w <= 0.4 &
        p >= 0.2 & p <= 0.4 &
        v >= 0.2 & v <= 0.4 ~ 'Center',
      
      w >= 0.65 & p < 0.65 & v < 0.65 ~ 'Epidemiological dominance',
      p >= 0.65 & w < 0.65 & v < 0.65 ~ 'Academic dominance',
      v >= 0.65 & w < 0.65 & p < 0.65 ~ 'Social dominance',
      
      w + p >= 0.80 & w < 0.7 & p < 0.7 & v < 0.5 ~ 'Epidemiological–Academic',
      p + v >= 0.80 & p < 0.7 & v < 0.7 & w < 0.5 ~ 'Academic–Social',
      w + v >= 0.80 & w < 0.7 & v < 0.7 & p < 0.5 ~ 'Epidemiological–Social',
      
      TRUE ~ 'Mixed zone'
    )
  ) %>%
  ungroup()

colors <- c(
  'Epidemiological dominance' = '#d62728',
  'Academic dominance' = '#ff7f0e',
  'Social dominance' = '#2ca02c',
  'Epidemiological–Academic' = '#9467bd',
  'Academic–Social' = '#1f77b4',
  'Epidemiological–Social' = '#8c564b',
  'Center' = '#e377c2',
  'Mixed zone' = '#7f7f7f'
)

df_norm$vjust <- -1
df_norm$vjust[which(df_norm$Cause.ID %in% c(570, 578))] <- 1

ggtern(data = df_norm, aes(x = p, y = v, z = w, label = Cause.Name, color=zone)) +
  geom_point(size = 4) +
  geom_text(aes(vjust = vjust), size = 3.5, show.legend = FALSE) +
  scale_color_manual(values = colors) +
  labs(
    T = 'Incidence\n(Epidemiological relevance)',
    L = 'Wikipedia pageviews\n(Social relevance)',
    R = 'Publications\n(Academic relevance)',
    color = ''
  ) +
  theme_bw()+
  theme_showtitles()+
  theme_showarrows()+
  theme_nomask()+
  theme(legend.position = 'bottom')
