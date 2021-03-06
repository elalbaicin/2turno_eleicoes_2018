# Biblioteca(s)
library(tidyverse)

# Constru��o de tr�s cen�rios
resultados <- tibble(Cen�rio = map(.x = c("... votado nulo",
                                          "... votado em Haddad",
                                          "... votado em Bolsonaro"),
                                   .f = rep,
                                   times = 2) %>% 
                       unlist() %>% 
                       factor(levels = unique(.),
                              ordered = TRUE),
                     Candidato = rep(c("Bolsonaro",
                                       "Haddad"),
                                     times = 3),
                     Votos = c(57797847,
                               47040906,
                               57797847,
                               47040907,
                               57797848,
                               47040906)) %>% 
  group_by(Cen�rio) %>% 
  mutate(Percentual = round(100 * Votos / sum(Votos),
                            digits = 2) %>% 
           paste0("%"))

# Gr�fico com o resultado do segundo turno em cada cen�rio
(p <- ggplot(resultados,
             aes(x = Candidato,
                 y = Votos / 10 ^ 6,
                 fill = Candidato)) +
    geom_col(position = "dodge") +
    geom_text(aes(label = Percentual),
              nudge_y = 1.5,
              show.legend = FALSE) +
    facet_wrap(facets = vars(Cen�rio)) +
    scale_fill_manual(values = c("#34C771", 
                                 "#C5122D")) +
    ggtitle("Resultado do segundo turno de 2018 se eu tivesse...") +
    theme_bw() +
    ylab("Milh�es de votos") +
    xlab("") +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_text(size = 15),
          axis.text.y = element_text(size = 13),
          title = element_text(size = 20),
          strip.text = element_text(size = 12),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 15)) +
    labs(caption = "Fonte: TSE; c�lculos do autor."))
