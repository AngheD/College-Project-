library(tidyverse)
library(ggplot2)
library(showtext)

archivo <- 'EUT_2021_2022.csv'
datos <- read.csv(archivo)

font_add_google('Bebas Neue')
font_add_google('Roboto Condensed')
showtext_auto()

tema <- function(){
  theme_gray() +
    theme(
      plot.title = element_text(size = 30, family = 'Bebas Neue', hjust = 0.5),
      plot.subtitle = element_text(size = 18, family = 'Roboto Condensed', hjust = 0.5),
      plot.margin = margin(0.5, 1, 0.2, 0.2, 'cm'),
      panel.background = element_rect(color = '#000000', fill = '#ffffff', size = 0.05),
      axis.text = element_text(size = 14, color = '#000000'),
      panel.grid = element_line(color = '#A8A8A8'),
      legend.position = 'none',
      strip.text = element_text(size = 14, color = '#ffffff'),
      strip.background = element_rect(fill = '#000000', color = '#000000', size = 0.05)
    )
}

color_barra <- '#6D4CFF'
color_sexo <- c("#0099ff", "#F14CFF")

#Seleccionamos columnas del dataframe.
criterio <- !is.na(datos$AG_1)&datos$AG_1%in%c(1:6)&!is.na(datos$AG_2)&datos$AG_2%in%c(1:6)&!is.na(datos$AG_3)&datos$AG_3%in%c(1:6)&!is.na(datos$AG_4)&datos$AG_4%in%c(1:6)&!is.na(datos$AG_5)&datos$AG_5%in%c(1:6)&!is.na(datos$AG_6)&datos$AG_6%in%c(1:6)
misdatos <- subset(datos, criterio, select = c(X, p2, p6, edad_tramos, AG_1, AG_2, AG_3, AG_4, AG_5, AG_6))

#Mejoramos la descripción de los factores con las respuestas dadas.
misdatos$AG_1 <- factor(misdatos$AG_1, levels=c(6:1), labels=c('No contesta', 'No sabe', 'Muy en desacuerdo','En desacuerdo', 'De acuerdo', 'Muy de acuerdo'))
misdatos$AG_2 <- factor(misdatos$AG_2, levels=c(6:1), labels=c('No contesta', 'No sabe', 'Muy en desacuerdo','En desacuerdo', 'De acuerdo', 'Muy de acuerdo'))
misdatos$AG_3 <- factor(misdatos$AG_3, levels=c(6:1), labels=c('No contesta', 'No sabe', 'Muy en desacuerdo','En desacuerdo', 'De acuerdo', 'Muy de acuerdo'))
misdatos$AG_4 <- factor(misdatos$AG_4, levels=c(6:1), labels=c('No contesta', 'No sabe', 'Muy en desacuerdo','En desacuerdo', 'De acuerdo', 'Muy de acuerdo'))
misdatos$AG_5 <- factor(misdatos$AG_5, levels=c(6:1), labels=c('No contesta', 'No sabe', 'Muy en desacuerdo','En desacuerdo', 'De acuerdo', 'Muy de acuerdo'))
misdatos$AG_6 <- factor(misdatos$AG_6, levels=c(6:1), labels=c('No contesta', 'No sabe', 'Muy en desacuerdo','En desacuerdo', 'De acuerdo', 'Muy de acuerdo'))
misdatos$p2 <- factor(misdatos$p2, levels=c(1:2), labels=c('Varón', 'Mujer'))
misdatos$p6 <- factor(misdatos$p6, levels=c(1:7), labels=c('Sin instrucción', 'Primaria incompleta', 'Primaria completa', 'Secundaria incompleta', 'Secundaria completa', 'Terciaria incompleta', 'Terciaria completa'))

pregunta1 <- ggplot(misdatos, aes(y = AG_1)) + geom_bar(fill = color_barra) + 
  labs(title='Afirmación 1', subtitle = '"Cuando una madre tiene un trabajo remunerado de tiempo completo los hijos sufren"', x='', y='') +
  tema()

pregunta2 <- ggplot(misdatos, aes(y = AG_2)) + geom_bar(fill = color_barra) + 
  labs(title='Afirmación 2', subtitle = '"En general, los hombres son mejores ejecutivos de empresas que las mujeres"', x='', y='') +
  tema()

pregunta3 <- ggplot(misdatos, aes(y = AG_3)) + geom_bar(fill = color_barra) + 
  labs(title='Afirmación 3', subtitle = '"Ser ama de casa es casi tan gratificante como tener un trabajo remunerado"', x='', y='') +
  tema()

pregunta4 <- ggplot(misdatos, aes(y = AG_4)) + geom_bar(fill = color_barra) + 
  labs(title='Afirmación 4', subtitle = '"Si una mujer gana más que su marido es casi seguro que creará problemas"', x='', y='') +
  tema()

pregunta5 <- ggplot(misdatos, aes(y = AG_5)) + geom_bar(fill = color_barra) + 
  labs(title='Afirmación 5', subtitle = '"Si los padres se divorcian, es mejor que los niños vivan de manera permanente con la madre"', x='', y='') +
  tema()

pregunta6 <- ggplot(misdatos, aes(y = AG_6)) + geom_bar(fill = color_barra) + 
  labs(title='Afirmación 6', subtitle = '"Los padres son tan buenos cuidando a los niños pequeños como las madres”.', x='', y='') +
  tema()

tablasexo1 <- table(misdatos$p2, misdatos$AG_1)
tablasexo1 <- tablasexo1/rowSums(tablasexo1)
dfsexo1 <- as.data.frame(tablasexo1)
p1_sexo <- ggplot(dfsexo1, aes(y=dfsexo1$Var2, x=dfsexo1$Freq)) + geom_bar(aes(fill = dfsexo1$Var1), stat = 'identity') +
  facet_wrap(~dfsexo1$Var1) + labs(title='Afirmación 1', subtitle = '"Cuando una madre tiene un trabajo remunerado de tiempo completo los hijos sufren"', x='', y='', 
                                   caption = 'Fuente: Elaboración propia según EUT 2021-2022') +
  tema() + scale_fill_manual(values = color_sexo)

p1_sexo

tablasexo2 <- table(misdatos$p2, misdatos$AG_2)
tablasexo2 <- tablasexo2/rowSums(tablasexo2)
dfsexo2 <- as.data.frame(tablasexo2)
p2_sexo <- ggplot(dfsexo2, aes(y=dfsexo2$Var2, x=dfsexo2$Freq)) + geom_bar(aes(fill = dfsexo2$Var1), stat = 'identity') +
  facet_wrap(~dfsexo2$Var1) + labs(title='Afirmación 2', subtitle = '"En general, los hombres son mejores ejecutivos de empresas que las mujeres"', x='', y='') +
  tema() + scale_fill_manual(values = color_sexo)

tablasexo3 <- table(misdatos$p2, misdatos$AG_3)
tablasexo3 <- tablasexo3/rowSums(tablasexo3)
dfsexo3 <- as.data.frame(tablasexo3)
p3_sexo <- ggplot(dfsexo3, aes(y=dfsexo3$Var2, x=dfsexo3$Freq)) + geom_bar(aes(fill = dfsexo3$Var1), stat = 'identity') +
  facet_wrap(~dfsexo3$Var1) + labs(title='Afirmación 3', subtitle = '"Ser ama de casa es casi tan gratificante como tener un trabajo remunerado"', x='', y='') +
  tema() + scale_fill_manual(values = color_sexo)

tablasexo4 <- table(misdatos$p2, misdatos$AG_4)
tablasexo4 <- tablasexo4/rowSums(tablasexo4)
dfsexo4 <- as.data.frame(tablasexo4)
p4_sexo <- ggplot(dfsexo4, aes(y=dfsexo4$Var2, x=dfsexo4$Freq)) + geom_bar(aes(fill = dfsexo4$Var1), stat = 'identity') +
  facet_wrap(~dfsexo4$Var1) + labs(title='Afirmación 4', subtitle = '"Si una mujer gana más que su marido es casi seguro que creará problemas"', x='', y='') +
  tema() + scale_fill_manual(values = color_sexo)

tablasexo5 <- table(misdatos$p2, misdatos$AG_5)
tablasexo5 <-tablasexo5/rowSums(tablasexo5)
dfsexo5 <- as.data.frame(tablasexo5)
p5_sexo <- ggplot(dfsexo5, aes(y=dfsexo5$Var2, x=dfsexo5$Freq)) + geom_bar(aes(fill = dfsexo5$Var1), stat = 'identity') +
  facet_wrap(~dfsexo5$Var1) + labs(title='Afirmación 5', subtitle = '"Si los padres se divorcian, es mejor que los niños vivan de manera permanente con la madre"', x='', y='') +
  tema() + scale_fill_manual(values = color_sexo)

tablasexo6 <- table(misdatos$p2, misdatos$AG_6)
tablasexo6 <-tablasexo6/rowSums(tablasexo6)
dfsexo6 <- as.data.frame(tablasexo6)
p6_sexo <- ggplot(dfsexo6, aes(y=dfsexo6$Var2, x=dfsexo6$Freq)) + geom_bar(aes(fill = dfsexo6$Var1), stat = 'identity') +
  facet_wrap(~dfsexo6$Var1) + labs(title='Afirmación 6', subtitle = '"Los padres son tan buenos cuidando a los niños pequeños como las madres"', x='', y='',
                                   caption = 'Fuente: Elaboración propia según EUT 2021-2022') +
  tema() + scale_fill_manual(values = color_sexo)

p6_sexo
#tablasexo1 <- table(misdatos$p2, misdatos$AG_1)/sum(tablasexo1)
#dfsexo1 <- as.data.frame(tablasexo1)
#p1_sexo <- ggplot(dfsexo1, aes(y=dfsexo1$Var2, x=dfsexo1$Freq)) + geom_bar(aes(fill = dfsexo1$Var1), stat = 'identity') +
#  facet_wrap(~dfsexo1$Var1) + labs(title='Afirmación 1', subtitle = '"Cuando una madre tiene un trabajo remunerado de tiempo completo los hijos sufren"', x='', y='') +
#  tema() + scale_fill_manual(values = color_sexo)

tablaestudios <- table(misdatos$p6)
tablaedades <- table(misdatos$edad_tramos)
tablaedades