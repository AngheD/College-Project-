#Lectura de archivo
EUT_2021 <- read.csv("EUT_2021_2022.csv")

#Librerías
library(ggplot2)
library(tidyverse)
library(dplyr)
library(showtext)
library(mclust)
library(reshape2)

#Fuentes
font_add_google('Bebas Neue')
font_add_google('Roboto Condensed')
showtext_auto()

#Colores
color_barra <- '#6D4CFF'
color_sexo <- c("#0099ff", "#F14CFF")

#Temas
tema <- function(){
  theme_gray() +
    theme(
      plot.title = element_text(size = 30, family = 'Bebas Neue', hjust = 0.5),
      plot.subtitle = element_text(size = 18, family = 'Roboto Condensed', hjust = 0.5),
      plot.margin = margin(0.5, 1, 0.2, 0.2, 'cm'),
      panel.background = element_rect(color = '#000000', fill = '#ffffff', size = 0.05),
      axis.text = element_text(size = 14, color = '#000000'),
      axis.title = element_text(size = 14, color = '#000000'),
      panel.grid = element_line(color = '#A8A8A8'),
      strip.text = element_text(size = 14, color = '#ffffff'),
      strip.background = element_rect(fill = '#000000', color = '#000000', size = 0.05),
      legend.key = element_rect(color = '#ffffff'),
      legend.title = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 1)
    )
}

#Funciones
vector_medidas <- function(datos){
  col <- c(mean(datos), sd(datos), min(datos), quantile(datos, c(0.25), names = FALSE), median(datos), quantile(datos, c(0.75), names = FALSE), max(datos))
  return(col)
}


# MANIPULACIÓN DEL ARCHIVO DE DATOS

# Creación de data.uso con las operaciones requeridas

data.uso <- EUT_2021 %>% 
  select(X, contains("_")) %>%   # seleccionar variables que contienen _
  select(X, A3_1:AG_6) %>%       # seleccionar variables desde A3_1 hasta AG_6
  pivot_longer(cols = A3_1:AG_6) %>%  # convertir a formato long todas las columnas seleccionadas
  mutate(value = ifelse(value == 99, 0, value)) %>%  # reemplazar 99 por 0
  replace_na(list(value = 0)) %>%  # reemplazar NA por 0 en todas las variables
  pivot_wider(names_from = name) %>%  # volver a formato wide
  mutate( 
    activismo = M_91_1 * 60 + M_91_2 + M_90_1 * 60 + M_90_2 + M_89_1 * 60 + M_89_2 +
      M_88_1 * 60 + M_88_2 + M_87_1 * 60 + M_87_2, 
    ayudaniños = L_851_1 * 60 + L_851_2 + L_84_1 * 60 + L_84_2,
    ayuda65 = L_851_1 * 60 + L_851_2 + L_851_1 * 60 + L_851_2,
    ayudadisca = L_861_1 * 60 + L_861_2 + L_862_1 * 60 + L_862_2,
    ayudarecibida = L_79_1 * 60 + L_79_2 + L_80_1 * 60 + L_80_2 + L_81_1 * 60 + L_81_2 +
      L_82_1 * 60 + L_82_2 + L_83_1 * 60 + L_83_2,
    apoyogratis = K_72_1 * 60 + K_72_2 + K_73_1 * 60 + K_73_2 + K_75_1 * 60 + K_75_2 +
      K_76_1 * 60 + K_76_2 + K_77_1 * 60 + K_77_2,
    cuidado65 = J5_65_1 * 60 + J5_65_2 + J5_66_1 * 60 + J5_66_2 + J5_67_1 * 60 + J5_67_2 +
      J5_68_1 * 60 + J5_68_2 + J5_68B_1 * 60 + J5_68B_2 + J5_69_1 * 60 + J5_69_2 +
      J5_70_1 * 60 + J5_70_2 + J5_71_1 * 60 + J5_71_2 + J5_72_1 * 60 + J5_72_2,
    cuidado6a12 = J4_58_1 * 60 + J4_58_2 + J4_59_1 * 60 + J4_59_2 + J4_60_1 * 60 + J4_60_2 +
      J4_61_1 * 60 + J4_61_2 + J4_62_1 * 60 + J4_62_2 + J4_63_1 * 60 + J4_63_2,
    cuidado4o5 = J3_50_1 * 60 + J3_50_2 + J3_51_1 * 60 + J3_51_2 + J3_52_1 * 60 + J3_52_2 +
      J3_53_1 * 60 + J3_53_2 + J3_54_1 * 60 + J3_54_2 + J3_55_1 * 60 + J3_55_2 +
      J3_56_1 * 60 + J3_56_2,
    cuidado4o5 = J2_42_1 * 60 + J2_42_2 + J2_43_1 * 60 + J2_43_2 + J2_44_1 * 60 + J2_44_2 +
      J2_45_1 * 60 + J2_45_2 + J2_46_1 * 60 + J2_46_2 + J2_47_1 * 60 + J2_47_2,
    cuidadodisca = J1_32_1 * 60 + J1_32_2 + J1_33_1 * 60 + J1_33_2 + J1_33_1 * 60 + J1_33_2 +
      J1_34_1 * 60 + J1_34_2 + J1_34B_1 * 60 + J1_34B_2 + J1_35_1 * 60 + J1_35_2 +
      J1_36_1 * 60 + J1_36_2 + J1_37_1 * 60 + J1_37_2 + J1_38_1 * 60 + J1_38_2,
    cuidados = (cuidado65 + cuidado6a12 + cuidado4o5 + cuidadodisca),
    tramites = H21_1 * 60 + H21_2,
    mantenimiento = G20_1 * 60 + G20_2,
    cría = F19_1 * 60 + F19_2,
    compras = E18_1 * 60 + E18_2 + E17_1 * 60 + E17_2,
    limpieza = D16_1 * 60 + D16_2 + D15_1 * 60 + D15_2 + C13_1 * 60 + C13_2 + + C14_1 * 60 + C14_2,
    alimentación = B10_1 * 60 + B10_2 + B11_1 * 60 + B11_2, 
    trabajo = H4_1 * 60 + H4_2,
    trabajonorem = alimentación + limpieza + compras + cría + mantenimiento + tramites + cuidados + apoyogratis + activismo
  ) %>%
  select(X, activismo, ayudaniños, ayuda65, ayudarecibida, apoyogratis, cuidado65, cuidado6a12, cuidado4o5, cuidados, tramites, mantenimiento, cría, compras, limpieza, alimentación, trabajo, trabajonorem)

# Realizar la unión con EUT_2021
data <- inner_join(EUT_2021, data.uso)

# paso las variables a horas
data <- data %>%
  mutate_at(vars(activismo:trabajonorem), ~ . / 60) 

# transformo las variables de segmentación a factor
data$p2 <- factor(data$p2, 
                  levels = c("1", "2"),
                  labels = c ("Varón", "Mujer"))

#Factores
data$p6 <- factor(data$p6, 
                  levels = c ("1", "2", "3", "4", "5", "6", "7"),
                  labels = c ("Sin instrucción", "Primaria incompleta", 
                              "Primaria completa", "Secundaria incompleta",
                              "Secundaria completa", "Terciaria incompleta", "Terciaria completa"))

data <- data %>% mutate(edad_categoria = case_when(
                          edad_tramos %in% c("[0,5)", "[5,10)") ~ "0-10",
                          edad_tramos %in% c("[10,15)", "[15,20)") ~ "10-20",
                          edad_tramos %in% c("[20,25)", "[25,30)") ~ "20-30",
                          edad_tramos %in% c("[30,35)", "[35,40)") ~ "30-40",
                          edad_tramos %in% c("[40,45)", "[45,50)") ~ "40-50",
                          edad_tramos %in% c("[50,55)", "[55,60)") ~ "50-60",
                          edad_tramos %in% c("[60,65)", "[65,70)") ~ "60-70",
                          edad_tramos %in% c("[70,75)", "[75,80)") ~ "70-80",
                          edad_tramos %in% c("[80,85)", "[85,90)") ~ "80-90",
                          edad_tramos %in% c("[90,95)", "[95,100)","[100,105)") ~ "90 o màs",TRUE ~ NA_character_))
                        
data$domdepartamento <- factor(data$domdepartamento, 
                          levels = c (1:19),
                          labels = c ("Montevideo", "Artigas", "Canelones", "Cerro Largo",
                                      "Colonia", "Durazno", "Flores", "Florida", "Lavalleja",
                                      "Maldonado", "Paysandú", "Río Negro", "Rivera", "Rocha","Salto", "San José", "Soriano", "Tacuarembó", "Treinta y Tres"))

data$region <- factor(case_when(
                            data$domdepartamento %in% c("Montevideo", "Canelones", "San José") ~ "Región Metropolitana",
                            data$domdepartamento %in% c("Colonia", "Soriano", "Río Negro", "Paysandú", "Salto") ~ "Región Litoral",
                            data$domdepartamento %in% c("Artigas", "Rivera", "Tacuarembó", "Cerro Largo") ~ "Región Norte",
                            data$domdepartamento %in% c("Rocha", "Lavalleja", "Maldonado", "Treinta y Tres", "Florida", "Durazno", "Flores") ~ "Región Este"
                          )
                        )
                        
# Armo un DF sin la poblaciòn que no es PEA Y LOS +95
data_filtrado <- data %>% filter(!(edad_tramos %in% c("[0,5)", "[5,10)","[10,15)","[95,100)","[100,105)")))
                        
#Se filtran los datos, considerando sólo los que tienen una respuesta registrada en las preguntas de percepción. 
criterio_percepcion <- data$AG_1%in%c(1:6)&data$AG_2%in%c(1:6)&data$AG_3%in%c(1:6)&data$AG_4%in%c(1:6)&data$AG_5%in%c(1:6)&data$AG_6%in%c(1:6)
data_percepcion <- data %>% filter(criterio_percepcion)

#Se crean factores descriptivos de las respuestas:
data_percepcion$AG_1 <- factor(data_percepcion$AG_1, levels=c(6:1), labels=c('No contesta', 'No sabe', 'Muy en desacuerdo','En desacuerdo', 'De acuerdo', 'Muy de acuerdo'))
data_percepcion$AG_2 <- factor(data_percepcion$AG_2, levels=c(6:1), labels=c('No contesta', 'No sabe', 'Muy en desacuerdo','En desacuerdo', 'De acuerdo', 'Muy de acuerdo'))
data_percepcion$AG_3 <- factor(data_percepcion$AG_3, levels=c(6:1), labels=c('No contesta', 'No sabe', 'Muy en desacuerdo','En desacuerdo', 'De acuerdo', 'Muy de acuerdo'))
data_percepcion$AG_4 <- factor(data_percepcion$AG_4, levels=c(6:1), labels=c('No contesta', 'No sabe', 'Muy en desacuerdo','En desacuerdo', 'De acuerdo', 'Muy de acuerdo'))
data_percepcion$AG_5 <- factor(data_percepcion$AG_5, levels=c(6:1), labels=c('No contesta', 'No sabe', 'Muy en desacuerdo','En desacuerdo', 'De acuerdo', 'Muy de acuerdo'))
data_percepcion$AG_6 <- factor(data_percepcion$AG_6, levels=c(6:1), labels=c('No contesta', 'No sabe', 'Muy en desacuerdo','En desacuerdo', 'De acuerdo', 'Muy de acuerdo'))

# ANÁLISIS DE DATOS

#Media trabajo no remunerado
tiempo_medio <- data_filtrado %>% group_by(region, p2) %>% summarise(tiempo_medio = mean(trabajonorem, na.rm = TRUE))
                        
# Crear el gráfico de barras facetado por región, ordenado por tiempo no remunerado de mayor a menor
ggplot(tiempo_medio, aes(y = reorder(region, -tiempo_medio), x = tiempo_medio, fill = p2)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = 'Trabajo no remunerado según región', subtitle = 'Trabajo no remunerado', 
             y = '', x = "Tiempo medio en horas", fill = "Género",
             caption = "Fuente: Elaboración propia según EUT 2021-2022") +
        tema() + scale_fill_manual(values = color_sexo) + 
        coord_cartesian(ylim = c(0, 5))
  

# Gráfico y correlación
        
ggplot(data_filtrado, aes(x = trabajo, y = trabajonorem, color = p2)) +
       geom_point() + # puntos coloreados por P2
       geom_smooth(method = "lm", se = FALSE) +  # línea de regresión lineal
       labs(x = "Trabajo remunerado",
            y = "Trabajo no remunerado",
            color = "Género",
            caption = "Fuente: Elaboración propia según EUT 2021-2022",
            title = "Relación entre trabajo remunerado y no remunerado según género",
            subtitle = "Cantidad de horas dedicadas el día anterior a cada tipo de trabajo") +  # etiquetas de ejes, leyenda y título
        tema() + scale_colour_manual(values = color_sexo) + # estilo del tema del gráfico
        scale_x_continuous(expand = c(0, 0)) +  # dejar que los límites del eje X se ajusten automáticamente
        scale_y_continuous(expand = c(0, 0), limits = c(0, 24)) # ajustar límites del eje Y
                            
          
# Gráfico y correlación facetado por nivel educativo
                        
ggplot(data_filtrado, aes(x = trabajo, y = trabajonorem, color = factor(p2))) +
      geom_point() +  # puntos coloreados por P2
      geom_smooth(method = "lm", se = FALSE) +  # línea de regresión lineal
      labs(x = "Trabajo remunerado",
           y = "Trabajo no remunerado",
           caption = "Fuente: Elaboración propia según EUT 2021-2022",
           color = "Género",
           title = "Relación entre el trabajo remunerado y no remunerado según nivel educativo",
           subtitle = "Cantidad de horas dedicadas el día anterior a cada tipo de trabajo") +  # etiquetas de ejes y leyenda
      facet_wrap(~ p6) +  # facetar por la variable p6
      tema() + scale_colour_manual(values = color_sexo) + # estilo del tema del gráfico
      scale_x_continuous(expand = c(0, 0), limits = c(0, 24)) +  # ajustar límites del eje X
      scale_y_continuous(expand = c(0, 0), limits = c(0, 24)) # ajustar límites del eje Y
                          
# Gráfico y correlación facetado por edad

ggplot(data_filtrado, aes(x = trabajo, y = trabajonorem, color = p2)) +
      geom_point() +  # puntos coloreados por P2
      geom_smooth(method = "lm", se = FALSE) +  # línea de regresión lineal
      labs(x = "Trabajo remunerado",
           y = "Trabajo no remunerado",
           caption = "Fuente: Elaboración propia según EUT 2021-2022",
           color = "Género",
           title = "Relación entre el trabajo remunerado y no remunerado según la edad",
           subtitle = "Cantidad de horas dedicadas el día anterior a cada tipo de trabajo") +  # etiquetas de ejes y leyenda
      facet_wrap(~ edad_categoria) +  # facetar por la variable edad_categoria
      tema() + scale_colour_manual(values = color_sexo) +  # estilo del tema del gráfico
      scale_y_continuous(expand = c(0, 0), limits = c(0, 24))  # ajustar límites del eje Y
                          
                        
# Calcular correlación

correlacion <- cor(data_filtrado$trabajonorem, data_filtrado$trabajo)
print(paste("Correlación entre trabajo no remunerado y trabajo remunerado:", correlacion))
                        
                        
# Calcular correlación entre trabajo no remunerado

variables <- data_filtrado[c("alimentación", "limpieza", "compras", "cría", "mantenimiento","tramites", "cuidados", "apoyogratis", "activismo")]
matriz_correlacion <- cor(variables)
matriz_correlacion_long <- melt(matriz_correlacion)

# Crear el heatmap con heatmap()
myColors <- colorRampPalette(c("#FFFFFF", color_barra))(100)

ggplot(matriz_correlacion_long, aes(Var2, Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradientn(colours = myColors) +
  labs(title = 'mapa de calor',
       subtitle = "Correlación entre actividades no remuneradas",
       fill = 'Valor',
       caption = "Fuente: Elaboración propia según EUT 2021-2022",
       x = "", y = "") +
  tema() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
    )  # Ajustar tamaño de las letras para el eje y


ggplot(data_filtrado, aes(x = trabajonorem, fill = factor(p2))) +
    geom_histogram(binwidth = 5, position = "identity", alpha = 0.7) +
    labs(
        title = "Cantidad de horas que dedicó al trabajo no remunerado el día anterior según género",
        x = "Trabajo no remunerado",
        y = "Frecuencia",
        fill = "Género"
        ) +
    coord_cartesian(xlim = c(0, 24)) +  # Limitar el eje x hasta 24
    theme_minimal() + labs(caption = "Fuente: Elaboración propia según EUT 2021-2022")
                        
                        
#histograma conjunto horas trabajo no remunerado, por maximo nivel educativo y facetado sexo
                        
ggplot(data_filtrado, aes(x = trabajonorem, fill = factor(p6))) +
      geom_histogram(binwidth = 5, position = "identity", alpha = 0.7) +
      labs(
           title = "Cantidad de horas que dedicó al trabajo no remunerado el día anterior según nivel educativo",
           x = "Trabajo no remunerado",
           y = "Frecuencia",
           fill = "Máximo nivel educativo alcanzado"
          ) + facet_wrap(~ p2) + 
      coord_cartesian(xlim = c(0, 24)) +  # Limitar el eje x hasta 24
      theme_minimal() + theme(
                              plot.title = element_text(size = 10),  # ajustar tamaño de la letra del título
                              plot.caption = element_text(size = 7, hjust = 0)  # ajustar tamaño de la letra de la fuente y alineación
                              ) +
      labs(caption = "Fuente: Elaboración propia según EUT 2021-2022")
                        
                        
#histograma conjunto horas trabajo no remunerado, facetado por nivel educativo segùn sexo
                        
ggplot(data_filtrado, aes(x = trabajonorem, fill = factor(p2))) +
      geom_histogram(binwidth = 5, position = "identity", alpha = 0.7) +
      labs(
           title = "Cantidad de horas que dedicó al trabajo no remunerado el día anterior según nivel educativo",
           x = "Trabajo no remunerado",
           y = "Frecuencia",
           fill = "Género"
           ) + facet_wrap(~ p6) + 
           coord_cartesian(xlim = c(0, 24)) +  # Limitar el eje x hasta 24
           theme_minimal() + theme(
           plot.title = element_text(size = 10),  # ajustar tamaño de la letra del título
           plot.caption = element_text(size = 7, hjust = 0)  # ajustar tamaño de la letra de la fuente y alineación
           ) +
       labs(caption = "Fuente: Elaboración propia según EUT 2021-2022")
                        
                        
                        
ggplot(data_filtrado, aes(x = trabajonorem, fill = factor(p6))) +
       geom_histogram(binwidth = 5, position = "identity", alpha = 0.7) +
       labs(
            title = "Histograma de trabajo no remunerado por nivel educativo",
            x = "Trabajo no remunerado",
            y = "Frecuencia",
            fill = "Máximo nivel educativo alcanzado"
            ) +
       coord_cartesian(xlim = c(0, 24)) +  # Limitar el eje x hasta 24
       theme_minimal()
                        
#Distribucion de trabajo no reminerado segìn genero 
                        
#lo mismo facetado
                        
media_trabajo <- mean (data_filtrado$trabajo, na.rm = FALSE)
                        
ggplot(data_filtrado, aes(x = factor(p2), y = trabajonorem, fill = factor(p2))) +
       geom_boxplot() +  # crear el diagrama de cajas
       geom_hline(yintercept = media_trabajo, linetype = "dashed", color = "red") +  # añadir la línea de referencia
       labs(
            x = "Género",
            y = "Trabajo no remunerado",
            title = "Distribución del trabajo no remunerado según género",
            fill = "Género",
            caption = "Fuente: Elaboración propia según EUT 2021-2022"
       ) +  # etiquetas de ejes, título y leyenda
       scale_y_continuous(limits = c(0, 24), expand = c(0, 0)) +  # ajustar límites del eje Y
       theme_minimal() +  # estilo del tema del gráfico
       theme(
             plot.title = element_text(size = 14),  # ajustar tamaño de la letra del título
             plot.caption = element_text(size = 7, hjust = 0),  # ajustar tamaño de la letra de la fuente y alineación
             axis.line.x = element_line(color = "black", size = 0.5),  # línea del eje X
             axis.line.y = element_line(color = "black", size = 0.5)  # línea del eje Y
       ) + facet_wrap(~ p6)
                        
                    
colores_genero <- c("Varón" = "#00BFC4", "Mujer" = "#F8766D")
                        
# Crear el gráfico
ggplot(data_filtrado, aes(x = factor(p2), y = trabajonorem, fill = factor(p2))) +
      geom_boxplot() +  # crear el diagrama de cajas
      geom_hline(yintercept = media_trabajo, linetype = "dashed", color = "red") +  # añadir la línea de referencia
      labs(
           x = "Género",
           y = "Trabajo no remunerado",
           title = "Distribución del trabajo no remunerado según género",
           fill = "Género",
           caption = "Fuente: Elaboración propia según EUT 2021-2022"
           ) +  # etiquetas de ejes, título y leyenda
      scale_y_continuous(limits = c(0, 24), expand = c(0, 0)) +  # ajustar límites del eje Y
      scale_fill_manual(values = colores_genero) +  # aplicar los colores personalizados
      theme_minimal() +  # estilo del tema del gráfico
      theme(
            plot.title = element_text(size = 14),  # ajustar tamaño de la letra del título
            plot.caption = element_text(size = 7, hjust = 0),  # ajustar tamaño de la letra de la fuente y alineación
            axis.line.x = element_line(color = "black", size = 0.5),  # línea del eje X
            axis.line.y = element_line(color = "black", size = 0.5)  # línea del eje Y
            ) + 
      facet_wrap(~ p6)  # facetas por el nivel de p6
                        
                        
                        
media_trabajo <- mean (data_filtrado$trabajonorem, na.rm = FALSE)
                        
ggplot(data_filtrado, aes(x = factor(p2), y = trabajonorem, fill = factor(p2))) +
      geom_boxplot(show.legend = FALSE) +  # crear el diagrama de cajas
      geom_hline(aes(yintercept = media_trabajo, linetype = 'Media de trabajo no remunerado'), color = color_barra, size = 0.8) +  # añadir la línea de referencia
      scale_linetype_manual(name = '', values = 'solid') +
      labs(
          x = "",
          y = "Trabajo no remunerado en horas",
          title = "Distribución del trabajo no remunerado según género y nivel educativo",
          caption = "Fuente: Elaboración propia según EUT 2021-2022"
          ) +  # etiquetas de ejes, título y leyenda
      scale_y_continuous(limits = c(0, 24), expand = c(0, 0)) +  # ajustar límites del eje Y
      tema() + scale_fill_manual(values = color_sexo) + 
      theme(legend.position="bottom") +# estilo del tema del gráfico
      facet_wrap(~ p6, nrow = 1)

                        
                        
                        
                        
                        #lo mismo sin facetar
                        
                        
                        ggplot(data_filtrado, aes(x = factor(p2), y = trabajonorem, fill = factor(p2))) +
                          geom_boxplot() +  # crear el diagrama de cajas
                          geom_hline(yintercept = media_trabajo, linetype = "dashed", color = "red") +  # añadir la línea de referencia
                          labs(
                            x = "Género",
                            y = "Trabajo no remunerado",
                            title = "Distribución del trabajo no remunerado según género",
                            fill = "Género",
                            caption = "Fuente: Elaboración propia según EUT 2021-2022"
                          ) +  # etiquetas de ejes, título y leyenda
                          scale_y_continuous(limits = c(0, 24), expand = c(0, 0)) +  # ajustar límites del eje Y
                          theme_minimal() +  # estilo del tema del gráfico
                          theme(
                            plot.title = element_text(size = 14),  # ajustar tamaño de la letra del título
                            plot.caption = element_text(size = 7, hjust = 0),  # ajustar tamaño de la letra de la fuente y alineación
                            axis.line.x = element_line(color = "black", size = 0.5),  # línea del eje X
                            axis.line.y = element_line(color = "black", size = 0.5)  # línea del eje Y
                          )
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        # Suponiendo que 'data' es tu dataframe con las variables calculadas en minutos
                        
                        ggplot(data_filtrado, aes(x = trabajo, y = cuidados, color = factor(p2))) +
                          geom_point() +
                          facet_wrap(~ edad_categoria, scales = "free") +
                          labs(
                            title = "Relación entre Trabajo y Cuidados por Sexo y Edad",
                            x = "Trabajo (horas)",
                            y = "Cuidados (horas)",
                            color = "Sexo"
                          ) +
                          theme_minimal()
                        
#                        library(ggplot2)
#                        library(GGally)
#                        
##                        library(ggplot2)
#                        library(GGally)
#                        
#                        # Seleccionar variables para ggpairs
#                        variables_ggpairs <- c("trabajonorem", "trabajo")
#                        
#                        # Crear el gráfico ggpairs sin facetas
#                        ggpairs_plot <- ggpairs(data_filtrado[, variables_ggpairs], columns = variables_ggpairs, 
#                                                upper = list(continuous = "points"), lower = list(continuous = "smooth"))
#                        
#                        # Pintar por p2 y facetar por p6 usando ggplot2
##                     geom_point() +
#                          geom_smooth(method = "lm", se = FALSE) +
#                          labs(
#                            x = "Trabajo no remunerado",
#                            y = "Trabajo remunerado",
#                            color = "Género",
#                            title = "Relación entre trabajo no remunerado y remunerado por género"
#                          ) +
###                          facet_grid(p6 ~ .) +
  #                        scale_x_continuous(limits = c(0, 24)) +
#                          scale_y_continuous(limits = c(0, 24)) +
#                          theme_minimal() +
###                          theme(
  #                          plot.title = element_text(size = 14),
  #                          axis.line = element_line(color = "black"),
  ##                          panel.grid.minor = element_blank(),
  #                          panel.grid.major = element_line(color = "gray", linetype = "dashed"),
  ###                          panel.border = element_blank()
     #                     ) 
                        
varon_aux <- subset(data_filtrado, data_filtrado$p2 %in% c('Varón') & data_filtrado$trabajonorem<=24)$trabajonorem
mujer_aux <- subset(data_filtrado, data_filtrado$p2 %in% c('Mujer') & data_filtrado$trabajonorem<=24)$trabajonorem
gral_aux <- subset(data_filtrado, data_filtrado$trabajonorem<=24)$trabajonorem

tabla_medidas <- data.frame(Varón = vector_medidas(varon_aux), Gral = vector_medidas(gral_aux), Mujer = vector_medidas(mujer_aux), row.names = c('Media', 'Desviación', 'min', 'q1', 'q2', 'q3', 'max'))




#    varon_aux), mean(mujer_aux), sd(varon_aux), sd(mujer_aux), min(varon_aux), min(mujer_aux)),
#                     quantile(varon_aux, c(0.25)), quantile(mujer_aux, c(0.25)), median(varon_aux), median(mujer_aux),
#                     quantile(varon_aux, c(0.75)), quantile(mujer_aux, c(0.75)), max(varon_aux), max(mujer_aux))),
#                     row.names = c('Media', 'Desviación', 'min', 'q1', 'q2', 'q3', 'max'), colnames(c('Varon', 'Mujer')))
  
  
#  }


media_varon <- mean(varon_aux)