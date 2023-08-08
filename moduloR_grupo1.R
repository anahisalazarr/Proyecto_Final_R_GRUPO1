#PROYECTO FINAL R#
#Anahí Salazar
#### INSTALAR PAQUETES #####
install.packages("openxlsx") # importa archivos Excel"
install.packages("tidyverse") #manipulacion y ordenamiento de datos"
install.packages("magrittr") # operador pipe
install.packages("readr")
install.packages("latex")
install.packages("knitr")
install.packages("tinytex")

####ACTIVAR PAQUETES####
library(tidyverse)
library(magrittr)
library(openxlsx)
library(dplyr)
library(readxl)
library(ggplot2)
library(latexpdf)
library(knitr)
####CARGAR BASE DE DATOS#####
balances_2014<-read.xlsx("C:/Users/Anahí Salazar/Desktop/PROYECTO_FINAL_G1/Datos/balances_2014.xlsx")
balances_2014<- tibble(balances_2014)
###Explorar la Data####
balances_2014 %>% view("Data")
str(balances_2014)

####Manipulación de datos####
####Select() para seleccionar las columnas que necesito y crear la nueva base
Empresas_final<- balances_2014 %>% select(nombre_cia,situacion,tipo,tamanio,pais,provincia,canton,
                                            ciudad,ciiu4_nivel1,ciiu4_nivel6,v345,v539,v599,
                                            v499,v698,v498,trab_direc,trab_admin) %>% 
####Renombro columnas####
   rename(Empresas=nombre_cia,Status=situacion,Tipo_de_Empresa=tipo, Tamanio=tamanio,
                          País=pais,Provincia=provincia,Cantón=canton,Ciudad=ciudad,
                          Actividad_Economica=ciiu4_nivel1,Subactividad=ciiu4_nivel6,
                          Activo_corriente=v345,Pasivo_corriente=v539,Total_Pasivo=v599,
                          Total_Activo=v499,Patrimonio=v698,Activos_nocorrientes=v498,
                          Trab_Directos=trab_direc,Trab_Admin=trab_admin) %>% view("Empresas") 


####Creación de nuevas variables calculadas para obtener los indicadores#### con la función mutate()
#Creación de nuevas columnas#

  Empresas2<-Empresas_final %>% mutate(Liquidez_corriente=Activo_corriente/Pasivo_corriente,
                                 Endeudamiento_Activo=Total_Pasivo/Total_Activo,
                                 Endeudamiento_Patrimonial=Total_Pasivo/Patrimonio,
                                 Endeudamiento_fijo=Patrimonio/Activos_nocorrientes,
                                 Apalancamiento=Total_Activo/Patrimonio) %>%  view("Empresas2")
  
####Limpieza y filtrado de indicadores con datos n/a####
  
  Liquidez_corriente2<- Empresas2 %>% filter(!is.na(Liquidez_corriente)& !is.infinite(Liquidez_corriente)) 

  
  Endeudamiento_Activo2 <-Empresas2 %>% filter(!is.na(Endeudamiento_Activo)& !is.infinite(Endeudamiento_Activo)) 
  

  Endeudamiento_Patrimonial2 <- Empresas2 %>% filter(!is.na(Endeudamiento_Patrimonial)& !is.infinite(Endeudamiento_Patrimonial))
  
  Endeudamiento_fijo2<- Empresas2 %>% filter(!is.na(Endeudamiento_fijo)& !is.infinite(Endeudamiento_fijo)) 
  
  Apalancamiento2<- Empresas2 %>% filter(!is.na(Apalancamiento)& !is.infinite(Apalancamiento))
  
  ####PARTE 2####
  #Preguntas de investigación##
  #1.El endeudamiento del activo fue mayor en empresas micro+pequeñas vs. grandes?+
  
  
  Endeudamiento_por_empresa <- Endeudamiento_Activo2 %>% group_by(Tamanio) %>% 
    summarise(endeudamiento_activo_prom= mean(Endeudamiento_Activo, na.rm = TRUE)) %>% view("Endeudamiento_De_Empresas_tamanio")
  
  ##Muestro resultados
  
  #Resultados <- Endeudamiento_por_empresa %>%  %>% view("suma")
  
  ####Respuesta: Sí, el endeudamiento del activo se muestra mayor en micro+pequeñas empresas que en
  #empresas grandes.
  
  #sumar fila 1;3 y 4  ##revisar
  Na <- Endeudamiento_por_empresa[4,] %>% view("No definida")
  Nueva <- Endeudamiento_por_empresa[c(3,5),] %>% view("Nueva")
  Grande <- Endeudamiento_por_empresa[1,] %>%  view("Grande")
  
  peq_micro <- Nueva %>% summarise(across(where(is.numeric), sum, na.rm = TRUE))
  
  peq_micro$Tamanio <- "Peq_micro"
  
  peq_micro$Endeudamiento_por_empresa <- sum(peq_micro$Endeudamiento_por_empresa,na.rm=TRUE)
  
  comparar_empresas <- bind_rows(peq_micro,Grande) %>% view ("Empresas pequeñas vs.Grandes")
  

  
  ##2.¿La liquidez por tipo de compañía es diferente entre aquellas empresas que tienen más de
  ### 60 trabajadores directos y que cuenta con 100 a 800 trabajadores administrativos?####
  
  liquidez_promedio <- Liquidez_corriente2 %>%
    filter(Trab_Directos > 60, Trab_Admin >= 100, Trab_Admin <= 800) %>%
    group_by(Tipo_de_Empresa) %>%
    summarise(prom_liquidez = mean(Liquidez_corriente, na.rm = TRUE)) %>% 
    view("Liquidez con mayor número de trabajadores por tipo de empresa")
  
  liquidez_promedio2 <- Liquidez_corriente2 %>%
    filter(Trab_Directos < 60, Trab_Admin <=100) %>%
    group_by(Tipo_de_Empresa) %>%
    summarise(prom_liquidez = mean(Liquidez_corriente, na.rm = TRUE)) %>% 
    view("Liquidez con menor número de trabajadores por tipo de empresa")
  
  ###Respuesta: La liquidez es diferente con respecto a las empresas que tienen más empleados.
  #los indicadores Un índice de menos de 1 (por ejemplo, 0,75), 
  #implicaría que no puede satisfacer sus pasivos corrientes. 
  #Una relación superior a 1 (por ejemplo, 2,0), indica que puede satisfacer sus facturas actuales. 
  #De hecho, un ratio de 2,0 significa que una empresa puede cubrir dos veces su pasivo corriente.
  
  #3.Top 10 de empresas con mayor apalancamiento
  top10_empresas<- Apalancamiento2 %>% arrange(desc(Apalancamiento)) %>% slice_head(n=10) %>% 
    view("Top_10_Empresas_Apalancamiento")
  
  
  ####TAREAS ESPECIFICAS 2,3,4####
  
  
  #2. Crea una tabla resumiendo el número total de empresas por actividad económica y
  #por actividad económica por cada cantón. La tabla simplemente debe aparecer
  #como un data frame o tibble en tu script.
  
  num_empresas_por_actividad <- Empresas2 %>%
    group_by(Actividad_Economica) %>%
    summarise(num_empresas = n(), .groups = "drop") %>%
    arrange(desc(num_empresas)) %>%
    view("Número de empresas por Actividad") 
  
  num_empresas_por_actividad_canton <- Empresas2 %>%
    group_by(Actividad_Economica, Cantón) %>%
    summarise(num_empresas = n(), .groups = "drop")%>% 
    arrange(desc(num_empresas)) %>%
    view("Número de empresas por Actividad y cantón")
  
 #3. Gráficos comparativos
  
  #Liquidez por status y provincia
  
  
  # Status
  sta <- ggplot(Liquidez_corriente2, aes(x = Status, y = Liquidez_corriente, fill = Provincia)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Status) +
    labs(title = "Comparación de Liquidez por Status y Provincia",
         x = "Status",
         y = "Liquidez",
         fill = "Provincia")
  
  # Provincia 
 pro <- ggplot(Liquidez_corriente2, aes(x = Provincia, y = Liquidez_corriente, fill = Status)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Provincia) +
    labs(title = "Comparación de Liquidez por Status y Provincia",
         x = "Provincia",
         y = "Liquidez",
         fill = "Status")
  
  # 4.Gráfico de barras de solvencia por status y provincia
  # Solvencia - Endeudamiento activo
  
  ggplot(Endeudamiento_Activo2, aes(x = Status, y = Endeudamiento_Activo, fill = Status)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Provincia) +
    labs(title = "Comparación de Solvencia-Endeudamiento activo por Status y Provincia",
         y = "Endeudamiento activo",
         fill = "Status")
  
  #Filtro por una provincia-PICHINCHA
  empresas_pichincha <- Endeudamiento_Activo2 %>% filter(Provincia=="PICHINCHA")
  
  ggplot(empresas_pichincha, aes(x = Status, y = Endeudamiento_Activo)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Comparación de Solvencia por Status y Provincia",
         x = "Status",
         y = "Solvencia")
  
  # Solvencia - Endeudamiento patrimonial 
  ggplot(Endeudamiento_Patrimonial2, aes(x = Status, y = Endeudamiento_Patrimonial, fill = Status)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Provincia) +
    labs(title = "Comparación de Solvencia Y endeudamiento patrimonial por Status y Provincia",
         y = "Endeudamiento patrimonial",
         fill = "Status")
  # Solvencia - Endeudamiento activo fijo 
  ggplot(Endeudamiento_fijo2, aes(x = Status, y = Endeudamiento_fijo, fill = Status)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Provincia) +
    labs(title = "Comparación de Solvencia endeudamiento del activo fijo por Status y Provincia",
         y = "Endeudamiento del activo fijo",
         fill = "Status")
  # Solvencia - apalancamiento
  ggplot(Apalancamiento2, aes(x = Status, y = Apalancamiento, fill = Status)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Provincia) +
    labs(title = "Comparación de Solvencia Apalancamiento por Status y Provincia",
         y = "Apalancamiento",
         fill = "Status")
  
  # Gráfico de barras de liquidez por tipo de empresa
  ggplot(Liquidez_corriente2, aes(x = Tipo_de_Empresa, y = Liquidez_corriente, fill = Tipo_de_Empresa)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Tipo_de_Empresa) +
    labs(title = "Comparación de Liquidez por tipo de empresas",
         y = "Liquidez_corriente",
         fill = "Tipo de Empresas")
  
  
  #5. Gráficos de barras de solvencia por tipo de empresa
  # Solvencia - Endeudamiento activo
  ggplot(Endeudamiento_Activo2, aes(x = Tipo_de_Empresa, y = Endeudamiento_Activo, fill = Tipo_de_Empresa)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Tipo_de_Empresa) +
    labs(title = "Comparación de Solvencia endeudamiento activo por Tipo de Empresas",
         y = "Endeudamiento activo",
         fill = "Tipo de empresas")
  # Solvencia - Endeudamiento patrimonial 
  ggplot(Endeudamiento_Patrimonial2, aes(x = Tipo_de_Empresa, y = Endeudamiento_Patrimonial, fill = Tipo_de_Empresa)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Tipo_de_Empresa) +
    labs(title = "Comparación de Solvencia endeudamiento patrimonial por Tipo de empresas",
         y = "Endeudamiento patrimonial",
         fill = "Tipo de empresas")
  # Solvencia - Endeudamiento activo fijo 
  ggplot(Endeudamiento_fijo2, aes(x = Tipo_de_Empresa, y = Endeudamiento_fijo, fill = Tipo_de_Empresa)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Tipo_de_Empresa) +
    labs(title = "Comparación de Solvencia endeudamiento del activo fijo por Tipo de empresas",
         y = "Endeudamiento del activo fijo",
         fill = "Tipo de empresas")
  # Solvencia - apalancamiento
  ggplot(Apalancamiento2, aes(x = Tipo_de_Empresa, y = Apalancamiento, fill = Tipo_de_Empresa)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Tipo_de_Empresa) +
    labs(title = "Comparación de Solvencia Apalancamiento por Tipo de empresas",
         y = "Apalancamiento",
         fill = "Tipo de empresas")
  