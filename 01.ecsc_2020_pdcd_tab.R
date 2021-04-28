# 0. librerías -------------------------------------------------------------------------------------
options(scipen=999)

Sys.setenv(http_proxy='http://staff-proxy.ul.ie:8080')
Sys.getenv("HTTPS_PROXY")

lib <- c('tidyr','plyr', 'ggplot2','viridis','dplyr',
         'forcats','hrbrthemes','data.table','curl','readxl','foreign')

lapply(lib, library, character.only = T);rm(lib)

# a. tablas de datos -------------------------------------------------------------------------------
# * a.1. lista de micro datos ----------------------------------------------------------------------

# # # lista de archivos

# # # definición de identificador único de descarga para la tabla de dirección de las tablas de 
# # # micro-dato de la ECSC 2020.

a1_fil  <- '13ef5yuDPVOl1aOzTwTE3os-p-vljBv5e'
a1_pat  <- 'https://docs.google.com/uc?id=%s&export=download'
a1_tab <- data.frame(data.table::fread(sprintf(a1_pat, a1_fil,download.method = 'curl'), 
                                       header = T,sep = ';'))

# # # define función de cargue.
# # # función de descarga de tablas con argumentos de identificador único y denominación.

a1_f1 <- function(x){data.table::fread(sprintf(a1_pat,x),header = T)}

# # # vector de denominación de tablas.
a1_nam <- a1_tab$name

# # # lista de tablas ECSC 2020.
a1_lta <- lapply(a1_tab$file_id,a1_f1)
names(a1_lta)  <- a1_nam

# * a.2. selección de módulos ----------------------------------------------------------------------

# # # no todos los módulos son utilizados en la construcción de los indicadores de problemas y 
# # # necesidades jurídicas.

# * * * * tablas de hogar y personas ---------------------------------------------------------------
a2_01 <- a1_lta[[ 1]] # 01.datos de la vivienda.
a2_04 <- a1_lta[[ 4]] # 04.características generales de las personas.
a2_16 <- a1_lta[[16]] # 16.características generales de las personas (cierre).

# * * * * tablas de necesidades jurídicas ----------------------------------------------------------

# # # 13.pdcd clsinp - ciclo largo sin priorizar - (0 < n < 3) - problemas.
# # # 14.pdcd clconp - ciclo largo priorizado    - (n > 2)     - personas.
# # # 15.pdcd cc     - ciclo corto               - (n-2 > 2)   - problemas.

a2_13 <- a1_lta[[13]]; a2_13$FEX_C <- as.numeric(gsub(',','.',a2_13$FEX_C)) # # 13.pdcd clsinp
a2_14 <- a1_lta[[14]]; a2_14$FEX_C <- as.numeric(gsub(',','.',a2_14$FEX_C)) # # 14.pdcd clconp
a2_15 <- a1_lta[[15]]; a2_15$FEX_C <- as.numeric(gsub(',','.',a2_15$FEX_C)) # # 15.pdcd cc

# * * * * identificadores y subconjuntos en tablas de necesidades jurídicas ------------------------

# # # Diagnostico de identificadores únicos en tablas. Solo la tabla a2_14 identifica de manera
# # # plena a los individuos. Las variables (SECUENCIA_P), (ORDEN) y (SECUENCIA_ENCUESTA), no 
# # # est?n codificadas en las tablas a2_13 y a2_15.

# # # a2_13 tabla de problemas ciclo largo sin priorizar.
a2_13$keyper <- paste0(
  a2_13$DIRECTORIO,
  a2_13$NRO_ENCUESTA,
  a2_13$SECUENCIA_P,
  a2_13$ORDEN
)

# # # a2_14 tabla de problemas ciclo largo priorizado
a2_14$keyper <- paste0(
  a2_14$DIRECTORIO,
  a2_14$NRO_ENCUESTA,
  a2_14$SECUENCIA_P,
  a2_14$ORDEN
)

# # # a2_15 tabla de problemas ciclo corto
a2_15$keyper <- paste0(
  a2_15$DIRECTORIO,
  a2_15$NRO_ENCUESTA,
  a2_15$SECUENCIA_P,
  a2_15$ORDEN
)

# # # identificadores únicos

table(duplicated(a2_13$keyper));table(duplicated(a2_14$keyper));table(duplicated(a2_15$keyper))

table(a2_15$keype%in%a2_14$keype)
table(a2_13$keype%in%a2_14$keype)

table(a2_15$keype%in%a2_14$keype)
table(a2_15$keype%in%a2_13$keype)

# b. Población ------------------------------------------------------------------------------------
# * b.1. marco de personas y hogares ---------------------------------------------------------------

# * * * * tabla de personas  -----------------------------------------------------------------------

# # # Identificadores en el módulo de inicio de características generales de personas
b1_04 <- a2_04
b1_04$keyper <- paste0(
  b1_04$DIRECTORIO,
  b1_04$NRO_ENCUESTA,
  b1_04$SECUENCIA_P,
  b1_04$ORDEN)

b1_04$keyhog <- paste0(b1_04$DIRECTORIO)

table(duplicated(b1_04$keyhog)); table(duplicated(b1_04$keyper))

# # # Identificadores en el módulo de cierre de características generales de personas
b1_16 <- a2_16
b1_16$keyper <- paste0(
  b1_16$DIRECTORIO,
  b1_16$NRO_ENCUESTA,
  b1_16$SECUENCIA_P,
  b1_16$ORDEN)

table(duplicated(b1_16$keyhog));table(duplicated(b1_16$keyper))

# * * * * tabla de hogares  ------------------------------------------------------------------------

# # # Identificadores en el módulo de hogares
b1_01 <- a2_01
b1_01$keyhog <- paste0(b1_01$DIRECTORIO)
table(duplicated(b1_01$keyhog))

# * * * * marco  -----------------------------------------------------------------------------------
# # # construcción de un marco de personas con información de características generales y del hogar.

# # # exclusión de variables redundantes en hogares y características generales cierre.
b1_01[ ,c('DIRECTORIO', 'NRO_ENCUESTA',
          'SECUENCIA_ENCUESTA','SECUENCIA_P',
          'ORDEN','FEX_C')] <- list(NULL)
b1_16[ ,c('DIRECTORIO', 'NRO_ENCUESTA',
          'SECUENCIA_ENCUESTA','SECUENCIA_P',
          'ORDEN','FEX_C')] <- list(NULL)

# # # check de identificadores en la tabla de hogares. pegue con el marco de personas mediante
# # # (key_hog).

table(b1_01$keyhog %in% b1_04$keyhog)
b1_04 <- merge(b1_04,b1_01, by ='keyhog', all.x = TRUE)

# # # check de identificadores en la tabla personas cierre, solo para mayores de 14 años. pegue con
# # # el marco de personas mediante (key_per).

table(b1_16$keyper %in% b1_04$keyper)
b1_04 <- merge(b1_04,b1_16, by ='keyper', all.x = TRUE)

table(is.na(b1_04$P6210S1),b1_04$P5785)
table(b1_04$P5785); sum(b1_04$FEX_C[(b1_04$P5785 < 18)])

# # # hogares con menores de edad ------------------------------------------------------------------
# # # construcción de variables indicadoras de menores y hogares con menores de edad.
# # # creación de dummy de menores y check de totales.
b1_04_meno <- b1_04
b1_04_meno$menor <- 0 ; b1_04_meno$menor[(b1_04_meno$P5785 < 18)] <- 1
table(b1_04_meno$menor) ; sum(b1_04_meno$FEX_C[(b1_04_meno$menor == 1)])

# # # agrupación para conteo de menores por hogar.
b1_04_meno <- doBy::summaryBy(menor + FEX_C ~ keyhog, 
                              FUN = sum, 
                              data = b1_04_meno[(b1_04_meno$menor == 1)])
b1_04_meno$menor       <-  1
b1_04_meno$menor_count <-  b1_04_meno$menor.sum 
b1_04_meno$menor.sum   <-  NULL
b1_04_meno$FEX_C.sum   <-  NULL

# # #  pegue con el marco de personas mediante (key_per).
b1_04 <- merge(b1_04,b1_04_meno, by ='keyhog', all.x = T); b1_04_meno <- NULL

table(is.na(b1_04$menor)); b1_04$menor[is.na(b1_04$menor) == T ] <- 0
table((b1_04$menor))

table(is.na(b1_04$menor_count)); b1_04$menor_count[is.na(b1_04$menor_count) == T ] <- 0
table((b1_04$menor_count)) 

sum(b1_04$FEX_C[(b1_04$P5785 < 18)]); sum(b1_04$FEX_C[(b1_04$menor == 1)])

b1_04_meno <- NULL

# # # clase y grupos territoriales ----------------------------------------------------------------- 
# # # revisión de agregados de población según clase y grupos territoriales 
table(b1_04$CLASE); table(is.na(b1_04$CLASE))

# # 28 ciudades ---> 18.407.691
sum(b1_04$FEX_C[which(b1_04$P5785 >= 15 & b1_04$CIUDADES28 == 1)])

# # cabecera ---> 29.379.989
sum(b1_04$FEX_C[which(b1_04$P5785 >= 15 & b1_04$CLASE == 'Cabecera')])

# # centro rural ---> 82.537.75
sum(b1_04$FEX_C[which(b1_04$P5785 >= 15 & b1_04$CLASE != 'Cabecera')])

# # total   ---> 37.633.764
sum(b1_04$FEX_C[which(b1_04$P5785 >= 18)])
nrow(b1_04[which(b1_04$P5785 >= 18)])

# c. Problemas declaradis y caracterizados  --------------------------------------------------------

# # # identificación de universo de declarantes a partir de la tabla de personas a2_14. en esta 
# # # solo se caracterizan, según ruta, los 2 principales problemas de aquellas personas que 
# # # declararon más de dos problemas. Los demás problemas son identificados por tipo pero no
# # # por ruta. 

# * c.1.consolidación de tabla de declarantes con controles demográficos ---------------------------

# # # exclusión de variables redundantes características generales.
c1_14 <- a2_14
b1_04[ ,c('DIRECTORIO', 'NRO_ENCUESTA',
          'SECUENCIA_ENCUESTA','SECUENCIA_P',
          'ORDEN','FEX_C')] <- list(NULL)

# # # pegue con el marco de personas mediante (key_per).
table(c1_14$keyper %in% b1_04$keyper)
c1_14 <- merge(c1_14,b1_04, all.x = TRUE, by = 'keyper')

sum(c1_14$FEX_C);table(duplicated(c1_14$keyper))
table(is.na(c1_14$menor))

# * * * * identificación de problemas priorizados --------------------------------------------------

c1_14 <- data.frame(c1_14)

# # # (nj_1_count) - conteo total de problemas declarados a partir de las variables (P1670SnA1_k).
# # # (n = c?digo de categor?a de problema).
# # # (k = c?digo de tipo de problema).

length(grep('A1_', names(c1_14)))
c1_14$nj_1_count <- rowSums(c1_14[,grep('A1_', names(c1_14))],na.rm = T)

# # # (nj_1) - conteo del al menos un problema declarado.

c1_14$nj_1 <- c1_14$nj_1_count/c1_14$nj_1_count
c1_14$nj_1[is.na(c1_14$nj_1)] <- 0
table(c1_14$nj_1)

# * * * * indicadores de declaración de problemas --------------------------------------------------

# # # # replica DANE para total nacional en nivel                                     ---> 6.052.445
# 
# sum(c1_14$nj_1,na.rm = T)
# sum(c1_14$FEX_C[which(c1_14$nj_1 == 1)])
# 
# # # # replica DANE para total nacional en tasa                                        ---> 17,102%
# 
# round((sum(c1_14$FEX_C[which(c1_14$nj_1 == 1)])/sum(c1_14$FEX_C))*100,3)
# 
# # # # replica DANE para cabecera en nivel                                           ---> 5.159.286
# 
# table(c1_14$CLASE,c1_14$nj_1); table(c1_14$CLASE); table(is.na(c1_14$CLASE))
# sum(c1_14$FEX_C[which(c1_14$nj_1 == 1 & c1_14$CLASE == 'Cabecera')])
# 
# # #  # replica DANE para cabecera en tasa                                             ---> 18,569%
# 
# round((sum(c1_14$FEX_C[which(c1_14$nj_1 == 1 & c1_14$CLASE == 'Cabecera')])/
# sum(c1_14$FEX_C[which( c1_14$CLASE == 'Cabecera')]))*100,3)
# 
# # # # replica  DANE para rural disperso en nivel                                      ---> 893.158
# 
# sum(c1_14$FEX_C[which(c1_14$nj_1 == 1 & 
#                         c1_14$CLASE == 'Centro poblado y rur')])
# 
# # # # replica  DANE para cabecera en tasa                                             ---> 11,743%
# round(
#   (
#     (sum(c1_14$FEX_C[which(c1_14$nj_1 == 1 & 
#                             c1_14$CLASE == 'Centro poblado y rur')])
#      /
#      sum(c1_14$FEX_C[which(c1_14$CLASE == 'Centro poblado y rur')])
#     )*100
#   ),
#    3)

# * c.2. caracterización de problemas declarados priorizados  --------------------------------------

# # # las tablas (c1_14 - a2_14) contienen información de caracterización de ruta para aquellos 
# # # dos problemas priorizados que fueron declarados por personas con al menos 3 problemas. dicha
# # # información está en una estructura wide con identificadores únicos de personas. 

# # # por esto, para recuperar los problemas declarados en necesario hacer un reshape
# # # a formato long que identifique el problema, no la persona que lo declare. No obstante la
# # # dimensión de la tabla no permite el uso de funciones, por lo que se generan subconjuntos de
# # # información

table(is.na(c1_14$P3014),c1_14$nj_1_count)

# # # diagnostico de subset para declarantes con al menos 3 problemas.

sum(c1_14$FEX_C[is.na(c1_14$P3014) == TRUE & c1_14$nj_1_count >= 3])

# # # subset de personas declarantes.

c1_14l2a <- c1_14[which(c1_14$nj_1_count != 0),]; sum(c1_14l2a$FEX_C)

# # # tablas auxiliares con la información de ruta de los 2 problemas priorizados. 
# # # tabla con variables de tipo de problema.

c1_14l2b <- c1_14l2a[c('keyper',names(c1_14l2a)[grep('A1_',names(c1_14l2a))])]

# # # tabla con variables de afectación del problema.

c1_14l2c <- c1_14l2a[c('keyper',names(c1_14l2a)[grep('P3011',names(c1_14l2a))])]

# # # tabla con variables de a?o de inicio del problema.

c1_14l2d <- c1_14l2a[c('keyper',names(c1_14l2a)[grep('P3009S1',names(c1_14l2a))])]

# # # tabla con variables de mes de inicio del problema.

c1_14l2e <- c1_14l2a[c('keyper',names(c1_14l2a)[grep('P3009S2',names(c1_14l2a))])]

# # # reshape por problema.
c1_14l2 <- tidyr::gather(c1_14l2b, var, nj_cuenta,  names(c1_14l2b)[grep('A1_',names(c1_14l2b))], factor_key = F)
c1_14l2 <- c1_14l2[is.na(c1_14l2$nj_cuenta) == FALSE,]

# # # reshape por afectación.
c1_14l3 <- tidyr::gather(c1_14l2c, var, nj_impacto,  names(c1_14l2c)[grep('P3011',names(c1_14l2c))], factor_key = F)
c1_14l3 <- c1_14l3[is.na(c1_14l3$nj_impacto) == FALSE,]

# # # reshape por a?o.
c1_14l4 <- tidyr::gather(c1_14l2d, var, nj_mes,  names(c1_14l2d)[grep('P3009S1',names(c1_14l2d))], factor_key = F)
c1_14l4 <- c1_14l4[is.na(c1_14l4$nj_mes) == FALSE,]

# # # reshape por mes.
c1_14l5 <- tidyr::gather(c1_14l2e, var, nj_ano,  names(c1_14l2e)[grep('P3009S2',names(c1_14l2e))], factor_key = F)
c1_14l5 <- c1_14l5[is.na(c1_14l5$nj_ano) == FALSE,]

# # # codificación de las variables de problema, afectación, a?o y mes para obtener el valor de 
# # # la categor?a a partir de su denominación

c1_14l2$var1 <- c1_14l2$var
c1_14l2$var  <-  gsub('A1','',c1_14l2$var)
c1_14l2$var  <-  gsub('P1670S','',c1_14l2$var)
c1_14l3$var  <-  gsub('P3011_'  ,'',c1_14l3$var)
c1_14l4$var  <-  gsub('P3009S1_','',c1_14l4$var)
c1_14l5$var  <-  gsub('P3009S2_','',c1_14l5$var)

# # # creación de llave seg?n persona y c?digo de problema por categor?a y tipo. 

c1_14l2$key_long   <-   paste0(c1_14l2$keyper,c1_14l2$var)
c1_14l3$key_long   <-   paste0(c1_14l3$keyper,c1_14l3$var)
c1_14l4$key_long   <-   paste0(c1_14l4$keyper,c1_14l4$var)
c1_14l5$key_long   <-   paste0(c1_14l5$keyper,c1_14l5$var)

table(c1_14l3$key_long %in% c1_14l2$key_long); c1_14l3$var    <-   NULL
table(c1_14l4$key_long %in% c1_14l2$key_long); c1_14l4$var    <-   NULL
table(c1_14l5$key_long %in% c1_14l2$key_long); c1_14l5$var    <-   NULL

# # # merge de las tablas en formato long  mediante (key_long).

c1_14l2    <-   merge(c1_14l2, c1_14l3, all.x = TRUE)
c1_14l2    <-   merge(c1_14l2, c1_14l4, all.x = TRUE)
c1_14l2    <-   merge(c1_14l2, c1_14l5, all.x = TRUE)

rm(c1_14l3,c1_14l4,c1_14l5,c1_14l2a,c1_14l2b,c1_14l2c,c1_14l2d,c1_14l2e)

# # # carga etiquetas de problemas y variables.

c1_tlab <- data.frame(openxlsx::read.xlsx(sprintf(a1_pat,
                                                  '1Ea4MZjz5qgbBP7GgTto1_0GbohGFtgeX',
                                                  download.method = 'curl')))

# # # merge de tabla de formato long  mediante descriptor de pregunta y tipo.

c1_14l2 <- merge(c1_14l2,c1_tlab[c('var','P3013')], all.x = T, by.x = 'var1', by.y = 'var' )

c1_14l2$key_long     <-    NULL
c1_14l2$var          <-    NULL
c1_14l2$var1         <-    NULL
c1_14l2$nj_cuenta    <-    NULL

# # # merge de tabla long de problemas con información de marco de declarantes mediante (keyper).

names(c1_14)
c1_15 <-merge(c1_14l2, c1_14[c(
  'keyper',
  'keyhog',
  'nj_1_count',
  'DIRECTORIO',
  'NRO_ENCUESTA',
  'SECUENCIA_ENCUESTA',
  'SECUENCIA_P',
  'ORDEN',
  'FEX_C',
  'DEPMUNI',
  'CIUDADES28',
  'CLASE',
  'P220',
  'P5785',
  'P5501',
  'P1402',
  'P1403',
  'P1365',
  'P1364',
  'P1363',
  'P1987',
  'P1988',
  'P1988S1',
  'P1989',
  'P1990',
  'P6210',
  'P6210S1',
  'P1366',
  'P756',
  'P756S3',
  'P755',
  'P755S3',
  'P753',
  'P753S3',
  'P1662',
  'P6080',            
  'P1906S1',
  'P1906S2',
  'P1906S3',
  'P1906S4',
  'P1906S5',
  'P1906S6',
  'P1906S7',
  'P1906S8',
  'P1906S9',
  'P3038',
  'P3039',
  'menor',
  'menor_count'
)], all.x = T, key = 'keyper')

# * c.3. identificadores de problemas --------------------------------------------------------------

# # # identificador un solo problema - (nj_class1)
c1_15$nj_class1 <- 1
c1_15 <- c1_15[order(c1_15$keyper, -abs(as.numeric(c1_15$keyper) * as.numeric(c1_15$nj_impacto)) ), ]
c1_15$nj_class1[duplicated(c1_15$keyper) == TRUE ] <- 2

# # # identificador de problema en ciclo largo  - (nj_class2)

# # # problemas priorizados caracterizados en (c1_14) según de preguntas de declaración 
# # # (P3013) y (P3014).

c1_14l1a <- c1_14[is.na(c1_14$P3013) == FALSE, c('keyper','P3013')]; c1_14l1a$nj_class2 <- 1
c1_14l1b <- c1_14[is.na(c1_14$P3014) == FALSE, c('keyper','P3014')]; c1_14l1b$nj_class2 <- 1
names(c1_14l1b) <- names(c1_14l1a)

c1_14l1 <- rbind(c1_14l1a,c1_14l1b)
c1_14l1$keyprob <- paste0(c1_14l1$keyper,c1_14l1$P3013)

# # # merge de indicadoras con marco de problemas (c1_15) mediante (keyprob)
c1_15$keyprob <- paste0(c1_15$keyper,c1_15$P3013); table(c1_14l1$keyprob%in%c1_15$keyprob)
c1_15   <-  merge(c1_15, c1_14l1[c('nj_class2','keyprob')], all.x = T)

rm(c1_14l1,c1_14l2,c1_14l1a,c1_14l1b)

# # # problemas priorizados no caracterizados en (c1_14) según conteo de problemas 
# # # declarados (nj_1_count)

c1_15$nj_class2[is.na(c1_15$nj_class2) == TRUE] <- 2
c1_15$nj_class2[which(c1_15$nj_1_count < 3)] <- 1

table(c1_15$nj_class2,c1_15$nj_1_count)

# # # check de totales 

# sum(c1_15$FEX_C)
# sum(c1_15$FEX_C[which(c1_15$nj_class2 == 1) ]) # problemas caracterizados en ciclo largo.
# sum(c1_15$FEX_C[which(c1_15$nj_class1 == 1) ]) # declarantes.
# 
# sum(c1_14$FEX_C[is.na(c1_14$P3013) == FALSE ]) # problemas caracterizados priorizados.

# # # cruce de etiquetas categor?as
c1_15 <- merge(c1_15,c1_tlab[2:6], all.x = TRUE, by = 'P3013')
a2_15 <- merge(a2_15,c1_tlab[2:6], all.x = TRUE, by.y = 'P3013', by.x = 'SECUENCIA_ENCUESTA')
a2_13 <- merge(a2_13,c1_tlab[2:6], all.x = TRUE, by.y = 'P3013', by.x = 'P3154')
table(is.na(a2_15$P3013_cat_laben))
table(is.na(a2_13$P3013_cat_laben))

table(is.na(c1_15$P3013_lab))
table(c1_15$nj_class1, c1_15$nj_1_count)
table(c1_15$nj_class2, c1_15$nj_1_count)

# * * * * tablas de salida de declaración ----------------------------------------------------------

# dt_ga <- doBy::summaryBy(FEX_C ~ P3013_cat_lab + nj_impacto, FUN = sum, data = c1_15)
# dt_gx <- doBy::summaryBy(FEX_C ~ 
#                                  P3013_laben +
#                                  P3013_cat_laben +
#                                  P3039 +
#                                  P3038 +
#                                  P220  +
#                                  P5785 +
#                                  P5501 +
#                                  P1365 +
#                                  P1363 +
#                                  P6210 +
#                                  P6210S1 +
#                                  P1366 +
#                                  P3039 +
#                                  menor +
#                                  menor_count +
#                                  nj_impacto +
#                                  nj_class1 +
#                                  nj_class2, FUN = sum, data = c1_15)
# 
# dt_gy <- doBy::summaryBy(FEX_C ~ 
#                            P3013_laben +
#                            P3013_cat_laben +
#                            P3039 +
#                            P3038 +
#                            P220  +
#                            P5785 +
#                            DEPMUNI +
#                            CIUDADES28 +
#                            CLASE +
#                            nj_impacto +
#                            nj_class1 +
#                            nj_class2, FUN = sum, data = c1_15)
# 
# data.table::fwrite(c1_15,'marco_declaracion_c1_15.csv',sep = ';',dec = ',')
# openxlsx::write.xlsx(dt_gx,'dt_gx.xlsx')
# openxlsx::write.xlsx(dt_gy,'dt_gy.xlsx')
# openxlsx::write.xlsx(c1_15,  'dt_gw.xlsx')
