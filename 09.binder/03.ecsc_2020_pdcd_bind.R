# 0. librerías -------------------------------------------------------------------------------------
options(scipen=999)
Sys.setenv(http_proxy='http://staff-proxy.ul.ie:8080')
Sys.getenv("HTTPS_PROXY")

library('tidyr')
library('plyr')
library('ggplot2')
library('viridis')
library('dplyr')
library('forcats')
library('hrbrthemes')
library('data.table')
library('curl')
library('readxl')
library('foreign')

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
# # # están codificadas en las tablas a2_13 y a2_15.

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

# d. Rutas de atención a problemas -----------------------------------------------------------------

#  * d.1. Ruta  de ciclo largo priorizado ----------------------------------------------------------

# # # subset de variables de ruta problemas priorizados caracterizados de orden 1 en (a2_14).
d1_14_1 <- as.data.frame(a2_14[is.na(a2_14$P3013) == FALSE,])

# # # carga de lista de variables (a2_14).
d1_14_1_v <- data.frame(openxlsx::read.xlsx(sprintf(a1_pat,
                                                    '1086VUBxukZks-Sn47GlQLgc_LmhlD7SQ',
                                                    download.method = 'curl'),sheet = 1))
d1_14_1 <- d1_14_1[c('DIRECTORIO',
                     'NRO_ENCUESTA',
                     'SECUENCIA_ENCUESTA',
                     'SECUENCIA_P',
                     'ORDEN',
                     'keyper',
                     'FEX_C',
                     'P3013',d1_14_1_v$vars)
]

# # # subset de variables de ruta problemas priorizados caracterizados de  orden 2 en (a2_14).
d1_14_2 <- as.data.frame(a2_14[is.na(a2_14$P3014) == FALSE,])

# # # carga de lista de variables (a2_14).
d1_14_2_v <- data.frame(openxlsx::read.xlsx(sprintf(a1_pat, 
                                                    '1086VUBxukZks-Sn47GlQLgc_LmhlD7SQ',
                                                    download.method = 'curl'),sheet = 2))
d1_14_2 <- d1_14_2[c('DIRECTORIO',
                     'NRO_ENCUESTA',
                     'SECUENCIA_ENCUESTA',
                     'SECUENCIA_P',
                     'ORDEN',
                     'keyper',
                     'FEX_C',
                     'P3014',d1_14_2_v$vars)
]
rm(d1_14_1_v,d1_14_2_v)

# # # append de tablas de ciclo largo y unificación de variables según orden (1 y 2)

names(d1_14_2) <- gsub('_2','',names(d1_14_2))
names(d1_14_1) <- gsub('_1','',names(d1_14_1))
names(d1_14_2)[names(d1_14_2) =='P3014'] <- 'P3013'

d1_14 <- rbind(d1_14_1,d1_14_2) 

# # #  diferencia de problemas priorizados declarados en el marco y caracterizados en la ruta.

length(unique(c1_15$keyper[which(c1_15$nj_1_count >= 3 )])) - nrow(d1_14)/2

#  * *  d.1.1. instituciones -----------------------------------------------------------------------

# # # las tablas de ciclo largo, en su pregunta, (P1674), no identifican de manera plena la ultima
# # # institución visitada. Esta variable solo tiene registro para el caso de los problemas a los
# # # que para su solución se acudió a más de una institución. Los problemas asociados a una
# # # única institución están agrupados en formato ancho en las variables (P1673S).

# * * *  d.1.1.1. ciclo largo priorizado problemas de orden 1 --------------------------------------

# # # subset de variables de ruta problemas priorizados caracterizados de orden 1 en (a2_14).
d1_14_1_ins <- as.data.frame(a2_14[is.na(a2_14$P3013) == FALSE,])

# # # solo ruta institucional del problema priorizado de orden uno.
d1_14_1_ins <- d1_14_1_ins[d1_14_1_ins$P1672_1 == 1,]
d1_14_1_ins <- d1_14_1_ins[,c('keyper',
                              names(d1_14_1_ins)[grep('P1673S', names(d1_14_1_ins))],
                              'P3013','P1674_1','FEX_C')]

# # # retira instituciones del problema caracterizado de orden 2
d1_14_1_ins <- d1_14_1_ins[, -grep('_2', colnames(d1_14_1_ins))]

d1_14_1_ins$ins_count <- rowSums(d1_14_1_ins[,grep('P1673S', names(d1_14_1_ins))],na.rm = T)

d1_14_1_ins <- tidyr::gather(d1_14_1_ins, var, insti,  
                             names(d1_14_1_ins)[grep('P1673S',names(d1_14_1_ins))], factor_key = F)

d1_14_1_ins <- d1_14_1_ins [is.na(d1_14_1_ins$insti) == FALSE,]; d1_14_1_ins$insti <- NULL

# # # solo hay (nas) en las personas que acudieron a una ?nica institución.
table(d1_14_1_ins$ins_count,is.na(d1_14_1_ins$P1674_1))

d1_14_1_ins$var  <-  gsub('P1673S','',d1_14_1_ins$var)
d1_14_1_ins$var  <-  gsub('_1','',d1_14_1_ins$var)

d1_14_1_ins$P1674_1c <- d1_14_1_ins$P1674_1
d1_14_1_ins$P1674_1c[is.na(d1_14_1_ins$P1674_1)] <- d1_14_1_ins$var[is.na(d1_14_1_ins$P1674_1)]

# * * *  d.1.1.2. ciclo largo priorizado problemas de orden 2 --------------------------------------

# # # subset de variables de ruta problemas priorizados caracterizados de orden 1 en (a2_14).
d1_14_2_ins <- as.data.frame(a2_14[is.na(a2_14$P3014) == FALSE,])

# # # solo ruta institucional del problema priorizado de orden uno.
d1_14_2_ins <- d1_14_2_ins[d1_14_2_ins$P1672_2 == 1,]
d1_14_2_ins <- d1_14_2_ins[,c('keyper',
                              names(d1_14_2_ins)[grep('P1673S', names(d1_14_2_ins))],
                              'P3014','P1674_2','FEX_C')]

# # # retira instituciones del problema caracterizado de orden 1
d1_14_2_ins <- d1_14_2_ins[, -grep('_1', colnames(d1_14_2_ins))]

d1_14_2_ins$ins_count <- rowSums(d1_14_2_ins[,grep('P1673S', names(d1_14_2_ins))],na.rm = T)

d1_14_2_ins <- tidyr::gather(d1_14_2_ins, var, insti,  
                             names(d1_14_2_ins)[grep('P1673S',names(d1_14_2_ins))], factor_key = F)

d1_14_2_ins <- d1_14_2_ins [is.na(d1_14_2_ins$insti) == FALSE,]; d1_14_2_ins$insti <- NULL

# # # solo hay (nas) en las personas que acudieron a una ?nica institución.
table(d1_14_2_ins$ins_count,is.na(d1_14_2_ins$P1674_2))

d1_14_2_ins$var  <-  gsub('P1673S','',d1_14_2_ins$var)
d1_14_2_ins$var  <-  gsub('_2','',d1_14_2_ins$var)

d1_14_2_ins$P1674_2c <- d1_14_2_ins$P1674_2
d1_14_2_ins$P1674_2c[is.na(d1_14_2_ins$P1674_2)] <- d1_14_2_ins$var[is.na(d1_14_2_ins$P1674_2)]

# * * *  d.1.1.3. ciclo marco de instituciones por problema ----------------------------------------

d1_14_1_ins$keyproi <- paste0(d1_14_1_ins$keyper,d1_14_1_ins$P3013)
d1_14_2_ins$keyproi <- paste0(d1_14_2_ins$keyper,d1_14_2_ins$P3014)

table(duplicated(d1_14_1_ins$keyproi))
table(duplicated(d1_14_2_ins$keyproi))

# table(is.na(d1_14_2_ins$P1674_2[duplicated(d1_14_2_ins$keyproi) == T]))

names(d1_14_1_ins) <- c("keyper","P3013","P1674","FEX_C","ins_count","var","P1674_c","keyproi")
names(d1_14_2_ins) <- names(d1_14_1_ins)

d1_14_ins <- rbind(d1_14_1_ins,d1_14_2_ins)
rm(d1_14_1_ins,d1_14_2_ins)

# # # retira duplicados para identificar ?ltima institución.

d1_14_ins <- d1_14_ins[!duplicated(d1_14_ins$keyproi),]

# # # merge con marco de ciclo largo de problemas priorizados

d1_14$keyproi <- paste0(d1_14$keyper,d1_14$P3013)
table(d1_14$keyproi[d1_14$P1672 == 1] %in% d1_14_ins$keyproi)

d1_14 <- merge(d1_14,d1_14_ins[c('keyproi','P1674_c','ins_count')], by = 'keyproi', all.x = T)
table(is.na(d1_14$P1674_c),d1_14$P1672)

d1_14$keyproi <- NULL
d1_14$keyper <- NULL
names(d1_14)

table(is.na(d1_14$P1674[d1_14$P1672 == 1]))
table(is.na(d1_14$P1674_c[d1_14$P1672 == 1]))

d1_14$P1674 <- NULL
d1_14$P1674 <- d1_14$P1674_c

d1_14$P1674_c <- NULL

# * d.2. Ruta  de ciclo largo no priorizado --------------------------------------------------------

# # # carga de lista de variables (a2_15) ciclo largo no priorizado.
d2_13_v <- data.frame(openxlsx::read.xlsx(sprintf(a1_pat, 
                                                  '1086VUBxukZks-Sn47GlQLgc_LmhlD7SQ',
                                                  download.method = 'curl'),sheet = 3))
d2_13 <- as.data.frame(a2_13) 
d2_13 <- d2_13[c('DIRECTORIO',
                 'NRO_ENCUESTA',
                 'SECUENCIA_ENCUESTA',
                 'SECUENCIA_P',
                 'ORDEN',
                 'FEX_C',
                 'HOG',
                 'P3154',d2_13_v$vars)
]

# # # unificación de variables seg?n orden (1 y 2)
rm(d2_13_v)
length((c1_15$keyper[which(c1_15$nj_1_count < 3 )]))
names(d2_13)[names(d2_13) =='P3154'] <- 'P3013'

# * * *  d.2.1. instituciones ----------------------------------------------------------------------

# # # solo ruta institucional 
d2_13_ins <- data.frame(a2_13[a2_13$P1672 == 1,])

d2_13_ins$keyper <- paste0(
  d2_13_ins$DIRECTORIO,
  d2_13_ins$NRO_ENCUESTA,
  d2_13_ins$SECUENCIA_P,
  d2_13_ins$ORDEN
)

d2_13_ins <- d2_13_ins[,c('keyper',names(d2_13_ins)[grep('P1673S', names(d2_13_ins))],
                          'P3154','P1674','FEX_C')]

d2_13_ins$ins_count <- rowSums(d2_13_ins[,grep('P1673S', names(d2_13_ins))],na.rm = T)

d2_13_ins <- tidyr::gather(d2_13_ins , var, insti,  
                           names(d2_13_ins)[grep('P1673S',names(d2_13_ins))], factor_key = F)

d2_13_ins <- d2_13_ins[is.na(d2_13_ins$insti) == FALSE,]; d2_13_ins$insti <- NULL

# # # solo hay (nas) en las personas que acudieron a una única institución.
table(d2_13_ins$ins_count,is.na(d2_13_ins$P1674))

d2_13_ins$var  <-  gsub('P1673S','',d2_13_ins$var)

d2_13_ins$P1674_c <- d2_13_ins$P1674
d2_13_ins$P1674_c[is.na(d2_13_ins$P1674)] <- d2_13_ins$var[is.na(d2_13_ins$P1674)]

# # # retira duplicados para identificar última institución.

d2_13_ins$keyproi <- paste0(d2_13_ins$keyper,d2_13_ins$P3154)

table(duplicated(d2_13_ins$keyproi))
d2_13_ins <- d2_13_ins[!duplicated(d2_13_ins$keyproi),]

# # # merge con marco de ciclo largo de problemas priorizados

d2_13$keyproi <- paste0(
  d2_13$DIRECTORIO,
  d2_13$NRO_ENCUESTA,
  d2_13$SECUENCIA_P,
  d2_13$ORDEN,
  d2_13$P3013
) 

table(d2_13$keyproi[d2_13$P1672 == 1] %in% d2_13_ins$keyproi)
table(d2_13_ins$keyproi %in% d2_13$keyproi[d2_13$P1672 == 1])

d2_13 <- merge(d2_13,d2_13_ins[c('keyproi','P1674_c','ins_count')], by = 'keyproi', all.x = T)
table(is.na(d2_13$P1674_c),d2_13$P1672)

d2_13$keyproi <- NULL

table(is.na(d2_13$P1674[d2_13$P1672 == 1]))
table(is.na(d2_13$P1674_c[d2_13$P1672 == 1]))

d2_13$P1674 <- NULL
d2_13$P1674 <- d2_13$P1674_c
d2_13$P1674_c <- NULL

# * d.3. Ruta  de ciclo corto ----------------------------------------------------------------------

# # # carga de lista de variables (a2_15) ciclo corto.
d3_15_v <- data.frame(openxlsx::read.xlsx(sprintf(a1_pat, 
                                                  '1086VUBxukZks-Sn47GlQLgc_LmhlD7SQ',
                                                  download.method = 'curl'),sheet = 4))
d3_15 <- as.data.frame(a2_15) 
d3_15 <- d3_15[c('DIRECTORIO',
                 'NRO_ENCUESTA',
                 'SECUENCIA_ENCUESTA',
                 'SECUENCIA_P',
                 'ORDEN',
                 'FEX_C',
                 'HOG',
                 d3_15_v$vars)
]

d3_15$P3013 <- d3_15$SECUENCIA_ENCUESTA 
d3_15$SECUENCIA_ENCUESTA <- NULL

rm(d3_15_v)
names(d3_15)

# # # unificación de variables seg?n orden (1 y 2)

names(d3_15)[names(d3_15) =='P1693'] <- 'P1672'
names(d3_15)[names(d3_15) =='P1695'] <- 'P1674'
names(d3_15)[names(d3_15) =='P1696'] <- 'P1685'

# * * *  d.3.1. instituciones ----------------------------------------------------------------------

# # # solo ruta institucional 
d3_15_ins <- data.frame(a2_15[a2_15$P1693 == 1,])

d3_15_ins$keyper <- paste0(
  d3_15_ins$DIRECTORIO,
  d3_15_ins$NRO_ENCUESTA,
  d3_15_ins$SECUENCIA_P,
  d3_15_ins$ORDEN
)

d3_15_ins <- d3_15_ins[,c('keyper',names(d3_15_ins)[grep('P1694S', names(d3_15_ins))],
                          'SECUENCIA_ENCUESTA','P1695','FEX_C')]

d3_15_ins$ins_count <- rowSums(d3_15_ins[,grep('P1694S', names(d3_15_ins))],na.rm = T)

d3_15_ins <- tidyr::gather(d3_15_ins , var, insti,  
                           names(d3_15_ins)[grep('P1694S',names(d3_15_ins))], factor_key = F)

d3_15_ins <- d3_15_ins[is.na(d3_15_ins$insti) == FALSE,]; d3_15_ins$insti <- NULL

# # # solo hay (nas) en las personas que acudieron a una única institución.
table(d3_15_ins$ins_count,is.na(d3_15_ins$P1695))

d3_15_ins$var  <-  gsub('P1694S','',d3_15_ins$var)

d3_15_ins$P1674_c <- d3_15_ins$P1695
d3_15_ins$P1674_c[is.na(d3_15_ins$P1695)] <- d3_15_ins$var[is.na(d3_15_ins$P1695)]

# # # retira duplicados para identificar última institución.

d3_15_ins$keyproi <- paste0(d3_15_ins$keyper,d3_15_ins$SECUENCIA_ENCUESTA)

table(duplicated(d3_15_ins$keyproi))
d3_15_ins <- d3_15_ins[!duplicated(d3_15_ins$keyproi),]

# # # merge con marco de ciclo largo de problemas priorizados

d3_15$keyproi <- paste0(
  d3_15$DIRECTORIO,
  d3_15$NRO_ENCUESTA,
  d3_15$SECUENCIA_P,
  d3_15$ORDEN,
  d3_15$P3013
) 

table(d3_15$keyproi[d3_15$P1672 == 1] %in% d3_15_ins$keyproi)
table(d3_15_ins$keyproi %in% d3_15$keyproi[d3_15$P1672 == 1])

d3_15 <- merge(d3_15,d3_15_ins[c('keyproi','P1674_c','ins_count')], by = 'keyproi', all.x = T)
table(is.na(d3_15$P1674_c),d3_15$P1672)

d3_15$keyproi <- NULL

table(is.na(d3_15$P1674[d3_15$P1672 == 1]))
table(is.na(d3_15$P1674_c[d3_15$P1672 == 1]))

d3_15$P1674 <- NULL
d3_15$P1674 <- d3_15$P1674_c
d3_15$P1674_c <- NULL

# * d.4. Rutas por ciclo  --------------------------------------------------------------------------
# # # para identificar las rutas en cada uno de los 3 tipos de ciclo es necesario utilizar el
# # # una llave común al marco de problemas declarados (c1_15). Estas llaves no son homogéneas
# # # y la codificación de sus variables cambia, en especial en las tablas de ciclo corto y 
# # # ciclo largo no priorizado.

names(d1_14)
names(d2_13)
names(d3_15)

table(is.na(d1_14$P1674[d1_14$P1672 == 1]))
table(is.na(d2_13$P1674[d2_13$P1672 == 1]))
table(is.na(d3_15$P1674[d3_15$P1672 == 1]))

# * * * * ciclo largo sin priorizar en marco de problemas ------------------------------------------

# # # marco de declaración
c1_15$keyprob_clsinp <- paste0(c1_15$P3013,
                               c1_15$DIRECTORIO,
                               c1_15$SECUENCIA_P,
                               c1_15$ORDEN
) 

# # # ciclo largo sin priorizar
d2_13$keyprob_clsinp <- paste0(d2_13$P3013,
                               d2_13$DIRECTORIO,
                               d2_13$HOG,
                               d2_13$SECUENCIA_P) 

table(duplicated(d2_13$keyprob_clsinp)); table(duplicated(c1_15$keyprob_clsinp))

# View(d2_13[duplicated(d2_13$keyprob) == TRUE, ] )
# View(d2_13[(d2_13$keyprob %in% c1_15$keyprob) == FALSE, ] )

table(d2_13$keyprob_clsinp %in% c1_15$keyprob_clsinp)
names(d2_13);d2_13$keyprob <- NULL 

# merge con marco de declaración mediante (keyprob_clsinp)
d4_13 <- merge(d2_13[,9:30],c1_15,all.x = T, by = 'keyprob_clsinp')
table(is.na(d4_13$P3013))
names(d4_13)

table(is.na(d4_13$P1674[d4_13$P1672 == 1]))

# * * * * ciclo largo con priorizar en marco de problemas ------------------------------------------

# # # marco de declaración
c1_15$keyprob_clconp <- paste0(c1_15$P3013,
                               c1_15$DIRECTORIO,
                               c1_15$SECUENCIA_P,
                               c1_15$ORDEN
) 

# # # ciclo largo priorizado
d1_14$keyprob_clconp <- paste0(d1_14$P3013,
                               d1_14$DIRECTORIO,
                               d1_14$SECUENCIA_P,
                               d1_14$ORDEN) 

table(duplicated(d1_14$keyprob_clconp));table(duplicated(c1_15$keyprob_clconp))
table(d1_14$keyprob_clconp %in% c1_15$keyprob_clconp)
names(d1_14)

# merge con marco de declaración mediante (keyprob_clconp)
d4_14 <- merge(d1_14[,8:29],c1_15,all.x = T, by = 'keyprob_clconp')
table(is.na(d4_14$P3013))
d4_14$keyprob_clsinp <- NULL
d4_14$keyprob_clconp <- NULL

table(is.na(d4_14$P1674[d4_14$P1672 == 1]))

# * * * * ciclo corto con marco de problemas -------------------------------------------------------

# # # marco de declaración
c1_15$keyprob_cc <- paste0(c1_15$P3013,
                           c1_15$DIRECTORIO,
                           c1_15$SECUENCIA_P,
                           c1_15$ORDEN
) 

# # # ciclo corto
d3_15$keyprob_cc <- paste0(d3_15$P3013,
                           d3_15$DIRECTORIO,
                           d3_15$HOG,
                           d3_15$SECUENCIA_P) 

table(duplicated(d3_15$keyprob_cc)); table(duplicated(c1_15$keyprob_cc))

table(d3_15$keyprob_cc %in% c1_15$keyprob_cc)
d3_15$P3013 <- NULL 

names(d3_15)
# # #  merge con marco de declaración mediante (keyprob_cc)
d4_15 <- merge(d3_15[,7:11],c1_15,all.x = T, by = 'keyprob_cc')
table(is.na(d4_15$P3013))

table(is.na(d4_14$P1674[d4_14$P1672 == 1]))

d4_15$keyprob_clsinp <- NULL
d4_15$keyprob_clconp <- NULL

# e. Marco de rutas de atención a problemas --------------------------------------------------------
# * e.1. append de ciclos por ruta -----------------------------------------------------------------

# # # en el append, las variables de caracterización de ruta no están disponibles para los problemas
# # # declarados en ciclo corto.

names(d4_14) %in% names(d4_13)
names(d4_14) %in% names(d4_15)
names(d4_13) %in% names(d4_15)

e1_15 <- rbind.fill(d4_13,d4_14,d4_15) 

table(is.na(e1_15$P1674[e1_15$P1672 == 1]))

# # # check de totales
# # # se evidencia la discrepancia entre la tabla de declaración (c1_15) y la de caracterización
# # # (e1_15).

sum(e1_15$FEX_C)
sum(e1_15$FEX_C[e1_15$nj_class1 == 1])
sum(c1_15$FEX_C[c1_15$nj_class1 == 1])

table(c1_15$keyper %in% e1_15$keyper); table(e1_15$keyper %in% c1_15$keyper)

# # # variable de numero de problemas caracterizados por individuo.
e1_15 <- e1_15 %>% group_by(keyper) %>% mutate(nj_2_count = n())

# # # check de totales
# e1_15[e1_15$nj_class2 == 1,] %>% group_by(P1672) %>% summarise(Suma = sum(FEX_C))
# e1_15[e1_15$nj_class2 == 1,] %>% group_by(P1672, P220,DEPMUNI) %>% summarise(Suma = sum(FEX_C))
# e1_15 %>% group_by(P1672) %>% summarise(Suma = sum(FEX_C))

table(e1_15$P1685);table(is.na(e1_15$P1685))

# # # para la construcción de los diferentes indicadores se inicia con una tabla del marco de 
# # # problemas caracterizados que incorpore solo los problemas sobre los que existe información
# # # de su solución, es decir que los niveles de la pregunta (P1685) sean diferentes de (9)

sum(e1_15$FEX_C)
sum(e1_15$FEX_C[!duplicated(e1_15$keyper)])

e2_15 <- e1_15[e1_15$P1685 != 9,]
e2_15$P1685[e2_15$P1685 == 2] <- 0

# # # exporta marcos de caracterización 
# openxlsx::write.xlsx(e2_15,'e2_15.xlsx')
# openxlsx::write.xlsx(e1_15,'e1_15.xlsx')

# # # Se construiran tres tipos de indicadores:

# # # 1. problemas                       - el total de problemas plenamente caracterizados
# # # 2. necesidades jurídicas generales - problemas afrontados sin ruta de acuerdo directo
# # # 3. necesidades jurídicas estrictas - problemas afrontados sin razones de las rutas:
# # #                                                       3.1. acuerdo directo
# # #                                                       3.2. acción violenta
# # #                                                       3.1. inacción

# # # Para cada grupo de indicadores se tomaran consideraciones adicionales:

# # # a. Marcos de caracterización de ciclo largo
# # # b. Marcos de caracterización de ciclo largo y corto

# * e.2. indicadores de  problemas declarados ------------------------------------------------------

# * * * * problemas ciclo largo -- tabla de personas -----------------------------------------------

sum(e2_15$FEX_C[!duplicated(e2_15$keyper)& e2_15$nj_class2 == 1])
e2_i1aper <- e2_15[e2_15$nj_class2 == 1,] %>% group_by(keyper) %>%
  summarise(
    fexc_individual = round(mean(FEX_C),6),
    impacto_medio   = mean(nj_impacto),
    problemas1      = mean(nj_1_count),
    problemas2      = mean(nj_2_count),
    satisfechos     = sum (P1685))

table(e2_i1aper$problemas1 >= e2_i1aper$satisfechos)
table(e2_i1aper$problemas2 >= e2_i1aper$satisfechos) 

e2_i1aper$satisfechos_min <-(e2_i1aper$satisfechos/e2_i1aper$satisfechos)
e2_i1aper$satisfechos_min[is.na(e2_i1aper$satisfechos_min) == TRUE] <-  0

#02a
sum(e2_i1aper$fexc_individual)
#02b
mean(e2_i1aper$satisfechos/e2_i1aper$problemas2)

# * * * * problemas ciclo largo -- tabla de problemas ----------------------------------------------
e2_i1apro <- e2_15[e2_15$nj_class2 == 1,]  %>% group_by(P1685) %>%
  summarise(
    fexc_individual = round(sum(FEX_C),6))

#02c
sum(e2_i1apro$fexc_individual)

#02d
sum(e2_i1apro$fexc_individual[e2_i1apro$P1685 == 1]) / sum(e2_i1apro$fexc_individual)

# * * * * problemas ciclo largo + corto -- tabla de personas ---------------------------------------
e2_i1bper <- e2_15 %>% group_by(keyper) %>%
  summarise(
    fexc_individual = round(mean(FEX_C),6),
    impacto_medio   = mean(nj_impacto),
    problemas1       = mean(nj_1_count),
    problemas2       = mean(nj_2_count),
    satisfechos     = sum (P1685))

table(e2_i1bper$problemas1 >= e2_i1bper$satisfechos) 
table(e2_i1bper$problemas2 >= e2_i1bper$satisfechos) 

e2_i1bper$satisfechos_min <-(e2_i1bper$satisfechos/e2_i1bper$satisfechos)
e2_i1bper$satisfechos_min[is.na(e2_i1bper$satisfechos_min) == TRUE] <-  0

sum(e2_i1bper$satisfechos_min * e2_i1bper$fexc_individual) / sum(e2_i1bper$fexc_individual)

#02e
sum(e2_i1bper$fexc_individual)
#02f
mean(e2_i1bper$satisfechos/e2_i1bper$problemas2)

# * * * * problemas ciclo largo + corto -- tabla de problemas --------------------------------------
e2_i1bpro <- e2_15 %>% group_by(P1685) %>%
  summarise(
    fexc_individual = round(sum(FEX_C),6))

#02g
sum(e2_i1bpro$fexc_individual)

#02h
sum(e2_i1bpro$fexc_individual[e2_i1bpro$P1685 == 1]) / sum(e2_i1bpro$fexc_individual)

# * e.3. indicadores de necesidades jurídicas generales --------------------------------------------
table(e1_15$P1685);table(is.na(e1_15$P1685))
e3_15 <- e1_15[e1_15$P1685 != 9,]
e3_15 <- e3_15[e3_15$P1672 != 2,]
e3_15$P1685[e3_15$P1685 == 2] <- 0

# * * * * nj generales ciclo largo -- tabla de personas --------------------------------------------
e3_i1aper <- e3_15[e3_15$nj_class2 == 1,] %>% group_by(keyper) %>%
  summarise(
    fexc_individual = round(mean(FEX_C),6),
    impacto_medio   = mean(nj_impacto),
    problemas1      = mean(nj_1_count),
    problemas2      = mean(nj_2_count),
    satisfechos     = sum (P1685))

table(e3_i1aper$problemas1 >= e3_i1aper$satisfechos) 
table(e3_i1aper$problemas2 >= e3_i1aper$satisfechos) 

e3_i1aper$satisfechos_min <-(e3_i1aper$satisfechos/e3_i1aper$satisfechos)
e3_i1aper$satisfechos_min[is.na(e3_i1aper$satisfechos_min) == TRUE] <-  0

#03a
sum(e3_i1aper$fexc_individual)
#03b
mean(e3_i1aper$satisfechos/e3_i1aper$problemas2)

# * * * * nj generales ciclo largo -- tabla de problemas ------------------------------------------

e3_i1apro <- e3_15[e3_15$nj_class2 == 1,]  %>% group_by(P1685) %>%
  summarise(
    fexc_individual = round(sum(FEX_C),6))

#03c
sum(e3_i1apro$fexc_individual)

#03d
sum(e3_i1apro$fexc_individual[e3_i1apro$P1685 == 1]) / sum(e3_i1apro$fexc_individual)

# * * * * nj ciclo largo + corto -- tabla de personas ---------------------------------------
e3_i1bper <- e3_15 %>% group_by(keyper) %>%
  summarise(
    fexc_individual = round(mean(FEX_C),6),
    impacto_medio   = mean(nj_impacto),
    problemas1       = mean(nj_1_count),
    problemas2       = mean(nj_2_count),
    satisfechos     = sum (P1685))

table(e3_i1bper$problemas1 >= e3_i1bper$satisfechos) 
table(e3_i1bper$problemas2 >= e3_i1bper$satisfechos) 

e3_i1bper$satisfechos_min <-(e3_i1bper$satisfechos/e3_i1bper$satisfechos)
e3_i1bper$satisfechos_min[is.na(e3_i1bper$satisfechos_min) == TRUE] <-  0

sum(e3_i1bper$satisfechos_min * e3_i1bper$fexc_individual) / sum(e3_i1bper$fexc_individual)

#03e
sum(e3_i1bper$fexc_individual)
#03f
mean(e3_i1bper$satisfechos/e3_i1bper$problemas2)

# * * * * nj ciclo largo + corto -- tabla de problemas --------------------------------------
e3_i1bpro <- e3_15 %>% group_by(P1685) %>%
  summarise(
    fexc_individual = round(sum(FEX_C),6))

#03g
sum(e3_i1bpro$fexc_individual)

#03h
sum(e3_i1bpro$fexc_individual[e3_i1bpro$P1685 == 1]) / sum(e3_i1bpro$fexc_individual)

# * e.4. indicadores de necesidades jurídicas estrictas --------------------------------------------
table(e1_15$P1685);table(is.na(e1_15$P1685))
e4_15 <- data.frame(e1_15[e1_15$P1685 != 9,]) # registro de solución
e4_15$P1685[e4_15$P1685 == 2] <- 0


table(e4_15$P1679); class(e4_15$P1679);table(is.na(e4_15$P1679))
e4_15$P1679[is.na(e4_15$P1679) == TRUE ] <- 0

e4_15 <- e4_15[which(e4_15$P1679 != 1),]  # prefiere arreglar pacíficamente, a través de diálogo o por sí mismo los problemas
e4_15 <- e4_15[which(e4_15$P1679 != 2),]  # es menos costoso o más ágil que otras soluciones.
e4_15 <- e4_15[which(e4_15$P1679 != 3),]  # el acuerdo dura más y los resultados son más beneficiosos
e4_15 <- e4_15[which(e4_15$P1679 != 4),]  # hace parte de sus costumbres, usos o tradiciones
e4_15 <- e4_15[which(e4_15$P1679 != 10),] # Porque el problema no fue tan grave.

# 5 Se lo sugirieron.
# 6 Lo presionaron.
# 7 No sabía ante cual autoridad acudir, qué hacer o cómo hacerlo.
# 8 Donde vive no cuenta con instituciones que solucionen conflictos.
# 9	No confía en las autoridades.

table(e4_15$P1681)
e4_15$P1681[is.na(e4_15$P1681) == TRUE ] <- 0

e4_15 <- e4_15[which(e4_15$P1681 != 1),]  # tenía mucha rabia, se dejó llevar, el otro se lo merecía.
e4_15 <- e4_15[which(e4_15$P1681 != 3),]  # es la forma como se resuelven los problemas aquí
e4_15 <- e4_15[which(e4_15$P1681 != 4),]  # no había otra opción, estaba en estado de necesidad (hambre de un menor, salud de una persona)
e4_15 <- e4_15[which(e4_15$P1681 != 8),]  # Otro

# 2	No confiaba en las autoridades.
# 5	No sabía ante quien acudir, qué hacer o cómo hacerlo.
# 6	Se lo sugirieron.
# 7	Lo presionaron.

table(e4_15$P1683)
e4_15$P1683[is.na(e4_15$P1683) == TRUE ] <- 0
e4_15 <- e4_15[which(e4_15$P1683 != 1),]

# * * * * nj estrictas ciclo largo -- tabla de personas --------------------------------------------
e4_i1aper <- e4_15[e4_15$nj_class2 == 1,] %>% group_by(keyper) %>%
  summarise(
    fexc_individual = round(mean(FEX_C),6),
    impacto_medio   = mean(nj_impacto),
    problemas1      = mean(nj_1_count),
    problemas2      = mean(nj_2_count),
    satisfechos     = sum (P1685))

table(e4_i1aper$problemas1 >= e4_i1aper$satisfechos) 
table(e4_i1aper$problemas2 >= e4_i1aper$satisfechos) 

e4_i1aper$satisfechos_min <-(e4_i1aper$satisfechos/e4_i1aper$satisfechos)
e4_i1aper$satisfechos_min[is.na(e4_i1aper$satisfechos_min) == TRUE] <-  0

#04a
sum(e4_i1aper$fexc_individual)
#04b
mean(e4_i1aper$satisfechos/e4_i1aper$problemas2)


# * * * * nj generales ciclo largo -- tabla de problemas ------------------------------------------

e4_i1apro <- e4_15[e4_15$nj_class2 == 1,]  %>% group_by(P1685) %>%
  summarise(
    fexc_individual = round(sum(FEX_C),6))

#04c
sum(e4_i1apro$fexc_individual)

#04d
sum(e4_i1apro$fexc_individual[e4_i1apro$P1685 == 1]) / sum(e4_i1apro$fexc_individual)

