Sys.setlocale(locale="es_ES.UTF-8")

dt_fil  <- '1y9ATpg7NrwqitwMEm5BbXvnLWlb8Xyth'
dt_pat  <- 'https://docs.google.com/uc?id=%s&export=download'
dt_tab  <- data.frame(openxlsx::read.xlsx(sprintf(dt_pat, dt_fil,download.method = 'curl')))

# a. define marcos --------------------------------------------------------------------------------
# * a.1 necesidades jurídicas ---------------------------------------------------------------------
dt_ln <- dt_tab[dt_tab$P1685 != 9,] # reporte de soución caracterizado
dt_ln <- dt_ln[dt_ln$P1672 != 2,]   # excluye ruta de acuerdo directo

dt_ln$P1685[dt_ln$P1685 == 2] <- 0
#sum(dt_ln$FEX_C)

# * a.2 problemas justiciables ---------------------------------------------------------------------
dt_lp <- dt_tab[dt_tab$P1685 != 9,] # reporte de soución caracterizado
#sum(dt_lp$FEX_C)


# b. boostrap para ECV -----------------------------------------------------------------------------
# * b.1 necesidades jurídicas grupo nacional + cat de problemas ------------------------------------

# estratos sobre los que se desarrollará el muestreo

stra <- unique(dt_ln$P3013_cat_lab)

# muestrea aleatoria del 40% (adhoc) del total de unidades en cada grupo de problema
# construcción de media y desviación en un vector de salida para cada agrupación
# imputación de cada grupo estimado junto con las estimacipnes puntuales de los fexc

boon  <- 1000
dtout <- data.frame(problema = NA, hatmu = NA, hatsd = NA, hatcv = NA )
lismu <- list()

for( j in 1:length(stra)){
x <- dt_ln[dt_ln$P3013_cat_lab == stra[j],]
x$id <- 1:nrow(x)

samn <- round(nrow(x)*.4)
mu <- NA

for( i in 1:boon){
  
  xs <- x[sample(x$id,samn,replace = T),]
  mu0 <- sum(xs$FEX_C,na.rm = T)
  mu <- c(mu,mu0)
  mu <- mu[complete.cases(mu)]
}

lismu[[j]] <- data.frame(mu)

hatmu <- round(mean(mu,na.rm = T),4)
hatsd <- round(sd(mu, na.rm = T),4)
hatcv <- round((hatsd/hatmu) * 100,4)

row_out <- c(stra[j],hatmu,hatsd,hatcv)
dtout <- rbind(dtout,row_out)
dtout <- dtout[!is.na(dtout$problema),]
}

# ** b.1.1 estimación punto ------------------------------------------------------------------------

dtout_1 <- doBy::summary_by(FEX_C  ~ P3013_cat_lab, data = dt_ln, FUN = sum )
dtout_2 <- merge(dtout,dtout_1, all.x = T, by.y = 'P3013_cat_lab', by.x = 'problema')
openxlsx::write.xlsx(dtout_2,'nj_prob.xlsx')

# * b.2 problemas grupo nacional + cat de problemas ------------------------------------------------

# estratos sobre los que se desarrollará el muestreo

stra <- unique(dt_ln$P3013_cat_lab)

# muestrea aleatoria del 40% (adhoc) del total de unidades en cada grupo de problema
# construcción de media y desviación en un vector de salida para cada agrupación
# imputación de cada grupo estimado junto con las estimacipnes puntuales de los fexc

boon  <- 1000
dtoutb <- data.frame(problema = NA, hatmu = NA, hatsd = NA, hatcv = NA )
lismub <- list()

for( j in 1:length(stra)){
  x <- dt_lp[dt_lp$P3013_cat_lab == stra[j],]
  x$id <- 1:nrow(x)
  
  samn <- round(nrow(x)*.4)
  mu <- NA
  
  for( i in 1:boon){
    
    xs <- x[sample(x$id,samn,replace = T),]
    mu0 <- sum(xs$FEX_C,na.rm = T)
    mu <- c(mu,mu0)
    mu <- mu[complete.cases(mu)]
  }
  
  lismub[[j]] <- data.frame(mu)
  
  hatmu <- round(mean(mu,na.rm = T),4)
  hatsd <- round(sd(mu, na.rm = T),4)
  hatcv <- round((hatsd/hatmu) * 100,4)
                  
  row_out <- c(stra[j],hatmu,hatsd,hatcv)
  dtoutb <- rbind(dtoutb,row_out)
  dtoutb <- dtoutb[!is.na(dtoutb$problema),]
}

# * b.2. estimación punto --------------------------------------------------------------------------

dtoutb_1 <- doBy::summary_by(FEX_C  ~ P3013_cat_lab, data = dt_lp, FUN = sum )
dtoutb_2 <- merge(dtoutb,dtoutb_1, all.x = T, by.y = 'P3013_cat_lab', by.x = 'problema')
openxlsx::write.xlsx(dtoutb_2,'lp_prob.xlsx')

# c. problemas grupo nacional + cat de problemas de empleo ------------------------------------------

# estratos sobre los que se desarrollará el muestreo
dt_ln_emp <- dt_ln[dt_ln$P3013_cat_lab == '4. Problemas relacionados con su trabajo o empleo, como falta de pago de salarios, reconocimiento o formalización de la relación laboral, cambio en las condiciones laborales, despido, acoso.',]
stra_emp  <- unique(dt_ln_emp$P3013_lab)

boon  <- 1000
dtout_emp <- data.frame(problema = NA, hatmu = NA, hatsd = NA, hatcv = NA )
lismu_emp <- list()

for( j in 1:length(stra_emp)){
  x <- dt_ln_emp[dt_ln_emp$P3013_lab == stra_emp[j],]
  x$id <- 1:nrow(x)
  
  samn <- round(nrow(x)*.6)
  mu <- NA
  
  for( i in 1:boon){
    
    xs <- x[sample(x$id,samn,replace = T),]
    mu0 <- sum(xs$FEX_C,na.rm = T)
    mu <- c(mu,mu0)
    mu <- mu[complete.cases(mu)]
  }
  
  lismu_emp[[j]] <- data.frame(mu)
  
  hatmu <- round(mean(mu,na.rm = T),4)
  hatsd <- round(sd(mu, na.rm = T),4)
  hatcv <- round((hatsd/hatmu) * 100,4)
  
  row_out <- c(stra_emp[j],hatmu,hatsd,hatcv)
  dtout_emp <- rbind(dtout_emp,row_out)
  dtout_emp <- dtout_emp[!is.na(dtout_emp$problema),]
}

# * c.2. estimación punto --------------------------------------------------------------------------

dtout_emp_1 <- doBy::summary_by(FEX_C  ~ P3013_lab, data = dt_ln_emp, FUN = sum )
dtout_emp_2 <- merge(dtout_emp,dtout_emp_1, all.x = T, by.y = 'P3013_lab', by.x = 'problema')
openxlsx::write.xlsx(dtout_emp_2,'dtout_emp.xlsx')

# d. problemas grupo nacional + cat de problemas de salud ------------------------------------------

# estratos sobre los que se desarrollará el muestreo
dt_ln_salu <- dt_lp[dt_lp$P3013_cat_lab == '10. Problemas relacionados con la prestación de los servicios de salud, pensión, riesgos laborales, como en la asignación de citas, medicamentos, calidad, oportunidad, afiliación, negación, reconocimiento o pago de la mesada pensional. ',]
stra_salu  <- unique(dt_ln_salu$P3013_lab)

boon  <- 1000
dtout_salu <- data.frame(problema = NA, hatmu = NA, hatsd = NA, hatcv = NA )
lismu_salu <- list()

for( j in 1:length(stra_salu)){
  x <- dt_ln_salu[dt_ln_salu$P3013_lab == stra_salu[j],]
  x$id <- 1:nrow(x)
  
  samn <- round(nrow(x)*.6)
  mu <- NA
  
  for( i in 1:boon){
    
    xs <- x[sample(x$id,samn,replace = T),]
    mu0 <- sum(xs$FEX_C,na.rm = T)
    mu <- c(mu,mu0)
    mu <- mu[complete.cases(mu)]
  }
  
  lismu_salu[[j]] <- data.frame(mu)
  
  hatmu <- round(mean(mu,na.rm = T),4)
  hatsd <- round(sd(mu, na.rm = T),4)
  hatcv <- round((hatsd/hatmu) * 100,4)
  
  row_out <- c(stra_salu[j],hatmu,hatsd,hatcv)
  dtout_salu <- rbind(dtout_salu,row_out)
  dtout_salu <- dtout_salu[!is.na(dtout_salu$problema),]
}

# * d.2. estimación punto --------------------------------------------------------------------------

dtout_salu_1 <- doBy::summary_by(FEX_C  ~ P3013_lab, data = dt_ln_salu, FUN = sum )
dtout_salu_2 <- merge(dtout_salu,dtout_salu_1, all.x = T, by.y = 'P3013_lab', by.x = 'problema')
openxlsx::write.xlsx(dtout_salu_2,'dtout_salu.xlsx')




# e. dist plot -------------------------------------------------------------------------------------

mudist <- do.call(cbind.data.frame, lismu)
names(mudist) <-   gsub("[^0-9.-]", "",substr(stra,1,3))
mudist$id <- 1:nrow(mudist)
names(mudist) <- c('salud','familia',
                   'estado','educación',
                   'discriminación',
                   'delitos','conflicto',
                   'consumo','servPublicos',
                   'empleo','deudas','vivienda',
                   'entorno','propiedad','medio ambiente','id')
library(ggplot2)
library(ggridges)
library(tidyr)

keycol <- "problema"
valuecol <- "mu"
gathercols <- names(mudist)[1:15]

mudistlong <- gather_(mudist, keycol, valuecol, gathercols)


theme_set(theme_minimal())
mudistlong$mul <- log(mudistlong$mu)

ggplot(
  mudistlong, 
  aes(x = `mul`, y = `problema`, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 10, size = 0, rel_min_height = 0.001) +
  scale_fill_viridis_c(name = "Logaritmo de fexc", option = "magma") +
  labs(title = 'Distribución de NJ declaradas sobre muestras del 40% en grupos de problemas')


