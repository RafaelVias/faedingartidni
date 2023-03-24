
# create standata
fostudagurinn_langi <- as.Date(Easter(1990:2021,shift = -2))
fostudagurinn_langi <- sort(with(d,which(date%in%fostudagurinn_langi)))
paskadagur          <- as.Date(Easter(1990:2021,shift = 0))
paskadagur          <- sort(with(d,which(date%in%paskadagur)))
hvitasunna          <- as.Date(c(Easter(1990:2021,shift = 49)))
hvitasunna          <- sort(with(d,which(date%in%hvitasunna)))
skirdagur           <- as.Date(Easter(1990:2021,shift = -3))
skirdagur           <- sort(with(d,which(date%in%skirdagur)))
laug_f_paska        <- as.Date(Easter(1990:2021,shift = -1))
laug_f_paska        <- sort(with(d,which(date%in%laug_f_paska)))
annar_i_paskum      <- as.Date(Easter(1990:2021,shift = 1))
annar_i_paskum      <- sort(with(d,which(date%in%annar_i_paskum)))
sumard_fyrsti       <- with(d,which(month==4&day_of_week==4&day>=19&day<=25))
uppstigningardagur  <- as.Date(c(Easter(1990:2021,shift = 39)))
uppstigningardagur  <- sort(with(d,which(date%in%uppstigningardagur)))
annar_i_hvitasunnu  <- as.Date(c(Easter(1990:2021,shift = 50)))
annar_i_hvitasunnu  <- sort(with(d,which(date%in%annar_i_hvitasunnu)))
frid_verzlunarm     <- with(d,which(month==8&day_of_week==1&day<=7))
standata <- list(x=d$id,
                 y=d$births_relative100,
                 N=length(d$id),
                 c_f1=1.5, 
                 M_f1=20, 
                 J_f2=20, 
                 day_of_week=d$day_of_week2,
                 day_of_year=d$day_of_year2,
                 fostudagurinn_langi=fostudagurinn_langi,   
                 paskadagur=paskadagur,                     
                 hvitasunna=hvitasunna,                     
                 skirdagur=skirdagur,                       
                 laug_f_paska=laug_f_paska,                 
                 annar_i_paskum=annar_i_paskum,             
                 sumard_fyrsti=sumard_fyrsti,               
                 uppstigningardagur=uppstigningardagur,     
                 annar_i_hvitasunnu=annar_i_hvitasunnu,     
                 frid_verzlunarm=frid_verzlunarm )

# run model
model <- cmdstan_model(stan_file = "Stan/normal_float.stan")
opt <- model$optimize(data=standata, init=0, algorithm='lbfgs',
                      history=100, tol_obj=10)
odraws <- opt$draws()
init <- sapply(c('lengthscale_f1','lengthscale_f2',
                 'sigma_f1','sigma_f2','sigma_f4','sigma_f5',
                 'nu_f4',
                 'sigma','beta_f1','beta_f2','beta_f3','beta_f4','beta_f5'),
               function(variable) {as.numeric(subset(odraws, variable=variable))})
fit <- model$sample(data=standata, iter_warmup=500, iter_sampling=500,
                    chains=4, seed = 1, parallel_chains=4,
                    init=function() { init })

# create outputs
draws <- fit$draws()
draws <- as_draws_matrix(draws)
Ef <- apply(subset(draws, variable='f'), 2, median)
Ef1 <- apply(subset(draws, variable='f1'), 2, median)
Ef1 <- Ef1 - mean(Ef1) + mean(d$births_relative100)
Ef2 <- apply(subset(draws, variable='f2'), 2, median)
Ef2 <- Ef2 - mean(Ef2) + mean(d$births_relative100)
Ef_day_of_week <- apply(subset(draws, variable='f_day_of_week'), 2, median)
Ef_day_of_week <- Ef_day_of_week - mean(Ef_day_of_week) + mean(d$births_relative100)
Ef4 <- apply(subset(draws, variable='beta_f4'), 2, median)*sd(d$births_relative100)
Ef4 <- Ef4+100
Efloats<-c(apply(subset(draws,variable='beta_f5'),2,median)*sd(d$births_relative100))
Efloats <- Efloats+100
floats1992 <- c(fostudagurinn_langi[3],
                paskadagur[3],
                hvitasunna[3],
                skirdagur[3],
                laug_f_paska[3],
                annar_i_paskum[3],
                sumard_fyrsti[3],
                uppstigningardagur[3],
                annar_i_hvitasunnu[3],
                frid_verzlunarm[3])-730
Ef4float <- Ef4
Ef4float[floats1992] <- Ef4float[floats1992]*Efloats[c(1,2,3,4,5,6,7,8,9,10)]/100

N=length(d$id)
storhatidadagar <- data.frame(x=c(as.Date("1992-01-01"),
                                  as.Date(d$date[floats1992[1:3]])+730,
                                  as.Date("1992-06-17"),
                                  as.Date("1992-12-24"),
                                  as.Date("1992-12-25"),
                                  as.Date("1992-12-31"),
                                  as.Date("1992-12-01")),
                              y=c(Ef4float[1],
                                  Ef4float[floats1992[1:3]],
                                  Ef4float[169],
                                  Ef4float[359],
                                  Ef4float[360],
                                  Ef4float[366],
                                  Ef4float[336]))
serstakir_fridagar <- data.frame(x=c(as.Date(d$date[floats1992[4:10]])+730,
                                     as.Date("1992-04-23"),
                                     as.Date("1992-05-01")),
                                 y=c(Ef4float[floats1992[4:10]],
                                     Ef4float[114],
                                     Ef4float[122]))
adrir_dagar <- data.frame(x=c(as.Date("1992-02-29")),
                          y=c(Ef4float[60]))
dagar <- rbind(data.frame(storhatidadagar,'type'="storhatidadagar"),
               data.frame(serstakir_fridagar, 'type'="serstakir_fridagar"),
               data.frame(adrir_dagar,'type'="adrir_dagar"))
rownames(dagar) <- NULL
dagar$type <- factor(dagar$type,
                     levels = c("storhatidadagar",
                                "serstakir_fridagar",
                                "adrir_dagar"))
dfs <- 3


