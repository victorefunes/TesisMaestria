library(foreign)
library(dplyr)
library(pcaPP)
library(Kendall)
library(reshape2)
library(ggplot2)
library(MASS)
library(ineq)
library(psych)
library(vcd)
library(IC2)

#Cargar datos
load("F:/cedlas/Base monografia/Base Educ/Base hogares/BaseT.Rdata")

#Variable estado civil
estciv<-rep(NA, length(bT$id))
estciv<-replace(estciv, bT$hombre_casado=="Casado" | bT$mujer_casada=="Casado",
                "Casado")
estciv<-replace(estciv, bT$hombre_soltero=="Soltero","No casado")
estciv<-replace(estciv, bT$mujer_soltera=="Soltero","No casado")
estciv<-replace(estciv, bT$hombre_divorciado=="Divorciado", "No casado")
estciv<-replace(estciv, bT$mujer_divorciada=="Divorciado", "No casado")
bT<-cbind(bT,estciv)

bT<-filter(bT, is.na(estciv)==FALSE)

#Generar variables de ingreso en precios constantes de 2012
bT<-bT %>% mutate(ila_mr=ila_m/ipc, ila_fr=ila_f/ipc, ii_mr=ii_m/ipc,
           ii_fr=ii_f/ipc, inla_mr=inla_m/ipc, inla_fr=inla_f/ipc)
bT<-bT %>% mutate(ii_t=ifelse(is.na(ii_m)==FALSE & is.na(ii_f)==FALSE, 
                              ii_m+ii_f, ifelse(is.na(ii_m)==TRUE, ii_f, ii_m)))
bT<-mutate(bT, itfr=itf/ipc)
bT<-mutate(bT, iresn=itf-ii_t)
bT<-mutate(bT, ii_tr=ii_t/ipc)
bT<-mutate(bT, ires=iresn/ipc)

bT<-filter(bT, ii_mr<quantile(ii_mr, p=0.995, na.rm=TRUE) | 
             is.na(ii_mr)==TRUE)

bT<-filter(bT, ii_fr<quantile(ii_fr, p=0.995, na.rm=TRUE) | 
             is.na(ii_fr)==TRUE)

bT<-filter(bT, ires<quantile(ires, p=0.995, na.rm=TRUE)| 
                is.na(ires)==TRUE)

bTc<-subset(bT, estciv=="Casado")

cv.df<- bTc %>% group_by (año) %>%
  summarise(mu_m=mean(ii_mr, na.rm=TRUE), 
            mu_f=mean(ii_fr, na.rm=TRUE),
            mu_t=mean(itfr, na.rm=TRUE),
            mu_r=mean(ires, na.rm=TRUE),
            sd_m=sd(ii_mr, na.rm=TRUE),
            sd_f=sd(ii_fr, na.rm=TRUE),
            sd_t=sd(itfr, na.rm=TRUE),
            sd_r=sd(ires, na.rm=TRUE),
            rho_mf=cor(ii_mr, ii_fr, method="pearson", use="complete.obs"),
            rho_mr=cor(ii_mr, ires, method="pearson", use="complete.obs"),
            rho_fr=cor(ii_fr, ires, method="pearson", use="complete.obs"))

cv.df<-mutate(cv.df, 
              cv_m=sd_m/mu_m, 
              cv_f=sd_f/mu_f,
              cv_r=sd_r/mu_r,
              cv_t=sd_t/mu_t,
              mu_ft=mu_f/mu_t)


coefvar<-subset(cv.df, select=c(año, cv_m, cv_f, cv_t, cv_r))

theme_set(theme_bw())
grcv<-ggplot() + 
  geom_line(data = coefvar, aes(x = año, y = cv_t, color="Total"), linetype=4,size=1)+
  geom_line(data = coefvar, aes(x = año, y = cv_m, color="Hombres"), size=1)+
  geom_line(data = coefvar, aes(x = año, y = cv_f, color="Mujeres"), size=1)+
  geom_line(data = coefvar, aes(x = año, y = cv_r, color="Residual"), size=1)+
  geom_point(data = coefvar, aes(x = año, y = cv_t, color="Total"), size=4)+
  geom_point(data = coefvar, aes(x = año, y = cv_m, color="Hombres"), size=4)+
  geom_point(data = coefvar, aes(x = año, y = cv_f, color="Mujeres"), size=4)+
  geom_point(data = coefvar, aes(x = año, y = cv_r, color="Residual"), size=4)+
  xlab("Año")+ylab(expression(CV[i]==SE[i]/bar(Y)[i]))+
  scale_color_manual(name="", values=c("grey70", "grey50", "grey20", "black"))+
  geom_vline(xintercept=2003, linetype="dotted")+
  theme(legend.position="bottom")
ggsave("C:/Users/VictorF/Dropbox/Tesis Maestría/Graficos/CoeffVar.eps", plot=grcv)

cv.df<-mutate(cv.df, alpha_m=mu_m/mu_t, alpha_f=mu_f/mu_t, alpha_r=mu_r/mu_t)

gralpha<-ggplot() + 
  geom_line(data = cv.df, aes(x = año, y = alpha_m, color="Hombres"), size=1)+
  geom_line(data = cv.df, aes(x = año, y = alpha_f, color="Mujeres"), size=1)+
  geom_line(data = cv.df, aes(x = año, y = alpha_r, color="Residual"), size=1)+
  geom_point(data = cv.df, aes(x = año, y = alpha_m, color="Hombres"), size=4)+
  geom_point(data = cv.df, aes(x = año, y = alpha_f, color="Mujeres"), size=4)+
  geom_point(data = cv.df, aes(x = año, y = alpha_r, color="Residual"), size=4)+
  xlab("Año")+ylab(expression(mu[k]/mu[T]))+
  geom_vline(xintercept=2003, linetype="dotted")+
  scale_color_manual(name="", values=c("darkgrey", "black", "grey85"))+
  theme(legend.position="bottom")
ggsave("C:/Users/VictorF/Dropbox/Tesis Maestría/Graficos/Share.eps", plot=gralpha)

grcorr<-ggplot() + 
  geom_line(data = cv.df, aes(x = año, y = rho_mf, color="rho_fm"), size=1)+
  geom_line(data = cv.df, aes(x = año, y = rho_mr, color="rho_mr"), size=1)+
  geom_line(data = cv.df, aes(x = año, y = rho_fr, color="rho_fr"), size=1)+
  geom_point(data = cv.df, aes(x = año, y = rho_mf, color="rho_fm"), size=4)+
  geom_point(data = cv.df, aes(x = año, y = rho_mr, color="rho_mr"), size=4)+
  geom_point(data = cv.df, aes(x = año, y = rho_fr, color="rho_fr"), size=4)+
  xlab("Año")+ylab("Coeficientes de correlación")+
  geom_vline(xintercept=2003, linetype="dotted")+
  scale_color_manual(name="", values=c("black", "grey30", "grey70"),
                     labels=c(expression(rho[fm]), expression(rho[mr]),
                              expression(rho[fr])))+
  theme(legend.position="bottom")
ggsave("C:/Users/VictorF/Dropbox/Tesis Maestría/Graficos/corr.eps", plot=grcorr)

## Descomposiciones
mar.df<- bTc %>% group_by(año) %>%
  summarise(y_m=mean(ii_mr, na.rm=TRUE), 
            y_f=mean(ii_fr, na.rm=TRUE),
            y_r=mean(ires, na.rm=TRUE),
            y_T=mean(itfr, na.rm=TRUE),
            sd_m=sd(ii_mr, na.rm=TRUE),
            sd_f=sd(ii_fr, na.rm=TRUE),
            sd_r=sd(ires, na.rm=TRUE),
            sd_T=sd(itfr, na.rm=TRUE),
            rho_mf=cor(ii_mr, ii_fr, 
                       method="pearson",use="pairwise.complete.obs"),
            rho_mr=cor(ii_mr, ires, 
                       method="pearson",use="pairwise.complete.obs"),
            rho_fr=cor(ii_fr, ires, 
                       method="pearson",use="pairwise.complete.obs"))

#Coeficientes de variación
mar.df<-mar.df %>% mutate(cv_m=sd_m/y_m,
                          cv_f=sd_f/y_f,
                          cv_r=sd_r/y_r,
                          cv_T=sd_T/y_T)

#Participaciones de cada componente
mar.df<-mar.df %>% mutate(s_m=y_m/y_T,
                          s_f=y_f/y_T,
                          s_r=y_r/y_T)

#Cálculo del cv total descompuesto (casados)
mar.df<-mar.df %>% mutate(cv_TD=sqrt(s_m^2*cv_m^2+s_f^2*cv_f^2+s_r^2*cv_r^2+
                            2*rho_mf*cv_m*cv_f*s_m*s_f+
                            2*rho_mr*cv_m*cv_r*s_m*s_r+
                            2*rho_fr*cv_f*cv_r*s_f*s_r))

#Cálculo del cv total decompuesto (Todos)

all.df<-bT %>% group_by(año, estciv) %>% 
  summarise(y_i=mean(itfr, na.rm=TRUE), mu_i=n(), sd_i=sd(itfr, na.rm=TRUE))
all.df<-tbl_df(reshape(data.frame(all.df), direction="wide", idvar="año", 
                timevar="estciv"))
colnames(all.df)<-c("año", "y_A", "n_A", "sd_A", "y_B", "n_B", "sd_B")

N.df<-bT %>% group_by(año) %>% 
  summarise(y_T=mean(itfr, na.rm=TRUE), n_T=n(), sd_T=sd(itfr, na.rm=TRUE))

all.df<-tbl_df(cbind(all.df, N.df[,-1]))
rm(N.df)

all.df<-all.df %>% mutate(mu_A=n_A/n_T, mu_B=n_B/n_T, cv_A=sd_A/y_A,
                          cv_B=sd_B/y_B, cv_T=sd_T/y_T)

#Cálculo del cv total descompuesto (todos)

all.df<-all.df %>% mutate(cv_TD=sqrt(mu_A*(y_A/y_T)^2*cv_A^2+
                                     mu_B*(y_B/y_T)^2*cv_B^2+
                                    (mu_A*(y_A/y_T)^2+mu_B*(y_B/y_T)^2)/y_T))

#Contrafáctico 1: se fijan todos los parámetros en sus valores iniciales
# excepto cv_m en la primer ecuación
mar.df<-mar.df %>% mutate(cv_TD1=sqrt(mar.df$s_m[1]^2*cv_m^2+mar.df$s_f[1]^2*
                                      mar.df$cv_f[1]^2+mar.df$s_r[1]^2*
                                      mar.df$cv_r[1]^2+
                                      2*mar.df$rho_mf[1]*cv_m*mar.df$cv_f[1]*
                                        mar.df$s_m[1]*mar.df$s_f[1]+
                                      2*mar.df$rho_mr[1]*mar.df$cv_m[1]*mar.df$cv_r[1]*
                                        mar.df$s_m[1]*mar.df$s_r[1]+
                                      2*mar.df$rho_fr[1]*mar.df$cv_f[1]*
                                        mar.df$cv_r[1]*mar.df$s_f[1]*mar.df$s_r[1]))


#Contrafáctico 2: se fijan todos los parámetros en sus valores iniciales
# excepto cv_m, cv_f y s_f en la primer ecuación
# Primero: sólo varía s_f
mar.df<-mar.df %>% mutate(cv_TD2=sqrt(mar.df$s_m[1]^2*cv_m^2+s_f^2*
                                      mar.df$cv_f[1]^2+mar.df$s_r[1]^2*
                                      mar.df$cv_r[1]^2+
                                    2*mar.df$rho_mf[1]*mar.df$cv_m[1]*mar.df$cv_f[1]*
                                      mar.df$s_m[1]*s_f+
                                    2*mar.df$rho_mr[1]*mar.df$cv_m[1]*mar.df$cv_r[1]*
                                      mar.df$s_m[1]*mar.df$s_r[1]+
                                    2*mar.df$rho_fr[1]*mar.df$cv_f[1]*
                                      mar.df$cv_r[1]*s_f*mar.df$s_r[1]))

#Segundo: varían s_f y cv_f
mar.df<-mar.df %>% mutate(cv_TD3=sqrt(mar.df$s_m[1]^2*cv_m^2+s_f^2*
                                        cv_f^2+mar.df$s_r[1]^2*
                                        mar.df$cv_r[1]^2+
                                        2*mar.df$rho_mf[1]*mar.df$cv_m[1]*cv_f*
                                        mar.df$s_m[1]*s_f+
                                        2*mar.df$rho_mr[1]*mar.df$cv_m[1]*mar.df$cv_r[1]*
                                        mar.df$s_m[1]*mar.df$s_r[1]+
                                        2*mar.df$rho_fr[1]*cv_f*
                                        mar.df$cv_r[1]*s_f*mar.df$s_r[1]))

#Contrafáctico 3: se fijan todos los parámetros en sus valores iniciales
# excepto cv_m, cv_f, s_f y rho_mf en la primer ecuación
mar.df<-mar.df %>% mutate(cv_TD4=sqrt(mar.df$s_m[1]^2*cv_m^2+s_f^2*
                                      cv_f^2+mar.df$s_r[1]^2*
                                      mar.df$cv_r[1]^2+
                                    2*rho_mf*cv_m*cv_f*
                                      mar.df$s_m[1]*s_f+
                                    2*mar.df$rho_mr[1]*cv_m*mar.df$cv_r[1]*
                                      s_m*mar.df$s_r[1]+
                                    2*mar.df$rho_fr[1]*cv_f*
                                      mar.df$cv_r[1]*s_f*mar.df$s_r[1]))


#Contrafáctico 4: se fija el valor de mu_A en la segunda ecuación en su 
# valor inicial y se calcula el cv_T
all.df<-cbind(all.df, cv_TD1=mar.df$cv_TD1, cv_TD2=mar.df$cv_TD2, 
              cv_TD3=mar.df$cv_TD3, cv_TD4=mar.df$cv_TD4)

all.df<-all.df %>% mutate(cv_T0=sqrt(mu_A*(y_A/y_T)^2*cv_TD^2+
                                       mu_B*(y_B/y_T)^2*cv_B^2+
                                       (mu_A*(y_A/y_T)^2+mu_B*(y_B/y_T)^2)/y_T))

all.df<-all.df %>% mutate(cv_T1=sqrt(0.8092*(y_A/y_T)^2*cv_TD1^2+
                                       (1-0.8092)*(y_B/y_T)^2*cv_B^2+
                                       (0.8092*(y_A/y_T)^2+(1-0.8092)*(y_B/y_T)^2)/y_T))

all.df<-all.df %>% mutate(cv_T2=sqrt(0.8092*(y_A/y_T)^2*cv_TD2^2+
                                       (1-0.8092)*(y_B/y_T)^2*cv_B^2+
                                       (0.8092*(y_A/y_T)^2+(1-0.8092)*(y_B/y_T)^2)/y_T))

all.df<-all.df %>% mutate(cv_T3=sqrt(0.8092*(y_A/y_T)^2*cv_TD3^2+
                                       (1-0.8092)*(y_B/y_T)^2*cv_B^2+
                                       (0.8092*(y_A/y_T)^2+(1-0.8092)*(y_B/y_T)^2)/y_T))

all.df<-all.df %>% mutate(cv_T4=sqrt(0.8092*(y_A/y_T)^2*cv_TD4^2+
                                       (1-0.8092)*(y_B/y_T)^2*cv_B^2+
                                       (0.8092*(y_A/y_T)^2+(1-0.8092)*(y_B/y_T)^2)/y_T))



#Valores relativos al año base
mar.df<-mar.df %>% mutate(var_cv_TD=(cv_TD/mar.df$cv_TD[1])-1,
                          var_cv_TD1=(cv_TD1/mar.df$cv_TD1[1])-1,
                          var_cv_TD2=(cv_TD2/mar.df$cv_TD2[1])-1,
                          var_cv_TD3=(cv_TD3/mar.df$cv_TD3[1])-1,
                          var_cv_TD4=(cv_TD4/mar.df$cv_TD4[1])-1)

all.df<-all.df %>% mutate(var_cv_T0=(cv_T0/all.df$cv_T0[1])-1,
                          var_cv_T1=(cv_T1/all.df$cv_T1[1])-1,
                          var_cv_T2=(cv_T2/all.df$cv_T2[1])-1,
                          var_cv_T3=(cv_T3/all.df$cv_T3[1])-1,
                          var_cv_T4=(cv_T4/all.df$cv_T4[1])-1)

#Coeficientes de variación por tipo de familia
cvs1<-ggplot() + 
  geom_line(data = mar.df, aes(x = año, y = var_cv_TD, color="Observado"), linetype=4, size=1)+
  geom_line(data = mar.df, aes(x = año, y = var_cv_TD1, color="CV hombres"), size=1)+
  geom_line(data = mar.df, aes(x = año, y = var_cv_TD3, color="+Participaciones y CV Mujeres"), size=1)+
  geom_line(data = mar.df, aes(x = año, y = var_cv_TD4, color="+Correlación de ingresos laborales"), size=1)+
#  geom_line(data = mar.df, aes(x = año, y = var_cv_TD5, color="+CV residual"), size=1)+
  geom_point(data = mar.df, aes(x = año, y = var_cv_TD, color="Observado"), size=4)+
  geom_point(data = mar.df, aes(x = año, y = var_cv_TD1, color="CV hombres"), size=4)+
  geom_point(data = mar.df, aes(x = año, y = var_cv_TD3, color="+Participaciones y CV Mujeres"), size=4)+
  geom_point(data = mar.df, aes(x = año, y = var_cv_TD4, color="+Correlación de ingresos laborales"), size=4)+
#  geom_point(data = mar.df, aes(x = año, y = var_cv_TD5, color="+CV residual"), size=4)+
  xlab("Año")+ylab("Coeficientes de variación")+
  geom_vline(xintercept=2003, linetype="dotted")+
  scale_color_manual(name="", values=c("black", "grey10", "grey50", "grey70"), 
                     limits=c("Observado","CV hombres",
                              "+Participaciones y CV Mujeres", 
                              "+Correlación de ingresos laborales"))+
  theme(legend.position="bottom")
cvs1
ggsave("C:/Users/VictorF/Dropbox/Tesis Maestría/Graficos/cvs1.eps", plot=cvs1)


cvs2<-ggplot() + 
  geom_line(data = all.df, aes(x = año, y = var_cv_T0, color="Observado"), linetype=4, size=1)+
  geom_line(data = all.df, aes(x = año, y = var_cv_T1, color="CV hombres"), size=1)+
  geom_line(data = all.df, aes(x = año, y = var_cv_T3, color="+Parts. y CV mujeres"), size=1)+
  geom_line(data = all.df, aes(x = año, y = var_cv_T4, color="Correlación ingresos laborales"), size=1)+
#  geom_line(data = all.df, aes(x = año, y = var_cv_T5, color="CV residual"), size=1)+
  geom_point(data = all.df, aes(x = año, y = var_cv_T0, color="Observado"), size=4)+
  geom_point(data = all.df, aes(x = año, y = var_cv_T1, color="CV hombres"), size=4)+
  geom_point(data = all.df, aes(x = año, y = var_cv_T3, color="+Parts. y CV mujeres"), size=4)+ 
  geom_point(data = all.df, aes(x = año, y = var_cv_T4, color="Correlación ingresos laborales"), size=4)+   
#  geom_point(data = all.df, aes(x = año, y = var_cv_T5, color="CV residual"), size=4)+
  xlab("Año")+ylab("Coeficientes de variación")+
  geom_vline(xintercept=2003, linetype="dotted")+
  scale_color_manual(name="", values=c("grey70", "grey50", "grey10", "black"),
                     limits=c("Correlación ingresos laborales", 
                              "+Parts. y CV mujeres",
                              "CV hombres","Observado"))+
  theme(legend.position="bottom")
cvs2
ggsave("C:/Users/VictorF/Dropbox/Tesis Maestría/Graficos/cvs2.eps", plot=cvs2)

mar.df<-mar.df %>% dplyr::select(-starts_with("cv_TD"))
all.df<-all.df %>% dplyr::select(-starts_with("cv_T"))

source("C:/Users/VictorF/Dropbox/Tesis Maestría/Codigos/Decomps.R")


#Tasas de variación con respecto al año base
mar.df<-mar.df %>% mutate(var_cv_T=(cv_T/mar.df$cv_T[1])-1)
mar.df<-mar.df %>% mutate(var_cv_m1=(cv_m1/mar.df$cv_T[1])-1)
mar.df<-mar.df %>% mutate(var_cv_m2=(cv_m2/mar.df$cv_T[1])-1)
mar.df<-mar.df %>% mutate(var_cv_m3=(cv_m3/mar.df$cv_T[1])-1)


cvs3<-ggplot() + 
  geom_line(data = mar.df, aes(x = año, y = var_cv_T, color="Observado"), linetype=4, size=1)+
  geom_line(data = mar.df, aes(x = año, y = var_cv_m1, color="Promedio de cv_m"), size=1)+
  geom_line(data = mar.df, aes(x = año, y = var_cv_m2, color="Promedio de s_f/cv_f"), size=1)+
  geom_line(data = mar.df, aes(x = año, y = var_cv_m3, color="Promedio de rho_mf"), size=1)+
  geom_point(data = mar.df, aes(x = año, y = var_cv_T, color="Observado"), size=4)+
  geom_point(data = mar.df, aes(x = año, y = var_cv_m1, color="Promedio de cv_m"), size=4)+
  geom_point(data = mar.df, aes(x = año, y = var_cv_m2, color="Promedio de s_f/cv_f"), size=4)+
  geom_point(data = mar.df, aes(x = año, y = var_cv_m3, color="Promedio de rho_mf"), size=4)+
  xlab("Año")+ylab("Coeficientes de variación")+
  geom_vline(xintercept=2003, linetype="dotted")+
  scale_color_manual(name="", values=c("black", "grey20", "grey60", "grey80"), 
                     limits=c("Observado","Promedio de cv_m",
                              "Promedio de s_f/cv_f","Promedio de rho_mf"))+
  theme(legend.position="bottom")
cvs3
ggsave("C:/Users/VictorF/Dropbox/Tesis Maestría/Graficos/cvs3.eps", plot=cvs3)

#Contrafáctico 1: se fijan todos los parámetros en sus valores iniciales
# excepto cv_m en la primer ecuación
mar.df<-mar.df %>% mutate(cv_TT1=sqrt(mar.df$s_m[1]^2*cv_m^2+mar.df$s_f[1]^2*
                                        mar.df$cv_f[1]^2+mar.df$s_r[1]^2*
                                        mar.df$cv_r[1]^2+
                                        2*mar.df$rho_mf[1]*cv_m*mar.df$cv_f[1]*
                                        mar.df$s_m[1]*mar.df$s_f[1]+
                                        2*mar.df$rho_mr[1]*cv_m*mar.df$cv_r[1]*
                                        mar.df$s_m[1]*mar.df$s_r[1]+
                                        2*mar.df$rho_fr[1]*mar.df$cv_f[1]*
                                        mar.df$cv_r[1]*mar.df$s_f[1]*mar.df$s_r[1]))


#Contrafáctico 2: se fijan todos los parámetros en sus valores iniciales
# excepto cv_f y s_f en la primer ecuación
# Primero: sólo varía s_f
mar.df<-mar.df %>% mutate(cv_TT2=sqrt(mar.df$s_m[1]^2*mar.df$cv_m[1]^2+s_f^2*
                                        mar.df$cv_f[1]^2+mar.df$s_r[1]^2*
                                        mar.df$cv_r[1]^2+
                                        2*mar.df$rho_mf[1]*mar.df$cv_m[1]*mar.df$cv_f[1]*
                                        mar.df$s_m[1]*s_f+
                                        2*mar.df$rho_mr[1]*mar.df$cv_m[1]*mar.df$cv_r[1]*
                                        mar.df$s_m[1]*mar.df$s_r[1]+
                                        2*mar.df$rho_fr[1]*mar.df$cv_f[1]*
                                        mar.df$cv_r[1]*s_f*mar.df$s_r[1]))

#Segundo: varían s_f y cv_f
mar.df<-mar.df %>% mutate(cv_TT3=sqrt(mar.df$s_m[1]^2*mar.df$cv_m[1]^2+s_f^2*
                                        cv_f^2+mar.df$s_r[1]^2*
                                        mar.df$cv_r[1]^2+
                                        2*mar.df$rho_mf[1]*mar.df$cv_m[1]*cv_f*
                                        mar.df$s_m[1]*s_f+
                                        2*mar.df$rho_mr[1]*mar.df$cv_m[1]*mar.df$cv_r[1]*
                                        mar.df$s_m[1]*mar.df$s_r[1]+
                                        2*mar.df$rho_fr[1]*cv_f*
                                        mar.df$cv_r[1]*s_f*mar.df$s_r[1]))

#Contrafáctico 3: se fijan todos los parámetros en sus valores iniciales
# excepto rho_mf en la primer ecuación
mar.df<-mar.df %>% mutate(cv_TT4=sqrt(mar.df$s_m[1]^2*mar.df$cv_m[1]^2+mar.df$s_f[1]^2*
                                        mar.df$cv_f[1]^2+mar.df$s_r[1]^2*
                                        mar.df$cv_r[1]^2+
                                        2*rho_mf*mar.df$cv_m[1]*mar.df$cv_f[1]*
                                        mar.df$s_m[1]*mar.df$s_f[1]+
                                        2*mar.df$rho_mr[1]*mar.df$cv_m[1]*mar.df$cv_r[1]*
                                        mar.df$s_m[1]*mar.df$s_r[1]+
                                        2*mar.df$rho_fr[1]*mar.df$cv_f[1]*
                                        mar.df$cv_r[1]*mar.df$s_f[1]*mar.df$s_r[1]))


#Contrafáctico 4: se fija el valor de mu_A en la segunda ecuación en su 
# valor inicial y se calcula el cv_T
all.df<-cbind(all.df, cv_TT0=mar.df$cv_T, 
              cv_TT1=mar.df$cv_TT1, 
              cv_TT2=mar.df$cv_TT2, 
              cv_TT3=mar.df$cv_TT3, 
              cv_TT4=mar.df$cv_TT4)

all.df<-all.df %>% mutate(cv_TT0=sqrt(mu_A*(y_A/y_T)^2*cv_T^2+
                                       mu_B*(y_B/y_T)^2*cv_B^2+
                                       (mu_A*(y_A/y_T)^2+mu_B*(y_B/y_T)^2)/y_T))

all.df<-all.df %>% mutate(cv_TT1=sqrt(0.8092*(y_A/y_T)^2*cv_TT1^2+
                                       (1-0.8092)*(y_B/y_T)^2*cv_B^2+
                                       (0.8092*(y_A/y_T)^2+(1-0.8092)*(y_B/y_T)^2)/y_T))

all.df<-all.df %>% mutate(cv_TT2=sqrt(0.8092*(y_A/y_T)^2*cv_TT2^2+
                                       (1-0.8092)*(y_B/y_T)^2*cv_B^2+
                                       (0.8092*(y_A/y_T)^2+(1-0.8092)*(y_B/y_T)^2)/y_T))

all.df<-all.df %>% mutate(cv_TT3=sqrt(0.8092*(y_A/y_T)^2*cv_TT3^2+
                                       (1-0.8092)*(y_B/y_T)^2*cv_B^2+
                                       (0.8092*(y_A/y_T)^2+(1-0.8092)*(y_B/y_T)^2)/y_T))

all.df<-all.df %>% mutate(cv_TT4=sqrt(0.8092*(y_A/y_T)^2*cv_TT4^2+
                                       (1-0.8092)*(y_B/y_T)^2*cv_B^2+
                                       (0.8092*(y_A/y_T)^2+(1-0.8092)*(y_B/y_T)^2)/y_T))

  
cvs4<-ggplot() + 
  geom_line(data = mar.df, aes(x = año, y = cv_T, color="Observado"), linetype=4, size=1)+
  geom_line(data = mar.df, aes(x = año, y = cv_TT1, color="CV hombres"), size=1)+
  geom_line(data = mar.df, aes(x = año, y = cv_TT2, color="+participaciones"), size=1)+
  geom_line(data = mar.df, aes(x = año, y = cv_TT3, color="+CV mujeres"), size=1)+
  geom_point(data = mar.df, aes(x = año, y = cv_T, color="Observado"), size=4)+
  geom_point(data = mar.df, aes(x = año, y = cv_TT1, color="CV hombres"), size=4)+
  geom_point(data = mar.df, aes(x = año, y = cv_TT2, color="+participaciones"), size=4)+
  geom_point(data = mar.df, aes(x = año, y = cv_TT3, color="+CV mujeres"), size=4)+
  xlab("Año")+ylab("Coeficientes de variación")+
  geom_vline(xintercept=2003, linetype="dotted")+
  scale_color_manual(name="", values=c("grey70", "grey50", "grey20", "black"))+
  theme(legend.position="bottom")
cvs4

cvs5<-ggplot() + 
  geom_line(data = all.df, aes(x = año, y = cv_TT0, color="Observado"), linetype=4, size=1)+
  geom_line(data = all.df, aes(x = año, y = cv_TT1, color="CV hombres"), size=1)+
  geom_line(data = all.df, aes(x = año, y = cv_TT3, color="+parts. y CV mujeres"), size=1)+
  geom_line(data = all.df, aes(x = año, y = cv_TT4, color="Rho libre"), size=1)+
  geom_point(data = all.df, aes(x = año, y = cv_TT0, color="Observado"), size=4)+
  geom_point(data = all.df, aes(x = año, y = cv_TT1, color="CV hombres"), size=4)+
  geom_point(data = all.df, aes(x = año, y = cv_TT3, color="+parts. y CV mujeres"), size=4)+ 
  geom_point(data = all.df, aes(x = año, y = cv_TT4, color="Rho libre"), size=4)+   
  xlab("Año")+ylab("Coeficientes de variación")+
  geom_vline(xintercept=2003, linetype="dotted")+
  scale_color_manual(name="", values=c("grey70", "grey50", "grey20", "black"))+
  theme(legend.position="bottom")
cvs5


rm(list=ls())

