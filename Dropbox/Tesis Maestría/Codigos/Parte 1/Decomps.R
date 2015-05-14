#Descomposiciones (18 variables)
#(1)
#cv_m
mar.df<-mar.df %>% mutate(cv_11=sqrt(mar.df$s_m[1]^2*cv_m^2+mar.df$s_f[1]^2*
                                       mar.df$cv_f[1]^2+mar.df$s_r[1]^2*
                                       mar.df$cv_r[1]^2+
                                       2*mar.df$rho_mf[1]*cv_m*mar.df$cv_f[1]*
                                       mar.df$s_m[1]*mar.df$s_f[1]+
                                       2*mar.df$rho_mr[1]*cv_m*mar.df$cv_r[1]*
                                       mar.df$s_m[1]*mar.df$s_r[1]+
                                       2*mar.df$rho_fr[1]*mar.df$cv_f[1]*
                                       mar.df$cv_r[1]*mar.df$s_f[1]*mar.df$s_r[1]))

#cv_m, cv_f/s_f
mar.df<-mar.df %>% mutate(cv_12=sqrt(mar.df$s_m[1]^2*cv_m^2+s_f^2*
                                       cv_f^2+mar.df$s_r[1]^2*
                                       mar.df$cv_r[1]^2+
                                       2*mar.df$rho_mf[1]*cv_m*cv_f*
                                       mar.df$s_m[1]*s_f+
                                       2*mar.df$rho_mr[1]*cv_m*mar.df$cv_r[1]*
                                       mar.df$s_m[1]*mar.df$s_r[1]+
                                       2*mar.df$rho_fr[1]*cv_f*
                                       mar.df$cv_r[1]*s_f*mar.df$s_r[1]))

#cv_m, cv_f/s_f, rho_mf
mar.df<-mar.df %>% mutate(cv_13=sqrt(mar.df$s_m[1]^2*cv_m^2+s_f^2*
                                       cv_f^2+mar.df$s_r[1]^2*
                                       mar.df$cv_r[1]^2+
                                       2*rho_mf*cv_m*cv_f*
                                       mar.df$s_m[1]*s_f+
                                       2*mar.df$rho_mr[1]*cv_m*mar.df$cv_r[1]*
                                       s_m*mar.df$s_r[1]+
                                       2*mar.df$rho_fr[1]*cv_f*
                                       mar.df$cv_r[1]*s_f*mar.df$s_r[1]))

#(2)
#cv_m
mar.df<-mar.df %>% mutate(cv_21=sqrt(mar.df$s_m[1]^2*cv_m^2+mar.df$s_f[1]^2*
                                       mar.df$cv_f[1]^2+mar.df$s_r[1]^2*
                                       mar.df$cv_r[1]^2+
                                       2*mar.df$rho_mf[1]*cv_m*mar.df$cv_f[1]*
                                       mar.df$s_m[1]*mar.df$s_f[1]+
                                       2*mar.df$rho_mr[1]*cv_m*mar.df$cv_r[1]*
                                       mar.df$s_m[1]*mar.df$s_r[1]+
                                       2*mar.df$rho_fr[1]*mar.df$cv_f[1]*
                                       mar.df$cv_r[1]*mar.df$s_f[1]*mar.df$s_r[1]))

#cv_m, rho_mf
mar.df<-mar.df %>% mutate(cv_22=sqrt(mar.df$s_m[1]^2*cv_m^2+mar.df$s_f[1]^2*
                                       mar.df$cv_f[1]^2+mar.df$s_r[1]^2*
                                       mar.df$cv_r[1]^2+
                                       2*rho_mf*cv_m*mar.df$cv_f[1]*
                                       mar.df$s_m[1]*mar.df$s_f[1]+
                                       2*mar.df$rho_mr[1]*cv_m*mar.df$cv_r[1]*
                                       mar.df$s_m[1]*mar.df$s_r[1]+
                                       2*mar.df$rho_fr[1]*mar.df$cv_f[1]*
                                       mar.df$cv_r[1]*mar.df$s_f[1]*mar.df$s_r[1]))
#cv_m, rho_mw, s_f/cv_f
mar.df<-mar.df %>% mutate(cv_23=sqrt(mar.df$s_m[1]^2*cv_m^2+s_f^2*
                                       cv_f^2+mar.df$s_r[1]^2*
                                       mar.df$cv_r[1]^2+2*rho_mf*cv_m*cv_f*
                                       mar.df$s_m[1]*s_f+
                                       2*mar.df$rho_mr[1]*cv_m*mar.df$cv_r[1]*
                                       mar.df$s_m[1]*mar.df$s_r[1]+
                                       2*mar.df$rho_fr[1]*cv_f*
                                       mar.df$cv_r[1]*s_f*mar.df$s_r[1]))

#(3)
#s_f/cv_f
mar.df<-mar.df %>% mutate(cv_31=sqrt(mar.df$s_m[1]^2*mar.df$cv_m[1]^2+s_f^2*
                                       cv_f^2+mar.df$s_r[1]^2*
                                       mar.df$cv_r[1]^2+
                                       2*mar.df$rho_mf[1]*mar.df$cv_m[1]*cv_f*
                                       mar.df$s_m[1]*s_f+
                                       2*mar.df$rho_mr[1]*mar.df$cv_m[1]*mar.df$cv_r[1]*
                                       mar.df$s_m[1]*mar.df$s_r[1]+
                                       2*mar.df$rho_fr[1]*cv_f*
                                       mar.df$cv_r[1]*s_f*mar.df$s_r[1]))

#s_f/cv_f, cv_m
mar.df<-mar.df %>% mutate(cv_32=sqrt(mar.df$s_m[1]^2*cv_m^2+s_f^2*
                                       cv_f^2+mar.df$s_r[1]^2*
                                       mar.df$cv_r[1]^2+
                                       2*mar.df$rho_mf[1]*cv_m*cv_f*
                                       mar.df$s_m[1]*s_f+
                                       2*mar.df$rho_mr[1]*cv_m*mar.df$cv_r[1]*
                                       mar.df$s_m[1]*mar.df$s_r[1]+
                                       2*mar.df$rho_fr[1]*cv_f*
                                       mar.df$cv_r[1]*s_f*mar.df$s_r[1]))

#s_f/cv_f, cv_m, rho_mf
mar.df<-mar.df %>% mutate(cv_33=sqrt(mar.df$s_m[1]^2*cv_m^2+s_f^2*
                                       cv_f^2+mar.df$s_r[1]^2*
                                       mar.df$cv_r[1]^2+
                                       2*rho_mf*cv_m*cv_f*
                                       mar.df$s_m[1]*s_f+
                                       2*mar.df$rho_mr[1]*cv_m*mar.df$cv_r[1]*
                                       mar.df$s_m[1]*mar.df$s_r[1]+
                                       2*mar.df$rho_fr[1]*cv_f*
                                       mar.df$cv_r[1]*s_f*mar.df$s_r[1]))

#(4)
#s_f/cv_f
mar.df<-mar.df %>% mutate(cv_41=sqrt(mar.df$s_m[1]^2*mar.df$cv_m[1]^2+s_f^2*
                                       cv_f^2+mar.df$s_r[1]^2*
                                       mar.df$cv_r[1]^2+
                                       2*mar.df$rho_mf[1]*mar.df$cv_m[1]*cv_f*
                                       mar.df$s_m[1]*s_f+
                                       2*mar.df$rho_mr[1]*mar.df$cv_m[1]*mar.df$cv_r[1]*
                                       mar.df$s_m[1]*mar.df$s_r[1]+
                                       2*mar.df$rho_fr[1]*cv_f*
                                       mar.df$cv_r[1]*s_f*mar.df$s_r[1]))
#s_f/cv_f, cv_m
mar.df<-mar.df %>% mutate(cv_42=sqrt(mar.df$s_m[1]^2*cv_m^2+s_f^2*
                                       cv_f^2+mar.df$s_r[1]^2*
                                       mar.df$cv_r[1]^2+
                                       2*mar.df$rho_mf[1]*cv_m*cv_f*
                                       mar.df$s_m[1]*s_f+
                                       2*mar.df$rho_mr[1]*cv_m*mar.df$cv_r[1]*
                                       mar.df$s_m[1]*mar.df$s_r[1]+
                                       2*mar.df$rho_fr[1]*cv_f*
                                       mar.df$cv_r[1]*s_f*mar.df$s_r[1]))

#s_f/cv_f,cv_m, rho_mf
mar.df<-mar.df %>% mutate(cv_43=sqrt(mar.df$s_m[1]^2*cv_m^2+s_f^2*
                                       cv_f^2+mar.df$s_r[1]^2*mar.df$cv_r[1]^2+
                                       2*rho_mf*cv_m*cv_f*
                                       mar.df$s_m[1]*s_f+
                                       2*mar.df$rho_mr[1]*cv_m*mar.df$cv_r[1]*
                                       mar.df$s_m[1]*mar.df$s_r[1]+
                                       2*mar.df$rho_fr[1]*cv_f*
                                       mar.df$cv_r[1]*s_f*mar.df$s_r[1]))

#(5)
#rho_mf
mar.df<-mar.df %>% mutate(cv_51=sqrt(mar.df$s_m[1]^2*mar.df$cv_m[1]^2+mar.df$s_f[1]^2*
                                        mar.df$cv_f[1]^2+mar.df$s_r[1]^2*
                                        mar.df$cv_r[1]^2+
                                        2*rho_mf*mar.df$cv_m[1]*mar.df$cv_f[1]*
                                        mar.df$s_m[1]*mar.df$s_f[1]+
                                        2*mar.df$rho_mr[1]*mar.df$cv_m[1]*mar.df$cv_r[1]*
                                        mar.df$s_m[1]*mar.df$s_r[1]+
                                        2*mar.df$rho_fr[1]*mar.df$cv_f[1]*
                                        mar.df$cv_r[1]*mar.df$s_f[1]*mar.df$s_r[1]))

#rho_mf, cv_m
mar.df<-mar.df %>% mutate(cv_52=sqrt(mar.df$s_m[1]^2*cv_m^2+mar.df$s_f[1]^2*
                                        mar.df$cv_f[1]^2+mar.df$s_r[1]^2*
                                        mar.df$cv_r[1]^2+
                                        2*rho_mf*cv_m*mar.df$cv_f[1]*
                                        mar.df$s_m[1]*mar.df$s_f[1]+
                                        2*mar.df$rho_mr[1]*cv_m*mar.df$cv_r[1]*
                                        mar.df$s_m[1]*mar.df$s_r[1]+
                                        2*mar.df$rho_fr[1]*mar.df$cv_f[1]*
                                        mar.df$cv_r[1]*mar.df$s_f[1]*mar.df$s_r[1]))

#rho_mf, cv_m, s_f/cv_f
mar.df<-mar.df %>% mutate(cv_53=sqrt(mar.df$s_m[1]^2*cv_m^2+s_f^2*
                                        cv_f^2+mar.df$s_r[1]^2*
                                        mar.df$cv_r[1]^2+
                                        2*rho_mf*cv_m*cv_f*
                                        mar.df$s_m[1]*s_f+
                                        2*mar.df$rho_mr[1]*cv_m*mar.df$cv_r[1]*
                                        mar.df$s_m[1]*mar.df$s_r[1]+
                                        2*mar.df$rho_fr[1]*cv_f*
                                        mar.df$cv_r[1]*s_f*mar.df$s_r[1]))


#(6)
#rho_mf
mar.df<-mar.df %>% mutate(cv_61=sqrt(mar.df$s_m[1]^2*mar.df$cv_m[1]^2+mar.df$s_f[1]^2*
                                        mar.df$cv_f[1]^2+mar.df$s_r[1]^2*
                                        mar.df$cv_r[1]^2+
                                        2*rho_mf*mar.df$cv_m[1]*mar.df$cv_f[1]*
                                        mar.df$s_m[1]*mar.df$s_f[1]+
                                        2*mar.df$rho_mr[1]*mar.df$cv_m[1]*mar.df$cv_r[1]*
                                        mar.df$s_m[1]*mar.df$s_r[1]+
                                        2*mar.df$rho_fr[1]*mar.df$cv_f[1]*
                                        mar.df$cv_r[1]*mar.df$s_f[1]*mar.df$s_r[1]))

#rho_mf, cv_m
mar.df<-mar.df %>% mutate(cv_62=sqrt(mar.df$s_m[1]^2*cv_m^2+mar.df$s_f[1]^2*
                                        mar.df$cv_f[1]^2+mar.df$s_r[1]^2*
                                        mar.df$cv_r[1]^2+
                                        2*rho_mf*cv_m*mar.df$cv_f[1]*
                                        mar.df$s_m[1]*mar.df$s_f[1]+
                                        2*mar.df$rho_mr[1]*cv_m*mar.df$cv_r[1]*
                                        mar.df$s_m[1]*mar.df$s_r[1]+
                                        2*mar.df$rho_fr[1]*mar.df$cv_f[1]*
                                        mar.df$cv_r[1]*mar.df$s_f[1]*mar.df$s_r[1]))

#rho_mf, cv_m, s_f/cv_f
mar.df<-mar.df %>% mutate(cv_63=sqrt(mar.df$s_m[1]^2*cv_m^2+s_f^2*
                                        cv_f^2+mar.df$s_r[1]^2*mar.df$cv_r[1]^2+
                                        2*rho_mf*cv_m*cv_f*
                                        mar.df$s_m[1]*s_f+
                                        2*mar.df$rho_mr[1]*cv_m*mar.df$cv_r[1]*
                                        mar.df$s_m[1]*mar.df$s_r[1]+
                                        2*mar.df$rho_fr[1]*cv_f*
                                        mar.df$cv_r[1]*s_f*mar.df$s_r[1]))


#Promedio de cv_m
mar.df<-mar.df %>% mutate(cv_m1=1/5*(cv_11+cv_32+cv_43+cv_52+cv_63))

#Promedio de s_f/cv_f
mar.df<-mar.df %>% mutate(cv_m2=1/5*(cv_12+cv_23+cv_31+cv_53+cv_62))  

#Promedio de rho_mf
mar.df<-mar.df %>% mutate(cv_m3=1/5*(cv_13+cv_22+cv_33+cv_42+cv_51))

#Descartar variables
mar.df<-subset(mar.df, select=-c(cv_11, cv_12, cv_13, cv_21, cv_22, cv_23,
                                 cv_31, cv_32, cv_33, cv_41, cv_42, cv_43,
                                 cv_51, cv_52, cv_53, cv_61, cv_62, cv_63))
