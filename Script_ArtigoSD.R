dados <- dados_completos

#renomeando as variáveis de financiamento 

attach(dados)

str(dados)


dados$v1 <- dados$valor.ComercializaÃ.Äƒo.de.bens.ou.realizaÃ.Äƒo.de.eventos
dados$v2 <- dados$valor.DoaÃ.Å.es.pela.Internet
dados$v3 <- dados$valor.Recursos.de.origens.nÄƒo.identificadas
dados$v4 <- dados$valor.Recursos.de.outros.candidatos.comitÄ.s
dados$v5 <- dados$valor.Recursos.de.partido.polÃ.tico
dados$v6 <- dados$valor.Recursos.de.pessoas.fÃ.sicas
dados$v7 <- dados$valor.Recursos.de.pessoas.jurÃ.dicas
dados$v8 <- dados$valor.Recursos.prÃ³prios
dados$v9 <- dados$valor.Rendimentos.de.aplicaÃ.Å.es.financeiras

#fazendo as variáveis de financiamento

dados$rec_part <- (dados$v4 + dados$v5)
dados$rec_prop <- dados$v8
dados$rec_fis <- dados$v6
dados$rec_jur <- dados$v7
dados$rec_outros <- dados$v3 + dados$v2 + dados$v9
dados$rec_total <- rowSums(dados[  ,68:76], na.rm=TRUE)
dados$rec_ext <- dados$rec_total - dados$rec_part


summary(rec_part)
summary(rec_prop)
summary(rec_fis)
summary(rec_jur)
summary(rec_outros)
summary(rec_total)
summary(rec_ext)


dados$rec_part.log <- log(dados$rec_part)
dados$rec_prop.log <- log(dados$rec_prop)
dados$rec_fis.log <- log(dados$rec_fis)
dados$rec_jur.log <- log(dados$rec_jur)
dados$rec_outros.log <- log(dados$rec_outros)
dados$rec_total.log <- log(dados$rec_total)
dados$rec_ext.log <- log(dados$rec_ext)

#observação: excluímos a v1 por só ter um valor dentre a amostra total.
#vendo quais os logs que ficaram com valores infinitos
summary(rec_part.log)
summary(rec_prop.log)
summary(rec_fis.log)
summary(rec_jur.log)
summary(rec_outros.log)
summary(rec_total.log)
summary(rec_ext.log)


#tirando os infinitos dos logs:
dados$rec_total.log[ is.infinite(dados$rec_total.log) ] <- 0
dados$rec_ext.log[ is.infinite(dados$rec_ext.log) ] <- 0
dados$rec_part.log[ is.infinite(dados$rec_part.log) ] <- 0
dados$rec_fis.log[ is.infinite(dados$rec_fis.log) ] <- 0
dados$rec_jur.log[ is.infinite(dados$rec_jur.log) ] <- 0
dados$rec_prop.log[ is.infinite(dados$rec_prop.log) ] <- 0

#variável pra indicar se foi eleito ou não em 2014
dados$eleito2014 <- ifelse(dados$COD_SIT_TOT_TURNO == 2 | dados$COD_SIT_TOT_TURNO == 3, c("1"), c("0"))
dados$eleito2014 <- as.factor(dados$eleito2014)
summary (eleito2014)

#variável pra indicar se foi candidato ou não em 2010
dados$cand2010 <- ifelse(dados$CODIGO_CARGO_2010 == 6, c("1"), c("0"))
#Deixando os NA como zero porque representam a não-candidatura em 2010:
dados$cand2010[is.na(cand2010)] <- 0
#deixando como factor para ser dummy
dados$cand2010 <- as.factor(cand2010)
summary (cand2010)

#variável pra indicar se foi eleito ou não em 2010
dados$eleito2010 <- ifelse(dados$COD_SIT_TOT_TURNO_2010 == 1 | dados$COD_SIT_TOT_TURNO_2010 == 5, c("1"), c("0"))
#deixando como factor para ser dummy
dados$eleito2010 <- as.factor(eleito2010)
summary (eleito2010)


#variável para identificar reeleição
dados$reeleito2014.f <- ifelse(dados$eleito2010 == 1 & dados$eleito2014 == 1, c("1"), c("0"))
#Deixando os NA como zero porque representam a não-eleição em 2010:
dados$reeleito2014.f[is.na(dados$reeleito2014.f)] <- 0
#deixando como factor para ser dummy
dados$reeleito2014.f <- as.factor(dados$reeleito2014.f)
summary (dados$reeleito2014.f)


#deixando o sexo como factor
dados$sexo.f <- as.factor(dados$DESCRICAO_SEXO)
summary (dados$sexo)

#Sexo dummy
dados$sexo <- as.factor(dados$CODIGO_SEXO)
summary (dados$sexo)

#tirando o log dos votos
dados$votos.log <- as.numeric(log(dados$TOTAL_VOTOS))
dados$votos.log[ is.infinite(dados$votos.log) ] <- 0
summary(dados$votos.log)

#variável pra indicar se recebeu recursos ou não
dados$recurso.f <- ifelse(dados$rec_total.log > 0, c("1"), c("0"))
dados$recurso.f <- as.factor(dados$recurso.f)
summary(recurso.f)

#variável pra indicar se recebeu votos ou não
dados$voto.f <- ifelse(dados$votos.log > 0, c("1"), c("0"))
dados$voto.f <- as.factor(dados$voto.f)
#Deixando os NA como zero porque representam não ter recebido votos em 2014:
dados$voto.f[is.na(dados$voto.f)] <- 0
summary(voto.f)

#variável para identificar candidaturas sem votos e sem recursos
dados$laranja.f <- ifelse(dados$recurso.f == 1 & dados$voto.f == 1, c("0"), c("1"))
dados$laranja.f <- as.factor(dados$laranja.f)
summary(laranja.f)

###DESCRITIVAS###

#correlação entre os recursos e total de votos
cor(rec_total.log, votos.log, use="complete")
cor(rec_jur.log, votos.log, use="complete") 
cor(rec_part.log, votos.log, use="complete")
cor(rec_prop.log, votos.log, use="complete")
cor(rec_fis.log, votos.log, use="complete")
cor(rec_jur.log, votos.log, use="complete")
cor(rec_outros.log, votos.log, use="complete")
cor(rec_ext.log, votos.log, use="complete")



#correlação entre os recursos e a eleição ou não
eleito2014.n <- as.numeric(eleito2014)

cor(rec_total.log, eleito2014.n, use="complete")
cor(rec_jur.log, eleito2014.n, use="complete") 
cor(rec_part.log, eleito2014.n, use="complete")
cor(rec_prop.log, eleito2014.n, use="complete")
cor(rec_fis.log, eleito2014.n, use="complete")
cor(rec_jur.log, eleito2014.n, use="complete")
cor(rec_outros.log, eleito2014.n, use="complete")
cor(rec_ext.log, eleito2014.n, use="complete")

#teste de independência entre ser eleito e o sexo
tbl_eleito2014_sexo.f <- table(eleito2014, sexo.f)
tbl_eleito2014_sexo.f
chisq.test(tbl_eleito2014_sexo.f)

#teste de independência entre ter recebido votos e o sexo
tbl_voto.f_sexo.f <- table(voto.f, sexo.f)
tbl_voto.f_sexo.f
chisq.test(tbl_voto.f_sexo.f)

#teste de independência entre ter recebido recursos e o sexo
tbl_recurso.f_sexo.f <- table(recurso.f, sexo.f)
tbl_recurso.f_sexo.f
chisq.test(tbl_recurso.f_sexo.f)

#teste de independência entre ser "laranja" e o sexo
tbl_laranja.f_sexo.f <- table(laranja.f, sexo.f)
tbl_laranja.f_sexo.f
chisq.test(tbl_laranja.f_sexo.f)

##fazendo boxplot maravilhoso##
#queremos ver a distribuição dos recursos entre eleitos e não eleitos

#deixando a variável pra eleito com label
dados$eleito2014 <- ifelse(dados$eleito2014 == 1, c("Eleito"), c("Não-Eleito"))

#deixando os dados do jeito que precisa pro ggplot, a partir da função melt
#a idéia é separar a variável categórica pela qual você quer comparar os dados
require(reshape2)
dados_recursos_sexo <- data.frame(sexo.f, rec_total.log, rec_part.log, rec_jur.log, rec_fis.log,rec_prop.log)
dados_recursos_sexo <- melt(dados_recursos_sexo, id.var = "sexo.f")

#rodando o gráfico maravilhoso
require(ggplot2)
p <- ggplot(data = dados_recursos_sexoPT, aes(x=variable, y=value)) + geom_boxplot(aes(fill=sexo.f))
p <- p + xlab("Tipo de Recuro") + ylab("Log dos Valores da Receita") + ggtitle("Distribuição dos Tipos de Recurso por Sexo - PT")
p <- p + guides(fill=guide_legend(title="Resultado"))
plot(p)


##scatter plot maravilhoso de correlação 
p <- qplot(rec_prop.log, votos.log,  size = I(1.5), alpha = I(0.4), colour = factor(sexo), shape = factor (eleito2014), na.rm=TRUE) 
#mudandoa a cor
p <- p + scale_colour_brewer(palette="Set1") +  scale_shape_manual(values=c(4, 19))
#adicionando a reta de correlação
lm(votos.log ~ rec_prop.log)
p <- p + geom_abline(intercept = 3.4150, slope = 0.5702, color="black", alpha = I(0.5)) 
#colocando legendas
p <- p + xlab("Recursos Próprios (log)") + ylab("Número de Votos (log)") + ggtitle("Distribuição dos Recursos Próprios Por votos")
plot(p)


##scatter plot maravilhoso scatter
p <- qplot(laranja.f, size = I(1.5), alpha = I(0.4), colour = factor(laranja.f), na.rm=TRUE) 
#mudandoa a cor
p <- p + scale_colour_brewer(palette="Set1") 
plot(p)


#gráfico de barras pros testes qui-quadrado:

#taxa de sucesso
df <- data.frame(a = c("Homens", "Mulheres", "Total"), b = c(10.49, 2.30, 7.74))
a <- ggplot(df, aes(a, b)) + geom_bar(stat = "identity") 
a <- a + xlab("Candidatura") + ylab("Porcentagem de Eleitos") + ggtitle("Taxa de Sucesso das candidaturas por sexo")
plot(a)

#candidaturas com recursos
df <- data.frame(a = c("Homens", "Mulheres", "Total"), b = c(81.26, 69.32, 77.46))
b <- ggplot(df, aes(a, b)) + geom_bar(stat = "identity") 
b <- b + xlab("Candidatura") + ylab("Porcentagem") + ggtitle("Candidaturas que receberam financiamento")
plot(b)

#candidaturas que receberam votos
df <- data.frame(a = c("Homens", "Mulheres", "Total"), b = c(85.17, 75.84, 82.20))
c <- ggplot(df, aes(a, b)) + geom_bar(stat = "identity") 
c <- c +  ylab("Porcentagem") + ggtitle("Candidaturas que receberam votos")
plot(c)

#candidaturas sem votos e sem recursos
df <- data.frame(a = c("Homens", "Mulheres", "Total"), b = c(26.11, 39.08, 30.24))
d <- ggplot(df, aes(a, b)) + geom_bar(stat = "identity") 
d <- d +  ylab("Porcentagem") + ggtitle("Candidaturas sem financiamento e sem votos")
plot(d)

install.packages("cowplot")
require("cowplot")
plot_grid(a, b, c, d, ncol = 2, nrow = 2)

#vendo por partido
dadosPT <- subset(dados, NUMERO_PARTIDO == 13)
dadosPSDB <- subset(dados, NUMERO_PARTIDO == 45)
dadosPMDB <- subset(dados,NUMERO_PARTIDO == 15)
dadosPP <- subset(dados,NUMERO_PARTIDO == 11)


##BOXPLOT ENTRE SEXO E RECURSOS##

###Para o PT:
require(reshape2)
dados_recursos_sexoPT <- data.frame(dadosPT$sexo.f, dadosPT$rec_total.log, dadosPT$rec_part.log, dadosPT$rec_jur.log, dadosPT$rec_fis.log, dadosPT$rec_prop.log)
dados_recursos_sexoPT <- melt(dados_recursos_sexoPT, id.var = "dadosPT.sexo.f")
#rodando o gráfico maravilhoso
require(ggplot2)
a <- ggplot(data = dados_recursos_sexoPT, aes(x=variable, y=value)) + geom_boxplot(aes(fill=dadosPT.sexo.f))
a <- a + xlab("Tipo de Recuro") + ylab("Log dos Valores da Receita") + ggtitle("Distribuição dos Tipos de Recurso por Sexo - PT")
a <- a + guides(fill=guide_legend(title="Resultado")) 
plot(a)

###Para o PSDB:
require(reshape2)
dados_recursos_sexoPSDB <- data.frame(dadosPSDB$sexo.f, dadosPSDB$rec_total.log, dadosPSDB$rec_part.log, dadosPSDB$rec_jur.log, dadosPSDB$rec_fis.log, dadosPSDB$rec_prop.log)
dados_recursos_sexoPSDB <- melt(dados_recursos_sexoPSDB, id.var = "dadosPSDB.sexo.f")
#rodando o gráfico maravilhoso
require(ggplot2)
b <- ggplot(data = dados_recursos_sexoPSDB, aes(x=variable, y=value)) + geom_boxplot(aes(fill=dadosPSDB.sexo.f))
b <- b + xlab("Tipo de Recuro") + ylab("Log dos Valores da Receita") + ggtitle("Distribuição dos Tipos de Recurso por Sexo - PSDB")
b <- b + guides(fill=guide_legend(title="Resultado"))
plot(b)

###Para o PMDB:
require(reshape2)
dados_recursos_sexoPMDB <- data.frame(dadosPMDB$sexo.f, dadosPMDB$rec_total.log, dadosPMDB$rec_part.log, dadosPMDB$rec_jur.log, dadosPMDB$rec_fis.log, dadosPMDB$rec_prop.log)
dados_recursos_sexoPMDB <- melt(dados_recursos_sexoPMDB, id.var = "dadosPMDB.sexo.f")
#rodando o gráfico maravilhoso
require(ggplot2)
c <- ggplot(data = dados_recursos_sexoPMDB, aes(x=variable, y=value)) + geom_boxplot(aes(fill=dadosPMDB.sexo.f))
c <- c + xlab("Tipo de Recuro") + ylab("Log dos Valores da Receita") + ggtitle("Distribuição dos Tipos de Recurso por Sexo - PMDB")
c <- c + guides(fill=guide_legend(title="Resultado"))
plot(c)

###Para o PP:
require(reshape2)
dados_recursos_sexoPP <- data.frame(dadosPP$sexo.f, dadosPP$rec_total.log, dadosPP$rec_part.log, dadosPP$rec_jur.log, dadosPP$rec_fis.log, dadosPP$rec_prop.log)
dados_recursos_sexoPP <- melt(dados_recursos_sexoPP, id.var = "dadosPP.sexo.f")
#rodando o gráfico maravilhoso
require(ggplot2)
d <- ggplot(data = dados_recursos_sexoPP, aes(x=variable, y=value)) + geom_boxplot(aes(fill=dadosPP.sexo.f))
d <- d + xlab("Tipo de Recuro") + ylab("Log dos Valores da Receita") + ggtitle("Distribuição dos Tipos de Recurso por Sexo - PP")
d <- d + guides(fill=guide_legend(title="Resultado"))
plot(d)

plot_grid(a, b, c, d, ncol = 2, nrow = 2)


##BOXPLOT ENTRE ELEITOS E RECURSOS##
###Para o PT:
require(reshape2)
dados_recursos_eleitoPT <- data.frame(dadosPT$eleito2014, dadosPT$rec_total.log, dadosPT$rec_part.log, dadosPT$rec_jur.log, dadosPT$rec_fis.log, dadosPT$rec_prop.log)
dados_recursos_eleitoPT <- melt(dados_recursos_eleitoPT, id.var = "dadosPT.eleito2014")
#rodando o gráfico maravilhoso
require(ggplot2)
a <- ggplot(data = dados_recursos_eleitoPT, aes(x=variable, y=value)) + geom_boxplot(aes(fill=dadosPT.eleito2014))
a <- a + xlab("Tipo de Recuro") + ylab("Log dos Valores da Receita") + ggtitle("Distribuição dos Tipos de Recurso por Eleito - PT")
a <- a + guides(fill=guide_legend(title="Resultado")) 
plot(a)

###Para o PSDB:
require(reshape2)
dados_recursos_eleitoPSDB <- data.frame(dadosPSDB$eleito2014, dadosPSDB$rec_total.log, dadosPSDB$rec_part.log, dadosPSDB$rec_jur.log, dadosPSDB$rec_fis.log, dadosPSDB$rec_prop.log)
dados_recursos_eleitoPSDB <- melt(dados_recursos_eleitoPSDB, id.var = "dadosPSDB.eleito2014")
#rodando o gráfico maravilhoso
require(ggplot2)
b <- ggplot(data = dados_recursos_eleitoPSDB, aes(x=variable, y=value)) + geom_boxplot(aes(fill=dadosPSDB.eleito2014))
b <- b + xlab("Tipo de Recuro") + ylab("Log dos Valores da Receita") + ggtitle("Distribuição dos Tipos de Recurso por Eleito - PSDB")
b <- b + guides(fill=guide_legend(title="Resultado"))
plot(b)

###Para o PMDB:
require(reshape2)
dados_recursos_eleitoPMDB <- data.frame(dadosPMDB$eleito2014, dadosPMDB$rec_total.log, dadosPMDB$rec_part.log, dadosPMDB$rec_jur.log, dadosPMDB$rec_fis.log, dadosPMDB$rec_prop.log)
dados_recursos_eleitoPMDB <- melt(dados_recursos_eleitoPMDB, id.var = "dadosPMDB.eleito2014")
#rodando o gráfico maravilhoso
require(ggplot2)
c <- ggplot(data = dados_recursos_eleitoPMDB, aes(x=variable, y=value)) + geom_boxplot(aes(fill=dadosPMDB.eleito2014))
c <- c + xlab("Tipo de Recuro") + ylab("Log dos Valores da Receita") + ggtitle("Distribuição dos Tipos de Recurso por Eleito - PMDB")
c <- c + guides(fill=guide_legend(title="Resultado"))
plot(c)

###Para o PP:
require(reshape2)
dados_recursos_eleitoPP <- data.frame(dadosPP$eleito2014, dadosPP$rec_total.log, dadosPP$rec_part.log, dadosPP$rec_jur.log, dadosPP$rec_fis.log, dadosPP$rec_prop.log)
dados_recursos_eleitoPP <- melt(dados_recursos_eleitoPP, id.var = "dadosPP.eleito2014")
#rodando o gráfico maravilhoso
require(ggplot2)
d <- ggplot(data = dados_recursos_eleitoPP, aes(x=variable, y=value)) + geom_boxplot(aes(fill=dadosPP.eleito2014))
d <- d + xlab("Tipo de Recuro") + ylab("Log dos Valores da Receita") + ggtitle("Distribuição dos Tipos de Recurso por Eleito - PP")
d <- d + guides(fill=guide_legend(title="Resultado"))
plot(d)

plot_grid(a, b, c, d, ncol = 2, nrow = 2)

###CORRELAÇÃO ENTRE VOTOS E RECURSOS###

##PT##
##Total
T_PT <- qplot(dadosPT$rec_total.log, dadosPT$votos.log,  size = I(2), alpha = I(0.6), colour = factor(dadosPT$sexo.f), shape = factor (dadosPT$eleito2014), na.rm=TRUE) 
#mudando a a cor
T_PT <- T_PT + scale_colour_brewer(palette="Set1") +  scale_shape_manual(values=c(19, 4))
#adicionando a reta de correlação
lm(dadosPT$votos.log ~ dadosPT$rec_total.log)
T_PT <- T_PT + geom_abline(intercept = 4.1803, slope = 0.4408, color="black", alpha = I(0.5))
#colocando legendas
T_PT <- T_PT + xlab("Recursos Totais (log)") + ylab("Número de Votos (log)") + ggtitle("Distribuição dos Recursos Totais Por votos - PT")


##Partidários
P_PT <- qplot(dadosPT$rec_part.log, dadosPT$votos.log,  size = I(2.5), alpha = I(0.8), colour = factor(dadosPT$sexo.f), shape = factor (dadosPT$eleito2014), na.rm=TRUE) 
#mudando a a cor
P_PT <- P_PT + scale_colour_brewer(palette="Set1") +  scale_shape_manual(values=c(19, 4))
#adicionando a reta de correlação
lm(dadosPT$votos.log ~ dadosPT$rec_part.log)
P_PT <- P_PT + geom_abline(intercept = 1.5727, slope = 0.7259, color="black", alpha = I(0.5))
#colocando legendas
P_PT <- P_PT + xlab("Recursos Partidários (log)") + ylab("Número de Votos (log)") + ggtitle("Distribuição dos Recursos Partidários Por votos - PT")


##Pessoas Jurídicas
J_PT <- qplot(dadosPT$rec_jur.log, dadosPT$votos.log,  size = I(2.5), alpha = I(0.8), colour = factor(dadosPT$sexo.f), shape = factor (dadosPT$eleito2014), na.rm=TRUE) 
#mudando a a cor
J_PT <- J_PT + scale_colour_brewer(palette="Set1") +  scale_shape_manual(values=c(19, 4))
#adicionando a reta de correlação
lm(dadosPT$votos.log ~ dadosPT$rec_jur.log)
J_PT <- J_PT + geom_abline(intercept = 5.2639, slope = 0.4575, color="black", alpha = I(0.5))
#colocando legendas
J_PT <- J_PT + xlab("Recursos de Pessoas Jurídicas (log)") + ylab("Número de Votos (log)") + ggtitle("Distribuição dos Recursos de Pessoas Jurídicas Por votos - PT")


##Pessoas Físicas
F_PT <- qplot(dadosPT$rec_fis.log, dadosPT$votos.log,  size = I(2.5), alpha = I(0.8), colour = factor(dadosPT$sexo.f), shape = factor (dadosPT$eleito2014), na.rm=TRUE) 
#mudando a a cor
F_PT <- F_PT + scale_colour_brewer(palette="Set1") +  scale_shape_manual(values=c(19, 4))
#adicionando a reta de correlação
lm(dadosPT$votos.log ~ dadosPT$rec_fis.log)
F_PT <- F_PT + geom_abline(intercept = 1.9444, slope = 0.7439, color="black", alpha = I(0.5))
#colocando legendas
F_PT <- F_PT + xlab("Recursos de Pessoas Físicas (log)") + ylab("Número de Votos (log)") + ggtitle("Distribuição dos Recursos de Pessoas Físicas Por votos - PT")


##Recursos Próprios
Pp_PT <- qplot(dadosPT$rec_prop.log, dadosPT$votos.log,  size = I(2.5), alpha = I(0.8), colour = factor(dadosPT$sexo.f), shape = factor (dadosPT$eleito2014), na.rm=TRUE) 
#mudando a a cor
Pp_PT <- Pp_PT + scale_colour_brewer(palette="Set1") +  scale_shape_manual(values=c(19, 4))
#adicionando a reta de correlação
lm(dadosPT$votos.log ~ dadosPT$rec_prop.log)
Pp_PT <- Pp_PT + geom_abline(intercept = 4.8453, slope = 0.5127, color="black", alpha = I(0.5))
#colocando legendas
Pp_PT <- Pp_PT + xlab("Recursos Próprios (log)") + ylab("Número de Votos (log)") + ggtitle("Distribuição dos Recursos Próprios Por votos - PT")


##PMDB##
##Total
T_PMDB <- qplot(dadosPMDB$rec_total.log, dadosPMDB$votos.log,  size = I(2), alpha = I(0.6), colour = factor(dadosPMDB$sexo.f), shape = factor (dadosPMDB$eleito2014), na.rm=TRUE) 
#mudando a a cor
T_PMDB <- T_PMDB + scale_colour_brewer(palette="Set1") +  scale_shape_manual(values=c(19, 4))
#adicionando a reta de correlação
lm(dadosPMDB$votos.log ~ dadosPMDB$rec_total.log)
T_PMDB <- T_PMDB + geom_abline(intercept = 3.4722, slope = 0.4624, color="black", alpha = I(0.5))
#colocando legendas
T_PMDB <- T_PMDB + xlab("Recursos Totais (log)") + ylab("Número de Votos (log)") + ggtitle("Distribuição dos Recursos Totais Por votos - PMDB")


##Partidários
P_PMDB <- qplot(dadosPMDB$rec_part.log, dadosPMDB$votos.log,  size = I(2.5), alpha = I(0.8), colour = factor(dadosPMDB$sexo.f), shape = factor (dadosPMDB$eleito2014), na.rm=TRUE) 
#mudando a a cor
P_PMDB <- P_PMDB + scale_colour_brewer(palette="Set1") +  scale_shape_manual(values=c(19, 4))
#adicionando a reta de correlação
lm(dadosPMDB$votos.log ~ dadosPMDB$rec_part.log)
P_PMDB <- P_PMDB + geom_abline(intercept = -2.760, slope = 1.012, color="black", alpha = I(0.5))
#colocando legendas
P_PMDB <- P_PMDB + xlab("Recursos Partidários (log)") + ylab("Número de Votos (log)") + ggtitle("Distribuição dos Recursos Partidários Por votos - PMDB")


##Pessoas Jurídicas
J_PMDB <- qplot(dadosPMDB$rec_jur.log, dadosPMDB$votos.log,  size = I(2.5), alpha = I(0.8), colour = factor(dadosPMDB$sexo.f), shape = factor (dadosPMDB$eleito2014), na.rm=TRUE) 
#mudando a a cor
J_PMDB <- J_PMDB + scale_colour_brewer(palette="Set1") +  scale_shape_manual(values=c(19, 4))
#adicionando a reta de correlação
lm(dadosPMDB$votos.log ~ dadosPMDB$rec_jur.log)
J_PMDB <- J_PMDB + geom_abline(intercept = 3.4755, slope = 0.5921, color="black", alpha = I(0.5))
#colocando legendas
J_PMDB <- J_PMDB + xlab("Recursos de Pessoas Jurídicas (log)") + ylab("Número de Votos (log)") + ggtitle("Distribuição dos Recursos de Pessoas Jurídicas Por votos - PMDB")


##Pessoas Físicas
F_PMDB <- qplot(dadosPMDB$rec_fis.log, dadosPMDB$votos.log,  size = I(2.5), alpha = I(0.8), colour = factor(dadosPMDB$sexo.f), shape = factor (dadosPMDB$eleito2014), na.rm=TRUE) 
#mudando a a cor
F_PMDB <- F_PMDB + scale_colour_brewer(palette="Set1") +  scale_shape_manual(values=c(19, 4))
#adicionando a reta de correlação
lm(dadosPMDB$votos.log ~ dadosPMDB$rec_fis.log)
F_PMDB <- F_PMDB + geom_abline(intercept = 3.0355, slope = 0.6409, color="black", alpha = I(0.5))
#colocando legendas
F_PMDB <- F_PMDB + xlab("Recursos de Pessoas Físicas (log)") + ylab("Número de Votos (log)") + ggtitle("Distribuição dos Recursos de Pessoas Físicas Por votos - PMDB")


##Recursos Próprios
Pp_PMDB <- qplot(dadosPMDB$rec_prop.log, dadosPMDB$votos.log,  size = I(2.5), alpha = I(0.8), colour = factor(dadosPMDB$sexo.f), shape = factor (dadosPMDB$eleito2014), na.rm=TRUE) 
#mudando a a cor
Pp_PMDB <- Pp_PMDB + scale_colour_brewer(palette="Set1") +  scale_shape_manual(values=c(19, 4))
#adicionando a reta de correlação
lm(dadosPMDB$votos.log ~ dadosPMDB$rec_prop.log)
Pp_PMDB <- Pp_PMDB + geom_abline(intercept = 3.2412, slope = 0.6191, color="black", alpha = I(0.5))
#colocando legendas
Pp_PMDB <- Pp_PMDB + xlab("Recursos Próprios (log)") + ylab("Número de Votos (log)") + ggtitle("Distribuição dos Recursos Próprios Por votos - PMDB")


##PSDB##
##Total
T_PSDB <- qplot(dadosPSDB$rec_total.log, dadosPSDB$votos.log,  size = I(2), alpha = I(0.6), colour = factor(dadosPSDB$sexo.f), shape = factor (dadosPSDB$eleito2014), na.rm=TRUE) 
#mudando a a cor
T_PSDB <- T_PSDB + scale_colour_brewer(palette="Set1") +  scale_shape_manual(values=c(19, 4))
#adicionando a reta de correlação
lm(dadosPSDB$votos.log ~ dadosPSDB$rec_total.log)
T_PSDB <- T_PSDB + geom_abline(intercept = 3.7865, slope = 0.44557, color="black", alpha = I(0.5))
#colocando legendas
T_PSDB <- T_PSDB + xlab("Recursos Totais (log)") + ylab("Número de Votos (log)") + ggtitle("Distribuição dos Recursos Totais Por votos - PSDB")

##Partidários
P_PSDB <- qplot(dadosPSDB$rec_part.log, dadosPSDB$votos.log,  size = I(2.5), alpha = I(0.8), colour = factor(dadosPSDB$sexo.f), shape = factor (dadosPSDB$eleito2014), na.rm=TRUE) 
#mudando a a cor
P_PSDB <- P_PSDB + scale_colour_brewer(palette="Set1") +  scale_shape_manual(values=c(19, 4))
#adicionando a reta de correlação
lm(dadosPSDB$votos.log ~ dadosPSDB$rec_part.log)
P_PSDB <- P_PSDB + geom_abline(intercept = 0.2216, slope = 0.8295, color="black", alpha = I(0.5))
#colocando legendas
P_PSDB <- P_PSDB + xlab("Recursos Partidários (log)") + ylab("Número de Votos (log)") + ggtitle("Distribuição dos Recursos Partidários Por votos - PSDB")


##Pessoas Jurídicas
J_PSDB <- qplot(dadosPSDB$rec_jur.log, dadosPSDB$votos.log,  size = I(2.5), alpha = I(0.8), colour = factor(dadosPSDB$sexo.f), shape = factor (dadosPSDB$eleito2014), na.rm=TRUE) 
#mudando a a cor
J_PSDB <- J_PSDB + scale_colour_brewer(palette="Set1") +  scale_shape_manual(values=c(19, 4))
#adicionando a reta de correlação
lm(dadosPSDB$votos.log ~ dadosPSDB$rec_jur.log)
J_PSDB <- J_PSDB + geom_abline(intercept = 2.9004, slope = 0.6292, color="black", alpha = I(0.5))
#colocando legendas
J_PSDB <- J_PSDB + xlab("Recursos de Pessoas Jurídicas (log)") + ylab("Número de Votos (log)") + ggtitle("Distribuição dos Recursos de Pessoas Jurídicas Por votos - PSDB")


##Pessoas Físicas
F_PSDB <- qplot(dadosPSDB$rec_fis.log, dadosPSDB$votos.log,  size = I(2.5), alpha = I(0.8), colour = factor(dadosPSDB$sexo.f), shape = factor (dadosPSDB$eleito2014), na.rm=TRUE) 
#mudando a a cor
F_PSDB <- F_PSDB + scale_colour_brewer(palette="Set1") +  scale_shape_manual(values=c(19, 4))
#adicionando a reta de correlação
lm(dadosPSDB$votos.log ~ dadosPSDB$rec_fis.log)
F_PSDB <- F_PSDB + geom_abline(intercept = 2.4607, slope = 0.7015, color="black", alpha = I(0.5))
#colocando legendas
F_PSDB <- F_PSDB + xlab("Recursos de Pessoas Físicas (log)") + ylab("Número de Votos (log)") + ggtitle("Distribuição dos Recursos de Pessoas Físicas Por votos - PSDB")


##Recursos Próprios
Pp_PSDB <- qplot(dadosPSDB$rec_prop.log, dadosPSDB$votos.log,  size = I(2.5), alpha = I(0.8), colour = factor(dadosPSDB$sexo.f), shape = factor (dadosPSDB$eleito2014), na.rm=TRUE) 
#mudando a a cor
Pp_PSDB <- Pp_PSDB + scale_colour_brewer(palette="Set1") +  scale_shape_manual(values=c(19, 4))
#adicionando a reta de correlação
lm(dadosPSDB$votos.log ~ dadosPSDB$rec_prop.log)
Pp_PSDB <- Pp_PSDB + geom_abline(intercept = 4.074, slope = 0.559, color="black", alpha = I(0.5))
#colocando legendas
Pp_PSDB <- Pp_PSDB + xlab("Recursos Próprios (log)") + ylab("Número de Votos (log)") + ggtitle("Distribuição dos Recursos Próprios Por votos - PSDB")


##PP##
##Total
T_PP <- qplot(dadosPP$rec_total.log, dadosPP$votos.log,  size = I(2), alpha = I(0.6), colour = factor(dadosPP$sexo.f), shape = factor (dadosPP$eleito2014), na.rm=TRUE) 
#mudando a a cor
T_PP <- T_PP + scale_colour_brewer(palette="Set1") +  scale_shape_manual(values=c(19, 4))
#adicionando a reta de correlação
lm(dadosPP$votos.log ~ dadosPP$rec_total.log)
T_PP <- T_PP + geom_abline(intercept = 2.5537, slope = 0.5447, color="black", alpha = I(0.5))
#colocando legendas
T_PP <- T_PP + xlab("Recursos Totais (log)") + ylab("Número de Votos (log)") + ggtitle("Distribuição dos Recursos Totais Por votos - PP")


##Partidários
P_PP <- qplot(dadosPP$rec_part.log, dadosPP$votos.log,  size = I(2.5), alpha = I(0.8), colour = factor(dadosPP$sexo.f), shape = factor (dadosPP$eleito2014), na.rm=TRUE) 
#mudando a a cor
P_PP <- P_PP + scale_colour_brewer(palette="Set1") +  scale_shape_manual(values=c(19, 4))
#adicionando a reta de correlação
lm(dadosPP$votos.log ~ dadosPP$rec_part.log)
P_PP <- P_PP + geom_abline(intercept = -1.5046, slope = 0.9285, color="black", alpha = I(0.5))
#colocando legendas
P_PP <- P_PP + xlab("Recursos Partidários (log)") + ylab("Número de Votos (log)") + ggtitle("Distribuição dos Recursos Partidários Por votos - PP")


##Pessoas Jurídicas
J_PP <- qplot(dadosPP$rec_jur.log, dadosPP$votos.log,  size = I(2.5), alpha = I(0.8), colour = factor(dadosPP$sexo.f), shape = factor (dadosPP$eleito2014), na.rm=TRUE) 
#mudando a a cor
J_PP <- J_PP + scale_colour_brewer(palette="Set1") +  scale_shape_manual(values=c(19, 4))
#adicionando a reta de correlação
lm(dadosPP$votos.log ~ dadosPP$rec_jur.log)
J_PP <- J_PP + geom_abline(intercept = 4.409, slope = 0.527, color="black", alpha = I(0.5))
#colocando legendas
J_PP <- J_PP + xlab("Recursos de Pessoas Jurídicas (log)") + ylab("Número de Votos (log)") + ggtitle("Distribuição dos Recursos de Pessoas Jurídicas Por votos - PP")


##Pessoas Físicas
F_PP <- qplot(dadosPP$rec_fis.log, dadosPP$votos.log,  size = I(2.5), alpha = I(0.8), colour = factor(dadosPP$sexo.f), shape = factor (dadosPP$eleito2014), na.rm=TRUE) 
#mudando a a cor
F_PP <- F_PP + scale_colour_brewer(palette="Set1") +  scale_shape_manual(values=c(19, 4))
#adicionando a reta de correlação
lm(dadosPP$votos.log ~ dadosPP$rec_fis.log)
F_PP <- F_PP + geom_abline(intercept = 0.8495, slope = 0.8843, color="black", alpha = I(0.5))
#colocando legendas
F_PP <- F_PP + xlab("Recursos de Pessoas Físicas (log)") + ylab("Número de Votos (log)") + ggtitle("Distribuição dos Recursos de Pessoas Físicas Por votos - PP")


##Recursos Próprios
Pp_PP <- qplot(dadosPP$rec_prop.log, dadosPP$votos.log,  size = I(2.5), alpha = I(0.8), colour = factor(dadosPP$sexo.f), shape = factor (dadosPP$eleito2014), na.rm=TRUE) 
#mudando a a cor
Pp_PP <- Pp_PP + scale_colour_brewer(palette="Set1") +  scale_shape_manual(values=c(19, 4))
#adicionando a reta de correlação
lm(dadosPP$votos.log ~ dadosPP$rec_prop.log)
Pp_PP <- Pp_PP + geom_abline(intercept = 3.8246, slope = 0.6022, color="black", alpha = I(0.5))
#colocando legendas
Pp_PP <- Pp_PP + xlab("Recursos Próprios (log)") + ylab("Número de Votos (log)") + ggtitle("Distribuição dos Recursos Próprios Por votos - PP")


###Colocando os scatter plots recursos x votos agrupados

#Por recursos Totais
T_partidos <- plot_grid(T_PT, T_PSDB, T_PMDB, T_PP, ncol = 2, nrow = 2)
plot(T_partidos)

#Por Recursos Partidários
P_partidos <- plot_grid(P_PT, P_PSDB, P_PMDB, P_PP, ncol = 2, nrow = 2)
plot(P_partidos)

#Por Recursos de Pessoa Jurídica
J_partidos <- plot_grid(J_PT, J_PSDB, J_PMDB, J_PP, ncol = 2, nrow = 2)
plot(J_partidos)

#Por Recursos de Pessoa Física
F_partidos <- plot_grid(F_PT, F_PSDB, F_PMDB, F_PP, ncol = 2, nrow = 2)
plot(F_partidos)

#Por Recursos Próprios
Pp_partidos <- plot_grid(Pp_PT, Pp_PSDB, Pp_PMDB, Pp_PP, ncol = 2, nrow = 2)
plot(Pp_partidos)

##CORRELAÇÕES POR PARTIDO
#PT
cor(dadosPT$rec_total.log, dadosPT$votos.log, use="complete")
cor(dadosPT$rec_part.log, dadosPT$votos.log, use="complete")
cor(dadosPT$rec_jur.log, dadosPT$votos.log, use="complete") 
cor(dadosPT$rec_fis.log, dadosPT$votos.log, use="complete")
cor(dadosPT$rec_prop.log, dadosPT$votos.log, use="complete")
#PSDB
cor(dadosPSDB$rec_total.log, dadosPSDB$votos.log, use="complete")
cor(dadosPSDB$rec_part.log, dadosPSDB$votos.log, use="complete")
cor(dadosPSDB$rec_jur.log, dadosPSDB$votos.log, use="complete") 
cor(dadosPSDB$rec_fis.log, dadosPSDB$votos.log, use="complete")
cor(dadosPSDB$rec_prop.log, dadosPSDB$votos.log, use="complete")
#PMDB
cor(dadosPMDB$rec_total.log, dadosPMDB$votos.log, use="complete")
cor(dadosPMDB$rec_part.log, dadosPMDB$votos.log, use="complete")
cor(dadosPMDB$rec_jur.log, dadosPMDB$votos.log, use="complete") 
cor(dadosPMDB$rec_fis.log, dadosPMDB$votos.log, use="complete")
cor(dadosPMDB$rec_prop.log, dadosPMDB$votos.log, use="complete")
#PP
cor(dadosPP$rec_total.log, dadosPP$votos.log, use="complete")
cor(dadosPP$rec_part.log, dadosPP$votos.log, use="complete")
cor(dadosPP$rec_jur.log, dadosPP$votos.log, use="complete") 
cor(dadosPP$rec_fis.log, dadosPP$votos.log, use="complete")
cor(dadosPP$rec_prop.log, dadosPP$votos.log, use="complete")


##retas de regressão pra cada partido

##PT##
reg.voto.rec_total_PT <- lm(dadosPT$votos.log ~ dadosPT$rec_total.log)
reg.voto.rec_part_PT <- lm(dadosPT$votos.log ~ dadosPT$rec_part.log)
reg.voto.rec_jur_PT <- lm(dadosPT$votos.log ~ dadosPT$rec_jur.log)
reg.voto.rec_fis_PT <- lm(dadosPT$votos.log ~ dadosPT$rec_fis.log)
reg.voto.rec_prop_PT <- lm(dadosPT$votos.log ~ dadosPT$rec_prop.log)
summary(reg.voto.rec_total_PT)
summary(reg.voto.rec_part_PT)
summary(reg.voto.rec_jur_PT)
summary(reg.voto.rec_fis_PT)
summary(reg.voto.rec_prop_PT)

##PSDB##
reg.voto.rec_total_PSDB <- lm(dadosPSDB$votos.log ~ dadosPSDB$rec_total.log)
reg.voto.rec_part_PSDB <- lm(dadosPSDB$votos.log ~ dadosPSDB$rec_part.log)
reg.voto.rec_jur_PSDB <- lm(dadosPSDB$votos.log ~ dadosPSDB$rec_jur.log)
reg.voto.rec_fis_PSDB <- lm(dadosPSDB$votos.log ~ dadosPSDB$rec_fis.log)
reg.voto.rec_prop_PSDB <- lm(dadosPSDB$votos.log ~ dadosPSDB$rec_prop.log)
summary(reg.voto.rec_total_PSDB)
summary(reg.voto.rec_part_PSDB)
summary(reg.voto.rec_jur_PSDB)
summary(reg.voto.rec_fis_PSDB)
summary(reg.voto.rec_prop_PSDB)

##PMDB##
reg.voto.rec_total_PMDB <- lm(dadosPMDB$votos.log ~ dadosPMDB$rec_total.log)
reg.voto.rec_part_PMDB <- lm(dadosPMDB$votos.log ~ dadosPMDB$rec_part.log)
reg.voto.rec_jur_PMDB <- lm(dadosPMDB$votos.log ~ dadosPMDB$rec_jur.log)
reg.voto.rec_fis_PMDB <- lm(dadosPMDB$votos.log ~ dadosPMDB$rec_fis.log)
reg.voto.rec_prop_PMDB <- lm(dadosPMDB$votos.log ~ dadosPMDB$rec_prop.log)
summary(reg.voto.rec_total_PMDB)
summary(reg.voto.rec_part_PMDB)
summary(reg.voto.rec_jur_PMDB)
summary(reg.voto.rec_fis_PMDB)
summary(reg.voto.rec_prop_PMDB)

##PP##
reg.voto.rec_total_PP <- lm(dadosPP$votos.log ~ dadosPP$rec_total.log)
reg.voto.rec_part_PP <- lm(dadosPP$votos.log ~ dadosPP$rec_part.log)
reg.voto.rec_jur_PP <- lm(dadosPP$votos.log ~ dadosPP$rec_jur.log)
reg.voto.rec_fis_PP <- lm(dadosPP$votos.log ~ dadosPP$rec_fis.log)
reg.voto.rec_prop_PP <- lm(dadosPP$votos.log ~ dadosPP$rec_prop.log)
summary(reg.voto.rec_total_PP)
summary(reg.voto.rec_part_PP)
summary(reg.voto.rec_jur_PP)
summary(reg.voto.rec_fis_PP)
summary(reg.voto.rec_prop_PP)

##TOTAIS##
reg.voto.rec_total <- lm(dados$votos.log ~ dados$rec_total.log)
reg.voto.rec_part <- lm(dados$votos.log ~ dados$rec_part.log)
reg.voto.rec_jur <- lm(dados$votos.log ~ dados$rec_jur.log)
reg.voto.rec_fis <- lm(dados$votos.log ~ dados$rec_fis.log)
reg.voto.rec_prop <- lm(dados$votos.log ~ dados$rec_prop.log)
summary(reg.voto.rec_total)
summary(reg.voto.rec_part)
summary(reg.voto.rec_jur)
summary(reg.voto.rec_fis)
summary(reg.voto.rec_prop)
