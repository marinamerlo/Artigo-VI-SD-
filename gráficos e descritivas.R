###Análises descritivas### 

#entre ter sido eleito ou não em 2010 e ter recebido ou não recursos do partido
cand2010n <- as.numeric(cand2010)
cor(cand2010n, apoio_part)
#entre ter sido candidato ou não em 2010 e ter recebido ou não recursos do partido
eleito2010n <- as.numeric(eleito2014)
cor(eleito2010n, apoio_part)

##fazendo boxplot maravilhoso##
#queremos ver a distribuição dos recursos entre eleitos e não eleitos

#deixando a variável pra eleito com label
eleito2014n <- ifelse(eleito2014 == 1, c("eleito"), c("não-eleito"))

#deixando os dados do jeito que precisa pro ggplot, a partir da função melt
#a idéia é separar a variável categórica pela qual você quer comparar os dados
require(reshape2)
dados_recursos <- data.frame(eleito2014, rec_fis.log,  rec_prop.log, rec_total.log, rec_part.log, rec_jur.log)
dados_recursos <- melt(dados_recursos, id.var = "eleito2014")

#rodando o gráfico maravilhoso
require(ggplot2)
ggplot(data = dados_recursos, aes(x=variable, y=value)) + geom_boxplot(aes(fill=eleito2014))

p + facet_wrap( ~ variable, scales="free")

#agora, vendo por sexo

#deixando a variável sexo com label
summary(sexo)
sexo <- ifelse(sexo == 1, c("Feminino"), c("Masculino"))

require(reshape2)
dados_recursos_sexo <- data.frame(sexo, rec_fis.log, rec_prop.log, rec_total.log, dados$rec_part.log, rec_jur.log)
dados_recursos_sexo <- melt(dados_recursos_sexo, id.var = "sexo")

#rodando o gráfico maravilhoso
require(ggplot2)
p <- ggplot(data = dados_recursos_sexo, aes(x=variable, y=value)) + geom_boxplot(aes(fill=sexo))
p <- p + xlab("Tipo de Recuro") + ylab("Log dos Valores da Receita") + ggtitle("Distribuição dos Tipos de Recurso por Sexo")
p <- p + guides(fill=guide_legend(title="Sexo"))
plot(p)

#tentando stacked bar plot maravilhoso#

dados_recursos <- data.frame(eleito2014, rec_fis, rec_prop, rec_part, rec_jur, rec_outros)
dados_recursos2 <- melt(dados_recursos, id.vars = "eleito2014")
ggplot(data = dados_recursos2, aes(x = eleito2014, y = value, fill = variable)) + geom_bar(stat = "identity") 

ggplot(dados_recursos2, aes(x=eleito2014, y=value, fill=variable)) + 
  geom_bar(stat="identity") +
  xlab("\nResultado na Eleição de 2014") +
  ylab("Tipo de Recurso") +
  theme_bw()


##scatter plot maravilhoso de correlação 
p <- qplot(rec_part.log, rec_ext.log,  size = I(4), alpha = I(0.5), colour = factor(eleito2014), na.rm=TRUE) 
#mudandoa a cor
p <- p + scale_colour_brewer(palette="Set2")
#adicionando a reta de correlação
lm(rec_ext.log ~ rec_part.log)
p <- p + geom_abline(intercept = 1.03730, slope = -0.02999, color="black", alpha = I(0.5)) 
#colocando legendas
p <- p + xlab("Recursos Partidários (log)") + ylab("Recursos Não-Partidários(log)") + ggtitle("Distribuição dos Recursos")
p <- p + guides(fill=guide_legend(title="Resultado na Eleição de 2014"))
plot(p)

#adicionando os quartis da distribuição dos dois recursos no scatter plot
summary(rec_ext.log)
summary(rec_part.log)
#recursos externos
p <- p + geom_hline(yintercept = 0.726, color="black", alpha = I(0.7)) #média de recursos externos
p <- p + geom_hline(yintercept = 1.028, color="black", alpha = I(0.7)) #3o quartil de recursos externos
p <- p + geom_hline(yintercept = 0.085, color="black", alpha = I(0.7)) #1o quartil de recursos externos
#recursos do partido
p <- p + geom_vline(xintercept = 8.572, color="black", alpha = I(0.7)) #1o quartil
p <- p + geom_vline(xintercept = 10.370, color="black", alpha = I(0.7)) #média
p <- p + geom_vline(xintercept = 12.320, color="black", alpha = I(0.7)) #3o quartil



