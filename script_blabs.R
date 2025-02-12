#definindo o diretorio
setwd("/Users/6912700.FFLCH/Desktop")

#abrindo o banco:
dados <- read.csv("/Users/6912700.FFLCH/Desktop", dec=".", header=TRUE, sep=";", stringsAsFactors=FALSE)

dados <- data.frame

#checando como os dados ficaram por variavel
str(dados_anpocs)

#como os valores vieram em chr, vamos deixa-las como numerico:
dados_anpocs$v1 <- as.numeric(dados_anpocs$v1)
dados_anpocs$v2 <- as.numeric(dados_anpocs$v2)
dados_anpocs$v3 <- as.numeric(dados_anpocs$v3)
dados_anpocs$v4 <- as.numeric(dados_anpocs$v4)
dados_anpocs$v5 <- as.numeric(dados_anpocs$v5)
dados_anpocs$v6 <- as.numeric(dados_anpocs$v6)
dados_anpocs$v7 <- as.numeric(dados_anpocs$v7)
dados_anpocs$v8 <- as.numeric(dados_anpocs$v8)


#teste dos dados
boxplot((log(dados_anpocs$v3)), na.rm =T)

#somando as vari�veis que sao de origem partid�ria e tirando o log dela:

rec_part <- is.na(dados_anpocs$v5 + dados_anpocs$v4 + dados_anpocs$v1)
summary(rec_part)
recpart <- log(rec_part)
summary (recpart)

boxplot (dados_anpocs$DESCRICAO_SEXO~recpart)
boxplot (recpart2 ~ dados_anpocs$DESCRICAO_SEXO)

rec_part2 <- (dados_anpocs$v5 + dados_anpocs$v4)
summary(rec_part2)
recpart2 <- log(rec_part2)
summary (recpart2)

#criando uma dummy para dizer se foi eleito ou n�o:              

dados_anpocs$eleito <- ifelse(dados_anpocs$COD_SIT_TOT_TURNO == 2 | dados_anpocs$COD_SIT_TOT_TURNO == 3, c("1"), c("0"))
dados_anpocs$eleito <- as.factor(dados_anpocs$eleito)
summary (dados_anpocs$eleito)

#deixando o sexo como factor
dados_anpocs$sexo <- as.factor(dados_anpocs$CODIGO_SEXO)
summary (dados_anpocs$sexo)


#criando uma vari�vel com todos os recursos:

rec_total <- rowSums(dados_anpocs[  ,33:41], na.rm=TRUE)
summary(rec_total)
rec_total.log <- as.numeric(log(rec_total))
summary(rec_total)

#criando o log de cada tipo de recurso:

rec_jur.log <- as.numeric(log(dados_anpocs$v7))
rec_fis.log <- as.numeric(log(dados_anpocs$v6))
rec_part.log <- recpart2

#vari�vel s� com recursos externos:
rec_ext.log <- rec_total.log - rec_part.log


#para diferenciar sexo e eleitos nos gr�ficos:
  #Cria cor diferente para homens e mulheres:
cols_sexo = ifelse(sexo=="FEMININO","red","black")
  #deixa o quadrado preenchido para eleitos e um x para n�o-eleitos
pch_eleito = ifelse(dados_anpocs$eleito=="1",15,4)

#scatterplot:
plot(rec_part.log,rec_ext.log, col=cols_sexo, pch=pch_eleito, cex=0.5) 
#linhas para os quartis dos recursos externos
abline(h=0.086)
abline(h=0.727)
abline(h=1.028)
#linhas para os quartis dos recursos do partido
abline(v=8.572)
abline(v=10.370)
abline(v=12.310)
#correla��o
corr_part_ext <- lm(rec_part.log ~ rec_ext.log)

#summary das vari�veis de recursos
summary(rec_part.log)
summary(rec_ext.log)
summary(rec_total.log)

boxplot(rec_ext.log ~ sexo)
boxplot(rec_part.log ~sexo)

#dummy para candidaturas com zero recursos:
dados_anpocs$laranja <- ifelse(rec_total> 0, c("0"), c("1"))
dados_anpocs$laranja <- as.factor(dados_anpocs$laranja)
summary (dados_anpocs$laranja)

#olhando a vari�vel "laranja" com "sexo"
table(dados_anpocs$laranja,sexo)
plot(sexo, dados_anpocs$laranja)

laranja.n <- as.numeric(dados_anpocs$laranja)

cor_laranja_sexo <- glm(laranja.n ~ sexo)

plot(cor_laranja_sexo)
mosaicplot(laranja.n ~ sexo, color=cols_sexo, type=cor_laranja_sexo)
summary (cor_laranja_sexo)

#dummy para candidaturas com zero recursos dos partidos:
dados_anpocs$apoio_part <- ifelse(rec_ext> 0, c("1"), c("0"))
dados_anpocs$apoio_part <- as.factor(dados_anpocs$apoio_part)
summary (dados_anpocs$apoio_part)



