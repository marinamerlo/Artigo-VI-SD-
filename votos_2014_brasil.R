##incluindo as informações de votação no banco de candidaturas e financiamento

#definindo o diretorio
setwd("C:/Users/Marina/Desktop/")
#listando os documentos com os dados
lista.arquivos <-list.files(file.path(getwd(),"/Votos 2014"))
#pegando somente os documentos somente das receitas
lista.arquivos <- grep(pattern="votacao_candidato_munzona_2014_", lista.arquivos, value=TRUE)
#excluindo o arquivo BR
lista.arquivos <- lista.arquivos[c(1:27)]
#criando um unico dataframe para todos os Estados
dados <- data.frame()
for(arquivo in lista.arquivos){
  print (arquivo)
  d <- read.table(file.path(getwd(),"/Votos 2014", arquivo), sep=";", header=FALSE, fileEncoding = "latin2", stringsAsFactors = F)
  dados <-rbind(dados, d)
}
print("cabô")

#salvando os arquivos
write.table(dados, file="votos_2014.txt")
print ("cabô")


#checando como os dados ficaram por variavel
str(dados)

#como os valores vieram em chr, vamos substituir as virgulas por pontos e deixar o resto em numerico:
dados$Valor.receita <- sub(",", ".", dados$Valor.receita)
dados$Valor.receita <- as.numeric(dados$Valor.receita)
#checando se todos os valores estao como numericos:
plot(density(dados$Valor.receita))  
#checando se existe missing:
length(dados$Valor.receita[is.na(dados$Valor.receita)])
#agregando as receitas pelo CPF dos candidatos e pelo tipo de receita recebida
receitas <- aggregate(dados$Valor.receita, by = list(dados$CPF.do.candidato, dados$Tipo.receita), FUN="sum")
#renomeando as variaveis
names(receitas) <- c("cpf", "tipo_receita", "valor")
#deixando observacoes unicas pra cada candidato por CPF
receitas <- reshape(receitas, timevar = "tipo_receita", idvar = "cpf", direction = "wide")

