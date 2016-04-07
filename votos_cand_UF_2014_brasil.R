##Pegando os dados de votação por candidato por UF##

#definindo o diretorio
setwd("C:/Users/Marina/Desktop/")

#listando os documentos com os dados
lista.arquivos <-list.files(file.path(getwd(),"/Votos 2014"))

#pegando somente os documentos somente das receitas
lista.arquivos <- grep(pattern="votacao_candidato_munzona_2014_", lista.arquivos, value=TRUE)
lista.arquivos <- lista.arquivos[c(1:27)] 

#criando o dataframe vazio que receberá os dados
dados <- data.frame()

#Loop para coletar os dados que queremos:
  #vai abrir cada uma das listas, renomear as colunas, pegar só os casos de Deputado Federal, fazer a soma de votos de candidatos por UF
  #dar nome as colunas resultantes, que é o Sequencial do Candidato (usado pro merge), a UF e total de votos recebidos na UF
  #incluir no dataframe vazio
  
for(arquivo in lista.arquivos){
  print (arquivo)
  d <- read.table(file.path(getwd(),"/Votos 2014", arquivo), sep=";", header=FALSE, fileEncoding = "latin2", stringsAsFactors = F)
  names(d) <- c("DATA_GERACAO",
                    "HORA_GERACAO",
                    "ANO_ELEICAO", 
                    "NUM_TURNO",
                    "DESCRICAO_ELEICAO",
                    "SIGLA_UF",
                    "SIGLA_UE",
                    "CODIGO_MUNICIPIO",
                    "NOME_MUNICIPIO",
                    "NUMERO_ZONA",
                    "CODIGO_CARGO",
                    "NUMERO_CAND", 
                    "SEQUENCIAL_CANDIDATO",
                    "NOME_CANDIDATO",
                    "NOME_URNA_CANDIDATO",
                    "DESCRICAO_CARGO",
                    "COD_SIT_CAND_SUPERIOR",
                    "DESC_SIT_CAND_SUPERIOR",
                    "CODIGO_SIT_CANDIDATO",
                    "DESC_SIT_CANDIDATO",
                    "CODIGO_SIT_CAND_TOT",
                    "DESC_SIT_CAND_TOT",
                    "NUMERO_PARTIDO",
                    "SIGLA_PARTIDO",
                    "NOME_PARTIDO",
                    "SEQUENCIAL_LEGENDA",
                    "NOME_COLIGACAO",
                    "COMPOSICAO_LEGENDA",
                    "TOTAL_VOTOS",
                    "TRANSITO")
  d <- subset (d, DESCRICAO_CARGO=="DEPUTADO FEDERAL")
  d <- aggregate(d$TOTAL_VOTOS, by = list(d$SEQUENCIAL_CANDIDATO, d$SIGLA_UF), FUN="sum")
  names(d) <- c("SEQUENCIAL_CANDIDATO", "SIGLA_UF", "TOTAL_VOTOS")
  dados <-rbind(dados, d)
}
print("cabô")


#checando o número de candidatos únicos
unique(dados$SEQUENCIAL_CANDIDATO)

#checando se puxou todas as UFs.
unique(dados$SIGLA_UF)

#checando a distribuição de votos
plot(dados$TOTAL_VOTOS)

#salvando o dataframe final
write.table(dados, file="votos_2014.txt", 
            sep = ";",
            quote = T,
            dec = ",",
            row.names = F,
            col.names = T)
print ("cabô")
