##incluindo as informações de votação no banco de candidaturas e financiamento

#definindo o diretorio
setwd("C:/Users/Marina/Desktop/")
#listando os documentos com os dados
lista.arquivos <-list.files(file.path(getwd(),"/Votos 2014"))
#pegando somente os documentos somente das receitas
lista.arquivos <- grep(pattern="votacao_candidato_munzona_2014_", lista.arquivos, value=TRUE)
lista.arquivos <- lista.arquivos[c(1:4)] #teste do script antes de rodar os 27 Estados.

#criando um unico dataframe para todos os Estados
dados <- data.frame()
#Loop para coletar os dados que queremos:
#vai abrir cada uma das listas, renomear as colunas, pegar só os casos de Deputado Federal, fazer a soma de votos de candidatos por UF
#dar nome as colunas resultantes
#incluir no banco com os outros
for(arquivo in lista.arquivos){
  print (arquivo)
  teste <- read.table(file.path(getwd(),"/Votos 2014", arquivo), sep=";", header=FALSE, fileEncoding = "latin2", stringsAsFactors = F)
  names(teste) <- c("DATA_GERACAO",
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
teste <- subset (teste, DESCRICAO_CARGO=="DEPUTADO FEDERAL")
teste <- aggregate(teste$TOTAL_VOTOS, by = list(teste$SEQUENCIAL_CANDIDATO, teste$SIGLA_UF), FUN="sum")
names(teste) <- c("SEQUENCIAL_CANDIDATO", "SIGLA_UF", "TOTAL_VOTOS")
dados <-rbind(dados, teste)
}
print("cabô")

#salvando os arquivos
write.table(dados, file="votos_2014.txt", 
            sep = ";",
            quote = T,
            dec = ",",
            row.names = F,
            col.names = T)
print ("cabô")


