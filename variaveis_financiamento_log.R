###teste para lidar com os NAs###

attach(dados)
summary(v1) #valor.Comercializa�ao.de.bens.ou.realiza�ao.de.eventos
summary(v2)	#valor.Doa�oes.pela.Internet
summary(v3)	#valor.Recursos.de.origens.nao.identificadas
summary(v4)	#valor.Recursos.de.outros.candidatos.comites
summary(v5)	#valor.Recursos.de.partido.pol�tico
summary(v6)	#valor.Recursos.de.pessoas.f�sicas
summary(v7)	#valor.Recursos.de.pessoas.jur�dicas
summary(v8)	#valor.Recursos.pr�prios
summary(v9)	#valor.Rendimentos.de.aplica�oes.financeiras

##as mais problem�ticas s�o as vari�veis v1, v2, e v9, com mais de 7000 casos de NA.
##as vari�veis V5, v6 e v8 tamb�m t�m zeros, o que d� problemas com o log depois
##ap�s tirar o log das vari�veis, vamos trocar os infinitos por 0 e tamb�m os NAs.

rec_part <- (v4 + v5)
rec_prop <- v8
rec_fis <- v6
rec_jur <- v7
rec_outros <- v3 + v2 + v9
rec_total <- rowSums(dados[  ,63:71], na.rm=TRUE)
rec_ext <- rec_total - rec_part


summary(rec_part)
summary(rec_prop)
summary(rec_fis)
summary(rec_jur)
summary(rec_outros)
summary(rec_total)
summary(rec_ext)


rec_part.log <- log(rec_part)
rec_prop.log <- log(rec_prop)
rec_fis.log <- log(rec_fis)
rec_jur.log <- log(rec_jur)
rec_outros.log <- log(rec_outros)
rec_total.log <- log(rec_total)
rec_ext.log <- log(rec_ext)

#observa��o: exclu�mos a v1 por s� ter um valor dentre a amostra total.
#vendo quais os logs que ficaram com valores infinitos
summary(rec_part.log)
summary(rec_prop.log)
summary(rec_fis.log)
summary(rec_jur.log)
summary(rec_outros.log)
summary(rec_total.log)
summary(rec_ext.log)


#tirando os infinitos dos logs:
rec_total.log[ is.infinite(rec_total.log) ] <- 0
rec_ext.log[ is.infinite(rec_ext.log) ] <- 0


