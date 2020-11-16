#' @title Validacao do Cadastro de Pessoas Fisicas
#'
#' @description This function validade the Brazilian Cadastro de Pessoas Fisicas (CPF).
#'
#' @param vetor_cpfs
#'
#' @return NULL
#'
#' @examples  validar_cpfs('073.835.475-75')
#'
#' @export validar_cpfs
#'


#validar_cpfs=function(vetor_cpfs)
#{
#  retorno=character(length = length(vetor_cpfs))
#  for(i in 1:length(vetor_cpfs))
#  {
#    cpf_original=trimws(gsub("/","",gsub("-","",gsub("\\.", "", vetor_cpfs[i]))))
#    cpf=as.numeric(unlist(stringr::str_split(string = cpf_original,pattern = "")))
#    v1=0
#    v2=0
#    for(j in (length(cpf)-2):1)
#    {
#      v1 = v1 + cpf[j] * (length(cpf) - j )
#    }
#    v1=11-(v1-11*floor(v1/11))
#    v1=ifelse(v1<10,v1,0)
#    cpf[length(cpf)-1]=v1
#    for(j in (length(cpf)-1):1)
#    {
#      v2 = v2 + cpf[j] * (length(cpf)+1 - j)
#    }
#    v2=11-(v2-11*floor(v2/11))
#    v2=ifelse(v2<10,v2,0)
#    cpf[length(cpf)]=v2
#    retorno[i]=ifelse(cpf_original==paste(cpf,collapse=""),"VÁLIDO","INVÁLIDO")
#  }
#  return(retorno)
#}

