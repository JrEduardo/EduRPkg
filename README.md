EduRPkg
=============================================================================

Este repositório contém funções que permitem a compreensão de conceitos
estatísticos por meio de visualizações gráficas interativas. Com esta
nova ferramenta espera-se um melhor aproveitamento, no âmbito do
ensino-aprendizagem de estatística, nos cursos, aulas, palestras, entre
outras formas de disseminação do conhecimento que a utilizem.

## Uso

Pode-se carregar as funções elaboradas conforme código reproduzível abaixo:


```{r}
### Carregando as funções

## http://tonybreyal.wordpress.com/2011/11/24/source_https-sourcing-an-r-script-from-github/
source_https <- function(url, ...) {
  # load package
  require(RCurl)
 
  # parse and evaluate each .R script
  sapply(c(url, ...), function(u) {
    eval(parse(text = getURL(u, followlocation = TRUE, cainfo =
                                 system.file("CurlSSL", "cacert.pem",
                                             package = "RCurl"))), envir
         = .GlobalEnv) 
  })
}
source_https("https://raw.githubusercontent.com/JrEduardo/EduRPkg/master/list.functions.R")
source_https(funs)

### Utilizando as funções

## Pacotes dependentes
install.packages("rpanel", dependencies = TRUE)

## Exemplos
require(rpanel)
rp.binom()
rp.norm()

```

## Bug report

Para bugs, sugestões ou dúvidas deixe sua mensagem em 
[Issues](https://github.com/JrEduardo/EduRPkg/issues).
