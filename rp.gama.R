#' @title Painel para Visualização da Distribuição Gamma
#' @name rp.gamma
#'
#' @description Esta função cria um painel interativo para visualização
#' da distribuição Gamma. 
#'
#' @return Abre uma janela com deslizadores e caixas de seleção para
#' visualização da influência dos parâmetros, \code{shape1} e \code{shape2},
#' na forma da distribuição Beta.
#' 
#' @export
#' @examples
#' \donttest{
#' ##-----------------------------------------------------------------------------
#'
#' require(rpanel)
#' rp.gamma()
#'
#' }

rp.beta <- function(){
    action <- function(panel){
        with(panel,
             {
                 if(sh1 == 0) sh1 <- 0.01
                 if(sh2 == 0) sh1 <- 0.01
                 ## Calculando estatisticas
                 mu <- sh1 / (sh1 + sh2)
                 des <- sqrt( sh1 * sh2 / ((sh1 + sh2)^2 * (sh1 + sh2)))
                 ## Plotando a distribuição de X
                 curve(dbeta(x, shape1=sh1, shape2=sh2), 0, 1,
                       ylim=c(0,7))
                 msg <- sprintf("mean: %0.2f \t sd: %0.2f",
                                mean=mu, sd=des)
                 mtext(msg, side=3, cex=0.8)
                 title(substitute(X %~% ~~"Beta" ~
                                  (list(shape1 == a, shape2 == b)),
                                  list(a=sh1, b=sh1)))
                 ## Caculando Probabilidades
                 Pr <- round(pbeta(q, shape1 = sh1, shape2 = sh2),
                             digits=3)
                 qq <- round(q, digits=3)
                 legend("topleft", bty="n", fill="gray70",
                        legend = substitute(P(X<=~q)==Pr,
                            list(q=qq, Pr=Pr)))
                 ## Visualizar Media e Variancia de X
                 if(showEX){
                     d <- dbeta(mu, shape1 = sh1, shape2 = sh2)
                     segments(mu, 0, mu, d, col=2, lwd=1.5)
                 }
                 if(showVX){
                     arrows(mu-des, 0, mu+des, 0, col= 2,
                            code=3, angle=90, length=0.05,
                            lwd=1.5)
                 }
                 ## Visualizar a probabilidade acumulada até q
                 if(showPr){
                     curve(dbeta(x, shape1=sh1, shape2=sh2), 0, 1,
                           ylim=c(0,7))
                     msg <- sprintf("mean: %0.2f \t sd: %0.2f",
                                    mean=mu, sd=des)
                     mtext(msg, side=3, cex=0.8)
                     title(substitute(X %~% ~~"Beta" ~
                                      (list(shape1 == a, shape2 == b)),
                                      list(a=sh1, b=sh1)))
                     x <- seq(0, q, by=0.01)
                     fx <- dbeta(x, shape1 = sh1, shape2 = sh2)
                     polygon(c(x, rev(x)),
                             c(fx, rep(0, length(fx))),
                             col="gray70")
                     legend("topleft", bty="n", fill="gray70",
                            legend=substitute(P(X<=~q)==Pr,
                                list(q=qq, Pr=Pr)))
                     ## Visualizar Media e Variancia de X
                     if(showEX){
                         d <- dbeta(mu, shape1 = sh1, shape2 = sh2)
                         segments(mu, 0, mu, d, col=2, lwd=1.5)
                     }
                     if(showVX){
                         arrows(mu-des, 0, mu+des, 0, col=2,
                                code=3, angle=90, length=0.05, 
                                lwd=1.5)
                     }
                 }
             })
        panel
    }
    ## Construção do painel e controladores
    panel <- rp.control(title="Distribuição Beta", size=c(300,100))
    rp.slider(panel, sh1, from=0, to=150, initval=10, resolution=1,
              action=action, showvalue=TRUE, title="Shape1")
    rp.slider(panel, sh2, from=0, to=150, initval=5, resolution=1,
              action=action, showvalue=TRUE, title="Shape2")
    rp.slider(panel, q, from=0, to=1, initval=0, resolution=0.01,
              action=action, showvalue=TRUE, title="quantil")
    rp.checkbox(panel, showEX, action=action, title="E(X)",
                labels="Esperança de X")
    rp.checkbox(panel, showVX, action=action, title="EP(X)",
                labels="Desvio Padrão de X")
    rp.checkbox(panel, showPr, action=action, title="Prob. acumulada",
                labels="Visualizar a prob. acumulada")
}
