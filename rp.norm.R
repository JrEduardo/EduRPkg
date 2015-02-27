#' @title Painel para Visualização da Distribuição Normal 
#' @name rp.norm
#'
#' @description Esta função cria um painel interativo para visualização
#' da distribuição Normal. Minha primeira função simples para aprender
#' sobre sistema de versionamento e trabalho colaboritivo git e github.
#'
#' @return Abre uma janela com deslizadores e caixas de seleção para
#' visualização da influência dos parâmetros, \code{mean} e \code{sd},
#' na forma da distribuição Normal. 
#' 
#' @export
#' @examples
#' \donttest{
#' ##-----------------------------------------------------------------------------
#'
#' require(rpanel)
#' rp.norm()
#'
#' }

rp.norm <- function(){
    action <- function(panel){
        with(panel, {
            if(des <= 0) des <- 0.01
            ## Plotando a distribuição de X
            curve(dnorm(x, mean=mu, sd=des), -5, 5,
                  ylim=c(0,1))
            msg <- sprintf("mean: %0.2f \t sd: %0.2f",
                           mean=mu, sd=des)
            mtext(msg, side=3, cex=0.8)
            title(substitute(X %~% ~~Normal ~ (list(a, b^2)),
                             list(a=mu, b=des)))
            ## Caculando Probabilidades
            Pr <- round(pnorm(q, mu, des),
                        digits=3)
            qq <- round(q, digits=3)
            legend("topleft", bty="n", fill="gray70",
                   legend=substitute(P(X<=~q)==Pr,
                       list(q=qq, Pr=Pr)))
            ## Visualizar Media e Variancia de X
            if(showEX){
                d <- dnorm(mu, mu, des)
                segments(mu, 0, mu, d, col=2, lwd=1.5)
            }
            if(showVX){
                d <- dnorm(mu+des, mu, des)
                segments(mu-des, d, mu+des, d, col=2, lwd=1.5)
            }
            ## Visualizar a probabilidade acumulada até q
            if(showPr){
                curve(dnorm(x, mean=mu, sd=des), -5, 5,
                      ylim=c(0,1))
                mtext(msg, side=3, cex=0.8)
                title(substitute(X %~% ~~Normal ~ (list(a, b^2)),
                                 list(a=mu, b=des)))
                x <- seq(-5, q, by=0.01)
                fx <- dnorm(x, mu, des)
                polygon(c(x, rev(x)),
                        c(fx, rep(0, length(fx))),
                        col="gray70")
                Pr <- round(pnorm(q, mu, des),
                            digits=3)
                qq <- round(q, digits=3)
                legend("topleft", bty="n", fill="gray70",
                       legend=substitute(P(X<=~q)==Pr,
                           list(q=qq, Pr=Pr)))
                ## Visualizando Media e Variancia
                if(showEX){
                    d <- dnorm(mu, mu, des)
                    segments(mu, 0, mu, d, col=2, lwd=1.5)
                }
                if(showVX){
                    d <- dnorm(mu+des, mu, des)
                    segments(mu-des, d, mu+des, d, col=2, lwd=1.5)
                }
            }
        })
        panel
    }
    ## Construção do painel e controladores
    panel <- rp.control(title="Distribuição Normal", size=c(300,100))
    rp.slider(panel, mu, from=-4, to=4, initval=0, resolution=0.1,
              action=action, showvalue=TRUE, title="Média")
    rp.slider(panel, des, from=0, to=3, initval=1, resolution=0.05,
              action=action, showvalue=TRUE, title="Desvio Padrão")
    rp.slider(panel, q, from=-4, to=4, initval=0, resolution=0.1,
              action=action, showvalue=TRUE, title="quantil")
    rp.checkbox(panel, showEX, action=action, title="E(X)",
                labels="Esperança de X")
    rp.checkbox(panel, showVX, action=action, title="EP(X)",
                labels="Desvio Padrão de X")
    rp.checkbox(panel, showPr, action=action, title="Prob. acumulada",
                labels="Visualizar a prob. acumulada")
}
