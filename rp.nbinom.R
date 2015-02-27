#' @title Painel para Visualização da Distribuição Binomial Negativa
#' @name rp.pois
#'
#' @description Esta função cria um painel interativo para visualização
#' da distribuição Binomial Negativa.
#'
#' @return Abre uma janela com deslizadores e caixas de seleção para
#' visualização da influência dos parâmetros \code{size} e \code{prob}
#' na forma da distribuição Binomial Negativa. 
#' 
#' @export
#' @examples
#' \donttest{
#' ##-----------------------------------------------------------------------------
#'
#' require(rpanel)
#' rp.nbinom()
#'
#' }

rp.nbinom <- function(){
    action <- function(panel){
        with(panel, {
            ## Plotando a distribuição
            x <- 0:size
            px <- dnbinom(x, size=size, prob=prob)
            mu <- size*(1-prob)/prob
            des <- sqrt(mu*(1/prob))
            plot(px~x, type="h", ylim=c(0, max(c(px), 0.4)), lwd=4)
            msg <- sprintf("mean: %0.2f \t sd: %0.2f",
                           mean=mu, sd=des)
            mtext(msg, side=3, cex=0.8)
            title(substitute(X %~% ~~Binomial~Negativa ~ (list(size==a,
                                                               prob==b)),
                             list(a=size, b=prob)))
            ## Calculando Probabilidades Acumuladas
            Pr <- round(pnbinom(q, size=size, prob=prob), 3)
            legend("topleft", bty="n", fill="gray70",
                   legend=substitute(P(X<=~q)==Pr,
                       list(q=q, Pr=Pr)))
            ## Visualizando Média e Desvio Padrão
            if(showEX){
                segments(mu, 0, mu, max(px), col=2, lwd=1.5)
            }
            if(showVX){
                segments(mu-des, 0, mu+des, 0, col=2, lwd=1.5)
            }
            ## Visualizando Probabilidade Acumulada
            if(showPr){
                plot(px~x, type="h", ylim=c(0, max(c(px), 0.4)), lwd=4)
                msg <- sprintf("mean: %0.2f \t sd: %0.2f",
                               mean=mu, sd=des)
                mtext(msg, side=3, cex=0.8)
                title(substitute(X %~% ~~Binomial~Negativa ~ (list(size==a,
                                                                   prob==b)),
                                 list(a=size, b=prob)))
                lines(px[1:(q+1)] ~ x[1:(q+1)], type="h", lwd=4,
                      col="gray70")
                legend("topleft", bty="n", fill="gray70",
                       legend=substitute(P(X<=~q)==Pr,
                           list(q=q, Pr=Pr)))
                ## Visualizando Média e Desvio Padrão
                if(showEX){
                    segments(mu, 0, mu, max(px), col=2, lwd=1.5)
                }
                if(showVX){
                    segments(mu-des, 0, mu+des, 0, col=2, lwd=1.5)
                }
            }
        })
        panel
    }
    ## Construção do painel e controladores
    panel <- rp.control(title="Distribuição Binomial Negativa", size=c(300,100))
    rp.slider(panel, size, from=2, to=80, initval=10, resolution=1,
          action=action, showvalue=TRUE, title="size")
    rp.slider(panel, prob, from=0.01, to=0.99, initval=0.5, resolution=0.01,
          action=action, showvalue=TRUE, title="prob")
    rp.doublebutton(panel, q, step = 1, title = "Quantil", showvalue=TRUE,
                    action = action, range = c(0, 80), initval=0)
    ## rp.slider(panel, q, from=0, to=80, initval=0, resolution=1,
    ##           action = action, showvalue=TRUE, title="Quantil")
    rp.checkbox(panel, showEX, action=action, title="E(X)",
                labels="Esperança de X")
    rp.checkbox(panel, showVX, action=action, title="EP(X)",
                labels="Desvio Padrão de X")
    rp.checkbox(panel, showPr, action=action, title="Prob. acumulada",
                labels="Visualizar a prob. acumulada")
}
