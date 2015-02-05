#' @title Painel para Visualização da Distribuição Poisson
#' @name rp.pois
#'
#' @description Esta função cria um painel interativo para visualização
#' da distribuição Poisson.
#'
#' @return Abre uma janela com deslizadores e caixas de seleção para
#' visualização da influência do parâmetro \code{lambda}
#' na forma da distribuição Poisson. 
#' 
#' @export
#' @examples
#' \donttest{
#' ##-----------------------------------------------------------------------------
#'
#' require(rpanel)
#' rp.pois()
#'
#' }

rp.pois <- function(){
    action <- function(panel){
        with(panel, {
            ## Plotando a distribuição
            x <- 0:100
            px <- dpois(x, lambda=lambda)
            mu <- lambda
            des <- sqrt(lambda)
            plot(px~x, type="h", ylim=c(0, max(c(px), 0.4)), lwd=4)
            msg <- sprintf("mean: %0.2f \t sd: %0.2f",
                           mean=mu, sd=des)
            mtext(msg, side=3, cex=0.8)
            title(substitute(X %~% ~~Poisson ~ (list(lambda==a)),
                             list(a=lambda)))
            ## Calculando Probabilidades Acumuladas
            Pr <- round(ppois(q, lambda=lambda), 3)
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
                title(substitute(X %~% ~~Poisson ~ (list(lambda==a)),
                                 list(a=lambda)))
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
    panel <- rp.control(title="Distribuição Poisson", size=c(300,100))
    rp.slider(panel, lambda, from=0.5, to=90, initval=10, resolution=0.25,
              action=action, showvalue=TRUE, title="lambda")
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
