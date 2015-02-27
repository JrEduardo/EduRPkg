#' @title Painel para Visualização da Distribuição Geométrica
#' @name rp.geom
#'
#' @description Esta função cria um painel interativo para visualização
#' da distribuição Geométrica.
#'
#' @return Abre uma janela com deslizadores e caixas de seleção para
#' visualização da influência do parâmetro \code{prob}
#' na forma da distribuição Geométrica. 
#' 
#' @export
#' @examples
#' \donttest{
#' ##-----------------------------------------------------------------------------
#'
#' require(rpanel)
#' rp.geom()
#'
#' }

help(pgeom, h="html")

rp.geom <- function(){
    action <- function(panel){
        with(panel, {
            ## Plotando a distribuição
            x <- 0:100
            px <- dgeom(x, prob = prob)
            mu <- (1 - prob)/prob
            des <- sqrt((1 - prob)/prob^2)
            plot(px~x, type="h", ylim=c(0, max(c(px), 0.5)), lwd=4)
            msg <- sprintf("mean: %0.2f \t sd: %0.2f",
                           mean=mu, sd=des)
            mtext(msg, side=3, cex=0.8)
            title(substitute(X %~% ~~Geométrica ~ (list(prob==a)),
                             list(a=prob)))
            ## Calculando Probabilidades Acumuladas
            Pr <- round(pgeom(q, prob = prob), 3)
            legend("topright", bty="n", fill="gray70",
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
                plot(px~x, type="h", ylim=c(0, max(c(px), 0.5)), lwd=4)
                msg <- sprintf("mean: %0.2f \t sd: %0.2f",
                               mean=mu, sd=des)
                mtext(msg, side=3, cex=0.8)
                title(substitute(X %~% ~~Geométrica ~ (list(prob==a)),
                                 list(a=prob)))
                lines(px[1:(q+1)] ~ x[1:(q+1)], type="h", lwd=4,
                      col="gray70")
                legend("topright", bty="n", fill="gray70",
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
    rp.slider(panel, prob, from=0.01, to=0.99, initval=0.2, resolution=0.01,
              action=action, showvalue=TRUE, title="Prob")
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
