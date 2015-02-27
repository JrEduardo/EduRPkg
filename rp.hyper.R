#' @title Painel para Visualização da Distribuição Hipergeométrica
#' @name rp.hyper
#'
#' @description Esta função cria um painel interativo para visualização
#' da distribuição Hipergeométrica.
#'
#' @return Abre uma janela com deslizadores e caixas de seleção para
#' visualização da influência dos parâmetros: \code{m}, \code{n} e \code{k}
#' na forma da distribuição Hipergeométrica. 
#' 
#' @export
#' @examples
#' \donttest{
#' ##-----------------------------------------------------------------------------
#'
#' require(rpanel)
#' rp.hyper()
#'
#' }

rp.hyper <- function(){
    action <- function(panel){
        with(panel, {
            ## Plotando a distribuição
            x <- max(c(0, k-m)):min(c(k, m))
            px <- dhyper(x, m=m, n=n, k=k)
            mu <- k*m/(m+n)
            des <- sqrt((m+n-k)/(m+n-1) * n * (m/(m+n)) * (1 - m/(m+n)))
            plot(px~x, type="h", ylim=c(0, max(c(px), 0.5)), lwd=4)
            msg <- sprintf("mean: %0.2f \t sd: %0.2f",
                           mean=mu, sd=des)
            mtext(msg, side=3, cex=0.8)
            title(substitute(X %~% ~~Hipergeométrica ~
                             (list(m==a, n==b, k==c)), 
                             list(a=m, b=n, c=k)))
            ## Calculando Probabilidades Acumuladas
            Pr <- round(phyper(q, m=m, n=n, k=k), 3)
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
                plot(px~x, type="h", ylim=c(0, max(c(px), 0.5)), lwd=4)
                msg <- sprintf("mean: %0.2f \t sd: %0.2f",
                               mean=mu, sd=des)
                mtext(msg, side=3, cex=0.8)
                title(substitute(X %~% ~~Hipergeométrica ~
                                 (list(m==a, n==b, k==c)), 
                                 list(a=m, b=n, c=k)))
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
    panel <- rp.control(title="Distribuição Hipergeométrica", size=c(300,100))
    rp.slider(panel, m, from=5, to=30, initval=10, resolution=1,
              action=action, showvalue=TRUE, title="Brancas")
    rp.slider(panel, n, from=2, to=15, initval=5, resolution=1,
              action=action, showvalue=TRUE, title="Pretas")
    rp.slider(panel, k, from=2, to=15, initval=5, resolution=1,
              action=action, showvalue=TRUE, title="Retiradas")
    rp.doublebutton(panel, q, step = 1, title = "Quantil", showvalue=TRUE,
                    action = action, range = c(0, 80), initval=0)
    ## rp.slider(panel, q, from=0, to=80, initval=0, resolution=1,
    ##           action=action, showvalue=TRUE, title="Quantil")
    rp.checkbox(panel, showEX, action=action, title="E(X)",
                labels="Esperança de X")
    rp.checkbox(panel, showVX, action=action, title="EP(X)",
                labels="Desvio Padrão de X")
    rp.checkbox(panel, showPr, action=action, title="Prob. acumulada",
                labels="Visualizar a prob. acumulada")
}
