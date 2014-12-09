#' @title Painel da Distribuição Normal 
#' @name rp.normal
#'
#' @description Esta função cria um painel interativo para visualização
#' da distribuição Normal padrão. Minha primeira função simples para
#' testar o sistema de versionamento e trabalho colaboritivo git e
#' github.
#'
#' @param object an object returned by \code{glht}. It is assumed that
#' the matrix used as the \code{linfct} argument in glht correspond a
#' matriz to get Tukey contrasts between least squares means.
#'
#' @param level the nominal significance level.
#'
#' @return an object of class \code{cld} with letters to resume mean
#' comparisons.
#' 
#' @seealso \code{\link{apc}}, \code{\link[doBy]{LSmatrix}},
#' \code{\link[multcomp]{glht}}.
#'
#' @export
#' @examples
#' \donttest{
#' ## Toy data 1: experiment with cultivars in several locations.
#' td1 <- expand.grid(loc=gl(5,1), block=gl(3,1), cult=LETTERS[1:6])
#' td1 <- subset(td1, !(loc==1 & cult=="A"))
#' td1 <- subset(td1, !(loc==2 & cult=="B"))
#' xtabs(~loc+cult, td1)
#' td1$y <- seq_len(nrow(td1))
#' 
#' require(lme4)
#' 
#' ## Fit the mixed model.
#' m0 <- lmer(y~loc*cult+(1|loc:block), data=td1)
#' logLik(m0)
#' 
#' ## The same model but without rank deficience.
#' td1$loccult <- with(td1, interaction(loc, cult, drop=TRUE))
#' m1 <- lmer(y~loccult+(1|loc:block), data=td1)
#' logLik(m1)
#' 
#' require(doBy)
#' 
#' X <- LSmatrix(lm(nobars(formula(m1)), data=td1), effect="loccult")
#' rownames(X) <- levels(td1$loccult)
#' dim(X)
#' 
#' Xs <- X[grepl(x=rownames(X), "^1\\."),]
#' 
#' require(wzRfun)
#' 
#' Xc <- apc(Xs)
#' 
#' require(multcomp)
#' 
#' g <- summary(glht(m1, linfct=Xc), test=adjusted(type="fdr"))
#' 
#' cld2(g)
#' 
#' confint(glht(m1, linfct=Xs), calpha=univariate_calpha())
#' }
#'
#'

require(rpanel)

pn <- function(panel){
    with(panel,
         {
             print(c(mu,  des, showPr, panel$panelname))
             des <<- des; mu <<- mu; showPr <<- showPr
             ## Plotando a distribuição
             curve(dnorm(x, mean=mu, sd=des), -5, 5,
                   ylim=c(0,1))
             ## Media e Variancia
             if(showEX){
                 abline(v=mu, col=2, lwd=1.5)
             }
             if(showVX){
                 d <- dnorm(mu+des, mu, des)
                 segments(mu-des, d, mu+des, d, col=2, lwd=1.5)
             }
             if(showPr){
                 rp.slider(panel, q, from=0, to=4, initval=0,
                           resolution=0.1, action=acum.norm,
                           showvalue=TRUE, title="quantil",
                           name="quantil")
             }
             if(!showPr){
                 rp.widget.dispose(panel, "quantil")
             }
         })
    panel
}

acum.norm <- function(panel){
    with(panel,
         {
             ## Probabilidade Acumulada
             if(showPr){                
                 x <- seq(0, q, by=0.01)
                 fx <- dnorm(x, mu, des)
                 polygon(c(x, rev(x)),
                         c(fx, rep(0, length(fx))),
                         col="gray70")
                 Pr <- round(pnorm(q, mu, des, lower=FALSE),
                             digits=3)
                 qq <- round(q, digits=3)
                 legend("topleft", bty="n", fill="gray70",
                        legend=substitute(P(0<~Z<=~q)==Pr,
                            list(q=qq, Pr=Pr)))
             }
     })
}

panel <- rp.control(title="Normal", size=c(300,100))
rp.slider(panel, mu, from=-4, to=4, initval=0, resolution=0.1,
          action=pn, showvalue=TRUE, title="mean")
rp.slider(panel, des, from=0, to=3, initval=1, resolution=0.1,
          action=pn, showvalue=TRUE, title="sd")
rp.checkbox(panel, showEX, action=pn, title="E(X)",
            labels="Mostrar o valor esperado?")
rp.checkbox(panel, showVX, action=pn, title="sd(X)",
            labels="Mostrar o desvio-padrão?")
rp.checkbox(panel, showPr, action=pn, title="Prob. acumulada",
            labels="Mostrar a probabilidade acumulada?")


rp.widget.dispose(panel, "quantil")


help(rpanel, h="html")

panel <- rp.control()
rp.slider(panel, v, 0, 1, no, name = "slider")

rp.widget.get(panel, "slider")


rp.var.get(panel$panelname, "q")

panel$panelname

