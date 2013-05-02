plot_odds <- function(x, title = NULL){
  tmp <- data.frame(cbind(exp(coef(x)), exp(confint(x))))
  odds <- tmp[-1,]
  names(odds) <- c('OR', 'lower', 'upper')
  odds$vars <- row.names(odds)
  modelCharacteristics <- as.character(as.expression(substitute(
    "(Intercept)" == ic * "," ~~ lambda == la * "," ~~ chi^2 == c2 * "," ~~ "AIC" == aic, 
    list(ic=sprintf("%.2f", exp(coef(x)[1])),
         la=sprintf("%.2f", logLik(x)),
         c2=sprintf("%.2f", with(x, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)), digits=3),
         aic=sprintf("%.2f", x$aic)))))
  ticks <- c(seq(.1, 1, by =.1), seq(0, 10, by =1), seq(10, 100, by =10))
  plot <- ggplot(odds, aes(y= OR, x = reorder(vars, OR))) +
          geom_point() +
          geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
          scale_y_log10(breaks=ticks, labels = ticks) +
          scale_x_discrete(labels='') +
          geom_hline(yintercept = 1, linetype=2) +
          coord_flip() +
          labs(title = title, x = '', y = 'Odds Ratio')
  return(plot + annotate("text", label=modelCharacteristics, 
                         parse=TRUE, x=-Inf, y=Inf, vjust=-0.5, hjust=1.1))
}