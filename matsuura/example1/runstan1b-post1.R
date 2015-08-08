library(rstan)

write.table(data.frame(summary(fit)$summary),
   file='output/fit_summary.txt', sep='\t', quote=FALSE, col.names=NA)
pdf('output/fit_traceplot.pdf', width=600/72, height=600/72)
traceplot(fit)
# traceplot(fit, pars=c('a','b'), window=c(100,1000))
dev.off()