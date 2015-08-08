library(ggplot2)

# after estimation

la <- rstan::extract(fit)
r_median <- apply(la$r, c(2,3), median)
png('output/fig4-left.png', pointsize=72, width=2400, height=2100)
persp(1:Ni, 1:Nj, r_median, theta=55, phi=40, expand=0.5,
   border='black', col='gray95', xlab='Plate Row', ylab='Plate Column', zlab='r', lwd=2)
dev.off()

mean_Y <- sapply(1:T, function(t) mean(m[d_design==t])) - mean(m)
qua <- apply(la$beta, 2, quantile, prob=c(0.1, 0.5, 0.9))
d_est <- data.frame(X=mean_Y, t(qua))
colnames(d_est) <- c('X', 'p10', 'p50', 'p90')

p <- ggplot()
p <- p + theme_bw() + theme(text=element_text(size=24))
p <- p + geom_pointrange(data=d_est, aes(x=X, y=p50, ymin=p10, ymax=p90), color='gray5', fill='gray95', size=0.8, shape=21)
p <- p + geom_abline(aes(slope=1, intercept=0), color='black', alpha=0.6, linetype='dashed')
p <- p + xlab(expression(bar(Y[T[i][j]]))) + ylab(expression(beta[t]))
p <- p + coord_cartesian(xlim=c(-5, 5), ylim=c(-5, 5))
ggsave(file='output/fig4-right.png', plot=p, dpi=300, width=4.534, height=4.534)