library(ggplot2)

m <- as.matrix(read.csv('input/data-plate.txt', header=FALSE))
rownames(m) <- 1:16
colnames(m) <- 1:24
d_melt <- reshape2::melt(m)
colnames(d_melt) <- c('Row','Column','Y')

p <- ggplot()
p <- p + theme_bw() + theme(text=element_text(size=20), legend.key.size=grid::unit(2.0, 'lines'))
p <- p + geom_tile(data=d_melt, aes(x=Column, y=Row, z=Y, fill=Y), color='black', alpha=0.9)
p <- p + scale_fill_gradient2(midpoint=median(m), low='black', mid='gray50', high='white')
p <- p + scale_y_reverse()
p <- p + xlab('Plate Column') + ylab('Plate Row')
p <- p + coord_cartesian(xlim=c(0.5, 24.5), ylim=c(0.5, 16.5))
ggsave(p, file='output/fig3.png', dpi=300, w=6.4, h=4)
