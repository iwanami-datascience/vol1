library(rjags) # R と JAGS をつなげる package
library(R2WinBUGS) # write.model() 使うため
model.bugs <- function()
{
	# コードリスト 1 の BUGS コード
}
file.model <- "model.bug.txt"
write.model(model.bugs, file.model)
