
Pause.Fun  <- function(msg=NULL) {	#  pause
# Prompt for a carriage return.
if (!is.null(msg)) cat(msg,"\n")
cat("Press Return to continue Q to stop()...\n")
temp <- readline()
if (temp == "Q") stop("user interrupt", call.=F)
# invisible()
}