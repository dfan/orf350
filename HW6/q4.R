library(igraph)

graphplot=function(X){
	ag=graph.adjacency(X, mode="undirected")
	V(ag)$colors=ifelse(degree(ag)<5, 'SkyBlue2', 'red')
	png(filename="gdp_nodewise.png", height=800, width=800)
	par(mai=c(0,0,0,0))
	plot.igraph(ag, vertex.color=V(ag)$colors, vertex.size=6, vertex.label.cex=0.8, layout=layout_nicely(ag))
	dev.off()
}

