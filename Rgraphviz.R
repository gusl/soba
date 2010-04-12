###################################################
### chunk number 1: createGraph1
###################################################
library("Rgraphviz")
set.seed(123)
V <- letters[1:10]
M <- 1:4


###################################################
### chunk number 2: createGraph2
###################################################
g1 <- randomGraph(V, M, 0.2)


###################################################
### chunk number 3: plotDot
###################################################
plot(g1)


###################################################
### chunk number 4: plotNeato
###################################################
plot(g1, "neato")


###################################################
### chunk number 5: plotTwopi
###################################################
plot(g1, "twopi")


###################################################
### chunk number 6: rEG
###################################################
rEG <- new("graphNEL", nodes=c("A", "B"), edgemode="directed")
rEG <- addEdge("A", "B", rEG, 1)
rEG <- addEdge("B", "A", rEG, 1)

rEG <- new("graphNEL", nodes=c("A", "B", "C"), edgemode="directed")
rEG <- addEdge("A", "B", rEG, 1)
rEG <- addEdge("B", "C", rEG, 1)

eAttrs <- list()
eAttrs$color <- c("a~d"="blue", "c~h"="purple")

###################################################
### chunk number 7: recipEdgesComb
###################################################
plot(rEG, edgeAttrs=eAttrs)

#make the colors continuous
eAttrs$color <- c("b~c"="#ff00ff")

#start from ff0000 and move to 0000ff
#
# start out with a linear interpolation in color space
# from (255,0,0) to (0,0,255)
#
# we have n(n-1)/2 edges
# divide the space
#
# thick edges

###################################################
### chunk number 8: recipEdgesDistinct
###################################################
plot(rEG, recipEdges="distinct")


###################################################
### chunk number 9: removedEdges
###################################################
removedEdges(g1)


###################################################
### chunk number 10: getSubgraphs
###################################################
sg1 <- subGraph(c("a","d","j","i"), g1)
sg1
sg2 <- subGraph(c("b","e","h"), g1)
sg3 <- subGraph(c("c","f","g"), g1)


###################################################
### chunk number 11: subGplot
###################################################
subGList <- vector(mode="list", length=3)
subGList[[1]] <- list(graph=sg1)
subGList[[2]] <- list(graph=sg2, cluster=FALSE)
subGList[[3]] <- list(graph=sg3)
plot(g1, subGList=subGList)


###################################################
### chunk number 12: subGPlot2
###################################################
sg1 <- subGraph(c("a","c","d","e","j"), g1)
sg2 <- subGraph(c("f","h","i"), g1)
plot(g1, subGList=list(list(graph=sg1), list(graph=sg2)))


###################################################
### chunk number 13: edgeNames
###################################################
edgeNames(g1)
edgeNames(g1, recipEdges="distinct")


###################################################
### chunk number 14: defAttrs
###################################################
defAttrs <- getDefaultAttrs()


###################################################
### chunk number 15: defAttrs2
###################################################
plot(g1, attrs=list(node=list(label="foo", fillcolor="lightgreen"),
           edge=list(color="cyan"),
           graph=list(rankdir="LR")))


###################################################
### chunk number 16: baseLists
###################################################
nAttrs <- list()
eAttrs <- list()


###################################################
### chunk number 17: makeLabels1
###################################################
z <- strsplit(packageDescription("Rgraphviz")$Description, " ")[[1]]
z <- z[1:numNodes(g1)]
names(z) = nodes(g1)
nAttrs$label <- z


###################################################
### chunk number 18: makeLabels2
###################################################
eAttrs$label <- c("a~h"="Label 1", "c~h"="Label 2")


###################################################
### chunk number 19: makeLabels3
###################################################
attrs <- list(node=list(shape="ellipse", fixedsize=FALSE))


###################################################
### chunk number 20: figLabels
###################################################
plot(g1, nodeAttrs=nAttrs, edgeAttrs=eAttrs, attrs=attrs)


###################################################
### chunk number 21: edgeWeights
###################################################
ew <- as.character(unlist(edgeWeights(g1)))
ew <- ew[setdiff(seq(along=ew), removedEdges(g1))]
names(ew) <- edgeNames(g1)
eAttrs$label <- ew
## FIXME (wh 17.6.06): This does not work - see bug report:
## attrs$edge$labelfontsize="27"


###################################################
### chunk number 22: edgeWeightLabels
###################################################
plot(g1, nodeAttrs=nAttrs, edgeAttrs=eAttrs, attrs=attrs)


###################################################
### chunk number 23: colors
###################################################
## Specify node drawing color
nAttrs$color <- c(a="red", b="red", g="green", d="blue")

## Specify edge drawing color
eAttrs$color <- c("a~d"="blue", "c~h"="purple")

## Specify node fill color
nAttrs$fillcolor <- c(j="yellow")

## label color
nAttrs$fontcolor <- c(e="green", f="red")
eAttrs$fontcolor <- c("a~h"="green", "c~h"="brown")

nAttrs
eAttrs

###
### make a blue edge between a and b
V <- letters(1:3)
eAttrs <- list()

plot(g1, nodeAttrs=nAttrs, edgeAttrs=eAttrs, attrs=attrs)
###
###

###################################################
### chunk number 24: figColors
###################################################
plot(g1, nodeAttrs=nAttrs, attrs=attrs)
plot(g1, nodeAttrs=nAttrs, edgeAttrs=eAttrs, attrs=attrs)

###################################################
### chunk number 25: nodeShapes
###################################################
attrs$node$shape <- "ellipse"
nAttrs$shape <- c(g="box", f="circle", j="box", a="plaintext")


###################################################
### chunk number 26: figNodeShapes
###################################################
plot(g1, attrs=attrs, nodeAttrs=nAttrs)


###################################################
### chunk number 27: getLists1
###################################################
nodes <- buildNodeList(g1)
edges <- buildEdgeList(g1)


###################################################
### chunk number 28: getLists2
###################################################
nodes[[1]]
edges[[1]]


###################################################
### chunk number 29: buildwithAttrs
###################################################
nodes <- buildNodeList(g1, nodeAttrs=nAttrs, defAttrs=defAttrs$node)
edges <- buildEdgeList(g1, edgeAttrs=eAttrs, defAttrs=defAttrs$edge)
nodes[[1]]
edges[[1]]


###################################################
### chunk number 30: arrowheads
###################################################
for(j in c("a~e", "a~h"))
  edges[[j]]@attrs$arrowhead <- "open"


###################################################
### chunk number 31: plotbuild
###################################################
vv <- agopen(name="foo", nodes=nodes, edges=edges, attrs=attrs, 
  edgeMode="undirected")
plot(vv)


###################################################
### chunk number 32: graph17
###################################################
data(graphExamples)
z <- graphExamples[[8]]
nNodes <- length(nodes(z))

nA <- list()
nA$fixedSize<-rep(FALSE, nNodes)
nA$height <- nA$width <- rep("1", nNodes)
nA$label <- rep("z", nNodes)
nA$color <- rep("green", nNodes)
nA$fillcolor <- rep("orange", nNodes)
nA$shape <- rep("circle", nNodes)
nA$fontcolor <- rep("blue", nNodes)
nA$fontsize <- rep(14, nNodes)
nA <- lapply(nA, function(x) { names(x) <- nodes(z); x})


###################################################
### chunk number 33: graph17
###################################################
plot(z, nodeAttrs=nA)


###################################################
### chunk number 34: pieChartCalc
###################################################
set.seed(123)
counts = matrix(rexp(numNodes(g1)*4), ncol=4)

g1layout <- agopen(g1, name="foo")

makeNodeDrawFunction <- function(x) {
  force(x)
  function(node, ur, attrs, radConv) {
    nc <- getNodeCenter(node)
    pieGlyph(x, 
             xpos=getX(nc),
             ypos=getY(nc),
             radius=getNodeRW(node),
             col=rainbow(4))
    text(getX(nc), getY(nc), paste(signif(sum(x), 2)), 
      cex=0.5, col="white", font=2)
  }
}

drawFuns <- apply(counts, 1, makeNodeDrawFunction)


###################################################
### chunk number 35: pieChartGraph
###################################################
plot(g1layout, drawNode=drawFuns, main="Example Pie Chart Plot")


###################################################
### chunk number 36: clusterGraph1
###################################################
cG <- new("clusterGraph", clusters=list(a=c(1:10), b=c(11:13),
                            c=c(14:20), d=c(21, 22)))


###################################################
### chunk number 37: cGdot
###################################################
plot(cG, main="dot")


###################################################
### chunk number 38: cGtwopi
###################################################
par(bg="#e0e0e0")
plot(cG, "twopi", main="twopi")


###################################################
### chunk number 39: cGneato
###################################################
plot(cG, "neato", main="neato")


###################################################
### chunk number 40: bipartite1
###################################################
set.seed(123)
nodes1 <- paste(0:7)
nodes2 <- letters[1:10]

ft <- cbind(sample(nodes1, 24, replace=TRUE), 
            sample(nodes2, 24, replace=TRUE))
ft <- ft[!duplicated(apply(ft, 1, paste, collapse="")),]

g <-  ftM2graphNEL(ft, edgemode='directed')
g


###################################################
### chunk number 41: bipartitelayout
###################################################
twocolors <- c("#D9EF8B", "#E0F3F8")
nodeType <- 1 + (nodes(g) %in% nodes1)
nA = makeNodeAttrs(g, fillcolor=twocolors[nodeType])

sg1 = subGraph(nodes1, g)
sgL = list(list(graph=sg1, cluster = FALSE, attrs = c(rank="sink")))

att = list(graph = list(rankdir = "LR", rank = ""))


###################################################
### chunk number 42: figbipartite
###################################################
plot(g, attrs = att, nodeAttrs=nA, subGList = sgL)


###################################################
### chunk number 43: agopenSimpleDemo
###################################################
library("graph")
g1_gz <- gzfile(system.file("GXL/graphExample-01.gxl.gz",package="graph"), open="rb")
g11_gz <- gzfile(system.file("GXL/graphExample-11.gxl.gz",package="graph"), open="rb")
g1 <- fromGXL(g1_gz)
g11 <- fromGXL(g11_gz)
g1_11 <- join(g1, g11)
sgl <- vector(mode="list", length=2)
sgl[[1]] <- list(graph=g1, cluster=FALSE)
sgl[[2]] <- list(graph=g11, cluster=TRUE)
ng <- agopenSimple(g1_11, "tmpsg", subGList=sgl)


###################################################
### chunk number 44: DataDefaultsDemo1
###################################################
graphDataDefaults(ng)
nodeDataDefaults(ng)
edgeDataDefaults(ng)


###################################################
### chunk number 45: DataDefaultsDemo2
###################################################
graphDataDefaults(ng, c("size", "bgcolor")) <- c("1", "yellow")
nodeDataDefaults(ng, c("fontcolor", "width")) <- c("blue", 0.5)
edgeDataDefaults(ng, c("color", "style")) <- c("green", "dotted")


###################################################
### chunk number 46: DataDemo1
###################################################
graphData(ng, "bgcolor")
nodeData(ng, "a", c("fontcolor", "width"))
edgeData(ng, "f", "h", c("color", "arrowhead"))



###################################################
### chunk number 47: DataDemo2
###################################################
graphData(ng, "bgcolor") <- "orange"
clusterData(ng, 2, "bgcolor") <- "red"
nodeData(ng, "a", c("fontcolor", "width")) <- c("red", "0.8")
edgeData(ng, "f", "h", c("color", "style")) <- c("blue", "solid")


###################################################
### chunk number 48: layoutRenderDemo1
###################################################
plot(ng, "neato")
plot(ng, "circo")


###################################################
### chunk number 49: layoutRenderDemo1
###################################################
toFile(ng, layoutType="dot", filename="test_dot.svg", fileType="svg")
toFile(ng, layoutType="circo", filename="test_circo.ps", fileType="ps")
toFile(ng, layoutType="twopi", filename="test_twopi.dot", fileType="dot")


###################################################
### chunk number 50: 
###################################################
sessionInfo()


