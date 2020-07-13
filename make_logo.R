setwd('~/Projects/scrySticker/')

getCircle <- function(center = c(0,0), radius = 1, bumps = 0){
    t <- seq(0, 2*pi, length.out = 500)
    x <- cos(t)
    y <- sin(t)
    # perturb based on random gaussian bumps
    bumps.x <- NULL
    bumps.y <- NULL
    for(i in seq_len(bumps)){
        width <- 10+rgamma(1, rate = .2, shape = 5)
        height <- min(abs(rnorm(4)))
        mid <- sample(1:500, 1)
        dir <- sample(c(-4,4),1)
        bigbump <- height*dir*dnorm(-499:1000, mean=mid, sd=width)
        bump <- bigbump[1:500] + bigbump[501:1000] + bigbump[1001:1500]
        bumps.x <- rbind(bumps.x, bump)
        
        width <- 10+rgamma(1, rate = .2, shape = 5)
        height <- min(abs(rnorm(4)))
        mid <- sample(1:500, 1)
        dir <- sample(c(-4,4),1)
        bigbump <- height*dir*dnorm(-499:1000, mean=mid, sd=width)
        bump <- bigbump[1:500] + bigbump[501:1000] + bigbump[1001:1500]
        bumps.y <- rbind(bumps.y, bump)
    }
    if(bumps>0){
        out <- cbind(center[1] + radius * (x + colSums(bumps.x)),
                     center[2] + radius * (y + colSums(bumps.y)))
    }else{
        out <- cbind(center[1] + radius * x,
                     center[2] + radius * y)
    }
    return(out)
}


darkpurple <- brewer.pal(10,'Paired')[10]
lightpurple <- brewer.pal(10,'Paired')[9]
lightblue <- brewer.pal(9,'Blues')[2]
darkblue <- colorby(c(1:10), colors = c(brewer.pal(9,"Blues")[9], 'black'))[6]
############################
#### single hex of cells ###
############################
# pdf(file='cells.pdf', width=5,height = 5, bg = 'transparent')
# par(mar=c(1,1,1,1))
plot(c(-2,2),c(-2,2), col='transparent', asp=1, axes=FALSE, xlab='',ylab='')
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =  darkblue)
for(i in 1:6){
    x <- c(0,1,1,0,-1,-1)[i]
    y <- c(2/sqrt(3), 1/sqrt(3), -1/sqrt(3), -2/sqrt(3), -1/sqrt(3), 1/sqrt(3))[i]
    circ <- getCircle(center=c(y,x), radius=.55, bumps=15)
    lines(circ, lwd=5, col=darkpurple)
    polygon(circ, border=NA, col=alpha(lightblue, alpha = .8))
    lines(circ, lwd=3, col=lightpurple)
    #points(y,x, cex=2.5, col=darkpurple, pch=16)
    circ2 <- getCircle(center=c(y,x), radius=.23, bumps=8)
    lines(circ2, lwd=4, col=lightpurple)
    polygon(circ2, border=NA, col=darkpurple)
}
circ <- getCircle(radius=.47)
lines(circ, lwd=5, col=darkpurple)
for(i in 1:8){
    angle <- c(135,105,75,45, -45,-75,-105,-135)[i]
    x <- .66 * cos(pi * angle/180)
    y <- .66 * sin(pi * angle/180)
    lines(c(0,x),c(0,y), lwd=5, col=darkpurple)
    lines(c(0,x),c(0,y), lwd=3, col=lightpurple)
}
polygon(circ, border=NA, col='white')
lines(circ, lwd=3, col=lightpurple)
#points(0,0, cex=2.7, col=darkpurple, pch=16)
circ2 <- getCircle(radius=.25)
lines(circ2, lwd=4, col=lightpurple)
polygon(circ2, border=NA, col=darkpurple)
points(0,0, pch=16, col=1, cex=3)
# dev.off()
# par(mar=c(5.1,4.1,4.1,2.1))















##########################
#### hex grid of cells ### (vertical)
##########################
plot(c(-5,5),c(-6,5), col='white', asp=1)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =  darkblue)
for(i in -5:5){
    if(i %% 2 == 0){
        x <- seq(-5,5, by=2/sqrt(3))
    }else{
        x<- seq(-5+1/sqrt(3),5, by=2/sqrt(3))
    }
    if(i==0){
        eye.x <- x[which.max(x >= 0)]
        x <- x[-which.max(x >= 0)]
    }
    y <- rep(i,length(x))
    for(j in 1:length(x)){
        circ <- getCircle(center=c(y[j],x[j]), radius=runif(1,.45,.55), bumps=15)
        lines(circ, lwd=5, col = darkpurple)
        polygon(circ, border=NA, col=alpha(lightblue, alpha = .8))
        lines(circ, lwd=3, col=lightpurple)
        #points(y,x, cex=2.5, col=darkpurple, pch=16)
        circ2 <- getCircle(center=c(y[j],x[j])+rnorm(2,sd=.05), radius=runif(1,.17,.23), bumps=15)
        lines(circ2, lwd=4, col=lightpurple)
        polygon(circ2, border=NA, col=darkpurple)
    }
}

circ <- getCircle(center = c(0,eye.x), radius=.47)
lines(circ, lwd=5, col=darkpurple)
for(i in 1:8){
    angle <- c(135,105,75,45, -45,-75,-105,-135)[i]
    x <- .66 * cos(pi * angle/180)
    y <- eye.x + .66 * sin(pi * angle/180)
    lines(c(0,x),c(eye.x,y), lwd=5, col=darkpurple)
    lines(c(0,x),c(eye.x,y), lwd=3, col=lightpurple)
}
polygon(circ, border=NA, col='white')
lines(circ, lwd=3, col=lightpurple)
circ2 <- getCircle(center = c(0,eye.x), radius=.25)
lines(circ2, lwd=4, col=lightpurple)
polygon(circ2, border=NA, col=darkpurple)
points(0,eye.x, pch=16, col=1, cex=3)




##########################
#### hex grid of cells ### (horizontal)
##########################
plot(c(-5,5),c(-5,5), col='white', asp=1)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =  darkblue)
coords <- lapply(-5:5, function(i){
    if(i %% 2 == 0){
        x <- seq(-5,5, by=2/sqrt(3))
    }else{
        x<- seq(-5+1/sqrt(3),5, by=2/sqrt(3))
    }
    if(i==0){
        eye.x <- x[which.max(x >= 0)]
        x <- x[-which.max(x >= 0)]
    }
    y <- rep(i,length(x))
    data.frame(x,y)
})
coords <- do.call(rbind, coords)
coords <- coords[sample(nrow(coords)),]
y <- coords$x
x <- coords$y
for(j in 1:nrow(coords)){
    circ <- getCircle(center=c(y[j],x[j]), radius=runif(1,.45,.55), bumps=15)
    lines(circ, lwd=5, col = darkpurple)
    polygon(circ, border=NA, col=alpha(lightblue, alpha = .8))
    lines(circ, lwd=3, col=lightpurple)
    #points(y,x, cex=2.5, col=darkpurple, pch=16)
    circ2 <- getCircle(center=c(y[j],x[j])+rnorm(2,sd=.05), radius=runif(1,.17,.23), bumps=15)
    lines(circ2, lwd=4, col=lightpurple)
    polygon(circ2, border=NA, col=darkpurple)
}


points(eye.y, eye.x, col='white')
col.seq <- colorby(1:50, colors = c('white',brewer.pal(5,'Set3')[2]))
cex.seq <- seq(1,10, length.out = 50)
alpha.seq <- seq(1,0, length.out = 50)

for(i in 1:50){
    points(eye.y, eye.x, col=alpha(col.seq[i], alpha=alpha.seq[i]), pch=1,
           cex = cex.seq[i])
}



circ <- getCircle(center = c(0,eye.x), radius=.47)
lines(circ, lwd=5, col=darkpurple)
for(i in 1:8){
    angle <- c(135,105,75,45, -45,-75,-105,-135)[i]
    x <- .66 * cos(pi * angle/180)
    y <- eye.x + .66 * sin(pi * angle/180)
    lines(c(0,x),c(eye.x,y), lwd=5, col=darkpurple)
    lines(c(0,x),c(eye.x,y), lwd=3, col=lightpurple)
}
polygon(circ, border=NA, col='white')
lines(circ, lwd=3, col=lightpurple)
circ2 <- getCircle(center = c(0,eye.x), radius=.25)
lines(circ2, lwd=4, col=lightpurple)
polygon(circ2, border=NA, col=darkpurple)
points(0,eye.x, pch=16, col=1, cex=3)
