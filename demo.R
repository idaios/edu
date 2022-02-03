## a software to test if sampling schemes (given fair sampling per pop) affects p-values
## we have two populations the first contains 10000 individuals and the second 100000.
p1 <- 0.2
p2 <- 0.2
N1 <- 10000
N2 <- 100000

## generate two populations
pop1 <- sample(c(0,1), size=N1, replace=TRUE, prob=c(p1, 1-p1))
pop2 <- sample(c(0,1), size=N2, replace=TRUE, prob=c(p2, 1-p2))


##k <- 2000
##p <- (1:(k-1))/k
##p
## p represents the percentage of the first population in the sample
p  <- c(0.1, ## fair sampling according to the popsize
        0.25,
        0.5,
        0.75,
        0.9)

replications <- 5000
pvalues <- matrix(NA, nrow=length(p), ncol=replications)
samplesize=800
for(i in 1:length(p)){
    ## sample size per population
    n1 <- samplesize*p[i]
    n2 <- samplesize*(1-p[i])
    ## get a sample
    for(j in 1:replications){
        sample1 <- sample(pop1, n1)
        sample2 <- sample(pop2, n2)
        ## count the 0s and 1s
        count1 <- table(factor(sample1, levels=c(0,1)))
        count2 <- table(factor(sample2, levels=c(0,1)))
        ## construct the matrix for the chi2
        mat <- matrix(c(count1, count2), nrow=2)
        xx <- chisq.test(mat, correct=FALSE)$p.value
        pvalues[i,j] <- xx
    }
}

maxden <- max(apply(pvalues, 1, function(x){ max(density(x)$y) }))
pdf("densities_chiSquare_equal.pdf")
plot(density(pvalues[1,]), xlab = "P value", ylab="Counts", type='l', col=1, ylim = c(0, maxden), main="Dependence of chi2 pvalue on sampling")
for(i in 2:length(p)){
    points(density(pvalues[i,]), type='l', col=i)
}
legend("topright", legend=p, pch=19, col=1:length(p))
dev.off()
    
