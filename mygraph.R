# directed graphs

mycompute_nodeHITS <- function(outlink_lst, max_iterations=100) {
    ####    N:  number of pages
    ####    h:  hubbiness vector    : h[i]  <- page i hubbiness
    ####                                    <- sum(authority of successors)
    ####                              max(h) <- 1
    ####    a:  authority vector    : a[i]  <- page i authority
    ####                                    <- sum(hubbiness of predecessors)
    ####                              max(a) <- 1
    ####    L:  link matrix         : dim(L) <- c(N, N)
    ####                              if (page i links to page j)   L[i, j] <- 1
    ####                                                    else    L[i, j] <- 0
    ####    #h <- lambda * L * a
    ####    #a <- mu * t(L) * h
    ####
    ####    #h <- lambda * mu * L * t(L) * h
    ####    #a <- lambda * mu * t(L) * L * a
    ####
    ####    Figure 5.19:

    N <- 5
    L <- matrix(rep(0, N*N), nrow=N, ncol=N)
    #L[i, ] <- c(0, 0, 0, 0, 0)
    L[1, ] <- c(0, 1, 1, 1, 0)
    L[2, ] <- c(1, 0, 0, 1, 0)
    L[3, ] <- c(0, 0, 0, 0, 1)
    L[4, ] <- c(0, 1, 1, 0, 0)
    L[5, ] <- c(0, 0, 0, 0, 0)
    print(L)

    h <- matrix(rep(1, N), nrow=N, ncol=1)
    a <- matrix(rep(0, N), nrow=N, ncol=1)
    ha <- cbind(h, a); print("iteration 0:"); print(ha)

    iterate_HITS <- function(L, ha) {
        h <- ha[, 1]
        a <- t(L) %*% h
        a <- a / max(a)
        h <- L %*% a
        h <- h / max(h)
        #print(h)
        #print(a)
        return(cbind(h, a))
    }
    ha <- iterate_HITS(L, ha); print("iteration 1:"); print(ha)
    ha <- iterate_HITS(L, ha); print("iteration 2:"); print(ha)
    ha <- iterate_HITS(L, ha); print("iteration 3:"); print(ha)
    ha <- iterate_HITS(L, ha); print("iteration 4:"); print(ha)
    ha <- iterate_HITS(L, ha); print("iteration 5:"); print(ha)
    ha <- iterate_HITS(L, ha); print("iteration 6:"); print(ha)
    ha <- iterate_HITS(L, ha); print("iteration 7:"); print(ha)
}

mycompute_noderank <- function(outlink_lst, beta=1, max_iterations=100) {
    ###     N:          number of pages
    ###     L[N, N]:    link matrix
    ###                 if (page i links to page j) L[i, j] <- 1
    ###                                      else   L[i, j] <- 0
    ###     K[N, 1]:    out-links knt for page i
    ###     M[N, N]:    transition matrix
    ###                 if page j has k out-links & one of them is to page i
    ###                         M[i, j] <- i / k
    ###                 else    M[i, j] <- 0
    ###     Rank[N, 1]: page rank for page i

    N <- length(outlink_lst)

    mymap_nodes <- function(N, nodes) {
        values <- lapply(nodes, function(node) grep(node, LETTERS))
        res <- rep(0, N)
        for (val in values) res[val] <- 1
        return(res)
    }

    myconvert_graph_to_df <- function(link_lst) {
        L_df <- data.frame(matrix(rep(0, N), nrow=N, ncol=N),
                           row.names=LETTERS[seq(1,N)])
        names(L_df) <- paste0("to_", row.names(L_df))
        for (node in names(link_lst)) L_df[node, ] <- mymap_nodes(N, link_lst[[node]])
        print(L_df)
        return(L_df)
    }

    L_df <- myconvert_graph_to_df(outlink_lst)

    L <- as.matrix(L_df)
    K <- matrix(rowSums(L), nrow=N, ncol=1)
    M <- L
    #M[1, ] <- L[1, ] / K[1]
    for (i in seq(dim(M)[1])) M[i, ] <- L[i, ] / K[i]
    M <- t(M)
    format(M, digits=4)

    Rank <- matrix(rep(1/ N, N), nrow=N, ncol=1); format(Rank, digits=4)
    n_iterations <- 0
    repeat {
        Rank_next <- beta * M %*% Rank +
            (1 - beta) * matrix(rep(1/ N, N), nrow=N, ncol=1)
        #format(Rank_next, digits=4)
        if ((sum(abs(Rank_next - Rank)) < 0.0001) |
            (n_iterations >= max_iterations))
            break()
        Rank <- Rank_next
        n_iterations <- n_iterations + 1
    }
    Rank_df <- data.frame(Rank, row.names=row.names(L_df))
    print(sprintf("Iterations: %d", n_iterations))
    print(format(Rank_df, digits=4))
    return(Rank_df)
}
# q3_Rank_iter5 <- mycompute_noderank(list(A=c("B", "C"),
#                                          B=c("C"),
#                                          C=c("A")),
#                                     beta=1.00, max_iterations=5) * 3
