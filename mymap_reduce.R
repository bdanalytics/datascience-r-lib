mycheck_prime <- function(n) n == 2L || all(n %% 2L:ceiling(sqrt(n)) != 0)
mycheck_prime(1)
mycheck_prime(2)
mycheck_prime(3)
mycheck_prime(4)
mycheck_prime(5)
mycheck_prime(7)
mycheck_prime(9)

map <- function(i) {
    ret_lst <- NULL
    for (p in c(2, seq(3, ceiling(i / 2), 2)))
        if ((i %% p == 0) & mycheck_prime(p)) {
            if (length(ret_lst) == 0)
                ret_lst <- list(list(p=p, i=i))
            else
                ret_lst <- append(ret_lst, list(list(p=p, i=i)))
        }
    return(ret_lst)
}
print(map(12))
print(map(15))
print(map(21))
print(map(24))
print(map(30))
print(map(49))

group <- function(p, tuple_lst) {
    i_grp <- NULL
    for (tuple in tuple_lst)
        if (tuple$p == p)
            i_grp <- c(i_grp, tuple$i)
    return(list(p=p, i_grp=i_grp))
}
print(group(2, c(map(24), map(30))))
print(group(5, c(map(24), map(30))))

reduce <- function(group_lst) {
    return(list(p=group_lst$p, i_sum=sum(group_lst$i_grp)))
}
print(reduce(group(2, c(map(24), map(30)))))
print(reduce(group(5, c(map(24), map(30)))))

print(reduce(group(6, c(map(15), map(21), map(24), map(30), map(49)))))
print(reduce(group(7, c(map(15), map(21), map(24), map(30), map(49)))))