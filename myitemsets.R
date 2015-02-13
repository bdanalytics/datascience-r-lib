get_rules <- function(baskets_df, minsup, minconf) {
    ######################################################################
    # Extract association rules from itemsets
    #
    # Params:
    #  baskets_df:  similar to
    #                   data.frame(tid=seq(10, 50, 10),
    #                              basket=c("Beer, Nuts, Diaper",
    #                                       "Beer, Coffee, Diaper",
    #                                       "Beer, Diaper, Eggs",
    #                                       "Nuts, Eggs, Milk",
    #                                       "Nuts, Coffee, Diaper, Eggs, Milk"))
    #  minsup: minimum support
    #  minconf: minimum confidence
    #
    # Returns:
    #  dataframe of association rules
    #
    # Enhancements: Extract rules of type X -> {Y1, Y2}
    ######################################################################

    require(plyr)

    items <- sort(unique(unlist(sapply(1:nrow(baskets_df),
        function(row_pos) gsub("^\\s+|\\s+$", "",
                                unlist(strsplit(baskets_df[row_pos, "basket"], "[,]")))))))

    for (item in items)
        baskets_df[item] <- sapply(1:nrow(baskets_df),
            function(baskets_row_pos) length(grep(item,
                                                baskets_df[baskets_row_pos, "basket"])))
    print(baskets_df)

    frq1_itemsets_df <- data.frame(item1=items, itemset=items, freq=rep(1, length(items)))
    #frq1_itemsets_df <- mutate(frq1_itemsets_df, knt=sum(baskets_df[itemset]))
    frq1_itemsets_df$knt <- sapply(1:nrow(frq1_itemsets_df),
        function(itemsets_row_pos)
                            sum(baskets_df[frq1_itemsets_df[itemsets_row_pos, "itemset"]]))
    frq1_itemsets_df$sup <- frq1_itemsets_df$knt * 1.0 / nrow(baskets_df)
    frq1_itemsets_df <- subset(frq1_itemsets_df, sup >= minsup)
    #print(frq1_itemsets_df)

    sup_items <- frq1_itemsets_df$itemset
    frq2_itemsets_df <- rbind(data.frame(item1=t(combn(sup_items, 2))[,1],
                                         item2=t(combn(sup_items, 2))[,2]),
                              data.frame(item1=t(combn(rev(sup_items), 2))[,1],
                                         item2=t(combn(rev(sup_items), 2))[,2]))
    frq2_itemsets_df$itemset <- sapply(1:nrow(frq2_itemsets_df),
        function(itemsets_row_pos) paste(frq2_itemsets_df[itemsets_row_pos, "item1"],
                                            frq2_itemsets_df[itemsets_row_pos, "item2"],
                                            sep=","))
    frq2_itemsets_df$knt <- sapply(1:nrow(frq2_itemsets_df),
        function(itemsets_row_pos)
                            sum(baskets_df[frq2_itemsets_df[itemsets_row_pos, "item1"]] &
                                baskets_df[frq2_itemsets_df[itemsets_row_pos, "item2"]]))
    frq2_itemsets_df$sup <- frq2_itemsets_df$knt * 1.0 / nrow(baskets_df)
    #print(head(frq2_itemsets_df))
    frq2_itemsets_df <- subset(frq2_itemsets_df, sup >= minsup)
    #print(frq2_itemsets_df)

    frq2_itemsets_df$conf <- sapply(1:nrow(frq2_itemsets_df),
        function(frq2_itemsets_row_pos)
            frq2_itemsets_df[frq2_itemsets_row_pos, "sup"] /
            frq1_itemsets_df[frq1_itemsets_df$itemset ==
                            frq2_itemsets_df[frq2_itemsets_row_pos, "item1"],
                                "sup"])
    #     itemsets_df <- mutate(itemsets_df,
    #                       conf=sup / itemsets_df[itemsets_df$itemset == item1, "sup"])
    frq2_itemsets_df <- subset(frq2_itemsets_df, conf >= minconf)
    #print(frq2_itemsets_df)

    rules_df <- data.frame(itemset_X=frq2_itemsets_df$item1,
                           itemset_Y=frq2_itemsets_df$item2,
                           sup=frq2_itemsets_df$sup,
                           conf=frq2_itemsets_df$conf)
    print(rules_df)
    return(rules_df)
}
