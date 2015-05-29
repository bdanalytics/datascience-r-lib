#suppressPackageStartupMessages(require())

myadd_chunk <- function(chnk_df, label, major.inc=FALSE) {
    # Can df elements be used in Rmd chunk specs ?
    #if (is.null(label)) stop("label parameter not specified")
    if (is.null(chnk_df)) {    # First chunk
        upd_chnk_df <- data.frame(label=label,
                                step_major=1,
                                step_minor=0,
                                bgn=proc.time()["elapsed"],
                                end=NA,
                                elapsed=NA)
    } else {
        chnk_df[nrow(chnk_df), "end"] <- proc.time()["elapsed"]
        chnk_df[nrow(chnk_df), "elapsed"] <- proc.time()["elapsed"] -
                                            chnk_df[nrow(chnk_df), "bgn"]
        upd_chnk_df <- rbind(chnk_df,
            data.frame(label=label,
                step_major=ifelse(major.inc,
                                        tail(chnk_df$step_major, 1)+1,
                                        tail(chnk_df$step_major, 1)),
                step_minor=ifelse(major.inc, 0,
                                        tail(chnk_df$step_minor, 1)+1),
                bgn=proc.time()["elapsed"],
                end=NA,
                elapsed=NA))
    }

    row.names(upd_chnk_df) <- 1:nrow(upd_chnk_df)
    print(tail(upd_chnk_df, min(2, nrow(upd_chnk_df))))
    return(upd_chnk_df)
}
# glb_chunks_df <- myadd_chunk(NULL, "import.data")
# glb_chunks_df <- myadd_chunk(glb_chunks_df, "inspect.data", major.inc=FALSE)
# glb_chunks_df <- myadd_chunk(glb_chunks_df, "predict.data.new", major.inc=TRUE)

mydsp_chunk <- function(chunks_df) {
    last_row <- tail(chunks_df, 1)
    return(paste0(last_row$step_major, ".",
                  last_row$step_minor, ": ",
                  gsub(".", " ", last_row$label, fixed=TRUE)))
}
#mydsp_chunk(glb_chunks_df)

myplt_chunk <- function(chunks_df) {
    print(orderBy(~ -duration, 
                  (chunks_df <- head(mutate(chunks_df, duration=end-bgn), -1))))
    print(sprintf("Total Elapsed Time: %s secs", 
                  format(max(chunks_df$end), big.mark=',')))
    tmp_chunks_df <- subset(chunks_df, (step_minor == 0) & 
                                            (label != "display.session.info"), 
                            select=c(label, step_major))
    names(tmp_chunks_df)[1] <- "label_major"
    plt_chunks_df <- merge(chunks_df, tmp_chunks_df, all.x=TRUE)
    plt_chunks_df$step_major_desc <- max(plt_chunks_df$step_major) - 
                                                plt_chunks_df$step_major
    print(ggplot(plt_chunks_df, aes(x=reorder(label_major, step_major_desc), 
                                     y=duration, fill=factor(step_minor))) + 
                     geom_bar(stat="identity") + coord_flip())
}                 

