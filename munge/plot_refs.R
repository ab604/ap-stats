# Plot AP data -----------------------------------------------------------------

256
# 

257
p1 <- ggplot(plot.ap,aes(x=ap.term_vec,y=ap.pubyear_vec))

258
#p1 + geom_boxplot(aes(fill = factor(ap.term_vec))) +

259
p1 + geom_boxplot()+
        
        260
#scale_fill_brewer(palette="Set1") +

261
xlab("") +
        
        262
# Amend y-axis label

263
ylab("Publication Year") +
        
        264
# Create yearly ticks 

265
scale_y_continuous(breaks=seq(1980,2015,5)) +
        
        266
# Remove the legend title

267
#theme(legend.title=element_blank()) +

268
# Add a plot titl

269
ggtitle("Publication Year of First 30 Google Scholar Hits")

270

271
# Plot MHC data ----------------------------------------------------------------

272

273
p2 <- ggplot(plot.mhc,aes(x=mhc.term_vec,y=mhc.pubyear_vec))

274
#p2 + geom_boxplot(aes(fill = factor(mhc.term_vec))) +

275
p2 + geom_boxplot()+
        
        276
#scale_fill_brewer(palette="Set1") +

277
xlab("") +
        
        278
# Amend y-axis label

279
ylab("Publication Year") +
        
        280
# Create yearly ticks 

281
scale_y_continuous(breaks=seq(1980,2015,5)) +
        
        282
# Remove the legend title

283
#theme(legend.title=element_blank()) +

284
# Add a plot titl

285
ggtitle("Publication Year of First 30 Google Scholar Hits")
