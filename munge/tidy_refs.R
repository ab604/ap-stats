# Clean up AP data for tables --------------------------------------------------
# A. Bailey 10th April 2015
library(ProjectTemplate)

# Preallocate vector
nt = length(ap.terms)
ap.proc = vector(mode="list",length=nt)
# Clean up
for(j in 1:nt){
        ap.proc[j] <- na.omit(ap.term_data[j])
        ap.proc[[j]]$title <- enc2utf8(ap.proc[[j]]$title)
        ap.proc[[j]]$first.author <- enc2utf8(ap.proc[[j]]$first.author)
        ap.proc[[j]]$title <- gsub("\\[HTML\\]", "", ap.proc[[j]]$title)
        ap.proc[[j]]$title <- gsub("\\[CITATION\\]", "", ap.proc[[j]]$title)
        ap.proc[[j]]$title <- gsub("\\[PDF\\]", "", ap.proc[[j]]$title)
        ap.proc[[j]]$title <- gsub("^\\s+|\\s+$", "", ap.proc[[j]]$title)
        ap.proc[[j]]$title <- gsub("^ C", "", ap.proc[[j]]$title)
        ap.proc[[j]]$title <- gsub( "[^[:alnum:][:blank:]+?&/\\-]", " "
                                    ,ap.proc[[j]]$title)
        ap.proc[[j]]$first.author <- gsub( "[^[:alnum:][:blank:]+?&/\\-]", " "
                                    ,ap.proc[[j]]$first.author)
        ap.proc[[j]]$cites[is.na(ap.proc[[j]]$cites)] <- 0
        
}

# Write tables
erap.tab <- data.frame(First.Author=ap.proc[[1]]$first.author,
                       Title=ap.proc[[1]]$title,Pub.Year=ap.proc[[1]]$pub_year, 
                       No.Cites=ap.proc[[1]]$cites)
write.table(erap.tab,file="src/eraptab.txt",sep="\t")

tpn.tab <- data.frame(First.Author=ap.proc[[2]]$first.author,
                      Title=ap.proc[[2]]$title,Pub.Year=ap.proc[[2]]$pub_year,
                      No.Cites=ap.proc[[2]]$cites)
write.table(tpn.tab,file="src/tpntab.txt",sep="\t")

mhc.tab <- data.frame(First.Author=ap.proc[[3]]$first.author,
                      Title=ap.proc[[3]]$title,Pub.Year=ap.proc[[3]]$pub_year, 
                      No.Cites=ap.proc[[3]]$cites)
write.table(mhc.tab,file="src/mhctab.txt",sep="\t")

car.tab <- data.frame(First.Author=ap.proc[[4]]$first.author,
                      Title=ap.proc[[4]]$title,Pub.Year=ap.proc[[4]]$pub_year,
                      No.Cites=ap.proc[[4]]$cites)
write.table(car.tab,file="src/cartab.txt",sep="\t")

erp.tab <- data.frame(First.Author=ap.proc[[5]]$first.author,
                      Title=ap.proc[[5]]$title,Pub.Year=ap.proc[[5]]$pub_year, 
                      No.Cites=ap.proc[[5]]$cites)
write.table(erp.tab,file="src/erptab.txt",sep="\t")

cln.tab <- data.frame(First.Author=ap.proc[[6]]$first.author,
                      Title=ap.proc[[6]]$title,Pub.Year=ap.proc[[6]]$pub_year, 
                      No.Cites=ap.proc[[6]]$cites)
write.table(cln.tab,file="src/clntab.txt",sep="\t")

tpr.tab <- data.frame(First.Author=ap.proc[[7]]$first.author,
                      Title=ap.proc[[7]]$title,Pub.Year=ap.proc[[7]]$pub_year,
                      No.Cites=ap.proc[[7]]$cites)
write.table(tpr.tab,file="src/tprtab.txt",sep="\t")

ug.tab <- data.frame(First.Author=ap.proc[[8]]$first.author,
                   Title=ap.proc[[8]]$title,Pub.Year=ap.proc[[8]]$pub_year,
                     No.Cites=ap.proc[[8]]$cites)
write.table(ug.tab,file="src/ugtab.txt",sep="\t")

# Clean-up MHC data and write tables -------------------------------------------
# Preallocate vector
nm = length(mhc.terms)
m.proc = vector(mode="list",length=nm)
# Clean up
for(j in 1:nm){
        m.proc[j] <- na.omit(mhc.term_data[j])
        m.proc[[j]]$title <- enc2utf8(m.proc[[j]]$title)
        m.proc[[j]]$first.author <- enc2utf8(m.proc[[j]]$first.author)
        m.proc[[j]]$title <- gsub("\\[CITATION\\]", "", m.proc[[j]]$title)
        m.proc[[j]]$title <- gsub("\\[HTML\\]", "", m.proc[[j]]$title)
        m.proc[[j]]$title <- gsub("\\[PDF\\]", "", m.proc[[j]]$title)
        m.proc[[j]]$title <- gsub( "[^[:alnum:][:blank:]+?&/\\-]", " "
                                   ,m.proc[[j]]$title)
        m.proc[[j]]$title <- gsub("^\\s+|\\s+$", "", m.proc[[j]]$title)
        m.proc[[j]]$title <- gsub("^ C", "", m.proc[[j]]$title)
        
        m.proc[[j]]$first.author <- gsub( "[^[:alnum:][:blank:]+?&/\\-]", " "
                                           ,m.proc[[j]]$first.author)
        m.proc[[j]]$cites[is.na(m.proc[[j]]$cites)] <- 0
}
# Write tables
mol.tab <- data.frame(First.Author=m.proc[[1]]$first.author,
                      Title=m.proc[[1]]$title,Pub.Year=m.proc[[1]]$pub_year, 
                      No.Cites=m.proc[[1]]$cites)
write.table(mol.tab,file="src/moltab.txt",sep="\t")

struc.tab <- data.frame(First.Author=m.proc[[2]]$first.author,
                        Title=m.proc[[2]]$title,Pub.Year=m.proc[[2]]$pub_year, 
                        No.Cites=m.proc[[2]]$cites)
write.table(struc.tab,file="src/sructab.txt",sep="\t")

conf.tab <- data.frame(First.Author=m.proc[[3]]$first.author,
                       Title=m.proc[[3]]$title,Pub.Year=m.proc[[3]]$pub_year, 
                       No.Cites=m.proc[[3]]$cites)
write.table(conf.tab,file="src/conftab.txt",sep="\t")

pep.tab <- data.frame(First.Author=m.proc[[4]]$first.author,
                      Title=m.proc[[4]]$title,Pub.Year=m.proc[[4]]$pub_year, 
                      No.Cites=m.proc[[4]]$cites)
write.table(pep.tab,file="src/peptab.txt",sep="\t")
