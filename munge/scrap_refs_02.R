## Scrape Google Scholar
## A. Bailey 8th April 2015 
## Based on Jeff Leeks script: https://gist.github.com/c5158965d77c21ade424.git

# Load libraries ---------------------------------------------------------------
library(httr)
library(XML)
library(dplyr)
library(RCurl)
library(ggplot2)
library(DT)
library(ProjectTemplate)

## Get the results for a specific term -----------------------------------------
# Scrape function for top npages hits
scrape_term = function(search_term,npages){
        # Base Google URL, get all results & query
         base_url = "http://scholar.google.com/scholar?lookup=0&q="
		# Create search string from terms concatenated with +
         search_string = paste0(strsplit(search_term," ")[[1]],collapse="+")
        # Create a list from the search terms with a data frame for each term
        dat = data.frame(NA,nrow=10*npages,ncol=3,NA)
        # Names for the columns in each data frame
        names(dat)=c("pub_year","cites","title","first.author")

        # Loop for searching
        for(i in 1:npages){               
                if(i==1){
                        url1 = paste0(base_url,search_string)
                }else{
                        start_string = paste0("&start=",(i-1)*10)
 
                        url1 = paste0(base_url,search_string,start_string)
 
                }
                 # Parse the document
                doc <- htmlParse(url1,encoding="UTF-8")
                # Extract paper titles
                titles <- xpathSApply(doc, "//h3[@class='gs_rt']", xmlValue)
                # Extract citations
                cites = xpathSApply(doc,
                                    '//*[contains(concat( " ", @class, " " ), concat( " ", "gs_ri", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "gs_fl", " " ))]//a[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]',
                                     xmlValue)
                # Subset number of citations
                cites = cites[1:10*3-2]
                cites = as.numeric(sapply(cites,function(x){strsplit(x,"Cited by ")[[1]][2]}))
                 # Extract publication details
                 pub <- xpathSApply(doc, "//div[@class='gs_a']", xmlValue)
                 # Extract year
                 pub_years =  as.integer(gsub(".*\\s(\\d{4})\\s.*", "\\1", pub))
                 # Extract first author
                 first_auth = sapply(strsplit(pub,","),head,1)
                 # Add to data frame
                 ind = ((i-1)*10+1):(i*10)
                 dat[ind,1] = pub_years
                 dat[ind,2] = cites
                 dat[ind,3] = titles
                 dat[ind,4] = first_auth
        }
        return(dat)
}

## Search for Antigen Processing key player terms ------------------------------
# Search terms
 ap.terms = c('ERAP1 aminopeptidase','tapasin',
             'major histocompatibility complex class I molecule -II',
			'calreticulin','erp57 reductase','calnexin', 
             'TAPBPR tapasin-related protein','UGGT glucosyltransferase')
# Number of terms
nterms=length(ap.terms)
# Preallocate vector
ap.term_data = vector(mode="list",length=nterms)
# Scrape top 30 hits for AP terms from Google Scholar 
npages = 3
for(i in 1:nterms){
        ap.term_data[[i]] = scrape_term(ap.terms[i],npages)
        ap.term_data[[i]] = cbind(ap.term_data[[i]],rep(ap.terms[i],npages*10))
        names(ap.term_data[[i]])[5] = "term"
        Sys.sleep(3)
        cat(i)
}
 
 # Create vector of search categories
ap.term_vec = as.vector(sapply(ap.term_data,function(x){x$term}))
# Put the term factor in order for the boxplot
ap.term_vec = reorder(ap.term_vec,rep(1:nterms,each=30))
# Make the axis abbreviated by changing labels 
levels(ap.term_vec) = c("ERAP1", "tapasin", "MHC I", "calreticulin","Erp57",
                     "calnexin","TAPBPR","UGGT")
# Create vector of publication year
ap.pubyear_vec = as.vector(sapply(ap.term_data,function(x){x$pub_year}))
# Get the titles
#ap.title_vec = as.vector(sapply(ap.term_data,function(x){x$title}))

# Get median publication date for each term
ap.median <- round(tapply(ap.pubyear_vec,ap.term_vec,
                          function(x){median(x,na.rm=T)}))
# Create data for plotting
plot.ap <- na.omit(data.frame(ap.pubyear_vec,ap.term_vec))
# Cache data -------------------------------------------------------------------
cache('ap.term_vec')

## Search for MHC I specific terms ---------------------------------------------
mhc.terms <- c('intext:major histocompatibility complex class I molecule -II -III -polymerase -nonclassical',
           'Structure of the human class i histocompatibility antigen -II -nonclassical -gene -antibody -plate -placenta -CDlb -genetics -Fc -DR1',
           'intext:mhc class I conformational -II -genes',
           'histocompatibility class i peptide repertoire -II -antibodies -Ib')
# Number of terms
nterms=length(mhc.terms)
# Preallocate vector
mhc.term_data = vector(mode="list",length=nterms)
# Scrape top 30 hits for MHC terms from Google Scholar 
npages = 3
for(i in 1:nterms){
        mhc.term_data[[i]] = scrape_term(mhc.terms[i],npages)
        mhc.term_data[[i]] = cbind(mhc.term_data[[i]],rep(mhc.terms[i],npages*10))
        names(mhc.term_data[[i]])[5] = "term"
        Sys.sleep(3
        cat(i)
}
# Create vector of search catergories
mhc.term_vec = as.vector(sapply(mhc.term_data,function(x){x$term}))
# Put the term factor in order for the boxplot
mhc.term_vec = reorder(mhc.term_vec,rep(1:nterms,each=30))
# Make the axis abbreviated by changing labels 
levels(mhc.term_vec) = c("Molecule", "Structure","Conformation",
                         "Pep. Reportoire")
# Create vector of publication year
mhc.pubyear_vec = as.vector(sapply(mhc.term_data,function(x){x$pub_year}))

# Get the titles
#mhc.title_vec = as.vector(sapply(mhc.term_data,function(x){x$title}))

# Get median publication date for each term
mhc.median <- round(tapply(mhc.pubyear_vec,mhc.term_vec,
                           function(x){median(x,na.rm=T)}))
# Create data for plotting
plot.mhc <- na.omit(data.frame(mhc.pubyear_vec,mhc.term_vec))
# Cache data -------------------------------------------------------------------
cache('mhc.term_vec')
# Clean up AP data for tables --------------------------------------------------
# Preallocate vector
nt = length(ap.terms)
ap.proc = vector(mode="list",length=nt)
for(j in 1:nt){
ap.proc[j] <- na.omit(ap.term_data[j])
ap.proc[[j]]$title <- enc2utf8(ap.proc[[j]]$title)
ap.proc[[j]]$title <- gsub("\\[HTML\\]", "", ap.proc[[j]]$title)
ap.proc[[j]]$title <- gsub("\\[PDF\\]", "", ap.proc[[j]]$title)
ap.proc[[j]]$title <- gsub( "[^[:alnum:][:blank:]+?&/\\-]", " "
                           ,ap.proc[[j]]$title)
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
 
181
                      No.Cites=ap.proc[[3]]$cites)
 
182
 
183
write.table(mhc.tab,file="mhctab.txt",sep="\t")
 
184
 
185
car.tab <- data.frame(First.Author=ap.proc[[4]]$first.author,
 
186
                      Title=ap.proc[[4]]$title,Pub.Year=ap.proc[[4]]$pub_year, 
 
187
                      No.Cites=ap.proc[[4]]$cites)
 
188
 
189
write.table(car.tab,file="cartab.txt",sep="\t")
 
190
 
191
erp.tab <- data.frame(First.Author=ap.proc[[5]]$first.author,
 
192
                      Title=ap.proc[[5]]$title,Pub.Year=ap.proc[[5]]$pub_year, 
 
193
                      No.Cites=ap.proc[[5]]$cites)
 
194
 
195
write.table(erp.tab,file="erptab.txt",sep="\t")
 
196
 
197
cln.tab <- data.frame(First.Author=ap.proc[[6]]$first.author,
 
198
                      Title=ap.proc[[6]]$title,Pub.Year=ap.proc[[6]]$pub_year, 
 
199
                      No.Cites=ap.proc[[6]]$cites)
 
200
 
201
write.table(cln.tab,file="clntab.txt",sep="\t")
 
202
 
203
tpr.tab <- data.frame(First.Author=ap.proc[[7]]$first.author,
 
204
                      Title=ap.proc[[7]]$title,Pub.Year=ap.proc[[7]]$pub_year, 
 
205
                      No.Cites=ap.proc[[7]]$cites)
 
206
 
207
write.table(tpr.tab,file="tprtab.txt",sep="\t")
 
208
 
209
ug.tab <- data.frame(First.Author=ap.proc[[8]]$first.author,
 
210
                      Title=ap.proc[[8]]$title,Pub.Year=ap.proc[[8]]$pub_year, 
 
211
                      No.Cites=ap.proc[[8]]$cites)
 
212
 
213
write.table(ug.tab,file="ugtab.txt",sep="\t")
 
214
 
215
# Clean-up MHC data and write tables -------------------------------------------
 
216
 
217
# Preallocate vector
 
218
nm = length(mhc.terms)
 
219
m.proc = vector(mode="list",length=nm)
 
220
 
221
for(j in 1:nm){
 
222
        m.proc[j] <- na.omit(mhc.term_data[j])
 
223
        m.proc[[j]]$title <- enc2utf8(m.proc[[j]]$title)
 
224
        m.proc[[j]]$title <- gsub("\\[HTML\\]", "", m.proc[[j]]$title)
 
225
        m.proc[[j]]$title <- gsub("\\[PDF\\]", "", m.proc[[j]]$title)
 
226
        m.proc[[j]]$title <- gsub( "[^[:alnum:][:blank:]+?&/\\-]", " "
 
227
                                   ,m.proc[[j]]$title)
 
228
}
 
229
 
230
# Write tables
 
231
mol.tab <- data.frame(First.Author=m.proc[[1]]$first.author,
 
232
                       Title=m.proc[[1]]$title,Pub.Year=m.proc[[1]]$pub_year, 
 
233
                       No.Cites=m.proc[[1]]$cites)
 
234
 
235
write.table(mol.tab,file="moltab.txt",sep="\t")
 
236
 
237
struc.tab <- data.frame(First.Author=m.proc[[2]]$first.author,
 
238
                      Title=m.proc[[2]]$title,Pub.Year=m.proc[[2]]$pub_year, 
 
239
                      No.Cites=m.proc[[2]]$cites)
 
240
 
241
write.table(struc.tab,file="sructab.txt",sep="\t")
 
242
 
243
conf.tab <- data.frame(First.Author=m.proc[[3]]$first.author,
 
244
                      Title=m.proc[[3]]$title,Pub.Year=m.proc[[3]]$pub_year, 
 
245
                      No.Cites=m.proc[[3]]$cites)
 
246
 
247
write.table(conf.tab,file="conftab.txt",sep="\t")
 
248
 
249
pep.tab <- data.frame(First.Author=m.proc[[4]]$first.author,
 
250
                      Title=m.proc[[4]]$title,Pub.Year=m.proc[[4]]$pub_year, 
 
251
                      No.Cites=m.proc[[4]]$cites)
 
252
 
253
write.table(pep.tab,file="peptab.txt",sep="\t")
 
254
 
255
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
