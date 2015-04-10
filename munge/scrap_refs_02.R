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
cache('ap.median')
cache('ap.pubyear_vec')
cache('plot.ap')
cache('ap.terms')

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
        Sys.sleep(3)
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
cache('mhc.median')
cache('mhc.pubyear_vec')
cache('plot.mhc')
cache('mhc.terms')
