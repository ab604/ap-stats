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
