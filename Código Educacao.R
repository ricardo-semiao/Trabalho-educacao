library(tidyverse)
library(aRxiv) #dados artigos, mas sem nÃºmero de citaÃ§Ãµes
library(rAltmetric) #dados de redes sociais
library(scholar) #dados de ranking dos autores
library(semscholar) #nÃºmero de citaÃ§Ãµes e artigos dos autores, e citaÃ§Ãµes por artigo
library(rorcid) #dados de formaÃ§Ã£o, bolsas, etc
library(textcat) #linguagem do texto
library(textreadr) #ler paginas
library(rvest)
library(stargazer)
library(coeftest)

#dicionários de nomes
{
  dic = readLines("m:/Users/Marcus/Downloads/0717-182/nam_dict.txt")
  dic = str_trim(gsub("([MF?=]+ .{1,20})( +[0-9+-].+)", "\\1", dic))
  dic = gsub("  ", " ", dic)
  dic = gsub("\\+", " ", dic)
  
  a = sapply(dic, strsplit, list(split=" "))
  names(a) = NULL
  for(i in 1:length(a)){
    a[[i]] = a[[i]][1:2]
    names(a[[i]]) = c("V1", "V2")}
  dic2 = as.data.frame(bind_rows(a))
  dic2 = dic2[!duplicated(dic2$V2),]
}


#baixar arxiv
{
date = cbind(c('20190105*', '20190204*'),
             c('20190305*', '20190504*'),
             c('20190505*', '20190704*'),
             c('20190705*', '20190904*'),
             c('20190905*', '20191104*'),
             c('20191105*', '20200104*'))

arxiv.df = NULL

for(i in 1:ncol(date)){
  query = paste0('submittedDate:[', date[1,i], ' TO ', date[2,i], ']')
  print(arxiv_count(query))
  arxiv.df = rbind(arxiv.df, arxiv_search(query=query, batchsize=200, limit=200))}

df = data.frame(
  arxivID = arxiv.df$id, #id arxiv
  Autores = arxiv.df$authors,
  Data = arxiv.df$submitted, #data (transformar em dummie trimestra/ver outras opÃ§Ãµes)
  Uni = arxiv.df$affiliations != "", #dummie afiliaÃ§Ã£o (universidade)
  Jornal = arxiv.df$journal_ref != "", #dummie jornal
  Autores = unlist(lapply(strsplit(arxiv.df$authors, "\\|"),length)), #número de autores
  Tema = arxiv.df$primary_category,
  stringsAsFactors = FALSE) #categoria

rm(date, query)
}


#genero por nome
{
  a = strsplit(df$Autores, "\\|")
  
  b = list()
  for(i in a){
    d = character()
    for(j in i){
      d = c(d, strsplit(j, " ")[[1]][1])}
    b = append(b, list(d))}
  
  # gender.c
  {
    bench = c("M")
    d = c()
    for(i in b){
      e = character()
      for(j in i){
        if(j %in% dic2$V2){
          e = c(e, dic2$V1[which(j == dic2$V2)][1])}
        else{e = NA
        break}}
      if(is.na(e)){d = c(d, NA)}
      else{d = c(d, mean(e %in% bench))}}
    dc = d
  }
  
  # babynames
  {
    bab = babynames::babynames
    colnames(bab) = c("y","sex","name", "n", "p")
    bab = bab[bab$y==2017,]
    
    k = 0.75
    d = c()
    for(i in b){
      e = character()
      for(j in i){
        if(j %in% bab$name){
          if(bab[which(j == bab$name),] > k){
            e = c(e, bab$sex[which(j == bab$name)])}
          else{e = NA
          break}}
        else{e = NA
        break}}
      if(is.na(e)){d = c(d, NA)}
      else{d = c(d, mean(e == "M"))}}
    db = d
  }
  
  # gender R (não utilizado)
  {
    k = 0.75
    d = c()
    for(i in b){
      e = character()
      for(j in i){
        f = gender(j, method="napp")
        lf = numeric()
        for(l in f){
          if(nrow(l)){if(l$gender %in% c("male", "female")){
            lf = c(lf, l[,paste0("proportion_",l$gender)] > k)}}
          else{lf = FALSE
          break}}
        if(any(lf)){
          e = c(e, l$gender)}
        else{e = NA
        break}}
      if(any(is.na(e))){d = c(d, NA)}
      else{d = c(d, mean(e == "male"))}}
    d
    }
  
  df$GeneroC = dc
  df$GeneroB = db
  
  df = df[!is.na(df$GeneroC) | !is.na(df$GeneroB),]
  rm(dic,dic2,bab,dc,db,d,bench,e,k,a,b)
  #df = na.omit(df)
}


#semscholar
{
  sems.df = NULL
  id = paste0('arxiv:', substr(df$arxivID,1,10))
  for(i in 1:nrow(df)){
    a = NA
    try({a = c(df$arxivID[i],s2_papers(id[i]))})
    sems.df = rbind(sems.df, a)}
  sems.df = as.data.frame(sems.df)
  
  #sems.df = sems.df[1:197,]
  #sems.df = as.matrix(sems.df)
  #cbind(1:210, sems.df$V1, sems.df$arxiv_id, df$arxivID)
  
  semcite = NULL
  for(i in 1:nrow(df)){
    a = nrow(sems.df$citations[[i]][[1]])
    if(is.null(a)){a=NA}
    semcite[i] = a}
  
  df$CiteSem = semcite #número de citações
  df$CiteInfSem = sems.df$influential_citation_count #número de citações influentes
  
  rm(semcite)
}


#Altmetrics
{
  altm.df = list()
  id = substr(df$arxivID,1,10)
  for(i in 1:nrow(df)){
    try({altm.df[[id[i]]] = list(c(arxivID=df$arxivID[i],
                                    unlist(altmetrics(arxiv=id[i]))))})}
  
  cols = grep("arxivID|context.+mean|cited_by|readers_cout|cohorts|score", names(altm.df[[3]][[1]]), value=TRUE)
  
  b = list()
  for(i in id){
    b[[i]] = altm.df[[i]][[1]][cols]}
  
  bind_rows(b)
  
  a = NULL
  for(i in altm.df){a = rbind(a, i[[1]])}
  a = as.data.frame(a)


# count, mean, rank, "pct", higher than; all, journal, similar age
# cited by posts, tweeters, accounts, "score", "history"
# readers count
# type
  df2 = df
  df = df[df$arxivID %in% a$arxivID,]
  
  df = cbind(df, a[cols])
  rm(cols, a)
}


#orcid
{
  orcid.df = list()
  for(i in 1:nrow(df)){
    auth = strsplit(strsplit(df$Autores[i], "\\|")[[1]], " ")
    or.row = list()
    
    for(j in auth){
      id = orcid_search(given_name=j[1], family_name=j[length(j)])$orcid
      a = orcid_educations(orcid=id)[1][[1]]$`affiliation-group`$summaries
      
      b = character()
      for(l in 1:length(a)){
        try({b[l] = a[[l]]$`education-summary.role-title`})}
      
      or.row[j[1]] = list(b)}
    
    orcid.df[df$arxivID[i]] = list(or.row)}
  
  a = numeric()
  for(i in orcid.df){
    b = sapply(i, length)
    a = rbind(a, c(mean(b), sum(b>0)/length(b), sum(grepl("PhD",i))/length(b)))}
  
  colnames(a) = c("EducMean", "EducLogi", "EducPhd")
  
  df = cbind(df,a)
  
  rm(or.row,a)
  }


#scholar (não usado, precisa de acesso especial)
{
  scholar.df = NULL
  missing = numeric(nrow(df))
  for(i in 67:nrow(df)){
    auth = strsplit(strsplit(df$Autores[i], "\\|")[[1]], " ")
    sch.row = list(NULL, NULL, NULL, NULL)
    
    missing = NA
    for(j in auth){
      link = paste0("https://scholar.google.fr/scholar?hl=fr&as_sdt=0%2C5&q=",
                    j[1], "+", j[length(j)],"&btnG=")
      
      page = read_html(link)
      text = html_nodes(page, ".gs_rt2")
      id = sub(".*user=([0-9A-Za-z_-]+)&.*", "\\1", text)
      
      
      if(length(id)>0){
        sch.row = list(
          c(sch.row[[1]], get_num_articles(id)),
          c(sch.row[[1]], get_num_distinct_journals(id)),
          c(sch.row[[1]], get_num_top_journals(id)),
          c(sch.row[[1]], predict_h_index(id)[1,2]))}
      
      else{sch.row = list(
        c(sch.row[[1]], 0),
        c(sch.row[[1]], 0),
        c(sch.row[[1]], 0),
        c(sch.row[[1]], 0))
      
      missing = 1}}
    
    
    scholar.df = rbind(scholar.df, c(df$arxivID[i], sch.row, missing))}
  
  a = NULL
  for(i in 1:nrow(scholar.df)){
    a = rbind(a, c(scholar.df[i,1],
                   scholar.df[i,6],
                   sapply(scholar.df[i,2:5], mean),
                   sapply(scholar.df[i,2:5], max)))}
  
  
  
  colnames(a) = c("arxivID", "SchoMiss",
                  paste0(c("Nart", "Njourn", "NtopJourn", "hindex"), "Mean"),
                  paste0(c("Nart", "Njourn", "NtopJourn", "hindex"), "Max"))
  
  
  scholar.df = as.data.frame(a)
  scholar.df$SchoMiss[is.na(scholar.df$SchoMiss)] = 0
  
  df = cbind(df, scholar.df[,-1])

#journal impact factor, rank
#num articles, journals, top journals, rank,
#get_citation_history(id)
#predict_h_index
}


#=============
#função para fazer modelo variando base, tipo de variável de genero, e y
mods = function(df, gen, mist, formula){
  df3 = df
  df3[,gen] = NULL
  df3 = na.omit(df3)
  df3 = df3[,-c(1:3)]
  df3$arxivID = NULL
  df3$arxivID.1 = NULL
  df3[,-4] = sapply(df3[,-4], as.numeric)
  df3$Tema1 = df3$Tema
  df3$Tema = NULL
  df3$context.similar_age_3m.mean = NULL
  df3$context.similar_age_journal_3m.mean = NULL
  df3$cited_by_accounts_count = NULL
  df3$cited_by_posts_count = NULL
  
  if(mist == FALSE){
    gen2 = grep("Genero", colnames(df3))
    Genero = df3[,gen2] == 1
    df3 = cbind(df3[,1:4], Genero, df3[,5:ncol(df3)])
    df3[,gen2] = NULL}
  
  #return(df3) #para fazer os gráficos
  return(lm(formula, df3))
  }


gen = c("GeneroB", "GeneroC")
mist = c(TRUE, FALSE)
form = c("CiteSem ~ . - CiteInfSem")

reslt = list()
for(g in gen){
  for(m in mist){
    for(f in form){
      reslt[[paste0(g,m,substr(f,1,6))]] = mods(df, g, m, f)}}}

stargazer(reslt[1:4], single.row=TRUE, no.space=TRUE, model.numbers=FALSE, omit.table.layout="n",
          omit="Tema1", df=FALSE, digits=2, omit.stat=c("adj.rsq","ser"), type="text")

lmtest::bptest(reslt[3])#teste de bp
coeftest(reslt[3], sandwich::vcovHC(reslt[3]))#correção de white

#distribuição de tema
df3$Tema1 = gsub("(.+)\\..+", "\\1", df3$Tema1)
mn = numeric()
for(i in unique(df3$Tema1)){
  mn[i] = nrow(df3[df3$Tema1==i&df3$Genero==TRUE,])/nrow(df3[df3$Tema1==i&df3$Genero==FALSE,])}

df3$Tema1 = factor(df3$Tema1, levels=names(mn)[order(mn)])

df3$Genero = ifelse(df3$Genero, "Mulher", "Homem")

ggplot(df3[-which(df3$Tema1=="quant-ph"),], aes(x=Tema1, fill=Genero)) +
  geom_bar(position="fill", stat="count") +
  labs(fill="Gênero", x="Tema", y="Proporçãp", title="Proporção de temas por gênero") +
  scale_y_continuous(labels = scales::percent) + theme_light() +
  theme(axis.text.x=element_text(angle=45, vjust=0.8)) +
  scale_fill_brewer(palette="Set2")


#distribuição dos resíduos
g = data.frame(resi = reslt[[3]]$residuals,
          fit = reslt[[3]]$fitted.values)

ggplot(g, aes(y=resi, x=fit)) +
  geom_point()

ggplot(g, aes(x=resi)) +
  geom_histogram()


#tabela de médias
means = NULL
for(i in colnames(df3)[-c(4,15)]){
  a = summary(lm(df3[,i] ~ df3$Genero - 1))
  means = rbind(means, c(i,
                         round(a$coefficients[1,1],2),
                         round(a$coefficients[1,1]+a$coefficients[2,1],2),
                         formatC(a$coefficients[2,4], format="e", digits=1)))}

means = na.omit(means)
colnames(means) = c("Variável", "Homens", "Mulheres", "P-valor")
stargazer(means)


#distribuição com gênero% (não usado)
g = df3[,c(4,7,9,12,13)]
g = pivot_longer(g, -1)

ggplot(g, aes(x=GeneroB, y=value)) + 
  geom_point() +
  facet_wrap(~name, scales="free_y") + 
  geom_smooth(method="lm", se = FALSE)

# Distribuição de citação por genero
df3$Genero = ifelse(df3$Genero, "Mulher", "Homem")
g = df3[,c("Genero", "CiteSem")]

ggplot(g, aes(x=CiteSem, fill=Genero)) +
  stat_density() + xlim(0,30) + 
  facet_wrap(~Genero) + theme_light() +
  labs(fill="Gênero", x="Citações", y="Densidade", title="Distribuições de citação por gênero") +
  theme(legend.position="none") + 
  scale_fill_brewer(palette="Set2")
