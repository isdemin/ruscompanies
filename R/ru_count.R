#' Считает число терминов по каждой из групп по трем аспектам
#'
#' Эта функция подсчитывает количество вхождений по каждой из тематических групп 
#' в разрезе аспектов деятельности компаний:
#' экологической,
#' социальной,
#' управленческой.
#'
#' @param text Лемматизированный текст в виде строки, леммы в которой разделены одиночными пробелами.
#' @return Список (list) количества терминов по отдельным темам в разрезе трех аспектов.
#' @export
ru_count <- function(text){
  dics = list(ru_environmental_lem, ru_social_lem, ru_governance_lem)
  ru_count_all=list()
  for (i in 1:length(dics)){
    dic=dics[[i]]
    j.count=rep(0,length(dic))
    for (j in 1:length(dic)) {
      print(mapply(ru_count_matches, dic[[j]], sample_text_lemmatized))
      j.count[[j]]=sum(mapply(count.matches, dic[[j]], sample_text_lemmatized))
    }
    names(j.count)=names(dic)
    ru_count_all[[i]]=j.count
  }
  names(ru_count_all)=c('Экологические аспекты','Социальные аспекты','Управленческие аспекты')
  ru_count_all
}