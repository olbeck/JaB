#' Zachary's karate club network
#'
#' @name karate
#'
#' @inherit igraphdata::karate
#'
#' @source This data set and documentation is directly from the \link[igraphdata]{igraphdata} package.
#'
#' @references
#'
#' \insertRef{igraphdata}{JaB}
#'
#' \insertRef{zachary-1977}{JaB}
#'
NULL

#' Paul Revere Network
#'
#' Association network of Paul Revere and other leaders from the American Revolution in the Boston area.
#'
#' This network includes a total of 254 men involved in Boston's revolutionary movement.
#' Each man was a member of one (or several) of the following seven groups: the Masonic
#' lodge that met at the Green Dragon Tavern; the Loyal Nine, which was the
#' nucleus of the Sons of Liberty; the North Caucus that met at the Salutation Tavern; the
#' Long Room Club in Dassett Alley; the Boston Committee of Correspondence; the men who
#' are known to have participated in the Boston Tea Party; and Whig leaders on a Tory
#' Enemies List.
#'
#' Nodes in this network are the men. An edge is present between two men if they
#' were both members of the same group. The edge weight is the number of groups they
#' were both members of. (e.g. John Adams and Dr. Allen were both part of the North Caucus so
#' there is an edge between them with weight one. John Bradford and William Powell were both members
#' of the Boston Committee and the London Enemies so there is an edge between them with weight two.)
#'
#'
#' @name paul.revere
#' @docType data
#' @usage
#' paul.revere
#' @format
#' An undirected `igraph` graph object.
#'
#' Vertex Attributes:
#'   \itemize{
#'    \item{"name": }{Names of individuals as "Surname.Firstname"}
#'    \item{"StAndrewsLodge": }{Logical, was the individual a member of St Andrews Lodge?}
#'    \item{"LoyalNine": }{Logical, was the individual a member of the Loyal Nine?}
#'    \item{"NorthCaucus": }{Logical, was the individual a member of the the North End Caucus?}
#'    \item{"LongRoomClub": }{Logical, was the individual a member of the Long Room Club?}
#'    \item{"TeaParty": }{Logical, did the individual participate in the Boston Tea Party?}
#'    \item{"BostonCommittee": }{Logical, was the individual a member of the Boston Committee?}
#'    \item{"LondonEnemies": }{Logical, was the individual a Whig leader on the Tory enemies list?}
#'  }
#'
#'
#' Edge Attributes:
#'   \itemize{
#'    \item{"weight": }{edge weight, number of shared organizational memberships between the two individuals}
#'    \item{"membership": }{vector of length `weight` stating which organizations the two individuals were both members of.}
#'  }
#'
#' @description
#'
#' Association network of Paul Revere and other leaders from the American Revolution in the Boston area.
#'
#'
#' @source Data originally published by \insertCite{fischer-1994;textual}{JaB} and
#' electronically published on GitHub by \insertCite{healy-2017;textual}{JaB}.
#' Also found in \insertCite{han-2009;textual}{JaB}.
#'
#'
#' @references
#'
#' \insertAllCited{}
#'
NULL
