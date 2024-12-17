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
#' @details
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
#'
#' @source Data originally published by \insertCite{fischer-1994;textual}{JaB} and
#' electronically published on GitHub by \insertCite{healy-2017;textual}{JaB}.
#' Also found in \insertCite{han-2009;textual}{JaB}.
#'
#' @examples
#' data("paul.revere")
#' set.seed(400)
#' plot(paul.revere, vertex.label = NA)
#'
#'
#' @references
#'
#' \insertAllCited{}
#'
NULL


#' Paul Revere Groups Network
#'
#' Association network of groups involved in the \link[paul.revere]{paul.revere}
#' network comprised of leaders from the American Revolution in the Boston area.
#'
#' @details
#'
#' This network includes the seven groups that the 254 men of the \link[paul.revere]{paul.revere}
#' network were involved in:  the Masonic
#' lodge that met at the Green Dragon Tavern; the Loyal Nine, which was the
#' nucleus of the Sons of Liberty; the North Caucus that met at the Salutation Tavern; the
#' Long Room Club in Dassett Alley; the Boston Committee of Correspondence; the men who
#' are known to have participated in the Boston Tea Party; and Whig leaders on a Tory
#' Enemies List.
#'
#' Nodes in this network are the groups. An edge exists between two groups if
#' at least one person was a member of both groups, and the edge weight is the number of people
#' who were members of both groups (e.g. Thomas Crafts and Henry Welles were both
#' members of St. Andrews Lodge and the Loyal Nine, so there is an edge of weight two
#' between St. Andrews Lodge and the Loyal Nine. There were no people in both the
#' Loyal Nine and the Long Room Club so there is not edge between them).
#'
#'
#' @name paul.revere.groups
#' @docType data
#' @usage
#' paul.revere.groups
#' @format
#' An undirected `igraph` graph object.
#'
#' Vertex Attributes:
#'   \itemize{
#'    \item{"name": }{Name of the group}
#'    \item{"members": }{list of people in \link[paul.revere]{paul.revere} who were known members of the group}
#'  }
#'
#' Edge Attributes:
#'   \itemize{
#'    \item{"weight": }{edge weight, people who were members of both groups}
#'    \item{"shared.members": }{vector of length `weight` stating which people were members of both groups}
#'  }
#' @source Data originally published by \insertCite{fischer-1994;textual}{JaB} and
#' electronically published on GitHub by \insertCite{healy-2017;textual}{JaB}.
#' Also found in \insertCite{han-2009;textual}{JaB}.
#'
#' @examples
#' data("paul.revere.groups")
#' set.seed(600)
#' plot(paul.revere.groups,
#'      edge.width = igraph::E(paul.revere.groups)$weight)
#'
#' @references
#'
#' \insertAllCited{}
NULL

#' Pester Hamster Network
#'
#' Friendship network among users of hamsterster.com from \insertCite{rossi-ahmed-2015;textual}{JaB}
#'
#' @name hamster
#' @docType data
#' @usage
#' hamster
#' @format
#' An undirected `igraph` graph object. Nodes are individual accounts from hamsterster.com
#' and an edge is present between two nodes if the respective accounts are friends.
#'
#' @source \url{https://networkrepository.com/petster-hamster.php}
#' @references
#'
#' \insertAllCited{}
#'
#' @examples
#' data("hamster")
#' set.seed(600)
#' plot(hamster, vertex.label = NA, vertex.size = 3)
#'
NULL

#' Trump World Network
#'
#' Association network of Donald Trump and associates pre-2020 inauguration
#' compiled by Buzz Feed News \insertCite{trump-world-buzzfeed}{JaB}.
#'
#' @details
#' In January 2017 just before Donald Trump's inauguration, Buzz Feed News published
#' and article asking to "Help Us Map TrumpWorld". They documented over 2,000 people
#' and organizations connected to the incoming Trump administration. They released a
#' public call asking for analysis of this data to help learn more about Trump's business
#' connections and how they may impact his administration.
#'
#' Nodes in this network are entities such as people, organizations, or federal agencies.
#' And edge is present between two nodes if there is a documented connection between
#' these two entities (e.g. ownership, committee member, familial relationship,
#' employee, business relationship, etc. )
#'
#'
#' @name trump.world
#' @docType data
#' @usage
#' trump.world
#' @format
#' An undirected `igraph` graph object.
#'
#' Vertex Attributes:
#'   \itemize{
#'    \item{"name": }{Names of entity.}
#'    \item{"EntityType": }{Type of entity.
#'      \itemize{
#'      \item{Person: }{Entity is a person.}
#'      \item{Federal Agency: }{Entity is a federal agency.}
#'      \item{Organization: }{Entity is a business (CORP, LLC, INC, LP, etc.), political organization (advocacy, PAC), religious organization (church, etc.)}
#'    }
#'  }
#' }
#'
#' Edge Attributes:
#'   \itemize{
#'    \item{"Connection": }{relationship type between the two entities}
#'    \item{"Source": }{url to source of connection.}
#'  }
#'
#'
#' @source Data originally published by Buzz Feed News at
#' \url{https://github.com/BuzzFeedNews/trumpworld} and
#' \url{https://docs.google.com/spreadsheets/d/1Z5Vo5pbvxKJ5XpfALZXvCzW26Cl4we3OaN73K9Ae5Ss/edit?gid=1996904412#gid=1996904412}.
#'
#' @examples
#' data("trump.world")
#' set.seed(39082)
#' plot(trump.world, vertex.label = NA)
#'
#'
#' @references
#'
#' \insertAllCited{}
#'
NULL
