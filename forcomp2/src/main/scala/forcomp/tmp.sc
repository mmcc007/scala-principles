package forcomp
import Anagrams._
object tmp {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  wordOccurrences("Robert")                       //> res0: forcomp.Anagrams.Occurrences = List((b,1), (e,1), (o,1), (r,2), (t,1))
                                                  //| 
  sentenceOccurrences(List("scala", "is", "fun")) //> res1: forcomp.Anagrams.Occurrences = List((a,2), (c,1), (f,1), (i,1), (l,1),
                                                  //|  (n,1), (s,2), (u,1))


}