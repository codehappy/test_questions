import org.scalatest.FunSuite

/**
  */
class QuestionsNoLoopTest extends FunSuite {

  val questionsNoLoop = QuestionsLoop()
  val testQuestons: Seq[(String, Seq[(String, String)])] = questionsNoLoop.expand(Seq("User" -> Seq("name" -> "oldpig", "gender" -> "male", "mixin" -> "Address"),
    "Address" -> Seq("Province" -> "Shanghai", "District" -> "Minhang")))
  println(testQuestons)
  assert(testQuestons == Seq("User" -> Seq("name" -> "oldpig", "gender" -> "male", "Province" -> "Shanghai", "District" -> "Minhang"),
    "Address" -> Seq("Province" -> "Shanghai", "District" -> "Minhang")))

  val testQuestonsLoop: Seq[(String, Seq[(String, String)])] = questionsNoLoop.expandLoop(Seq("User" -> Seq("name" -> "oldpig", "gender" -> "male", "mixin" -> "Address"),
    "Address" -> Seq("Province" -> "Shanghai", "District" -> "Minhang")))
  println(testQuestonsLoop)
  assert(testQuestonsLoop == Seq("User" -> Seq("name" -> "oldpig", "gender" -> "male", "Province" -> "Shanghai", "District" -> "Minhang"),
    "Address" -> Seq("Province" -> "Shanghai", "District" -> "Minhang")))

  val testQuestonsLoop1: Seq[(String, Seq[(String, String)])] = questionsNoLoop.expandLoop(Seq(
    "User" -> Seq("name" -> "oldpig", "gender" -> "male", "mixin" -> "Address"),
    "Address" -> Seq("Province" -> "Shanghai", "District" -> "Minhang", "mixin" -> "Address1"),
    "Address1" -> Seq("Province1" -> "Shanghai1", "District1" -> "Minhang1","mixin" -> "Address")
  ))
  println(testQuestonsLoop1)


}
