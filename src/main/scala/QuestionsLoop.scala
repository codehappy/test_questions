import scala.collection.mutable.ListBuffer

/**
  * author: pengge
  * date: 2016/9/9 11:25
  * version: V1.0
  *
  * 定义数据类型
  * type Definitions = Seq[(String, Seq[(String, String)])]
  * 形如：
  * val defs: Definitions = Seq("User" -> Seq("name" -> "oldpig", "gender" -> "male", "mixin" -> "Address"),
  * "Address" -> Seq("Province" -> "Shanghai", "District" -> "Minhang"))
  * 其中 mixin 是一个特殊指令，以它为 key 的值对 "mixin" -> v 将被同一个 Definitions 中以 v 为 key 的值对 v -> (s: Seq) 中 s 的内容所替代。 例如上例中的定义，经过替代后的形式为：
  * def expand(d: Definitions): Definitions
  *
  * assert(expand(defs) == Seq("User" -> Seq("name" -> "oldpig", "gender" -> "male", "Province" -> "Shanghai", "District" -> "Minhang"),
  * "Address" -> Seq("Province" -> "Shanghai", "District" -> "Minhang"))
  *
  */
class QuestionsLoop {

  /**
    * 定义数据类型
    */
  type Definitions = Seq[(String, Seq[(String, String)])]

  /**
    * 循环判断mixin
    * 条件：
    * 可能存在嵌套mixin的情况，即 A mixin B, B mixin C, C mixin D
    * 可能存在多个mixin，同一个v也可能被mixin多次，即 A mixin B, A mixin C, B mixin C
    * 不存在循环mixin的情况，即 A mixin B, B mixin A, 或 A mixin B, B mixin C, C mixin A
    * 除了 mixin 外，不存在两个相同的 key
    *
    * @param keyDatas
    * @param datas
    * @return
    */
  private def replaceMinxinValue(keyDatas: (String, Seq[(String, String)]), datas: Definitions): (String, Seq[(String, String)]) = {
    val valueSeq: ListBuffer[(String, String)] = ListBuffer()
    keyDatas._2.foreach(keyData => {
      if ("mixin".eq(keyData._1)) {
        datas.foreach(data => {
          if (keyData._2.eq(data._1)) {
            valueSeq ++= replaceMinxinValue(data, datas)._2
          }
        })
      } else {
        valueSeq += (keyData)
      }
    })
    (keyDatas._1, valueSeq.toList)
  }

  /**
    * 在代码中探测循环mixin，发现则抛出异常
    * 在结果中保持输入数据次序
    * 存在 2 个或多个 key 相同的情况
    *
    * @param keyDatas
    * @param datas
    * @param index
    * @param loopSeq
    * @return
    */
  private def replaceMinxinValueLoop(keyDatas: (String, Seq[(String, String)]), datas: Definitions, index: Int, loopSeq: ListBuffer[String]): (String, Seq[(String, String)]) = {
    val valueSeq: ListBuffer[(String, String)] = ListBuffer()
    keyDatas._2.foreach(keyData => {
      if ("mixin".eq(keyData._1)) {
        val strSeqPre: String = index + "," + keyDatas._1
        if (loopSeq contains strSeqPre) {
          throw new Exception("探测循环mixin")
        }
        loopSeq += strSeqPre
        for (i <- 0 until datas.size) {
          if (keyData._2.eq(datas(i)._1)) {
            valueSeq ++= replaceMinxinValueLoop(datas(i), datas, i, loopSeq)._2
          }
        }
      } else {
        valueSeq += (keyData)
      }
    })
    (keyDatas._1, valueSeq.toList)
  }

  def expand(datas: Definitions): Definitions = for (data <- datas) yield replaceMinxinValue(data, datas)

  def expandLoop(datas: Definitions): Definitions = for (i <- 0 until datas.size)  yield replaceMinxinValueLoop(datas(i), datas, i, ListBuffer())

}

object QuestionsLoop {
  def apply(): QuestionsLoop = new QuestionsLoop
}

