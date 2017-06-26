package com.fola

trait GNode {
    def getName: String

    def getChildren: List[GNode]
}

case class GNodeImpl(name: String, children: List[GNode]) extends GNode {
    override def getName: String = name

    override def getChildren: List[GNode] = children

    override def toString: String = name
}


object CapFunctions extends App {

    def walkGraph(node: GNode): List[GNode] = {
        @annotation.tailrec
        def go(children: List[GNode], acc: List[GNode]): List[GNode] = children match {
            case Nil => acc
            case x :: Nil => go(x.getChildren, x :: acc)
            case x :: xs => go(x.getChildren ++ xs, x :: acc)
        }

        go(node.getChildren, node :: Nil).reverse
    }

    def paths(node: GNode): List[List[GNode]] = {

        def getLengthOfListOfList(list: List[List[GNode]]): Int = {

            @annotation.tailrec
            def go(nodesListOfList: List[List[GNode]], acc: Int): Int = nodesListOfList match {
                case Nil => acc
                case x :: Nil => acc + x.length
                case x :: xs => go(xs, acc + x.length)
            }

            go(list, 0)
        }

        def expandNode(node: GNode): List[List[GNode]] = node.getChildren match {
            case Nil => List(node :: Nil)
            case x :: Nil => List(List(node, x))
            case xs => xs.map(y => List(node, y))
        }

        def expandList(list: List[GNode]): List[List[GNode]] = list.last.getChildren match {
            case Nil => List(list)
            case x :: Nil => List(list :+ x)
            case xs => xs.map(y => list :+ y)
        }

        def expandListOfList(list: List[List[GNode]], store: List[List[GNode]]): List[List[GNode]] = list match {
            case Nil => store
            case x :: Nil => expandList(x)
            case xs => xs.map(y => expandList(y)).reduceLeft(_ ++ _)
        }

        val acc: List[List[GNode]] = expandNode(node)

        @annotation.tailrec
        def go(list: List[List[GNode]], current: List[List[GNode]]): List[List[GNode]] = {
            val result = expandListOfList(list, current)
            if (getLengthOfListOfList(result) > getLengthOfListOfList(current)) {
                go(result, result)
            } else {
                result
            }
        }

        go(acc, acc)
    }

}
