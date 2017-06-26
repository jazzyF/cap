package com.fola

import org.scalatest.{FlatSpec, Matchers}

class CapFunctionsSpec extends FlatSpec with Matchers {

    val E: GNode = GNodeImpl("E", Nil)
    val F: GNode = GNodeImpl("F", Nil)
    val G: GNode = GNodeImpl("G", Nil)
    val H: GNode = GNodeImpl("H", Nil)
    val I: GNode = GNodeImpl("I", Nil)
    val J: GNode = GNodeImpl("J", Nil)

    val B: GNode = GNodeImpl("B", E :: F :: Nil)
    val C: GNode = GNodeImpl("C", G :: H :: I :: Nil)
    val D: GNode = GNodeImpl("D", J :: Nil)

    val A: GNode = GNodeImpl("A", B :: C :: D :: Nil)

    "The CapFunctions.walkGraph(A)" should "return all GNodes" in {
        val expectedResult = A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: Nil

        val actualResult = CapFunctions.walkGraph(A)

        expectedResult should contain allElementsOf actualResult

        actualResult should contain allElementsOf expectedResult
    }

    "The CapFunctions.walkGraph(B)" should "return B E F" in {
        val expectedResult = B :: E :: F :: Nil

        val actualResult = CapFunctions.walkGraph(B)

        expectedResult should contain allElementsOf actualResult

        actualResult should contain allElementsOf expectedResult
    }

    "The CapFunctions.walkGraph(D)" should "return D J" in {
        val expectedResult = D :: J :: Nil

        val actualResult = CapFunctions.walkGraph(D)

        expectedResult should contain allElementsOf actualResult

        actualResult should contain allElementsOf expectedResult
    }

    "The CapFunctions.walkGraph(C)" should "return C G H I" in {
        val expectedResult = C :: G :: H :: I :: Nil

        val actualResult = CapFunctions.walkGraph(C)

        expectedResult should contain allElementsOf actualResult

        actualResult should contain allElementsOf expectedResult
    }


    "The CapFunctions.walkGraph(G)" should "return G" in {
        val expectedResult = G :: Nil

        val actualResult = CapFunctions.walkGraph(G)

        expectedResult should contain allElementsOf actualResult

        actualResult should contain allElementsOf expectedResult
    }

    "The CapFunctions.paths(A)" should "return List(List(A, B, E), List(A, B, F), List(A, C, G), List(A, C, H), List(A, C, I), List(A, D, J))" in {
        val expectedResult = List(List(A, B, E), List(A, B, F), List(A, C, G), List(A, C, H), List(A, C, I), List(A, D, J))

        val actualResult = CapFunctions.paths(A)

        expectedResult should contain allElementsOf actualResult

        actualResult should contain allElementsOf expectedResult
    }

    "The CapFunctions.paths(B)" should "return List(List(B, E), List(B, F))" in {
        val expectedResult = List(List(B, E), List(B, F))

        val actualResult = CapFunctions.paths(B)

        expectedResult should contain allElementsOf actualResult

        actualResult should contain allElementsOf expectedResult
    }

    "The CapFunctions.paths(D)" should "return List(List(D, J))" in {
        val expectedResult = List(List(D, J))

        val actualResult = CapFunctions.paths(D)

        expectedResult should contain allElementsOf actualResult

        actualResult should contain allElementsOf expectedResult
    }

    "The CapFunctions.paths(J)" should "return List(List(J))" in {
        val expectedResult = List(List(J))

        val actualResult = CapFunctions.paths(J)

        expectedResult should contain allElementsOf actualResult

        actualResult should contain allElementsOf expectedResult
    }

}
