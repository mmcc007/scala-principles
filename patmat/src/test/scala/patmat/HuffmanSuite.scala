package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times(\"hello, world\")") {
    assert(times(List('a', 'b', 'a')) === List(('a', 2), ('b', 1)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("makeOrderedLeafList for another frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 3), ('x', 1))) === List(Leaf('x', 1), Leaf('t', 2), Leaf('e', 3)))
  }

  test("makeOrderedLeafList for another2 frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('y', 4), ('e', 3), ('x', 1))) === List(Leaf('x', 1), Leaf('t', 2), Leaf('e', 3), Leaf('y', 4)))
  }

  test("combine of some leaf list2") {
    val leaflist = List(Leaf('t', 3), Leaf('e', 4), Leaf('x', 5))
    assert(combine(leaflist) === List(Leaf('x', 5), Fork(Leaf('t', 3), Leaf('e', 4), List('t', 'e'), 7)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("create code tree from some ordered leaf list") {
    val orderedleaflist = List(
      Leaf('1', 5),
      Leaf('2', 7),
      Leaf('3', 10),
      Leaf('4', 15),
      Leaf('5', 20),
      Leaf('6', 45))
    assert(until(singleton, combine)(orderedleaflist).head ===
      Fork(
        Leaf('6', 45),
        Fork(
          Fork(
            Leaf('3', 10),
            Fork(Leaf('1', 5),
              Leaf('2', 7),
              List('1', '2'),
              12),
            List('3', '1', '2'),
            22),
          Fork(Leaf('4', 15),
            Leaf('5', 20),
            List('4', '5'),
            35),
          List('3', '1', '2', '4', '5'),
          57),
        List('6', '3', '1', '2', '4', '5'),
        102))
  }

  test("testing encode function with two item list") {
    val t1 = List('a', 'a', 'b')
    val t1Tree = createCodeTree(t1)
    val toEncode = List('a', 'a', 'b')
    assert(encode(t1Tree)(toEncode) === List[Bit](1, 1, 0))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("testing quickEncode function with two item list") {
    val t1 = List('a', 'a', 'b')
    val t1Tree = createCodeTree(t1)
    val toEncode = List('a', 'a', 'b')
    assert(quickEncode(t1Tree)(toEncode) === List[Bit](1, 1, 0))
  }

  test("decode and quickEncode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decoding the secret") {
    assert(decode(frenchCode, secret) === List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l'))
  }

  test("decode and quickEncode some bits with frenchCode") {
    val encodeStr = "encoreuntextetressecret"
    assert(decode(frenchCode, quickEncode(frenchCode)(encodeStr.toList)) === encodeStr.toList)
  }

  test("decode and encode a very short text with a larger tree") {
    val encodeStr = "abu"
    assert(decode(frenchCode, quickEncode(frenchCode)(encodeStr.toList)) === encodeStr.toList)
  }

  test("decode and encode some longer text should be identity") {
    val text = "literature from 45 BC, making it over 2000 years old.".toList
    val tree = createCodeTree(text)
    assert(decode(tree, encode(tree)(text)) === text)
  }

  test("decode and quickEncode some longer text should be identity") {
    val text = "literature from 45 BC, making it over 2000 years old. Richard Mc".toList
    val tree = createCodeTree(text)
    assert(decode(tree, quickEncode(tree)(text)) === text)
  }

}
