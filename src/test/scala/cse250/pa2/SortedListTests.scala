package cse250.pa2
/**
 * cse250.pa2.SortedListTests
 *
 * Copyright 2022 Oliver Kennedy (okennedy@buffalo.edu)
 *           2022 Eric Mikida (epmikida@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT:
 * Person#:
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */

import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

class SortedListTests extends AnyFlatSpec {

  /**
   * A "seeded" random instance that ensures that tests are 
   * deterministic.  The numbers that come out will be pseudorandom;
   * they'll be "random-like", but it'll be the same random sequence
   * each time, unless you change the seed value (The integer 250 
   * in our case).
   */
  val SeededRandom = new Random(250)


  behavior of "SortedList"
  it should "insert reverse-order elements in order" in {
    val list = new SortedList[Int]()

    // Inserting in reverse order should always insert at the head

    for (i <- Seq.range(start = 9, end = -1, step = -1)) {
      list.insert(i)
    }

    // The for loop below invokes list via its 'iterator' method.
    // .zipWithIndex specifically wraps the list so that you get
    // back a 2-tuple (element, index) when the iterator is 
    // constructed.  Since we've inserted every element in the
    // range (-1, 9] == [0, 9] == [0, 10), every element should
    // be located at its index. 

    for ((elem, index) <- list.zipWithIndex) {
      assert(elem == index)
    }
  }

  it should "insert in-order elements in order" in {
    val list = new SortedList[Int]()

    // Inserting in order should always insert at the tail

    for (i <- 0 until 10) {
      list.insert(i)
    }

    // The for loop below invokes list via its 'iterator' method.
    // .zipWithIndex specifically wraps the list so that you get
    // back a 2-tuple (element, index) when the iterator is 
    // constructed.  Since we've inserted every element in the
    // range [0, 10), every element should be located at its index.

    for ((elem, index) <- list.zipWithIndex) {
      assert(elem == index)
    }
  }

  it should "insert random-order elements in order" in {
    val list = new SortedList[Int]()

    // Inserting in random order should test all of the other cases of insert

    for (i <- SeededRandom.shuffle(IndexedSeq.range(start = 0, end = 30))) {
      list.insert(i)
    }

    // The for loop below invokes list via its 'iterator' method.
    // .zipWithIndex specifically wraps the list so that you get
    // back a 2-tuple (element, index) when the iterator is 
    // constructed.  Since we've inserted every element in the
    // range [0, 30), every element should be located at its index.

    for ((elem, index) <- list.zipWithIndex) {
      assert(elem == index)
    }

    // Pick 10 distinct random indexes in the range [0, 30) and test
    // the apply method by checking to see if the element at that index
    // is equal to the index (following the same logic as the last test).

    for (index <- SeededRandom.shuffle(IndexedSeq.range(start = 0, end = 30)).take(10)) {
      assert(list(index) == index)
    }
  }

  it should "insert in-order elements with hints" in {
    val list = new SortedList[Int]()

    // The hinted version of the for loop should be quite a bit faster
    // for in-order insertions if we pass in a "hint" pointing at the
    // tail of the list (the last element we inserted).  If this test case
    // is a bit on the slow side, then you may have an O(n) implementation
    // of the hinted insert.

    var tail = list.insert(0)
    for (i <- 1 until 100) {
      tail = list.insert(i, tail)
    }

    // Test using the same logic as above

    for ((elem, index) <- list.zipWithIndex) {
      assert(elem == index)
    }
  }

  it should "efficiently update elements" in {
    val list = new SortedList[Int]()

    // The hinted version of the for loop should be quite a bit faster
    // for in-order insertions if we pass in a "hint" pointing at the
    // tail of the list (the last element we inserted).  If this test case
    // is a bit on the slow side, then you may have an O(n) implementation
    // of the hinted insert.

    var first = list.insert(0)
    var tail = first
    for (i <- 1 until 2000) {
      tail = list.insert(i * 2, tail)
    }

    // Start with the head element of the list and move it forward 1000 spaces
    // If test case is a bit on the slow side, then you may have an O(n) 
    // implementation of either the hinted remove() or the hinted insert.

    for (i <- 0 until 1000) {
      first = list.update(first, i * 2 + 1)
    }

    // Check to see if all of the elements are present and in-order
    for ((elem, index) <- list.zipWithIndex) {
      if (index < 999) {
        assert(elem == ((index + 1) * 2))
      } else if (index == 999) {
        assert(elem == 999 * 2 + 1)
      } else {
        assert(elem == index * 2)
      }
    }
  }

  it should "be a linked list" in {
    val list = new SortedList[Int]()

    var tail = list.insert(0)
    for (i <- 1 until 10) {
      list.insert(i, tail)
    }

    var current = list.headNode
    assert(current.isDefined)
    for (i <- 0 until 10) {
      assert(current.isDefined)
      assert(current.get.value == i)
      current = current.get.next
    }
  }

  it should "have next and previous correctly" in {
    val list = new SortedList[Int]()

    val tail = list.insert(0)
    for (i <- 1 until 10) {
      list.insert(i, tail)
    }

    var current = list.headNode
    for (i <- 0 until 10) {
      if (i == 0) {
        assert(current.get.next.get.prev.get.value == current.get.value)
      } else if (i >= 9) {
        assert(current.get.prev.get.next.get.value == current.get.value)
      } else {
        assert(current.get.next.get.prev.get.value == current.get.value)
        assert(current.get.prev.get.next.get.value == current.get.value)
      }
      current = current.get.next
    }
  }

  it should "update" in {
    val list = new SortedList[Int]()

    val tail = list.insert(0)
    for (i <- 1 until 10) {
      list.insert(i, tail)
      if (i != 1) {
        val tails = list.lastNode.get.value
        assert(list.lastNode.get.value != tails)
      }
    }

    val list2 = new SortedList[Int]()
    val listed = List(9,8,7,6,5,4,3,2,1)
    for (i <- listed) {
      list2.insert(i, tail)
      if (i != 9) {
        val heads = list2.headNode.get.value
        assert(list2.headNode.get.value != heads)
      }
    }
  }
}
