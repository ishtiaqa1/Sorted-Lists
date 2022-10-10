package cse250.pa2
/**
 * cse250.pa2.SortedList
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

import scala.collection.mutable

/**
 * A linked list that stores its elements in sorted order.  
 * 
 * When an element is inserted into the list (or updated), it is positioned 
 * such that <tt>node.next</tt> is the next greater value being stored in
 * the list and <tt>node.prev</tt> is the next lesser value being stored in
 * the list.
 * 
 * SortedList "hinted" variants of several methods, where the caller may
 * provide a reference to a value that is close to the search term in the
 * sorted list.  If this is actually the case, the runtime of these methods,
 * which is normally linear <i>in the size of the list</i> will drop to linear
 * in the number of records between the hint and the search term.
 */
class SortedList[T: Ordering] extends mutable.Seq[T]
{
  var headNode: Option[SortedListNode[T]] = None
  var lastNode: Option[SortedListNode[T]] = None
  var length = 0

  /**
   * Compare two values of the sequence type  
   * @param      a         The first element to compare
   * @param      b         The second element to compare
   * @return               <tt>0</tt> if a = b, a negative integer if a < b, 
   *                       a positive integer if a > b 
   * 
   * This function is parameterized by a user-provided Ordering[T] 
   * implementation.  Complexity requirements listed below assume that
   * this function runs in O(1).
   */
  private def compare(a: T, b: T): Int = 
    Ordering[T].compare(a, b)

  /**
   * Find a reference to the element or the element that would precede it
   * @param      elem      The element to find
   * @param      hint      An element "close" to the element to search for.
   * @return               The first node containing the greatest element 
   *                       equal to or below elem.
   * 
   * If the list contains elem, this function should return the first occurrence
   * of it in the list.
   * 
   * If the list does not contain elem, this function should return a reference
   * to the element that would precede it if it were in the list, or None if 
   * elem is lower than the lowest element in the list.
   * 
   * If hint is at position i and elem is at position j, then this function
   * should run in O( |i-j| )
   */
  def findRefBefore(elem: T, hint: SortedListNode[T]): Option[SortedListNode[T]] = 
  {
    ???
  }

  /**
   * Find a reference to the element or the element that would precede it
   * @param      elem      The element to find
   * @return               The first node containing the greatest element 
   *                       equal to or below elem.
   * 
   * If the list contains elem, this function should return the first occurrence
   * of it in the list.
   * 
   * If the list does not contain elem, this function should return a reference
   * to the element that would precede it if it were in the list, or None if 
   * elem is lower than the lowest element in the list.
   * 
   * This function should run in O(length)
   */
  def findRefBefore(elem: T): Option[SortedListNode[T]] = 
  {
    ???
  }

  /**
   * Insert a new value into the list.  
   * @param      elem      The value to insert
   * @param      hint      An element "close" to the position where the 
   *                       element is to be inserted.
   * @return               A reference to the inserted value
   * 
   * The value should be placed so that the list remains in sorted order.
   * After the insertion, the inserted element's <tt>next</tt> method
   * should return a reference to the next greatest element, and the 
   * <tt>prev</tt> method should return a reference to the next least 
   * element.  
   * 
   * If elem is already in the list, the new node may be created before
   * or after the existing value(s).
   * 
   * If hint is at position i and elem should be inserted at position j, 
   * then this function should run in O( |i-j| )
   */
  def insert(elem: T, hint: SortedListNode[T]): SortedListNode[T] =
  {
    ???
  }

  /**
   * Insert a new value into the list.  
   * @param      elem      The value to insert
   * @return               A reference to the inserted value
   * 
   * The value should be placed so that the list remains in sorted order.
   * After the insertion, the inserted element's <tt>next</tt> method
   * should return a reference to the next greatest element, and the 
   * <tt>prev</tt> method should return a reference to the next least 
   * element.  
   * 
   * If elem is already in the list, the new node may be created before
   * or after the existing value(s).
   * 
   * This function should run in O(length)
   */
  def insert(elem: T): SortedListNode[T] =
  {
    ???
  }

  /**
   * Find a reference to the specified element
   * @param      elem      The element to find
   * @return               Some(node) of the node containing elem, or None
   *                       if elem is not present in the list
   * 
   * This function should run in O(length)
   */
  def findRef(elem: T): Option[SortedListNode[T]] = 
  {
    ???
  }

  /**
   * Return a reference to the element at the specified index
   * @param      idx       The index to look up
   * @return               The node <b>currently</b> at the specified index
   *
   * @throw IndexOutOfBoundsException if idx < 0 or idx >= length
   * 
   * If the list changes, references to nodes who's values are unchanged
   * should remain valid, even if their index changes.
   *
   * This function should run in O(idx)
   */
  def getRef(idx: Int): SortedListNode[T] =
  {
    ???
  }

  /**
   * Return the value at the specified index
   * @param      idx       The index to look up
   * @return               The value currently at the specified index
   * 
   * @throw IndexOutOfBoundsException if idx < 0 or idx >= length
   * 
   * This function should run in O(idx)
   */
  def apply(idx: Int): T = 
  {
    ???
  }

  /**
   * Remove the referenced node from the list
   * @param      ref       The node to remove (must be part of the list)
   * @return               The value of the removed node
   * 
   * This function should run in O(1)
   */
  def remove(ref: SortedListNode[T]): T =
  {
    ???
  }

  /**
   * Modify a value presently in the list
   * @param      ref       A reference to the node to be updated
   * @param      elem      The value to update the node
   * @return               A reference to the updated node
   * 
   * 
   * If i is the position of ref before the update and j is the position
   * of ref after the update, then this function should run in O( |i-j| ) 
   */
  def update(ref: SortedListNode[T], elem: T): SortedListNode[T] = 
  {
    val ret = insert(elem, ref)
    remove(ref)
    return ret
  }

  /**
   * Modify a value presently in the list
   * @param      idx       The index of the value 
   * 
   * This function should run in O(length)
   */
  def update(idx: Int, elem: T): Unit = 
  {
    update(getRef(idx), elem)
  }

  /**
   * Return an iterator over the elements of the collection.
   * @return               An iterator over the elements of the collection
   *
   * The iterator should return elements in sorted order from least to 
   * greatest (according to the [[compare]] method in this class).
   * 
   * The iterator's <tt>next</tt> and <tt>hasNext<tt> methods should both
   * run in O(1).
   */
  def iterator: Iterator[T] = 
  {
    return new Iterator[T] {
      var current = headNode

      def hasNext: Boolean = current.isDefined
      def next(): T = 
      {
        val ret = current.get
        current = ret.next
        return ret.value
      }
    }
  }

  /**
   * Return the last element of the list
   * @return               The last element of the list
   * 
   * This function should run in O(1)
   */
  override def last = lastNode.get.value
  
}