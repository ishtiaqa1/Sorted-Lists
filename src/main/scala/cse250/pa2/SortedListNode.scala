package cse250.pa2
/**
 * cse250.pa2.SortedListNode
 *
 * Copyright 2022 Oliver Kennedy (okennedy@buffalo.edu)
 *           2022 Eric Mikida (epmikida@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 */

import scala.collection.mutable

class SortedListNode[T](
  val value: T, 
  var next: Option[SortedListNode[T]] = None,
  var prev: Option[SortedListNode[T]] = None,
)
