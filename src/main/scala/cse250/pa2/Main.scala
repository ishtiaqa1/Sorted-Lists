package cse250.pa2
/**
 * cse250.pa2.Main
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

import scala.util.Random
import org.jline.terminal.TerminalBuilder
import org.jline.terminal.Terminal
import org.jline.utils.InfoCmp.Capability
import scala.collection.mutable.ArrayBuffer

object Main
{
  implicit val racerOrder = new Ordering[Racer]
  {
    def compare(x: Racer, y: Racer): Int = 
      Ordering[(Long, Int)].compare((-x.position, x.id), (-y.position, y.id))
  }

  def main(args: Array[String])
  {
    val term = TerminalBuilder.builder()
                              .system(true)
                              .build

    val NUM_RACERS = Math.min(term.getWidth(), 80)/3 - 5
    val FINISH_LINE = 30
    val NUM_LINES_SHOWN = 20
    val MAX_STEPS = 10000000
    val MAX_SPEED = 4

    assert(NUM_RACERS > 10, "Your terminal window is too narrow (or you're running in an unsupported dumb terminal)")

    // A lookup table giving O(1) access to racers by their position
    val racers = new Array[SortedListNode[Racer]](NUM_RACERS)

    // A lookup table giving O(k) access to the top-k positioned racers.
    val positions = new SortedList[Racer]()

    // Initialize a random set of racers
    for((glyph, idx) <- Random.shuffle(GLYPHS.toIndexedSeq)
                              .take(NUM_RACERS)
                              .zipWithIndex)
    {
      // Pick a random speed for the racer from 0 to 5 (inclusive), as well
      // as a random (but unique) glyph chosen by the shuffle above
      val racer = new Racer(idx, glyph, speed = Random.nextInt(MAX_SPEED))

      // Save a reference to the racer's position in the order.
      racers(idx) = positions.insert(racer)
    }

    var iter = 0
    while(positions.head.position < FINISH_LINE && iter < MAX_STEPS){
      // Start by clearing the screen
      term.puts(Capability.clear_screen)

      // Update all of the racers' positions
      for(i <- racers.indices)
      {
        val racer = racers(i)

        // Racers with the highest speed have a 1/3 chance of moving to the
        // next spot, while the others have a progressively lower chance
        val chanceToAdvance = (MAX_SPEED+3) - racer.value.speed
        val didAdvance = Random.nextInt(chanceToAdvance) == 0

        // Here's where the data structure kicks in: each racer is going
        // to move at most forward by a small number of spots... This is
        // O(1)-ish, even if the function could be O(n) in the worst 
        // case.
        if(didAdvance)
        {
          racers(i) = positions.update(racer, racer.value.nextPosition)
        }
      }

      // Now render the race-course
      val leadPosition = positions.head.position
      val racersInOrder = positions.iterator.buffered
      val positionsToFinishLine = FINISH_LINE - leadPosition + 1
      val lineToDrawFinishLineOn = 5 - positionsToFinishLine

      // The animation looks better if the lead racer is not all the 
      // way at the top.  Draw the first five lines as empty lines
      // (and we can use this to draw a finish line to).
      for(line <- 0 until 5)
      {
        // Figure out if we draw racetrack stripes on the side
        // Draw them every fifth line, streaming downwards with
        // each iteration
        val stripesOnSideOfRoad = 
          if( (line - iter) % 5 == 0 ){ "|" } else { ":" }

        // Each racer is 3 characters wide.  If we're drawing a finish line
        // then draw it with '=' characters.  Otherwise, just draw spaces.
        if(line == lineToDrawFinishLineOn){
          term.writer.write(
            stripesOnSideOfRoad + " " +
            ("==="*NUM_RACERS) + " " +
            stripesOnSideOfRoad + "\n"
          )
        } else {
          term.writer.write(
            stripesOnSideOfRoad + " " +
            ("   "*NUM_RACERS) + " " +
            stripesOnSideOfRoad + "\n"
          )
        }
      }
      for(line <- 5 until NUM_LINES_SHOWN)
      {
        var positionOnCurrentLine = leadPosition - (line - 5)

        // Pick out all of the racers on the current line
        // and sort them by their id (the column in which we)
        // will draw it
        // Get an iterator to the result, and make it a "buffered"
        // iterator so that we can "peek" at the next element that
        // next will return
        val racersOnCurrentLine = 
          {
            var buffer = ArrayBuffer[Racer]()
            while(
              racersInOrder.hasNext && 
              racersInOrder.head.position >= positionOnCurrentLine
            ){
              buffer.append(racersInOrder.next)
            }
            buffer.sortBy { _.id }.iterator.buffered
          }

        // Figure out if we draw racetrack stripes on the side
        // Draw them every fifth line, streaming downwards with
        // each iteration
        val stripesOnSideOfRoad = 
          if( (line - iter) % 5 == 0 ){ "|" } else { ":" }

        // Render one character per column.  The for/yield syntax is
        // a 'map' operation.  It will generate a new sequence with
        // one element for each element we iterate over (i.e., one
        // for each value of i between 0 and NUM_RACERS).
        val renderedLine: Seq[String] =
          for(i <- 0 until NUM_RACERS) 
          yield {
            // Check if we have more racers to display, and if so
            // check to see if we're in the right column to display
            // the next racer we need to show (remember, these are
            // stored sorted on their id)
            if(
              racersOnCurrentLine.hasNext
              && racersOnCurrentLine.head.id == i
            ){ 
              // If we're in the right column, display it
              racersOnCurrentLine.next.toString + " "
            } else { 
              // If we're not, put in a placeholder space
              "   " 
            }
          }

        term.writer.write(
          stripesOnSideOfRoad + " " +
          renderedLine.mkString + " " +
          stripesOnSideOfRoad + "\n"
        )

        // Draw the line
      }
      term.writer.write("-" * term.getWidth()+"\n")

      // Enable the following lines for debugging info
      // for(racer <- positions){
      //   term.writer.write(s"$racer:${racer.id} @ ${racer.position}\n")
      // }

      term.flush()

      // Sleep a little to keep the pace of the animation steady
      Thread.sleep(100)
      iter += 1
    }
    println(s"${positions.head} wins")

  }

  class Racer(
    val id: Int,
    val glyph: String,
    val speed: Int,
    val position: Long = 0 
  )
  {
    override def toString(): String = 
     // s"[$glyph $id] @ $position"
     // s"$glyph @ $position"
      glyph

    def nextPosition = 
      new Racer(
        id,
        glyph,
        speed,
        position + 1
      )
  }



  val GLYPHS = Array(
    "👮",
    "👷",
    "👸",
    "💃",
    "🚴",
    "🚶",
    "🤵",
    "🤹",
    "🤺",
    "🤶",
    "🥷",
    "🦸",
    "🦹",
    "🧌",
    "🧗",
    "🧙",
    "🧚",
    "🧛",
    "🧜",
    "🧝",
    "🧞",
    "🧟",
    "🐀",
    "🐂",
    "🐃",
    "🐄",
    "🐅",
    "🐆",
    "🐇",
    "🐈",
    "🐉",
    "🐊",
    "🐋",
    "🐌",
    "🐍",
    "🐎",
    "🐏",
    "🐐",
    "🐑",
    "🐒",
    "🐓",
    "🐔",
    "🐕",
    "🐖",
    "🐗",
    "🐘",
    "🐙",
    "🐛",
    "🐜",
    "🐝",
    "🐞",
    "🐟",
    "🐠",
    "🐢",
    "🐥",
    "🐦",
    "🐧",
    "🐨", /* why is it smiling?!? */
    "🐩",
    "🐪",
  )


}