package threesbrain.game.core

import scala.util.Random
import Move._

object ThreesGame {
    def newGame() = {
        val freshStack = makeNewStack()
        val positions = Random.shuffle(List.range(0, 16)).take(9).map(i => (i % 4, i / 4))
        
        // add first 9 cards to board
        val startCards = positions.zip(freshStack.take(9))
        val startCells = startCards.foldLeft(List.fill(4)(List.fill(4)(0)))({ case (cells, ((posX, posY), card)) =>
            cells.updated(posX, cells(posX).updated(posY, card))
        })  
        
        new ThreesGame(startCells, freshStack(9), freshStack.drop(10))
    }
    
    private def makeNewStack() = {
        Random.shuffle(List.tabulate(12)(_ % 3 + 1))
    }
}

case class ThreesGame(cells: List[List[Int]], nextCard: Int, stack: List[Int]) {

  import ThreesGame._
    // returns a possible next nextCard and the corresponding future stack
    private def drawNewCard(cells: List[List[Int]]) = {
        val max = cells.flatten.max
        if (max >= 7 && Random.nextFloat() < 1/21.0f) // bonus card
            (Random.nextInt(max - 6) + 4, stack) // stack is not touched
        else // normal card
            (stack.head, if (stack.tail == Nil) makeNewStack() else stack.tail) // draw from stack
    }
    
    // make a move and place a new card
    def move(move: Move): ThreesGame = {
        def shiftLine(line: List[Int]): List[Int] = line match {
            case 0 :: cs => cs ::: List(0)
            case c1 :: c2 :: cs if (c1 + c2 == 3) => 3 :: cs ::: List(0)
            case c1 :: c2 :: cs if (c1 == c2 && c1 + c2 > 4) => c1 + 1 :: cs ::: List(0)
            case c1 :: c2 :: cs => c1 :: shiftLine(c2 :: cs)
            case _ => line
        }
        def moveUp(cells: List[List[Int]]) = {
            val shifted = cells.map(shiftLine)
            val changed = shifted.zip(cells).zipWithIndex.collect({case ((shifted, orig), i) if shifted != orig => i})
            if (changed == Nil) shifted
            else {
                val newCardLine = changed(Random.nextInt(changed.length))
                shifted.updated(newCardLine, shifted(newCardLine).dropRight(1) ::: List(nextCard))
            }
        }
        val newCells = move match {
            case Left => moveUp(cells.transpose).transpose
            case Right => moveUp(cells.transpose.map(_.reverse)).map(_.reverse).transpose
            case Up => moveUp(cells)
            case Down => moveUp(cells.map(_.reverse)).map(_.reverse)
        }
        if (newCells == cells) this
        else {
            val (newNextCard, newStack) = drawNewCard(newCells)
            ThreesGame(newCells, newNextCard, newStack)
        }
    }

    def score = cells.flatten.map(n =>
        if (n < 3) 0
        else math.pow(3, n-2).toInt
    ).sum
    
    def cardsInStack = (1 to 3).map(n => stack.count(_ == n)).toList

    def validMoves = Move.values.filter(m => move(m) != this)

    override def toString = {
        def cardName(n: Int) = n match {
            case 0 => "."
            case n if (n <= 3) => n
            case n => (3 * (1 << (n-3))).toString
        }
        cells.transpose.map(_.map(cardName).mkString("\t")).mkString("\n") +
            "\nNext card: " + cardName(nextCard) +
            "\nStack: " + List("ones", "twos", "threes").zip(cardsInStack).map(
                x => x._1 + "=" + x._2).mkString(", ")
    }
}

