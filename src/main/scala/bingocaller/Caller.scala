package bingocaller

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Random

import sys.process._

object Caller {

  def play(numbers: Seq[Int], callOuts: Map[Int, String], voice: String): Unit = {
    val shuffledNumbers = shuffleNumbers(numbers)
    callTurn(shuffledNumbers, callOuts, Seq(), voice)
  }

  @tailrec
  def callTurn(numbers: Seq[Int], callOuts: Map[Int, String], calledNumbers: Seq[Int], voice: String): Unit = {
    println("\u001B[2J")
    if (numbers.isEmpty) {
      println("All numbers called! The game's over!")
    }
    else {
      val number = numbers.head
      val callOut = s"${callOuts(number)}, number $number!"
      println(s"\u001b[0;0H$callOut")

      getAsciiNumber(number).foreach(println)

      s"say -v $voice $callOut" !

      val updatedCalledNumbers = calledNumbers :+ number
      if (continueGame("Do you wish to continue")) {
        callTurn(numbers.tail, callOuts, updatedCalledNumbers, voice)
      }
      else {

        printCalledNumbers(updatedCalledNumbers)

        if(!continueGame("Bingo")){
          callTurn(numbers.tail, callOuts, updatedCalledNumbers, voice)
        }
        else{
          println("\u001B[2J")
          println("\u001b[0;0H")
          Source.fromResource("bingo.txt").getLines().foreach(println)
          s"say -v $voice BINGO!" !
        }

      }
    }
  }

  private def printCalledNumbers(calledNumbers: Seq[Int]): Unit = {
    println("\u001B[2J")
    val singleDigitNumbers = calledNumbers.filter(i => i < 10)
    val doubleDigitNumbers = calledNumbers.filter(i => i > 9).groupBy { i => i.toString.head }.values.toSeq.sortBy(_.head)
    println("\u001b[0;0HCalled Numbers:")
    println(singleDigitNumbers.sorted.mkString(", "))
    doubleDigitNumbers.foreach(line => println(line.sorted.mkString(", ")))
  }

  @tailrec
  private def continueGame(question: String): Boolean = {
    println(s"$question (y/n)?")
    Console.in.readLine().toLowerCase match {
      case "y" => true
      case "n" => false
      case _ => continueGame(question)
    }
  }

  private def getAsciiNumber(number: Int): Seq[String] = {
    val numberLines = number
    .toString
    .map(n => Source.fromResource(s"$n.txt").getLines().toSeq)
    .transpose

    val leftMax = numberLines.map(_.head).maxBy(_.length).length


    numberLines.map { (i: Seq[String]) =>
      i.map { s =>
        val l = if(leftMax - s.length > 0 ) leftMax - s.length else 0
        s + " "*l
      }
        .mkString(" ")
    }
  }

  def getCallOuts(name: String): Map[Int, String] = {
    Source
      .fromResource(name)
      .getLines()
      .zipWithIndex
      .map { case (callOut, i) => (i + 1, callOut) }
      .toMap
  }

  private def shuffleNumbers(numbers: Seq[Int]): Seq[Int] = {
    Random.shuffle(numbers)
  }


}
