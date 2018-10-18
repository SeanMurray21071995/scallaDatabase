import com.sun.net.httpserver.Authenticator
import slick.jdbc.MySQLProfile.api._

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}
object Main extends App {

  val db = Database.forConfig("mysqlDB") // The config string refers to mysqlDB that we defined in application.conf
  val peopleTable = TableQuery[People] // represents the actual table on which we will be building queries on

  val dropPeopleCmd = DBIO.seq(peopleTable.schema.drop) // schema definition to generate DROP statement for people table
  val initPeopleCmd = DBIO.seq(peopleTable.schema.create) // schema definition to generate a CREATE TABLE command

  def dropDB = {
    val dropFuture = Future {
      db.run(dropPeopleCmd)
    } //do a drop followed by initialisePeople
    Await.result(dropFuture, Duration.Inf).andThen { //Attempt to drop the table, Await does not block here
      case Success(_) => initialisePeople
      case Failure(error) =>
        println("Dropping the table failed due to: " + error.getMessage)
        initialisePeople
    }
  }

  def initialisePeople = { //initialise people
    val setupFuture = Future {
      db.run(initPeopleCmd)
    } //once our DB has finished initializing we are ready to roll, Await does not block
    Await.result(setupFuture, Duration.Inf).andThen {
      case Success(_) => runQuery
      case Failure(error) => println("Dropping the table failed due to: " + error.getMessage)
    }
  }

  def runQuery = {
    val insertPeople = Future {
      val query = peopleTable ++= Seq(        // insert into `PEOPLE` (`PER_FNAME`,`PER_LNAME`,`PER_AGE`)  values (?,?,?)
        (10, "John", "King", 36),
        (20, "John", "Burnett", 24),
        (40, "Senga","MacPherson",27),
        (50, "Angus","Johnston",25),
        (30,"Fraser","Campbell",23)
      )
      println(query.statements.head)         // would print out the query one line up
      db.run(query)
    }
    Await.result(insertPeople, Duration.Inf).andThen {
      case Success(_) => listPeople
      case Failure(error) => println("Welp! Something went wrong! " + error.getMessage)
    }
  }

  def listPeople = {
    val queryFuture = Future {
         db.run(peopleTable.result).map(_.foreach {                                               // simple query that selects everything from People and prints them out
           case (id, fName, lName, age) => println(s" $id $fName $lName $age")
         })
    }
    Await.result(queryFuture, Duration.Inf).andThen {
      case Success(_) =>  db.close()  //cleanup DB connection
          case Failure(error) =>
            println("Listing people failed due to: " + error.getMessage)
    }
  }

  def updatePeople ={
    val queryFuture = Future{
      val query = (1,"Blair","Menzies",36)
      db.run(peopleTable.filter(_.fName==="John").update(query))
    }
    Await.result(queryFuture,Duration.Inf).andThen{
      case Success(_) =>println("Success")
      case Failure(error) =>println("Failed")
    }
  }
  def deletePeople ={
    val queryFuture = Future{
      db.run(peopleTable.filter(_.fName==="Blair").delete)
    }
    Await.result(queryFuture,Duration.Inf).andThen{
      case Success(_) =>println("deleted: Blair")
      case Failure(error)=> println("Failed")
    }
  }
  def searchPeople ={
    val name = "Senga"
    val queryFuture = Future{
      db.run(peopleTable.filter(_.fName===name).result)
    }
    Await.result(queryFuture,Duration.Inf).andThen{
      case Success(_) =>println(s"found $name")
      case Failure(error) =>println("Failed")
    }
  }
  def countNumberOPeople={
    val queryFuture = Future{
      db.run(peopleTable.size.result).map({case num => println(s"There is $num people in the database")})
    }
    Await.result(queryFuture,Duration.Inf).andThen{
      case Success(_) =>println("The result been proccessed")
      case Failure(error) =>println("Failed")
    }
  }
  def averageAge={
    val queryFuture = Future{
      db.run(peopleTable.map(person=>person.age).avg/*.sum./(peopleTable.size)*/.result)
        .map({
          case Some(i: Int)=>println(s"The average age is: ${i}")
          case None=>println("Nothing was returned") })
    }
    Await.result(queryFuture, Duration.Inf).andThen{
      case Success(_) => println("The result has been proccessed")
      case Failure(error) => println("Failed")
    }
  }
  def mostCommonName = {
    val queryFuture = Future {
      db.run(peopleTable.map(_.fName).result).map({case i => println(i)})
    }
    Await.result(queryFuture, Duration.Inf).andThen {
        case Success(_) => println("The result has been processed")
        case Failure(error) => println("Failed")
    }
  }
  mostCommonName

  Thread.sleep(10000)
}

