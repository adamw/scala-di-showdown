package com.softwaremill.di.done

import java.util.concurrent.ConcurrentHashMap

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object Step4 extends App {
  class CRUD[K, V] {
    private val s = new ConcurrentHashMap[K, V]()

    def create(k: K, v: V): Future[Boolean] = Future.successful(s.putIfAbsent(k, v) == null)
    def read(k: K): Future[Option[V]] = Future.successful(Option(s.get(k)))
    def update(k: K, v: V): Future[Unit] = Future.successful(s.put(k, v)) // todo
    def delete(k: K): Future[Boolean] = Future.successful(s.remove(k) != null)
  }

  //

  type FoodName = String
  type Quantity = Int
  type FoodCRUD = CRUD[String, Int]

  class Fridge(iot: IoT) {
    def addFood(n: FoodName, q: Quantity): FoodCRUD => Future[Unit] = { fc =>
      for {
        current <- fc.read(n)
        updated = current.map(c => c + q).getOrElse(q)
        _ <- fc.update(n, updated)
        _ <- iot.notifyUser(s"$q of $n added to the fridge ")
      } yield ()
    }

    def takeFood(n: FoodName, q: Quantity): FoodCRUD => Future[Quantity] = { fc =>
      for {
        current <- fc.read(n)
        inStock = current.getOrElse(0)
        taken = Math.min(inStock, q)
        left = inStock - taken
        _ <- if (left > 0) fc.update(n, left) else fc.delete(n)
        _ <- iot.notifyUser(s"$taken of $n taken from the fridge")
      } yield taken
    }
  }

  //

  class Cooker(f: Fridge) {
    def cookSauce(q: Quantity): FoodCRUD => Future[Quantity] = { fc =>
      for {
        tomatoQ <- f.takeFood("tomato", q)(fc)
        vegQ <- f.takeFood("non-tomato veggies", q)(fc)
        _ <- f.takeFood("garlic", q * 2)(fc)
        sauceQ = tomatoQ / 2 + vegQ * 3 / 4
        _ <- f.addFood("sauce", sauceQ)(fc)
      } yield sauceQ
    }

    def cookPasta(q: Quantity): FoodCRUD => Future[Quantity] = { fc =>
      for {
        pastaQ <- f.takeFood("pasta", q)(fc)
        _ <- f.takeFood("salt", 10)(fc)
        _ <- f.addFood("cooked pasta", pastaQ)(fc)
      } yield pastaQ
    }
  }

  //

  trait IoT {
    def notifyUser(msg: String): Future[Unit]
  }

  class CloudIoTGateway(username: String, pass: String) extends IoT {
    override def notifyUser(msg: String): Future[Unit] = Future.successful {
      println("Sending via cloud: " + msg)
    }
  }

  //

  lazy val fc = new FoodCRUD
  lazy val fridge = new Fridge(iot)
  lazy val iot = new CloudIoTGateway("scott", "tiger")
  lazy val cooker = new Cooker(fridge)
  
  //

  val shopping = { fc: FoodCRUD =>
    for {
      _ <- fridge.addFood("tomato", 10)(fc)
      _ <- fridge.addFood("non-tomato veggies", 15)(fc)
      _ <- fridge.addFood("garlic", 42)(fc)
      _ <- fridge.addFood("salt", 1)(fc)
      _ <- fridge.addFood("pasta", 5)(fc)
    } yield ()
  }

  val cooking = { fc: FoodCRUD =>
    for {
      _ <- shopping(fc)
      sq <- cooker.cookSauce(5)(fc)
      pq <- cooker.cookPasta(10)(fc)
    } yield s"Cooked $sq sauce and $pq pasta"
  }

  val eating = Await.result(cooking(fc), 1.minute)
  println(eating)
}
