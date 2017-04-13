package com.softwaremill.di.done

import java.util.concurrent.ConcurrentHashMap

import cats.data.ReaderT
import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object Step6 extends App {
  case class CloudConfig(username: String, password: String)

  trait KVStore[K, V] {
    def create(k: K, v: V): Future[Boolean]
    def read(k: K): Future[Option[V]]
    def update(k: K, v: V): Future[Unit]
    def delete(k: K): Future[Boolean]
  }

  class InMemoryKVStore[K, V] extends KVStore[K, V] {
    private val s = new ConcurrentHashMap[K, V]()

    def create(k: K, v: V): Future[Boolean] = Future.successful(s.putIfAbsent(k, v) == null)
    def read(k: K): Future[Option[V]] = Future.successful(Option(s.get(k)))
    def update(k: K, v: V): Future[Unit] = Future.successful(s.put(k, v)) // todo
    def delete(k: K): Future[Boolean] = Future.successful(s.remove(k) != null)
  }

  class CloudKVStore[K, V](config: CloudConfig) extends KVStore[K, V] {
    override def create(k: K, v: V): Future[Boolean] = ???
    override def read(k: K): Future[Option[V]] = ???
    override def update(k: K, v: V): Future[Unit] = ???
    override def delete(k: K): Future[Boolean] = ???
  }

  //

  type FoodName = String
  type Quantity = Int
  type FoodKV = KVStore[String, Int]
  type KVResult[T] = ReaderT[Future, FoodKV, T]

  class Fridge(iot: IoT) {
    def addFood(n: FoodName, q: Quantity): KVResult[Unit] = ReaderT { fc =>
      for {
        current <- fc.read(n)
        updated = current.map(c => c + q).getOrElse(q)
        _ <- fc.update(n, updated)
        _ <- iot.notifyUser(s"$q of $n added to the fridge ")
      } yield ()
    }

    def takeFood(n: FoodName, q: Quantity): KVResult[Int] = ReaderT { fc =>
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
    def cookSauce(q: Quantity): KVResult[Int] = {
      for {
        tomatoQ <- f.takeFood("tomato", q)
        vegQ <- f.takeFood("non-tomato veggies", q)
        _ <- f.takeFood("garlic", q * 2)
        sauceQ = tomatoQ / 2 + vegQ * 3 / 4
        _ <- f.addFood("sauce", sauceQ)
      } yield sauceQ
    }

    def cookPasta(q: Quantity): KVResult[Int] = {
      for {
        pastaQ <- f.takeFood("pasta", q)
        _ <- f.takeFood("salt", 10)
        _ <- f.addFood("cooked pasta", pastaQ)
      } yield pastaQ
    }
  }

  //

  trait IoT {
    def notifyUser(msg: String): Future[Unit]
  }

  class CloudIoTGateway(cfg: CloudConfig) extends IoT {
    override def notifyUser(msg: String): Future[Unit] = Future.successful {
      println("Sending via cloud: " + msg)
    }
  }

  //

  lazy val cfg = CloudConfig("scott", "tiger")
  lazy val fc = new CloudKVStore[FoodName, Quantity](cfg)
  lazy val fridge = new Fridge(iot)
  lazy val iot = new CloudIoTGateway(cfg)
  lazy val cooker = new Cooker(fridge)

  //

  val shopping = {
    for {
      _ <- fridge.addFood("tomato", 10)
      _ <- fridge.addFood("non-tomato veggies", 15)
      _ <- fridge.addFood("garlic", 42)
      _ <- fridge.addFood("salt", 1)
      _ <- fridge.addFood("pasta", 5)
    } yield ()
  }

  val cooking = {
    for {
      _ <- shopping
      sq <- cooker.cookSauce(5)
      pq <- cooker.cookPasta(10)
    } yield s"Cooked $sq sauce and $pq pasta"
  }

  val eating = Await.result(cooking.run(fc), 1.minute)
  println(eating)
}
