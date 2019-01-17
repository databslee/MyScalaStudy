trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object State {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
    if (n < 0) nonNegativeInt(rng2) else (n, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, rng2) = rng.nextInt
    val m = (n.toDouble - 1) / n.toDouble
    (m, rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, state) = nonNegativeInt(rng)
    val (d, state2) = double(rng)

    ((i, d), state2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (id, state) = intDouble(rng)

    (id.swap, state)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, state) = double(rng)
    val (d2, state2) = double(state)
    val (d3, state3) = double(state2)

    ((d1, d2, d3), state3)
  }

  def ints(count: Int)(rng: RNG):(List[Int], RNG) = {
    (null, rng)
  }



  type State[S, +A] = S => (A, S)
  type Rand[A] = State[RNG, A]
  def unit[A](a: A): Rand[A] = rng => (a, rng)
  def map[S, A, B](a: S => (A,S))(f: A => B): S => (B,S) = { rng =>
    val (s, rng2) = a(rng)
    (f(s), rng2)
  }

  def doubleWithMap(rng: RNG): (Double, RNG) =
    (map(nonNegativeInt)(_.toDouble / Int.MaxValue))(rng)

  def main(args: Array[String]): Unit = {
    val rng1 = SimpleRNG(42)
    val(n1,rng2) = rng1.nextInt
    println(n1)
    val(n2,rng3) = rng2.nextInt
    println(n2)
    val(n3,rng4) = rng3.nextInt
    println(n3)

    val(n4, rng5) = nonNegativeInt(rng1)
    println(n4)
    val(n5, rng6) = nonNegativeInt(rng2)
    println(n5)


    val(n6, rng7) = double(rng1)
    println(n6)
    val(n7, rng8) = double(rng7)
    println(n7)
  }
}