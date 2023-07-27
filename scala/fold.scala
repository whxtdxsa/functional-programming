import FoldRight.foldRight
object FoldRight {
    def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B =
        l match {
            case Nil => z
            case x::xs => f(x, foldRight(xs, z)(f))
        }

    def sum2(l: List[Int]) =
        foldRight(l, 0)((x, y) => x + y) 
    
    def product2(l: List[Double]) =
        foldRight(l, 1.0)(_ * _)

    def length[A](l: List[A]): Int = 
        foldRight(l, 0)((_,acc) => acc + 1)

    def append[A](l: List[A], z: List[A]) = 
        foldRight(l, z)(_::_)


}

object FoldLeft {
    def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
        l match {
            case Nil => z
            case x::xs => foldLeft(xs, f(z, x))(f)
        }
    
    def sum3(l: List[Int]) =
        foldLeft(l, 0)(_ + _)

    def product3(l: List[Double]) =
        foldLeft(l, 1.0)(_ * _)

    def length[A](l: List[A]): Int =
        foldLeft(l, 0)((acc, _) => acc + 1)

    def reverse[A](l: List[A]): List[A] = 
        foldLeft(l, List[A]())((acc, h) => h :: acc)
    
    def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = 
        foldLeft[A, B](reverse[A](l), z)((x, y) => f(y, x))

    def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A,B) => B): B =
        foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)

    def concat[A](l: List[List[A]]): List[A] =
        foldRight(l, Nil: List[A])((h, t) => foldRight(h, t)((x, y) => x::y))

    def plusOne(l: List[Int]): List[Int] = 
        foldRight(l, Nil: List[Int])( (h, t) => (h + 1)::t)

    def dToS(l: List[Double]) =
        foldRight(l, Nil: List[String])( (h, t) => h.toString::t)

    def map_1[A, B](as: List[A])(f: A => B): List[B] =
        foldRight(as, Nil: List[B])((h, t) => f(h) :: t)

    def map_2[A,B](l: List[A])(f: A => B): List[B] =
        val buf = new collection.mutable.ListBuffer[B]
        def go(l: List[A]): Unit = l match
          case Nil => ()
          case h::t => buf += f(h); go(t)
        go(l)
        List(buf.toList*) // converting from the standard Scala list to the list we've defined here

    def filter[A](l: List[A])(f: A => Boolean): List[A] =
        val buf = new collection.mutable.ListBuffer[A]
        def go(l: List[A]): Unit = l match
            case Nil => ()
            case h::t => if(f(h)) buf += h; go(t)
        go(l)
        List(buf.toList*)

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
        concat(map_2(l)(f))

    def filter_1[A](l: List[A])(f: A => Boolean): List[A] =
        flatMap(l)(i => if f(i) then List(i) else Nil)


    
}