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
}

