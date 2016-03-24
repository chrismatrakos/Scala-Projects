// Emrah Kaya       -- emrk@itu.dk
// Christo Matrakou -- chmat@itu.dk

package adpro
import scala.language.higherKinds

// The implementation is based on Section 3 of the paper.
//
// This implementation is designed to be eager, following the regular strictness
// of Scala.  However it would be an interesting exercise to extend it so that
// it is possibly lazy, like in the paper of Hinze and Paterson.  The obvious
// choice is to make values of elements stored in the queue lazy.  Then there is
// also a discussion of possible suspension of the middle element of the tree on
// page 7.

// QUESTION I: Complete the implementation of Finger Trees below.  Incomplete
// places are marked ...
//
// I am Simulating a package with an object, because type declarations
// can only be placed in objects (so this allows me to place Digit on top).

object data {

  // The interface spec for reducible structures, plus two useful derived
  // reducers that the paper introduces (toList and toTree)

  // I changed the type of reducers to not use curried operators, but regular
  // binary operators.  This is more natural in Scala, and gives easier to read
  // syntax of expressions.  Curried style is preferred in Haskell.

  trait Reduce[F[_]] {
    def reduceR[A,B] (opr: (A,B) => B) (fa: F[A], b: B) :B
    def reduceL[A,B] (opl: (B,A) => B) (b: B, fa: F[A]) :B

    // page 3

    def toList[A] (fa: F[A]) :List[A] =
      reduceR[A, List[A]]((a, b) => a +: b)(fa, List())
    // page 6
    //
    def toTree[A] (fa :F[A]) :FingerTree[A] =
      reduceR((a:A, b:FingerTree[A]) => FingerTree.addL(a, b))(fa, Empty())
  }

  // Types for Finger trees after Hinze and Pattersoni (page 4)

  type Digit[A] = List[A]

  sealed trait Node[+A] {

    // uncomment the delagation once Node.toList is implemented
    //
    def toList :List[A] = Node.toList (this)
  }

  case class Node2[A] (l :A, r :A) extends Node[A]
  case class Node3[A] (l: A, m: A, r: A) extends Node[A]

  sealed trait FingerTree[+A] {

    // The following methods are convenience delagation so we can use
    // the operations both as methods and functions.
    // Uncomment them once you have implemented the corresponding functions.

    def addL[B >:A] (b: B) :FingerTree[B] = FingerTree.addL (b,this)
    def addR[B >:A] (b: B) :FingerTree[B] = FingerTree.addR (this,b)
    def toList :List[A] = FingerTree.toList (this)

    def headL :A = FingerTree.headL (this)
    def tailL :FingerTree[A] = FingerTree.tailL (this)
    def headR :A = FingerTree.headR (this)
    def tailR :FingerTree[A] = FingerTree.tailR (this)

    // page 7 (but this version uses polymorphis for efficiency, so we can
    // implement it differently; If you want to follow the paper closely move them to
    // FingerTree object and delegate the methods, so my tests still work.

    def empty :Boolean =    if (this == Empty()) true else false
    def nonEmpty :Boolean = if (this == Empty()) false else true
  }
  case class Empty () extends FingerTree[Nothing] {

    // page 7
    //
    // override def empty =  ...
    // override def nonEmpty = ...
  }
  case class Single[A] (data: A) extends FingerTree[A]
  // paramter names: pr - prefix, m - middle, sf - suffix
  case class Deep[A] (pr: Digit[A], m: FingerTree[Node[A]], sf: Digit[A]) extends FingerTree[A]

  // page 6
  //
  // Types of views on trees
  // The types are provided for educational purposes.  I do not use the view
  // types in my implementation. I implement views as Scala extractors.
  // But you may want to implement views first like in the paper, and then
  // conver them to Scala extractors.

  // In the paper views are generic in the type of tree used. Here I make them
  // fixed for FingerTrees.

  sealed trait ViewL[+A]
  case class NilTree () extends ViewL[Nothing]
  case class ConsL[A] (hd: A, tl: FingerTree[A]) extends ViewL[A]

  // Left extractors for Finger Trees (we use the same algorithm as viewL in the
  // paper). You can do this, once you implemented the views the book way.
  // Once the extractors are implemented you can pattern match on NilTree, ConsL
  // and ConsR
  //
  // See an example extractor implemented for Digit below (Digit.unapply)

  object NilTree { // we use the same extractor for both left and right views
     def unapply[A] (t: FingerTree[A]) :Boolean = t match {
       case Empty()   => true
       case _         => false
     }
  }

  object ConsL {
    def unapply[A] (t: FingerTree[A]) :Option[(A, FingerTree[A])] = t match {
      case NilTree()      => None
      case Single(x)      => Some(x, Empty())
      case Deep(p, m, s)  => Some(p.head, FingerTree.deepL(p.tail, m, s))
    }
  }

  object ConsR {
    def unapply[A] (t: FingerTree[A]) :Option[(FingerTree[A], A)] = t match {
      case NilTree()      => None
      case Single(x)      => Some(Empty(), x)
      case Deep(p, m, s)  => Some(FingerTree.deepR(p, m, s.tail), s.head)
    }
  }

  // several convenience operations for Digits.
  //
  object Digit  extends Reduce[Digit] { // uncomment once the interfaces are provided

    // page 3, top
    //
    def reduceR[A,Z] (opr: (A,Z) => Z) (d: Digit[A], z: Z) :Z =
      d.foldRight(z)(opr)
    def reduceL[A,Z] (opl: (Z,A) => Z) (z: Z, d: Digit[A]) :Z =
      d.foldLeft(z)(opl)

    // Digit inherits toTree from Reduce[Digit] that we will also apply to other
    // lists, but this object is a convenient place to put it (even if not all
    // lists are digits)

    // This is a factory method that allows us to use Digit (...) like a
    // constructor
    def apply[A] (as: A*) : Digit[A] = List(as:_*)

    // This is an example of extractor, so that we can use Digit(...) in pattern
    // matching.  Case classes have extractors automatically, but Digit defined
    // as above is not a case class, but just a type name.
    def unapplySeq[A] (d: Digit[A]): Option[Seq[A]] = Some (d)
  }



  object Node  extends Reduce[Node] {

    //page 5, top
    // instance of Reduce Node

    //reducer

    def reduceR[A,Z] (opr: (A,Z) => Z) (n :Node[A], z: Z) :Z = n match{
      case Node2(a, b)    => opr(a, opr(b, z))
      case Node3(a, b, c) => opr(a, opr(b, opr(c, z)))
    }

    //reducel
    def reduceL[A,Z] (opl: (Z,A) => Z) (z: Z, n :Node[A]) :Z = n match{
      case Node2(b, a)    => opl(opl(z, b), a)
      //case Node2(b, a)    => opl(opl(z, a), b)
      case Node3(c, b, a) => opl(opl(opl(z, c), b), a)
    }
  }


  // Most of the paper's key functions are in the module below.

 object FingerTree extends  Reduce[FingerTree] {// uncomment once the interface is implemented

    // page 5

    //p = prefix   s = suffix

    def reduceR[A, Z](opr: (A, Z) => Z)(t: FingerTree[A], z: Z): Z = t match {
      case Empty()        => z
      case Single(x)      => opr (x, z)
      case Deep(p, m, s)  =>
        Digit.reduceR(opr)(p, reduceR(Node.reduceR(opr) _)(m, Digit.reduceR(opr)(s, z)))
    }

    def reduceL[A, Z](opl: (Z, A) => Z)(z: Z, t: FingerTree[A]): Z = t match {
      case Empty()        => z
      case Single(x)      => opl(z, x)
      case Deep(p, m, s)  =>
        Digit.reduceL(opl)(reduceL(Node.reduceL(opl) _)(Digit.reduceL(opl)(z, p), m), s)
    }

    // page 5 bottom (the left triangle); Actually we could use the left
    // triangle in Scala but I am somewhat old fashioned ...

    def addL[A]( a: A, t: FingerTree[A]): FingerTree[A] = t match {
      case Empty()                        => Single(a)
      case Single(b)                      => Deep(Digit(a), Empty(), Digit(b))
      case Deep(b::c::d::e::Nil, m, sf)   => Deep(Digit(a, b), addL(Node3(c, d, e), m), sf)
      case Deep(pr, m, sf)                => Deep(Digit(a) ++ pr, m, sf)
    }

    def addR[A](t: FingerTree[A], a: A): FingerTree[A] = t match {
      case Empty()                        => Single(a)
      case Single(b)                      => Deep(Digit(b), Empty(), Digit(a))
      case Deep(pr, m, e::d::c::b::Nil)   => Deep(pr, addR(m, Node3(e, d, c)), Digit(b, a))
      case Deep(pr, m, sf)                => Deep(pr, m, sf ++ Digit(a))
    }

    // page 6
    //
    // This is a direct translation of view to Scala. You can replace it later
    // with extractors in Scala, see above objects NilTree and ConsL (this is an
    // alternative formulation which is more idiomatic Scala, and slightly
    // better integrated into the language than the Haskell version).
    // In Haskell we need to call viewL(t) to pattern match on views.  In Scala,
    // with extractors in place, we can directly pattern match on t.
    //

    // def viewL[A] (t: FingerTree[A]) :ViewL[A] = ...

    // page 6
    //
      // A smart constructor that allows pr to be empty

   //data View L s a = Nil L | ConsL a (s a)

    def deepL[A] (pr: Digit[A], m: FingerTree[Node[A]], sf: Digit[A]) :FingerTree[A] = pr match{
      case Nil => m match {
        case NilTree()    => Digit.toTree(sf)
        case ConsL(a, m)  => Deep(Node.toList(a), m, sf)
      }
      case _ => Deep(pr, m, sf)
    }

    def deepR[A](pr: Digit[A], m: FingerTree[Node[A]], sf: Digit[A]): FingerTree[A] = sf match{
      case Nil => m match {
        case NilTree()    => Digit.toTree(pr)
        case ConsR(m, a)  => Deep(pr, m, Node.toList(a))
      }
      case _ => Deep(pr,m,sf)
    }

    // page 7

    def headL[A](t: FingerTree[A]): A = t match {
      case ConsL(a, _) => a
    }

    def tailL[A](t: FingerTree[A]): FingerTree[A] = t match {
      case ConsL(_, b) => b
    }

    def headR[A](t: FingerTree[A]): A = t match {
      case ConsR(_, c) => c
    }

    def tailR[A](t: FingerTree[A]): FingerTree[A] = t match{
      case ConsR(d, _) => d
    }
  }
}

/* QUESTION II
 *
 * Compare efficiency of your FingerTrees and DoubleLinkedList (imperative)
 * implementation in the standard library.  Try storing integers and lists of
 * various sizes (short, middle, long).  You can use scalacheck to generate long
 * lists of random values (or write generators manually).  Then try also to
 * measure performance of 3 interesting lists length (established using
 * integers) and 3 different sizes of random string data (short, middle, and
 * very long, say in MB).
 *
 * One interesting idea is to generate a random list of operations: addR (n),
 * addL(n), popL, popR, and compare performance of the two structures on many
 * random lists.   Measuring just insertions, is not stressing the FingerTrees
 * enough.  The trees are also restructured on popping. (NB. popL is the same as
 * tailL, and popR is the same as tailR)
 *
 * Recall that due to just-in-time compilation, it is useful to run a
 * computation several times before you start measuring.  Then run it several
 * times and take average (or sum).  All these run have to happen in the same
 * virtual machine session.
 *
 * Summarize the results below in a simple table listing times for 4 selected
 * lengths of integer lists (for DoubleLinkedList and for Finger Trees). Before
 * the table write approximately 100 words explaining what you measured.
 *
 * Size | FingerTree time | DoubleLinkedList
 * -----------------------------------------
 *      |                 |
 * -----------------------------------------
 *      |                 |
 * -----------------------------------------
 *      |                 |
 * -----------------------------------------
 *      |                 |
 * -----------------------------------------
 *
 * Then report the string numbers in the table like the above
 *
 * FingerTreeList
 * Size of List \ Size of String | short | medium | very long
 * ----------------------------------------------------------
 * enter list size               |       |        |
 * ----------------------------------------------------------
 * enter list size               |       |        |
 * ----------------------------------------------------------
 * enter list size               |       |        |
 * ----------------------------------------------------------
 *
 * DoubleLinkedList
 *  Size of List \ Size of String | short | medium | very long
 * ----------------------------------------------------------
 * enter list size               |       |        |
 * ----------------------------------------------------------
 * enter list size               |       |        |
 * ----------------------------------------------------------
 * enter list size               |       |        |
 * ----------------------------------------------------------A
 *
 * Repeat the table for FingerTrees and for doubly linked list.  Remember to
 * include units of time, and of size in all tables.  Replace list sizes, and
 * short/medium/very-long above with concrete numbers.
 *
 * Finally summarize your conclusion in approximately 100 words.
 *
 * #########################
 * SOLUTION FOR QUESTION II:
 * #########################
 * HOW WE TESTED IT.
 * We used ScalaCheck to generate lists of integers (first table)
 * and strings (two following tables). For each run of the integer tests, we
 * tested the performance of the methods addR, addL, popL and popR for the
 * FingerTrees and DoubleLinksList data structures. For integer lists, the sizes
 * 50, 250, 500 and 1000 were tested.
 * We also tested string lists, with te sizes 50, 500, 1000.
 * Note, for DoubleLinkedList's we also ran 100 and 250, so we could get a
 * better idea of it's performance.
 * If possible every test was run five times, and the average noted down.
 * All of the tests were run on Windows 10, Intel i7-5500U CPU @ 2.40 2.40
 * with 8 GB RAM.
 *
 * Size | FingerTree time | DoubleLinkedList
 * -----------------------------------------
 * 50   | 353ms           | 231ms
 * -----------------------------------------
 * 250  | 489ms           | 1s 470ms
 * -----------------------------------------
 * 500  | 569ms           | 17s 371ms
 * -----------------------------------------
 * 1000 | 694s            | 5m 26s
 * -----------------------------------------
 *
 * FingerTree list
 * Size of List \ Size of String    | 10    | 1000  | 100000
 * ----------------------------------------------------------
 * 50                               | 563ms | 455ms | 458ms
 * ----------------------------------------------------------
 * 500                              | 615ms | 572ms | 592ms
 * ----------------------------------------------------------
 * 1000                             | 690ms | 642ms | 634ms
 * ----------------------------------------------------------
 *
 * DoubleLinked list
 * Size of List \ Size of String    | 10         | 1000     | 100000
 * ----------------------------------------------------------
 * 50                               | 807ms     | 1s 60ms   | 683ms
 * ----------------------------------------------------------
 * 100                              | 3s 82ms   | 3s 357ms  | 3s 234ms
 * ----------------------------------------------------------
 * 250                              | 1m 33s    | 1m 32s    | 1m 32s
 * ----------------------------------------------------------
 * 500                              | DNF       | DNF       | DNF
 * ----------------------------------------------------------
 * 1000                             | DNF       | DNF       | DNF
 * ----------------------------------------------------------
 *
 * As we can see from the times noted in the tables above, it is very clear
 * that the FingerTree data structure out performs DoubleLinkedList. It seems
 * that FingerTrees performs even faster for lists consisting of heavy items
 * (very long strings). The DoubleLinkedList performs quite well for short lists
 * consisting of light items - while it's very slow as the size and contents grow
 * in size. As we can see from the table, we couldn't even get a meassurement of
 * the sizes 5000 and 1000.
 */
