package funsets

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite extends munit.FunSuite:

  import FunSets.*

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets:
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
    val s5 = singletonSet(5)
    val s6 = singletonSet(6)
    val s7 = singletonSet(7)
    val neg1 = singletonSet(-1)
    val neg2 = singletonSet(-2)
    val neg3 = singletonSet(-3)

  /**
   * This test is currently disabled (by using .ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * .ignore annotation.
   */

  test("singleton passes for integers 1, 2, and 3") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets:
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(contains(s2, 2), "Singleton")
      assert(contains(s3, 3), "Singleton")
  }

  test("union contains all elements of each set") {
    new TestSets:
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
  }

  test("filter function passes each test") {
    new TestSets:
      // Create a set of (1, 2, 3, 4)
      val a = union(union(union(s1, s2), s3), s4)
      // Define a predicate for integers less than 3
      val filt1: FunSet = (n: Int) => n < 3
      // Create the filtered set
      val s = filter(a, filt1)
      assert(contains(s, 1), "Filter 1")
      assert(contains(s, 2), "Filter 2")
      assert(!contains(s, 3), "Filter 3")
  }

  test("difference function passes each test") {
    new TestSets:
      // Create a set of (1, 2, 6, 7)
      val a = union(union(union(s1, s2), s6), s7)
      // Create a set of (2, 3, 4, 6)
      val b = union(union(union(s2, s3), s4), s6)
      // Create the difference set
      val s = diff(a, b)
      assert(contains(s, 1), "Difference 1")
      assert(!contains(s, 2), "Difference 2")
      assert(!contains(s, 3), "Difference 3")
      assert(!contains(s, 4), "Difference 4")
      assert(!contains(s, 6), "Difference 6")
      assert(contains(s, 7), "Difference 7")
  }


  test("intersection function passes each test") {
    new TestSets:
      // Create a set of (1, 2, 6, 7)
      val a = union(union(union(s1, s2), s6), s7)
      // Create a set of (2, 3, 4, 6)
      val b = union(union(union(s2, s3), s4), s6)
      // Create the difference set
      val s = intersect(a, b)
      assert(!contains(s, 1), "Difference 1")
      assert(contains(s, 2), "Difference 2")
      assert(!contains(s, 3), "Difference 3")
      assert(!contains(s, 4), "Difference 4")
      assert(contains(s, 6), "Difference 6")
      assert(!contains(s, 7), "Difference 7")
  }


  test("forall function passes each test") {
    new TestSets:
      // Create a Predicate of all integers less than 3
      val p: FunSet = (n: Int) => n < 3
      // Create a set of (1, 2, 6, 7)
      val a = union(union(union(s1, s2), s6), s7)
      // Create a set of (4, 5, 6, 7)
      val b = union(union(union(s4, s5), s6), s7)
      // Create a set of (-3, -2, -1, 1)
      val c = union(union(union(neg1, neg2), neg3), s1)
      assert(!forall(a, p), "forall 1st set")
      assert(!forall(b, p), "forall 2nd set")
      assert(forall(c, p), "forall 3rd set")
  }


  test("exists function passes each test") {
    new TestSets:
      // Create a Predicate of all integers less than 3
      val p: FunSet = (n: Int) => n < 3
      // Create a set of (1, 2, 6, 7)
      val a = union(union(union(s1, s2), s6), s7)
      // Create a set of (4, 5, 6, 7)
      val b = union(union(union(s4, s5), s6), s7)
      // Create a set of (-3, -2, -1, 1)
      val c = union(union(union(neg1, neg2), neg3), s1)
      // Create a set of (-1, 7)
      val d = union(neg1, s7)
      assert(exists(a, p), "exist 1st set")
      assert(!exists(b, p), "exist 2nd set")
      assert(exists(c, p), "exist 3rd set")
      assert(exists(d, p), "exist 4th set")
  }


  test("map function passes each test") {
    new TestSets:
      // Create a set of (-3, 1, 2, 7)
      val a = union(union(union(s1, s2), neg3), s7)
      // Create a function to multiply all set values by 3
      def mult3(a: Int): Int = a * 3
      // Create a function to add 2 to all set values
      def add2(a: Int): Int = a + 2
      assert(!contains(map(a, add2), 1), "map 1st test")
      assert(contains(map(a, add2), 4), "map 2nd test")
      assert(contains(map(a, add2), 9), "map 3rd test")
      assert(!contains(map(a, add2), 2), "map 4th test")
      assert(contains(map(a, mult3), -9), "map 5th test")
      assert(contains(map(a, mult3), 3), "map 6th test")
      assert(!contains(map(a, mult3), 5), "map 7th test")
  }



  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
