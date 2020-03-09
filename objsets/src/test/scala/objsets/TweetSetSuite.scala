package objsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

//  test("filter: on slava set") {
//    new TestSets {
//      val s1 = new Empty
//      val s2 = s1.incl(new Tweet("a", "10", 20))
//      val s3 = s2.incl(new Tweet("a", "15", 20))
//      val s4 = s3.incl(new Tweet("a", "13", 20))
//      val s5 = s4.incl(new Tweet("a", "12", 20))
//      val s6 = s5.incl(new Tweet("a", "5", 20))
//      val s7 = s6.incl(new Tweet("a", "7", 20))
//      val s8 = s7.incl(new Tweet("a", "3", 20))
//      val s9 = s8.incl(new Tweet("a", "4", 20))
//      val s10 = s9.incl(new Tweet("a", "2", 20))
//      val s11 = s10.incl(new Tweet("a", "16", 2))
//      val s12 = s11.incl(new Tweet("a", "17", 2))
//      assert(size(s12.filter(tw => tw.retweets == 20)) === 9)
//    }
//  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }

  }
