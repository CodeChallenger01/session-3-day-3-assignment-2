import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LinkedListOperationTest extends AnyFlatSpec with Matchers {

  val intObject = new LinkedList[Int]
  val stringObject = new LinkedList[String]
  val longObject = new LinkedList[Long]

  //TEST CASES FOR INTEGER DATA TYPE
  "it" should "match with empty list" in {
    assert(intObject.head == null)
  }

  "it" should "match with first element" in {
    intObject.insertElement(21)
    intObject.head.data shouldBe 21
  }

  "it" should "match with second element" in {
    intObject.insertElement(25)
    intObject.head.data == 25
  }

  "it" should "match with has elements" in {
    assert(intObject.hasElements)
  }

  "it" should "not match with searched element" in {
    val search = intObject.searchElement(21)
    assert(search != 21)
  }

  "it" should "match with deleted element" in {
    println(intObject.deleteElement(21))
  }

  "it " should "match with traverse element" in{
    val firstElement=intObject.insertElement(21)
    val list=intObject.traverseElement()
    assert(list==firstElement)
  }



  //TEST CASES OF STRING DATA TYPE
  "it" should "match with empty list of string" in {
    assert(stringObject.head == null)
  }

  "it" should "match with first element of string" in {
    stringObject.insertElement("Manish")
    stringObject.head.data shouldBe "Manish"
  }

  "it" should "match with second element of string" in {
    stringObject.insertElement("Rohan")
    stringObject.head.data == "Rohan"
  }

  "it" should "match with has elements of string" in {
    assert(stringObject.hasElements)
  }

  "it" should "not match with searched element of string" in {
    val search = stringObject.searchElement("Rahul")
    assert(search != "Rahul")
  }

  "it" should "match with deleted element of string" in {
    println(stringObject.deleteElement("Rohan"))
  }

  "it " should "match with traverse element of string" in {
    val firstElement = stringObject.insertElement("Manish")
    val list = intObject.traverseElement()
    assert(list == firstElement)
  }


  //TEST CASES FOR LONG DATA TYPE
  "it" should "match with empty list of long integer" in {
    assert(longObject.head == null)
  }

  "it" should "match with first element of long integer" in {
    longObject.insertElement(21l)
    longObject.head.data shouldBe 21l
  }

  "it" should "match with second element of long integer" in {
    longObject.insertElement(25l)
    intObject.head.data == 25l
  }

  "it" should "match with has elements of long integer" in {
    assert(longObject.hasElements)
  }

  "it" should "not match with searched element of long integer" in {
    val search = longObject.searchElement(21l)
    assert(search != 21)
  }

  "it" should "match with deleted element of long integer" in {
    println(longObject.deleteElement(21l))
  }

  "it " should "match with traverse element of long" in {
    val firstElement = longObject.insertElement(2l)
    val list = longObject.traverseElement()
    assert(list == firstElement)
  }

}