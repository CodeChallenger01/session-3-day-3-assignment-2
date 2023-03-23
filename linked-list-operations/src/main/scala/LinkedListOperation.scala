class LinkedList[A] {
  case class Node[A](val data: A, var next: Node[A])
  var head: Node[A] = null

  //This method is used to insert the element
  def insertElement(value: A): Unit = {
    try{
      if(value==0 || value==null) throw new RuntimeException("Invalid Value")
      else{
        def insert(node: Node[A]): Unit = {
          if (node.next == null) {
            node.next = new Node[A](value, null)
          }
          else {
            insert(node.next)
          }
        }

        if (head == null) {
          head = new Node(value, null)
        }
        else {
          insert(head)
        }
      }
    }
    catch{
      case ex: RuntimeException=> "Exception resolved"
    }

  }

  //this method is used to delete the element
  def deleteElement(value: A): Unit = {
    try{
      if(value==0 || value==null) throw new RuntimeException("Invalid Input")
      else{
        def delete(node: Node[A]): Unit = {
          if (node == null || node.next == null) return

          if (node.next.data == value) {
            node.next = node.next.next
          }
          else {
            delete(node.next)
          }
        }

        if (head != null && head.data == value) {
          head = head.next
        }
        else {
          delete(head)
        }
      }
    }
    catch {
      case ex => new RuntimeException("Invalid Input")
    }

  }

  //this method is used to traverse the element
  def searchElement(value: A): Option[Node[A]] = {
    def search(node: Node[A]): Option[Node[A]] = {
      if (node == null) {
        None
      }
      else if (node.data == value) {
        Some(node)
      }
      else {
        search(node.next)
      }
    }

    search(head)
  }

  //this method is used to traverse the element
  def traverseElement(): Unit = {
    var current = head
    while(current!=null){
      print(current.data)
      current=current.next
    }
  }
  //To check link is present or not
  def hasElements: Boolean = {
    if (head == null) false
    else true
  }
}

object Link extends App {
  val int = new LinkedList[Int]
  int.insertElement(2)
  int.insertElement(3)
  int.traverseElement()
}