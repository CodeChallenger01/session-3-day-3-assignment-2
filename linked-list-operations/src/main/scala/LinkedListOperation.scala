class LinkedList[A] {
  case class Node[A](val data: A, var next: Node[A])
  var head: Node[A] = null

  //This method is used to insert the element
  def insertElement(value: A): Unit = {
    def insert(node: Node[A]): Unit = {
      if (node.next == null) {
        node.next = new Node[A](value, null)
      } else {
        insert(node.next)
      }
    }

    if (head == null) {
      head = new Node(value, null)
    } else {
      insert(head)
    }
  }

  //this method is used to delete the element
  def deleteElement(value: A): Unit = {
    def delete(node: Node[A]): Unit = {
      if (node == null || node.next == null) return

      if (node.next.data == value) {
        node.next = node.next.next
      } else {
        delete(node.next)
      }
    }

    if (head != null && head.data == value) {
      head = head.next
    } else {
      delete(head)
    }
  }

  //this method is used to traverse the element
  def traverseElement(fun: A => Unit): Unit = {
    def traverse(node: Node[A]): Unit = {
      if (node != null) {
        fun(node.data)
        traverse(node.next)
      }
    }

    traverse(head)
  }

  //this method is used to traverse the element
  def searchElement(value: A): Option[Node[A]] = {
    def search(node: Node[A]): Option[Node[A]] = {
      if (node == null) {
        None
      } else if (node.data == value) {
        Some(node)
      } else {
        search(node.next)
      }
    }

    search(head)
  }

  //To check link is present or not
  def hasElements: Boolean = {
    if (head == null) false
    else true
  }
}

object Link extends App {

}