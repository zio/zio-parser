package zio.parser.internal

/** A very fast, growable/shrinkable, mutable stack.
  *
  * Based on zio.internal.Stack
  */
private[zio] final class Stack[A <: AnyRef]() {
  private[this] var array   = new Array[AnyRef](13)
  private[this] var size    = 0
  private[this] var nesting = 0

  /** Determines if the stack is empty.
    */
  def isEmpty: Boolean = size == 0

  /** Pushes an item onto the stack.
    */
  def push(a: A): Unit =
    if (size == 13) {
      array = Array(array, a, null, null, null, null, null, null, null, null, null, null, null)
      size = 2
      nesting += 1
    } else {
      array(size) = a
      size += 1
    }

  /** Pops an item off the stack, or returns `null` if the stack is empty.
    */
  def pop(): A =
    if (size <= 0) {
      null.asInstanceOf[A]
    } else {
      val idx = size - 1
      var a   = array(idx)
      if (idx == 0 && nesting > 0) {
        array = a.asInstanceOf[Array[AnyRef]]
        a = array(12)
        array(12) = null // GC
        size = 12
        nesting -= 1
      } else {
        array(idx) = null // GC
        size = idx
      }
      a.asInstanceOf[A]
    }

  /** Peeks the item on the head of the stack, or returns `null` if empty.
    */
  def peek(): A =
    if (size <= 0) {
      null.asInstanceOf[A]
    } else {
      val idx = size - 1
      var a   = array(idx)
      if (idx == 0 && nesting > 0) a = (a.asInstanceOf[Array[AnyRef]])(12)
      a.asInstanceOf[A]
    }

  def peekOrElse(a: A): A = if (size <= 0) a else peek()

  override def clone(): Stack[A] = {
    val cloned = new Stack[A]
    cloned.cloneFrom(array, size, nesting)
    cloned
  }

  private def cloneFrom(otherArray: Array[AnyRef], otherSize: Int, otherNesting: Int): Unit = {
    this.size = otherSize
    this.nesting = otherNesting
    this.array = otherArray.clone()
    var idx = 0
    var arr = this.array
    while (idx < nesting) {
      val c = arr(0).asInstanceOf[Array[AnyRef]].clone()
      arr(0) = c
      arr = c
      idx = idx + 1
    }
  }
}

private[zio] object Stack {
  def apply[A <: AnyRef](): Stack[A]     = new Stack[A]
  def apply[A <: AnyRef](a: A): Stack[A] = {
    val stack = new Stack[A]

    stack.push(a)

    stack
  }
}
