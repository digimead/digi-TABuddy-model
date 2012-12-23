package org.digimead.tabuddy.model.history

import scala.collection.mutable
import scala.collection.script

trait ObservableElementBuffer[A] extends mutable.Buffer[A] with mutable.Publisher[script.Message[A] with mutable.Undoable] {

  type Pub <: ObservableElementBuffer[A]
  abstract override def +=(element: A): this.type = {
    super.+=(element)
    publish(new script.Include(script.End, element) with mutable.Undoable {
      def undo() { trimEnd(1) }
    })
    this
  }

  abstract override def +=:(element: A): this.type = {
    super.+=:(element)
    publish(new script.Include(script.Start, element) with mutable.Undoable {
      def undo() { trimStart(1) }
    })
    this
  }

  abstract override def update(n: Int, newelement: A): Unit = {
    val oldelement = apply(n)
    super.update(n, newelement)
    publish(new script.Update(script.Index(n), newelement) with mutable.Undoable {
      def undo() { update(n, oldelement) }
    })
  }

  abstract override def remove(n: Int): A = {
    val oldelement = apply(n)
    super.remove(n)
    publish(new script.Remove(script.Index(n), oldelement) with mutable.Undoable {
      def undo() { insert(n, oldelement) }
    })
    oldelement
  }

  abstract override def clear(): Unit = {
    super.clear
    publish(new script.Reset with mutable.Undoable {
      def undo() { throw new UnsupportedOperationException("cannot undo") }
    })
  }
/*  abstract override def +=(elem: A): this.type = {
    if (!contains(elem)) {
      super.+=(elem)
      publish(new script.Include(elem) with mutable.Undoable { def undo = -=(elem) })
    }
    this
  }

  abstract override def -=(elem: A): this.type = {
    if (contains(elem)) {
//      super.-=(elem)
      publish(new script.Remove(elem) with mutable.Undoable { def undo = +=(elem) })
    }
    this
  }

  abstract override def clear(): Unit = {
//    super.clear
    publish(new script.Reset with mutable.Undoable {
      def undo(): Unit = throw new UnsupportedOperationException("cannot undo")
    })
  }*/
}
