package com.liftworkshop.model
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._

class Todo extends LongKeyedMapper[Todo] with IdPK {
  def getSingleton = Todo

  object done extends MappedBoolean(this)
  object owner extends MappedLongForeignKey(this, User)
  object priority extends MappedInt(this) {
    override def defaultValue = 5
    override def validations = validPriority _ :: super.validations
    def validPriority(in: Int): List[FieldError] =
      if (in > 0 && in <= 10) Nil
      else List(FieldError(this, <b>Priority must be 1-10</b>))
    override def _toForm = Full(select(Todo.priorityList,
                                       Full(is.toString),
                                       f => set(f.toInt)))
  }

  object desc extends MappedPoliteString(this, 128) {
    override def validations =
      valMinLen(3, "Description must be 3 characters") _ ::
    super.validations
  }
}

object Todo extends Todo with LongKeyedMetaMapper[Todo] {
  lazy val priorityList = (1 to 10).map(v => (v.toString, v.toString))
}
