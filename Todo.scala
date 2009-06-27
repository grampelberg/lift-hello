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
  }

  object desc extends MappedPoliteString(this, 128)
}

object Todo extends Todo with LongKeyedMetaMapper[ToDo]
