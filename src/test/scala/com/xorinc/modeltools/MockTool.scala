package com.xorinc.modeltools

import com.google.gson.JsonObject
import com.xorinc.modeltools.tools.{ToolArgs, Tool}

class MockTool extends Tool {

  override def apply(in: String, args: ToolArgs[this.type]) = in
  override def process(in: JsonObject, args: ToolArgs[this.type]) = ???
  override def parse(map: Map[String, Any]) = ???
}
