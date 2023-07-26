/*
 * Copyright (c) bdew, 2013 - 2014
 * https://github.com/bdew/bdlib
 *
 * This mod is distributed under the terms of the Minecraft Mod Public
 * License 1.0, or MMPL. Please check the contents of the license located in
 * http://bdew.net/minecraft-mod-public-license/
 */

package net.bdew.lib.network

import java.io.{
  ObjectInputStream,
  ObjectOutputStream,
  ObjectStreamClass,
  Serializable
}
import java.lang.{Boolean => BoxedBoolean}
import java.util
import java.util.concurrent.ConcurrentHashMap
import java.util.function.{Function => JavaFunction}
import scala.collection.mutable

import cpw.mods.fml.common.network.NetworkRegistry
import cpw.mods.fml.common.network.internal.FMLProxyPacket
import io.netty.buffer.{ByteBufInputStream, ByteBufOutputStream, Unpooled}
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.ChannelHandlerContext
import io.netty.handler.codec.MessageToMessageCodec
import net.bdew.lib.BdLib
import net.bdew.lib.network.SerializedMessageCodec._

@Sharable
class SerializedMessageCodec[T <: NetChannel]
    extends MessageToMessageCodec[FMLProxyPacket, BaseMessage[T]] {
  def encode(
      ctx: ChannelHandlerContext,
      msg: BaseMessage[T],
      out: util.List[AnyRef]
  ) {
    val buff = Unpooled.buffer()
    val writer = new ObjectOutputStream(new ByteBufOutputStream(buff))
    writer.writeObject(msg)
    val pkt = new FMLProxyPacket(
      buff,
      ctx.channel.attr(NetworkRegistry.FML_CHANNEL).get
    )
    out.add(pkt)
  }

  def decode(
      ctx: ChannelHandlerContext,
      msg: FMLProxyPacket,
      out: util.List[AnyRef]
  ) {
    try {
      val reader = new ObjectInputStream(
        new ByteBufInputStream(msg.payload())
      ) {
        override def resolveClass(desc: ObjectStreamClass): Class[_] = {
          val name = desc.getName
          if (
            !validClassCache.computeIfAbsent(
              name,
              new JavaFunction[String, BoxedBoolean]() {
                override def apply(v1: String): BoxedBoolean =
                  validClasses.contains(v1) || validClassPrefixes
                    .exists(v1.startsWith)
              }
            )
          ) {
            BdLib.log.warn(
              "Received disallowed message component class: {}",
              name
            )
            if (enforceFilter) {
              throw new InvalidClassException(name)
            }
          }
          super.resolveClass(desc)
        }
      }
      out.add(reader.readObject())
    } catch {
      case e: Throwable => BdLib.log.error("Error decoding packet", e)
    }
  }
}

object SerializedMessageCodec {

  /** A hardcoded list of possible loaded - and probably safe - classes
    *
    * Can be extended via configurations
    */
  val validClasses: mutable.Set[String] = mutable.Set(
    "scala.Enumeration",
    "scala.Enumeration$Val",
    "scala.Enumeration$Value",
    "scala.collection.mutable.HashMap",
    "scala.Enumeration$ValueSet",
    "scala.Enumeration$ValueSet$",
    "scala.collection.immutable.BitSet",
    "scala.collection.immutable.BitSet$BitSet1",
    "scala.collection.immutable.List$SerializationProxy",
    "java.lang.Float",
    "java.lang.Double",
    "java.lang.Integer",
    "java.lang.Number",
    "net.bdew.lib.multiblock.data.RSMode$",
    "[B",
    "net.bdew.lib.network.NBTTagCompoundSerialize",
    "net.bdew.lib.network.ItemStackSerialize",
    "net.bdew.lib.network.BaseMessage",
    "net.bdew.lib.multiblock.data.MsgOutputCfgRSMode",
    "net.bdew.lib.multiblock.network.MsgOutputCfg",
    "net.bdew.lib.multiblock.network.MsgOutputCfgSlot",

    // ae2stuff stuff
    "net.bdew.ae2stuff.items.visualiser.VisualisationData",
    "net.bdew.ae2stuff.items.visualiser.VisualisationModes$",
    "net.bdew.ae2stuff.network.MsgSetLock",
    "net.bdew.ae2stuff.network.MsgSetRecipe",
    "net.bdew.ae2stuff.network.MsgSetRecipe2",
    "net.bdew.ae2stuff.network.MsgSetRecipe3",
    "net.bdew.ae2stuff.network.MsgVisualisationData",
    "net.bdew.ae2stuff.network.MsgVisualisationMode",

    // gendustry
    // none

    // pressure pipes
    "net.minecraftforge.common.util.ForgeDirection",
    "net.bdew.pressure.blocks.router.gui.MsgSetRouterSideControl",
    "net.bdew.pressure.items.configurator.MsgSetFluidFilter",
    "net.bdew.pressure.items.configurator.MsgUnsetFluidFilter",
    "net.bdew.pressure.network.MsgTankUpdate",

    // advanced generators
    "net.bdew.generators.network.PktDumpBuffers",

    // compacter
    "net.bdew.compacter.network.MsgSetRecurseMode",
    "net.bdew.compacter.network.MsgSetCraftMode",
    "net.bdew.compacter.network.MsgSetRsMode",
    "net.bdew.compacter.blocks.compacter.RecurseMode$",
    "net.bdew.compacter.blocks.compacter.CraftMode",
    "net.bdew.compacter.blocks.compacter.CraftMode$"

    // add other bdew mods in configurations if you need. this is as far as I'd like to support - glee8e
  )
  val validClassPrefixes: mutable.Set[String] = mutable.Set()
  val validClassCache: ConcurrentHashMap[String, BoxedBoolean] =
    new ConcurrentHashMap[String, BoxedBoolean]()
  var enforceFilter: Boolean = true

  def addValidClass(clazz: Class[_]): Unit = {
    if (classOf[Serializable].isAssignableFrom(clazz)) {
      if (validClasses.add(clazz.getName)) {
        // recursively add all superclasses that are also serializable
        // only keep adding if class not already added
        addValidClass(clazz.getSuperclass)
        // recursively add all serialized fields
        val desc = ObjectStreamClass.lookup(clazz)
        if (desc != null)
          desc.getFields
            .map { it => it.getType }
            .foreach(SerializedMessageCodec.addValidClass)
      }
    }
  }
}

class InvalidClassException(msg: String) extends ClassNotFoundException(msg)
