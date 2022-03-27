/*
 * Copyright (c) bdew, 2013 - 2014
 * https://github.com/bdew/bdlib
 *
 * This mod is distributed under the terms of the Minecraft Mod Public
 * License 1.0, or MMPL. Please check the contents of the license located in
 * http://bdew.net/minecraft-mod-public-license/
 */

package net.bdew.lib

import cpw.mods.fml.common.Mod.EventHandler
import cpw.mods.fml.common.event.{FMLPreInitializationEvent, FMLServerStartingEvent, FMLServerStoppingEvent}
import cpw.mods.fml.common.{FMLCommonHandler, Mod}
import net.bdew.lib.multiblock.data.MsgOutputCfgRSMode
import net.bdew.lib.multiblock.network.{MsgOutputCfg, MsgOutputCfgSlot, NetHandler}
import net.bdew.lib.network.{BaseMessage, NetworkSecurityLoader, SerializedMessageCodec}
import net.bdew.lib.tooltip.TooltipHandler
import net.minecraft.command.CommandHandler
import org.apache.logging.log4j.Logger

import java.io.File

@Mod(modid = "bdlib", name = "BD lib", version = "GRADLETOKEN_VERSION", modLanguage = "scala")
object BdLib {
  var log: Logger = null
  var configDir: File = null

  def logDebug(msg: String, args: Any*) = log.debug(msg.format(args: _*))
  def logInfo(msg: String, args: Any*) = log.info(msg.format(args: _*))
  def logWarn(msg: String, args: Any*) = log.warn(msg.format(args: _*))
  def logError(msg: String, args: Any*) = log.error(msg.format(args: _*))
  def logWarnException(msg: String, t: Throwable, args: Any*) = log.warn(msg.format(args: _*), t)
  def logErrorException(msg: String, t: Throwable, args: Any*) = log.error(msg.format(args: _*), t)

  val onServerStarting = Event[FMLServerStartingEvent]
  val onServerStopping = Event[FMLServerStoppingEvent]

  @EventHandler
  def preInit(ev: FMLPreInitializationEvent) {
    log = ev.getModLog
    log.info("bdlib GRADLETOKEN_VERSION loaded")
    log.debug("List of loaded APIs: " + ApiReporter.APIs)
    FMLCommonHandler.instance().registerCrashCallable(ApiReporter)
    configDir = new File(ev.getModConfigurationDirectory, "bdlib")
    SerializedMessageCodec.addValidClass(classOf[BaseMessage[_]])
    SerializedMessageCodec.addValidClass(classOf[MsgOutputCfgRSMode])
    SerializedMessageCodec.addValidClass(classOf[MsgOutputCfg])
    SerializedMessageCodec.addValidClass(classOf[MsgOutputCfgSlot])
    NetworkSecurityLoader.loadConfigFiles()
    NetHandler.init()
    if (ev.getSide.isClient) {
      TooltipHandler.init()
    }
  }

  @EventHandler
  def serverStarting(event: FMLServerStartingEvent) {
    val commandHandler = event.getServer.getCommandManager.asInstanceOf[CommandHandler]
    commandHandler.registerCommand(CommandDumpRegistry)
    commandHandler.registerCommand(CommandOreDistribution)
    onServerStarting.trigger(event)
  }

  @EventHandler
  def serverStopping(event: FMLServerStoppingEvent) {
    onServerStopping.trigger(event)
  }
}
