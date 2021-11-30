/*
 * Copyright (c) bdew, 2013 - 2014
 * https://github.com/bdew/bdlib
 *
 * This mod is distributed under the terms of the Minecraft Mod Public
 * License 1.0, or MMPL. Please check the contents of the license located in
 * http://bdew.net/minecraft-mod-public-license/
 */

package net.bdew.lib.power

import net.bdew.lib.data.DataSlotItemStack
import net.bdew.lib.data.base.UpdateKind
import net.bdew.lib.items.ItemUtils

abstract class TileItemProcessor extends TileBaseProcessor {
  val output = DataSlotItemStack("output", this).setUpdate(UpdateKind.SAVE)
  val outputSlots: Seq[Int]

  def isWorking = output :!= null

  def tryFinish(): Boolean = {
    output := ItemUtils.addStackToSlots(output, this, outputSlots, false)
    return output :== null
  }
}
