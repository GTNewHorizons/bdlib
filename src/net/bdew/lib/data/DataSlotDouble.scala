/*
 * Copyright (c) bdew, 2013 - 2014
 * https://github.com/bdew/bdlib
 *
 * This mod is distributed under the terms of the Minecraft Mod Public
 * License 1.0, or MMPL. Please check the contents of the license located in
 * http://bdew.net/minecraft-mod-public-license/
 */

package net.bdew.lib.data

import net.bdew.lib.data.base.{DataSlotNumeric, TileDataSlots, UpdateKind}
import net.minecraft.nbt.NBTTagCompound

case class DataSlotDouble(name: String, parent: TileDataSlots, default: Double = 0) extends DataSlotNumeric[Double](default) {
  def save(t: NBTTagCompound, kind: UpdateKind.Value) = t.setDouble(name, cval)
  def load(t: NBTTagCompound, kind: UpdateKind.Value) = cval = t.getDouble(name)
}
