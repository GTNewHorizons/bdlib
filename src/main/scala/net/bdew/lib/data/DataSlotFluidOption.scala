/*
 * Copyright (c) bdew, 2013 - 2015
 * https://github.com/bdew/bdlib
 *
 * This mod is distributed under the terms of the Minecraft Mod Public
 * License 1.0, or MMPL. Please check the contents of the license located in
 * http://bdew.net/minecraft-mod-public-license/
 */

package net.bdew.lib.data

import net.bdew.lib.data.base.{DataSlotContainer, UpdateKind}
import net.minecraft.nbt.NBTTagCompound
import net.minecraftforge.fluids.{Fluid, FluidRegistry}

case class DataSlotFluidOption(name: String, parent: DataSlotContainer) extends DataSlotOption[Fluid] {
  setUpdate(UpdateKind.SAVE, UpdateKind.GUI)

  override def save(t: NBTTagCompound, kind: UpdateKind.Value) {
    value foreach { x => t.setString(name, x.getName) }
  }

  override def load(t: NBTTagCompound, kind: UpdateKind.Value): Unit = {
    if (t.hasKey(name)) {
      val fluidName = t.getString(name)
      if (FluidRegistry.isFluidRegistered(fluidName)) {
        value = Option(FluidRegistry.getFluid(fluidName))
      } else unset()
    } else unset()
  }
}
