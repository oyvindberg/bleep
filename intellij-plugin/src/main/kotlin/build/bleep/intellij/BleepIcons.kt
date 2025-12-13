package build.bleep.intellij

import com.intellij.openapi.util.IconLoader
import javax.swing.Icon

object BleepIcons {
    @JvmField
    val BLEEP: Icon = IconLoader.getIcon("/icons/bleep.svg", BleepIcons::class.java)
}
