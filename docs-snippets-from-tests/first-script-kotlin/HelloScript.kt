package scripts

import bleepscript.BleepScript
import bleepscript.Commands
import bleepscript.Started

class HelloScript : BleepScript("hello") {
  override fun run(started: Started, commands: Commands, args: List<String>) {
    val projectCount = started.build().explodedProjects().size
    started.logger().info("This build has $projectCount projects")
    if (args.isEmpty()) {
      started.logger().info("Hello, world!")
    } else {
      started.logger().info("Hello, ${args.joinToString(" ")}!")
    }
  }
}
