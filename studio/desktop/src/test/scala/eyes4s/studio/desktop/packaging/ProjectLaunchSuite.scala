/*
 * Copyright 2026 canardlapin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package eyes4s.studio.desktop.packaging

class ProjectLaunchSuite extends munit.FunSuite:
  test("early OS events and CLI paths survive until the host is attached") {
    val launch = ProjectLaunch()
    var seen   = Vector.empty[String]
    launch.submit(Vector("/tmp/first project.eyes", "/tmp/second.eyes"))
    launch.attach(path => seen :+= path)
    launch.submit(Vector("/tmp/later.eyes"))
    assertEquals(seen, Vector("/tmp/first project.eyes", "/tmp/second.eyes", "/tmp/later.eyes"))
  }

  test("reattaching does not replay old requests; shutdown releases the host") {
    val launch = ProjectLaunch()
    var old    = Vector.empty[String]
    var next   = Vector.empty[String]
    launch.attach(path => old :+= path)
    launch.submit(Vector("one.eyes"))
    launch.detach()
    launch.submit(Vector("two.eyes"))
    launch.attach(path => next :+= path)
    launch.attach(path => next :+= path)
    assertEquals(old, Vector("one.eyes"))
    assertEquals(next, Vector("two.eyes"))
  }

  test(
    "the pinned Glass bridge forwards quit and appearance; future events cannot silently disappear"
  ) {
    import com.sun.glass.ui.{Application as Glass}
    var quit     = false
    var changed  = false
    var paths    = Vector.empty[String]
    val previous = new Glass.EventHandler:
      override def handleQuitAction(app: Glass, time: Long): Unit = quit = true
      override def handlePreferencesChanged(values: java.util.Map[String, Object]): Unit =
        changed = true
    val bridge = MacProjectFiles.Forwarding(Some(previous), files => paths ++= files)
    bridge.handleQuitAction(null, 1L)
    bridge.handlePreferencesChanged(java.util.Map.of())
    bridge.handleOpenFilesAction(null, 2L, Array("/tmp/é sample.eyes"))
    assert(quit && changed)
    assertEquals(paths, Vector("/tmp/é sample.eyes"))
    val upstream    = classOf[Glass.EventHandler].getDeclaredMethods.map(_.getName).toSet
    val implemented =
      classOf[MacProjectFiles.Forwarding].getDeclaredMethods.map(_.getName).toSet
    assertEquals(upstream.diff(implemented), Set.empty[String])
  }
