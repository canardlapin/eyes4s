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

package eyes4s.studio.desktop.analysis

import eyes4s.plan.*
import eyes4s.studio.app.text.{RecipeText, RecipeTextId as T}
import eyes4s.studio.app.vm.{A11yRole, FocusStop}
import eyes4s.studio.desktop.shell.DisabledReason
import javafx.scene.{Node, Parent}
import javafx.scene.control.{Button, CheckBox, ComboBox, Label, TextField}
import javafx.scene.layout.VBox
import javafx.util.StringConverter
import scala.jdk.CollectionConverters.*

/** A recursive renderer of served field shapes. There is no recipe field table:
  * a new field using a known shape appears without a desktop change.
  */
final class DescriptorForm(changed: FormValues => Unit):
  private final case class Entry(node: Node, read: () => RawValue)
  private var roots       = Vector.empty[(FieldId, Entry)]
  private var binding     = false
  private var hidden      = FormValues.empty
  private var controls    = Vector.empty[(Node, A11yRole)]
  private var textEntries = Map.empty[String, TextField]
  val node: VBox          = VBox(10)
  node.getStyleClass.add("descriptor-form")
  def textFields: Map[String, TextField] = textEntries
  def values: FormValues                 =
    roots.foldLeft(hidden)((values, entry) => values.updated(entry._1, entry._2.read()))
  def focusStops: Vector[FocusStop] =
    val roles                   = controls.toMap
    def shown(n: Node): Boolean =
      Iterator.iterate(n)(_.getParent).takeWhile(_ != null).forall(_.isVisible)
    def descendants(n: Node): Vector[Node] = n +: (n match
      case parent: Parent =>
        parent.getChildrenUnmodifiable.asScala.toVector.flatMap(descendants)
      case _ => Vector.empty)
    descendants(node).flatMap(control =>
      roles
        .get(control)
        .filter(_ => shown(control) && !control.isDisabled && control.isFocusTraversable)
        .map(role => FocusStop(role, control.getAccessibleText))
    )
  private def edited(): Unit = if !binding then changed(values)
  private def label(text: String, style: String = "t12-label"): Label =
    val result = Label(text)
    result.setWrapText(true)
    result.getStyleClass.add(style)
    result
  private def track[A <: Node](control: A, role: A11yRole, path: String, name: String): A =
    control match
      case _: TextField | _: ComboBox[?] => control.getStyleClass.add("studio-field")
      case _: Button                     => control.getStyleClass.add("studio-button")
      case _                             => ()
    control.setId(s"recipe-$path")
    control.setAccessibleText(name)
    controls :+= (control -> role)
    control

  /** `omit` lets a richer component render a served field, while retaining its raw value. */
  def show(view: FormView, raw: FormValues, omit: Set[String] = Set.empty): Unit =
    binding = true
    try
      roots = Vector.empty
      controls = Vector.empty
      textEntries = Map.empty
      hidden = FormValues.from(raw.values.filter((id, _) => omit(id.value)))
      roots = view.fields.filterNot(v => omit(v.id.value)).map { field =>
        field.id -> entry(field, raw.get(field.id), field.id.value)
      }
      node.getChildren.setAll(roots.map(_._2.node).asJava): Unit
    finally binding = false

  private def initial(view: FieldView): RawValue = view.default.fold[RawValue](view.kind match
    case FieldKind.Numeric(_, _, _)                    => RawValue.Number("")
    case FieldKind.Choice(ChoiceSource.Fixed(options)) =>
      options.headOption.fold[RawValue](RawValue.Absent)(o => RawValue.Choice(o.token))
    case FieldKind.Choice(_)                  => RawValue.Choice("")
    case FieldKind.Toggle                     => RawValue.Flag(false)
    case FieldKind.Text | FieldKind.Reference => RawValue.Text("")
    case FieldKind.Group(parts, _) => RawValue.Group(parts.map(p => p.id -> initial(p)))
    case FieldKind.Optional(_, _)  => RawValue.Absent
    case FieldKind.Repeated(of, minimum, _) => RawValue.Items(Vector.fill(minimum)(initial(of)))
    case FieldKind.Variant(cases)           =>
      cases.headOption.fold[RawValue](RawValue.Absent)(c =>
        RawValue.Variant(c.token, c.parts.map(p => p.id -> initial(p)))
      ))(_.raw)

  private def entry(view: FieldView, raw: RawValue, path: String): Entry =
    val title    = RecipeText(T.Field, view.meaning)
    val metadata = view.kind match
      case FieldKind.Numeric(_, shape, bounds) =>
        s"${view.quantity.symbol} · $shape · ${bounds.render}"
      case _ => view.quantity.symbol
    val heading = label(title)
    val meta    =
      label(RecipeText(T.Metadata, view.id.value, view.version.toString, metadata), "t11")
    val body = control(view, raw, path, title)
    heading.setLabelFor(body.node)
    val box = VBox(3, heading, body.node, meta)
    view.default.foreach(d =>
      box.getChildren.add(label(RecipeText(T.Default, d.raw.toString, d.reason), "t11"))
    )
    Entry(box, body.read)

  private def control(view: FieldView, raw: RawValue, path: String, title: String): Entry =
    def text(read: String => RawValue): Entry =
      val value = raw match
        case RawValue.Number(s) => s
        case RawValue.Text(s)   => s
        case RawValue.Choice(s) => s
        case _                  => ""
      val field = track(TextField(value), A11yRole.TextField, path, title)
      field.getStyleClass.addAll("t12", "mono")
      textEntries = textEntries.updated(path, field)
      field.textProperty.addListener((_, _, _) => edited())
      Entry(field, () => read(field.getText))
    def options(
        items: Vector[ChoiceOption],
        selected: String,
        change: String => Unit
    ): ComboBox[ChoiceOption] =
      val box = track(ComboBox[ChoiceOption](), A11yRole.ComboBox, path, title)
      box.getItems.setAll(items.asJava)
      box.setConverter(new StringConverter[ChoiceOption]:
        def toString(value: ChoiceOption): String =
          Option(value).fold("")(o => RecipeText(T.Served, o.label))
        def fromString(value: String): ChoiceOption = box.getValue)
      items.find(_.token == selected).foreach(box.setValue)
      box.setOnAction(_ => Option(box.getValue).foreach(o => change(o.token)))
      box
    view.kind match
      case FieldKind.Numeric(_, _, _)                  => text(RawValue.Number(_))
      case FieldKind.Text                              => text(RawValue.Text(_))
      case FieldKind.Choice(ChoiceSource.FromInput(_)) => text(RawValue.Choice(_))
      case FieldKind.Choice(ChoiceSource.Fixed(items)) =>
        val selected = raw match
          case RawValue.Choice(s) => s
          case _                  => ""
        val box = options(items, selected, _ => edited())
        Entry(
          box,
          () =>
            Option(box.getValue).fold[RawValue](RawValue.Absent)(o => RawValue.Choice(o.token))
        )
      case FieldKind.Toggle =>
        val box = track(CheckBox(title), A11yRole.CheckBox, path, title)
        box.setSelected(raw == RawValue.Flag(true))
        box.setOnAction(_ => edited())
        Entry(box, () => RawValue.Flag(box.isSelected))
      case FieldKind.Reference =>
        val field  = text(RawValue.Text(_))
        val reason = label(RecipeText(T.ReadOnly, title), "t11")
        field.node.setDisable(true)
        DisabledReason.attach(field.node, reason)
        Entry(VBox(3, field.node, reason), () => raw)
      case FieldKind.Group(parts, _) =>
        val held = raw match
          case RawValue.Group(values) => values.toMap
          case _                      => Map.empty[FieldId, RawValue]
        val children = parts.map(p =>
          p.id -> entry(p, held.getOrElse(p.id, RawValue.Absent), s"$path.${p.id.value}")
        )
        val box = VBox(6)
        box.getChildren.setAll(children.map(_._2.node).asJava): Unit
        Entry(box, () => RawValue.Group(children.map((id, child) => id -> child.read())))
      case FieldKind.Optional(inner, absent) =>
        val use = track(
          CheckBox(RecipeText(T.Optional, title)),
          A11yRole.CheckBox,
          path,
          RecipeText(T.Optional, title)
        )
        use.setSelected(raw != RawValue.Absent)
        val child  = entry(inner, if raw == RawValue.Absent then initial(inner) else raw, path)
        val reason = label(RecipeText(T.Served, absent), "t11")
        child.node.setDisable(!use.isSelected)
        DisabledReason.attach(child.node, reason)
        use.setOnAction { _ => child.node.setDisable(!use.isSelected); edited() }
        Entry(
          VBox(4, use, child.node, reason),
          () => if use.isSelected then child.read() else RawValue.Absent
        )
      case FieldKind.Variant(cases) =>
        val token = raw match
          case RawValue.Variant(t, _) => t
          case _                      => cases.headOption.fold("")(_.token)
        var children = Vector.empty[(FieldId, Entry)]
        val box      = VBox(6)
        def show(token: String, parts: Map[FieldId, RawValue]): Unit =
          val oldNodes = children.flatMap(_._2.node.lookupAll("*").asScala).toSet
          controls = controls.filterNot(c => oldNodes(c._1))
          children = cases.find(_.token == token).toVector.flatMap(_.parts).map { p =>
            p.id -> entry(p, parts.getOrElse(p.id, initial(p)), s"$path.${p.id.value}")
          }
          box.getChildren.setAll(children.map(_._2.node).asJava): Unit
        val held = raw match
          case RawValue.Variant(_, values) => values.toMap
          case _                           => Map.empty[FieldId, RawValue]
        val pick = options(
          cases.map(c => ChoiceOption(c.token, c.label)),
          token,
          t => { show(t, Map.empty); edited() }
        )
        show(token, held)
        Entry(
          VBox(4, pick, box),
          () =>
            Option(pick.getValue).fold[RawValue](RawValue.Absent)(c =>
              RawValue.Variant(c.token, children.map((id, child) => id -> child.read()))
            )
        )
      case FieldKind.Repeated(of, minimum, maximum) =>
        var children = Vector.empty[Entry]
        val box      = VBox(6)
        val add      = track(
          Button(RecipeText(T.Add, title)),
          A11yRole.Button,
          s"$path.add",
          RecipeText(T.Add, title)
        )
        val reason = label(RecipeText(T.Served, view.meaning), "t11")
        DisabledReason.attach(add, reason)
        def rebuild(values: Vector[RawValue]): Unit =
          controls = controls.filterNot(c => box.lookupAll("*").contains(c._1))
          children = values.zipWithIndex.map((value, i) => entry(of, value, s"$path.$i"))
          val rows = children.zipWithIndex.map { (child, i) =>
            val remove = track(
              Button(RecipeText(T.Remove, title)),
              A11yRole.Button,
              s"$path.$i.remove",
              RecipeText(T.Remove, title)
            )
            remove.setDisable(children.size <= minimum)
            DisabledReason.attach(remove, reason)
            remove.setOnAction { _ =>
              rebuild(children.map(_.read()).patch(i, Nil, 1)); edited()
            }
            VBox(3, child.node, remove)
          }
          box.getChildren.setAll(rows.asJava)
          add.setDisable(maximum.exists(children.size >= _))
          reason.setText(
            Vector(
              Option.when(children.size <= minimum)(
                RecipeText(T.MinimumItems, minimum.toString)
              ),
              maximum
                .filter(children.size >= _)
                .map(n => RecipeText(T.MaximumItems, n.toString))
            ).flatten.mkString(" ")
          )
        raw match
          case RawValue.Items(values) => rebuild(values)
          case _                      => rebuild(Vector.empty)
        add.setOnAction { _ => rebuild(children.map(_.read()) :+ initial(of)); edited() }
        Entry(VBox(4, box, add, reason), () => RawValue.Items(children.map(_.read())))
