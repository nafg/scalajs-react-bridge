package com.payalabs.scalajs.react.bridge

import japgolly.scalajs.react.CtorType.{ChildArg, Children}
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Js
import japgolly.scalajs.react.component.Js.{RawMounted, UnmountedWithRawType}
import japgolly.scalajs.react.vdom.VdomElement

import scala.language.experimental.macros
import scala.reflect.macros.blackbox._
import scala.scalajs.js
import scala.scalajs.js.Dynamic.literal
import js.Dynamic.global
import scala.scalajs.js.{JSON, Object}

/**
 * See project's [README.md](https://github.com/payalabs/scalajs-react-bridge)
 */

abstract class ReactBridgeComponent {
  type Component = ReactBridgeComponent.Component

  def component(componentPrefixes: Array[String],
                componentName: String,
                propsList: List[(String, js.UndefOr[js.Any])],
                keyOption: Option[js.UndefOr[Key]]): Component = {
    val props = literal()
    propsList.foreach { case (k, jsV) =>
      jsV.foreach { v =>
        props.updateDynamic(k)(v)
      }
    }

    val componentFunction = componentPrefixes.foldLeft(global) {
      _.selectDynamic(_)
    }.selectDynamic(componentName)

    val jsComponent = JsComponent[js.Object, Children.Varargs, Null](componentFunction)

    jsComponent.mapCtorType { c =>
      val withProps = c.withProps(props)
      keyOption.fold(withProps)(key => key.fold(withProps)(k => withProps.withKey(k)))
    }
  }
}

object ReactBridgeComponent {

  type Component = Js.ComponentSimple[Object, CtorType.Children, UnmountedWithRawType[Object, Null, RawMounted]]

  // See https://meta.plasm.us/posts/2013/06/21/macro-methods-and-subtypes
  implicit class ReactNativeComponentThisThing[A <: ReactBridgeComponent](val value: A) extends AnyVal {
    def autoConstruct: Component = macro ReactBridgeComponent.autoConstructImpl[A]
  }

  def autoConstructImpl[A <: ReactBridgeComponent : c.WeakTypeTag](c: Context): c.Expr[Component] = {
    import c.universe._

    val tpe = weakTypeTag[A].tpe

    val typeShortName = tpe.typeSymbol.fullName.split('.').last

    val (props, key) = computeParams(c)(tpe)

    val componentNamespace = tpe.baseClasses.flatMap {
      ts => ts.annotations.filter(_.tree.tpe == typeOf[ComponentNamespace])
    }.headOption.map(a => a.tree.children.tail.head.toString.tail.init.split('.')).getOrElse(Array[String]())

    val componentTree =
      q"""
         import com.payalabs.scalajs.react.bridge.JsWriter
         ${c.prefix.tree}.value.component($componentNamespace, $typeShortName, $props, $key)
      """

    c.Expr[Component](componentTree)
  }

  private def computeParams(c: Context)(tpe: c.universe.Type): (List[(String, c.universe.Tree)], Option[c.universe.Tree]) = {
    import c.universe._

    val applyMethod = tpe.members.find(_.name.toString == "apply")

    applyMethod match {
      case None =>
        c.error(NoPosition, "Components that extend ReactComponentBridge must defined the apply method")
        (List(), None)
      case Some(applyMethod) =>
        val params = applyMethod.asMethod.paramLists.head
        val (propsParam, keyParam) = params.partition(_.name.toString != "key")

        val convertedProps = propsParam.map { param =>
          val rawParamType = param.typeSignature
          val converted = {
            if (rawParamType.typeConstructor == typeOf[scala.scalajs.js.UndefOr[Any]].typeConstructor) {
              val paramType = rawParamType.typeArgs.head
              val converter = q"implicitly[JsWriter[$paramType]]"
              q"${param.name.toTermName}.map(v => $converter.toJs(v))"
            } else {
              val paramType = rawParamType
              val converter = q"implicitly[JsWriter[$paramType]]"
              q"$converter.toJs(${param.name.toTermName})"
            }
          }
          (param.name.toString, converted)
        }

        (convertedProps, keyParam.headOption.map(kp => q"${kp.name.toTermName}"))
    }
  }
}
