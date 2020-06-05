package org.jetbrains.plugins.scala.lang.psi

import com.intellij.psi.PsiClass
import org.apache.commons.lang.StringEscapeUtils.escapeHtml
import org.jetbrains.plugins.scala.extensions.{PsiClassExt, PsiNamedElementExt}

/** see [[com.intellij.codeInsight.documentation.DocumentationManagerProtocol]] */
object HtmlPsiUtils {

  def psiElementLink(fqn: String, content: String): String = {
    val href = psiElementHref(fqn)
    val escapedContent = escape(content)
    val contentWrapped = s"""<code>$escapedContent</code>"""
    s"""<a href="$href">$contentWrapped</a>"""
  }

  def psiElementHref(fqn: String): String =
    s"psi_element://${escape(fqn)}"

  def classLink(clazz: PsiClass): String =
    psiElementLink(clazz.qualifiedName, clazz.name)

  def classLinkSafe(clazz: PsiClass): Option[String] =
    Option(clazz.qualifiedName).map(psiElementLink(_, clazz.name))

  def classFullLink(clazz: PsiClass): String = {
    val qualifiedName = clazz.qualifiedName
    psiElementLink(qualifiedName, qualifiedName)
  }

  def escape(text: String): String = escapeHtml(text)
}
