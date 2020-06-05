package org.jetbrains.plugins.scala.editor.documentationProvider

import com.intellij.codeInsight.javadoc.JavaDocUtil
import com.intellij.psi.impl.source.tree.LeafPsiElement
import com.intellij.psi.javadoc.PsiDocComment
import com.intellij.psi.{PsiClass, PsiElement}
import org.apache.commons.lang.StringEscapeUtils
import org.apache.commons.lang3.StringUtils
import org.jetbrains.plugins.scala.extensions.{IteratorExt, ObjectExt, PsiClassExt, PsiElementExt, PsiMemberExt, PsiNamedElementExt}
import org.jetbrains.plugins.scala.lang.psi.HtmlPsiUtils
import org.jetbrains.plugins.scala.lang.psi.api.base.ScStableCodeReference
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunction, ScTypeAlias}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScDocCommentOwner, ScTemplateDefinition}
import org.jetbrains.plugins.scala.lang.scaladoc.lexer.ScalaDocTokenType
import org.jetbrains.plugins.scala.lang.scaladoc.lexer.docsyntax.ScaladocSyntaxElementType
import org.jetbrains.plugins.scala.lang.scaladoc.parser.parsing.MyScaladocParsing
import org.jetbrains.plugins.scala.lang.scaladoc.psi.api.{ScDocComment, ScDocSyntaxElement, ScDocTag, ScDocTagValue}

import scala.collection.TraversableOnce
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

// TODO: Extract to bundle all section names
// TODO: add links to @see tag: SCL-9520
// TODO: remove maximum common indentation from code examples {{{ }}} not to shift it far to the right
// TODO: review escaping of parameter names/type parameters/throws class names/returns...
//  see com.intellij.openapi.util.text.StringUtil.replaceUnicodeEscapeSequences
// TODO: from https://docs.scala-lang.org/overviews/scaladoc/for-library-authors.html#markup
//  Comment Inheritance /Implicit
//  If a comment is not provided for an entity at the current inheritance level, but is supplied for the overridden entity at a higher level
//  in the inheritance hierarchy, the comment from the super-class will be used.
//  Likewise if @param, @tparam, @return and other entity tags are omitted but available from a superclass, those comments will be used.
private object ScaladocWikiProcessor {

  final case class WikiProcessorResult(content: String, sections: Seq[Section])
  final case class Section(title: String, content: String)

  def fooTODO(commentOwner: ScDocCommentOwner, comment: ScDocComment): WikiProcessorResult = {
    implicit val macroFinder: MacroFinderImpl = new MacroFinderImpl(commentOwner, getDefineTagInnerNodeText)
    implicit val context: PsiElement = comment

    val contentBuffer = StringBuilder.newBuilder
    generateContent(comment, contentBuffer)

    val tags = comment.tags.toArray // TODO: remove toArray conversion

    tags.find(_.name == MyScaladocParsing.INHERITDOC_TAG) match {
      case Some(inheritDocTag) =>
        addInheritedDocText(commentOwner, contentBuffer)
        if (StringUtils.isNotBlank(contentBuffer))
          contentBuffer.append("<br>")
        visitNode(inheritDocTag, contentBuffer)
      case None        =>
    }

    val content = contentBuffer.result
    val sections = buildSections(tags)
    WikiProcessorResult(content, sections)
  }

  private def buildSections(tags: Array[ScDocTag])
                           (implicit macroFinder: MacroFinder, context: PsiElement): Seq[Section] = {
    val sections = ArrayBuffer.empty[Section]

    sections ++= prepareSimpleSections(tags, MyScaladocParsing.DEPRECATED_TAG, "Deprecated")

    val paramsSection     = prepareParamsSection(tags)
    val typeParamsSection = prepareTypeParamsSection(tags)
    val returnsSection    = prepareReturnsSection(tags)
    val throwsSection     = prepareThrowsSection(tags)

    sections ++=
      paramsSection ++=
      typeParamsSection ++=
      returnsSection ++=
      throwsSection

    sections ++=
      prepareSimpleSections(tags, MyScaladocParsing.NOTE_TAG, "Note:") ++=
      prepareSimpleSections(tags, MyScaladocParsing.EXAMPLE_TAG, "Example:") ++=
      prepareSimpleSections(tags, MyScaladocParsing.SEE_TAG, "See also:") ++=
      prepareSimpleSections(tags, MyScaladocParsing.SINCE_TAG, "Since:") ++=
      prepareSimpleSections(tags, MyScaladocParsing.TODO_TAG, "Todo:")

    sections
  }

  private def prepareSimpleSections(tags: Array[ScDocTag], tagName: String, sectionTitle: String)
                                   (implicit macroFinder: MacroFinder, context: PsiElement): Seq[Section] = {
    val matchingTags = tags.filter(_.name == tagName)
    matchingTags.map { tag =>
      val sectionContent = nodesText(tag.children)
      Section(sectionTitle, sectionContent.trim)
    }
  }

  private def prepareParamsSection(tags: Array[ScDocTag])
                                  (implicit macroFinder: MacroFinder, context: PsiElement) = {
    val paramTags = tags.filter(_.name == MyScaladocParsing.PARAM_TAG)
    val paramTagsInfo = paramTags.flatMap(parameterInfo(_))
    if (paramTagsInfo.nonEmpty) {
      val content = parameterInfosText(paramTagsInfo)
      Some(Section("Params:", content))
    } else None
  }

  private def prepareTypeParamsSection(tags: Array[ScDocTag])
                                      (implicit macroFinder: MacroFinder, context: PsiElement) = {
    val typeParamTags = tags.filter(_.name == MyScaladocParsing.TYPE_PARAM_TAG)
    val typeParamTagsInfo = typeParamTags.flatMap(parameterInfo(_))
    if (typeParamTagsInfo.nonEmpty) {
      val content = parameterInfosText(typeParamTagsInfo)
      Some(Section("Type parameters:", content))
    } else None
  }

  private def prepareReturnsSection(tags: Array[ScDocTag])
                                   (implicit macroFinder: MacroFinder, context: PsiElement) = {
    // TODO: if there is inherited doc, get return description from there
    val returnTag = tags.find(_.name == MyScaladocParsing.RETURN_TAG)
    returnTag.map(innerContentText(_)).map(Section("Returns:", _))
  }

  private def prepareThrowsSection(tags: Array[ScDocTag])
                                  (implicit macroFinder: MacroFinder, context: PsiElement) = {
    val throwTags      = tags.filter(_.name == MyScaladocParsing.THROWS_TAG)
    val throwTagsInfos = throwTags.flatMap(throwsInfo(_))
    if (throwTagsInfos.nonEmpty) {
      val content = parameterInfosText(throwTagsInfos)
      Some(Section("Throws:", content))
    } else None
  }

  private def parameterInfo(tag: ScDocTag)
                           (implicit macroFinder: MacroFinder, context: PsiElement): Option[ParamInfo] =
    tag.children.findByType[ScDocTagValue].map { tagValue =>
      val tagDescription = StringBuilder.newBuilder
      tagValue.nextSiblings.foreach(visitNode(_, tagDescription))
      ParamInfo(tagValue.getText, tagDescription.result())
    }

  private def throwsInfo(tag: ScDocTag)(implicit macroFinder: MacroFinder, context: PsiElement): Option[ParamInfo] = {
    val exceptionRef = tag.children.findByType[ScStableCodeReference]
    exceptionRef.map { ref =>
      val value = generatePsiElementLink(ref, context)
      val description = nodesText(ref.nextSiblings)
      ParamInfo(value, description)
    }
  }

  private def parameterInfosText(infos: Seq[ParamInfo]): String =
    infos.map(p => s"${p.value} &ndash; ${p.description.trim}").mkString("<p>")

  // e.g. @throws Exception(value) condition(description)
  private case class ParamInfo(value: String, description: String)

  private def generatePsiElementLink(ref: ScStableCodeReference, context: PsiElement): String = {
    def refText = ref.getText.trim
    val result = for {
      resolved <- Option(ref.resolve())
      qualifiedName <- qualifiedNameForElement(resolved)
    } yield {
      val label = resolved match {
        case clazz: PsiClass        => JavaDocUtil.getShortestClassName(clazz, context)
        case _                      => refText
      }
      HtmlPsiUtils.psiElementLink(qualifiedName, label)
    }

    result.getOrElse(unresolvedReference(refText))
  }

  private def unresolvedReference(text: String): String =
    s"<font color=red>$text</font>"

  private def qualifiedNameForElement(element: PsiElement): Option[String] =
    element match {
      case clazz: PsiClass    => Option(clazz.qualifiedName)
      case alias: ScTypeAlias => alias.qualifiedNameOpt
      case _                  => None
    }

  private def generateContent(comment: PsiDocComment, buffer: StringBuilder)
                             (implicit macroFinder: MacroFinder): Unit = {
    val contentElements = comment.getDescriptionElements
    contentElements.foreach(visitNode(_, buffer)(macroFinder, comment))
  }

  private def addInheritedDocText(commentOwner: ScDocCommentOwner, buffer: StringBuilder)
                                 (implicit macroFinder: MacroFinder): Unit = {
    val superCommentOpt = commentOwner match {
      case fun: ScFunction             => fun.superMethod.flatMap(_.getDocComment.toOption)
      case clazz: ScTemplateDefinition => clazz.supers.headOption.flatMap(_.getDocComment.toOption)
      case _                           => None
    }

    superCommentOpt.foreach { superComment =>
      // TODO: should we handle inherited tags somehow?
      val macroFinder = new MacroFinderImpl(commentOwner, getDefineTagInnerNodeText)
      if (StringUtils.isNotBlank(buffer))
        buffer.append("<br>")
      generateContent(superComment, buffer)(macroFinder)
    }
  }

  private def innerContentText(element: PsiElement)
                              (implicit macroFinder: MacroFinder, context: PsiElement): String = {
    val buffer = StringBuilder.newBuilder
    visitNode(element, buffer)
    buffer.result
  }

  // TODO: we currently do not support recursive macro
  private def getDefineTagInnerNodeText(element: PsiElement): String =
    innerContentText(element)(new MacroFinderDummy, element)

  private def nodesText(elements: TraversableOnce[PsiElement])
                       (implicit macroFinder: MacroFinder, context: PsiElement): String = {
    val buffer = StringBuilder.newBuilder
    elements.foreach(visitNode(_, buffer))
    buffer.result
  }

  private def visitNodes(elements: TraversableOnce[PsiElement], buffer: StringBuilder)
                        (implicit macroFinder: MacroFinder, context: PsiElement): Unit =
    elements.foreach(visitNode(_, buffer))

  private def visitNode(element: PsiElement, buffer: StringBuilder)
                       (implicit macroFinder: MacroFinder, context: PsiElement): Unit = {
    val isLeafNode = element.getFirstChild == null
    if (isLeafNode)
      visitLeafNode(element, buffer)
    else element match {
      case syntax: ScDocSyntaxElement =>
        visitSyntaxNode(syntax, buffer)
      case _ =>
        element.children.foreach(visitNode(_, buffer))
    }
  }

  private def visitSyntaxNode(syntax: ScDocSyntaxElement, buffer: StringBuilder)
                             (implicit macroFinder: MacroFinder, context: PsiElement): Unit = {
    val markupTagElement = syntax.firstChild.filter(_.elementType.isInstanceOf[ScaladocSyntaxElementType])
    val markupTag = markupTagElement.map(_.getText)
    if (markupTag.contains("[[")) {
      buffer.append(generateLink(syntax))
    } else {
      markupTag.flatMap(markupTagToHtmlTag) match {
        case Some(htmlTag) =>
          buffer.append(s"<$htmlTag>")
          visitNodes(syntax.children.filter(isMarkupInner), buffer)
          buffer.append(s"</$htmlTag>")
        case None          =>
          visitNodes(syntax.children, buffer)
      }
    }
  }

  // wrong markup can contain no closing tag e.g. `__text` (it's wrong but we handle anyway and show inspection)
  private def isMarkupInner(child: PsiElement): Boolean =
    child match {
      case _: LeafPsiElement => !child.elementType.isInstanceOf[ScaladocSyntaxElementType]
      case _                 => true
    }

  private def generateLink(linkElement: ScDocSyntaxElement)
                          (implicit macroFinder: MacroFinder, context: PsiElement): String = {
    val firstChild = linkElement.getFirstChild

    val isHttpLink = firstChild.elementType == ScalaDocTokenType.DOC_HTTP_LINK_TAG
    if (isHttpLink)
      generateHttpLink(linkElement)
    else
      firstChild.getNextSibling match {
        case ref: ScStableCodeReference => generatePsiElementLink(ref, context)
        case _                          =>
          val innerText = nodesText(linkElement.children.filter(isMarkupInner))
          unresolvedReference(innerText)
      }
  }

  private def generateHttpLink(linkElement: ScDocSyntaxElement)
                              (implicit macroFinder: MacroFinder, context: PsiElement): String = {
    // NOTE: all the hacks are required due to how currently parser works, ideally
    val firstChild = linkElement.getFirstChild
    val nextChild = firstChild.getNextSibling
    val linkText = nextChild.getText
    val (href, labelStart) = linkText.indexWhere(_.isWhitespace) match {
      case -1       => (linkText.trim, linkText.trim)
      case spaceIdx => (linkText.substring(0, spaceIdx).trim, linkText.substring(spaceIdx + 1))
    }
    val labelTailText = nodesText(nextChild.nextSiblings.filter(isMarkupInner))
    val label = (labelStart + labelTailText).trim
    htmlLink(href, if (label.nonEmpty) label else href)
  }

  private def htmlLink(href: String, label: String): String = {
    import StringEscapeUtils.escapeHtml
    s"""<a href="${escapeHtml(href)}">$label</a>""".stripMargin
  }

  private def visitLeafNode(
    element: PsiElement,
    result: StringBuilder
  )(implicit macroFinder: MacroFinder): Unit = {
    val elementText = element.getText
    val elementType = element.getNode.getElementType
    elementType match {
      case ScalaDocTokenType.DOC_COMMENT_LEADING_ASTERISKS => // skip, these can come from tags content
      case ScalaDocTokenType.DOC_TAG_NAME => // skip
      case ScalaDocTokenType.DOC_TAG_VALUE_TOKEN => // skip
      case ScalaDocTokenType.DOC_INNER_CODE_TAG       => result.append("<pre><code>")
      case ScalaDocTokenType.DOC_INNER_CLOSE_CODE_TAG => result.append("</code></pre>")
      case ScalaDocTokenType.DOC_COMMENT_DATA         => result.append(elementText)
      case ScalaDocTokenType.DOC_MACROS               =>
        val macroValue = Try(macroFinder.getMacroBody(elementText.stripPrefix("$"))).toOption.flatten
        result.append(macroValue.getOrElse(s"[Cannot find macro: $elementText]"))
      case ScalaDocTokenType.DOC_WHITESPACE if elementText.contains("\n") =>
        // if it's trailing new line (new line between doc lines), do not add leading whitespaces
        // (in case the comment is intended far to the right), just add simple indent
        result.append("\n ")
      case _ =>
        result.append(elementText)
    }
  }

  private def markupTagToHtmlTag(markupTag: String): Option[String] = markupTag match {
    case "__"  => Some("u")
    case "'''" => Some("b")
    case "''"  => Some("i")
    case "`"   => Some("tt")
    case ",,"  => Some("sub")
    case "^"   => Some("sup")
    case h if h.nonEmpty && h.forall(_ == '=') =>
      // TODO: looks like <h1>, <h2>... tags do not work properly in the platform
      Some(s"h${h.length}")
    case _ => None
  }
}
