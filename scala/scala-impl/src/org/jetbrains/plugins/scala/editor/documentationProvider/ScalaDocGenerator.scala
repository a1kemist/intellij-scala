package org.jetbrains.plugins.scala.editor.documentationProvider

import com.intellij.codeInsight.javadoc.JavaDocInfoGenerator
import com.intellij.lang.documentation.DocumentationMarkup
import com.intellij.psi._
import com.intellij.psi.javadoc.PsiDocComment
import org.apache.commons.lang.StringEscapeUtils.escapeHtml
import org.jetbrains.plugins.scala.editor.documentationProvider.ScalaDocumentationUtils.EmptyDoc
import org.jetbrains.plugins.scala.editor.documentationProvider.ScaladocWikiProcessor.WikiProcessorResult
import org.jetbrains.plugins.scala.editor.documentationProvider.extensions.PsiMethodExt
import org.jetbrains.plugins.scala.extensions.{&&, PsiClassExt, PsiElementExt, PsiMemberExt, PsiNamedElementExt}
import org.jetbrains.plugins.scala.lang.psi
import org.jetbrains.plugins.scala.lang.psi.HtmlPsiUtils
import org.jetbrains.plugins.scala.lang.psi.api.base.ScAnnotationsHolder
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScBindingPattern
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.api.statements._
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter
import org.jetbrains.plugins.scala.lang.psi.api.toplevel._
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.templates.{ScExtendsBlock, ScTemplateParents}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScDocCommentOwner, ScMember, ScObject, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.types.api.presentation.AccessModifierRenderer.AccessQualifierRenderer
import org.jetbrains.plugins.scala.lang.psi.types.api.presentation.TextEscaper.Html
import org.jetbrains.plugins.scala.lang.psi.types.api.presentation.TypeAnnotationRenderer.ParameterTypeDecorateOptions
import org.jetbrains.plugins.scala.lang.psi.types.api.presentation._
import org.jetbrains.plugins.scala.lang.psi.types.{ScParameterizedType, ScType, TypePresentationContext}
import org.jetbrains.plugins.scala.lang.scaladoc.psi.api.ScDocComment
import org.jetbrains.plugins.scala.project.ProjectContext

object ScalaDocGenerator {

  def generateDoc(elementWithDoc: PsiElement, originalElement: Option[PsiElement]): String = {
    val e = elementWithDoc.getNavigationElement

    implicit val projectContext: ProjectContext = e.projectContext
    implicit def typeRenderer: TypeRenderer = {
      val presentableContext = originalElement.fold(TypePresentationContext.emptyContext)(TypePresentationContext.psiElementPresentationContext)
      _.urlText(presentableContext)
    }

    val builder = new HtmlBuilderWrapper
    import builder._

    def appendDefinitionSection(mainPart: => Unit): Unit = {
      append(DocumentationMarkup.DEFINITION_START)
      mainPart
      append(DocumentationMarkup.DEFINITION_END)
    }

    def appendDefinitionSectionWithComment(docOwner: PsiDocCommentOwner)(mainPart: => Unit): Unit = {
      appendDefinitionSection(mainPart)
      append(parseDocComment(docOwner))
    }

    def appendContainingClass(elem: ScMember): Unit =
      parseClassUrl(elem) match {
        case Some(psiLink) =>
          append(psiLink)
          appendNl()
        case _ =>
      }

    def appendDeclMainSection(element: PsiElement): Unit =
      appendDeclMainSection2(element, element)

    def appendDeclMainSection2(element: PsiElement, keywordOwner: PsiElement): Unit = {
      element match {
        case an: ScAnnotationsHolder =>
          val annotationsRendered = annotationsRenderer.renderAnnotations(an)
          append(annotationsRendered)
        case _ =>
      }

      //        val start = length

      element match {
        case m: ScModifierListOwner =>
          val renderer = new ModifiersRenderer(new AccessModifierRenderer(AccessQualifierRenderer.WithHtmlPsiLink))
          append(renderer.render(m))
        case _ =>
      }

      append(ScalaDocumentationUtils.getKeyword(keywordOwner))

      b {
        append(element match {
          case named: ScNamedElement => escapeHtml(named.name)
          case value: ScValueOrVariable => escapeHtml(value.declaredNames.head) // TODO
          case _ => "_"
        })
      }

      element match {
        case tpeParamOwner: ScTypeParametersOwner =>
          val renderer = new TypeParamsRenderer(typeRenderer, new TypeBoundsRenderer(Html))
          append(renderer.renderParams(tpeParamOwner))
        case _ =>
      }

      element match {
        case params: ScParameterOwner =>
          val renderer = definitionParamsRenderer(typeRenderer)
          // TODO: since SCL-13777 spaces are effectively not used! cause we remove all new lines and spaces after rendering
          //  review SCL-13777, maybe we should improve formatting of large classes
          //val spaces = length - start - 7
          val paramsRendered = renderer.renderClauses(params).replaceAll("\n\\s*", "")
          append(paramsRendered)
        case _ =>
      }


      val typeAnnotation = element match {
        case _: ScObject              => "" // ignore, object doesn't need type annotation
        case typed: ScTypedDefinition => typeAnnotationRenderer.render(typed)
        case typed: ScValueOrVariable => typeAnnotationRenderer.render(typed)
        case _                        => ""
      }
      append(typeAnnotation)
    }

    def appendTypeDef(typedef: ScTypeDefinition): Unit =
      appendDefinitionSectionWithComment(typedef) {
        val path = typedef.getPath
        if (path.nonEmpty) {
          append(path)
          appendNl()
        }
        appendDeclMainSection(typedef)
        val extendsListRendered = parseExtendsBlock(typedef.extendsBlock)
        if (extendsListRendered.nonEmpty) {
          appendNl()
          append(extendsListRendered)
        }
      }

    def appendFunction(fun: ScFunction): Unit =
      appendDefinitionSectionWithComment(fun) {
        appendContainingClass(fun)
        appendDeclMainSection(fun)
      }

    def appendTypeAlias(tpe: ScTypeAlias): Unit =
      appendDefinitionSectionWithComment(tpe) {
        appendContainingClass(tpe)
        appendDeclMainSection(tpe)
        tpe match {
          case definition: ScTypeAliasDefinition =>
            val tp = definition.aliasedTypeElement.flatMap(_.`type`().toOption).getOrElse(psi.types.api.Any)
            append(s" = ${typeRenderer(tp)}")
          case _ =>
        }
      }

    def appendValOrVar(decl: ScValueOrVariable): Unit =
      appendDefinitionSectionWithComment(decl) {
        decl match {
          case decl: ScMember =>
            appendContainingClass(decl)
          case _ =>
        }
        appendDeclMainSection(decl)
      }

    def appendBindingPattern(pattern: ScBindingPattern): Unit =
      pattern.nameContext match {
        case (definition: ScValueOrVariable) && (_: ScPatternDefinition | _: ScVariableDefinition) =>
          appendDefinitionSectionWithComment(definition) {
            appendContainingClass(definition)
            appendDeclMainSection2(pattern, definition)
          }
        case _                           =>
      }

    // TODO: it should contain description of the parameter from the scaladoc
    def appendParameter(param: ScParameter): Unit =
      appendDefinitionSection {
        appendDeclMainSection(param)
      }

    html {
      body {
        e match {
          case typeDef: ScTypeDefinition => appendTypeDef(typeDef)
          case fun: ScFunction           => appendFunction(fun)
          case tpe: ScTypeAlias          => appendTypeAlias(tpe)
          case decl: ScValueOrVariable   => appendValOrVar(decl)
          case pattern: ScBindingPattern => appendBindingPattern(pattern)
          case param: ScParameter        => appendParameter(param)
          case _                         =>
        }
      }
    }

    val result = builder.result()
    result
  }

  private def definitionParamsRenderer(implicit typeRenderer: TypeRenderer): ParametersRenderer = {
    val parameterRenderer = new ParameterRenderer(
      typeRenderer,
      ModifiersRenderer.WithHtmlPsiLink,
      typeAnnotationRenderer(typeRenderer),
      TextEscaper.Html,
      withMemberModifiers = false,
      withAnnotations = true
    )
    new ParametersRenderer(
      parameterRenderer,
      renderImplicitModifier = true,
      clausesSeparator = "",
    )
  }

  private def typeAnnotationRenderer(implicit typeRenderer: TypeRenderer): TypeAnnotationRenderer =
    new TypeAnnotationRenderer(typeRenderer, ParameterTypeDecorateOptions.DecorateAll)

  private def annotationsRenderer(implicit typeRenderer: TypeRenderer): AnnotationsRendererLike =
    new AnnotationsRenderer(typeRenderer, "\n", TextEscaper.Html) {
      override def shouldSkipArguments(annotationType: ScType, arguments: Seq[ScExpression]): Boolean =
        arguments.isEmpty || isThrowsAnnotationConstructor(annotationType, arguments)

      // see SCL-17608
      private def isThrowsAnnotationConstructor(annotationType: ScType, arguments: Seq[ScExpression]): Boolean =
        if (arguments.size == 1) {
          //assuming that @throws annotation has single constructor with parametrized type which accepts java.lang.Class
          annotationType.extractClass.exists { clazz =>
            clazz.qualifiedName == "scala.throws" &&
              arguments.head.`type`().exists(_.isInstanceOf[ScParameterizedType])
          }
        } else false
    }

  private def parseClassUrl(elem: ScMember): Option[String]= {
    val clazz = elem.containingClass
    if (clazz == null) None
    else Some(HtmlPsiUtils.classFullLink(clazz))
  }

  private def parseExtendsBlock(elem: ScExtendsBlock)
                               (implicit typeToString: TypeRenderer): String = {
    val buffer: StringBuilder = new StringBuilder()
    elem.templateParents match {
      case Some(x: ScTemplateParents) =>
        val seq = x.allTypeElements
        buffer.append(typeToString(seq.head.`type`().getOrAny))
        if (seq.length > 1) {
          buffer.append("\n")
          for (i <- 1 until seq.length) {
            if (i > 1)
              buffer.append(" ")
            buffer.append("with ")
            buffer.append(typeToString(seq(i).`type`().getOrAny))
          }
        }
      case None =>
        if (elem.isUnderCaseClass) {
          buffer.append(HtmlPsiUtils.psiElementLink("scala.Product", "Product"))
        }
    }

    if (buffer.isEmpty) EmptyDoc
    else "extends " + buffer
  }

  // TODO: strange naming.. not "parse", it not only parses but also resolves base
  private def parseDocComment(potentialDocOwner: PsiDocCommentOwner): String =
    findActualComment(potentialDocOwner).fold(EmptyDoc) { case (docOwner, docComment, isInherited) =>
      parseDocComment(docOwner, docComment, isInherited)
    }

  private def findActualComment(docOwner: PsiDocCommentOwner): Option[(PsiDocCommentOwner, PsiDocComment, Boolean)] =
    docOwner.getDocComment match {
      case null =>
        superElementWithDocComment(docOwner) match {
          case Some((base, baseComment)) =>
            Some((base, baseComment, true))
          case _ =>
            None
        }
      case docComment =>
        Some((docOwner, docComment, false))
    }

  private def parseDocComment(
    docOwner: PsiDocCommentOwner,
    docComment: PsiDocComment,
    isInherited: Boolean
  ): String = {
    val commentParsed = (docOwner, docComment) match {
      case (owner: ScDocCommentOwner, doc: ScDocComment) => generateScalaDocInfoContent(owner, doc)
      case _                                             => generateJavaDocInfoContent(docOwner)
    }
    if (isInherited)
      wrapWithInheritedDescription(docOwner.containingClass)(commentParsed)
    else
      commentParsed
  }

  private def superElementWithDocComment(docOwner: PsiDocCommentOwner) =
    docOwner match {
      case method: PsiMethod => superMethodWithDocComment(method)
      case _                 => None
    }

  private def superMethodWithDocComment(method: PsiMethod): Option[(PsiMethod, PsiDocComment)] =
    method.superMethods.map(base => (base, base.getDocComment)).find(_._2 != null)

  def generateScalaDocInfoContent(
    docCommentOwner: ScDocCommentOwner,
    docComment: ScDocComment
  ): String = {
    val WikiProcessorResult(content, sections) = ScaladocWikiProcessor.fooTODO(docCommentOwner, docComment)
    val buffer = StringBuilder.newBuilder
    if (content.nonEmpty) {
      buffer
        .append(DocumentationMarkup.CONTENT_START)
        .append(content).append("<p>")
        .append(DocumentationMarkup.CONTENT_END)
    }
    if (sections.nonEmpty) {
      buffer.append(DocumentationMarkup.SECTIONS_START)
      appendSections(sections, buffer)
      buffer.append(DocumentationMarkup.SECTIONS_END)
    }
    buffer.result
  }

  def generateRenderedScalaDocContent(
    docCommentOwner: PsiDocCommentOwner,
    docComment: ScDocComment
  ): String = ???
//    prepareFakeJavaElementWithComment(docCommentOwner, docComment) match {
//      case Some((javaElement, sections)) =>
//        val javaDoc = generateRenderedJavaDocInfo(javaElement)
//        val result = insertCustomSections(javaDoc, sections)
//        result
//      case _ => ""
//    }

  private def generateRenderedJavaDocInfo(element: PsiElement): String = {
    val generator = new JavaDocInfoGenerator(element.getProject, element)
    generator.generateRenderedDocInfo
  }

  private def appendSections(sections: Seq[ScaladocWikiProcessor.Section], result: StringBuilder): Unit =
    sections.foreach { section =>
      import DocumentationMarkup._
      result
        .append(SECTION_HEADER_START)
        .append(section.title)
        .append(SECTION_SEPARATOR)
        .append(section.content)
        .append(SECTION_END)
    }

  private def generateJavaDocInfoContent(element: PsiElement): String = {
    val javadoc = generateJavaDocInfo(element)
    val javadocContent = extractJavaDocContent(javadoc)
    javadocContent
  }

  private def generateJavaDocInfo(element: PsiElement): String = {
    val builder = new java.lang.StringBuilder()
    val generator = new JavaDocInfoGenerator(element.getProject, element)
    generator.generateDocInfoCore(builder, false)
    builder.toString
  }

  // TODO: this is far from perfect to rely on text... =(
  //  dive deep into Javadoc generation and implement in a more safe and structural way
  private def extractJavaDocContent(javadoc: String): String = {
    val contentStartIdx = javadoc.indexOf(DocumentationMarkup.CONTENT_START) match {
      case -1 => javadoc.indexOf(DocumentationMarkup.SECTIONS_START)
      case idx => idx
    }
    if (contentStartIdx > 0) javadoc.substring(contentStartIdx)
    else javadoc
  }

  private def wrapWithInheritedDescription(clazz: PsiClass)(text: String): String = {
    val prefix =
      s"""${DocumentationMarkup.CONTENT_START}
         |<b>Description copied from class: </b>
         |${HtmlPsiUtils.psiElementLink(clazz.qualifiedName, clazz.name)}
         |${DocumentationMarkup.CONTENT_END}""".stripMargin
    prefix + text
  }
}
