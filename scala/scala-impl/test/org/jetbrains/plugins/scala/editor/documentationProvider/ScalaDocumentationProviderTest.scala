package org.jetbrains.plugins.scala.editor.documentationProvider

import org.locationtech.jts.util.AssertionFailedException

// TODO: in-editor doc: code example in the end of the doc produces new line
class ScalaDocumentationProviderTest extends ScalaDocumentationProviderTestBase {

  def testClass(): Unit =
    doGenerateDocDefinitionTest(
      s"""package a.b.c
         |
         |class ${|}A
         |""".stripMargin,
      s"""a.b.c
         |class <b>A</b>""".stripMargin
    )

  def testClass_TopLevel(): Unit =
    doGenerateDocDefinitionTest(
      s"""class ${|}A""",
      s"""class <b>A</b>"""
    )

  def testClass_WithSuperClass(): Unit =
    doGenerateDocDefinitionTest(
      s"""package a.b.c
         |
         |class ${|}A extends Exception""".stripMargin,
      s"""a.b.c
         |class <b>A</b>
         |extends <a href="psi_element://scala.Exception"><code>Exception</code></a>""".stripMargin
    )

  def testTrait(): Unit =
    doGenerateDocDefinitionTest(
      s"""package a.b.c
         |
         |trait ${|}T
         |""".stripMargin,
      s"""a.b.c
         |trait <b>T</b>""".stripMargin
    )

  def testObject(): Unit =
    doGenerateDocDefinitionTest(
      s"""package a.b.c
         |
         |object ${|}O
         |""".stripMargin,
      s"""a.b.c
         |object <b>O</b>""".stripMargin
    )

  def testTypeAlias(): Unit =
    doGenerateDocDefinitionTest(
      s"""object O {
         |  type ${|}MyType = java.lang.Exception
         |}""".stripMargin,
      s"""<a href="psi_element://O"><code>O</code></a>
         |type <b>MyType</b> = <a href="psi_element://java.lang.Exception"><code>Exception</code></a>""".stripMargin
    )

  def testClass_WithSuperClassAndTraits(): Unit =
    doGenerateDocDefinitionTest(
      s"""package a.b.c
         |trait T1
         |trait T2
         |class ${|}A extends Exception with T1 with T2""".stripMargin,
      s"""a.b.c
         |class <b>A</b>
         |extends <a href="psi_element://scala.Exception"><code>Exception</code></a>
         |with <a href="psi_element://a.b.c.T1"><code>T1</code></a> with <a href="psi_element://a.b.c.T2"><code>T2</code></a>""".stripMargin
    )

  // for not it's not a business requirement just fixing implementation in tests
  def testClassExtendingAnotherClassShouldNotInheritDoc(): Unit =
    doGenerateDocDefinitionTest(
      s"""/** description of A */
         |class A
         |class ${|}B extends A
         |""".stripMargin,
      """class <b>B</b>
        |extends <a href="psi_element://A"><code>A</code></a>""".stripMargin
    )

  def testClassExtendingAnotherJavaClassShouldNotInheritDoc(): Unit = {
    getFixture.addFileToProject("J.java",
      s"""/** description of base class J */
         |class J {}
         |""".stripMargin
    )
    doGenerateDocDefinitionTest(
      s"""class ${|}B extends J""".stripMargin,
      s"""class <b>B</b>
         |extends <a href="psi_element://J"><code>J</code></a>""".stripMargin
    )
  }

  def testClass_WithVariousGenericsWithBounds(): Unit =
    doGenerateDocDefinitionTest(
      s"""trait Trait[A]
         |abstract class ${|}Class[T <: Trait[_ >: Object]]
         |  extends Comparable[_ <: Trait[_ >: String]]""".stripMargin,
      "abstract class <b>Class</b>[T &lt;: <a href=\"psi_element://Trait\"><code>Trait</code></a>" +
        "[_ &gt;: <a href=\"psi_element://java.lang.Object\"><code>Object</code></a>]" +
        "]\n" +
        "extends <a href=\"psi_element://java.lang.Comparable\"><code>Comparable</code></a>" +
        "[_ &lt;: <a href=\"psi_element://Trait\"><code>Trait</code></a>" +
        "[_ &gt;: <a href=\"psi_element://scala.Predef.String\"><code>String</code></a>]]"
    )

  def testMethod(): Unit =
    doGenerateDocBodyTest(
      s"""class A {
         |  /** description of foo */
         |  def ${|}foo: String = ???
         |}""".stripMargin,
      s"""$DefinitionStart<a href="psi_element://A"><code>A</code></a>
         |def <b>foo</b>: <a href="psi_element://scala.Predef.String"><code>String</code></a>$DefinitionEnd
         |$ContentStart description of foo <p>$ContentEnd
         |""".stripMargin
    )

  def testMethod_Overriding(): Unit = {
    getFixture.addFileToProject("BaseScalaClass.scala",
      s"""class BaseScalaClass {
         |  /** description of base method from BaseScalaClass */
         |  def baseMethod: String = ???
         |}
         |""".stripMargin
    )
    doGenerateDocBodyTest(
      s"""class A extends BaseScalaClass {
         |  /** description of base method from A */
         |  def ${|}baseMethod: String = ???
         |}
         |""".stripMargin,
      s"""$DefinitionStart<a href="psi_element://A"><code>A</code></a>
         |def <b>baseMethod</b>: <a href="psi_element://scala.Predef.String"><code>String</code></a>$DefinitionEnd
         |$ContentStart description of base method from A <p>$ContentEnd
         |""".stripMargin
    )
  }

  def testMethod_WithEmptyDoc_Overriding(): Unit = {
    getFixture.addFileToProject("BaseScalaClass.scala",
      s"""class BaseScalaClass {
         |  /** description of base method from BaseScalaClass */
         |  def baseMethod: String = ???
         |}
         |""".stripMargin
    )
    // TODO: do we need override keyword as text in <pre> section?
    //  Java uses `Overrides` section for that (e.g. Overrides: foo in class BaseClass)
    doGenerateDocBodyTest(
      s"""class A extends BaseScalaClass {
         |  override def ${|}baseMethod: String = ???
         |}""".stripMargin,
      s"""$DefinitionStart<a href="psi_element://A"><code>A</code></a>
         |override def <b>baseMethod</b>: <a href="psi_element://scala.Predef.String"><code>String</code></a>$DefinitionEnd
         |$ContentStart
         |<b>Description copied from class: </b>
         |<a href="psi_element://BaseScalaClass"><code>BaseScalaClass</code></a>
         |$ContentEnd
         |$ContentStart
         | description of base method from BaseScalaClass <p>
         |$ContentEnd
         |""".stripMargin
    )
  }

  def testMethod_WithEmptyDoc_OverridingJavaMethod(): Unit = {
    getFixture.addFileToProject("BaseJavaClass.java",
      s"""public class BaseJavaClass {
         |  /** description of base method from BaseJavaClass */
         |  String ${|}baseMethod() { return null; }
         |}
         |""".stripMargin
    )
    doGenerateDocBodyTest(
      s"""class A extends BaseJavaClass {
         |  override def ${|}baseMethod: String = ???
         |}
         |""".stripMargin,
      s"""$DefinitionStart<a href="psi_element://A"><code>A</code></a>
         |override def <b>baseMethod</b>: <a href="psi_element://scala.Predef.String"><code>String</code></a>$DefinitionEnd
         |$ContentStart
         |<b>Description copied from class: </b>
         |<a href="psi_element://BaseJavaClass"><code>BaseJavaClass</code></a>
         |$ContentEnd
         |$ContentStart
         |description of base method from BaseJavaClass <p>
         |$ContentEnd
         |$SectionsStart<p>$SectionsEnd
         |""".stripMargin
    )
  }

  def testMethod_WithEmptyDoc_OverridingJavaMethod_TagsInJavadoc(): Unit = {
    getFixture.addFileToProject("BaseJavaClass.java",
      s"""public class BaseJavaClass {
         |  /** @return modules to compile before run. Empty list to build project */
         |  String[] getModules() {  return null;  }
         |}""".stripMargin
    )
    doGenerateDocBodyTest(
      s"""class A extends BaseJavaClass {
         |  override def ${|}getModules: String = ???
         |}
         |""".stripMargin,
      s"""$DefinitionStart<a href="psi_element://A"><code>A</code></a>
         |override def <b>getModules</b>: <a href="psi_element://scala.Predef.String"><code>String</code></a>$DefinitionEnd
         |$ContentStart
         |<b>Description copied from class: </b>
         |<a href="psi_element://BaseJavaClass"><code>BaseJavaClass</code></a>
         |$ContentEnd
         |$SectionsStart
         |<p><tr>
         |<td valign='top' class='section'><p>Returns:</td>
         |<td valign='top'><p>modules to compile before run. Empty list to build project </td>
         |$SectionsEnd""".stripMargin
    )
  }

  def testMethod_WithAccessModifier(): Unit =
    doGenerateDocDefinitionTest(
      s"""class X {
         |  protected def ${|}f1 = 42
         |}
         |""".stripMargin,
      s"""<a href="psi_element://X"><code>X</code></a>
         |protected def <b>f1</b>: <a href="psi_element://scala.Int"><code>Int</code></a>""".stripMargin
    )

  def testMethod_WithAccessModifierWithThisQualifier(): Unit =
    doGenerateDocDefinitionTest(
      s"""class X {
         |  protected[this] def ${|}f1 = 42
         |}
         |""".stripMargin,
      s"""<a href="psi_element://X"><code>X</code></a>
         |protected[this] def <b>f1</b>: <a href="psi_element://scala.Int"><code>Int</code></a>""".stripMargin
    )

  def testMethod_HigherKindedTypeParameters(): Unit =
    doGenerateDocDefinitionTest(
      s"""object O {
         |  def ${|}f[A[_, B]] = 42
         |}""".stripMargin,
      s"""<a href="psi_element://O"><code>O</code></a>
         |def <b>f</b>[A[_, B]]: <a href="psi_element://scala.Int"><code>Int</code></a>""".stripMargin
    )

  def testMethod_HigherKindedTypeParameters_1(): Unit =
    doGenerateDocDefinitionTest(
      s"""trait ${|}T[X[_, Y[_, Z]]]
         |""".stripMargin,
      """trait <b>T</b>[X[_, Y[_, Z]]]"""
    )

  def testMethod_HigherKindedTypeParameters_ReferToParameterInExtendsList(): Unit = {
    val input1  =
      s"""trait Trait1[A]
         |trait Trait2[A, CC[X] <: Seq[X]]
         |extends Trait1[CC[A]]
         |val ${|}x: Trait2[_, _] = ???""".stripMargin
    val expectedDoc =
      s"""${DefinitionStart}val <b>x</b>: <a href="psi_element://Trait2"><code>Trait2</code></a>[_, _]$DefinitionEnd"""
    doGenerateDocBodyTest(input1, expectedDoc)
  }

  private def sharedValVarDefinition(definitionBody: String): String = {
    s"""class X {
       |  /**
       |   * some description
       |   *
       |   * @note some note
       |   */
       |  $definitionBody
       |}""".stripMargin
  }

  private def doValVarTest(definitionBody: String, expectedDocDefinitionSection: String): Unit =
    doGenerateDocBodyTest(
      sharedValVarDefinition(definitionBody),
      s"""$DefinitionStart$expectedDocDefinitionSection$DefinitionEnd
         |$ContentStart some description <p>$ContentEnd
         |$SectionsStart
         |<tr><td valign='top' class='section'><p>Note:</td><td valign='top'>some note</td>
         |$SectionsEnd""".stripMargin
    )

  def testMemberValue(): Unit =
    doValVarTest(
      s"""val ${|}v = 1""",
      s"""<a href="psi_element://X"><code>X</code></a>\nval <b>v</b>: <a href="psi_element://scala.Int"><code>Int</code></a>"""
    )

  def testMemberVariable(): Unit =
    doValVarTest(
      s"""var ${|}v = 1""",
      s"""<a href="psi_element://X"><code>X</code></a>\nvar <b>v</b>: <a href="psi_element://scala.Int"><code>Int</code></a>"""
    )

  def testMemberValuePattern(): Unit =
    doValVarTest(
      s"""val (v1, ${|}v2) = (1, "str")""",
      s"""<a href="psi_element://X"><code>X</code></a>\nval <b>v2</b>: <a href="psi_element://java.lang.String"><code>String</code></a>"""
    )

  def testMemberValuePattern_1(): Unit =
    doValVarTest(
      s"""val Tuple2(v1, ${|}v2) = (1, "str")""",
      // java.lang.String cause it's an inferred type
      s"""<a href="psi_element://X"><code>X</code></a>\nval <b>v2</b>: <a href="psi_element://java.lang.String"><code>String</code></a>"""
    )

  def testMemberVariablePattern(): Unit =
    doValVarTest(
      s"""var (v1, ${|}v2) = (1, "str")""",
      s"""<a href="psi_element://X"><code>X</code></a>\nvar <b>v2</b>: <a href="psi_element://java.lang.String"><code>String</code></a>"""
    )

  def testMemberVariablePattern_1(): Unit =
    doValVarTest(
      s"""var Tuple2(v1, ${|}v2) = (1, "str")""",
      s"""<a href="psi_element://X"><code>X</code></a>\nvar <b>v2</b>: <a href="psi_element://java.lang.String"><code>String</code></a>"""
    )

  def testMemberValue_Abstract(): Unit =
    doValVarTest(
      s"""val ${|}v: Int""",
      s"""<a href="psi_element://X"><code>X</code></a>\nval <b>v</b>: <a href="psi_element://scala.Int"><code>Int</code></a>"""
    )

  def testMemberVariable_Abstract(): Unit =
    doValVarTest(
      s"""var ${|}v: Int""",
      s"""<a href="psi_element://X"><code>X</code></a>\nvar <b>v</b>: <a href="psi_element://scala.Int"><code>Int</code></a>"""
    )

  def testAllTags(): Unit = {
    // Should only add to sections these tags in the same order:
    // @deprecated @param, @tparam, @return, @throws
    // @example @note @see @since @to-do
    val fileText    =
      s"""class AllTags {
         |  /**
         |   * @see some text
         |   * @author some text
         |   * @note some text
         |   * @since some text
         |   * @define some text
         |   * @version some text
         |   * @todo some text
         |   * @usecase some text
         |   * @example some text
         |   * @deprecated some text
         |   * @migration some text
         |   * @group some text
         |   * @groupname some text
         |   * @groupdesc some text
         |   * @groupprio some text
         |   * @constructor some text
         |   * @return some text
         |   * @tparam T some text
         |   * @throws Exception some text
         |   * @param p some text
         |   */
         |  def ${|}foo[T](p: String) = 42
         |}
         |""".stripMargin

    val expectedDoc =
      s"""<tr><td valign='top' class='section'><p>Deprecated</td>
         |<td valign='top'>some text</td>
         |<tr><td valign='top' class='section'><p>Params:</td>
         |<td valign='top'>p &ndash; some text</td>
         |<tr><td valign='top' class='section'><p>Type parameters:</td>
         |<td valign='top'>T &ndash; some text</td>
         |<tr><td valign='top' class='section'><p>Returns:</td>
         |<td valign='top'> some text</td>
         |<tr><td valign='top' class='section'><p>Throws:</td>
         |<td valign='top'><a href="psi_element://scala.Exception"><code>Exception</code></a> &ndash; some text</td>
         |<tr><td valign='top' class='section'><p>Note:</td>
         |<td valign='top'>some text</td>
         |<tr><td valign='top' class='section'><p>Example:</td>
         |<td valign='top'>some text</td>
         |<tr><td valign='top' class='section'><p>See also:</td>
         |<td valign='top'>some text</td>
         |<tr><td valign='top' class='section'><p>Since:</td>
         |<td valign='top'>some text</td>
         |<tr><td valign='top' class='section'><p>Todo:</td>
         |<td valign='top'>some text</td>
         |""".stripMargin
    doGenerateDocSectionsTest(fileText, expectedDoc)
  }

  //note strong requirement, just fixating current behaviour
  def testTags_AuthorTagShouldBeIgnored(): Unit = {
    val fileText =
      s"""/**
         |  * Description
         |  * @author Some Name 1
         |  * @author Some Name 2
         |  */
         |class ${|}A {}
         |""".stripMargin
    val expectedDoc =
      s"""${DefinitionStart}class <b>A</b>$DefinitionEnd
         |$ContentStart Description <p>$ContentEnd
         |""".stripMargin

    doGenerateDocBodyTest(
      fileText,
      expectedDoc
    )
  }

  def testTags_ParamsForMethod(): Unit = {
    val fileText =
      s"""/**
         | * @param i aaa
         | * @param j bbb
         | * @param k ccc
         | * @tparam T ddd
         | * @tparam E eee
         | *           ggg
         | */
         |def ${|}f[T, E](i: Int, j: Int, k: String) {}
         |""".stripMargin
    val expectedDoc =
      s"""<tr><td valign='top' class='section'><p>Params:</td>
         |<td valign='top'>i &ndash; aaa  <p>j &ndash; bbb  <p>k &ndash; ccc  </td>
         |<tr><td valign='top' class='section'><p>Type parameters:</td>
         |<td valign='top'>
         |T &ndash; ddd<p>
         |E &ndash; eee\n            ggg</td>
         |""".stripMargin
    doGenerateDocSectionsTest(fileText, expectedDoc)
  }

  def testTags_Throws(): Unit = {
    val fileText =
      s"""/**
         |  * @throws Exception some condition 1
         |  * @throws java.lang.IllegalAccessException some condition 2
         |  * @throws java.util.ConcurrentModificationException some condition 3
         |  */
         |def ${|}g() {}
         |""".stripMargin
    val expectedDoc =
      s"""<tr><td valign='top' class='section'><p>Throws:</td>
         |<td valign='top'>
         |<a href="psi_element://scala.Exception"><code>Exception</code></a>
         | &ndash; some condition 1
         |<p><a href="psi_element://java.lang.IllegalAccessException"><code>IllegalAccessException</code></a>
         | &ndash; some condition 2
         |<p><a href="psi_element://java.util.ConcurrentModificationException"><code>java.util.ConcurrentModificationException</code></a>
         | &ndash; some condition 3</td>
         |""".stripMargin
    doGenerateDocSectionsTest(fileText, expectedDoc)
  }

  def testTags_Throws_ShouldUseShortestExceptionName(): Unit = {
    val fileText =
      s"""import java.util._
         |/** @throws java.util.ConcurrentModificationException some condition */
         |def ${|}g() {}
         |""".stripMargin
    val expectedDoc =
      s"""<tr><td valign='top' class='section'><p>Throws:</td>
         |<td valign='top'>
         |<a href="psi_element://java.util.ConcurrentModificationException"><code>ConcurrentModificationException</code></a>
         | &ndash; some condition</td>
         |""".stripMargin
    doGenerateDocSectionsTest(fileText, expectedDoc)
  }

  def testTags_ParamsForClass(): Unit = {
    val fileText =
      s"""/**
         |  * @param o
         |  * @tparam E
         |  * @param f description for f
         |  * @tparam K description for K
         |  */
         |class ${|}A[E, K](o: Any, f: AnyVal) {}
         |""".stripMargin
    val expectedDoc  =
      s"""<tr><td valign='top' class='section'><p>Params:</td>
         |<td valign='top'>o &ndash;<p>f &ndash; description for f  </td>
         |<tr><td valign='top' class='section'><p>Type parameters:</td>
         |<td valign='top'>E &ndash;<p>K &ndash; description for K</td>
         |""".stripMargin

    doGenerateDocSectionsTest(fileText, expectedDoc)
  }

  def testTags_ParamsForTypeAlias(): Unit = {
    val fileText =
      s"""/**
         |  * @tparam A
         |  * @tparam B    description for B
         |  * @tparam C
         |  */
         |type ${|}myType[A, B, C] = java.util.HashMap[A, java.util.HashMap[B, C]]
         |""".stripMargin
    val expectedDoc  =
      s"""<tr><td valign='top' class='section'><p>Type parameters:</td>
         |<td valign='top'>A &ndash;  <p>B &ndash; description for B<p>C &ndash;</td>""".stripMargin
    doGenerateDocSectionsTest(fileText, expectedDoc)
  }

  def testFontStyles_Nested_Underscore_Power_Italic(): Unit =
    doGenerateDocBodyTest(
      s"""/**
         | * __xxx^yyy''zzz''yyy^xxx__
         | */
         |val ${|}a = 1
         |""".stripMargin,
      s"""${DefinitionStart}val <b>a</b>: <a href="psi_element://scala.Int"><code>Int</code></a>$DefinitionEnd
         |$ContentStart   <u>xxx<sup>yyy<i>zzz</i>yyy</sup>xxx</u><p>$ContentEnd
         |""".stripMargin
    )

  def testFontStyles_InTags(): Unit = {
    val fileText =
      s"""/**
         | * @note '''__bold with underscore text__'''
         | * @see abc,,index text __index with underscore text__ index text 2,,def
         | */
         |def ${|}f() {}
         |""".stripMargin

    val expectedDoc  =
      s"""<tr><td valign='top' class='section'><p>Note:</td>
         |<td valign='top'><b><u>bold with underscore text</u></b></td>
         |<tr><td valign='top' class='section'><p>See also:</td>
         |<td valign='top'>abc<sub>index text <u>index with underscore text</u> index text 2</sub>def</td>""".stripMargin

    doGenerateDocSectionsTest(fileText, expectedDoc)
  }

  def testHttpLinks(): Unit = {
    val fileText =
      s"""/**
         | * [[http://example.org]]<br>
         | * [[http://example.org    ]]<br>
         | */
         |val ${|}a = 1
         |""".stripMargin
    val expectedDoc =
      s"""${DefinitionStart}val <b>a</b>: <a href="psi_element://scala.Int"><code>Int</code></a>$DefinitionEnd
         |$ContentStart
         |   <a href="http://example.org">http://example.org</a><br>
         |   <a href="http://example.org">http://example.org</a><br>
         |   <p>
         |$ContentEnd""".stripMargin
    doGenerateDocBodyTest(fileText, expectedDoc)
  }

  def testHttpLinks_WithDescription_Simple(): Unit = {
    val fileText =
      s"""/**
         | * [[http://example.org   label]]<br>
         | * [[http://example.org   label  ]]<br>
         | * [[http://example.org   label with   spaces    ]]<br>
         | */
         |val ${|}a = 1
         |""".stripMargin
    val expectedDoc =
      s"""${DefinitionStart}val <b>a</b>: <a href="psi_element://scala.Int"><code>Int</code></a>$DefinitionEnd
         |$ContentStart
         |   <a href="http://example.org">label</a><br>
         |   <a href="http://example.org">label</a><br>
         |   <a href="http://example.org">label with   spaces</a><br>
         |   <p>
         |$ContentEnd""".stripMargin
    doGenerateDocBodyTest(fileText, expectedDoc)
  }

  def testHttpLinks_WithDescription_WithMarkupSyntax(): Unit = {
    val fileText =
      s"""/**
         | * [[http://example.org   '''label with markdown text''']]<br>
         | * [[http://example.org   label '''with markdown text'''  ]]<br>
         | * [[http://example.org   '''label with markdown''' text  ]]<br>
         | * [[http://example.org   label '''with markdown''' text  ]]<br>
         | * [[http://example.org   label '''__with nested__ markdown''' text  ]]<br>
         | */
         |val ${|}a = 1
         |""".stripMargin
    val expectedDoc =
      s"""${DefinitionStart}val <b>a</b>: <a href="psi_element://scala.Int"><code>Int</code></a>$DefinitionEnd
         |$ContentStart
         |   <a href="http://example.org"><b>label with markdown text</b></a><br>
         |   <a href="http://example.org">label <b>with markdown text</b></a><br>
         |   <a href="http://example.org"><b>label with markdown</b> text</a><br>
         |   <a href="http://example.org">label <b>with markdown</b> text</a><br>
         |   <a href="http://example.org">label <b><u>with nested</u> markdown</b> text</a><br>
         |   <p>
         |$ContentEnd""".stripMargin
    doGenerateDocBodyTest(fileText, expectedDoc)
  }

  def testHttpLinks_WithMarkupSyntaxOutside(): Unit = {
    val fileText =
      s"""/**
         | * ,,__[[http://example.org]]__,,<br>
         | * ,,__[[http://example.org   label text]]__,,<br>
         | * [[http://example.org]] ^[[http://example.org]]^<br>
         | * [[http://example.org  label  1 ]] ^[[http://example.org label 2]]^<br>
         | */
         |val ${|}a = 1
         |""".stripMargin
    val expectedDoc =
      s"""${DefinitionStart}val <b>a</b>: <a href="psi_element://scala.Int"><code>Int</code></a>$DefinitionEnd
         |$ContentStart
         |   <sub><u><a href="http://example.org">http://example.org</a></u></sub><br>
         |   <sub><u><a href="http://example.org">label text</a></u></sub><br>
         |   <a href="http://example.org">http://example.org</a> <sup><a href="http://example.org">http://example.org</a></sup><br>
         |   <a href="http://example.org">label  1</a> <sup><a href="http://example.org">label 2</a></sup><br>
         |   <p>
         |$ContentEnd""".stripMargin
    doGenerateDocBodyTest(fileText, expectedDoc)
  }

  def testCodeLinks(): Unit = {
    val fileText =
      s"""/**
         | */
         |val ${|}a = 1
         |""".stripMargin
    val expectedDoc =
      s"""${DefinitionStart}val <b>a</b>: <a href="psi_element://scala.Int"><code>Int</code></a>$DefinitionEnd
         |$ContentStart
         |<p>
         |$ContentEnd""".stripMargin
    doGenerateDocBodyTest(fileText, expectedDoc)
  }

  def testMalformedFontStyles(): Unit =
    doGenerateDocBodyTest(
      s"""/**
         | * ^blah-blah
         | *
         | * __aaaaaaa,,bbbbbbb
         | */
         |val ${|}a = 1
         |""".stripMargin,
      s"""${DefinitionStart}val <b>a</b>: <a href="psi_element://scala.Int"><code>Int</code></a>$DefinitionEnd
         |$ContentStart
         |<sup>blah-blah  </sup><u>aaaaaaa<sub>bbbbbbb </sub></u><p>
         |$ContentEnd""".stripMargin
    )

  def testMalformedTags(): Unit =
    doGenerateDocBodyTest(
      s"""/**
         |  * @gmm
         |  * @
         |  @see
         |  * @param
         |  * @note aaaaa
         |  */
         |val ${|}a = 1
         |""".stripMargin,
      s"""${DefinitionStart}val <b>a</b>: <a href="psi_element://scala.Int"><code>Int</code></a>$DefinitionEnd
         |$ContentStart<p>$ContentEnd
         |$SectionsStart
         |<tr><td valign='top' class='section'><p>Note:</td>
         |<td valign='top'>aaaaa</td>
         |<tr><td valign='top' class='section'><p>See also:</td>
         |<td valign='top'></td>
         |$SectionsEnd""".stripMargin
    )

  def testTags_InheritDoc_SimpleContent(): Unit = {
    val fileText =
      s"""class A {
         |  /**
         |   * Parent description
         |   */
         |  def f = 42
         |}
         |
         |class B extends A {
         |  /**
         |   * Child description
         |   * @inheritdoc
         |   * Extra child description
         |   */
         |  override def ${|}f = 23
         |}
         |""".stripMargin

    val expectedDoc =
      s"""$DefinitionStart<a href="psi_element://B"><code>B</code></a>
         |override def <b>f</b>: <a href="psi_element://scala.Int"><code>Int</code></a>$DefinitionEnd
         |$ContentStart
         |Child description<br>
         |Parent description<br>
         |Extra child description<p>
         |$ContentEnd""".stripMargin
    doGenerateDocBodyTest(fileText, expectedDoc)
  }

  def testTags_InheritDoc_WithMacro(): Unit = {
    val fileText =
      s"""
         |/**
         | *
         | * @define THIS A
         | */
         |class A {
         | /**
         |  * The function f defined in $$THIS returns some integer without no special property. (previously defined in $$PARENT)
         |  * @param i An ignored parameter.
         |  * @return The value $$RESULT.
         |  */
         | def f(i: Int) = 3
         |}
         |
         |/**
         | * @define THIS B
         | * @define PARENT A
         | */
         |class B extends A {
         |  /**
         |   * @inheritdoc
         |   * Some notes on implementation performance, the function runs in O(1).
         |   * @param i An important parameter
         |   */
         |  override def ${|}f(i: Int) = i + 3
         |}
         |""".stripMargin

    val expectedDoc =
      s"""$DefinitionStart<a href="psi_element://B"><code>B</code></a>
         |override def <b>f</b>(i: <a href="psi_element://scala.Int"><code>Int</code></a>): <a href="psi_element://scala.Int"><code>Int</code></a>$DefinitionEnd
         |$ContentStart
         |      The function f defined in B returns some integer without no special property. (previously defined in A)<br>
         |   \n   Some notes on implementation performance, the function runs in O(1).
         |   <p>
         |$ContentEnd
         |$SectionsStart
         |<tr><td valign='top' class='section'><p>Params:</td>
         |<td valign='top'>i &ndash; An important parameter</td>
         |$SectionsEnd""".stripMargin
    doGenerateDocBodyTest(fileText, expectedDoc)
  }

  def testMacro_Simple(): Unit =
    doGenerateDocBodyTest(
      s"""/**
         | * @define THIS A
         | */
         |trait A {
         |  /**
         |   * Function defined in $$THIS
         |   */
         |  def ${|}boo()
         |}
         |""".stripMargin,
      s"""$DefinitionStart<a href="psi_element://A"><code>A</code></a>
         |def <b>boo</b>(): <a href="psi_element://scala.Unit"><code>Unit</code></a>$DefinitionEnd
         |$ContentStart   Function defined in A <p>$ContentEnd
         |""".stripMargin
    )

  def testMacro_Complicated(): Unit =
    doGenerateDocBodyTest(
      s"""/**
         | * @define KEY1 VALUE1
         | */
         |trait A {
         |  /**
         |   * @define KEY_UNREACHED VALUE_UNREACHED
         |   */
         |  def boo() = 1
         |}
         |
         |/**
         | * @define KEY2 VALUE2
         | */
         |trait B {
         |}
         |
         |class C extends A with B {
         |  /**
         |   * a $$KEY1 b $$KEY2 c $$KEY_UNREACHED
         |   */
         |  override def ${|}boo() = 2
         |}
         |""".stripMargin,
      s"""$DefinitionStart<a href="psi_element://C"><code>C</code></a>
         |override def <b>boo</b>(): <a href="psi_element://scala.Int"><code>Int</code></a>$DefinitionEnd
         |$ContentStart
         |a VALUE1 b VALUE2 c [Cannot find macro: $$KEY_UNREACHED] <p>
         |$ContentEnd""".stripMargin
    )

  def testMacro_Wiki(): Unit =
    doGenerateDocBodyTest(
      s"""/**
         | * @define none `None`
         | */
         |class A {
         |  /**
         |   * $$none
         |   */
         |   def ${|}foo() = {}
         |}
         |""".stripMargin,
      s"""$DefinitionStart<a href="psi_element://A"><code>A</code></a>
         |def <b>foo</b>(): <a href="psi_element://scala.Unit"><code>Unit</code></a>$DefinitionEnd
         |$ContentStart <tt>None</tt><p>$ContentEnd
         |""".stripMargin
    )


  /** Returns true if the option is $$none, false otherwise */
  def testMacro_Undefined(): Unit =
    doGenerateDocContentTest(
      """/** Returns true if the option is $none, false otherwise */
         |class A {}
         |""".stripMargin,
      """Returns true if the option is [Cannot find macro: $none], false otherwise<p>""".stripMargin
    )

  def testAnnotationArgs(): Unit = {
    val fileContent =
      s"""class Outer {
         |  @deprecated("use 'foo' instead", "1.2.3")
         |  @transient
         |  def ${|}boo() {}
         |}""".stripMargin

    val expectedDoc =
      s"""<a href="psi_element://Outer"><code>Outer</code></a>
         |@<a href="psi_element://scala.deprecated"><code>deprecated</code></a>(&quot;use 'foo' instead&quot;, &quot;1.2.3&quot;)
         |@<a href="psi_element://scala.transient"><code>transient</code></a>
         |def <b>boo</b>(): <a href="psi_element://scala.Unit"><code>Unit</code></a>""".stripMargin

    doGenerateDocDefinitionTest(fileContent, expectedDoc)
  }

  def testAnnotationArgs_WithInnerHtmlTextShouldBeEscaped(): Unit = {
    val fileContent =
      s"""class Outer {
         |  @deprecatedName("inner tags <p>example</p>", "since 2020")
         |  def ${|}boo() {}
         |}""".stripMargin

    val expectedDoc =
      s"""<a href="psi_element://Outer"><code>Outer</code></a>
         |@<a href="psi_element://scala.deprecatedName"><code>deprecatedName</code></a>(&quot;inner tags &lt;p&gt;example&lt;/p&gt;&quot;, &quot;since 2020&quot;)
         |def <b>boo</b>(): <a href="psi_element://scala.Unit"><code>Unit</code></a>""".stripMargin
    doGenerateDocDefinitionTest(fileContent, expectedDoc)
  }

  def testAnnotation_Throws_ShouldIgnoreExceptionClassArgument(): Unit = {
    // NOTE: the exception class is already shown in the annotation type, see SCL-17608
    val fileContent =
      """@throws(classOf[Exception])
        |@throws[Exception]("reason 1")
        |@throws(classOf[java.util.ConcurrentModificationException])
        |@throws[java.util.ConcurrentModificationException]("reason 2")
        |def goo() {}
        |""".stripMargin

    val expectedDoc =
      s"""@<a href="psi_element://scala.throws"><code>throws</code></a>[<a href="psi_element://scala.Exception"><code>Exception</code></a>]
         |@<a href="psi_element://scala.throws"><code>throws</code></a>[<a href="psi_element://scala.Exception"><code>Exception</code></a>](&quot;reason 1&quot;)
         |@<a href="psi_element://scala.throws"><code>throws</code></a>[<a href="psi_element://java.util.ConcurrentModificationException"><code>ConcurrentModificationException</code></a>]
         |@<a href="psi_element://scala.throws"><code>throws</code></a>[<a href="psi_element://java.util.ConcurrentModificationException"><code>ConcurrentModificationException</code></a>](&quot;reason 2&quot;)
         |def <b>goo</b>(): <a href="psi_element://scala.Unit"><code>Unit</code></a>""".stripMargin

    doGenerateDocDefinitionTest(fileContent, expectedDoc)
  }

  def testTrait_SpecialChars_InfixType(): Unit =
    doGenerateDocDefinitionTest(
      s"""object A {
         |  trait <:<[A,B]
         |  def ${|}f(a: Int <:< String): Unit = {}
         |}""".stripMargin,
      "<a href=\"psi_element://A\"><code>A</code></a>\n" +
        "def <b>f</b>(" +
        "a: <a href=\"psi_element://scala.Int\"><code>Int</code></a>" +
        " <a href=\"psi_element://A.&lt;:&lt;\"><code>&lt;:&lt;</code></a> " +
        "<a href=\"psi_element://scala.Predef.String\"><code>String</code></a>" +
        "): <a href=\"psi_element://scala.Unit\"><code>Unit</code></a>"
    )

  def testNestedScaladocShouldBeTreatedAsCommentData(): Unit ={
    val fileContent =
      s"""/**
        | * text 1
        | * /**
        | * * text inner 1
        | * */
        | * text 2
        | *
        | * {{{/**   text inner   3   */}}}
        | */
        |class ${|}NestedDoc""".stripMargin

    val expectedContent =
      """text 1   /**   * text inner 1   */   text 2
        |   <pre><code>/**   text inner   3   */</code></pre> <p>""".stripMargin

    doGenerateDocContentTest(fileContent, expectedContent)
  }

  def testWIP_FAILING_TEST(): Unit ={
    throw new AssertionFailedException("failing test to trigger TeamCity tests")
  }
}