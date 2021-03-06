package org.jetbrains.plugins.scala
package refactoring.extractMethod

import org.jetbrains.plugins.scala.lang.formatting.settings.ScalaCodeStyleSettings
import org.jetbrains.plugins.scala.util.TypeAnnotationSettings

/**
 * Nikolay.Tropin
 * 2014-05-20
 */
class ScalaExtractMethodInnerClass extends ScalaExtractMethodTestBase {
  override def folderPath: String = super.folderPath + "innerClass/"

  def testNoReturnSeveralOutput(): Unit = {
    val settings = TypeAnnotationSettings.alwaysAddType(ScalaCodeStyleSettings.getInstance(getProjectAdapter))
    doTest(settings = TypeAnnotationSettings.noTypeAnnotationForPublic(settings))
  }

  def testReturnSeveralOutput1(): Unit = doTest()

  def testReturnSeveralOutput2(): Unit = doTest()

  def testUnitReturnSeveralOutput1(): Unit = doTest()

  def testUnitReturnSeveralOutput2(): Unit = doTest()
}
