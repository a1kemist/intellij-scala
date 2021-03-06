package org.jetbrains.plugins.scala.testingSupport.utest

import org.jetbrains.plugins.scala.lang.structureView.element.Test

trait UTestSimpleTest extends UTestTestCase {

  protected val uTestTestName = "UTestTest"

  protected val uTestFileName = uTestTestName + ".scala"

  addSourceFile(uTestFileName,
    s"""
       |import utest._
       |$testSuiteSecondPrefix
       |
       |object UTestTest extends TestSuite {
       |  val tests = TestSuite {
       |    "outer1" - {}
       |
       |    "outer2" - {
       |      "inner2_1" - {}
       |    }
       |
       |    "sameName" - {
       |      "sameName" - {}
       |    }
       |
       |    "failed" - {
       |      assert(false)
       |    }
       |  }
       |}
      """.stripMargin.trim())

  protected val outer1_Path  = List("[root]", uTestTestName, "tests", "outer1")
  protected val inner2_1Path = List("[root]", uTestTestName, "tests", "outer2", "inner2_1")
  protected val sameNamePath = List("[root]", uTestTestName, "tests", "sameName", "sameName")
  protected val failedPath   = List("[root]", uTestTestName, "tests", "failed")

  def testSingleTest(): Unit = {
    runTestByLocation2(8, 10, uTestFileName,
      assertConfigAndSettings(_, uTestTestName, "tests\\outer2\\inner2_1"),
      root => {
        assertResultTreeHasExactNamedPath(root, inner2_1Path)
        assertResultTreeDoesNotHaveNodes(root, "outer1", "inner1_1")
      })
  }

  def testSameName(): Unit = {
    runTestByLocation2(12, 10, uTestFileName,
      assertConfigAndSettings(_, uTestTestName, "tests\\sameName\\sameName"),
      root => assertResultTreeHasExactNamedPath(root, sameNamePath))
  }

  def testClassSuite(): Unit = {
    runTestByLocation2(3, 3, uTestFileName,
      assertConfigAndSettings(_, uTestTestName),
      root => assertResultTreeHasExactNamedPaths(root)(Seq(
        outer1_Path,
        inner2_1Path,
        sameNamePath,
        failedPath,
      )))
  }

  def testFileStructureView(): Unit = {
    //notice that we only test here nodes that produce TestStructureViewElement in file structure view
    //this means that root test scopes (methods) are not tested here; instead, they are tested in testFileStructureViewHierarchy
    runFileStructureViewTest(uTestTestName, Test.NormalStatusId,
      "\"outer1\"", "\"outer2\"",  "\"sameName\"", "\"failed\"")
  }

  def testFileStructureViewHierarchy(): Unit = {
    runFileStructureViewTest(uTestTestName, "\"outer1\"", Some("tests"))
    runFileStructureViewTest(uTestTestName, "\"outer2\"", Some("tests"))
    runFileStructureViewTest(uTestTestName, "\"inner2_1\"", Some("\"outer2\""))
    runFileStructureViewTest(uTestTestName, "\"sameName\"", Some("tests"))
    runFileStructureViewTest(uTestTestName, "\"sameName\"", Some("\"sameName\""))
    runFileStructureViewTest(uTestTestName, "\"failed\"", Some("tests"))
  }

  def testDuplicateConfig(): Unit = {
    runDuplicateConfigTest(8, 10, uTestFileName, assertConfigAndSettings(_, uTestTestName, "tests\\outer2\\inner2_1"))
  }

  def testGoToSourceSuccessful(): Unit = {
    runGoToSourceTest(4, 7, uTestFileName,
      assertConfigAndSettings(_, uTestTestName, "tests"),
      List("[root]", uTestTestName, "tests"), 4)
  }

  def testGoToSourceFailed(): Unit = {
    //notice that 'goToSource' now travels only to method: right now, we don't identify exact line of code in test
    //execution completion callback
    runGoToSourceTest(16, 5, uTestFileName,
      assertConfigAndSettings(_, uTestTestName, "tests\\failed"),
      failedPath, 4)
  }
}
