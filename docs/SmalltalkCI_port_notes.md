### setup 
1. `rowan` branch of GsDevKit_home
```
git clone https://github.com/GsDevKit/GsDevKit_home.git
cd GsDevKit_home
. bin/defHOME_PATH.env    # define GS_HOME env var and put $GS_HOME into PATH
git checkout rowan
installServerClient
createClient tode
```
2. `smalltalkci_port` branch of Rowan
```
cd $GS_HOME/shared/repos
git clone git@github.com:GemTalk/Rowan.git
cd Rowan 
git checkout smalltalkci_port
```
3. FileSystemGs
```
cd $GS_HOME/shared/repos
git clone git@github.com:GemTalk/FileSystemGs.git
```
4. `rowan` branch of SmalltalkCI
```
cd $GS_HOME/shared/repos/smalltalkCI
git remote add dkh git@github.com:dalehenrich/smalltalkCI.git
git fetch --all
git checkout rowan
```
5. Fork the projects that you will be changing. [SmalltalkCi](https://github.com/dalehenrich/smalltalkCI), obviously and [GsDevKit_home](https://github.com/GsDevKit/GsDevKit_home) as you might have to adjust some scripts for testing ... [Rowan](https://github.com/GemTalk/Rowan), since it's possible that bugs will be fixed, etc.
6. Build a smalltalkCI stone (using tODE/GLASS):
```
createStone smalltalkCI_3215 3.2.15
```
7. Load smalltalkCI into smalltalkCi_3215 stone using tode command:
```
project load --loads=`#('tests')` SmalltalkCI	# load SmalltalkCI
test project SmalltalkCI			# bring up test browser
```
8. Current test results (SCIExcludedTests errors and failures are normal - SmalltalkCI needs ):
```
58 run, 52 passes, 1 expected defects, 3 failures, 1 errors, 1 unexpected passes
	58 tests
	58 run
	52 passes
	1 expected defects
	3 failures
SCIExcludedTests>>#testAssertError
SCIExcludedTests>>#testFailure
SCIExcludedTests>>#testThisIsAVeryLongMethodNameThatProbablyNeedsToBeContractedInOrderToBeDisplayedCorrectlyInATravisLog
	1 errors
SCIExcludedTests>>#testError
	1 unexpected passes
```
9. Create two rowan/smalltalkCI stones dev and test:
```
createStone -RG rowan_ci_3215 3.2.15
cd $GS_HOME/server/stones/rowan_ci_3215

cat - >> custom_stone.env << EOF
export ROWAN_PROJECTS_HOME=$GS_HOME/shared/repos
EOF
stopNetldi rowan_ci_3215
startNetldi rowan_ci_3215
ln -s $GS_HOME/shared/repos/smalltalkCI/gemstone/gsdevkit/stones/newBuild_rowan_smalltalkci	#rebuild rowan/smalltalkCI stone new stone in-place

createStone -RG test_rowan_ci_3215 3.2.15
cd $GS_HOME/server/stones/test_rowan_ci_3215

cat - >> custom_stone.env << EOF
export ROWAN_PROJECTS_HOME=$GS_HOME/shared/repos
EOF
stopNetldi test_rowan_ci_3215
startNetldi testrowan_ci_3215

ln -s $GS_HOME/shared/repos/smalltalkCI/gemstone/gsdevkit/stones/newBuild_rowan_smalltalkci	#rebuild rowan/smalltalkCI stone new stone in-place
ln -s $GS_HOME/shared/repos/smalltalkCI/gemstone/gsdevkit/stones/newBuild_rowan_test_smalltalkci	#rebuild rowan/smalltalkCI stone new stone in-place and run tests
```
10. Current test results running `newBuild_rowan_test_smalltalkci` script  11-2-18):
```
smalltalkCI tests
66 run, 15 passed, 5 failed, 46 errors
  errors
	FastUUIDTest>>#testComparison
	FastUUIDTest>>#testComparisonA
	FastUUIDTest>>#testCreation
	FastUUIDTest>>#testCreationEquality
	FastUUIDTest>>#testCreationFromString
	FastUUIDTest>>#testCreationFromStringNotNil
	FastUUIDTest>>#testCreationFromStringNotNil
	FastUUIDTest>>#testCreationNil
	FastUUIDTest>>#testCreationNodeBased
	FastUUIDTest>>#testCreationNodeBased
	FastUUIDTest>>#testDuplicationsKinda
	FastUUIDTest>>#testOrder
	FastUUIDTest>>#testOrder
	FastUUIDTest>>#testUniqueness
	FastUUIDTest>>#testUniqueness
	SCICustomScriptTest>>#testFileInFrom
	SCICustomScriptTest>>#testFileInFrom
	SCIExcludedTests>>#testError
	SCIExcludedTests>>#testFailure
	SCIExcludedTests>>#testShouldFail
	SCITestReporterStdoutTest>>#testReportFailure
	SCITestReporterStdoutTest>>#testReportSuccess
	SCITestReporterXMLTest>>#testReportFailure
	SCITestReporterXMLTest>>#testReportSuccess
	SCITestRunnerTest>>#testRunClasses
	SCITestRunnerTest>>#testRunSpecNamed
	SmalltalkCISpecTest>>#testCompatibleCustomScripts
	SmalltalkCISpecTest>>#testFromFile
	SmalltalkCISpecTest>>#testFromFile
	SmalltalkCITest>>#testAppVeyorDetection
	SmalltalkCITest>>#testAuthor
	SmalltalkCITest>>#testClassesForCategories
	SmalltalkCITest>>#testClassesForPackages
	SmalltalkCITest>>#testClassesFrom
	SmalltalkCITest>>#testClassesInCategory
	SmalltalkCITest>>#testClassesInPackage
	SmalltalkCITest>>#testClassesOfProjects
	SmalltalkCITest>>#testClassesWithCategoryNames
	SmalltalkCITest>>#testClassesWithPackageNames
	SmalltalkCITest>>#testGitLabCIDetection
	SmalltalkCITest>>#testNew
	SmalltalkCITest>>#testNewTravisID
	SmalltalkCITest>>#testResolveAllWith
	SmalltalkCITest>>#testResolveWith
	SmalltalkCITest>>#testTravisDetection
	SmalltalkCITest>>#testTravisFold
  failures
	FastUUIDTest>>#testCreation
	FastUUIDTest>>#testCreationEquality
	FastUUIDTest>>#testDuplicationsKinda
	SCIExcludedTests>>#testAssertError
	SCIExcludedTests>>#testThisIsAVeryLongMethodNameThatProbablyNeedsToBeContractedInOrderToBeDisplayedCorrectlyInATravisLog
```
11. Use [Jadeite Oscar-3.0.25](https://github.com/GemTalk/Jadeite/releases/tag/Oscar-3.0.25) to browse code in stones

### state of the port
The immediate goal is to get to the point where we've reduced the failures to those due to missing SmalltalkCI Rowan support classes.

The UUID classes and tests have been included in the most recent Rowan work (branch `issue_361`). Having FileSystem support available will also make implementing Rowan support easier, so it makes sense to merge `issue_361` into the `smalltalk_ci` branch of Rowan and get back to the same point we're at now:
1. tag the current SHA of `smalltalk_ci` branch so that we can easily get back to this point to compare
2. create a candidateV2.0 branch so that becomes the share point for Rowan base changes between my work for [Issse #361](https://github.com/GemTalk/Rowan/issues/361) and the work on the smalltalk ci port
3. merge `candidateV2.0` branch into `smalltalk_ci` branch and resolve issues (UUID duplication is expected ... Rowan/FileSystemGs wins
