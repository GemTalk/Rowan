Class {
	#name : 'RowanPackageServiceTest',
	#superclass : 'RowanServicesTest',
	#category : 'Rowan-Services-Tests'
}

{ #category : 'tests' }
RowanPackageServiceTest >> test_compileAndSelectClass [
  | package browserService |
  [ 
  self assert: RowanCommandResult results isEmpty.
  package := self createJadeiteTestPackage.
  browserService := RowanBrowserService new.
  self assert: RowanCommandResult results isEmpty.
  browserService
    compileClass:
      'RowanServicesTest rwSubclass: ''RowanTestCompile'' 
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: ''' , self servicesTestPackageName
        ,
          '''
		options: #()'.
  self assert: RowanCommandResult results size equals: 0.	"we no longer return a service on first stage of compile"
  browserService recompileMethodsAfterClassCompilation.
  self assert: RowanCommandResult results size equals: 5.
  self
    assert: (RowanCommandResult results at: 4) name
    equals: 'RowanTestCompile'.
  self assert: browserService selectedClass name equals: 'RowanTestCompile' ]
    ensure: [ self unloadServicesTestProject ]
]

{ #category : 'tests' }
RowanPackageServiceTest >> test_compileAndSelectClassDifferentPackage [
  | package browserService testsPackage |
  [ 
  self assert: RowanCommandResult results isEmpty.
  package := self createJadeiteTestPackage.
  browserService := RowanBrowserService new.
  self assert: RowanCommandResult results isEmpty.
  browserService
    compileClass:
      'RowanServicesTest rwSubclass: ''RowanTestCompile''
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: ''Rowan-Services-Tests''
		options: #()'.
  self assert: RowanCommandResult results size equals: 0.	"we no longer return a service on first stage of compile"
  browserService recompileMethodsAfterClassCompilation.
  self assert: RowanCommandResult results size equals: 5.
  self
    assert: (RowanCommandResult results at: 4) name
    equals: 'RowanTestCompile'.
  self
    assert: RowanCommandResult results first name
    equals: 'Rowan-Services-Tests'.
  testsPackage := RowanCommandResult results first.
  self assert: testsPackage selectedClass name equals: 'RowanTestCompile' ]
    ensure: [ self unloadServicesTestProject ]
]

{ #category : 'tests' }
RowanPackageServiceTest >> test_packageWasDeleted [
  "NOTE - use commit/abort in tests carefully. 
	Can cause hard-to-diagnose problems later 
	in test runs"

  | packageService projectDef packageDef |
  self
    jadeiteIssueTested: #'issue284'
    withTitle:
      '(3.0.49 and 3.0.50) project browser not updated properly on reload of project'.
  projectDef := self createJadeiteTestProject.
  [ 
  System commitTransaction.	"commit project but not package"
  projectDef
    packageNamed: self servicesTestPackageName
    ifAbsent: [ 
      packageDef := RwPackageDefinition newNamed: self servicesTestPackageName.
      projectDef addPackage: packageDef ].
  Rowan projectTools load loadProjectDefinition: projectDef.
  packageService := RowanPackageService
    forPackageNamed: self servicesTestPackageName.
  self deny: packageService wasDeleted.
  System abortTransaction.
  self assert: packageService wasDeleted ]
    ensure: [ 
      self unloadServicesTestProject.
      System commitTransaction ]
]

{ #category : 'tests' }
RowanPackageServiceTest >> test_testClassesIncludesExtensions [
	| packageService testClassNames |
	self jadeiteIssueTested: #issue378 withTitle: '(3.0.53) test class not defined in package shows up in package of SUnit browser'.
	packageService := RowanPackageService forPackageNamed: 'Rowan-Services-Tests'.
	packageService testClasses. 
	testClassNames := packageService jadeite_testClasses collect:[:classService | classService name].
	self assert: (testClassNames includes: 'RwRowanProjectIssuesTest'). "extension tests"
	self assert: (testClassNames includes: 'RowanPackageServiceTest').

	packageService := RowanPackageService forPackageNamed: 'Rowan-Tests'.
	packageService testClasses. 
	testClassNames := packageService jadeite_testClasses collect:[:classService | classService name].
	self assert: (testClassNames includes: 'RwRowanProjectIssuesTest').
]

{ #category : 'tests' }
RowanPackageServiceTest >> testClassHierarchy [

	"format for client is:
		#nil-> #(<class serviceA1> <class service A2) ...
		#<classService A1> -> #(<classService B1> <classService B2> ... )
		#<classService A2> -> #(<classService B3> <classService B4> ... )
		#<classService B1> -> #(<classService C1> <classService C2> ... )
		etc"

	| packageService objectClassService |

	self jadeiteIssueTested: #issue239 withTitle: 'long update times possible when selecting a package with `hierarchy` tab selected'.
	packageService := RowanPackageService forPackageNamed: 'Rowan-Tests'. 
	packageService classHierarchy. "<-- infinite recursion occured here"
	self assert: (packageService hierarchyServices at: #nil) size equals: 1. "duplicate Object services was the problem"
	objectClassService := (packageService hierarchyServices at: #nil) first.
	self assert: objectClassService name = 'Object'.
	self assert: ((packageService hierarchyServices at: objectClassService) detect:[:classService | classService name = 'TestAsserter'] ifNone: []) notNil.
	self assert: ((packageService hierarchyServices keys) detect:[:classService | (classService ~~ #nil) and: [classService name = 'TestCase']] ifNone: []) notNil.
]

{ #category : 'tests' }
RowanPackageServiceTest >> testCompileClassSelectsPackageAndClass [

	| package browserService newClassService |

	self jadeiteIssueTested: #issue228 withTitle: 'lose selected class in project browser when new version created'.
	package := self createJadeiteTestPackage. 
	[browserService := RowanBrowserService new.
	browserService compileClass: 
		'RowanServicesTest rwSubclass: ''TestCompileClass''
			instVarNames: #()
			classVars: #()
			classInstVars: #()
			poolDictionaries: #()
			category: ''', self servicesTestPackageName, '''
			options: #()'.
	self assert: RowanCommandResult results size equals: 0. "we no longer return a service on first stage of compile"
	browserService recompileMethodsAfterClassCompilation. 
	newClassService := RowanCommandResult results at: 4.
	self assert: newClassService name equals: 'TestCompileClass'.]
		ensure:[RowanBrowserService new unloadProjectsNamed: (Array with: self servicesTestProjectName)]
]
