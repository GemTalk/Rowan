Class {
	#name : 'RowanPackageServiceTest',
	#superclass : 'RowanServicesTest',
	#category : 'Rowan-Services-Tests'
}

{ #category : 'tests' }
RowanPackageServiceTest >> test_compileAndSelectClass [

	| package packageService |
	System commitTransaction.
	[self assert: RowanCommandResult results isEmpty. 
	package := self createJadeiteTestPackage. 
	packageService := RowanPackageService forPackageNamed: self servicesTestPackageName.
	self assert: RowanCommandResult results isEmpty. 
	packageService compileClass: 
	'RowanServicesTest rwSubclass: ''RowanTestCompile'' 
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: ''', self servicesTestPackageName,  '''
		options: #()'.
	self assert: RowanCommandResult results size equals: 1. 
	self assert: RowanCommandResult results first name equals: 'RowanTestCompile'. 
	self assert: packageService selectedClass name equals: 'RowanTestCompile'.
	self assert: packageService selectedClass selectedPackageServices first equals: packageService] 
		ensure:[System abortTransaction.  "get rid of test package and class"]
]

{ #category : 'tests' }
RowanPackageServiceTest >> test_compileAndSelectClassDifferentPackage [

	| package packageService testsPackage |
	System commitTransaction.
	[self assert: RowanCommandResult results isEmpty. 
	package := self createJadeiteTestPackage. 
	packageService := RowanPackageService forPackageNamed: self servicesTestPackageName.
	self assert: RowanCommandResult results isEmpty. 
	packageService compileClass: 
	'RowanServicesTest rwSubclass: ''RowanTestCompile''
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: ''Rowan-Services-Tests''
		options: #()'.
	self assert: RowanCommandResult results size equals: 2. 
	self assert: RowanCommandResult results last name equals: 'RowanTestCompile'. 
	self assert: RowanCommandResult results first name equals: 'Rowan-Services-Tests'. 
	testsPackage := RowanCommandResult results first.
	self assert: testsPackage selectedClass name equals: 'RowanTestCompile'.
	self assert: testsPackage selectedClass selectedPackageServices first equals: testsPackage] 
		ensure:[System abortTransaction.  "get rid of test package and class"]
]

{ #category : 'tests' }
RowanPackageServiceTest >> test_packageWasDeleted [

	| package packageService |
	System commitTransaction. 
	self jadeiteIssueTested: #issue284 withTitle: '(3.0.49 and 3.0.50) project browser not updated properly on reload of project'.
	package := self createJadeiteTestPackage. 
	packageService := RowanPackageService forPackageNamed: self servicesTestPackageName.
	self deny: packageService wasDeleted.
	System abortTransaction. 
	self assert: packageService wasDeleted.
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

	| package packageService newClassService |

	self jadeiteIssueTested: #issue228 withTitle: 'lose selected class in project browser when new version created'.
	package := self createJadeiteTestPackage. 
	[packageService := RowanPackageService forPackageNamed: self servicesTestPackageName.
	packageService compileClass: 
		'RowanServicesTest rwSubclass: ''TestCompileClass''
			instVarNames: #()
			classVars: #()
			classInstVars: #()
			poolDictionaries: #()
			category: ''', self servicesTestPackageName, '''
			options: #()'.
	self assert: RowanCommandResult results size equals: 1.
	newClassService := RowanCommandResult results first.
	self assert: newClassService name equals: 'TestCompileClass'. 
	self assert: newClassService selectedPackageServices size equals: 1. 
	self assert: newClassService selectedPackageServices first name equals: self servicesTestPackageName. 
	self assert: newClassService selectedPackageServices first selectedClass == newClassService]
		ensure:[RowanBrowserService new unloadProjectsNamed: (Array with: self servicesTestProjectName)]
]
