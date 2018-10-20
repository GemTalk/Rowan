Class {
	#name : 'RowanPackageServiceTest',
	#superclass : 'RowanServicesTest',
	#category : 'Rowan-Services-Tests'
}

{ #category : 'tests' }
RowanPackageServiceTest >> testClassHierarchy [

	"format for client is:
		#nil-> #(<class serviceA1> <class service A2) ...
		#<classService A1> -> #(<classService B1> <classService B2> ... )
		#<classService A2> -> #(<classService B3> <classService B4> ... )
		#<classService B1> -> #(<classService C1> <classService C2> ... )
		etc"

	| packageService objectClassService |

	self testsJadeiteIssue: #issue239 withTitle: 'long update times possible when selecting a package with `hierarchy` tab selected'.
	packageService := RowanPackageService forPackageNamed: 'Rowan-Tests'. 
	packageService classHierarchy. "<-- infinite recursion occured here"
	self assert: (packageService hierarchyServices at: #nil) size equals: 1. "duplicate Object services was the problem"
	objectClassService := (packageService hierarchyServices at: #nil) first.
	self assert: objectClassService name = 'Object'.
	self assert: ((packageService hierarchyServices at: objectClassService) detect:[:classService | classService name = 'TestAsserter'] ifNone: []) notNil.
	self assert: ((packageService hierarchyServices keys) detect:[:classService | (classService ~~ #nil) and: [classService name = 'TestCase']] ifNone: []) notNil.
]
