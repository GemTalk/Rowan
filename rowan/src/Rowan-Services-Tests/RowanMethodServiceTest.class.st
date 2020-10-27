Class {
	#name : 'RowanMethodServiceTest',
	#superclass : 'RowanServicesTest',
	#category : 'Rowan-Services-Tests'
}

{ #category : 'support' }
RowanMethodServiceTest >> setUp [

	super setUp.
	self createServicesTestClass. 
	self loadServicesTestProject.
]

{ #category : 'constants' }
RowanMethodServiceTest >> stepPoint1Source [
	"sent from the client, too" 

^'simpleMethod

	| array |
	array := Array new. 
			"^3"		"^2"
	array add: OrderedCollection new. 
			"^5"							"^4"
	array size.
			"^6"
	^array'
]

{ #category : 'constants' }
RowanMethodServiceTest >> stepPoint2Source [
	"sent from the client, too" 


^'simpleMethod2

	| array |
	array := Array new. 
						"^1"
	array add: (RowanClassService forClassNamed: Fraction). 
			"^3"								"^2"
	array do:[:classService | 
			"^4"  
				| stepPoints |
			stepPoints := classService stepPoints.
												"^5"
			stepPoints size
							"^6"]. 
	array size.
			"^7"
	^array'
]

{ #category : 'constants' }
RowanMethodServiceTest >> stepPoint3Source [
	"sent from the client, too" 

	"missing step point numbers were optimized away or screwed
	up by the 3.2.15 server. "

^'initialize: aGsProcess status: aString

	| theOrganizer frames oop status |
	theOrganizer := ClassOrganizer new. 
												"^1"
	frames := Array new: aGsProcess stackDepth.
							"^3"					"^2"
	1 to: aGsProcess stackDepth do: [:i | 
								"^4"
		frames at: i put: (RowanFrameService process: aGsProcess level: i organizer: theOrganizer).
				"^5"											"^6"
	].
	oop := aGsProcess asOop.  
								"^9"
	status := aString.'
]

{ #category : 'support' }
RowanMethodServiceTest >> tearDown [

	self unloadServicesTestProject.
	super tearDown.
]

{ #category : 'tests' }
RowanMethodServiceTest >> test_classNameIsString [

	| methodService organizer |
	self jadeiteIssueTested: #issue441 withTitle: '(3.0.62) suspicious code in RowanClassService>>hierarchyClassServiceFor:'.
	organizer := ClassOrganizer new. 
	methodService := RowanMethodService forGsNMethod: (RowanMethodServiceTest compiledMethodAt: #setUp) organizer: organizer.
	self assert: (methodService className isKindOf: String).
	self assert: methodService className = 'RowanMethodServiceTest'.

	methodService := RowanMethodService forSelector: #setUp class: RowanMethodServiceTest meta: false organizer: organizer.
	self assert: (methodService className isKindOf: String).
	self assert: methodService className = 'RowanMethodServiceTest'.

	methodService := RowanMethodService source: 'fnoodle' selector: #fnoodle category: 'other' className: 'RowanMethodServiceTest' packageName: 'Rowan-Services-Tests' meta: true. 
	self assert: (methodService className isKindOf: String).
	self assert: methodService className = 'RowanMethodServiceTest'.
]

{ #category : 'tests' }
RowanMethodServiceTest >> test_isTestMethod [
	"isTestMethod not getting initialized correctly" 
	| methodService organizer |
	self jadeiteIssueTested: #issue512 withTitle: '"run selected" can get disabled after save in debugger (3.0.76)'.
	organizer := ClassOrganizer new. 
	methodService := RowanMethodService forGsNMethod: (RowanMethodServiceTest compiledMethodAt: #test_isTestMethod) organizer: organizer.
	self assert: methodService isTestMethod. 
	methodService := RowanMethodService forGsNMethod: (RowanMethodService compiledMethodAt: #initialize) organizer: organizer.
	self deny: methodService isTestMethod.
]

{ #category : 'tests' }
RowanMethodServiceTest >> test_reformatSource [
	| classService methodService source strm indentString |
		classService := RowanClassService forClassNamed: self servicesDefaultClassName.
	source := 'abc | abc | abc := 1. abc := abc + abc. ^abc'.
	classService saveMethodSource: source category: 'other'.
	self assert: (classService methodsNamed: #abc) size equals: 1.
	methodService := (classService methodsNamed: #abc) first.
	self assert: methodService source equals: source.
	methodService reformatSource.
	indentString := RBConfigurableFormatter indentString.
	strm := WriteStream on: String new.
	strm 
		nextPutAll: 'abc'; lf;
		nextPutAll:  indentString, '| abc |'; lf;
		nextPutAll:  indentString, 'abc := 1.'; lf;
		nextPutAll:  indentString, 'abc := abc + abc.'; lf;
		nextPutAll:  indentString, '^ abc';
		yourself.
	self assert: methodService reformatSource source equals: strm contents

]

{ #category : 'tests' }
RowanMethodServiceTest >> test_runMethodTest [

	| methodService classService |
	self jadeiteIssueTested: #issue410 withTitle: 'Selecting class in Project Browser changes test status icons in SUnit Browser'.
	methodService := RowanMethodService new.
	methodService runTest: #test_matchingPattern inClassName: 'RowanAnsweringServiceTest'.
	self assert: methodService testResult equals: 'passed'.
	self createServicesTestTestClass.
	self loadServicesTestProject.
	classService := RowanClassService forClassNamed: self servicesDefaultTestClassName.
	classService saveMethodSource: 'testMethod1  self assert: false' category: 'failing test'.
	methodService runTest: #testMethod1 inClassName: self servicesDefaultTestClassName.
	self assert: methodService testResult equals: 'failure'.
	classService saveMethodSource: 'testMethod2  1 zork' category: 'failing test'.
	methodService runTest: #testMethod2 inClassName: self servicesDefaultTestClassName.
	self assert: methodService testResult equals: 'error'.
]

{ #category : 'tests' }
RowanMethodServiceTest >> test_setDefinedClass [
	| methodService organizer theMethod methodServicesClass |
	self
		jadeiteIssueTested: #'issue828'
		withTitle:
			'`UndefinedObject does not understand #asString` from `Browse Selected method` in SUnit Browser'.
	organizer := ClassOrganizer new.
	theMethod := RowanMethodServiceTest compiledMethodAt: #'test_setDefinedClass'.
	methodService := RowanMethodService
		forGsNMethod: theMethod
		organizer: organizer.
	methodService definedClassName: nil.	"reset for test"
	self assert: methodService definedClassName equals: nil.
	methodServicesClass := theMethod inClass.
	methodService setDefinedClass: methodServicesClass.
	self assert: methodService definedClassName equals: 'RowanMethodServiceTest'.

	theMethod := RowanServicesTest compiledMethodAt: #'compileUnicodeMethod'.
	methodService := RowanMethodService
		forGsNMethod: theMethod
		organizer: organizer.
	methodService definedClassName: nil.	"reset for test"
	self assert: methodService definedClassName equals: nil.
	methodService setDefinedClass: methodServicesClass.
	self assert: methodService definedClassName equals: 'RowanServicesTest'
]
