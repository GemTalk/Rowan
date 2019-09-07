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
						"^1"
	array add: OrderedCollection new. 
			"^3"							"^2"
	array size.
			"^4"
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

	super tearDown.
	self unloadServicesTestProject
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
	System abortTransaction.
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
RowanMethodServiceTest >> test_noStepPoints [
	| classService methodService |
	classService := RowanClassService forClassNamed: self servicesDefaultClassName.
	classService saveMethodSource: 'abc' category: 'testing step points'.
	methodService := RowanMethodService forSelector: #abc class: classService classOrMeta meta: false organizer: ClassOrganizer new.
	self assert: methodService stepPoints isEmpty.
]

{ #category : 'tests' }
RowanMethodServiceTest >> test_reformatSource [
	| classService methodService source |
		classService := RowanClassService forClassNamed: self servicesDefaultClassName.
	source := 'abc | abc | abc := 1. abc := abc + abc. ^abc'.
	classService saveMethodSource: source category: 'other'.
	self assert: (classService methodsNamed: #abc) size equals: 1.
	methodService := (classService methodsNamed: #abc) first.
	self assert: methodService source equals: source.
	methodService reformatSource.
	self assert: methodService reformatSource source equals: 'abc
  | abc |
  abc := 1.
  abc := abc + abc.
  ^ abc'
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
RowanMethodServiceTest >> test_stepPoint1 [
	| classService methodService |
	classService := RowanClassService forClassNamed: self servicesDefaultClassName.
	classService saveMethodSource: self stepPoint1Source category: 'testing step points'.
	methodService := RowanMethodService forSelector: #simpleMethod class: classService classOrMeta meta: false organizer: ClassOrganizer new.
	self assert: methodService stepPoints size equals: 4.		
	self assert: (methodService source copyFrom: 42 to: 44) asSymbol equals: (methodService stepPoints at: 1) value. "#new"
	self assert: (methodService source copyFrom: 89 to: 91) asSymbol equals: (methodService stepPoints at: 2) value. "#new"
	self assert: (methodService source copyFrom: 66 to: 69) asSymbol equals: (methodService stepPoints at: 3) value. "#add:"
	self assert: (methodService source copyFrom: 121 to: 124) asSymbol equals: (methodService stepPoints at: 4) value. "#size"
]

{ #category : 'tests' }
RowanMethodServiceTest >> test_stepPoint2 [
	| classService methodService |
	classService := RowanClassService forClassNamed: self servicesDefaultClassName.
	classService saveMethodSource: self stepPoint2Source category: 'testing step points'.
	methodService := RowanMethodService forSelector: #simpleMethod2 class: classService classOrMeta meta: false organizer: ClassOrganizer new.
	self assert: methodService stepPoints size equals: 7.		
	self assert: (methodService source copyFrom: 43 to: 45) asSymbol equals: (methodService stepPoints at: 1) value. "#new"
	self assert: (methodService source copyFrom: 91 to: 104) asSymbol equals: (methodService stepPoints at: 2) value. "#forClassNamed:"
	self assert: (methodService source copyFrom: 67 to: 70) asSymbol equals: (methodService stepPoints at: 3) value. "#add:"
	self assert: (methodService source copyFrom: 145 to: 147) asSymbol equals: (methodService stepPoints at: 4) value. "#do:"
	self assert: (methodService source copyFrom: 295 to: 298) asSymbol equals: (methodService stepPoints at: 5) value. "#size"
	self assert: (methodService source copyFrom: 225 to: 234) asSymbol equals: (methodService stepPoints at: 6) value. "#stepPoints"
	self assert: (methodService source copyFrom: 268 to: 271) asSymbol equals: (methodService stepPoints at: 7) value. "#size"
]

{ #category : 'tests' }
RowanMethodServiceTest >> test_stepPoint3 [
	| classService methodService |
	classService := RowanClassService forClassNamed: self servicesDefaultClassName.
	classService saveMethodSource: self stepPoint3Source category: 'testing step points'.
	methodService := RowanMethodService forSelector: #'initialize:status:' class: classService classOrMeta meta: false organizer: ClassOrganizer new.
	self assert: methodService stepPoints size equals: 9.		
	self assert: (methodService source copyFrom: 109 to: 111) asSymbol equals: (methodService stepPoints at: 1) value. 
	self assert: (methodService stepPoints at: 1) value equals: #new.
	self assert: (methodService source copyFrom: 165 to: 174) asSymbol equals: (methodService stepPoints at: 2) value.
	self assert: (methodService stepPoints at: 2) value equals: #stackDepth.
	self assert: (methodService source copyFrom: 149 to: 152) asSymbol equals: (methodService stepPoints at: 3) value. 
	self assert: (methodService stepPoints at: 3) value equals: #new:.
	self assert: (methodService source copyFrom: 216 to: 225) asSymbol equals: (methodService stepPoints at: 4) value. 
	self assert: (methodService stepPoints at: 4) value equals: #stackDepth.
	self assert: (methodService source copyFrom: 290 to: 297) asSymbol equals: #process:. 
	self assert: (methodService stepPoints at: 5) value equals: #'process:level:organizer:'. 
	self assert: (methodService source copyFrom: 260 to: 262) asSymbol equals: #at:. 		
	self assert: (methodService stepPoints at: 6) value equals: #at:put:.

	"7 & 8 are present in 3.2.15 but are incorrect presumably due to optimization. Later server versions may get this fixed"
	
	self assert: (methodService source copyFrom: 392 to: 396) asSymbol equals: (methodService stepPoints at: 9) value. "#asOop"
self assert: (methodService stepPoints at: 9) value equals: #asOop.
]
