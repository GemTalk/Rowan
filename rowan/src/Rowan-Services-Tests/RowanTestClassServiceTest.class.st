Class {
	#name : 'RowanTestClassServiceTest',
	#superclass : 'RowanClassServiceTest',
	#category : 'Rowan-Services-Tests'
}

{ #category : 'support' }
RowanTestClassServiceTest class >> shouldInheritSelectors [

	^true
]

{ #category : 'support' }
RowanTestClassServiceTest >> nonTestMethod [

	"do not remove. Used in test"
]

{ #category : 'tests' }
RowanTestClassServiceTest >> test_isTestCase [

	| classService |
	self jadeiteIssueTested: #issue253 withTitle: 'popup method menu in method list is slow for JadeServer methods'. 
	classService := RowanClassService forClassNamed: self class.
	self assert: classService isTestCase.
	classService isTestCase: false. 
	self deny: classService isTestCase. 
	classService setIsTestCaseCommand. 
	self assert: classService isTestCase.

	classService := RowanClassService forClassNamed: 'JadeServer'.
	self deny: classService isTestCase.
]

{ #category : 'tests' }
RowanTestClassServiceTest >> test_runClassService [

	self jadeiteIssueTested: #issue341 withTitle: 'Run sunit tests from class pass when they should not'.
]

{ #category : 'tests' }
RowanTestClassServiceTest >> test_setTestClass [

	| classService |
	classService := RowanClassService new name:  'RowanMethodService'. 
	self assert: classService isTestCase equals: nil.
	classService setIsTestCase. 
	self deny: classService isTestCase. 
	
	classService := RowanClassService new name:  'RowanTestClassServiceTest'. 
	self assert: classService isTestCase equals: nil.
	classService setIsTestCase. 
	self assert: classService isTestCase.
]

{ #category : 'tests' }
RowanTestClassServiceTest >> test_testSelectors [

	| classService |
	self jadeiteIssueTested: #issue341 withTitle: 'Run sunit tests from class pass when they should not'.
	classService := RowanClassService forClassNamed: 'RowanTestClassServiceTest'. 
	self deny: (classService tests detect:[:methodService | methodService selector = #test_testSelectors] ifNone:[]) equals: nil.
	self assert: (classService tests detect:[:methodService | methodService selector = #nonTestMethod] ifNone:[]) equals: nil.
]
