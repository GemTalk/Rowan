Class {
	#name : 'RowanTestClassServiceTest',
	#superclass : 'RowanClassServiceTest',
	#category : 'Rowan-Services-Tests'
}

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
