Class {
	#name : 'RowanClassServiceTest',
	#superclass : 'RowanServicesTest',
	#category : 'Rowan-Services-Tests'
}

{ #category : 'support' }
RowanClassServiceTest >> createClassNamed: className [

^Object rwSubclass: className
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	category: 'Rowan-Services-Tests'
	options: #()
]

{ #category : 'tests' }
RowanClassServiceTest >> testAddCategory [
	| classService className behavior |
	className := 'AddCategoryTest'. 
	behavior := self createClassNamed: className.
	self deny: (behavior categoryNames includes: 'fnoodle'). 
	classService := RowanClassService forClassNamed: className meta: false. 
	classService addCategory: 'fnoodle'.
	self assert: (behavior categoryNames includes: #fnoodle).
]
