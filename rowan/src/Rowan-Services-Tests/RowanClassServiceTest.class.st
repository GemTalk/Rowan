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

{ #category : 'tests' }
RowanClassServiceTest >> testClassComment [
	| classService className behavior |
	className := 'ClassCommentTest'. 
	behavior := self createClassNamed: className.
	self assert: behavior comment equals: String new. 
	classService := RowanClassService forClassNamed: className meta: false. 
	classService classComment: 'This is a test'. 
	self assert: behavior comment equals: 'This is a test'.
]

{ #category : 'tests' }
RowanClassServiceTest >> testClassFromName [
	"always return thisClass" 
	| classService |
	classService := RowanClassService forClassNamed: 'Array' meta: false.
	self assert: classService classFromName equals: Array.
	classService := RowanClassService forClassNamed: 'OrderedCollection' meta: true.
	self assert: classService classFromName equals: OrderedCollection
]

{ #category : 'tests' }
RowanClassServiceTest >> testClassHierarchy [
	"return class hierarchy in format client can use.
	#nil -> #(Object class service)
	Object class service -> #(RowanService class service) 
	etc"
  
	| classService hierarchy objectClassService rowanServiceService |
	classService := RowanClassService forClassNamed: 'RowanClassService'.
	hierarchy := classService classHierarchy hierarchyServices. 
	self assert: (hierarchy isKindOf: Dictionary). 
	self assert: ((hierarchy at: #nil) isKindOf: Array).
	self assert: (hierarchy at: #nil) size equals: 1. 
	objectClassService := (hierarchy at: #nil) first.
	self assert: (objectClassService isKindOf: RowanClassService).
	self assert: objectClassService name equals: 'Object'.
	rowanServiceService := (hierarchy at: objectClassService) first. 
	self assert: rowanServiceService name equals: 'RowanService'.
	self assert: (hierarchy at: rowanServiceService) first name equals: 'RowanClassService'.
]
