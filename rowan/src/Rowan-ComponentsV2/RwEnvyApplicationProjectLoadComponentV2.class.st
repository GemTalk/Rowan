Class {
	#name : 'RwEnvyApplicationProjectLoadComponentV2',
	#superclass : 'RwAbstractEnvyProjectLoadComponentV2',
	#instVars : [
		'applicationNames'
	],
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'comparing' }
RwEnvyApplicationProjectLoadComponentV2 >> = aRwAbstractSimpleProjectLoadComponentV2 [
	^ super = aRwAbstractSimpleProjectLoadComponentV2
		and: [ 
			self applicationNames
				= aRwAbstractSimpleProjectLoadComponentV2 applicationNames ]
]

{ #category : 'visiting' }
RwEnvyApplicationProjectLoadComponentV2 >> acceptVisitor: aVisitor [
	"an ENVY component may be nested inside of any other component"

	^ aVisitor visitEnvyApplicationProjectLoadComponent: self
]

{ #category : 'accessing' }
RwEnvyApplicationProjectLoadComponentV2 >> addApplicationNamed: aComponentName [
	"avoid duplicates and keep the list sorted"

	self applicationNames add: aComponentName.
	applicationNames := applicationNames asSet asArray sort
]

{ #category : 'accessing' }
RwEnvyApplicationProjectLoadComponentV2 >> addSubApplicationNamed: aComponentName [
	"avoid duplicates and keep the list sorted"

	self subApplicationNames add: aComponentName.
	subApplicationNames := subApplicationNames asSet asArray sort
]

{ #category : 'accessing' }
RwEnvyApplicationProjectLoadComponentV2 >> applicationNames [
	^ applicationNames
]

{ #category : 'comparing' }
RwEnvyApplicationProjectLoadComponentV2 >> hash [
	^ super hash bitXor: self applicationNames hash
]

{ #category : 'initialization' }
RwEnvyApplicationProjectLoadComponentV2 >> initialize [
	super initialize.
	applicationNames := {}
]
