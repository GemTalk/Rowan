Class {
	#name : 'RwAbstractSimpleProjectLoadComponentV2',
	#superclass : 'RwBasicProjectLoadComponentV2',
	#instVars : [
		'packageNames',
		'condition'
	],
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'comparing' }
RwAbstractSimpleProjectLoadComponentV2 >> = aRwAbstractSimpleProjectLoadComponentV2 [
	(aRwAbstractSimpleProjectLoadComponentV2 isKindOf: self class)
		ifFalse: [ ^ false ].
	^ (((self name = aRwAbstractSimpleProjectLoadComponentV2 name
		and: [ self comment = aRwAbstractSimpleProjectLoadComponentV2 comment ])
		and: [ 
			self conditionalPackageMapSpecs
				= aRwAbstractSimpleProjectLoadComponentV2 conditionalPackageMapSpecs ])
		and: [ self condition = aRwAbstractSimpleProjectLoadComponentV2 condition ])
		and: [ self packageNames = aRwAbstractSimpleProjectLoadComponentV2 packageNames ]
]

{ #category : 'accessing' }
RwAbstractSimpleProjectLoadComponentV2 >> addPackageNames: aPackageNames [
	"add packages to default conditional/group if applicable"

	self packageNames addAll: aPackageNames.
	packageNames := packageNames asSet asArray sort
]

{ #category : 'accessing' }
RwAbstractSimpleProjectLoadComponentV2 >> condition [
	^condition
]

{ #category : 'accessing' }
RwAbstractSimpleProjectLoadComponentV2 >> condition: aString [
	false
		ifTrue: [ 
			aString isString
				ifFalse: [ self error: 'The condition is constrained to be a string' ] ].
	condition := aString
]

{ #category : 'comparing' }
RwAbstractSimpleProjectLoadComponentV2 >> hash [
	| hashValue |
	hashValue := self name hash.
	hashValue := hashValue bitXor: self comment hash.
	hashValue := hashValue bitXor: self conditionalPackageMapSpecs hash.
	hashValue := hashValue bitXor: self packageNames hash.
	^ hashValue
]

{ #category : 'initialization' }
RwAbstractSimpleProjectLoadComponentV2 >> initialize [
	super initialize.
	condition := 'common'.
	packageNames := {}
]

{ #category : 'accessing' }
RwAbstractSimpleProjectLoadComponentV2 >> packageNames [

	^ packageNames
]

{ #category : 'accessing' }
RwAbstractSimpleProjectLoadComponentV2 >> referencePath [
	^ Path from: self name
]

{ #category : 'accessing' }
RwAbstractSimpleProjectLoadComponentV2 >> removeComponentNamed: aComponentName [
	self subclassResponsibility: #'removeComponentNamed:'
]

{ #category : 'accessing' }
RwAbstractSimpleProjectLoadComponentV2 >> removePackageNamed: aPackageName [
	self packageNames remove: aPackageName ifAbsent: [  ]
]

{ #category : 'accessing' }
RwAbstractSimpleProjectLoadComponentV2 >> removeProjectNamed: aProjectName [
	self subclassResponsibility: #'removeProjectNamed:'
]
