Class {
	#name : 'RwAbstractSimpleProjectLoadComponentV2',
	#superclass : 'RwBasicProjectLoadComponentV2',
	#instVars : [
		'condition',
		'groupName',
		'componentNames'
	],
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'comparing' }
RwAbstractSimpleProjectLoadComponentV2 >> = aRwAbstractSimpleProjectLoadComponentV2 [
	(aRwAbstractSimpleProjectLoadComponentV2 isKindOf: self class)
		ifFalse: [ ^ false ].
	^ ((((self name = aRwAbstractSimpleProjectLoadComponentV2 name
		and: [ self comment = aRwAbstractSimpleProjectLoadComponentV2 comment ])
		and: [ 
			self conditionalPackageMapSpecs
				= aRwAbstractSimpleProjectLoadComponentV2 conditionalPackageMapSpecs ])
		and: [ self condition = aRwAbstractSimpleProjectLoadComponentV2 condition ])
		and: [ self groupName = aRwAbstractSimpleProjectLoadComponentV2 groupName ])
		and: [ self componentNames = aRwAbstractSimpleProjectLoadComponentV2 componentNames ]
]

{ #category : 'accessing' }
RwAbstractSimpleProjectLoadComponentV2 >> addComponentNamed: aComponentName [ 
	"avoid duplicates and keep the list sorted"
	self componentNames add: aComponentName.
	componentNames := componentNames asSet asArray sort
]

{ #category : 'accessing' }
RwAbstractSimpleProjectLoadComponentV2 >> componentNames [

	^ componentNames
]

{ #category : 'accessing' }
RwAbstractSimpleProjectLoadComponentV2 >> condition [
	^condition
]

{ #category : 'accessing' }
RwAbstractSimpleProjectLoadComponentV2 >> condition: object [
	condition := object
]

{ #category : 'accessing' }
RwAbstractSimpleProjectLoadComponentV2 >> groupName [
	^groupName
]

{ #category : 'accessing' }
RwAbstractSimpleProjectLoadComponentV2 >> groupName: object [
	groupName := object
]

{ #category : 'comparing' }
RwAbstractSimpleProjectLoadComponentV2 >> hash [
	| hashValue |
	hashValue := self name hash.
	hashValue := hashValue bitXor: self comment.
	hashValue := hashValue bitXor: self conditionalPackageMapSpecs.
	hashValue := hashValue bitXor: self groupName.
	hashValue := hashValue bitXor: self componentNames.
	^ hashValue
]

{ #category : 'initialization' }
RwAbstractSimpleProjectLoadComponentV2 >> initialize [
	super initialize.
	condition := 'common'.
	groupName := 'core'.
	componentNames := {}.
]
