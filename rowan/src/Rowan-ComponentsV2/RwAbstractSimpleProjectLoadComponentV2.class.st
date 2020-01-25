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

{ #category : 'private' }
RwAbstractSimpleProjectLoadComponentV2 >> conditionalPropertyMatchers [
	| conditionalPropertyMatchers |
	conditionalPropertyMatchers := Dictionary new
		at: {(self _platformPatternMatcherFor: self condition)} put: {groupName};
		yourself.
	^ conditionalPropertyMatchers
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
	hashValue := hashValue bitXor: self comment hash.
	hashValue := hashValue bitXor: self conditionalPackageMapSpecs hash.
	hashValue := hashValue bitXor: self groupName hash.
	hashValue := hashValue bitXor: self componentNames hash.
	^ hashValue
]

{ #category : 'initialization' }
RwAbstractSimpleProjectLoadComponentV2 >> initialize [
	super initialize.
	condition := 'common'.
	groupName := 'core'.
	componentNames := {}.
]

{ #category : 'accessing' }
RwAbstractSimpleProjectLoadComponentV2 >> packageNamesForPlatformConfigurationAttributes: platformConfigurationAttributes groupNames: groupNames [
	"Answer the collection of package names defined in the receiver."

	| allDefinedPackageNames matchers |
	allDefinedPackageNames := Set new.
	matchers := self conditionalPropertyMatchers.
	platformConfigurationAttributes
		do: [ :anObject | 
			matchers
				keysAndValuesDo: [ :ar :theGroupNames | 
					ar
						do: [ :matcher | 
							(matcher match: anObject)
								ifTrue: [ 
									(theGroupNames includes: groupName)
										ifTrue: [ allDefinedPackageNames addAll: self packageNames ] ] ] ] ].
	^ allDefinedPackageNames
]
