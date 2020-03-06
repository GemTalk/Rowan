Class {
	#name : 'RwAbstractRowanProjectLoadComponentV2',
	#superclass : 'RwAbstractSimpleProjectLoadComponentV2',
	#instVars : [
		'componentNames',
		'groupName'
	],
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'comparing' }
RwAbstractRowanProjectLoadComponentV2 >> = aRwAbstractSimpleProjectLoadComponentV2 [
	^ super = aRwAbstractSimpleProjectLoadComponentV2
		and: [ 
			self componentNames = aRwAbstractSimpleProjectLoadComponentV2 componentNames
				and: [ self groupName = aRwAbstractSimpleProjectLoadComponentV2 groupName ] ]
]

{ #category : 'accessing' }
RwAbstractRowanProjectLoadComponentV2 >> addComponentNamed: aComponentName [ 
	"avoid duplicates and keep the list sorted"
	self componentNames add: aComponentName.
	componentNames := componentNames asSet asArray sort
]

{ #category : 'accessing' }
RwAbstractRowanProjectLoadComponentV2 >> componentNames [

	^ componentNames
]

{ #category : 'private' }
RwAbstractRowanProjectLoadComponentV2 >> conditionalPropertyMatchers [
	| conditionalPropertyMatchers |
	conditionalPropertyMatchers := Dictionary new
		at: {(self _platformPatternMatcherFor: self condition)} put: {groupName};
		yourself.
	^ conditionalPropertyMatchers
]

{ #category : 'accessing' }
RwAbstractRowanProjectLoadComponentV2 >> groupName [
	^groupName
]

{ #category : 'accessing' }
RwAbstractRowanProjectLoadComponentV2 >> groupName: object [
	groupName := object
]

{ #category : 'comparing' }
RwAbstractRowanProjectLoadComponentV2 >> hash [
	| hashValue |
	hashValue := super hash bitXor: self groupName hash.
	^ hashValue bitXor: self componentNames hash
]

{ #category : 'initialization' }
RwAbstractRowanProjectLoadComponentV2 >> initialize [
	super initialize.
	componentNames := {}
]

{ #category : 'accessing' }
RwAbstractRowanProjectLoadComponentV2 >> packageNamesForPlatformConfigurationAttributes: platformConfigurationAttributes groupNames: groupNames [
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

{ #category : 'accessing' }
RwAbstractRowanProjectLoadComponentV2 >> removeComponentNamed: aComponentName [
	self componentNames remove: aComponentName ifAbsent: [  ]
]
