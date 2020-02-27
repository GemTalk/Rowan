Class {
	#name : 'RwAbstractRowanProjectLoadComponentV2',
	#superclass : 'RwAbstractSimpleProjectLoadComponentV2',
	#instVars : [
		'componentNames'
	],
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'comparing' }
RwAbstractRowanProjectLoadComponentV2 >> = aRwAbstractSimpleProjectLoadComponentV2 [
	^ super = aRwAbstractSimpleProjectLoadComponentV2
		and: [ self componentNames = aRwAbstractSimpleProjectLoadComponentV2 componentNames ]
]

{ #category : 'accessing' }
RwAbstractRowanProjectLoadComponentV2 >> addComponentNamed: aComponentName [ 
	"avoid duplicates and keep the list sorted"
	self componentNames add: aComponentName.
	componentNames := componentNames asSet asArray sort
]

{ #category : 'accessing' }
RwAbstractRowanProjectLoadComponentV2 >> addComponentNames: aComponentNames [
	"add component names"

	self componentNames addAll: aComponentNames.
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
		at: {(self _platformPatternMatcherFor: self condition)} put: {};
		yourself.
	^ conditionalPropertyMatchers
]

{ #category : 'comparing' }
RwAbstractRowanProjectLoadComponentV2 >> hash [
	^ super hash bitXor: self componentNames hash
]

{ #category : 'initialization' }
RwAbstractRowanProjectLoadComponentV2 >> initialize [
	super initialize.
	componentNames := {}
]

{ #category : 'accessing' }
RwAbstractRowanProjectLoadComponentV2 >> packageNamesForPlatformConfigurationAttributes: platformConfigurationAttributes [
	"Answer the collection of package names defined in the receiver."

	| allDefinedPackageNames matchers |
	allDefinedPackageNames := Set new.
	matchers := self conditionalPropertyMatchers.
	platformConfigurationAttributes
		do: [ :anObject | 
			matchers
				keysAndValuesDo: [ :ar :ignored | 
					ar
						do: [ :matcher | 
							(matcher match: anObject)
								ifTrue: [ allDefinedPackageNames addAll: self packageNames ] ] ] ].
	^ allDefinedPackageNames
]

{ #category : 'accessing' }
RwAbstractRowanProjectLoadComponentV2 >> removeComponentNamed: aComponentName [
	self componentNames remove: aComponentName ifAbsent: [  ]
]
