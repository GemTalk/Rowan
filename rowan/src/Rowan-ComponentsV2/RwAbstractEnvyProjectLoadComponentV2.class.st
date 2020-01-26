Class {
	#name : 'RwAbstractEnvyProjectLoadComponentV2',
	#superclass : 'RwAbstractSimpleProjectLoadComponentV2',
	#instVars : [
		'subApplicationNames'
	],
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'comparing' }
RwAbstractEnvyProjectLoadComponentV2 >> = aRwAbstractSimpleProjectLoadComponentV2 [
	^ super = aRwAbstractSimpleProjectLoadComponentV2
		and: [ 
			self subApplicationNames
				= aRwAbstractSimpleProjectLoadComponentV2 subApplicationNames ]
]

{ #category : 'visiting' }
RwAbstractEnvyProjectLoadComponentV2 >> acceptNestedVisitor: aVisitor [
	"an ENVY component may be nested inside of any other component"

	^ self acceptVisitor: aVisitor
]

{ #category : 'accessing' }
RwAbstractEnvyProjectLoadComponentV2 >> addSubApplicationNamed: aComponentName [
	"avoid duplicates and keep the list sorted"

	self subApplicationNames add: aComponentName.
	subApplicationNames := subApplicationNames asSet asArray sort
]

{ #category : 'private' }
RwAbstractEnvyProjectLoadComponentV2 >> conditionalPropertyMatchers [
	| conditionalPropertyMatchers |
	conditionalPropertyMatchers := Dictionary new
		at: {(self _platformPatternMatcherFor: self condition)} put: {};
		yourself.
	^ conditionalPropertyMatchers
]

{ #category : 'comparing' }
RwAbstractEnvyProjectLoadComponentV2 >> hash [
	^ super hash bitXor: self subApplicationNames hash
]

{ #category : 'initialization' }
RwAbstractEnvyProjectLoadComponentV2 >> initialize [
	super initialize.
	subApplicationNames := {}
]

{ #category : 'accessing' }
RwAbstractEnvyProjectLoadComponentV2 >> packageNamesForPlatformConfigurationAttributes: platformConfigurationAttributes groupNames: groupNames [
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
RwAbstractEnvyProjectLoadComponentV2 >> subApplicationNames [
	^ subApplicationNames
]
