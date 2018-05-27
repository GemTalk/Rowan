Class {
	#name : 'RwProjectLoadConfiguration',
	#superclass : 'RwAbstractProjectLoadConfiguration',
	#instVars : [
		'definedGroupNames'
	],
	#category : 'Rowan-Configurations'
}

{ #category : 'visiting' }
RwProjectLoadConfiguration >> acceptVisitor: aVisitor [

	^aVisitor visitProjectLoadConfiguration: self

]

{ #category : 'accessing' }
RwProjectLoadConfiguration >> conditionalPackageMapSpecMatchers [

	conditionalPackageMapSpecMatchers ifNotNil: [:val | ^ val ]. 
	conditionalPackageMapSpecMatchers := Dictionary new.
	self conditionalPackageMapSpecs keysAndValuesDo: [:platformPattern :packageMapSpecsMap |
		conditionalPackageMapSpecMatchers
			at: (self _platformPatternMatcherFor: platformPattern)
			put: packageMapSpecsMap ].
	^ conditionalPackageMapSpecMatchers

]

{ #category : 'accessing' }
RwProjectLoadConfiguration >> conditionalPackageMapSpecs [

	^conditionalPackageMapSpecs ifNil: [ conditionalPackageMapSpecs := Dictionary new ]

]

{ #category : 'accessing' }
RwProjectLoadConfiguration >> definedGroupNames [

	^definedGroupNames ifNil: [ definedGroupNames := {} ]

]

{ #category : 'accessing' }
RwProjectLoadConfiguration >> definedGroupNames: aColl [

	definedGroupNames := aColl

]
