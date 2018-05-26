Class {
	#name : 'RwProjectLoadConfiguration',
	#superclass : 'RwAbstractProjectLoadConfiguration',
	#instVars : [
		'definedGroupNames',
		'configurationNames'
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
RwProjectLoadConfiguration >> configurationNames [

	^configurationNames ifNil: [ configurationNames := {} ]

]

{ #category : 'accessing' }
RwProjectLoadConfiguration >> definedGroupNames [

	^definedGroupNames ifNil: [ definedGroupNames := {} ]

]
