Class {
	#name : 'RwAbstractProjectComponentVisitorV2',
	#superclass : 'Object',
	#instVars : [
		'projectLoadSpecs',
		'readComponents',
		'readProjects',
		'visitedComponents',
		'visitedComponentNames',
		'platformConditionalAttributes',
		'definedGroupNames',
		'projectNames',
		'groupNames',
		'componentNames'
	],
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'instance creation' }
RwAbstractProjectComponentVisitorV2 class >> new [

	^super new initialize

]

{ #category : 'private' }
RwAbstractProjectComponentVisitorV2 >> _addPackageNames: somePackageNames for: aComponent [

	self subclassResponsibility: #_addPackageNames:for:

]

{ #category : 'private' }
RwAbstractProjectComponentVisitorV2 >> _matchPlatformAttributes: platformPatternMatcher [

	self platformConditionalAttributes do: [:anObject |
		(platformPatternMatcher match: anObject) ifTrue: [ ^true ] ].
	^false
]

{ #category : 'private' }
RwAbstractProjectComponentVisitorV2 >> _platformAttributeMatchIn: platformMatchersList [

	platformMatchersList do: [:platformPatternMatcher |
		(self _matchPlatformAttributes: platformPatternMatcher) 
			ifTrue: [ ^true ] ].
	^false

]

{ #category : 'private' }
RwAbstractProjectComponentVisitorV2 >> _processConditionalProperties: aProjectLoadConfiguration [
	aProjectLoadConfiguration conditionalPropertyMatchers
		keysAndValuesDo: [ :platformMatchers :groupMap | 
			(self _platformAttributeMatchIn: platformMatchers)
				ifTrue: [ 
					groupMap
						keysAndValuesDo: [ :group :map | 
							self
								_addPackageNames: (map at: #'packageNames' ifAbsent: [ #() ])
								for: aProjectLoadConfiguration.
							self componentNames addAll: (map at: #'componentNames' ifAbsent: [ #() ]).
							self projectNames addAll: (map at: #'projectNames' ifAbsent: [ #() ]) ] ] ]
]

{ #category : 'private' }
RwAbstractProjectComponentVisitorV2 >> _visited: aComponent [

	visitedComponentNames add:  aComponent name.
	visitedComponents at: aComponent name put: aComponent.
]

{ #category : 'accessing' }
RwAbstractProjectComponentVisitorV2 >> componentNames [

	^ componentNames
]

{ #category : 'accessing' }
RwAbstractProjectComponentVisitorV2 >> componentsPath [
	^ self subclassResponsibility: #'componentsPath'
]

{ #category : 'initialization' }
RwAbstractProjectComponentVisitorV2 >> initialize [
	visitedComponentNames := Set new.
	projectNames := Set new.
	componentNames := Set new.
	readComponents := Dictionary new.
	readProjects := Dictionary new.
	platformConditionalAttributes := #().
	projectLoadSpecs := Set new.
	visitedComponents := Dictionary new
]

{ #category : 'accessing' }
RwAbstractProjectComponentVisitorV2 >> packageNames [

	self subclassResponsibility: #packageNames
]

{ #category : 'accessing' }
RwAbstractProjectComponentVisitorV2 >> platformConditionalAttributes [

	^ platformConditionalAttributes
]

{ #category : 'accessing' }
RwAbstractProjectComponentVisitorV2 >> platformConditionalAttributes: aColl [

	platformConditionalAttributes := aColl
]

{ #category : 'accessing' }
RwAbstractProjectComponentVisitorV2 >> projectLoadSpecs [

	^ projectLoadSpecs
]

{ #category : 'accessing' }
RwAbstractProjectComponentVisitorV2 >> projectNames [

	^ projectNames
]

{ #category : 'accessing' }
RwAbstractProjectComponentVisitorV2 >> projectsPath [
	^ self subclassResponsibility: #'projectsPath'
]

{ #category : 'accessing' }
RwAbstractProjectComponentVisitorV2 >> readComponents [

	^ readComponents
]

{ #category : 'accessing' }
RwAbstractProjectComponentVisitorV2 >> readProjects [

	^ readProjects
]

{ #category : 'visiting' }
RwAbstractProjectComponentVisitorV2 >> visit: aProjectLoadComponent [

	^aProjectLoadComponent acceptVisitor: self
]

{ #category : 'accessing' }
RwAbstractProjectComponentVisitorV2 >> visitedComponents [

	^ visitedComponents
]

{ #category : 'visiting' }
RwAbstractProjectComponentVisitorV2 >> visitLoadSpecification: aLoadSpecification [

	self projectLoadSpecs add: aLoadSpecification
]

{ #category : 'visiting' }
RwAbstractProjectComponentVisitorV2 >> visitNested: aProjectLoadComponent [

	^aProjectLoadComponent acceptNestedVisitor: self
]

{ #category : 'visiting' }
RwAbstractProjectComponentVisitorV2 >> visitSimpleProjectLoadComponent: aSimpleProjectLoadComponent [
	(visitedComponentNames includes: aSimpleProjectLoadComponent name)
		ifTrue: [ ^ self ].

	self _visited: aSimpleProjectLoadComponent.

	aSimpleProjectLoadComponent conditionalPropertyMatchers
		keysAndValuesDo: [ :platformMatchers :ignored | 
			(self _platformAttributeMatchIn: platformMatchers)
				ifTrue: [ 
					self
						_addPackageNames: aSimpleProjectLoadComponent packageNames
						for: aSimpleProjectLoadComponent.
					self componentNames addAll: aSimpleProjectLoadComponent componentNames.
					self projectNames addAll: aSimpleProjectLoadComponent projectNames ] ].

	(self
		_components: self componentsPath
		forProject: aSimpleProjectLoadComponent projectName)
		do: [ :component | 
			(visitedComponentNames includes: component name)
				ifFalse: [ component acceptNestedVisitor: self ] ].

	(self
		_projects: self projectsPath
		forProject: aSimpleProjectLoadComponent projectName)
		do: [ :projectSpec | projectSpec acceptVisitor: self ]

]
