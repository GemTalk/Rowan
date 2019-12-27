Class {
	#name : 'RwAbstractProjectComponentVisitorV2',
	#superclass : 'Object',
	#instVars : [
		'projectLoadSpecs',
		'visitedComponents',
		'visitedComponentNames',
		'platformAttributes',
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

	self platformAttributes do: [:anObject |
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
RwAbstractProjectComponentVisitorV2 >> _processConditionalPackageNames: aProjectLoadConfiguration [

	aProjectLoadConfiguration conditionalPackageMatchers keysAndValuesDo: [:platformMatchers :groupMap | 
		(self _platformAttributeMatchIn: platformMatchers)
			ifTrue: [
				groupMap keysAndValuesDo: [:group :map |
					(self groupNames includes: group)
						ifTrue: [ 
							self _addPackageNames: (map at: #packageNames ifAbsent: [ #() ]) for: aProjectLoadConfiguration.
							self componentNames addAll: (map at: #componentNames ifAbsent: [ #()] ) ] ] ] ].

]

{ #category : 'private' }
RwAbstractProjectComponentVisitorV2 >> _processGroupNames [

	| seedGroupNames seeded |
	seedGroupNames := groupNames asSet copy.
	seeded := Set new.
	[seedGroupNames isEmpty ]
		whileFalse: [ 
			seedGroupNames copy do: [:groupName |
				| referencedGroupNames |
				"make sure that required groups are included in group names, recursively"
				seedGroupNames remove: groupName.
				referencedGroupNames := definedGroupNames at: groupName ifAbsent: [ #() ].
				groupNames addAll: referencedGroupNames.
				referencedGroupNames do: [:refGroupName |
					(seeded includes: refGroupName)
						ifFalse: [
						"ensure that we seed each group only once"
						seeded add: refGroupName.
						seedGroupNames add: refGroupName ] ] ] ].
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

{ #category : 'accessing' }
RwAbstractProjectComponentVisitorV2 >> groupNames [

	^ groupNames

]

{ #category : 'accessing' }
RwAbstractProjectComponentVisitorV2 >> groupNames: aColl [

	groupNames := aColl copy

]

{ #category : 'initialization' }
RwAbstractProjectComponentVisitorV2 >> initialize [
	visitedComponentNames := Set new.
	projectNames := Set new.
	componentNames := Set new.
	platformAttributes := #().
	groupNames := Set new.
	projectLoadSpecs := Set new.
	visitedComponents := Dictionary new
]

{ #category : 'accessing' }
RwAbstractProjectComponentVisitorV2 >> packageNames [

	self subclassResponsibility: #packageNames
]

{ #category : 'accessing' }
RwAbstractProjectComponentVisitorV2 >> platformAttributes [

	^ platformAttributes

]

{ #category : 'accessing' }
RwAbstractProjectComponentVisitorV2 >> platformAttributes: aColl [

	platformAttributes := aColl

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
RwAbstractProjectComponentVisitorV2 >> visitProjectLoadComponent: aProjectLoadComponent [

	(visitedComponentNames includes: aProjectLoadComponent name)
		ifTrue: [ ^ self ].

	self _visited: aProjectLoadComponent.

	definedGroupNames := aProjectLoadComponent definedGroupNames.
	self _processGroupNames.

	self _processConditionalPackageNames: aProjectLoadComponent.

	self componentNames addAll: aProjectLoadComponent componentNames.
	(self
		_components: self componentsPath
		forProject: aProjectLoadComponent projectName)
		do: [ :component | component acceptNestedVisitor: self ].

	self projectNames addAll: aProjectLoadComponent projectNames.
	(self
		_projects: self projectsPath
		forProject: aProjectLoadComponent projectName)
		do: [ :projectSpec | projectSpec acceptVisitor: self ]
]
