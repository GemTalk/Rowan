Class {
	#name : 'RwProjectLoadConfigurationVisitorV2',
	#superclass : 'Object',
	#instVars : [
		'packageNames',
		'visitedComponentNames',
		'platformAttributes',
		'definedGroupNames',
		'groupNames',
		'componentNames',
		'componentsRoot'
	],
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'instance creation' }
RwProjectLoadConfigurationVisitorV2 class >> new [

	^super new initialize

]

{ #category : 'private' }
RwProjectLoadConfigurationVisitorV2 >> _addPackageNames: somePackageNames for: aConfiguration [

	self packageNames addAll: somePackageNames

]

{ #category : 'private' }
RwProjectLoadConfigurationVisitorV2 >> _components: componentDirPath forProject: aProjectName [

	| urlBase |
	self componentNames isEmpty ifTrue: [ ^ #() ].
	urlBase := 'file:' ,componentDirPath asFileReference pathString, '/'.
	^ self componentNames
		collect: [ :componentName | 
			| url |
			url := urlBase , componentName , '.ston'.
			(RwAbstractProjectConfiguration fromUrl: url)
				projectName: aProjectName;
				yourself ]
]

{ #category : 'private' }
RwProjectLoadConfigurationVisitorV2 >> _matchPlatformAttributes: platformPatternMatcher [

	self platformAttributes do: [:anObject |
		(platformPatternMatcher match: anObject) ifTrue: [ ^true ] ].
	^false

]

{ #category : 'private' }
RwProjectLoadConfigurationVisitorV2 >> _platformAttributeMatchIn: platformMatchersList [

	platformMatchersList do: [:platformPatternMatcher |
		(self _matchPlatformAttributes: platformPatternMatcher) 
			ifTrue: [ ^true ] ].
	^false

]

{ #category : 'private' }
RwProjectLoadConfigurationVisitorV2 >> _processConditionalPackageNames: aProjectLoadConfiguration [

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
RwProjectLoadConfigurationVisitorV2 >> _processGroupNames [

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
RwProjectLoadConfigurationVisitorV2 >> _visited: aComponent [

	visitedComponentNames add:  aComponent name.
]

{ #category : 'accessing' }
RwProjectLoadConfigurationVisitorV2 >> componentNames [

	^ componentNames
]

{ #category : 'accessing' }
RwProjectLoadConfigurationVisitorV2 >> componentsRoot [
	^ componentsRoot
]

{ #category : 'accessing' }
RwProjectLoadConfigurationVisitorV2 >> componentsRoot: aStringOrFileReference [
	componentsRoot := aStringOrFileReference asFileReference
]

{ #category : 'accessing' }
RwProjectLoadConfigurationVisitorV2 >> groupNames [

	^ groupNames

]

{ #category : 'accessing' }
RwProjectLoadConfigurationVisitorV2 >> groupNames: aColl [

	groupNames := aColl copy

]

{ #category : 'initialization' }
RwProjectLoadConfigurationVisitorV2 >> initialize [
	packageNames := Set new.
	visitedComponentNames := Set new.
	componentNames := Set new.
	platformAttributes := #().
	groupNames := Set new
]

{ #category : 'accessing' }
RwProjectLoadConfigurationVisitorV2 >> packageNames [

	^ packageNames
]

{ #category : 'accessing' }
RwProjectLoadConfigurationVisitorV2 >> platformAttributes [

	^ platformAttributes

]

{ #category : 'accessing' }
RwProjectLoadConfigurationVisitorV2 >> platformAttributes: aColl [

	platformAttributes := aColl

]

{ #category : 'visiting' }
RwProjectLoadConfigurationVisitorV2 >> visit: aProjectComponent [

	^aProjectComponent acceptVisitor: self
]

{ #category : 'visiting' }
RwProjectLoadConfigurationVisitorV2 >> visitNested: aProjectComponent [

	^aProjectComponent acceptNestedVisitor: self
]

{ #category : 'visiting' }
RwProjectLoadConfigurationVisitorV2 >> visitProjectLoadComponent: aProjectLoadComponent [
	(visitedComponentNames includes: aProjectLoadComponent name)
		ifTrue: [ ^ self ].

	self _visited: aProjectLoadComponent.

	definedGroupNames := aProjectLoadComponent definedGroupNames.
	self _processGroupNames.

	self _processConditionalPackageNames: aProjectLoadComponent.

	self componentNames addAll: aProjectLoadComponent componentNames.
	(self
		_components: self componentsRoor
		forProject: aProjectLoadComponent projectName)
		do: [ :component | component acceptNestedVisitor: self ]
]
