Class {
	#name : 'RwProjectLoadConfigurationVisitorV2',
	#superclass : 'Object',
	#instVars : [
		'packageNames',
		'packageMapSpecs',
		'visitedComponentNames',
		'platformAttributes',
		'definedGroupNames',
		'groupNames',
		'componentNames',
		'componentsRoot'
	],
	#category : 'Rowan-Configurations'
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
RwProjectLoadConfigurationVisitorV2 >> _processConditionalPackageMapSpecs: aProjectLoadConfiguration [

	| currentUserId |
	currentUserId := Rowan image currentUserId.
	aProjectLoadConfiguration conditionalPackageMapSpecMatchers keysAndValuesDo: [:platformMatcher :thePackageMapSpecsMap | 
		(self _platformAttributeMatchIn: {platformMatcher})
			ifTrue: [
				(thePackageMapSpecsMap 
					at: currentUserId 
					ifAbsent: [ 
						"<allusers> is preferred, but for the alpha, there are outstanding configs that use <default>"
						thePackageMapSpecsMap at: 'default'  
							ifAbsent: [ thePackageMapSpecsMap at: 'allusers'  ifAbsent: [] ] ])
								ifNotNil: [:thePackageMapSpecs |
									(thePackageMapSpecs at: #defaultSymbolDictName otherwise: nil) 
										ifNotNil: [:name | packageMapSpecs at: #defaultSymbolDictName put: name ].
									(thePackageMapSpecs at: #defaultUseSessionMethodsForExtensions otherwise: nil) 
										ifNotNil: [:boolean | packageMapSpecs at: #defaultUseSessionMethodsForExtensions put: boolean  ].
									(thePackageMapSpecs at: #packageNameToPlatformPropertiesMap otherwise: nil) 
										ifNotNil: [:theMap | 
											| map |
											map := packageMapSpecs at: #packageNameToPlatformPropertiesMap ifAbsentPut: [ Dictionary new ].
											theMap keysAndValuesDo: [:thePackageName :thePropMap | 
												thePropMap keysAndValuesDo: [:thePropertyName :thePropertyValue |
													| packageMap |
													packageMap  := map at: thePackageName ifAbsentPut: [ Dictionary new ].
													(#('symbolDictName' 'userId' 'useSessionMethodsForExtensions' ) includes: thePropertyName)
														ifTrue: [ packageMap at: thePropertyName put: thePropertyValue  ] 
														ifFalse: [ self error: 'Unrecognized property name ', thePropertyName printString, ' in package name to properties map' ] ] ] ] ] ] ]

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
	groupNames := Set new.
	packageMapSpecs := Dictionary new.
]

{ #category : 'accessing' }
RwProjectLoadConfigurationVisitorV2 >> packageMapSpecs [

	^ packageMapSpecs

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
RwProjectLoadConfigurationVisitorV2 >> visitNestedProjectLoadComponent: aNestedProjectLoadComponent [

	(visitedComponentNames includes: aNestedProjectLoadComponent name)
		ifTrue: [ ^ self ].

	self _visited: aNestedProjectLoadComponent. 

	self _processConditionalPackageNames: aNestedProjectLoadComponent.

	self _processConditionalPackageMapSpecs: aNestedProjectLoadComponent.

	self visitedComponentNames addAll: aNestedProjectLoadComponent componentNames.
	(self _components: self componentRoot forProject: aNestedProjectLoadComponent projectName)
		do: [:component | component acceptNestedVisitor: self ].
]

{ #category : 'visiting' }
RwProjectLoadConfigurationVisitorV2 >> visitProjectLoadComponent: aProjectLoadComponent [

	(visitedComponentNames includes: aProjectLoadComponent name)
		ifTrue: [ ^ self ].

	self _visited: aProjectLoadComponent. 

	definedGroupNames := aProjectLoadComponent definedGroupNames.
	self _processGroupNames.

	self _processConditionalPackageNames: aProjectLoadComponent.

	self _processConditionalPackageMapSpecs: aProjectLoadComponent.

	self componentNames addAll: aProjectLoadComponent componentNames.
	(self _components: self componentsRoor forProject: aProjectLoadComponent projectName)
		do: [:component | component acceptNestedVisitor: self ].
]
