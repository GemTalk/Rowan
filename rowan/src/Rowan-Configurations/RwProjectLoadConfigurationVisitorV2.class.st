Class {
	#name : 'RwProjectLoadConfigurationVisitorV2',
	#superclass : 'Object',
	#instVars : [
		'packageNames',
		'packageMapSpecs',
		'visitedConfigurationNames',
		'platformAttributes',
		'definedGroupNames',
		'groupNames',
		'configurationNames',
		'configurationBasePath'
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
RwProjectLoadConfigurationVisitorV2 >> _configurations: configDirPath forProject: aProjectName [

	| urlBase |
	self configurationNames isEmpty ifTrue: [ ^ #() ].
	urlBase := 'file:' ,configDirPath asFileReference pathString, '/'.
	^ self configurationNames
		collect: [ :configName | 
			| url |
			url := urlBase , configName , '.ston'.
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
							self configurationNames addAll: (map at: #configurationNames ifAbsent: [ #()] ) ] ] ] ].

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
RwProjectLoadConfigurationVisitorV2 >> _visited: aConfiguration [

	visitedConfigurationNames add:  aConfiguration name.

]

{ #category : 'accessing' }
RwProjectLoadConfigurationVisitorV2 >> configurationBasePath [

	^ configurationBasePath

]

{ #category : 'accessing' }
RwProjectLoadConfigurationVisitorV2 >> configurationBasePath: aString [

	configurationBasePath := aString

]

{ #category : 'accessing' }
RwProjectLoadConfigurationVisitorV2 >> configurationNames [

	^ configurationNames

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
	visitedConfigurationNames := Set new.
	configurationNames := Set new.
	platformAttributes := #().
	groupNames := Set new.
	configurationBasePath := ''.
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
RwProjectLoadConfigurationVisitorV2 >> visit: aProjectConfiguration [

	^aProjectConfiguration acceptVisitor: self

]

{ #category : 'visiting' }
RwProjectLoadConfigurationVisitorV2 >> visitNested: aProjectConfiguration [

	^aProjectConfiguration acceptNestedVisitor: self
]

{ #category : 'visiting' }
RwProjectLoadConfigurationVisitorV2 >> visitNestedProjectLoadConfiguration: aNestedProjectLoadConfiguration [

	(visitedConfigurationNames includes: aNestedProjectLoadConfiguration name)
		ifTrue: [ ^ self ].

	self _visited: aNestedProjectLoadConfiguration. 

	self _processConditionalPackageNames: aNestedProjectLoadConfiguration.

	self _processConditionalPackageMapSpecs: aNestedProjectLoadConfiguration.

	self configurationNames addAll: aNestedProjectLoadConfiguration configurationNames.
	(self _configurations: self configurationBasePath, '/' forProject: aNestedProjectLoadConfiguration projectName)
		do: [:config | config acceptNestedVisitor: self ].

]

{ #category : 'visiting' }
RwProjectLoadConfigurationVisitorV2 >> visitProjectCompoundConfiguration: aProjectCompoundConfiguration [

	(visitedConfigurationNames includes: aProjectCompoundConfiguration name)
		ifTrue: [ ^ self ].

	self _visited: aProjectCompoundConfiguration. 

	self _addPackageNames: aProjectCompoundConfiguration packageNames for: aProjectCompoundConfiguration.

	aProjectCompoundConfiguration _configurations
		do: [:config | config acceptCompoundVisitor: self ].

]

{ #category : 'visiting' }
RwProjectLoadConfigurationVisitorV2 >> visitProjectLoadConfiguration: aProjectLoadConfiguration [

	(visitedConfigurationNames includes: aProjectLoadConfiguration name)
		ifTrue: [ ^ self ].

	self _visited: aProjectLoadConfiguration. 

	definedGroupNames := aProjectLoadConfiguration definedGroupNames.
	self _processGroupNames.

	self _processConditionalPackageNames: aProjectLoadConfiguration.

	self _processConditionalPackageMapSpecs: aProjectLoadConfiguration.

	self configurationNames addAll: aProjectLoadConfiguration configurationNames.
	(self _configurations: self configurationBasePath, '/' forProject: aProjectLoadConfiguration projectName)
		do: [:config | config acceptNestedVisitor: self ].
]
