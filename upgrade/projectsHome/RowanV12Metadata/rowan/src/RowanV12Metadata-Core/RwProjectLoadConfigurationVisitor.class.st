Class {
	#name : 'RwProjectLoadConfigurationVisitor',
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
	#category : 'RowanV12Metadata-Core'
}

{ #category : 'instance creation' }
RwProjectLoadConfigurationVisitor class >> new [

	^super new initialize

]

{ #category : 'private' }
RwProjectLoadConfigurationVisitor >> _addPackageNames: somePackageNames for: aConfiguration [

	self packageNames addAll: somePackageNames

]

{ #category : 'private' }
RwProjectLoadConfigurationVisitor >> _configurations: configDirPath forProject: aProjectName [

	| urlBase |
	self configurationNames isEmpty ifTrue: [ ^ #() ].
	urlBase := 'file:' ,configDirPath.
	^ self configurationNames
		collect: [ :configName | 
			| url |
			url := urlBase , configName , '.ston'.
			(RwAbstractProjectConfiguration fromUrl: url)
				projectName: aProjectName;
				yourself ]

]

{ #category : 'private' }
RwProjectLoadConfigurationVisitor >> _matchPlatformAttributes: platformPatternMatcher [

	self platformAttributes do: [:anObject |
		(platformPatternMatcher match: anObject) ifTrue: [ ^true ] ].
	^false

]

{ #category : 'private' }
RwProjectLoadConfigurationVisitor >> _platformAttributeMatchIn: platformMatchersList [

	platformMatchersList do: [:platformPatternMatcher |
		(self _matchPlatformAttributes: platformPatternMatcher) 
			ifTrue: [ ^true ] ].
	^false

]

{ #category : 'private' }
RwProjectLoadConfigurationVisitor >> _processConditionalPackageMapSpecs: aProjectLoadConfiguration [

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
RwProjectLoadConfigurationVisitor >> _processConditionalPackageNames: aProjectLoadConfiguration [

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
RwProjectLoadConfigurationVisitor >> _visited: aConfiguration [

	visitedConfigurationNames add:  aConfiguration name.

]

{ #category : 'accessing' }
RwProjectLoadConfigurationVisitor >> configurationBasePath [

	^ configurationBasePath

]

{ #category : 'accessing' }
RwProjectLoadConfigurationVisitor >> configurationBasePath: aString [

	configurationBasePath := aString

]

{ #category : 'accessing' }
RwProjectLoadConfigurationVisitor >> configurationNames [

	^ configurationNames

]

{ #category : 'accessing' }
RwProjectLoadConfigurationVisitor >> groupNames [

	^ groupNames

]

{ #category : 'accessing' }
RwProjectLoadConfigurationVisitor >> groupNames: aColl [

	groupNames := aColl copy

]

{ #category : 'initialization' }
RwProjectLoadConfigurationVisitor >> initialize [

	packageNames := Set new.
	visitedConfigurationNames := Set new.
	configurationNames := Set new.
	platformAttributes := #().
	groupNames := {}.
	configurationBasePath := ''.
	packageMapSpecs := Dictionary new.

]

{ #category : 'accessing' }
RwProjectLoadConfigurationVisitor >> packageMapSpecs [

	^ packageMapSpecs

]

{ #category : 'accessing' }
RwProjectLoadConfigurationVisitor >> packageNames [

	^ packageNames

]

{ #category : 'accessing' }
RwProjectLoadConfigurationVisitor >> platformAttributes [

	^ platformAttributes

]

{ #category : 'accessing' }
RwProjectLoadConfigurationVisitor >> platformAttributes: aColl [

	platformAttributes := aColl

]

{ #category : 'visiting' }
RwProjectLoadConfigurationVisitor >> visit: aProjectConfiguration [

	^aProjectConfiguration acceptVisitor: self

]

{ #category : 'visiting' }
RwProjectLoadConfigurationVisitor >> visitNestedProjectLoadConfiguration: aNestedProjectLoadConfiguration [

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
RwProjectLoadConfigurationVisitor >> visitProjectCompoundConfiguration: aProjectCompoundConfiguration [

	(visitedConfigurationNames includes: aProjectCompoundConfiguration name)
		ifTrue: [ ^ self ].

	self _visited: aProjectCompoundConfiguration. 

	self _addPackageNames: aProjectCompoundConfiguration packageNames for: aProjectCompoundConfiguration.

	aProjectCompoundConfiguration _configurations
		do: [:config | config acceptCompoundVisitor: self ].

]

{ #category : 'visiting' }
RwProjectLoadConfigurationVisitor >> visitProjectLoadConfiguration: aProjectLoadConfiguration [

	| seedGroupNames |
	(visitedConfigurationNames includes: aProjectLoadConfiguration name)
		ifTrue: [ ^ self ].

	self _visited: aProjectLoadConfiguration. 

	definedGroupNames := aProjectLoadConfiguration definedGroupNames.
	seedGroupNames := groupNames asSet copy.
	[seedGroupNames isEmpty ]
		whileFalse: [ 
			seedGroupNames copy do: [:groupName |
				| referencedGroupNames |
				"make sure that required groups are included in group names, recursively"
				seedGroupNames remove: groupName.
				referencedGroupNames := definedGroupNames at: groupName ifAbsent: [ #() ].
				groupNames addAll: referencedGroupNames.
				seedGroupNames addAll: referencedGroupNames ] ].

	self _processConditionalPackageNames: aProjectLoadConfiguration.

	self _processConditionalPackageMapSpecs: aProjectLoadConfiguration.

	self configurationNames addAll: aProjectLoadConfiguration configurationNames.
	(self _configurations: self configurationBasePath, '/' forProject: aProjectLoadConfiguration projectName)
		do: [:config | config acceptNestedVisitor: self ].

]
