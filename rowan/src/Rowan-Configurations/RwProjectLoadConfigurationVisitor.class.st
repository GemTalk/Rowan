Class {
	#name : 'RwProjectLoadConfigurationVisitor',
	#superclass : 'Object',
	#instVars : [
		'packageNames',
		'packageMapSpecs',
		'visitedConfigurationNames',
		'platformAttributes',
		'projectSpec',
		'definedGroupNames',
		'groupNames',
		'configurationNames',
		'configurationBasePath'
	],
	#category : 'Rowan-Configurations'
}

{ #category : 'instance creation' }
RwProjectLoadConfigurationVisitor class >> new [

	^super new initialize

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
	aProjectLoadConfiguration conditionalPackageMapSpecMatchers keysAndValuesDo: [:platformMatcher :packageMapSpecsMap | 
		(self _platformAttributeMatchIn: {platformMatcher})
			ifTrue: [
				(packageMapSpecsMap 
					at: currentUserId 
					ifAbsent: [ packageMapSpecsMap at: 'default'  ifAbsent: [] ])
						ifNotNil: [:thePackageMapSpecs |
							| platformSpec |
							platformSpec := (self projectSpec platformSpec at: 'gemstone').
							(thePackageMapSpecs at: #defaultSymbolDictName otherwise: nil) 
								ifNotNil: [:name | platformSpec defaultSymbolDictName: name ].
							(thePackageMapSpecs at: #defaultUseSessionMethodsForExtensions otherwise: nil) 
								ifNotNil: [:boolean | 
									platformSpec defaultUseSessionMethodsForExtensions: boolean  ].
							(thePackageMapSpecs at: #packageNameToPlatformPropertiesMap otherwise: nil) 
								ifNotNil: [:map | 
									map keysAndValuesDo: [:packageName :propMap | 
										propMap keysAndValuesDo: [:propertyName :propertyValue |
											propertyName = 'symbolDictName'
												ifTrue: [ platformSpec symbolDictName: propertyValue forPackageNamed: packageName ].
											propertyName = 'userId'
												ifTrue: [ platformSpec userId: propertyValue forPackageNamed: packageName ].
											propertyName = 'useSessionMethodsForExtensions'
												ifTrue: [ platformSpec useSessionMethodsForExtensions: propertyValue forPackageNamed: packageName ] ] ] ] ] ] ]

]

{ #category : 'private' }
RwProjectLoadConfigurationVisitor >> _processConditionalPackageNames: aProjectLoadConfiguration [

	aProjectLoadConfiguration conditionalPackageMatchers keysAndValuesDo: [:platformMatchers :groupMap | 
		(self _platformAttributeMatchIn: platformMatchers)
			ifTrue: [
				groupMap keysAndValuesDo: [:group :map |
					(self groupNames includes: group)
						ifTrue: [ 
							self packageNames addAll: (map at: #packageNames).
							self configurationNames addAll: (map at: #configurationNames) ] ] ] ].

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

	groupNames := aColl

]

{ #category : 'initialization' }
RwProjectLoadConfigurationVisitor >> initialize [

	packageNames := Set new.
	visitedConfigurationNames := Set new.
	configurationNames := Set new.
	platformAttributes := #().
	groupNames := #().
	configurationBasePath := ''

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

{ #category : 'accessing' }
RwProjectLoadConfigurationVisitor >> projectSpec [

	^ projectSpec

]

{ #category : 'accessing' }
RwProjectLoadConfigurationVisitor >> projectSpec: aSpec [

	projectSpec := aSpec

]

{ #category : 'visiting' }
RwProjectLoadConfigurationVisitor >> visit: aProjectConfiguration [

	^aProjectConfiguration acceptVisitor: self

]

{ #category : 'visiting' }
RwProjectLoadConfigurationVisitor >> visitNestedProjectLoadConfiguration: aNestedProjectLoadConfiguration [

	(visitedConfigurationNames includes: aNestedProjectLoadConfiguration name)
		ifTrue: [ ^ self ].

	visitedConfigurationNames add:  aNestedProjectLoadConfiguration name. 

	self _processConditionalPackageNames: aNestedProjectLoadConfiguration.

	self _processConditionalPackageMapSpecs: aNestedProjectLoadConfiguration.

	(self _configurations: self configurationBasePath, '/' forProject: aNestedProjectLoadConfiguration projectName)
		do: [:config | config acceptNestedVisitor: self ].

	self configurationNames addAll: aNestedProjectLoadConfiguration configurationNames.
	(self _configurations: self configurationBasePath, '/' forProject: aNestedProjectLoadConfiguration projectName)
		do: [:config | config acceptNestedVisitor: self ].

]

{ #category : 'visiting' }
RwProjectLoadConfigurationVisitor >> visitProjectCompoundConfiguration: aProjectCompoundConfiguration [

	(visitedConfigurationNames includes: aProjectCompoundConfiguration name)
		ifTrue: [ ^ self ].

	visitedConfigurationNames add:  aProjectCompoundConfiguration name. 
	packageNames addAll: aProjectCompoundConfiguration packageNames.

	aProjectCompoundConfiguration _configurations
		do: [:config | config acceptCompoundVisitor: self ].

]

{ #category : 'visiting' }
RwProjectLoadConfigurationVisitor >> visitProjectLoadConfiguration: aProjectLoadConfiguration [

	(visitedConfigurationNames includes: aProjectLoadConfiguration name)
		ifTrue: [ ^ self ].

	visitedConfigurationNames add:  aProjectLoadConfiguration name. 

	definedGroupNames := aProjectLoadConfiguration definedGroupNames.
	groupNames := groupNames asSet.
	self groupNames copy do: [:groupName |
		"make sure that required groups are included in group names"
		groupNames addAll: (definedGroupNames at: groupName ifAbsent: [ #() ]) ].

	self _processConditionalPackageNames: aProjectLoadConfiguration.

	self _processConditionalPackageMapSpecs: aProjectLoadConfiguration.

	self configurationNames addAll: aProjectLoadConfiguration configurationNames.
	(self _configurations: self configurationBasePath, '/' forProject: aProjectLoadConfiguration projectName)
		do: [:config | config acceptNestedVisitor: self ].

]
