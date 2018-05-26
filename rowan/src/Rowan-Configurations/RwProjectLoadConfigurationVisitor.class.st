Class {
	#name : 'RwProjectLoadConfigurationVisitor',
	#superclass : 'Object',
	#instVars : [
		'packageNames',
		'packageMapSpecs',
		'visitedConfigurationNames',
		'platformAttributes',
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
RwProjectLoadConfigurationVisitor >> _configurations: configDirPath [

	| urlBase |
	self configurationNames isEmpty ifTrue: [ ^ #() ].
	urlBase := 'file:' ,configDirPath.
	^ self configurationNames
		collect: [ :configName | 
			| url |
			url := urlBase , configName , '.ston'.
			(RwAbstractProjectConfiguration fromUrl: url)
				projectName: self projectName;
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
RwProjectLoadConfigurationVisitor >> _processConditionalPackageNames: aProjectLoadConfiguraiton [

	aProjectLoadConfiguraiton conditionalPackageMatchers keysAndValuesDo: [:platformMatchers :groupMap | 
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

{ #category : 'visiting' }
RwProjectLoadConfigurationVisitor >> visit: aProjectConfiguration [

	^aProjectConfiguration acceptVisitor: self

]

{ #category : 'visiting' }
RwProjectLoadConfigurationVisitor >> visitNestedProjectLoadConfiguration: aNestedProjecLoadConfiguration [

	(visitedConfigurationNames includes: aNestedProjecLoadConfiguration name)
		ifTrue: [ ^ self ].

	visitedConfigurationNames add:  aNestedProjecLoadConfiguration name. 

	self _processConditionalPackageNames: aNestedProjecLoadConfiguration.

	(self _configurations: self configurationBasePath, '/')
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
RwProjectLoadConfigurationVisitor >> visitProjectLoadConfiguration: aProjecLoadConfiguration [

	| currentUserId |
	(visitedConfigurationNames includes: aProjecLoadConfiguration name)
		ifTrue: [ ^ self ].

	visitedConfigurationNames add:  aProjecLoadConfiguration name. 

	self _processConditionalPackageNames: aProjecLoadConfiguration.

	currentUserId := Rowan image currentUserId.
	aProjecLoadConfiguration conditionalPackageMapSpecMatchers keysAndValuesDo: [:platformMatcher :packageMapSpecsMap | 
		(self _platformAttributeMatchIn: {platformMatcher})
			ifTrue: [
				packageMapSpecs ifNotNil: [ self halt ].
				packageMapSpecs := packageMapSpecsMap 
					at: currentUserId 
					ifAbsent: [ packageMapSpecsMap at: 'default'  ifAbsent: [] ] ] ].

	(self _configurations: self configurationBasePath, '/')
		do: [:config | config acceptNestedVisitor: self ].

]
