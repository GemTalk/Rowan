Class {
	#name : 'RwProjectLoadConfigurationVisitor',
	#superclass : 'RwProjectConfigurationVisitor',
	#instVars : [
		'platformAttributes',
		'groupNames',
		'configurationNames',
		'configurationBasePath'
	],
	#category : 'Rowan-Configurations'
}

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
RwProjectLoadConfigurationVisitor >> _platformAttributeMatchIn: platformList [

	platformList do: [:platformPattern |
		(self platformAttributes includes: platformPattern) 
			ifTrue: [ ^true ] ].
	^false

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

	super initialize.
	configurationNames := Set new.
	platformAttributes := #().
	groupNames := #().
	configurationBasePath := ''

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
RwProjectLoadConfigurationVisitor >> visitProjectLoadConfiguration: aProjecLoadConfiguration [

	(visitedConfigurationNames includes: aProjecLoadConfiguration name)
		ifTrue: [ ^ self ].

	visitedConfigurationNames add:  aProjecLoadConfiguration name. 

	aProjecLoadConfiguration conditionalPackages keysAndValuesDo: [:platformList :groupMap | 
		(self _platformAttributeMatchIn: platformList)
			ifTrue: [
				groupMap keysAndValuesDo: [:group :map |
					(self groupNames includes: group)
						ifTrue: [ 
							self packageNames addAll: (map at: #packageNames).
							self configurationNames addAll: (map at: #configurationNames) ] ] ] ].

	(self _configurations: self configurationBasePath, '/')
		do: [:config | config acceptVisitor: self ].

]
