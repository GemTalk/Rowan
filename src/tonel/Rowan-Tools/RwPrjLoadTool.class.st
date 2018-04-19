Class {
	#name : 'RwPrjLoadTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools'
}

{ #category : 'private' }
RwPrjLoadTool >> _bootstrapLoadProjectNamed: projectNamed withConfiguration: configName [

	"load the named project from disk"

	| projectSetDefinition |
	projectSetDefinition := self class read
		readProjectSetForProjectNamed: projectNamed
		withConfiguration: configName.
	^ self _bootstrapLoadProjectSetDefinition: projectSetDefinition instanceMigrator: Rowan platform instanceMigrator

]

{ #category : 'private' }
RwPrjLoadTool >> _bootstrapLoadProjectSetDefinition: projectSetDefinitionToLoad instanceMigrator: instanceMigrator [

	| loadedProjectSet diff |
	loadedProjectSet := projectSetDefinitionToLoad deriveLoadedThings
		asProjectDefinitionSet.
	diff := projectSetDefinitionToLoad compareAgainstBase: loadedProjectSet.
	diff isEmpty
		ifTrue: [ 
			| msg |
			msg := 'The projects are already up to date'.
			self inform: msg.
			^ msg ].
	[ Rowan image _bootstrapApplyModification: diff instanceMigrator: instanceMigrator ]
		on: RwExistingAssociationWithSameKeyNotification
		do: [:ex | ex resume ].
	projectSetDefinitionToLoad definitions
		do: [ :project | 
			self specification: project specification.
			project specification updateLoadedCommitIdForTool: self ].
	^ diff

]

{ #category : 'smalltalk api' }
RwPrjLoadTool >> loadProjectDefinition: projectDefinition [

	| projectSetDefinition |
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.

	^ self loadProjectSetDefinition: projectSetDefinition
]

{ #category : 'smalltalk api' }
RwPrjLoadTool >> loadProjectNamed: projectNamed [

	"load the named project from disk"

	| projectSetDefinition |
	projectSetDefinition := self class read
		readProjectSetForProjectNamed: projectNamed.
	^ self loadProjectSetDefinition: projectSetDefinition
]

{ #category : 'smalltalk api' }
RwPrjLoadTool >> loadProjectNamed: projectNamed instanceMigrator: instanceMigrator [

	"load the named project from disk"

	| projectSetDefinition |
	projectSetDefinition := self class read
		readProjectSetForProjectNamed: projectNamed.
	^ self
		loadProjectSetDefinition: projectSetDefinition
		instanceMigrator: instanceMigrator
]

{ #category : 'smalltalk api' }
RwPrjLoadTool >> loadProjectNamed: projectNamed withConfiguration: configName [

	"load the named project from disk"

	| projectSetDefinition |
	projectSetDefinition := self class read
		readProjectSetForProjectNamed: projectNamed
		withConfiguration: configName.
	^ self loadProjectSetDefinition: projectSetDefinition
]

{ #category : 'smalltalk api' }
RwPrjLoadTool >> loadProjectNamed: projectNamed withConfiguration: configName instanceMigrator: instanceMigrator [

	"load the named project from disk"

	| projectSetDefinition |
	projectSetDefinition := self class read
		readProjectSetForProjectNamed: projectNamed
		withConfiguration: configName.
	^ self
		loadProjectSetDefinition: projectSetDefinition
		instanceMigrator: instanceMigrator
]

{ #category : 'smalltalk api' }
RwPrjLoadTool >> loadProjectSetDefinition: projectSetDefinitionToLoad [

	^ self
		loadProjectSetDefinition: projectSetDefinitionToLoad
		instanceMigrator: Rowan platform instanceMigrator
]

{ #category : 'smalltalk api' }
RwPrjLoadTool >> loadProjectSetDefinition: projectSetDefinitionToLoad instanceMigrator: instanceMigrator [

	| loadedProjectSet diff |
	loadedProjectSet := projectSetDefinitionToLoad deriveLoadedThings
		asProjectDefinitionSet.
	diff := projectSetDefinitionToLoad compareAgainstBase: loadedProjectSet.
	diff isEmpty
		ifTrue: [ 
			| msg |
			msg := 'The projects are already up to date'.
			self inform: msg.
			^ msg ].
	Rowan image applyModification: diff instanceMigrator: instanceMigrator.
	projectSetDefinitionToLoad definitions
		do: [ :project | 
			self specification: project specification.
			project specification updateLoadedCommitIdForTool: self ].
	^ diff
]

{ #category : 'smalltalk api' }
RwPrjLoadTool >> loadSpecUrl: aString [
  ^ self loadSpecification: (RwSpecification fromUrl: aString)
]
