Class {
	#name : 'RwPrjLoadTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools'
}

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
RwPrjLoadTool >> loadProjectNamed: projectNamed withConfiguration: configName [

	"load the named project from disk"

	| projectSetDefinition |
	projectSetDefinition := self class read
		readProjectSetForProjectNamed: projectNamed
		withConfiguration: configName.
	^ self loadProjectSetDefinition: projectSetDefinition
]

{ #category : 'smalltalk api' }
RwPrjLoadTool >> loadProjectSetDefinition: projectSetDefinitionToLoad [

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
	Rowan image applyModification: diff.
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
