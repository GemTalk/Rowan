Class {
	#name : 'RwPrjLoadTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools'
}

{ #category : 'image' }
RwPrjLoadTool >> doUpdateLoadedPackageNames: aPackageNames [

	^ specification imageSpec loadedPackageNames: aPackageNames
]

{ #category : 'smalltalk api' }
RwPrjLoadTool >> loadPackageDefinition: packageDefinition specification: aRwSpecification [

	^ self
		loadPackageDefinitions: {packageDefinition}
		specification: aRwSpecification
]

{ #category : 'smalltalk api' }
RwPrjLoadTool >> loadPackageDefinitions: packageDefinitions specification: aRwSpecification [

	| packageSetToLoad |
	self specification: aRwSpecification.
	packageSetToLoad := RwPackageSetDefinition new.
	packageDefinitions do: [ :def | packageSetToLoad addPackage: def ].
	^ self
		loadPackageSetDefinition: packageSetToLoad
		specification: aRwSpecification
]

{ #category : 'smalltalk api' }
RwPrjLoadTool >> loadPackageSetDefinition: packageSetToLoad specification: aRwSpecification [

	| loadedPackageSet diff |
	self specification: aRwSpecification.
	loadedPackageSet := Rowan
		loadedPackageDefinitionsNamed: packageSetToLoad packageNames.
	diff := packageSetToLoad compareAgainstBase: loadedPackageSet.
	diff isEmpty
		ifTrue: [ 
			| msg |
			msg := 'The project ' , specification specName , ' is already up to date'.
			self inform: msg.
			^ msg ].
	Rowan image applyModification: diff for: aRwSpecification.
	specification
		updateLoadedCommitIdForTool: self;
		updateLoadedPackageNamesTool: self
			packageNames:
				(Rowan loadedPackageDefinitionsNamed: packageSetToLoad packageNames)
						packageNames.
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
			project specification
				updateLoadedCommitIdForTool: self;
				updateLoadedPackageNamesTool: self
					packageNames:
						(Rowan loadedPackageDefinitionsNamed: project packageNames forProject: project)
								definitionNames ].
	^ diff
]

{ #category : 'smalltalk api' }
RwPrjLoadTool >> loadSpecUrl: aString [
  ^ self loadSpecification: (RwSpecification fromUrl: aString)
]
