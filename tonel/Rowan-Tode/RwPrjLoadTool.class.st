Class {
	#name : 'RwPrjLoadTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tode'
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
		loadedPackageDefinitionsNamed: packageSetToLoad packageNamesInLoadOrder.
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

{ #category : 'man page' }
RwPrjLoadTool >> manPage [
	^ TDManPage
		commandName: 'load'
		sourceMethod: self class name asString , '>>manPage'
		fromString:
			'NAME
  prj load - Read project from repository and install (disk -> image)
SYNOPSIS
  prj load [--loads=<load-expression>] <spec-url>
DESCRIPTION
EXAMPLES

  # first download and clone the project, then load
  prj clone --gitRoot=$GS_HOME/shared/repos \
    https://raw.githubusercontent.com/dalehenrich/CypressReferenceImplementation/rowan/Rowan.ston
  prj load Rowan

  prj load Rowan										# load default packages
  prj load --loads=ALL Rowan						# load all packages
  prj load --loads=`#( ''Rowan-Tode'')` Rowan		# load Rowan-Tode package in addition 
'
]

{ #category : 'public' }
RwPrjLoadTool >> packageTools [

	^self platform packageTools
]
