Extension { #name : 'RwPrjReadTool' }

{ #category : '*rowan-tools-deprecated' }
RwPrjReadTool >> readProjectSetForProjectNamed: projectName withConfiguration: configName [

	| projectDefinition repo spec url config |
	self deprecated: 'RwPrjReadTool>>readProjectSetForProjectNamed:withConfiguration: deprecated in Rowan 1.3.0'.
	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	spec := projectDefinition specification.
	url := 'file:' , spec repositoryRootPath , '/' , spec configsPath , '/'
		, configName , '.ston'.
	[ config := RwAbstractProjectConfiguration fromUrl: url ]
		on: Error
		do: [ :ignored | 
			^ self readProjectSetForProjectNamed: projectName ].
	repo := projectDefinition repositoryUrl asRwRepository.
	config projectName: projectName.
	^ self 
		_readProjectSetForProjectDefinition: projectDefinition 
		packageNames: config resolvePackageNames 
		fromRepo: repo
]

{ #category : '*rowan-tools-deprecated' }
RwPrjReadTool >> validate: aRwSpecification [
	self deprecated: 'RwPrjReadTool>>validate: deprecated in Rowan 1.3.0'.
  super validate: aRwSpecification.
  self validateForRead: aRwSpecification
]
