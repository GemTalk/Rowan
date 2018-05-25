Class {
	#name : 'RwPrjReadTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'private' }
RwPrjReadTool >> _readProjectSetForProjectDefinition: projectDefinition packageNames: packageNames fromRepo: repo [

	packageNames
		do: [ :packageName | 
			| structureName reader packageDefinition |
			structureName := packageName , repo packageExtension.
			reader := repo reader.
			reader
				packageStructure: (RwCypressPackageStructure named: structureName);
				packageDirectory:
						(reader fileUtils
								directoryFromPath: structureName
								relativeTo: repo directoryPath);
				readPackageStructure.
			packageDefinition := reader packageStructure rwSnapshot.
			projectDefinition addOrUpdatePackage: packageDefinition ].
	^ RwProjectSetDefinition new
		addProject: projectDefinition;
		yourself

]

{ #category : 'smalltalk api' }
RwPrjReadTool >> readProjectSetForProjectNamed: projectName [

	| projectDefinition repo spec packageNames |
	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	spec := projectDefinition specification.
	spec loadedConfigurationName
		ifNotNil: [:configName |
			"project has been loaded previously, use the loaded config and group names"
			^ self 
					readProjectSetForProjectNamed: projectName
					withConfiguration: configName
					groupNames: spec loadedGroupNames ].
	spec defaultConfigurationName
		ifNotNil: [:configName |
			"project has not been loaded previously use the default config and group names from the spec"
			^ self 
					readProjectSetForProjectNamed: projectName
					withConfiguration: configName
					groupNames: spec defaultGroupNames ].
	"no configuration associated with the project, load all of the packages in the repository"
	repo := projectDefinition repositoryUrl asRwRepository.
	packageNames := repo packageNames.
	^ self 
		_readProjectSetForProjectDefinition: projectDefinition 
		packageNames: packageNames
		fromRepo: repo

]

{ #category : 'smalltalk api' }
RwPrjReadTool >> readProjectSetForProjectNamed: projectName withConfiguration: configName [

	| projectDefinition repo spec url config |
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

{ #category : 'smalltalk api' }
RwPrjReadTool >> readProjectSetForProjectNamed: projectName withConfiguration: configName groupNames: groupNames [

	| projectDefinition repo spec url config |
	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	spec := projectDefinition specification.
	url := 'file:' , spec repositoryRootPath , '/' , spec configsPath , '/'
		, configName , '.ston'.
	config := RwAbstractProjectConfiguration fromUrl: url.
	repo := projectDefinition repositoryUrl asRwRepository.
	config projectName: projectName.
	^ self 
		_readProjectSetForProjectDefinition: projectDefinition 
		packageNames: config resolvePackageNames 
		fromRepo: repo

]

{ #category : 'validation' }
RwPrjReadTool >> validate: aRwSpecification [
  super validate: aRwSpecification.
  self validateForRead: aRwSpecification
]
