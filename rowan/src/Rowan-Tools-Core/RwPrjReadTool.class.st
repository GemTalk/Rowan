Class {
	#name : 'RwPrjReadTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'private' }
RwPrjReadTool >> _readProjectDefinition: projectDefinition packageNames: packageNames fromRepo: repo [

	projectDefinition packageNames do: [:packageName | 
		(packageNames includes: packageName) 
			ifFalse: [ 
				"deleted package"
				projectDefinition removePackageNamed: packageName ] ].
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
	^ projectDefinition

]

{ #category : 'private' }
RwPrjReadTool >> _readProjectSetForProjectDefinition: projectDefinition packageNames: packageNames fromRepo: repo [

	self _readProjectDefinition: projectDefinition packageNames: packageNames fromRepo: repo.
	^ RwProjectSetDefinition new
		addProject: projectDefinition;
		yourself

]

{ #category : 'private' }
RwPrjReadTool >> _visitConfigurations: visitorClass forProjectDefinition: projectDefinition withConfigurations: configNames groupNames: groupNames [

	| spec projectName configDirectory visitor |
	spec := projectDefinition specification.
	projectName := projectDefinition name.
	configDirectory := spec repositoryRootPath , '/' , spec configsPath , '/'.
	(Rowan fileUtilities directoryExists: configDirectory)
		ifFalse: [ ^ nil ].
	visitor := visitorClass new
		platformAttributes: Rowan platformConfigurationAttributes;
		groupNames: groupNames;
		configurationBasePath: configDirectory;
		yourself.
	configNames do: [:configName |
		| config url |
		url := 'file:' , configDirectory, configName , '.ston'.
		config := RwAbstractProjectConfiguration fromUrl: url.
		config projectName: projectName.
		visitor visit: config ].
	^visitor

]

{ #category : 'read configurations' }
RwPrjReadTool >> readConfigurationsForProjectDefinition: projectDefinition withConfigurations: configNames groupNames: groupNames [

	^ self 
		_visitConfigurations: RwProjectLoadConfigurationVisitor 
		forProjectDefinition: projectDefinition 
		withConfigurations: configNames 
		groupNames: groupNames

]

{ #category : 'read project definitions' }
RwPrjReadTool >> readProjectDefinition: projectDefinition [
	"read all of the packages in the existing Rowan project associated with the given project definition"

	|  repo packageNames |
	repo := projectDefinition repositoryUrl asRwRepository.
	packageNames := repo packageNames.
	^ self 
		_readProjectDefinition: projectDefinition 
		packageNames: packageNames
		fromRepo: repo

]

{ #category : 'read project definitions' }
RwPrjReadTool >> readProjectSetForProjectDefinition: projectDefinition [

	| repo spec packageNames |
	spec := projectDefinition specification.
	spec loadedConfigurationNames
		ifNotNil: [:configNames |
			"project has been loaded previously, use the loaded config and group names"
			^ self 
					readProjectSetForProjectDefinition: projectDefinition
					withConfigurations: configNames
					groupNames: spec loadedGroupNames ].
	spec defaultConfigurationNames
		ifNotNil: [:configNames |
			"project has not been loaded previously use the default config and group names from the spec"
			^ self 
					readProjectSetForProjectDefinition: projectDefinition
					withConfigurations: configNames
					groupNames: spec defaultGroupNames ].
	"no configuration associated with the project, load all of the packages in the repository"
	repo := projectDefinition repositoryUrl asRwRepository.
	packageNames := repo packageNames.
	^ self 
		_readProjectSetForProjectDefinition: projectDefinition 
		packageNames: packageNames
		fromRepo: repo

]

{ #category : 'read project definitions' }
RwPrjReadTool >> readProjectSetForProjectDefinition: projectDefinition withConfigurations: configNames groupNames: groupNames [

	| projectName repo spec packageNames projectSetDefinition packageMapSpecs visitor |
	projectName := projectDefinition name.
	spec := projectDefinition specification.
	repo := projectDefinition repositoryUrl asRwRepository.
	visitor := self readConfigurationsForProjectDefinition: projectDefinition withConfigurations: configNames groupNames: groupNames.
	visitor 
		ifNotNil: [ 
			packageNames := visitor packageNames.
			packageMapSpecs := visitor packageMapSpecs ]
		ifNil: [ 
			packageNames := projectDefinition packageNames.
			packageMapSpecs := Dictionary new ].	
	projectSetDefinition := self 
		_readProjectSetForProjectDefinition: projectDefinition 
		packageNames: packageNames 
		fromRepo: repo.
	((projectSetDefinition properties at: 'loadedProjectInfo' ifAbsentPut: [Dictionary new])
		at: projectName ifAbsentPut: [ Dictionary new ])
			at: 'loadedConfigurationNames' put: configNames;
			at: 'loadedGroupNames' put: groupNames;
			at: 'packageMapSpecs' put: packageMapSpecs.
	^ projectSetDefinition

]

{ #category : 'read project by name' }
RwPrjReadTool >> readProjectSetForProjectNamed: projectName [

	| projectDefinition |
	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	^ self readProjectSetForProjectDefinition: projectDefinition

]

{ #category : 'deprecation candidates' }
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

{ #category : 'read project by name' }
RwPrjReadTool >> readProjectSetForProjectNamed: projectName withConfigurations: configNames groupNames: groupNames [

	| projectDefinition |
	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	^ self readProjectSetForProjectDefinition: projectDefinition withConfigurations: configNames groupNames: groupNames

]

{ #category : 'validation' }
RwPrjReadTool >> validate: aRwSpecification [
  super validate: aRwSpecification.
  self validateForRead: aRwSpecification
]
