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
	spec loadedConfigurationNames
		ifNotNil: [:configNames |
			"project has been loaded previously, use the loaded config and group names"
			^ self 
					readProjectSetForProjectNamed: projectName
					withConfigurations: configNames
					groupNames: spec loadedGroupNames ].
	spec defaultConfigurationNames
		ifNotNil: [:configNames |
			"project has not been loaded previously use the default config and group names from the spec"
			^ self 
					readProjectSetForProjectNamed: projectName
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

{ #category : 'smalltalk api' }
RwPrjReadTool >> readProjectSetForProjectNamed: projectName withConfigurations: configNames groupNames: groupNames [

	| projectDefinition repo spec url packageNames  configDirectory |
	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	spec := projectDefinition specification.
	repo := projectDefinition repositoryUrl asRwRepository.
	configDirectory := spec repositoryRootPath , '/' , spec configsPath , '/'.
	(Rowan fileUtilities directoryExists: configDirectory)
		ifTrue: [ 
			| visitor configs thePackageMapSpecs gemstoneSpec |
			visitor := RwProjectLoadConfigurationVisitor new
				platformAttributes: Rowan platformConfigurationAttributes;
				groupNames: groupNames;
				configurationBasePath: configDirectory;
				yourself.
			configs := configNames collect: [:configName |
				| config |
				url := 'file:' , configDirectory, configName , '.ston'.
				config := RwAbstractProjectConfiguration fromUrl: url.
				config projectName: projectName.
				visitor visit: config ].
			packageNames := visitor packageNames.
			thePackageMapSpecs := visitor packageMapSpecs.
			gemstoneSpec := spec platformSpec at: 'gemstone'.
			(thePackageMapSpecs at: #defaultSymbolDictName otherwise: nil) 
				ifNotNil: [:name | gemstoneSpec defaultSymbolDictName: name ].
			(thePackageMapSpecs at: #defaultUseSessionMethodsForExtensions otherwise: nil) 
				ifNotNil: [:boolean | 
					gemstoneSpec defaultUseSessionMethodsForExtensions: boolean  ].
			(thePackageMapSpecs at: #packageNameToPlatformPropertiesMap otherwise: nil) 
				ifNotNil: [:map | 
					map keysAndValuesDo: [:packageName :propMap | 
						propMap keysAndValuesDo: [:propertyName :propertyValue |
							propertyName = 'symbolDictName'
								ifTrue: [ gemstoneSpec symbolDictName: propertyValue forPackageNamed: packageName ].
							propertyName = 'userId'
								ifTrue: [ gemstoneSpec userId: propertyValue forPackageNamed: packageName ].
							propertyName = 'useSessionMethodsForExtensions'
								ifTrue: [ gemstoneSpec useSessionMethodsForExtensions: propertyValue forPackageNamed: packageName ] ] ] ] ]
		ifFalse: [ packageNames := projectDefinition packageNames ].	
	^ self 
		_readProjectSetForProjectDefinition: projectDefinition 
		packageNames: packageNames 
		fromRepo: repo

]

{ #category : 'validation' }
RwPrjReadTool >> validate: aRwSpecification [
  super validate: aRwSpecification.
  self validateForRead: aRwSpecification
]
