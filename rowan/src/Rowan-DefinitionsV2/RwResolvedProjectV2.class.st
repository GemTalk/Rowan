Class {
	#name : 'RwResolvedProjectV2',
	#superclass : 'RwAbstractResolvedProjectV2',
	#category : 'Rowan-DefinitionsV2'
}

{ #category : 'instance creation' }
RwResolvedProjectV2 class >> basicLoadSpecification: anRwLoadSpecificationV2 [
	"resolve ensures that the project directory already exists on disk (cloned for git projects) or created on disk for new projects
		answer  the project definition specified by the receiver and any dependent projects"

	"if the project directory already exists on disk, then read the project definition(s) from disk"

	| loadSpecification projectDefinition |
	loadSpecification := anRwLoadSpecificationV2 copy.
	projectDefinition := RwProjectDefinitionV2 basicNew
		properties:
				(Dictionary new
						add: 'name' -> loadSpecification projectName;
						yourself);
		packages: Dictionary new;
		components: RwResolvedLoadComponentsV2 new;
		projectDefinitionSourceProperty: RwLoadedProject _projectModifiedProjectSourceValue;
		yourself.
	^ self basicNew
		_projectDefinition: projectDefinition;
		_loadSpecification: loadSpecification;
		yourself
]

{ #category : 'instance creation' }
RwResolvedProjectV2 class >> loadSpecification: anRwLoadSpecificationV2 [
	"resolve ensures that the project directory already exists on disk (cloned for git projects) or created on disk for new projects
		answer  the project definition specified by the receiver and any dependent projects"

	"if the project directory already exists on disk, then read the project definition(s) from disk"

	^(self basicLoadSpecification: anRwLoadSpecificationV2)
		resolve
]

{ #category : 'instance creation' }
RwResolvedProjectV2 class >> loadSpecification: anRwLoadSpecificationV2 platformAttributes: platformAttributes [
	"resolve ensures that the project directory already exists on disk (cloned for git projects) or created on disk for new projects
		answer  the project definition specified by the receiver and any dependent projects"

	"if the project directory already exists on disk, then read the project definition(s) from disk"

	^ (self basicLoadSpecification: anRwLoadSpecificationV2)
		resolve: platformAttributes
]

{ #category : 'private' }
RwResolvedProjectV2 >> _checkProjectDirectoryStructure [
	"answer true if the basic project directory structure is present"

	self requiredProjectRoots
		do: [ :fileRef | 
			fileRef exists
				ifFalse: [ ^ false ] ].
	^ true
]

{ #category : 'private' }
RwResolvedProjectV2 >> _loadTool [

	^ self _projectDefinition _loadTool
]

{ #category : 'visiting' }
RwResolvedProjectV2 >> acceptVisitor: aVisitor [
	^ aVisitor visitResolvedProjectV2: self
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addComponentNamed: aComponentName definedGroupNames: groupNameDict comment: commentString [
	^ self _projectDefinition
		addComponentNamed: aComponentName
		definedGroupNames: groupNameDict
		comment: commentString
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addComponentNamed: componentName toComponentNamed: toComponentName [
	^ self _projectDefinition
		addComponentNamed: componentName
		toComponentNamed: toComponentName
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addComponentNamed: componentName toComponentNamed: toComponentName withConditions: conditionArray andGroupName: groupName [
	^ self _projectDefinition
		addComponentNamed: componentName
		toComponentNamed: toComponentName
		withConditions: conditionArray
		andGroupName: groupName
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addNestedComponentNamed: aComponentName definedGroupNames: groupNameDict comment: commentString [
	^ self _projectDefinition
		addNestedComponentNamed: aComponentName
		definedGroupNames: groupNameDict
		comment: commentString
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addNewComponentNamed: componentName [
	^ self _projectDefinition addNewComponentNamed: componentName
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addNewComponentNamed: aComponentName comment: aString [
	^ self _projectDefinition addNewComponentNamed: aComponentName comment: aString
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addNewComponentNamed: aComponentName condition: condition comment: aString [
	^ self _projectDefinition addNewComponentNamed: aComponentName condition: condition comment: aString
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addNewComponentNamed: componentName toComponentNamed: toComponentName condition: conditionPathArray [
	^ self
		addNewComponentNamed: componentName
		toComponentNamed: toComponentName
		condition: conditionPathArray
		comment: ''
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addNewComponentNamed: componentName toComponentNamed: toComponentName condition: conditionPathArray comment: aString [
	^ self _projectDefinition
		addNewComponentNamed: componentName
		toComponentNamed: toComponentName
		condition: conditionPathArray
		comment: aString
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addNewNestedComponentNamed: aComponentName comment: aString [
	^ self _projectDefinition
		addNewNestedComponentNamed: aComponentName
		comment: aString
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addPackageNamed: packageName [
	"the package is expected to already be present in a component - used when reading packages from disk"

	^ self _projectDefinition addPackageNamed: packageName
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addPackageNamed: packageName toComponentNamed: componentName [
	^ self _projectDefinition
		addPackageNamed: packageName
		toComponentNamed: componentName
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addPackageNamed: packageName toComponentNamed: componentName gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc [
	^ self _projectDefinition
		addPackageNamed: packageName
		toComponentNamed: componentName
		gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addPackageNamed: packageName toComponentNamed: componentName withConditions: conditionArray andGroupName: groupName [
	^ self _projectDefinition
		addPackageNamed: packageName
		toComponentNamed: componentName
		withConditions: conditionArray
		andGroupName: groupName
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addPackageNamed: packageName toComponentNamed: componentName withConditions: conditionArray andGroupName: groupName gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc [
	(self groupNames includes: groupName)
		ifFalse: [ self error: 'The group ' , groupName printString , ' is undefined' ].
	^ self _projectDefinition
		addPackageNamed: packageName
		toComponentNamed: componentName
		withConditions: conditionArray
		andGroupName: groupName
		gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addPackagesNamed: packageNames toComponentNamed: componentName [ 
	^ self _projectDefinition
		addPackagesNamed: packageNames
		toComponentNamed: componentName
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addPackagesNamed: packageNames toComponentNamed: componentName withConditions: conditionArray andGroupName: groupName [
	(self groupNames includes: groupName)
		ifFalse: [ self error: 'The group ' , groupName printString , ' is undefined' ].
	^ self _projectDefinition
		addPackagesNamed: packageNames
		toComponentNamed: componentName
		withConditions: conditionArray
		andGroupName: groupName
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addPlatformComponentNamed: componentName toComponentNamed: toComponentName alias: alias condition: conditionPathArray [
	^ self _projectDefinition
		addPlatformComponentNamed: componentName
		toComponentNamed: toComponentName
		alias: alias
		condition: conditionPathArray
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addPlatformComponentNamed: aComponentName toComponentNamed: toComponentName pathNameArray: pathNameArray conditionPathArray: conditionPathArray [
	^ self
		addPlatformComponentNamed: aComponentName
		toComponentNamed: toComponentName
		pathNameArray: pathNameArray
		conditionPathArray: conditionPathArray
		comment: ''
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addPlatformComponentNamed: aComponentName toComponentNamed: toComponentName pathNameArray: pathNameArray conditionPathArray: conditionPathArray comment: aString [
	^ self _projectDefinition
		addPlatformComponentNamed: aComponentName
		toComponentNamed: toComponentName
		pathNameArray: pathNameArray
		conditionPathArray: conditionPathArray
		comment: aString
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addPlatformNestedComponentNamed: aComponentName condition: conditionArray comment: commentString [
	^ self _projectDefinition
		addPlatformNestedComponentNamed: aComponentName
		condition: conditionArray
		comment: commentString
]

{ #category : 'accessing' }
RwResolvedProjectV2 >> addPostloadDoitName: doitName withSource: doitSource toComponentNamed: aComponentName [
	| component |
	component := self componentNamed: aComponentName.
	component postloadDoitName: doitName.
	component doitDict at: doitName put: doitSource
]

{ #category : 'accessing' }
RwResolvedProjectV2 >> addPreloadDoitName: doitName withSource: doitSource toComponentNamed: aComponentName [
	| component |
	component := self componentNamed: aComponentName.
	component preloadDoitName: doitName.
	component doitDict at: doitName put: doitSource
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addProjectNamed:projectName toComponentNamed: componentName [ 
	^ self _projectDefinition
		addProjectNamed:projectName
		toComponentNamed: componentName
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addProjectNamed:projectName toComponentNamed: componentName withConditions: conditionArray andGroupName: groupName [
	^ self _projectDefinition
		addProjectNamed:projectName
		toComponentNamed: componentName
		withConditions: conditionArray
		andGroupName: groupName
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addRawPackageNamed: packageName [
	"not sure I like how this is used ... the component structure needs to be kept in sync with packages, so this is not quite the route to go, unless we ensure that the component has an entry for the package"

	"see similar comment in addPackages:forComponent: and _addComponent"

	^ self _projectDefinition addRawPackageNamed: packageName
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addSimpleComponentNamed: aComponentName comment: commentString [
	^ self _projectDefinition
		addSimpleComponentNamed: aComponentName
		condition: 'common'
		comment: commentString
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addSimpleComponentNamed: aComponentName condition: condition comment: commentString [
	^ self _projectDefinition
		addSimpleComponentNamed: aComponentName
		condition: condition
		comment: commentString
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addSimpleNestedComponentNamed: aComponentName condition: condition  comment: commentString [
	^ self _projectDefinition
		addSimpleNestedComponentNamed: aComponentName
		condition: condition
		comment: commentString
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addSimpleNestedComponentNamed: aComponentName condition: condition groupName: groupName comment: commentString [
	^ self _projectDefinition
		addSimpleNestedComponentNamed: aComponentName
		condition: condition
		groupName: groupName
		comment: commentString
]

{ #category : 'actions' }
RwResolvedProjectV2 >> checkout: aCommittish [
	^ self _projectRepository checkout: aCommittish
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> comment [
	^ self _projectDefinition comment
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> comment: aString [
	self _projectDefinition comment: aString
]

{ #category : 'querying' }
RwResolvedProjectV2 >> commitLog: logLimit [
	^ self _projectRepository commitLog: logLimit
]

{ #category : 'accessing' }
RwResolvedProjectV2 >> compileWhileReading [
	"true means compile method defs while reading tonel files for immediate detection of syntax errors"

	^ (self dynamicInstVarAt: #'compileWhileReading') ifNil: [ false ]
]

{ #category : 'accessing' }
RwResolvedProjectV2 >> compileWhileReading: aBoolean [
	"true means compile method defs while reading tonel files for immediate detection of syntax errors"

	self dynamicInstVarAt: #'compileWhileReading' put: aBoolean
]

{ #category : 'querying' }
RwResolvedProjectV2 >> componentForPackageNamed: packageName [
	"Answer nil if no component found"

	^ self _projectDefinition componentForPackageNamed: packageName
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> componentNamed: aComponentName [
	^ self _projectDefinition componentNamed: aComponentName
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> componentNamed: aComponentName ifAbsent: absentBlock [
	^ self _projectDefinition componentNamed: aComponentName ifAbsent: absentBlock
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> components [
	^ self _projectDefinition components
]

{ #category : 'accessing' }
RwResolvedProjectV2 >> componentsWithDoits [
	^ self _projectDefinition components componentsWithDoits
]

{ #category : 'copying' }
RwResolvedProjectV2 >> copyForLoadedProject [
	"project definition is not part of the copy for loaded projects"

	^ RwResolvedProjectV2 new
		_projectRepository: projectRepository copy;
		_loadSpecification: loadSpecification copy;
		_projectSpecification: projectSpecification copy;
		_projectStructure: projectDefinition components copy;
		yourself
]

{ #category : 'load specification' }
RwResolvedProjectV2 >> diskUrl [
	^ self _loadSpecification diskUrl
]

{ #category : 'load specification' }
RwResolvedProjectV2 >> diskUrl: anUrlString [
	self _loadSpecification diskUrl: anUrlString
]

{ #category : 'actions' }
RwResolvedProjectV2 >> export [
	Rowan projectTools writeV2 writeResolvedProject: self
]

{ #category : 'exporting' }
RwResolvedProjectV2 >> exportComponents [
	self components
		do: [ :component | 
			component
				exportToUrl: 'file:' , self componentsRoot pathString , '/';
				yourself ].
	self components isEmpty
		ifTrue: [ 
			"add README.md as placeholder to ensure that the directory is preserved by git"
			self componentsRoot / 'README' , 'md' writeStreamDo: [ :fileStream |  ] ]
]

{ #category : 'exporting' }
RwResolvedProjectV2 >> exportLoadSpecification [
	self _loadSpecification exportTo: self specsRoot
]

{ #category : 'exporting' }
RwResolvedProjectV2 >> exportPackages [
	| projectSetDefinition |
	projectSetDefinition := [ 
	RwProjectSetDefinition new
		addProject: self copy read;
		yourself ]
		on: Error
		do: [ :ignored | RwProjectSetDefinition new ].
	self
		exportPackages: projectSetDefinition
		packagesRoot: self packagesRoot
		packageFormat: self packageFormat
		packageConvention: self packageConvention
]

{ #category : 'exporting' }
RwResolvedProjectV2 >> exportPackages: diskProjectSetDefinition packagesRoot: packagesRoot packageFormat: packageFormat packageConvention: packageConvention [
	| projectSetDefinition visitor projectSetModification writerVisitorClass |
	packagesRoot / 'properties' , 'st'
		writeStreamDo: [ :fileStream | 
			fileStream
				nextPutAll: '{ ';
				lf;
				tab;
				nextPutAll: '#format : ' , packageFormat printString , ',';
				lf;
				tab;
				nextPutAll: '#convention : ' , packageConvention printString;
				lf;
				nextPutAll: '}';
				lf ].	"write out packages"
	writerVisitorClass := packageFormat = 'tonel'
		ifTrue: [ RwModificationTonelWriterVisitorV2 ]
		ifFalse: [ RwModificationFiletreeWriterVisitorV2 ].
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: self.
	projectSetModification := projectSetDefinition
		compareAgainstBase: diskProjectSetDefinition.
	visitor := writerVisitorClass new
		packagesRoot: packagesRoot;
		yourself.

	visitor visit: projectSetModification
]

{ #category : 'exporting' }
RwResolvedProjectV2 >> exportProjects [
	self projectNames do: [ :projectName | self error: 'not yet implemented' ].
	self projectNames isEmpty
		ifTrue: [ 
			"add README.md as placeholder to ensure that the directory is preserved by git"
			self projectsRoot / 'README' , 'md' writeStreamDo: [ :fileStream |  ] ]
]

{ #category : 'exporting' }
RwResolvedProjectV2 >> exportProjectSpecification [
	self _projectSpecification exportTo: self repositoryRoot
]

{ #category : 'exporting' }
RwResolvedProjectV2 >> exportTopazFormatTo: filePath [
	| projectSetDefinition projectSetModification visitor fileReference |
	fileReference := filePath asFileReference.
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: self.
	projectSetModification := projectSetDefinition
		compareAgainstBase: RwProjectSetDefinition new.
	visitor := RwGsModificationTopazWriterVisitorV2 new
		repositoryRootPath: fileReference parent;
		topazFilename: fileReference basename;
		yourself.
	visitor visit: projectSetModification
]

{ #category : 'exporting' }
RwResolvedProjectV2 >> exportTopazFormatTo: filePath usingPackageNamesMap: packageNamesMap [
	| projectSetDefinition projectSetModification visitor fileReference |
	fileReference := filePath asFileReference.
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: self.
	projectSetModification := projectSetDefinition
		compareAgainstBase: RwProjectSetDefinition new.
	visitor := RwGsModificationTopazWriterVisitorV2 new
		repositoryRootPath: fileReference parent;
		topazFilename: fileReference basename;
		topazFilenamePackageNamesMap: packageNamesMap;
		yourself.
	visitor visit: projectSetModification
]

{ #category : 'load specification' }
RwResolvedProjectV2 >> gitUrl [
	^ self _loadSpecification gitUrl
]

{ #category : 'load specification' }
RwResolvedProjectV2 >> gitUrl: anUrlString [
	self _loadSpecification gitUrl: anUrlString
]

{ #category : 'initialization' }
RwResolvedProjectV2 >> initialize [
	"repository must be explicitly created"

	super initialize.
	projectDefinition := RwProjectDefinitionV2 new
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> isEmpty [
	^ self _projectDefinition isEmpty
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> key [
	^ self projectName
]

{ #category : 'actions' }
RwResolvedProjectV2 >> load [
	"load the receiver into the image"

	self _validate: self platformConditionalAttributes.
	^ Rowan projectTools loadV2 loadProjectDefinition: self projectDefinition
]

{ #category : 'actions' }
RwResolvedProjectV2 >> load: instanceMigrator [
	"load the receiver into the image"

	self _validate: self platformConditionalAttributes.
	^ Rowan projectTools loadV2
		loadProjectDefinition: self
		instanceMigrator: instanceMigrator
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> loadedCommitId [
	^ self _projectSpecification loadedCommitId
]

{ #category : 'actions' }
RwResolvedProjectV2 >> loadProjectSet [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"load a project definition set that will contain the project definition along with any dependent project definitions"

	self _validate: self platformConditionalAttributes.
	^ Rowan projectTools loadV2
		loadProjectSetDefinition:
			(self readProjectSet: self platformConditionalAttributes)
]

{ #category : 'actions' }
RwResolvedProjectV2 >> loadProjectSet: instanceMigrator [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"load a project definition set that will contain the project definition along with any dependent project definitions"

	self _validate: self platformConditionalAttributes.
	^ Rowan projectTools loadV2
		loadProjectSetDefinition: self readProjectSet
		instanceMigrator: instanceMigrator
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> movePackageNamed: aPackageName toComponentNamed: aComponentName [
	^ self _projectDefinition
		movePackageNamed: aPackageName
		toComponentNamed: aComponentName
]

{ #category : '-- loader compat --' }
RwResolvedProjectV2 >> name [
	"sender in loader code that's shared between RwComponentProjectDefinition and RwResolvedProjectV2, 
		should use projectAlias, but need to wait until we're no longer using RwComponentProjectDefinition"

	^ self projectName
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> packageConvention [
	^ self _projectSpecification packageConvention
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> packageFormat [
	^ self _projectSpecification packageFormat
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> packageFormat: aString [
	^ self _projectSpecification packageFormat: aString
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> packageFormatIfAbsent: absentBlock [
	^ self _projectSpecification packageFormatIfAbsent: absentBlock
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> packageNamed: aString [
	^ self _projectDefinition packageNamed: aString
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> packageNamed: aString ifAbsent: absentBlock [

	^ self _projectDefinition packageNamed: aString ifAbsent: absentBlock
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> packageNames [
	^ self _projectDefinition packageNames
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> packages [
	^ self _projectDefinition packages
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> packages: aPackageDictionary [
	^ self _projectDefinition packages: aPackageDictionary
]

{ #category : 'printing' }
RwResolvedProjectV2 >> printOn: aStream [
	super printOn: aStream.
	projectDefinition
		ifNotNil: [ 
			aStream
				nextPutAll: ' for ';
				nextPutAll: self _projectDefinition projectName ]
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> projectDefinitionSourceProperty [
	^ self _projectDefinition projectDefinitionSourceProperty
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> projectDefinitionSourceProperty: sourceKeyOrNil [
	self _projectDefinition projectDefinitionSourceProperty: sourceKeyOrNil
]

{ #category : 'accessing' }
RwResolvedProjectV2 >> projectName: aString [
	super projectName: aString.
	self _projectDefinition projectName: aString
]

{ #category : 'accessing' }
RwResolvedProjectV2 >> projectNames [
	"not yet implemented"

	^ #()
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> projectUrl [
	^ self _loadSpecification projectUrl
]

{ #category : 'actions' }
RwResolvedProjectV2 >> read [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return the receiver with a new set of definitions read from disk"

	self repositoryExists
		ifTrue: [ ^ self readProjectComponentNames: self componentNames ]
]

{ #category : 'actions' }
RwResolvedProjectV2 >> read: platformConditionalAttributes [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return the receiver with a new set of definitions read from disk"

	^ self
		readProjectComponentNames: self componentNames
		platformConditionalAttributes: platformConditionalAttributes
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> readPackageNames: packageNames [
	| format visitorClass |
	format := self
		packageFormatIfAbsent: [ 
			| formatFromDisk |
			formatFromDisk := (RwAbstractReaderWriterVisitor
				_repositoryPropertyDictFor: self packagesRoot)
				at: #'format'
				ifAbsent: [ 'tonel' ].
			self packageFormat: formatFromDisk.
			formatFromDisk ].
	self _projectDefinition packages: Dictionary new.
	visitorClass := format = 'tonel'
		ifTrue: [ RwRepositoryResolvedProjectTonelReaderVisitorV2 ]
		ifFalse: [ RwRepositoryResolvedProjectFiletreeReaderVisitorV2 ].
	^ visitorClass new
		compileWhileReading: self compileWhileReading;
		packageNames: packageNames;
		visit: self
]

{ #category : 'actions' }
RwResolvedProjectV2 >> readProjectComponentNames: componentNames [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return the receiver with a new set of definitions read from disk"

	^ self
		readProjectComponentNames: componentNames
		platformConditionalAttributes: self platformConditionalAttributes
]

{ #category : 'actions' }
RwResolvedProjectV2 >> readProjectComponentNames: componentNames platformConditionalAttributes: platformConditionalAttributes [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return the receiver with a new set of definitions read from disk"

	^ Rowan projectTools readV2
		readProjectForResolvedProject: self
		withComponentNames: componentNames
		platformConditionalAttributes: platformConditionalAttributes
]

{ #category : 'actions' }
RwResolvedProjectV2 >> readProjectSet [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return a project definition set that will contain the project definition along with any dependent project definitions"

	^ self readProjectSetComponentNames: self componentNames
]

{ #category : 'actions' }
RwResolvedProjectV2 >> readProjectSet: platformConditionalAttributes [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return a project definition set that will contain the project definition along with any dependent project definitions"

	^ self
		readProjectSetComponentNames: self componentNames
		platformConditionalAttributes: platformConditionalAttributes
]

{ #category : 'actions' }
RwResolvedProjectV2 >> readProjectSetComponentNames: componentNames [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return a project definition set that will contain the project definition along with any dependent project definitions"

	^ self
		readProjectSetComponentNames: componentNames
		platformConditionalAttributes: self platformConditionalAttributes
]

{ #category : 'actions' }
RwResolvedProjectV2 >> readProjectSetComponentNames: componentNames platformConditionalAttributes: platformConditionalAttributes [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return a project definition set that will contain the project definition along with any dependent project definitions"

	^ Rowan projectTools readV2
		readProjectSetForResolvedProject: self
		withComponentNames: componentNames
		platformConditionalAttributes: platformConditionalAttributes
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> removeComponentNamed: aComponentName [
	^ self _projectDefinition removeComponentNamed: aComponentName
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> removeComponentNamed: componentName fromComponentNamed: fromComponentName withConditions: conditionArray andGroupName: groupName [
	^ self _projectDefinition
		removeComponentNamed: componentName
		fromComponentNamed: fromComponentName
		withConditions: conditionArray
		andGroupName: groupName
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> removePackageNamed: packageName [
	^ self _projectDefinition removePackageNamed: packageName
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> removePackageNamed: packageName fromComponentNamed: componentName [
	^ self _projectDefinition
		removePackageNamed: packageName
		fromComponentNamed: componentName
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> removePackageNamed: packageName fromComponentNamed: componentName withConditions: conditionArray andGroupName: groupName [
	^ self _projectDefinition
		removePackageNamed: packageName
		fromComponentNamed: componentName
		withConditions: conditionArray
		andGroupName: groupName
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> removeProjectNamed: aProjectName [
	^ self _projectDefinition removeProjectNamed: aProjectName
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> renameComponentNamed: aComponentPath to: aComponentName [
	^ self _projectDefinition
		renameComponentNamed: aComponentPath
		to: aComponentName
]

{ #category : '-- loader compat --' }
RwResolvedProjectV2 >> repositoryCommitId [
	^ self _projectRepository commitId
]

{ #category : 'project specification' }
RwResolvedProjectV2 >> repoType: aSymbol [
	self _projectSpecification repoType: aSymbol
]

{ #category : 'actions' }
RwResolvedProjectV2 >> resolve [
	"resolve the projectSpecation (clone remote repo or connect to existing repo on disk) and read 
		project from disk, if project is present on disk"

	self _projectRepository resolve
		ifTrue: [ 
			self _projectRepository checkAndUpdateRepositoryRevision: self.
			self _checkProjectDirectoryStructure
				ifTrue: [ 
					"update project definition from disk"
					self read.
					self
						projectDefinitionSourceProperty:
							RwLoadedProject _projectLoadedDefinitionSourceWithDependentProjectsValue ] ]
]

{ #category : 'actions' }
RwResolvedProjectV2 >> resolve: platformConfigurationAttributes [
	"resolve the projectSpecation (clone remote repo or connect to existing repo on disk) and read 
		project from disk, if project is present on disk"

	self _projectRepository resolve
		ifTrue: [ 
			self _projectRepository checkAndUpdateRepositoryRevision: self.
			self _checkProjectDirectoryStructure
				ifTrue: [ 
					"update project definition from disk"
					self read: platformConfigurationAttributes ] ]
]

{ #category : 'load specification' }
RwResolvedProjectV2 >> revision [
	^ self _loadSpecification revision
]

{ #category : 'load specification' }
RwResolvedProjectV2 >> revision: aRevisionString [
	^ self _loadSpecification revision: aRevisionString
]

{ #category : '-- loader compat --' }
RwResolvedProjectV2 >> symbolDictNameForPackageNamed: aPackageName [
	"sender in loader code that's shared between RwComponentProjectDefinition and RwResolvedProjectV2, 
		should use gemstoneSymbolDictNameForPackageNamed:, but need to wait until we're no longer 
		using RwComponentProjectDefinition"

	^ self gemstoneSymbolDictNameForPackageNamed: aPackageName
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> updateLoadedCommitId [
	self _projectSpecification loadedCommitId: self _projectRepository commitId
]

{ #category : 'testing' }
RwResolvedProjectV2 >> useGit [
	^ self _projectRepository useGit
]
