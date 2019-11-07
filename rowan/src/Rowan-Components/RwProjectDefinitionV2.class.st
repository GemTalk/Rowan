Class {
	#name : 'RwProjectDefinitionV2',
	#superclass : 'RwAbstractProjectDefinitionV2',
	#category : 'Rowan-Components'
}

{ #category : 'instance creation' }
RwProjectDefinitionV2 class >> fromLoadSpecification: aRwProjectLoadSpecificationV2 [
	| projectReferenceDefintion |
	projectReferenceDefintion := RwProjectReferenceDefinitionV2
		fromLoadSpecification: aRwProjectLoadSpecificationV2.
	^ self basicNew
		properties:
				(Dictionary new
						add: 'name' -> aRwProjectLoadSpecificationV2 projectName;
						add: 'projectRef' -> projectReferenceDefintion;
						yourself);
		packages: Dictionary new;
		projectDefinitionSourceProperty: RwLoadedProject _projectSourceValueNewProject;
		yourself
]

{ #category : 'instance creation' }
RwProjectDefinitionV2 class >> projectName: projectName comment: commentString [
	| projectLoadSpecification |
	projectLoadSpecification := RwProjectLoadSpecificationV2 new
		projectName: projectName;
		yourself.
	^ (self fromLoadSpecification: projectLoadSpecification)
		comment: commentString;
		yourself
]

{ #category : 'visiting' }
RwProjectDefinitionV2 >> acceptVisitor: aVisitor [

	^ aVisitor visitComponentProjectDefinition: self
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addComponentNamed: aComponentName comment: commentString [

	| component |
	component := self 
		components at: aComponentName 
		ifAbsentPut: [ RwProjectLoadComponentV2 newNamed: aComponentName for: self projectName ].
	^ component 
		addDefinedGroupName: self defaultGroupName includeGroups: #();
		comment: commentString;
		yourself
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addComponentNamed: aComponentName definedGroupNames: groupNameDict comment: commentString [
	| component |
	component := self components
		at: aComponentName
		ifAbsentPut: [ RwProjectLoadComponentV2 newNamed: aComponentName for: self projectName ].
	groupNameDict
		keysAndValuesDo: [ :groupName :includeGroups | component defineGroupNamed: groupName toIncludeGroups: includeGroups ].
	component comment: commentString.
	^ component
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addPackageNamed: packageName [

	self shouldNotImplement
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addPackageNamed: packageName toComponentNamed: componentName withConditions: conditionArray andGroupName: groupName [
	| package component |
	(self groupNames includes: groupName)
		ifFalse: [ self error: 'The group ' , groupName printString , ' is undefined' ].
	self
		componentNamed: componentName
		ifAbsent: [ self error: 'The component ' , componentName printString , ' is undefined' ].
	package := RwPackageDefinition newNamed: packageName.
	self _addPackage: package.
	component := self components
		at: componentName
		ifAbsentPut: [ RwProjectLoadComponentV2 newNamed: componentName for: self name ].
	component
		conditionalPackagesAtConditions: conditionArray
		andGroup: groupName
		addPackageNames: {packageName}.
	^ package
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addPackageNamed: packageName toComponentNamed: componentName withConditions: conditionArray andGroupName: groupName gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc [
	| package component |
	(self groupNames includes: groupName)
		ifFalse: [ self error: 'The group ' , groupName printString , ' is undefined' ].
	self
		componentNamed: componentName
		ifAbsent: [ self error: 'The component ' , componentName printString , ' is undefined' ].
	package := RwPackageDefinition newNamed: packageName.
	self _addPackage: package.
	component := self components
		at: componentName
		ifAbsentPut: [ RwProjectLoadComponentV2 newNamed: componentName for: self name ].
	component
		conditionalPackageMapSpecsAtGemStoneUserId: aSymbolDictAssoc key
			andPackageName: packageName
			setSymbolDictNameTo: aSymbolDictAssoc value;
		conditionalPackagesAtConditions: conditionArray
			andGroup: groupName
			addPackageNames: {packageName}.
	^ package
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addPackages: somePackageNames forComponent: aComponent [
	"not sure I like how this is used ... the component structure needs to be kept in sync with packages, so this is not quite the route to go, unless we ensure that the component has an entry for the package"

	"see similar comment in addRawPackageNamed:"

	"should be sent from the component visitor ... not unexpected to have a duplicate, but the new
		component --- presumably freshly read from disk --- wins"

	| componentName |
	componentName := aComponent name.
	(self components at: componentName ifAbsent: [  ])
		ifNil: [ self components at: componentName put: aComponent ]
		ifNotNil: [ :theComponent | 
			theComponent ~~ aComponent
				ifTrue: [ 
					"newest component wins"
					self components at: componentName put: aComponent ] ].
	somePackageNames asSet
		do: [ :packageName | super addPackageNamed: packageName ]
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addPackagesNamed: packageNames toComponentNamed: componentName withConditions: conditionArray andGroupName: groupName [


	^ packageNames collect: [:packageName | 
		self addPackageNamed: packageName toComponentNamed: componentName withConditions: conditionArray andGroupName: groupName ]
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addPackagesNamed: packageNames toComponentNamed: componentName withConditions: conditionArray andGroupName: groupName  gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc [

	^ packageNames collect: [:packageName | 
		self addPackageNamed: packageName toComponentNamed: componentName withConditions: conditionArray andGroupName: groupName  gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc ]
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addRawPackageNamed: packageName [
	"not sure I like how this is used ... the component structure needs to be kept in sync with packages, so this is not quite the route to go, unless we ensure that the component has an entry for the package"

	"see similar comment in addPackages:forComponent:"

	^ self addPackage: (RwPackageDefinition newNamed: packageName)
]

{ #category : 'properties' }
RwProjectDefinitionV2 >> canCommit [

	^ self projectRef canCommit
]

{ #category : 'actions' }
RwProjectDefinitionV2 >> clone [
	"clone remote git project to disk"

	self cloneRepository.
	^ self read						"refresh receiver from the cloned repository and answer project definition set that contains reciever along with any dependent projects"
]

{ #category : 'actions' }
RwProjectDefinitionV2 >> cloneRepository [
	"clone remote git project to disk"

	self projectRef clone
]

{ #category : 'properties' }
RwProjectDefinitionV2 >> comment [

	^ self projectRef comment
]

{ #category : 'properties' }
RwProjectDefinitionV2 >> comment: aString [

	self projectRef comment: aString
]

{ #category : 'actions' }
RwProjectDefinitionV2 >> commit: message [
	"commit the repository associated with receiver ..."

	self canCommit
		ifFalse: [ 
			| msg |
			msg := 'repository for project ' , self name printString
				, ' does not support commit operations.'.
			self inform: msg.
			^ msg ].
	^ self projectRef doCommit: message
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> componentNamed: aComponentName [

	^ self componentNamed: aComponentName ifAbsent: [ self error: 'The component named ', aComponentName printString, ' was not found' ]
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> componentNamed: aComponentName ifAbsent: absentBlock [

	^ self components 
		at: aComponentName 
		ifAbsent: absentBlock
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> components [

	^ self projectRef components
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> components: aComponentDefinitionDictionary [

	self projectRef components: aComponentDefinitionDictionary
]

{ #category : 'properties' }
RwProjectDefinitionV2 >> componentsPath [

	^ self projectRef componentsPath
]

{ #category : 'properties' }
RwProjectDefinitionV2 >> componentsPath: aDirectoryPathString [

	^ self projectRef componentsPath: aDirectoryPathString
]

{ #category : 'properties' }
RwProjectDefinitionV2 >> componentsRoot [
	^ self projectRef componentsRoot
]

{ #category : 'actions' }
RwProjectDefinitionV2 >> create [
	"write the project structure to disk, if it doesn't already exist"

	Rowan projectTools create
		createComponentProject: self.
]

{ #category : 'actions' }
RwProjectDefinitionV2 >> export [

	Rowan projectTools write writeComponentProjectDefinition: self
]

{ #category : 'exporting' }
RwProjectDefinitionV2 >> exportComponents [

	self components isEmpty ifTrue: [
		self addComponentNamed: self defaultComponentName comment: '' ].
	self components values do: [:component|
		component exportToUrl: 'file:',  self configsRoot pathString, '/' ].
	self components isEmpty
		ifTrue: [
			"add README.md as placeholder to ensure that the directory is preserved by git"
			(self configsRoot /  'README', 'md') writeStreamDo: [ :fileStream | ] ]
]

{ #category : 'exporting' }
RwProjectDefinitionV2 >> exportPackages [
	"attempt to do incremental write to disk, however, if disk cannot be read, write all packages to disk"
	| projectSetDefinition |
	projectSetDefinition := [ (self class newForProjectReference: self projectRef) readProjectSet ]
		on: Error
		do: [:ignored | RwProjectSetDefinition new ].
	self exportPackages: projectSetDefinition.
]

{ #category : 'exporting' }
RwProjectDefinitionV2 >> exportPackages: diskProjectSetDefinition [

	| projectSetDefinition visitor projectSetModification writerVisitorClass |
	(self packagesRoot /  'properties', 'st') 
		writeStreamDo: [ :fileStream | 
			fileStream nextPutAll: '{ '; 
				lf;
				tab;
				nextPutAll: '#format : ' , self packageFormat printString, ',' ;
				lf;
				tab;
				nextPutAll: '#convention : ', self packageConvention printString;
				lf;
				nextPutAll: '}';
				lf ].

"write out packages"
	writerVisitorClass := self packageFormat = 'tonel'
		ifTrue: [ RwModificationTonelWriterVisitor ]
		ifFalse: [ RwModificationFiletreeWriterVisitor ].
	projectSetDefinition:= RwProjectSetDefinition new.
	projectSetDefinition addDefinition: self.
	projectSetModification := projectSetDefinition compareAgainstBase: diskProjectSetDefinition.
	visitor := writerVisitorClass new.

	visitor visit: projectSetModification.
]

{ #category : 'exporting' }
RwProjectDefinitionV2 >> exportProjects [

	self projectNames do: [:projectName |
		self error: 'not yet implemented' ].
	self projectNames isEmpty
		ifTrue: [
			"add README.md as placeholder to ensure that the directory is preserved by git"
			(self projectsRoot /  'README', 'md') writeStreamDo: [ :fileStream | ] ]
]

{ #category : 'exporting' }
RwProjectDefinitionV2 >> exportSpecification [

	self projectRef exportSpecification
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> gitRoot: aGitRootReferenceOrString [ 

	^ self projectRef gitRoot: aGitRootReferenceOrString
]

{ #category : 'properties' }
RwProjectDefinitionV2 >> key [
	"Answer an object that can be used to uniquely identify myself in the context of my container."

	^self projectAlias
]

{ #category : 'actions' }
RwProjectDefinitionV2 >> load [
	"load the receiver into the image"

	^ Rowan projectTools load loadComponentProjectDefinition: self
]

{ #category : 'actions' }
RwProjectDefinitionV2 >> load: instanceMigrator [
	"load the receiver into the image"

	^ Rowan projectTools load loadComponentProjectDefinition: self instanceMigrator: instanceMigrator
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> loadedCommitId [

	^ self projectRef loadedCommitId
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> packageConvention [

	^ self projectRef packageConvention
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> packageConvention: aString [

	self projectRef packageConvention: aString
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> packageFormat [

	^ self projectRef packageFormat
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> packageFormat: aString [

	self projectRef packageFormat: aString
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> packageFormatIfAbsent: aBlock [

	^ self projectRef packageFormatIfAbsent: aBlock
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> packageNameToPlatformPropertiesMap [

	^self projectRef packageNameToPlatformPropertiesMap
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> packageNameToPlatformPropertiesMap: aDictionary [

	^self projectRef packageNameToPlatformPropertiesMap: aDictionary
]

{ #category : 'properties' }
RwProjectDefinitionV2 >> packagesPath [

	^ self projectRef packagesPath
]

{ #category : 'properties' }
RwProjectDefinitionV2 >> packagesPath: aString [

	^ self projectRef packagesPath: aString
]

{ #category : 'properties' }
RwProjectDefinitionV2 >> packagesRoot [

	^ self projectRef packagesRoot
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> projectAlias [
	"project alias is used as the name of the root directory for the project ... necessary if the project is 
		embedded in another project's git repository or you want use a non-default directory on disk"

	^ self projectRef projectAlias
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> projectAlias: aStringOrNil [
	"project alias is used as the name of the root directory for the project ... necessary if the project is 
		embedded in another project's git repository or you want use a non-default directory on disk"

	self projectRef projectAlias: aStringOrNil
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> projectHome [

	^ self projectRef projectHome
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> projectHome: aProjectHomeReferenceOrString [

	^ self projectRef projectHome: aProjectHomeReferenceOrString
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> projectLoadSpecification [
	^ self projectRef projectLoadSpecification
]

{ #category : 'properties' }
RwProjectDefinitionV2 >> projectName [ 

	^ self projectRef projectName
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> projectNames [

	^ self projectRef projectNames
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> projectNames: anArray [

	self projectRef projectNames: anArray
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> projectRef [

	^ self propertyAt: 'projectRef' ifAbsent: [ ]
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> projectRef: aRwProjectReferenceDefinition [

	^ self propertyAt: 'projectRef' put: aRwProjectReferenceDefinition
]

{ #category : 'properties' }
RwProjectDefinitionV2 >> projectsPath [

	^ self projectRef projectsPath
]

{ #category : 'properties' }
RwProjectDefinitionV2 >> projectsPath: aString [

	^ self projectRef projectsPath: aString
]

{ #category : 'properties' }
RwProjectDefinitionV2 >> projectsRoot [

	^ self projectRef projectsRoot
]

{ #category : 'properties' }
RwProjectDefinitionV2 >> projectUrl [

	^ self projectRef projectUrl
]

{ #category : 'actions' }
RwProjectDefinitionV2 >> read [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the default component and group names"

	"return a project definition set that will contain the project definition along with any dependent project definitions"

	^ Rowan projectTools read
		readProjectSetForComponentProjectDefinition: self 
			withConfigurations: self defaultConfigurationNames
			groupNames: self defaultGroupNames
]

{ #category : 'actions' }
RwProjectDefinitionV2 >> read: platformConfigurationAttributes [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the default component and group names"

	"return a project definition set that will contain the project definition along with any dependent project definitions"

	^ Rowan projectTools read
		readProjectSetForComponentProjectDefinition: self 
			withConfigurations: self defaultConfigurationNames
			groupNames: self defaultGroupNames
			platformConfigurationAttributes: platformConfigurationAttributes
]

{ #category : 'actions' }
RwProjectDefinitionV2 >> readPackageNames: packageNames [

	"drop all existing packages on the floor and replace with fresh versions of the packageNames read from disk"

	| format visitorClass |
	packages := Dictionary new. 
	format := self 
		packageFormatIfAbsent: [  
			| formatFromDisk |
			formatFromDisk := (RwAbstractReaderWriterVisitor _repositoryPropertyDictFor: self packagesRoot)
				at: #format ifAbsent: [ 'tonel' ].
			self packageFormat: formatFromDisk.
			formatFromDisk ].
	visitorClass := format = 'tonel'
		ifTrue: [ RwRepositoryComponentProjectTonelReaderVisitor ]
		ifFalse: [ RwRepositoryComponentProjectFiletreeReaderVisitor ].
	^ visitorClass new
		packageNames: packageNames;
		visit: self.
]

{ #category : 'tool api' }
RwProjectDefinitionV2 >> readProjectSet [

	^ Rowan projectTools read  readProjectSetForComponentProjectDefinition: self
]

{ #category : 'actions' }
RwProjectDefinitionV2 >> readProjectSetForPackageNames: packageNames [

	"drop all existing packages on the floor and replace with fresh versions of the packageNames read from disk"

	| format visitorClass |
	packages := Dictionary new. 
	format := self 
		packageFormatIfAbsent: [  
			| formatFromDisk |
			formatFromDisk := (RwAbstractReaderWriterVisitor _repositoryPropertyDictFor: self packagesRoot)
				at: #format ifAbsent: [ 'tonel' ].
			self packageFormat: formatFromDisk.
			formatFromDisk ].
	visitorClass := format = 'tonel'
		ifTrue: [ RwRepositoryComponentProjectTonelReaderVisitor ]
		ifFalse: [ RwRepositoryComponentProjectFiletreeReaderVisitor ].
	^ visitorClass new
		packageNames: packageNames;
		visit: self.
]

{ #category : 'reading' }
RwProjectDefinitionV2 >> readProjectSetReadTool: readTool withConfigurations: theConfigNames groupNames: theGroupNames [

	^ readTool
		readProjectSetForComponentProjectDefinition: self
			withConfigurations: theConfigNames 
			groupNames: theGroupNames
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> removePackage: aPackageDefinition [

	super removePackage: aPackageDefinition
]

{ #category : 'properties' }
RwProjectDefinitionV2 >> repositoryRoot [
	"Root directory of the project. The configsPath, repoPath, specsPath, and projectsPath are specified relative to the repository root."

	^ self projectRef repositoryRoot
]

{ #category : 'properties' }
RwProjectDefinitionV2 >> repositoryRoot: aFileReferenceOrPath [
	"Root directory of the project. The configsPath, repoPath, specsPath, and projectsPath are specified relative to the repository root."

	^ self projectRef repositoryRoot: aFileReferenceOrPath
]

{ #category : 'temporary compat' }
RwProjectDefinitionV2 >> repositoryRootPath [

	^ self repositoryRoot fullName
]

{ #category : 'temporary compat' }
RwProjectDefinitionV2 >> specification [

self deprecated: 'temporary patch .. sender should be replaced with projectRef'.
	^ self projectRef asSpecification
]

{ #category : 'properties' }
RwProjectDefinitionV2 >> specsPath [

	^ self projectRef specsPath
]

{ #category : 'properties' }
RwProjectDefinitionV2 >> specsPath: aDirectoryPathString [

	self projectRef specsPath: aDirectoryPathString
]

{ #category : 'properties' }
RwProjectDefinitionV2 >> specsRoot [

	^ self projectRef specsRoot
]

{ #category : 'loading' }
RwProjectDefinitionV2 >> updateLoadedCommitId [

	self projectRef updateLoadedCommitId
]

{ #category : 'loading' }
RwProjectDefinitionV2 >> updateLoadedComponentInfoFor: aLoadedProject from: projectInfo [
	"really need to update loaded components"

	| theComponentNames |
	aLoadedProject loadedGroupNames: (projectInfo at: 'loadedGroupNames').

	theComponentNames := projectInfo at: 'loadedConfigurationNames'.
	aLoadedProject components keys do: [:aComponentName |
		(theComponentNames includes: aComponentName)
			ifFalse: [ aLoadedProject components removeKey: aComponentName ] ].

	theComponentNames do: [:configName |
		aLoadedProject components at: configName put: (self components at: configName) copy ].

	self updateGsPlatformLoadedComponentInfoFor: aLoadedProject from: projectInfo
]

{ #category : 'properties' }
RwProjectDefinitionV2 >> useGit [

	^ self projectRef useGit
]
