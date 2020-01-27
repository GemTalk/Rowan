Class {
	#name : 'RwProjectDefinitionV2',
	#superclass : 'RwAbstractProjectDefinitionV2',
	#category : 'Rowan-DefinitionsV2'
}

{ #category : 'actions' }
RwProjectDefinitionV2 >> _deleteTool [

	^ Rowan projectTools delete
]

{ #category : 'actions' }
RwProjectDefinitionV2 >> _loadTool [

	^ Rowan projectTools loadV2
]

{ #category : 'private' }
RwProjectDefinitionV2 >> _validate [
	"ensure that the data structures within the receiver contain valid information"

	"make sure that list of packages is consistent between components and project definition"

	| definitionPackageNames componentPackageNames missingFromComponent missingFromDefinition errorMessage |
	definitionPackageNames := self packageNames asSet.
	componentPackageNames := Set new.
	self components_validate.
	missingFromComponent := definitionPackageNames - componentPackageNames.
	missingFromDefinition := componentPackageNames - definitionPackageNames.
	(missingFromComponent isEmpty and: [ missingFromDefinition isEmpty ])
		ifTrue: [ ^ true ].
	errorMessage := WriteStream on: String new.
	errorMessage
		nextPutAll:
				'Inconsistency between packages defined and referenced in components:';
		lf.
	missingFromComponent isEmpty
		ifFalse: [ 
			errorMessage
				tab;
				nextPutAll:
						'The following packages are defined, but not referenced in a component:';
				lf.
			missingFromComponent
				do: [ :packageName | 
					errorMessage
						tag;
						tab;
						nextPutAll: packageName;
						lf ] ].
	missingFromDefinition isEmpty
		ifFalse: [ 
			errorMessage
				tab;
				nextPutAll:
						'The following packages are referenced in a component, but not defined:';
				lf.
			missingFromDefinition
				do: [ :packageName | 
					errorMessage
						tab;
						tab;
						nextPutAll: packageName;
						lf ] ].
	self error: errorMessage contents
]

{ #category : 'private' }
RwProjectDefinitionV2 >> _validate: platformConfigurationAttributes groupNames: groupNames [
	"ensure that the data structures within the receiver contain valid information"

	"make sure that list of packages is consistent between components and project definition
		It's okay to have a definition that is not managed by a component.
		It's NOT okay to have component package that is not defined."

	| definitionPackageNames componentPackageNames missingFromComponent errorMessage |
	definitionPackageNames := self packageNames asSet.
	componentPackageNames := Set new.
	self components
		_validate: platformConfigurationAttributes
		groupNames: groupNames.
	missingFromComponent := componentPackageNames - definitionPackageNames.
	missingFromComponent isEmpty
		ifTrue: [ ^ true ].
	errorMessage := WriteStream on: String new.
	errorMessage
		nextPutAll: 'Component references package(s) that are not defined';
		lf.
	errorMessage
		tab;
		nextPutAll:
				'The following packages are defined, but not referenced in a component:';
		lf.
	missingFromComponent
		do: [ :packageName | 
			errorMessage
				tab;
				tab;
				nextPutAll: packageName;
				lf ].
	self error: errorMessage contents
]

{ #category : 'visiting' }
RwProjectDefinitionV2 >> acceptVisitor: aVisitor [

	^ aVisitor visitComponentProjectDefinition: self
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addApplicationNamed: aComponentName condition: condition groupName: groupName comment: commentString [
	^ self components
		addApplicationNamed: aComponentName
		condition: condition
		groupName: groupName
		comment: commentString
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addApplicationNamed: componentName toComponentNamed: toComponentName [
	^ self components
		addApplicationNamed: componentName
		toComponentNamed: toComponentName
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addComponentNamed: aComponentName definedGroupNames: groupNameDict comment: commentString [
	^ self components
		addComponentNamed: aComponentName
		definedGroupNames: groupNameDict
		comment: commentString
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addComponentNamed: componentName toComponentNamed: toComponentName withConditions: conditionArray andGroupName: groupName [
	^ self components
		addComponentNamed: componentName
		toComponentNamed: toComponentName
		withConditions: conditionArray
		andGroupName: groupName
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addNestedComponentNamed: aComponentName definedGroupNames: groupNameDict comment: commentString [
	^ self components
		addNestedComponentNamed: aComponentName
		definedGroupNames: groupNameDict
		comment: commentString
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addPackageNamed: packageName [

	self shouldNotImplement
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addPackageNamed: packageName toComponentNamed: componentName [
	| package |
	self components
		componentNamed: componentName
		ifAbsent: [ self error: 'The component ' , componentName printString , ' is undefined' ].
	package := RwPackageDefinition newNamed: packageName.
	self
		_addPackage: package
		ifPresent: [ 
			"no problem ... just update the component"
			 ].
	self components addPackageNamed: packageName toComponentNamed: componentName.
	^ package
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addPackageNamed: packageName toComponentNamed: componentName withConditions: conditionArray andGroupName: groupName [
	| package |
	self components
		componentNamed: componentName
		ifAbsent: [ self error: 'The component ' , componentName printString , ' is undefined' ].
	package := RwPackageDefinition newNamed: packageName.
	self
		_addPackage: package
		ifPresent: [ 
			"no problem ... just update the component"
			 ].
	self components
		addPackageNamed: packageName
		toComponentNamed: componentName
		withConditions: conditionArray
		andGroupName: groupName.
	^ package
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addPackageNamed: packageName toComponentNamed: componentName withConditions: conditionArray andGroupName: groupName gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc [
	| package |
	self components
		componentNamed: componentName
		ifAbsent: [ self error: 'The component ' , componentName printString , ' is undefined' ].
	package := RwPackageDefinition newNamed: packageName.
	self
		_addPackage: package
		ifPresent: [ 
			"no problem ... just update the component"
			 ].
	self components
		addPackageNamed: packageName
		toComponentNamed: componentName
		withConditions: conditionArray
		andGroupName: groupName
		gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc.
	^ package
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addPackages: somePackageNames forComponent: aComponent [
	"not sure I like how this is used ... the component structure needs to be kept in sync with packages, so this is not quite the route to go, unless we ensure that the component has an entry for the package"

	"see similar comment in addRawPackageNamed: _addComponent"

	"should be sent from the component visitor ... not unexpected to have a duplicate, but the new
		component --- presumably freshly read from disk --- wins"

	self components _addComponent: aComponent.
	somePackageNames asSet
		do: [ :packageName | self _addPackage: (RwPackageDefinition newNamed: packageName) ]
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addPackagesNamed: packageNames toComponentNamed: componentName [
	^ packageNames
		collect: [ :packageName | self addPackageNamed: packageName toComponentNamed: componentName ]
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
RwProjectDefinitionV2 >> addProjectNamed: projectName toComponentNamed: toComponentName withConditions: conditionArray andGroupName: groupName [
	^ self components
		addProjectNamed: projectName
		toComponentNamed: toComponentName
		withConditions: conditionArray
		andGroupName: groupName
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addRawPackageNamed: packageName [
	"not sure I like how this is used ... the component structure needs to be kept in sync with packages, so this is not quite the route to go, unless we ensure that the component has an entry for the package"

	"see similar comment in addPackages:forComponent: and _addComponent"

	^ self _addPackage: (RwPackageDefinition newNamed: packageName)
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addSimpleComponentNamed: aComponentName condition: condition groupName: groupName comment: commentString [
	^ self components
		addSimpleComponentNamed: aComponentName
		condition: condition
		groupName: groupName
		comment: commentString
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addSubApplicationNamed: aComponentName condition: condition groupName: groupName comment: commentString [
	^ self components
		addSubApplicationNamed: aComponentName
		condition: condition
		groupName: groupName
		comment: commentString
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addSubApplicationNamed: componentName toComponentNamed: toComponentName [
	^ self components
		addSubApplicationNamed: componentName
		toComponentNamed: toComponentName
]

{ #category : 'actions' }
RwProjectDefinitionV2 >> clone [
	"clone remote git project to disk"

	self cloneRepository.
	^ self read						"refresh receiver from the cloned repository and answer project definition set that contains reciever along with any dependent projects"
]

{ #category : 'actions' }
RwProjectDefinitionV2 >> create [
	"write the project structure to disk, if it doesn't already exist"

	Rowan projectTools create
		createComponentProject: self.
]

{ #category : 'actions' }
RwProjectDefinitionV2 >> export [

	Rowan projectTools writeV2 writeProjectDefinition: self
]

{ #category : 'exporting' }
RwProjectDefinitionV2 >> exportComponents [
	self components export: self componentsRoot
]

{ #category : 'exporting' }
RwProjectDefinitionV2 >> exportPackages: diskProjectSetDefinition packagesRoot: packagesRoot packageFormat: packageFormat packageConvention: packageConvention [
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
RwProjectDefinitionV2 >> exportPackagesTo: packagesRoot packageFormat: packageFormat packageConvention: packageConvention [
	"attempt to do incremental write to disk, however, if disk cannot be read, write all packages to disk"

	| projectSetDefinition |
	projectSetDefinition := [ (self class newForProjectReference: self projectRef) readProjectSet ]
		on: Error
		do: [ :ignored | RwProjectSetDefinition new ].
	self
		exportPackages: projectSetDefinition
		packagesRoot: packagesRoot
		packageFormat: packageFormat
		packageConvention: packageConvention
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

{ #category : 'properties' }
RwProjectDefinitionV2 >> key [
	"Answer an object that can be used to uniquely identify myself in the context of my container."

	^self projectName
]

{ #category : 'actions' }
RwProjectDefinitionV2 >> load [
	"load the receiver into the image"

	^ self _loadTool loadProjectDefinition: self
]

{ #category : 'actions' }
RwProjectDefinitionV2 >> load: instanceMigrator [
	"load the receiver into the image"

	^ self _loadTool loadProjectDefinition: self instanceMigrator: instanceMigrator
]

{ #category : 'properties' }
RwProjectDefinitionV2 >> projectName [
	^ self propertyAt: 'name'
]

{ #category : 'properties' }
RwProjectDefinitionV2 >> projectName: aString [
	self propertyAt: 'name' put: aString
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

{ #category : 'tool api' }
RwProjectDefinitionV2 >> readProjectSet [

	^ Rowan projectTools read  readProjectSetForComponentProjectDefinition: self
]

{ #category : 'reading' }
RwProjectDefinitionV2 >> readProjectSetReadTool: readTool withConfigurations: theConfigNames groupNames: theGroupNames [

	^ readTool
		readProjectSetForComponentProjectDefinition: self
			withConfigurations: theConfigNames 
			groupNames: theGroupNames
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> removeComponentNamed: componentName fromComponentNamed: fromComponentName withConditions: conditionArray andGroupName: groupName [
	| component |
	component := self componentNamed: fromComponentName.
	component
		conditionalPropertiesAtConditions: conditionArray
		andGroup: groupName
		removeComponentNames: {componentName}.
	^ component
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> removePackage: aPackageDefinition [
	self components removePackageNamed: aPackageDefinition name.
	^ super removePackage: aPackageDefinition
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> removePackageNamed: packageName fromComponentNamed: componentName withConditions: conditionArray andGroupName: groupName [
	"do not remove package from defintion, remove it from the named component only. 
		Use removePackage:, if you want the package completely removed from definition"

	| component |
	component := self componentNamed: componentName.
	component
		conditionalPropertiesAtConditions: conditionArray
		andGroup: groupName
		removePackageNames: {packageName}.
	^ component
]

{ #category : 'temporary compat' }
RwProjectDefinitionV2 >> repositoryRootPath [

	^ self repositoryRoot fullName
]
