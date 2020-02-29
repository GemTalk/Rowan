Class {
	#name : 'RwProjectDefinitionV2',
	#superclass : 'RwAbstractProjectDefinitionV2',
	#category : 'Rowan-DefinitionsV2'
}

{ #category : 'topaz tonel support' }
RwProjectDefinitionV2 class >> topazReadTonelFile: fileName [
    "Read a single file and compile the methods within that file.
     Definition/redefinition of the class not implemented yet "
  | projectDef package clsName clsDef cls methBlk envId |
  projectDef := RwRepositoryResolvedProjectTonelReaderVisitorV2
     readClassFile: fileName
     projectName: 'TopazLoad'   "synthetic projed and package names"
     packageName: 'TopazPackage' .
  package := projectDef packageNamed: 'TopazPackage_1' .
  envId :=   projectDef methodEnvForPackageNamed: 'TopazPackage_1' .
  package classDefinitions ifNotNil:[:defs | | sz |
    (sz := defs size) > 1 ifTrue:[
      Error signal:'more than one class definition in file ' , fileName
    ].
    sz > 0 ifTrue:[ clsDef :=  defs values at: 1 ].
  ].
  package classExtensions ifNotNil:[:defs | | sz |
    (sz := defs size) > 1 ifTrue:[
      Error signal:'more than one class extension in file ' , fileName
    ].
    sz > 0 ifTrue:[
      clsDef ifNotNil:[ Error signal:'both class definition and extension in file ', fileName].
      clsDef :=  defs values at: 1.
    ].
  ].   
  clsDef ifNil:[ Error signal:'neither class definition nor extension in file ', fileName].
  clsName := clsDef name .
  (cls := System myUserProfile resolveSymbol: (clsName asSymbol)) ifNil:[
    "creating the class not implemented yet"
    Error signal:'class ' , clsName , ' not found by name (class must already exist)'.
  ].
  cls := cls"anAssociation" value.

  methBlk := [ :methDef "a RwMethodDefinition" |
    [
      false ifTrue:[ GsFile gciLogServer:'compiling ', cls name, ' >> ', methDef selector].
      cls compileMethod: methDef source
      dictionaries: System myUserProfile symbolList
      category: methDef protocol asSymbol
      intoMethodDict: nil "install into the class's dictionaries"
      intoCategories: nil
      intoPragmas: nil
      environmentId:  envId
    ] on: (CompileError, CompileWarning) do:[:ex | 
      ex addText: (RwRepositoryResolvedProjectTonelReaderVisitorV2 lineNumberStringForMethod: methDef ).
      ex pass
    ]
   ].
  clsDef instanceMethodDefinitions do: methBlk .
  cls := cls class .
  clsDef classMethodDefinitions do: methBlk .
]

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
RwProjectDefinitionV2 >> _validate: platformConfigurationAttributes [
	"ensure that the data structures within the receiver contain valid information"

	"make sure that list of packages is consistent between components and project definition
		It's okay to have a definition that is not managed by a component.
		It's NOT okay to have component package that is not defined."

	| definitionPackageNames componentPackageNames missingFromComponent errorMessage |
	definitionPackageNames := self packageNames asSet.
	componentPackageNames := self components _validate: platformConfigurationAttributes.
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
RwProjectDefinitionV2 >> addComponentNamed: componentName toComponentNamed: toComponentName [
	^ self components
		addComponentNamed: componentName
		toComponentNamed: toComponentName
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
RwProjectDefinitionV2 >> addNewComponentNamed: aComponentName [
	^ self addNewComponentNamed: aComponentName condition: 'common' comment: ''
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addNewComponentNamed: aComponentName comment: aString [
	^ self addNewComponentNamed: aComponentName condition: 'common' comment: aString
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addNewComponentNamed: aComponentName condition: condition comment: aString [
	^ self components
		addSimpleComponentNamed: aComponentName
		condition: condition
		comment: aString
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addNewComponentNamed: aComponentName toComponentNamed: toComponentName condition: conditionPathArray [
	"return the path name of the new component"

	^ self
		addNewComponentNamed: aComponentName
		toComponentNamed: toComponentName
		pathNameArray: conditionPathArray
		conditionPathArray: conditionPathArray
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addNewComponentNamed: aComponentName toComponentNamed: toComponentName condition: conditionPathArray comment: aString [
	"return the path name of the new component"

	^ self
		addNewComponentNamed: aComponentName
		toComponentNamed: toComponentName
		pathNameArray: conditionPathArray
		conditionPathArray: conditionPathArray
		comment: aString
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addNewComponentNamed: aComponentName toComponentNamed: toComponentName pathNameArray: pathNameArray conditionPathArray: conditionPathArray [
	"return the path name of the new component"

	^ self
		addNewComponentNamed: aComponentName
		toComponentNamed: toComponentName
		pathNameArray: pathNameArray
		conditionPathArray: conditionPathArray
		comment: ''
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addNewComponentNamed: aComponentName toComponentNamed: toComponentName pathNameArray: pathNameArray conditionPathArray: conditionPathArray comment: aString [
	"return the path name of the new component"

	| theComponentName toComponent path compositePath |
	toComponent := self componentNamed: toComponentName.
	path := RelativePath withAll: pathNameArray.
	1 to: pathNameArray size - 1 do: [ :pathIndex | 
		| segmentName intermediateComponentName |
		"ensure that we have the appropriate intermediate component structure"
		segmentName := pathNameArray at: pathIndex.
		compositePath := compositePath
			ifNil: [ Path * segmentName ]
			ifNotNil: [ compositePath / segmentName ].
		intermediateComponentName := (compositePath / aComponentName) pathString.
		toComponent := self components
			componentNamed: intermediateComponentName
			ifAbsent: [ 
				| newComponent |
				newComponent := self components
					addSimpleNestedComponentNamed: intermediateComponentName
					condition: (conditionPathArray at: pathIndex)
					comment: ''.
				toComponent addComponentNamed: intermediateComponentName.
				newComponent ] ].
	theComponentName := (path / aComponentName) pathString.
	self components
		addSimpleNestedComponentNamed: theComponentName
		condition: conditionPathArray last
		comment: aString.
	toComponent addComponentNamed: theComponentName.
	^ theComponentName
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addNewNestedComponentNamed: aComponentName comment: aString [
	^ self components
		addSimpleNestedComponentNamed: aComponentName
		condition: 'common'
		comment: aString
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addPackageNamed: packageName [
	"the package is expected to already be present in a component - used when reading packages from disk"

	| package |
	(self components componentForPackageNamed: packageName)
		ifNil: [ 
			self
				error:
					'The package ' , packageName printString
						, ' must already be present in a component' ].
	package := RwPackageDefinition newNamed: packageName.
	self
		_addPackage: package
		ifPresent: [ 
			"no problem ... just update the component"
			 ]
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
RwProjectDefinitionV2 >> addPackageNamed: packageName toComponentNamed: componentName gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc [
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
		gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc.
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
RwProjectDefinitionV2 >> addPlatformComponentNamed: aComponentName toComponentNamed: toComponentName alias: alias condition: conditionPathArray [
	"return the path name of the new component"

	| theComponentName toComponent path compositePath condition pathArray |
	self
		deprecated:
			'Should be using addPlatformComponentNamed:toComponentNamed:pathNameArray:conditionPathArray:'.
	toComponent := self componentNamed: toComponentName.
	condition := conditionPathArray last.
	pathArray := conditionPathArray copy.
	pathArray at: pathArray size put: alias.
	path := RelativePath withAll: pathArray.
	path parent
		do: [ :segment | 
			| intermediateComponentName |
			"ensure that we have the appropriate intermediate component structure"
			compositePath := compositePath
				ifNil: [ Path * segment ]
				ifNotNil: [ compositePath / segment ].
			intermediateComponentName := (compositePath / aComponentName) pathString.
			toComponent := self components
				componentNamed: intermediateComponentName
				ifAbsent: [ 
					| newComponent |
					newComponent := self components
						addSimpleNestedComponentNamed: intermediateComponentName
						condition: segment
						comment: ''.
					toComponent addComponentNamed: intermediateComponentName.
					newComponent ] ].
	theComponentName := (path / aComponentName) pathString.
	self components
		addPlatformNestedComponentNamed: theComponentName
		condition: condition
		comment: ''.
	toComponent addComponentNamed: theComponentName.
	^ theComponentName
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addPlatformComponentNamed: aComponentName toComponentNamed: toComponentName pathNameArray: pathNameArray conditionPathArray: conditionPathArray [
	"return the path name of the new component"

	^ self
		addPlatformComponentNamed: aComponentName
		toComponentNamed: toComponentName
		pathNameArray: pathNameArray
		conditionPathArray: conditionPathArray
		comment: ''
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addPlatformComponentNamed: aComponentName toComponentNamed: toComponentName pathNameArray: pathNameArray conditionPathArray: conditionPathArray comment: aString [
	"return the path name of the new component"

	| theComponentName toComponent path compositePath condition |
	toComponent := self componentNamed: toComponentName.
	condition := conditionPathArray last.
	path := RelativePath withAll: pathNameArray.
	1 to: pathNameArray size - 1 do: [ :pathIndex | 
		| segmentName intermediateComponentName |
		"ensure that we have the appropriate intermediate component structure"
		segmentName := pathNameArray at: pathIndex.
		compositePath := compositePath
			ifNil: [ Path * segmentName ]
			ifNotNil: [ compositePath / segmentName ].
		intermediateComponentName := (compositePath / aComponentName) pathString.
		toComponent := self components
			componentNamed: intermediateComponentName
			ifAbsent: [ 
				| newComponent |
				newComponent := self components
					addSimpleNestedComponentNamed: intermediateComponentName
					condition: (conditionPathArray at: pathIndex)
					comment: ''.
				toComponent addComponentNamed: intermediateComponentName.
				newComponent ] ].
	theComponentName := (path / aComponentName) pathString.
	self components
		addPlatformNestedComponentNamed: theComponentName
		condition: condition
		comment: ''.
	toComponent addComponentNamed: theComponentName.
	^ theComponentName
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addPlatformNestedComponentNamed: aComponentName condition: conditionArray comment: commentString [
	^ self components
		addPlatformNestedComponentNamed: aComponentName
		condition: conditionArray
		comment: commentString
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addProjectNamed: projectName toComponentNamed: toComponentName [
	^ self components
		addProjectNamed: projectName
		toComponentNamed: toComponentName
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
RwProjectDefinitionV2 >> addSimpleComponentNamed: aComponentName condition: condition comment: commentString [
	^ self components
		addSimpleComponentNamed: aComponentName
		condition: condition
		comment: commentString
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
RwProjectDefinitionV2 >> addSimpleNestedComponentNamed: aComponentName condition: condition comment: commentString [
	^ self components
		addSimpleNestedComponentNamed: aComponentName
		condition: condition
		comment: commentString
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addSimpleNestedComponentNamed: aComponentName condition: condition groupName: groupName comment: commentString [
	^ self components
		addSimpleNestedComponentNamed: aComponentName
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

{ #category : 'accessing' }
RwProjectDefinitionV2 >> movePackageNamed: aPackageName toComponentNamed: aComponentName [
	self components removePackageNamed: aPackageName.
	self components
		addPackageNamed: aPackageName
		toComponentNamed: aComponentName
]

{ #category : 'properties' }
RwProjectDefinitionV2 >> projectName [
	^ self propertyAt: 'name' ifAbsent: [ nil ]
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

{ #category : 'reading' }
RwProjectDefinitionV2 >> readProjectSet [

	^ Rowan projectTools readV2 readProjectSetForComponentProjectDefinition: self
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
RwProjectDefinitionV2 >> removePackageNamed: packageName fromComponentNamed: componentName [
	"do not remove package from defintion, remove it from the named component only. 
		Use removePackage:, if you want the package completely removed from definition"

	| component |
	component := self componentNamed: componentName.
	component removePackageNamed: packageName.
	^ component
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

{ #category : 'accessing' }
RwProjectDefinitionV2 >> renameComponentNamed: aComponentPath to: aComponentName [
	^ self components renameComponentNamed: aComponentPath to: aComponentName
]

{ #category : 'temporary compat' }
RwProjectDefinitionV2 >> repositoryRootPath [

	^ self repositoryRoot fullName
]
