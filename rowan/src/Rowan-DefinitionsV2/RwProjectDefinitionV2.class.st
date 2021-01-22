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

{ #category : 'components' }
RwProjectDefinitionV2 >> addComponentNamed: componentName toComponentNamed: toComponentName [
	"add existing component named componentName to component named toComponentName"

	^ self components
		addComponentNamed: componentName
		toComponentNamed: toComponentName
]

{ #category : 'components' }
RwProjectDefinitionV2 >> addComponentStructureFor: componentBasename startingAtComponentNamed: toComponentName pathNameArray: pathNameArray conditionPathArray: conditionPathArray comment: aString [
	"
	<pathNameArray> and <conditionPathArray> should be of equal size. The 
		<pathNameArray> lists the names of the directories that will be created 
		on demand starting in the parent directory of the <toComponentName> 
		component. <conditionPathArray> lists the conditions that will be used 
		when creating the subcomponent at each level. If the condition is an Array
		a platform subcomponent will be created, otherwise a subcomponent
		will be created. The name of each subcomponent formed using 
		<componentBasename> and the directory path based on the <pathNameArray>.
		The name of the first subcomponent created will be added to the component
		names of the <toComponentName> component.

	Return the last component created.
	"

	| theComponentName toComponent path compositePath condition theLastComponent |
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
		intermediateComponentName := (compositePath / componentBasename) pathString.
		toComponent := self components
			componentNamed: intermediateComponentName
			ifAbsent: [ 
				| newComponent |
				newComponent := self
					addSubcomponentNamed: intermediateComponentName
					condition: (conditionPathArray at: pathIndex)
					comment: aString.
				toComponent addComponentNamed: intermediateComponentName.
				newComponent ] ].
	theComponentName := (path / componentBasename) pathString.
	theLastComponent := condition _isArray
		ifTrue: [ 
			self
				addPlatformSubcomponentNamed: theComponentName
				condition: condition
				comment: aString ]
		ifFalse: [ 
			self
				addSubcomponentNamed: theComponentName
				condition: condition
				comment: aString ].
	toComponent addComponentNamed: theComponentName.
	^ theLastComponent
]

{ #category : 'components' }
RwProjectDefinitionV2 >> addLoadComponentNamed: aComponentName [
	"add a new instance of RwLoadComponent to the project components"

	^ self addLoadComponentNamed: aComponentName comment: ''
]

{ #category : 'components' }
RwProjectDefinitionV2 >> addLoadComponentNamed: aComponentName comment: aString [
	"add a new instance of RwLoadComponent to the project components"

	(UserGlobals at: #'USE_NEW_COMPONENT_CLASSES' ifAbsent: [ false ])
		ifTrue: [ ^ self components addLoadComponentNamed: aComponentName comment: aString ]
		ifFalse: [ 
			^ self components
				addSimpleComponentNamed: aComponentName
				condition: 'common'
				comment: aString ]
]

{ #category : 'components to be cleaned up' }
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
			 ].
	^ package
]

{ #category : 'components' }
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

{ #category : 'components' }
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

{ #category : 'components to be cleaned up' }
RwProjectDefinitionV2 >> addPackages: somePackageNames forComponent: aComponent [
	"not sure I like how this is used ... the component structure needs to be kept in sync with packages, so this is not quite the route to go, unless we ensure that the component has an entry for the package"

	"see similar comment in addRawPackageNamed: _addComponent"

	"should be sent from the component visitor ... not unexpected to have a duplicate, but the new
		component --- presumably freshly read from disk --- wins"

	self components _addComponent: aComponent.
	somePackageNames asSet
		do: [ :packageName | self _addPackage: (RwPackageDefinition newNamed: packageName) ]
]

{ #category : 'components' }
RwProjectDefinitionV2 >> addPackagesNamed: packageNames toComponentNamed: componentName [
	^ packageNames
		collect: [ :packageName | self addPackageNamed: packageName toComponentNamed: componentName ]
]

{ #category : 'components to be cleaned up' }
RwProjectDefinitionV2 >> addPlatformNestedComponentNamed: aComponentName condition: conditionArray comment: commentString [
	^ self components
		addPlatformNestedComponentNamed: aComponentName
		condition: conditionArray
		comment: commentString
]

{ #category : 'components' }
RwProjectDefinitionV2 >> addPlatformSubcomponentNamed: aComponentName condition: condition comment: aString [
	(UserGlobals at: #'USE_NEW_COMPONENT_CLASSES' ifAbsent: [ false ])
		ifTrue: [ 
			^ self components
				addPlatformSubcomponentNamed: aComponentName
				condition: condition
				comment: aString ]
		ifFalse: [ 
			^ self components
				addPlatformNestedComponentNamed: aComponentName
				condition: condition
				comment: aString ]
]

{ #category : 'components' }
RwProjectDefinitionV2 >> addProjectNamed: projectName toComponentNamed: toComponentName [
	^ self components
		addProjectNamed: projectName
		toComponentNamed: toComponentName
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> addRawPackageNamed: packageName [
	"not sure I like how this is used ... the component structure needs to be kept in sync with packages, so this is not quite the route to go, unless we ensure that the component has an entry for the package"

	"see similar comment in addPackages:forComponent: and _addComponent"

	^ self _addPackage: (RwPackageDefinition newNamed: packageName)
]

{ #category : 'components to be cleaned up' }
RwProjectDefinitionV2 >> addSimpleComponentNamed: aComponentName condition: condition comment: commentString [
	^ self components
		addSimpleComponentNamed: aComponentName
		condition: condition
		comment: commentString
]

{ #category : 'components to be cleaned up' }
RwProjectDefinitionV2 >> addSimpleNestedComponentNamed: aComponentName condition: condition comment: commentString [
	^ self components
		addSimpleNestedComponentNamed: aComponentName
		condition: condition
		comment: commentString
]

{ #category : 'components' }
RwProjectDefinitionV2 >> addSubcomponentNamed: aComponentName condition: condition comment: aString [
	(UserGlobals at: #'USE_NEW_COMPONENT_CLASSES' ifAbsent: [ false ])
		ifTrue: [ 
			^ self components
				addSubcomponentNamed: aComponentName
				condition: condition
				comment: aString ]
		ifFalse: [ 
			^ self components
				addSimpleNestedComponentNamed: aComponentName
				condition: condition
				comment: aString ]
]

{ #category : 'querying' }
RwProjectDefinitionV2 >> componentForPackageNamed: packageName [
	"Answer nil if no component found"

	^ self components componentForPackageNamed: packageName
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

{ #category : 'components' }
RwProjectDefinitionV2 >> movePackageNamed: aPackageName toComponentNamed: aComponentName [
	self
		movePackageNamed: aPackageName
		toComponentNamed: aComponentName
		asPackageName: aPackageName
]

{ #category : 'components' }
RwProjectDefinitionV2 >> movePackageNamed: aPackageName toComponentNamed: aComponentName asPackageName: newPackageName [
	self components removePackageNamed: aPackageName.
	self components
		addPackageNamed: newPackageName
		toComponentNamed: aComponentName
]

{ #category : 'querying' }
RwProjectDefinitionV2 >> packageForClassNamed: className [
	"Answer nil if no class found"

	self packages
		do: [ :package | 
			(package classDefinitions includesKey: className)
				ifTrue: [ ^ package ] ].
	^ nil
]

{ #category : 'properties' }
RwProjectDefinitionV2 >> projectName [
	^ self propertyAt: 'name' ifAbsent: [ nil ]
]

{ #category : 'properties' }
RwProjectDefinitionV2 >> projectName: aString [
	self propertyAt: 'name' put: aString
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> removePackage: aPackageDefinition [
	self components removePackageNamed: aPackageDefinition name.
	^ super removePackage: aPackageDefinition
]

{ #category : 'components' }
RwProjectDefinitionV2 >> removePackageNamed: packageName fromComponentNamed: componentName [
	"do not remove package from defintion, remove it from the named component only. 
		Use removePackage:, if you want the package completely removed from definition"

	| component |
	component := self componentNamed: componentName.
	component removePackageNamed: packageName.
	^ component
]

{ #category : 'components' }
RwProjectDefinitionV2 >> renameComponentNamed: aComponentPath to: aComponentName [
	^ self components renameComponentNamed: aComponentPath to: aComponentName
]

{ #category : 'accessing' }
RwProjectDefinitionV2 >> renamePackageNamed: packageName to: newPackageName [
	| theComponent |
	theComponent := self componentForPackageNamed: packageName.
	self
		movePackageNamed: packageName
		toComponentNamed: theComponent name
		asPackageName: newPackageName
]

{ #category : 'temporary compat' }
RwProjectDefinitionV2 >> repositoryRootPath [

	^ self repositoryRoot fullName
]
