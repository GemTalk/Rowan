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

	| loadSpecification projectDefinition pDict |
	loadSpecification := anRwLoadSpecificationV2 copy.
	(pDict := SymbolDictionary new)
						at: #'name' put: loadSpecification projectName .
	(projectDefinition := RwProjectDefinition basicNew)
		properties: pDict;
		packages: Dictionary new;
		projectDefinitionSourceProperty:
				RwLoadedProject _projectModifiedProjectSourceValue.
	^ self basicNew
		_projectDefinition: projectDefinition;
		_projectComponents: RwResolvedProjectComponentsV2 new;
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
RwResolvedProjectV2 class >> loadSpecification: anRwLoadSpecificationV2 customConditionalAttributes: customConditionalAttributes platformAttributes: platformAttributes [
	"resolve ensures that the project directory already exists on disk (cloned for git projects) or created on disk for new projects
		answer  the project definition specified by the receiver and any dependent projects"

	"if the project directory already exists on disk, then read the project definition(s) from disk"

	^ (self basicLoadSpecification: anRwLoadSpecificationV2)
		resolve: customConditionalAttributes platformConditionalAttributes: platformAttributes
]

{ #category : 'instance creation' }
RwResolvedProjectV2 class >> loadSpecificationProjectSet: anRwLoadSpecificationV2 [
	"resolve ensures that the project directory already exists on disk (cloned for git projects) or created on disk for new projects
		answer  the project definition specified by the receiver and any dependent projects"

	"if the project directory already exists on disk, then read the project definition(s) from disk"

	^(self basicLoadSpecification: anRwLoadSpecificationV2)
		resolveProjectSet
]

{ #category : 'instance creation' }
RwResolvedProjectV2 class >> loadSpecificationProjectSet: anRwLoadSpecificationV2 customConditionalAttributes: customConditionalAttributes [
	"resolve ensures that the project directory already exists on disk (cloned for git projects) 
		or created on disk for new projects. If the project directory already exists on disk, 
		then read the project definition(s) from disk using the specified customConditionalAttributes

		Answer  the project definition specified by the receiver and any dependent projects"

	^ (self basicLoadSpecification: anRwLoadSpecificationV2)
		resolveProjectSet: customConditionalAttributes
]

{ #category : 'instance creation' }
RwResolvedProjectV2 class >> loadSpecificationProjectSet: anRwLoadSpecificationV2 customConditionalAttributes: customConditionalAttributes platformAttributes: platformAttributes [
	"resolve ensures that the project directory already exists on disk (cloned for git projects) 
		or created on disk for new projects. If the project directory already exists on disk, 
		then read the project definition(s) from disk using the specified customConditionalAttributes
		and platformAttributes.

		Answer  the project definition specified by the receiver and any dependent projects"

	^ (self basicLoadSpecification: anRwLoadSpecificationV2)
		resolveProjectSet: customConditionalAttributes
		platformConditionalAttributes: platformAttributes
]

{ #category : 'private' }
RwResolvedProjectV2 >> _addPackageNames: somePackageNames for: aComponent [
	self addPackages: somePackageNames forComponent: aComponent
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

{ #category : 'project definition' }
RwResolvedProjectV2 >> _projectDefinitionCustomConditionalAttributes [
	^ self _projectDefinition _projectDefinitionCustomConditionalAttributes
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> _projectDefinitionCustomConditionalAttributes: customConditionalAttributes [
	self _projectDefinition _projectDefinitionCustomConditionalAttributes: customConditionalAttributes
]

{ #category : 'comparing' }
RwResolvedProjectV2 >> _projectDefinitionForCompare [
	^ self _projectDefinition
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> _projectDefinitionPlatformConditionalAttributes [
	^ self _projectDefinition _projectDefinitionPlatformConditionalAttributes
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> _projectDefinitionPlatformConditionalAttributes: platformConditionalAttributes [
	self _projectDefinition _projectDefinitionPlatformConditionalAttributes: platformConditionalAttributes
]

{ #category : 'visiting' }
RwResolvedProjectV2 >> acceptVisitor: aVisitor [
	^ aVisitor visitResolvedProjectV2: self
]

{ #category : 'components' }
RwResolvedProjectV2 >> addComponentNamed: componentName toComponentNamed: toComponentName [
	"add existing component named componentName to component named toComponentName"

	^ self _projectComponents
		addComponentNamed: componentName
		toComponentNamed: toComponentName
]

{ #category : 'components' }
RwResolvedProjectV2 >> addComponentOrPackageGroup: aComponentOrPackageGroup [
	"add existing component "

	^ aComponentOrPackageGroup _addToResolvedProjectComponents: self _projectComponents
]

{ #category : 'components' }
RwResolvedProjectV2 >> addComponentOrPackageGroup: aComponentOrPackageGroup toComponentNamed: toComponentName [
	"add existing component to component named toComponentName"

	self addComponentOrPackageGroup: aComponentOrPackageGroup.
	self
		addComponentNamed: aComponentOrPackageGroup name
		toComponentNamed: toComponentName
]

{ #category : 'component structure' }
RwResolvedProjectV2 >> addComponentStructureFor: componentBasename startingAtComponentNamed: toComponentName pathNameArray: pathNameArray conditionPathArray: conditionPathArray comment: aString [
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
		toComponent := self _projectComponents
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
RwResolvedProjectV2 >> addLoadComponentNamed: componentName [
	"add a new instance of RwLoadComponent to the project components and add the componentName
		to the load spec (i.e., it will be loaded when the load spec is loaded)"

	^ self addLoadComponentNamed: componentName comment: ''
]

{ #category : 'components' }
RwResolvedProjectV2 >> addLoadComponentNamed: aComponentName comment: aString [
	"add a new instance of RwLoadComponent to the project components and add the componentName
		to the load spec (i.e., it will be loaded when the load spec is loaded)"

	self _loadSpecification addComponentNamed: aComponentName.
	(UserGlobals at: #'USE_NEW_COMPONENT_CLASSES' ifAbsent: [ false ])
		ifTrue: [ ^ self _projectComponents addLoadComponentNamed: aComponentName comment: aString ]
		ifFalse: [ 
			^ self _projectComponents
				addSimpleComponentNamed: aComponentName
				condition: 'common'
				comment: aString ]
]

{ #category : 'components' }
RwResolvedProjectV2 >> addPackageGroupNamed: aComponentName condition: condition comment: aString [
	^ self _projectComponents
		addPackageGroupNamed: aComponentName
		condition: condition
		comment: aString
]

{ #category : 'component structure' }
RwResolvedProjectV2 >> addPackageGroupStructureFor: componentBasename startingAtComponentNamed: toComponentName pathNameArray: pathNameArray conditionPathArray: conditionPathArray comment: aString [
	"
	<pathNameArray> and <conditionPathArray> should be of equal size. The 
		<pathNameArray> lists the names of the directories that will be created 
		on demand starting in the parent directory of the <toComponentName> 
		component. <conditionPathArray> lists the conditions that will be used 
		when creating the package group at each level. The name of each
		package group is formed using  <componentBasename> and the directory
		path based on the <pathNameArray>. The name of the first pacakge 
		group created will be added to the component names of the 
		<toComponentName> component.

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
		toComponent := self
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
	theLastComponent := self
		addPackageGroupNamed: theComponentName
		condition: condition
		comment: aString.
	toComponent addComponentNamed: theComponentName.
	^ theLastComponent
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addPackageNamed: packageName [
	"the package is expected to already be present in a component - used when reading packages from disk"

	| package |
	(self _projectComponents componentForPackageNamed: packageName)
		ifNil: [ 
			self
				error:
					'The package ' , packageName printString
						, ' must already be present in a component' ].
	package := RwPackageDefinition newNamed: packageName.
	self _projectDefinition
		_addPackage: package
		ifPresent: [ 
			"no problem ... just update the component"
			 ].
	^ package
]

{ #category : 'components' }
RwResolvedProjectV2 >> addPackageNamed: packageName toComponentNamed: componentName [
	| package |
	self
		componentNamed: componentName
		ifAbsent: [ self error: 'The component ' , componentName printString , ' is undefined' ].
	package := RwPackageDefinition newNamed: packageName.
	self _projectDefinition
		_addPackage: package
		ifPresent: [ 
			"no problem ... just update the component"
			 ].
	self _projectComponents
		addPackageNamed: packageName
		toComponentNamed: componentName.
	^ package
]

{ #category : 'components' }
RwResolvedProjectV2 >> addPackageNamed: packageName toComponentNamed: componentName gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc [
	| package |
	self
		componentNamed: componentName
		ifAbsent: [ self error: 'The component ' , componentName printString , ' is undefined' ].
	package := RwPackageDefinition newNamed: packageName.
	self _projectDefinition
		_addPackage: package
		ifPresent: [ 
			"no problem ... just update the component"
			 ].
	self _projectComponents
		addPackageNamed: packageName
		toComponentNamed: componentName
		gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc.
	^ package
]

{ #category : 'components' }
RwResolvedProjectV2 >> addPackages: somePackageNames forComponent: aComponent [
	"not sure I like how this is used ... the component structure needs to be kept in sync with packages, so this is not quite the route to go, unless we ensure that the component has an entry for the package"

	"see similar comment in addRawPackageNamed: _addComponent"

	"should be sent from the component visitor ... not unexpected to have a duplicate, but the new
		component --- presumably freshly read from disk --- wins"

	self _projectComponents _addComponent: aComponent.
	somePackageNames asSet
		do: [ :packageName | self _projectDefinition _addPackage: (RwPackageDefinition newNamed: packageName) ]
]

{ #category : 'components' }
RwResolvedProjectV2 >> addPackagesNamed: packageNames toComponentNamed: componentName [
	^ packageNames
		collect: [ :packageName | self addPackageNamed: packageName toComponentNamed: componentName ]
]

{ #category : 'components' }
RwResolvedProjectV2 >> addPlatformSubcomponentNamed: aComponentName condition: conditionArray comment: aString [
	"Add the named subcomponent with the given condition to the named project and add the new component to the toComponentName component"

	(UserGlobals at: #'USE_NEW_COMPONENT_CLASSES' ifAbsent: [ false ])
		ifTrue: [ 
			^ self _projectComponents
				addPlatformSubcomponentNamed: aComponentName
				condition: conditionArray
				comment: aString ]
		ifFalse: [ 
			^ self _projectComponents
				addPlatformNestedComponentNamed: aComponentName
				condition: conditionArray
				comment: aString ]
]

{ #category : 'components' }
RwResolvedProjectV2 >> addPlatformSubcomponentNamed: aComponentName condition: conditionArray comment: aString toComponentNamed: toComponentName [
	"Add the named subcomponent with the given condition to the named project and add the new component to the toComponentName component"

	| sub |
	sub := self
		addPlatformSubcomponentNamed: aComponentName
		condition: conditionArray
		comment: aString.
	self addComponentNamed: aComponentName toComponentNamed: toComponentName.
	^ sub
]

{ #category : 'components' }
RwResolvedProjectV2 >> addPlatformSubcomponentNamed: aComponentName condition: conditionArray toComponentNamed: toComponentName [
	"Add the named subcomponent with the given condition to the named project and add the new component to the toComponentName component"

	^ self
		addPlatformSubcomponentNamed: aComponentName
		condition: conditionArray
		comment: ''
		toComponentNamed: toComponentName
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

{ #category : 'components' }
RwResolvedProjectV2 >> addProjectNamed: projectName toComponentNamed: toComponentName [
	^ self _projectComponents
		addProjectNamed: projectName
		toComponentNamed: toComponentName
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> addRawPackageNamed: packageName [
	"not sure I like how this is used ... the component structure needs to be kept in sync with packages, so this is not quite the route to go, unless we ensure that the component has an entry for the package"

	"see similar comment in addPackages:forComponent: and _addComponent"

	^ self _projectDefinition addRawPackageNamed: packageName
]

{ #category : 'components' }
RwResolvedProjectV2 >> addSubcomponentNamed: componentName condition: condition [
	^ self addSubcomponentNamed: componentName condition: condition comment: ''
]

{ #category : 'components' }
RwResolvedProjectV2 >> addSubcomponentNamed: aComponentName condition: condition comment: aString [
	(UserGlobals at: #'USE_NEW_COMPONENT_CLASSES' ifAbsent: [ false ])
		ifTrue: [ 
			^ self _projectComponents
				addSubcomponentNamed: aComponentName
				condition: condition
				comment: aString ]
		ifFalse: [ 
			^ self _projectComponents
				addSimpleNestedComponentNamed: aComponentName
				condition: condition
				comment: aString ]
]

{ #category : 'components' }
RwResolvedProjectV2 >> addSubcomponentNamed: componentName condition: condition comment: aString toComponentNamed: toComponentName [
	"Add the named subcomponent with the given condition to the named project and add the new component to the toComponentName component"

	| sub |
	sub := self
		addSubcomponentNamed: componentName
		condition: condition
		comment: aString.
	self addComponentNamed: componentName toComponentNamed: toComponentName.
	^ sub
]

{ #category : 'components' }
RwResolvedProjectV2 >> addSubcomponentNamed: componentName condition: condition toComponentNamed: toComponentName [
	"Add the named subcomponent with the given condition to the named project and add the new component to the toComponentName component"

	^ self
		addSubcomponentNamed: componentName
		condition: condition
		comment: ''
		toComponentNamed: toComponentName
]

{ #category : 'component structure' }
RwResolvedProjectV2 >> addSubcomponentStructureFor: componentBasename startingAtComponentNamed: toComponentName conditionPathArray: conditionPathArray [
	"
	PREFERRED method for adding subcomponent structure following the convention that the directory names reflect the condition names.

	When adding platform component structure, where the condition is an Array instead of a String, use 
		addComponentStructureFor:startingAtComponentNamed:pathNameArray:conditionPathArray:.

	Return the last component created.
	"

	^ self 
		addSubcomponentStructureFor: componentBasename
		startingAtComponentNamed: toComponentName
		conditionPathArray: conditionPathArray
		comment: ''
]

{ #category : 'component structure' }
RwResolvedProjectV2 >> addSubcomponentStructureFor: componentBasename startingAtComponentNamed: toComponentName conditionPathArray: conditionPathArray comment: aString [
	"
	PREFERRED method for adding subcomponent structure following the convention that the directory names reflect the condition names.

	When adding platform component structure, where the condition is an Array instead of a String, use 
		addComponentStructureFor:startingAtComponentNamed:pathNameArray:conditionPathArray:.

	Return the last component created.
	"

	^ self 
		addComponentStructureFor: componentBasename
		startingAtComponentNamed: toComponentName
		pathNameArray: conditionPathArray
		conditionPathArray: conditionPathArray
		comment: aString
]

{ #category : 'components' }
RwResolvedProjectV2 >> allComponentsIn: componentNameOrArrayOfNames matchBlock: matchBlock notFound: notFoundBlock do: aBlock [
	| visited theBlock componentNames subs |
	visited := IdentitySet new.
	theBlock := [ :component | 
	(visited includes: component)
		ifFalse: [ 
			aBlock value: component.
			visited add: component.
			subs := self
				subcomponentsOf: component name
				matchBlock: matchBlock
				ifNone: notFoundBlock.
			subs do: theBlock ] ].
	componentNames := componentNameOrArrayOfNames _isArray
		ifTrue: [ componentNameOrArrayOfNames ]
		ifFalse: [ {componentNameOrArrayOfNames} ].
	^ componentNames
		do: [ :componentName | 
			subs := self
				subcomponentsOf: componentName
				matchBlock: matchBlock
				ifNone: notFoundBlock.
			subs do: theBlock ]
]

{ #category : 'components' }
RwResolvedProjectV2 >> allPackageNamesIn: componentNameOrArrayOfNames matchBlock: matchBlock notFound: notFoundBlock [
	| packageNames visited theBlock attributes componentNames |
	visited := IdentitySet new.
	packageNames := Set new.
	attributes := self platformConditionalAttributes.
	theBlock := [ :component | 
	(visited includes: component)
		ifFalse: [ 
			packageNames addAll: component packageNames.
			visited add: component.
			(self
				subcomponentsOf: component name
				matchBlock: matchBlock
				ifNone: notFoundBlock) do: theBlock ] ].
	componentNames := componentNameOrArrayOfNames _isArray
		ifTrue: [ componentNameOrArrayOfNames ]
		ifFalse: [ {componentNameOrArrayOfNames} ].
	componentNames
		do: [ :componentName | 
			| aComponent |
			aComponent := self _projectComponents
				componentNamed: componentName
				ifAbsent: [ notFoundBlock cull: componentName ].
			packageNames addAll: aComponent packageNames.
			visited add: aComponent.
			(self
				subcomponentsOf: componentName
				matchBlock: matchBlock
				ifNone: notFoundBlock) do: theBlock ].
	^ packageNames asArray sort
]

{ #category : 'converting' }
RwResolvedProjectV2 >> asDefinition [
  ^ self
]

{ #category : 'actions' }
RwResolvedProjectV2 >> checkout: aCommittish [
	^ self _projectRepository checkout: aCommittish
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> comment [
	^ self _loadSpecification comment
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> comment: aString [
	self _loadSpecification comment: aString
]

{ #category : 'querying' }
RwResolvedProjectV2 >> commitId [
	^ self _projectRepository commitId
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

	^ self _projectComponents componentForPackageNamed: packageName
]

{ #category : 'components' }
RwResolvedProjectV2 >> componentNamed: aComponentName [
	^ self _projectComponents componentNamed: aComponentName
]

{ #category : 'components' }
RwResolvedProjectV2 >> componentNamed: aComponentName ifAbsent: absentBlock [
	^ self _projectComponents componentNamed: aComponentName ifAbsent: absentBlock
]

{ #category : 'components' }
RwResolvedProjectV2 >> componentOrPackageGroupNamed: aComponentName [
	^ self _projectComponents componentOrPackageGroupNamed: aComponentName
]

{ #category : 'components' }
RwResolvedProjectV2 >> componentOrPackageGroupNamed: aComponentName ifAbsent: absentBlock [
	^ self _projectComponents
		componentOrPackageGroupNamed: aComponentName
		ifAbsent: absentBlock
]

{ #category : 'accessing' }
RwResolvedProjectV2 >> componentsWithDoits [
	^ self _projectComponents componentsWithDoits
]

{ #category : 'accessing' }
RwResolvedProjectV2 >> conditionalAttributes: conditionalAttributes [
	^ self _projectDefinitionCustomConditionalAttributes: conditionalAttributes
]

{ #category : 'copying' }
RwResolvedProjectV2 >> copyForLoadedProject [
	"project definition is not part of the copy for loaded projects"

	^ RwResolvedProjectV2 new
		_projectRepository: projectRepository copy;
		_loadSpecification: loadSpecification copy;
		_projectSpecification: projectSpecification copy;
		_projectComponents: projectComponents copy;
		yourself
]

{ #category : 'actions' }
RwResolvedProjectV2 >> create [
	"RwComponentProjectDefinition tests compatibility ... eventually get rid of this"
	self resolve; export
]

{ #category : 'accessing' }
RwResolvedProjectV2 >> customConditionalAttributes [
	^ self _projectDefinitionCustomConditionalAttributes
		ifNil: [ super customConditionalAttributes ]
]

{ #category : '-- loader compat --' }
RwResolvedProjectV2 >> diskRepositoryRoot: repositoryRootPathString [
	projectRepository := self _projectRepository
		diskRepositoryRoot: repositoryRootPathString
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
	self _projectComponents export: self componentsRoot
]

{ #category : 'exporting' }
RwResolvedProjectV2 >> exportPackages [
	| projectSetDefinition |
	projectSetDefinition := [  | def |
	  (def := RwProjectSetDefinition new)
		  addProject: self copy read .
		def ]
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
	(visitor := writerVisitorClass new)
		packagesRoot: packagesRoot .

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
RwResolvedProjectV2 >> exportTopazFormatTo: filePath logClassCreation: logClassCreation excludeClassInitializers: excludeClassInitializers excludeRemoveAllMethods: excludeRemoveAllMethods [
	| projectSetDefinition projectSetModification visitor fileReference |
	fileReference := filePath asFileReference.
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: self.
	projectSetModification := projectSetDefinition
		compareAgainstBase: RwProjectSetDefinition new.
	(visitor := RwGsModificationTopazWriterVisitorV2 new)
		logCreation: logClassCreation;
		excludeClassInitializers: excludeClassInitializers;
		excludeRemoveAllMethods: excludeRemoveAllMethods;
		repositoryRootPath: fileReference parent;
		topazFilename: fileReference base;
		filenameExtension: fileReference extension .

	visitor visit: projectSetModification
]

{ #category : 'exporting' }
RwResolvedProjectV2 >> exportTopazFormatTo: filePath logClassCreation: logClassCreation excludeClassInitializers: excludeClassInitializers excludeRemoveAllMethods: excludeRemoveAllMethods usingPackageNamesMap: packageNamesMap [
	| projectSetDefinition projectSetModification visitor fileReference |
	fileReference := filePath asFileReference.
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: self.
	projectSetModification := projectSetDefinition
		compareAgainstBase: RwProjectSetDefinition new.
	(visitor := RwGsModificationTopazWriterVisitorV2 new)
		logCreation: logClassCreation;
		excludeClassInitializers: excludeClassInitializers;
		excludeRemoveAllMethods: excludeRemoveAllMethods;
		repositoryRootPath: fileReference parent;
		topazFilename: fileReference base;
		topazFilenamePackageNamesMap: packageNamesMap .

	visitor visit: projectSetModification
]

{ #category : '-- loader compat --' }
RwResolvedProjectV2 >> gitRepositoryRoot: repositoryRootPathString [
	projectRepository := self _projectRepository
		gitRepositoryRoot: repositoryRootPathString
		revision: self _loadSpecification revision
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
	projectDefinition := RwProjectDefinition new.
	projectComponents := RwResolvedProjectComponentsV2 new
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
	"
		load only the receiver into the image. Required projects for the receiver are only loaded if they are not already 
			present in the image.

		To explicitly load the receiver AND required projects, construct a project set containing projects to be loaded 
			and send #load to the project set.
	"

	self _validate: self platformConditionalAttributes, self customConditionalAttributes.
	^ Rowan projectTools loadV2 loadProjectDefinition: self projectDefinition
]

{ #category : 'actions' }
RwResolvedProjectV2 >> load: instanceMigrator [
	"
		load only the receiver into the image, using the specified instance migrator. Required projects for the receiver are only 
			loaded if they are not already present in the image.
	"

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
	"
		refresh the contents of the receiver from disk and create a project set that includes project definitions of
			required projects, also read from disk. Then load the entire project set.
	"

	self _validate: self platformConditionalAttributes.
	^ Rowan projectTools loadV2
		loadProjectSetDefinition:
			(self
				readProjectSet: self customConditionalAttributes
				platformConditionalAttributes: self platformConditionalAttributes)
]

{ #category : 'actions' }
RwResolvedProjectV2 >> loadProjectSet: conditionalAttributes [
	"
		refresh the contents of the receiver from disk and create a project set that
			includes project definitions of required projects, also read from disk. Then
			load the entire project set.

		Use the specified conditional attributes when reading the receiver from disk.
	"

	^ Rowan projectTools loadV2
		loadProjectSetDefinition: (self readProjectSet: conditionalAttributes)
]

{ #category : 'actions' }
RwResolvedProjectV2 >> loadProjectSet: conditionalAttributes instanceMigrator: instanceMigrator [
	"
		refresh the contents of the receiver from disk and create a project set 
			that includes project definitions of required projects, also read from disk. 
			Then load the entire project set, using the specified instance migrator.

		Use the specified conditional attributes when reading the receiver from disk.

		Use the instanceMigrator to handle new versions of any classes that may result from the load.
	"

	^ Rowan projectTools loadV2
		loadProjectSetDefinition: (self readProjectSet: conditionalAttributes)
		instanceMigrator: instanceMigrator
]

{ #category : 'actions' }
RwResolvedProjectV2 >> loadProjectSet: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [
	"
		refresh the contents of the receiver from disk and create a project set that includes project definitions of
			required projects, also read from disk. Then load the entire project set.

		Use the specified platform conditional attributes when reading the receiver from disk.
	"

	self _validate: self platformConditionalAttributes.
	^ Rowan projectTools loadV2
		loadProjectSetDefinition:
			(self
				readProjectSet: customConditionalAttributes
				platformConditionalAttributes: platformConditionalAttributes)
]

{ #category : 'actions' }
RwResolvedProjectV2 >> loadProjectSet: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes instanceMigrator: instanceMigrator [
	"
		refresh the contents of the receiver from disk and create a project set that includes project definitions of
			required projects, also read from disk. Then load the entire project set, using the specified 
			instance migrator.

		Use the specified platform conditional attributes when reading the receiver from disk.

		Use the instanceMigrator to handle new versions of any classes that may result from the load.
	"

	self _validate: self platformConditionalAttributes.
	^ Rowan projectTools loadV2
		loadProjectSetDefinition:
			(self
				readProjectSet: customConditionalAttributes
				platformConditionalAttributes: platformConditionalAttributes)
		instanceMigrator: instanceMigrator
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> moveClassExtensionNamed: aClassName fromPackageNamed: fromPackageName toPackageNamed: toPackageName [
	| toPackage fromPackage classDef |
	toPackage := self
		packageNamed: toPackageName
		ifAbsent: [ self error: 'No package found name ' , toPackageName printString ].
	fromPackage := self
		packageNamed: fromPackageName
		ifAbsent: [ self error: 'No package found name ' , fromPackageName printString ].
	classDef := fromPackage removeClassExtensionNamed: aClassName.
	classDef
		moveToPackageNamed: toPackageName
		packageConvention: self packageConvention.
	toPackage addClassExtensionDefinition: classDef
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> moveClassNamed: aClassName toPackageNamed: aPackageName [
	| thePackage |
	thePackage := self
		packageNamed: aPackageName
		ifAbsent: [ self error: 'No package found name ' , aPackageName printString ].
	(self packageForClassNamed: aClassName)
		ifNil: [ self error: 'No class definition found for ' , aClassName printString ]
		ifNotNil: [ :package | 
			| classDef |
			classDef := package removeClassNamed: aClassName.
			classDef
				moveToPackageNamed: aPackageName
				packageConvention: self packageConvention.
			thePackage addClassDefinition: classDef ]
]

{ #category : 'components' }
RwResolvedProjectV2 >> movePackageNamed: aPackageName toComponentNamed: aComponentName [
	^ self
		movePackageNamed: aPackageName
		toComponentNamed: aComponentName
		asPackageName: aPackageName
]

{ #category : 'components' }
RwResolvedProjectV2 >> movePackageNamed: aPackageName toComponentNamed: aComponentName asPackageName: newPackageName [
	self _projectComponents removePackageNamed: aPackageName.
	^ self _projectComponents
		addPackageNamed: newPackageName
		toComponentNamed: aComponentName
]

{ #category : '-- loader compat --' }
RwResolvedProjectV2 >> name [
	"sender in loader code that's shared between RwComponentProjectDefinition and RwResolvedProjectV2, 
		should use projectAlias, but need to wait until we're no longer using RwComponentProjectDefinition"

	^ self projectName
]

{ #category : 'querying' }
RwResolvedProjectV2 >> packageForClassNamed: className [
	"Answer nil if no class found"

	^ self _projectDefinition packageForClassNamed: className
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

{ #category : 'components' }
RwResolvedProjectV2 >> packageGroupNamed: aPackageGroupName [
	^ self _projectComponents packageGroupNamed: aPackageGroupName
]

{ #category : 'components' }
RwResolvedProjectV2 >> packageGroupNamed: aComponentName ifAbsent: absentBlock [
	^ self _projectComponents
		packageGroupNamed: aComponentName
		ifAbsent: absentBlock
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

{ #category : 'comparing' }
RwResolvedProjectV2 >> packagesForCompare [
	^ self packages
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

{ #category : 'accessing' }
RwResolvedProjectV2 >> projectVersion [
	^ self _projectSpecification projectVersion
]

{ #category : 'accessing' }
RwResolvedProjectV2 >> projectVersion: aStringOrVersion [
	^ self _projectSpecification projectVersion: aStringOrVersion
]

{ #category : 'actions' }
RwResolvedProjectV2 >> read [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return the receiver with a new set of definitions read from disk"

	self repositoryExists
		ifTrue: [ ^ self readProjectComponentNames: self componentNames ]
]

{ #category : 'actions' }
RwResolvedProjectV2 >> read: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return the receiver with a new set of definitions read from disk"

	^ self
		readProjectComponentNames: self componentNames
		customConditionalAttributes: customConditionalAttributes
		platformConditionalAttributes: platformConditionalAttributes
]

{ #category : '-- loader compat --' }
RwResolvedProjectV2 >> readOnlyRepositoryRoot: repositoryRootPathString commitId: commitId [
	projectRepository := self _projectRepository
		readOnlyRepositoryRoot: repositoryRootPathString
		commitId: commitId.

	(self loadedCommitId ifNil: [ true ] ifNotNil: [ :aString | aString isEmpty ])
		ifTrue: [ self _projectSpecification loadedCommitId: commitId ]
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

{ #category : 'project definition' }
RwResolvedProjectV2 >> readPackageNamesBlock: packageNamesBlock [
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
		packageNamesBlock: packageNamesBlock;
		visit: self
]

{ #category : 'actions' }
RwResolvedProjectV2 >> readProjectComponentNames: componentNames [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return the receiver with a new set of definitions read from disk"

	^ self
		readProjectComponentNames: componentNames
		customConditionalAttributes: self customConditionalAttributes
		platformConditionalAttributes: self platformConditionalAttributes
]

{ #category : 'actions' }
RwResolvedProjectV2 >> readProjectComponentNames: componentNames customConditionalAttributes: customConditionalAttributes [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return the receiver with a new set of definitions read from disk"

	self componentNames: componentNames. "record the list of component names used to create this instance of the project definition"
	^ Rowan projectTools readV2
		readProjectForResolvedProject: self
		withComponentNames: componentNames
		customConditionalAttributes: customConditionalAttributes
		platformConditionalAttributes: self platformConditionalAttributes
]

{ #category : 'actions' }
RwResolvedProjectV2 >> readProjectComponentNames: componentNames customConditionalAttributes: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return the receiver with a new set of definitions read from disk"

	self componentNames: componentNames. "record the list of component names used to create this instance of the project definition"
	^ Rowan projectTools readV2
		readProjectForResolvedProject: self
		withComponentNames: componentNames
		customConditionalAttributes: customConditionalAttributes
		platformConditionalAttributes: platformConditionalAttributes
]

{ #category : 'actions' }
RwResolvedProjectV2 >> readProjectSet [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return a project definition set that will contain the project definition along with any dependent project definitions"

	^ self readProjectSetComponentNames: self componentNames
]

{ #category : 'actions' }
RwResolvedProjectV2 >> readProjectSet: conditionalAttributes [
	"refresh the contents of the receiver ... use conditionalAttributes to determine which components will be loaded"

	"return a project definition set that will contain the project definition along with any dependent project definitions"

	^ self
		readProjectSet: conditionalAttributes
		platformConditionalAttributes: self platformConditionalAttributes
]

{ #category : 'actions' }
RwResolvedProjectV2 >> readProjectSet: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return a project definition set that will contain the project definition along with any dependent project definitions"

	^ Rowan projectTools readV2
		readProjectSetForResolvedProject: self
		withComponentNames: self componentNames
		customConditionalAttributes: customConditionalAttributes
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

{ #category : 'components' }
RwResolvedProjectV2 >> removeComponentNamed: aComponentName [
	^ self _projectComponents removeComponentNamed: aComponentName
]

{ #category : 'components' }
RwResolvedProjectV2 >> removePackageGroupNamed: aComponentName [
	^ self _projectComponents removePackageGroupNamed: aComponentName
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> removePackageNamed: packageName [
	(self componentForPackageNamed: packageName) removePackageNamed: packageName.
	^ self _projectDefinition removePackageNamed: packageName
]

{ #category : 'components' }
RwResolvedProjectV2 >> removePackageNamed: packageName fromComponentNamed: componentName [
	"do not remove package from defintion, remove it from the named component only. 
		Use removePackage:, if you want the package completely removed from definition"

	| component |
	component := self componentNamed: componentName.
	component removePackageNamed: packageName.
	^ component
]

{ #category : 'components' }
RwResolvedProjectV2 >> removeProjectNamed: aProjectName [
	^ self _projectComponents removeProjectNamed: aProjectName
]

{ #category : 'components' }
RwResolvedProjectV2 >> renameComponentNamed: aComponentPath to: aComponentName [
	"change the basename of aComponentPath to <baseName>, i.e., the path is not changed"

	^ self _projectComponents
		renameComponentNamed: aComponentPath
		to: aComponentName
]

{ #category : 'component structure' }
RwResolvedProjectV2 >> renameComponentStructureFor: oldBaseName to: newBaseName startingAtComponentNamed: startingComponentName [
	"
		Renames the component structure for the component named <componentNamePath> 
			starting at <startingComponentName>. 
		Each component with the same basename as <componentNamePath> will be renamed 
			with a new baseName. After the structure
		rename is complete, all references to renamed components will be changed. If an 
			existing component is encountered with the same name along the path, the rename
			process will stop at that point.
	"

	| refPath theComponents |
	theComponents := {(self componentOrPackageGroupNamed: startingComponentName)}.
	[ theComponents isEmpty ]
		whileFalse: [ 
			| theList |
			theList := theComponents.
			theComponents := {}.
			theList
				do: [ :theComponent | 
					theComponent componentNames
						do: [ :oldComponentPath | 
							| component |
							refPath := Path from: oldComponentPath.
							refPath basename = oldBaseName
								ifTrue: [ 
									"rename, then add this component (look up anew) to the list of components we need to scan for newBaseName"
									component := self componentOrPackageGroupNamed: oldComponentPath.
									component renameTo: newBaseName in: self.
									theComponents
										add:
											(self componentOrPackageGroupNamed: (refPath parent / newBaseName) pathString) ] ] ] ]
]

{ #category : 'components' }
RwResolvedProjectV2 >> renamePackageGroupNamed: aComponentPath to: aComponentName [
	"change the basename of aComponentPath to <baseName>, i.e., the path is not changed"

	^ self _projectComponents
		renamePackageGroupNamed: aComponentPath
		to: aComponentName
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> renamePackageNamed: packageName to: newPackageName [
	| thePackageDef theComponent |
	theComponent := self componentForPackageNamed: packageName.
	thePackageDef := self removePackageNamed: packageName.
	thePackageDef
		renameTo: newPackageName
		packageConvention: self packageConvention.
	self _projectDefinition _addPackage: thePackageDef.
	self
		movePackageNamed: packageName
		toComponentNamed: theComponent name
		asPackageName: newPackageName.
	^ thePackageDef
]

{ #category : 'project specification' }
RwResolvedProjectV2 >> repoType: aSymbol [
	"#disk, #git or #none"

	self _projectSpecification repoType: aSymbol
]

{ #category : 'accessing' }
RwResolvedProjectV2 >> requiredProjectNames [
	^ self
		requiredProjectNames: self customConditionalAttributes
		platformConditionalAttributes: self platformConditionalAttributes
]

{ #category : 'accessing' }
RwResolvedProjectV2 >> requiredProjectNames: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [
	| requiredProjectNames |
	requiredProjectNames := Set new.
	self _projectComponents
		conditionalComponentsStartingWith: self componentNames
		customConditionalAttributes: customConditionalAttributes
		platformConditionalAttributes: platformConditionalAttributes
		do: [ :aComponent | requiredProjectNames addAll: aComponent projectNames ].
	^ requiredProjectNames
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
RwResolvedProjectV2 >> resolve: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [
	"resolve the projectSpecation (clone remote repo or connect to existing repo on disk) and read 
		project from disk, if project is present on disk"

	self _projectRepository resolve
		ifTrue: [ 
			self _projectRepository checkAndUpdateRepositoryRevision: self.
			self _checkProjectDirectoryStructure
				ifTrue: [ 
					"update project definition from disk"
					self
						read: customConditionalAttributes
						platformConditionalAttributes: platformConditionalAttributes ] ]
]

{ #category : 'actions' }
RwResolvedProjectV2 >> resolveProjectSet [
	"resolve the loadSpecation (clone remote repo or connect to existing repo on disk) and read 
		project set from disk, if project is present on disk (project set will include required projects)"
  | res |
	self _projectRepository resolve
		ifTrue: [ 
			self _projectRepository checkAndUpdateRepositoryRevision: self.
			self _checkProjectDirectoryStructure
				ifTrue: [ 
					"read project and required projects from disk"
					^ self readProjectSet ] ].
	(res := RwProjectSetDefinition new)
		addProject: self .
  ^ res
]

{ #category : 'actions' }
RwResolvedProjectV2 >> resolveProjectSet: conditionalAttributes [
	"resolve the loadsSpecation (clone remote repo or connect to existing repo on disk) and read 
		project set from disk, if project is present on disk (includes required projects)t"
  | res |
	self _projectRepository resolve
		ifTrue: [ 
			self _projectRepository checkAndUpdateRepositoryRevision: self.
			self _checkProjectDirectoryStructure
				ifTrue: [ 
					"update project definition from disk"
					^ self readProjectSet: conditionalAttributes ] ].
	(res := RwProjectSetDefinition new)
		addProject: self .
  ^ res
]

{ #category : 'actions' }
RwResolvedProjectV2 >> resolveProjectSet: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [
	"resolve the loadsSpecation (clone remote repo or connect to existing repo on disk) and read 
		project set from disk, if project is present on disk (includes required projects)t"
  | res |
	self _projectRepository resolve
		ifTrue: [ 
			self _projectRepository checkAndUpdateRepositoryRevision: self.
			self _checkProjectDirectoryStructure
				ifTrue: [ 
					"update project definition from disk"
					^ self readProjectSet: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes ] ].
	(res := RwProjectSetDefinition new)
		addProject: self .
  ^ res
]

{ #category : 'load specification' }
RwResolvedProjectV2 >> revision [
	^ self _loadSpecification revision
]

{ #category : 'load specification' }
RwResolvedProjectV2 >> revision: aRevisionString [
	^ self _loadSpecification revision: aRevisionString
]

{ #category : 'components' }
RwResolvedProjectV2 >> subcomponentsOf: componentName matchBlock: matchBlock ifNone: noneBlock [
	| aComponent subcomponents |
	subcomponents := {}.
	aComponent := self
		componentOrPackageGroupName: componentName
		ifAbsent: [ 
			"noneBlock, if it returns, should answer a component"
			noneBlock cull: componentName ].
	(matchBlock value: aComponent)
		ifFalse: [ 
			"The component is not loadable, so ignore it's subcomponents"
			^ subcomponents ].
	aComponent componentNames
		do: [ :subcomponentName | 
			| subcomponent |
			subcomponent := self
				componentOrPackageGroupName: subcomponentName
				ifAbsent: [ 
					"noneBlock, if it returns, should answer a component"
					noneBlock cull: subcomponentName ].
			(matchBlock value: subcomponent)
				ifTrue: [ subcomponents add: subcomponent ] ].
	^ subcomponents
]

{ #category : 'project definition' }
RwResolvedProjectV2 >> updateLoadedCommitId [
	self _projectSpecification loadedCommitId: self _projectRepository commitId
]

{ #category : 'testing' }
RwResolvedProjectV2 >> useGit [
	^ self _projectRepository useGit
]
