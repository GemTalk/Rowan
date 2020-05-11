Class {
	#name : 'RwDefinedProject',
	#superclass : 'RwAbstractUnloadedProject',
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwDefinedProject class >> fromResolvedProject: aResolvedProject [
	^ (self newNamed: aResolvedProject name)
		_resolvedProject: aResolvedProject _resolvedProject;
		yourself
]

{ #category : 'instance creation' }
RwDefinedProject class >> newEmbeddedProjectNamed: aName [
	| new spec |
	new := self new initializeForName: aName.
	spec := RwEmbeddedLoadSpecificationV2 new
		projectName: aName;
		yourself.
	new _resolvedProject _loadSpecification: spec.
	^ new
]

{ #category : 'instance creation' }
RwDefinedProject class >> newNamed: aName [

	^ self new
		initializeForName: aName;
		_resolvedProject;
		yourself
]

{ #category : 'private' }
RwDefinedProject >> _gemstoneAllUsersName [
	^ self _resolvedProject _gemstoneAllUsersName
]

{ #category : 'private' }
RwDefinedProject >> _loadSpecification [
	^ self _resolvedProject _loadSpecification
]

{ #category : 'private' }
RwDefinedProject >> _projectRepository [
	^ self _resolvedProject _projectRepository
]

{ #category : 'private' }
RwDefinedProject >> _projectSpecification [
	^ self _resolvedProject _projectSpecification
]

{ #category : 'private' }
RwDefinedProject >> _resolvedProject [
	^ resolvedProject
		ifNil: [ 
			resolvedProject := RwResolvedProjectV2 new
				projectName: self name;
				yourself ]
]

{ #category : 'accessing' }
RwDefinedProject >> _validate: platformConfigurationAttributes [
	^ self _resolvedProject _validate: platformConfigurationAttributes
]

{ #category : 'accessing' }
RwDefinedProject >> addComponentNamed: componentName toComponentNamed: toComponentName [
	^ self _resolvedProject
		addComponentNamed: componentName
		toComponentNamed: toComponentName
]

{ #category : 'accessing' }
RwDefinedProject >> addNewComponentNamed: componentName [
	^ self _resolvedProject addNewComponentNamed: componentName
]

{ #category : 'accessing' }
RwDefinedProject >> addNewComponentNamed: aComponentName comment: aString [
	^ self _resolvedProject addNewComponentNamed: aComponentName comment: aString
]

{ #category : 'accessing' }
RwDefinedProject >> addNewComponentNamed: aComponentName condition: condition comment: aString [
	^ self _resolvedProject
		addNewComponentNamed: aComponentName
		condition: condition
		comment: aString
]

{ #category : 'accessing' }
RwDefinedProject >> addNewComponentNamed: aComponentName toComponentNamed: toComponentName condition: conditionPathArray [
	^ self _resolvedProject
		addNewComponentNamed: aComponentName
		toComponentNamed: toComponentName
		condition: conditionPathArray
]

{ #category : 'accessing' }
RwDefinedProject >> addNewComponentNamed: aComponentName toComponentNamed: toComponentName condition: conditionPathArray comment: aString [
	^ self _resolvedProject
		addNewComponentNamed: aComponentName
		toComponentNamed: toComponentName
		condition: conditionPathArray
		comment: aString
]

{ #category : 'accessing' }
RwDefinedProject >> addNewNestedComponentNamed: aComponentName comment: aString [
	^ self _resolvedProject
		addNewNestedComponentNamed: aComponentName
		comment: aString
]

{ #category : 'accessing' }
RwDefinedProject >> addPackageNamed: packageName [
	"the package is expected to already be present in a component - used when reading packages from disk"

	^ self _resolvedProject addPackageNamed: packageName
]

{ #category : 'accessing' }
RwDefinedProject >> addPackageNamed: packageName toComponentNamed: componentName [
	^ self _resolvedProject
		addPackageNamed: packageName
		toComponentNamed: componentName
]

{ #category : 'accessing' }
RwDefinedProject >> addPackageNamed: packageName toComponentNamed: componentName gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc [
	^ self _resolvedProject
		addPackageNamed: packageName
		toComponentNamed: componentName
		gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc
]

{ #category : 'accessing' }
RwDefinedProject >> addPackagesNamed: packageName toComponentNamed: componentName [
	^ self _resolvedProject
		addPackagesNamed: packageName
		toComponentNamed: componentName
]

{ #category : 'accessing' }
RwDefinedProject >> addPlatformComponentNamed: aComponentName toComponentNamed: toComponentName pathNameArray: pathNameArray conditionPathArray: conditionPathArray [
	^ self _resolvedProject
		addPlatformComponentNamed: aComponentName
		toComponentNamed: toComponentName
		pathNameArray: pathNameArray
		conditionPathArray: conditionPathArray
]

{ #category : 'accessing' }
RwDefinedProject >> addPlatformComponentNamed: aComponentName toComponentNamed: toComponentName pathNameArray: pathNameArray conditionPathArray: conditionPathArray comment: aString [
	^ self _resolvedProject
		addPlatformComponentNamed: aComponentName
		toComponentNamed: toComponentName
		pathNameArray: pathNameArray
		conditionPathArray: conditionPathArray
		comment: aString
]

{ #category : 'accessing' }
RwDefinedProject >> addPostloadDoitName: doitName withSource: doitSource toComponentNamed: aComponentName [
	^ self _resolvedProject
		addPostloadDoitName: doitName
		withSource: doitSource
		toComponentNamed: aComponentName
]

{ #category : 'accessing' }
RwDefinedProject >> addPreloadDoitName: doitName withSource: doitSource toComponentNamed: aComponentName [
	^ self _resolvedProject
		addPreloadDoitName: doitName
		withSource: doitSource
		toComponentNamed: aComponentName
]

{ #category : 'accessing' }
RwDefinedProject >> addProjectNamed: projectName toComponentNamed: toComponentName [
	^ self _resolvedProject
		addProjectNamed: projectName
		toComponentNamed: toComponentName
]

{ #category : 'accessing' }
RwDefinedProject >> addRawPackageNamed: packageName [
	^ self _resolvedProject addRawPackageNamed: packageName
]

{ #category : 'accessing' }
RwDefinedProject >> addSimpleNestedComponentNamed: aComponentName condition: condition comment: commentString [
	^ self _resolvedProject
		addSimpleNestedComponentNamed: aComponentName
		condition: condition
		comment: commentString
]

{ #category : 'accessing' }
RwDefinedProject >> addTopLevelComponentNamed: componentName [
	^ self _resolvedProject addTopLevelComponentNamed: componentName
]

{ #category : 'accessing' }
RwDefinedProject >> componentNamed: componentName [
	^ self _resolvedProject componentNamed: componentName
]

{ #category : 'accessing' }
RwDefinedProject >> customConditionalAttributes [
	^ self _loadSpecification customConditionalAttributes
]

{ #category : 'accessing' }
RwDefinedProject >> customConditionalAttributes: anArray [
	self _loadSpecification customConditionalAttributes: anArray
]

{ #category : 'transitions' }
RwDefinedProject >> defined [
	^ self
]

{ #category : 'accessing' }
RwDefinedProject >> gemstoneSetDefaultSymbolDictNameForUser: userId to: symbolDictName [
	self _resolvedProject
		gemstoneSetDefaultSymbolDictNameForUser: userId
		to: symbolDictName
]

{ #category : 'accessing' }
RwDefinedProject >> gemstoneSetSymbolDictName: symbolDictName forPackageNamed: packageName [
	self _resolvedProject
		gemstoneSetSymbolDictName: symbolDictName
		forPackageNamed: packageName
]

{ #category : 'testing' }
RwDefinedProject >> isStrict [
	^ self _resolvedProject isStrict
]

{ #category : 'accessing' }
RwDefinedProject >> loadSpecification [

	^ self _resolvedProject loadSpecification
]

{ #category : 'accessing' }
RwDefinedProject >> moveClassNamed: aClassName toPackageNamed: aPackageName [
	^ self _resolvedProject moveClassNamed: aClassName toPackageNamed: aPackageName
]

{ #category : 'accessing' }
RwDefinedProject >> movePackageNamed: aPackageName toComponentNamed: aComponentName [
	^ self _resolvedProject
		movePackageNamed: aPackageName
		toComponentNamed: aComponentName
]

{ #category : 'accessing' }
RwDefinedProject >> packageConvention: aString [
	"
		RowanHybrid	- [default] Class category is package name, method protocol with leading $* is case insensitive package name
		Monticello		- Class category is package name, method protocol with leading $* begins with case insensitive package name
		Rowan			- Class category and method protocol are not overloaded with packaging information
	"

	self _projectSpecification packageConvention: aString
]

{ #category : 'accessing' }
RwDefinedProject >> packageNamed: aString [
	^ self _resolvedProject packageNamed: aString
]

{ #category : 'accessing' }
RwDefinedProject >> packagesPath: aString [
	self _projectSpecification packagesPath: aString
]

{ #category : 'accessing' }
RwDefinedProject >> projectAlias: aString [
	^ self _resolvedProject projectAlias: aString
]

{ #category : 'accessing' }
RwDefinedProject >> projectSpecFile: relativePathString [
	^ self _resolvedProject projectSpecFile: relativePathString
]

{ #category : 'accessing' }
RwDefinedProject >> projectsRoot [
	^ self _resolvedProject projectsRoot
]

{ #category : 'accessing' }
RwDefinedProject >> removeComponentNamed: aComponentName [
	^ self _resolvedProject removeComponentNamed: aComponentName
]

{ #category : 'accessing' }
RwDefinedProject >> removePackageNamed: packageName fromComponentNamed: componentName [
	^ self _resolvedProject
		removePackageNamed: packageName
		fromComponentNamed: componentName
]

{ #category : 'accessing' }
RwDefinedProject >> renameComponentNamed: aComponentPath to: aComponentName [
	^ self _resolvedProject renameComponentNamed: aComponentPath to: aComponentName
]

{ #category : 'accessing' }
RwDefinedProject >> renamePackageNamed: packageName to: newPackageName [
	^ self _resolvedProject renamePackageNamed: packageName to: newPackageName
]

{ #category : 'accessing' }
RwDefinedProject >> repositoryRoot [
	^ self _resolvedProject repositoryRoot
]

{ #category : 'accessing' }
RwDefinedProject >> repoType: aSymbol [
	"#disk, #git or #none"

	self _resolvedProject repoType: aSymbol
]

{ #category : 'transitions' }
RwDefinedProject >> resolve [
	^ RwResolvedProject fromDefinedProject: self
]

{ #category : 'transitions' }
RwDefinedProject >> resolveStrict [
	^ RwResolvedProject fromStrictDefinedProject: self
]

{ #category : 'accessing' }
RwDefinedProject >> resolveWithParentProject: aResolvedRwProject [
	"give embedded projects a chance to resolve cleanly"

	^ (RwResolvedProject newNamed: aResolvedRwProject name)
		_resolvedProject:
				(self _loadSpecification
						resolveWithParentProject: aResolvedRwProject _resolvedProject) resolve;
		yourself
]

{ #category : 'accessing' }
RwDefinedProject >> revision: aString [
	self _loadSpecification revision: aString
]

{ #category : 'accessing' }
RwDefinedProject >> specName: aString [
	self _loadSpecification specName: aString
]
