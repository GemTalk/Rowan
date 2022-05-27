Class {
	#name : 'RwDefinedProject',
	#superclass : 'RwAbstractUnloadedProject',
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwDefinedProject class >> fromResolvedProject: aResolvedProject [
	^ (self newNamed: aResolvedProject name)
		_concreteProject: aResolvedProject _concreteProject;
		yourself
]

{ #category : 'instance creation' }
RwDefinedProject class >> newNamed: aName [

	^ self new
		initializeForName: aName;
		_concreteProject;
		yourself
]

{ #category : 'private' }
RwDefinedProject >> _concreteProject [
	^ concreteProject
		ifNil: [ 
			concreteProject := RwResolvedProjectV2 new
				projectName: self name;
				yourself ]
]

{ #category : 'private' }
RwDefinedProject >> _gemstoneAllUsersName [
	^ self _concreteProject _gemstoneAllUsersName
]

{ #category : 'accessing' }
RwDefinedProject >> _validate: conditionalAttributes [
	^ self _concreteProject _validate: conditionalAttributes
]

{ #category : 'components' }
RwDefinedProject >> addComponentNamed: componentName toComponentNamed: toComponentName [
	"add existing component named componentName to component named toComponentName"

	^ self _concreteProject
		addComponentNamed: componentName
		toComponentNamed: toComponentName
]

{ #category : 'accessing' }
RwDefinedProject >> addComponentNames: anArray [
	"add to the existing component names"

	^ self _concreteProject addComponentNames: anArray
]

{ #category : 'components' }
RwDefinedProject >> addComponentOrPackageGroup: aComponentOrPackageGroup toComponentNamed: toComponentName [
	"add existing component to component named toComponentName"

	self _concreteProject
		addComponentOrPackageGroup: aComponentOrPackageGroup
		toComponentNamed: toComponentName
]

{ #category : 'component structure' }
RwDefinedProject >> addComponentStructureFor: componentBasename startingAtComponentNamed: toComponentName pathNameArray: pathNameArray conditionPathArray: conditionPathArray [
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

	^ self
		addComponentStructureFor: componentBasename
		startingAtComponentNamed: toComponentName
		pathNameArray: pathNameArray
		conditionPathArray: conditionPathArray
		comment: ''
]

{ #category : 'component structure' }
RwDefinedProject >> addComponentStructureFor: componentBasename startingAtComponentNamed: toComponentName pathNameArray: pathNameArray conditionPathArray: conditionPathArray comment: aString [
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

	^ self _concreteProject
		addComponentStructureFor: componentBasename
		startingAtComponentNamed: toComponentName
		pathNameArray: pathNameArray
		conditionPathArray: conditionPathArray
		comment: aString
]

{ #category : 'accessing' }
RwDefinedProject >> addCustomConditionalAttributes: anArray [
	"add to the existing custom conditional attributes"

	^ self _concreteProject addCustomConditionalAttributes: anArray
]

{ #category : 'components' }
RwDefinedProject >> addLoadComponentNamed: componentName [
	"add a new instance of RwLoadComponent to the project components and add the componentName
		to the load spec (i.e., it will be loaded when the load spec is loaded)"

	^ self addLoadComponentNamed: componentName comment: ''
]

{ #category : 'components' }
RwDefinedProject >> addLoadComponentNamed: componentName comment: aString [
	"add a new instance of RwLoadComponent to the project components and add the componentName
		to the load spec (i.e., it will be loaded when the load spec is loaded)"

	^ self _concreteProject addLoadComponentNamed: componentName comment: aString
]

{ #category : 'components' }
RwDefinedProject >> addPackageGroupNamed: aComponentName condition: condition comment: aString [
	^ self _concreteProject
		addPackageGroupNamed: aComponentName
		condition: condition
		comment: aString
]

{ #category : 'component structure' }
RwDefinedProject >> addPackageGroupStructureFor: componentBasename startingAtComponentNamed: toComponentName pathNameArray: pathNameArray conditionPathArray: conditionPathArray comment: aString [
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

	^ self _concreteProject
		addPackageGroupStructureFor: componentBasename
		startingAtComponentNamed: toComponentName
		pathNameArray: pathNameArray
		conditionPathArray: conditionPathArray
		comment: aString
]

{ #category : 'accessing' }
RwDefinedProject >> addPackageNamed: packageName [
	"the package is expected to already be present in a component - used when reading packages from disk"

	^ self _concreteProject addPackageNamed: packageName
]

{ #category : 'accessing' }
RwDefinedProject >> addPackageNamed: packageName toComponentNamed: componentName [
	^ self _concreteProject
		addPackageNamed: packageName
		toComponentNamed: componentName
]

{ #category : 'accessing' }
RwDefinedProject >> addPackageNamed: packageName toComponentNamed: componentName gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc [
	^ self _concreteProject
		addPackageNamed: packageName
		toComponentNamed: componentName
		gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc
]

{ #category : 'accessing' }
RwDefinedProject >> addPackagesNamed: packageNames toComponentNamed: componentName [
	^ self _concreteProject
		addPackagesNamed: packageNames
		toComponentNamed: componentName
]

{ #category : 'components' }
RwDefinedProject >> addPlatformSubcomponentNamed: componentName condition: condition comment: aString [
	^ self _concreteProject
		addPlatformSubcomponentNamed: componentName
		condition: condition
		comment: aString
]

{ #category : 'components' }
RwDefinedProject >> addPlatformSubcomponentNamed: componentName condition: condition comment: aString toComponentNamed: toComponentName [
	"Add the named subcomponent with the given condition to the named project and add the new component to the toComponentName component"

	^ self _concreteProject
		addPlatformSubcomponentNamed: componentName
		condition: condition
		comment: aString
		toComponentNamed: toComponentName
]

{ #category : 'components' }
RwDefinedProject >> addPlatformSubcomponentNamed: componentName condition: condition toComponentNamed: toComponentName [
	"Add the named subcomponent with the given condition to the named project and add the new component to the toComponentName component"

	^ self
		addPlatformSubcomponentNamed: componentName
		condition: condition
		comment: ''
		toComponentNamed: toComponentName
]

{ #category : 'accessing' }
RwDefinedProject >> addPostloadDoitName: doitName withSource: doitSource toComponentNamed: aComponentName [
	^ self _concreteProject
		addPostloadDoitName: doitName
		withSource: doitSource
		toComponentNamed: aComponentName
]

{ #category : 'accessing' }
RwDefinedProject >> addPreloadDoitName: doitName withSource: doitSource toComponentNamed: aComponentName [
	^ self _concreteProject
		addPreloadDoitName: doitName
		withSource: doitSource
		toComponentNamed: aComponentName
]

{ #category : 'accessing' }
RwDefinedProject >> addProjectNamed: projectName toComponentNamed: toComponentName [
	^ self _concreteProject
		addProjectNamed: projectName
		toComponentNamed: toComponentName
]

{ #category : 'accessing' }
RwDefinedProject >> addRawPackageNamed: packageName [
	^ self _concreteProject addRawPackageNamed: packageName
]

{ #category : 'components' }
RwDefinedProject >> addSubcomponentNamed: componentName condition: condition [
	^ self _concreteProject
		addSubcomponentNamed: componentName
		condition: condition
]

{ #category : 'components' }
RwDefinedProject >> addSubcomponentNamed: componentName condition: condition comment: aString [
	^ self _concreteProject
		addSubcomponentNamed: componentName
		condition: condition
		comment: aString
]

{ #category : 'components' }
RwDefinedProject >> addSubcomponentNamed: componentName condition: condition comment: aString toComponentNamed: toComponentName [
	"Add the named subcomponent with the given condition to the named project and add the new component to the toComponentName component"

	^ self _concreteProject
		addSubcomponentNamed: componentName
		condition: condition
		comment: aString
		toComponentNamed: toComponentName
]

{ #category : 'components' }
RwDefinedProject >> addSubcomponentNamed: componentName condition: condition toComponentNamed: toComponentName [
	"Add the named subcomponent with the given condition to the named project and add the new component to the toComponentName component"

	^ self addSubcomponentNamed: componentName condition: condition comment: '' toComponentNamed: toComponentName
]

{ #category : 'component structure' }
RwDefinedProject >> addSubcomponentStructureFor: componentBasename startingAtComponentNamed: toComponentName conditionPathArray: conditionPathArray [
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
RwDefinedProject >> addSubcomponentStructureFor: componentBasename startingAtComponentNamed: toComponentName conditionPathArray: conditionPathArray comment: aString [
	"
	PREFERRED method for adding subcomponent structure following the convention that the directory names reflect the condition names.

	When adding platform component structure, where the condition is an Array instead of a String, use 
		addComponentStructureFor:startingAtComponentNamed:pathNameArray:conditionPathArray:comment:.

	Return the last component created.
	"

	^ self _concreteProject
		addSubcomponentStructureFor: componentBasename
		startingAtComponentNamed: toComponentName
		conditionPathArray: conditionPathArray
		comment: aString
]

{ #category : 'accessing' }
RwDefinedProject >> componentNamed: componentName [
	^ self _concreteProject componentNamed: componentName
]

{ #category : 'accessing' }
RwDefinedProject >> componentNamed: aComponentName ifAbsent: absentBlock [
	^ self _concreteProject componentNamed: aComponentName ifAbsent: absentBlock
]

{ #category : 'accessing' }
RwDefinedProject >> componentNames [
	"list of component names from the load specification used to load the project "

	^ self _concreteProject componentNames
]

{ #category : 'accessing' }
RwDefinedProject >> componentNames: anArray [
	^ self _concreteProject componentNames: anArray
]

{ #category : 'accessing' }
RwDefinedProject >> componentOrPackageGroupNamed: componentName [
	^ self _concreteProject componentOrPackageGroupNamed: componentName
]

{ #category : 'accessing' }
RwDefinedProject >> componentOrPackageGroupNamed: aComponentName ifAbsent: absentBlock [
	^ self _concreteProject componentOrPackageGroupNamed: aComponentName ifAbsent: absentBlock
]

{ #category : 'accessing' }
RwDefinedProject >> componentsPath: aString [
	self _projectSpecification componentsPath: aString
]

{ #category : 'accessing' }
RwDefinedProject >> customConditionalAttributes: anArray [
	self loadSpecification customConditionalAttributes: anArray
]

{ #category : 'transitions' }
RwDefinedProject >> defined [
	^ self
]

{ #category : 'accessing' }
RwDefinedProject >> gemstoneSetDefaultSymbolDictNameForUser: userId to: symbolDictName [
	self _concreteProject
		gemstoneSetDefaultSymbolDictNameForUser: userId
		to: symbolDictName
]

{ #category : 'accessing' }
RwDefinedProject >> gemstoneSetSymbolDictName: symbolDictName forPackageNamed: packageName [
	self _concreteProject
		gemstoneSetSymbolDictName: symbolDictName
		forPackageNamed: packageName
]

{ #category : 'accessing' }
RwDefinedProject >> gemstoneSetSymbolDictNameForUser: userId to: symbolDictName forPackageNamed: packageName [
	self _concreteProject
		gemstoneSetSymbolDictNameForUser: userId
		to: symbolDictName
		forPackageNamed: packageName
]

{ #category : 'accessing' }
RwDefinedProject >> gemstoneSetUseSessionMethodsForExtensions: userId to: aBool [
	self _concreteProject
		gemstoneSetUseSessionMethodsForExtensions: userId
		to: aBool
]

{ #category : 'accessing' }
RwDefinedProject >> gemstoneSetUseSessionMethodsForExtensionsForUser: userId to: aBool forPackageNamed: packageName [
	self _concreteProject
		gemstoneSetUseSessionMethodsForExtensionsForUser: userId
		to: aBool
		forPackageNamed: packageName
]

{ #category : 'accessing' }
RwDefinedProject >> gemstoneSymbolDictNameForPackageNamed: packageName forUser: userId [
	^ self _concreteProject
		gemstoneSymbolDictNameForPackageNamed: packageName
		forUser: userId
]

{ #category : 'accessing' }
RwDefinedProject >> gemstoneUseSessionMethodsForExtensionsForPackageNamed: packageName forUser: userId [
	^ self _concreteProject
		gemstoneUseSessionMethodsForExtensionsForPackageNamed: packageName
		forUser: userId
]

{ #category : 'testing' }
RwDefinedProject >> isStrict [
	^ self _concreteProject isStrict
]

{ #category : 'accessing' }
RwDefinedProject >> loadSpecification [

	^ self _concreteProject loadSpecification
]

{ #category : 'accessing' }
RwDefinedProject >> moveClassExtensionNamed: aClassName fromPackageNamed: fromPackageName toPackageNamed: toPackageName [
	^ self _concreteProject
		moveClassExtensionNamed: aClassName
		fromPackageNamed: fromPackageName
		toPackageNamed: toPackageName
]

{ #category : 'accessing' }
RwDefinedProject >> moveClassNamed: aClassName toPackageNamed: aPackageName [
	^ self _concreteProject moveClassNamed: aClassName toPackageNamed: aPackageName
]

{ #category : 'accessing' }
RwDefinedProject >> movePackageNamed: aPackageName toComponentNamed: aComponentName [
	^ self _concreteProject
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

{ #category : 'components' }
RwDefinedProject >> packageGroupNamed: componentName [
	^ self _concreteProject packageGroupNamed: componentName
]

{ #category : 'components' }
RwDefinedProject >> packageGroupNames [
	^ self _projectComponents packageGroupNames
]

{ #category : 'accessing' }
RwDefinedProject >> packageNamed: aString [
	^ self _concreteProject packageNamed: aString
]

{ #category : 'accessing' }
RwDefinedProject >> packageNamed: aString ifAbsent: absentBlock [
	^ self _concreteProject packageNamed: aString ifAbsent: absentBlock
]

{ #category : 'accessing' }
RwDefinedProject >> packagesPath: aString [
	self _projectSpecification packagesPath: aString
]

{ #category : 'accessing' }
RwDefinedProject >> projectAlias: aString [
	^ self _concreteProject projectAlias: aString
]

{ #category : 'accessing' }
RwDefinedProject >> projectsPath: aString [
	self _projectSpecification projectsPath: aString
]

{ #category : 'accessing' }
RwDefinedProject >> projectSpecFile: relativePathString [
	^ self _concreteProject projectSpecFile: relativePathString
]

{ #category : 'accessing' }
RwDefinedProject >> projectSpecPath: aStringOrNil [
	"nil value indicates projectSpec file is in repository root directory"

	self _projectSpecification projectSpecPath: aStringOrNil
]

{ #category : 'accessing' }
RwDefinedProject >> projectsRoot [
	^ self _concreteProject projectsRoot
]

{ #category : 'accessing' }
RwDefinedProject >> removeComponentNamed: aComponentName [
	^ self _concreteProject removeComponentNamed: aComponentName
]

{ #category : 'accessing' }
RwDefinedProject >> removeComponentNames: anArray [
	"remove from the existing component names"

	^ self _concreteProject removeComponentNames: anArray
]

{ #category : 'accessing' }
RwDefinedProject >> removeCustomConditionalAttributes: anArray [
	"remove from the existing custom conditional attributes"

	^ self _concreteProject removeCustomConditionalAttributes: anArray
]

{ #category : 'accessing' }
RwDefinedProject >> removePackageGroupNamed: aComponentName [
	^ self _concreteProject removePackageGroupNamed: aComponentName
]

{ #category : 'accessing' }
RwDefinedProject >> removePackageNamed: packageName [
	^ self _concreteProject removePackageNamed: packageName
]

{ #category : 'accessing' }
RwDefinedProject >> removePackageNamed: packageName fromComponentNamed: componentName [
	^ self _concreteProject
		removePackageNamed: packageName
		fromComponentNamed: componentName
]

{ #category : 'accessing' }
RwDefinedProject >> renameComponentNamed: aComponentPath to: aComponentName [
	"change the basename of aComponentPath to <baseName>, i.e., the path is not changed"

	^ self _concreteProject renameComponentNamed: aComponentPath to: aComponentName
]

{ #category : 'component structure' }
RwDefinedProject >> renameComponentStructureFor: componentNamePath to: baseName startingAtComponentNamed: startingComponentName [
	"
		Renames the component structure for the component named <componentNamePath> starting at <startingComponentName>. 
		Each component with the same basename as <componentNamePath> will be renamed with a new baseName. After the structure
		rename is complete, all references to renamed components will be changed. If an existing component is encountered with the same
		name along the path, the rename process will stop at that point.
	"

	^ self _concreteProject
		renameComponentStructureFor: componentNamePath
		to: baseName
		startingAtComponentNamed: startingComponentName
]

{ #category : 'accessing' }
RwDefinedProject >> renamePackageGroupNamed: aComponentPath to: aComponentName [
	^ self _concreteProject renamePackageGroupNamed: aComponentPath to: aComponentName
]

{ #category : 'accessing' }
RwDefinedProject >> renamePackageNamed: packageName to: newPackageName [
	^ self _concreteProject renamePackageNamed: packageName to: newPackageName
]

{ #category : 'accessing' }
RwDefinedProject >> repoType: aSymbol [
	"#disk, #git or #none"

	self _concreteProject repoType: aSymbol
]

{ #category : 'transitions' }
RwDefinedProject >> resolveProject [
	^ RwResolvedFromDefinedProject fromDefinedProject: self
]

{ #category : 'transitions' }
RwDefinedProject >> resolveStrict [
	^ RwResolvedFromDefinedProject fromStrictDefinedProject: self
]

{ #category : 'accessing' }
RwDefinedProject >> revision: aString [
	self loadSpecification revision: aString
]

{ #category : 'accessing' }
RwDefinedProject >> specName: aString [
	self loadSpecification specName: aString
]

{ #category : 'accessing' }
RwDefinedProject >> specsPath: aString [
	self _projectSpecification specsPath: aString
]
