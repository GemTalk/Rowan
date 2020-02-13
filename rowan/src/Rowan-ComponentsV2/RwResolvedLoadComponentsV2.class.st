Class {
	#name : 'RwResolvedLoadComponentsV2',
	#superclass : 'Object',
	#instVars : [
		'components'
	],
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'instance creation' }
RwResolvedLoadComponentsV2 class >> new [

	^self basicNew initialize

]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> _addComponent: aComponent [
	"not sure I like how this is used ... the component structure needs to be kept in sync with packages, so this is not quite the route to go, unless we ensure that the component has an entry for the package"

	"see similar comment in addRawPackageNamed: and addPackages:forComponent: "

	"should be sent from the component visitor ... not unexpected to have a duplicate, but the new
		component --- presumably freshly read from disk --- wins"

	^ self components at: aComponent name put: aComponent
]

{ #category : 'private' }
RwResolvedLoadComponentsV2 >> _gemstoneAllUsersName [
	^ RwLoadSpecificationV2 _gemstoneAllUsersName
]

{ #category : 'private' }
RwResolvedLoadComponentsV2 >> _validate [
	"ensure that each of the components is valid and return a list of the package names managed by all components"

	| componentPackageNames |
	componentPackageNames := Set new.
	self components
		keysAndValuesDo: [ :componentName :component | 
			component
				validate;
				_validateDoits.
			componentPackageNames addAll: component packageNames ].
	^ componentPackageNames
]

{ #category : 'private' }
RwResolvedLoadComponentsV2 >> _validate: platformConfigurationAttributes groupNames: groupNames [
	"ensure that each of the components is valid and return a list of the package names managed by all components for the given group name"

	| componentPackageNames |
	componentPackageNames := Set new.
	self components
		keysAndValuesDo: [ :componentName :component | 
			component
				validate;
				_validateDoits.
			componentPackageNames
				addAll:
					(component
						packageNamesForPlatformConfigurationAttributes:
							platformConfigurationAttributes
						groupNames: groupNames) ].
	^ componentPackageNames
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> addApplicationNamed: aComponentName condition: condition groupName: groupName comment: commentString [
	| component |
	self components
		at: aComponentName
		ifPresent: [ 
			self
				error: 'The component ' , aComponentName printString , ' is already present' ].
	component := self components
		at: aComponentName
		ifAbsentPut: [ RwEnvyApplicationProjectLoadComponentV2 newNamed: aComponentName ].
	component
		condition: condition;
		groupName: groupName;
		comment: commentString;
		yourself.
	^ component
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> addApplicationNamed: componentName toComponentNamed: toComponentName [
	| component |
	component := self
		componentNamed: toComponentName
		ifAbsent: [ self error: 'The component ' , toComponentName printString , ' is undefined' ].
	component addApplicationNamed: componentName
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> addComponentNamed: aComponentName definedGroupNames: groupNameDict comment: commentString [
	| component |
	self components
		at: aComponentName
		ifPresent: [ 
			self
				error: 'The component ' , aComponentName printString , ' is already present' ].
	component := self components
		at: aComponentName
		ifAbsentPut: [ RwProjectLoadComponentV2 newNamed: aComponentName ].
	groupNameDict
		keysAndValuesDo: [ :groupName :includeGroups | component defineGroupNamed: groupName toIncludeGroups: includeGroups ].
	component comment: commentString.
	^ component
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> addComponentNamed: componentName toComponentNamed: toComponentName [
	| component |
	component := self
		componentNamed: toComponentName
		ifAbsent: [ self error: 'The component ' , toComponentName printString , ' is undefined' ].
	component addComponentNamed: componentName
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> addComponentNamed: componentName toComponentNamed: toComponentName withConditions: conditionArray andGroupName: groupName [
	| component |
	component :=  self
		componentNamed: toComponentName
		ifAbsent: [ self error: 'The component ' , toComponentName printString , ' is undefined' ].
	component
		conditionalPropertiesAtConditions: conditionArray
		andGroup: groupName
		addComponentNames: {componentName}
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> addNestedComponentNamed: aComponentName definedGroupNames: groupNameDict comment: commentString [
	| component |
	self components
		at: aComponentName
		ifPresent: [ 
			self
				error: 'The component ' , aComponentName printString , ' is already present' ].
	component := self components
		at: aComponentName
		ifAbsentPut: [ RwNestedProjectLoadComponentV2 newNamed: aComponentName ].
	groupNameDict
		keysAndValuesDo: [ :groupName :includeGroups | component defineGroupNamed: groupName toIncludeGroups: includeGroups ].
	component comment: commentString.
	^ component
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> addPackageNamed: packageName toComponentNamed: componentName [ 
	| component |
	component := self
		componentNamed: componentName
		ifAbsent: [ self error: 'The component ' , componentName printString , ' is undefined' ].
	component
		addPackageNames: {packageName}.
	^ component
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> addPackageNamed: packageName toComponentNamed: componentName withConditions: conditionArray andGroupName: groupName [
	| component |
	component := self
		componentNamed: componentName
		ifAbsent: [ self error: 'The component ' , componentName printString , ' is undefined' ].
	component
		conditionalPropertiesAtConditions: conditionArray
		andGroup: groupName
		addPackageNames: {packageName}.
	^ component
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> addPackageNamed: packageName toComponentNamed: componentName withConditions: conditionArray andGroupName: groupName gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc [
	| component |
	component := self
		componentNamed: componentName
		ifAbsent: [ self error: 'The component ' , componentName printString , ' is undefined' ].
	component
		conditionalPackageMapSpecsAtGemStoneUserId: aSymbolDictAssoc key
			andPackageName: packageName
			setSymbolDictNameTo: aSymbolDictAssoc value;
		conditionalPropertiesAtConditions: conditionArray
			andGroup: groupName
			addPackageNames: {packageName}.
	^ component
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> addPackagesNamed: packageNames toComponentNamed: aComponentName [
	packageNames
		do: [ :packageName | self addPackageNamed: packageName toComponentNamed: aComponentName ]
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> addPackagesNamed: packageNames toComponentNamed: componentName withConditions: conditionArray andGroupName: groupName [
	^ packageNames
		collect: [ :packageName | 
			self
				addPackageNamed: packageName
				toComponentNamed: componentName
				withConditions: conditionArray
				andGroupName: groupName ]
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> addPlatformNestedComponentNamed: aComponentName condition: conditionArray comment: commentString [
	| component |
	self components
		at: aComponentName
		ifPresent: [ 
			self
				error: 'The component ' , aComponentName printString , ' is already present' ].
	component := self components
		at: aComponentName
		ifAbsentPut: [ RwPlatformNestedProjectLoadComponentV2 newNamed: aComponentName ].
	component
		condition: conditionArray;
		comment: commentString;
		yourself.
	^ component
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> addProjectNamed: projectName toComponentNamed: toComponentName [
	| component |
	component := self
		componentNamed: toComponentName
		ifAbsent: [ self error: 'The component ' , toComponentName printString , ' is undefined' ].
	component addProjectNamed: projectName
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> addProjectNamed: projectName toComponentNamed: toComponentName withConditions: conditionArray andGroupName: groupName [
	| component |
	component := self
		componentNamed: toComponentName
		ifAbsent: [ self error: 'The component ' , toComponentName printString , ' is undefined' ].
	component
		conditionalPropertiesAtConditions: conditionArray
		andGroup: groupName
		addProjectNames: {projectName}
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> addSimpleComponentNamed: aComponentName condition: condition comment: commentString [
	| component |
	self components
		at: aComponentName
		ifPresent: [ 
			self
				error: 'The component ' , aComponentName printString , ' is already present' ].
	component := self components
		at: aComponentName
		ifAbsentPut: [ RwSimpleProjectLoadComponentV2 newNamed: aComponentName ].
	component
		condition: condition;
		comment: commentString;
		yourself.
	^ component
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> addSimpleComponentNamed: aComponentName condition: condition groupName: groupName comment: commentString [
	| component |
	self components
		at: aComponentName
		ifPresent: [ 
			self
				error: 'The component ' , aComponentName printString , ' is already present' ].
	component := self components
		at: aComponentName
		ifAbsentPut: [ RwSimpleProjectLoadComponentV2 newNamed: aComponentName ].
	component
		condition: condition;
		groupName: groupName;
		comment: commentString;
		yourself.
	^ component
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> addSimpleNestedComponentNamed: aComponentName condition: condition comment: commentString [
	| component |
	self components
		at: aComponentName
		ifPresent: [:ignored |
			self
				error: 'The component ' , aComponentName printString , ' is already present' ].
	component := self components
		at: aComponentName
		ifAbsentPut: [ RwSimpleNestedProjectLoadComponentV2 newNamed: aComponentName ].
	component
		condition: condition;
		comment: commentString;
		yourself.
	^ component
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> addSimpleNestedComponentNamed: aComponentName condition: condition groupName: groupName comment: commentString [
	| component |
	self components
		at: aComponentName
		ifPresent: [ 
			self
				error: 'The component ' , aComponentName printString , ' is already present' ].
	component := self components
		at: aComponentName
		ifAbsentPut: [ RwSimpleNestedProjectLoadComponentV2 newNamed: aComponentName ].
	component
		condition: condition;
		groupName: groupName;
		comment: commentString;
		yourself.
	^ component
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> addSubApplicationNamed: aComponentName condition: condition groupName: groupName comment: commentString [
	| component |
	self components
		at: aComponentName
		ifPresent: [ 
			self
				error: 'The component ' , aComponentName printString , ' is already present' ].
	component := self components
		at: aComponentName
		ifAbsentPut: [ RwEnvySubApplicationProjectLoadComponentV2 newNamed: aComponentName ].
	component
		condition: condition;
		groupName: groupName;
		comment: commentString;
		yourself.
	^ component
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> addSubApplicationNamed: componentName toComponentNamed: toComponentName [
	| component |
	component := self
		componentNamed: toComponentName
		ifAbsent: [ self error: 'The component ' , toComponentName printString , ' is undefined' ].
	component addSubApplicationNamed: componentName
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> categoryComponentsFor: componentNameList [
	"category components are the components listed in the top-level loaded components"

	| topLevel categoryComponents |
	topLevel := componentNameList
		collect: [ :componentName | self componentNamed: componentName ].
	categoryComponents := Set new.
	topLevel
		do: [ :component | 
			component componentNames
				do: [ :componentName | categoryComponents add: (self componentNamed: componentName) ] ].
	^ categoryComponents asArray
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> componentNamed: aComponentName [
	^ self
		componentNamed: aComponentName
		ifAbsent: [ self error: 'No component named ' , aComponentName printString , ' found' ]
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> componentNamed: aComponentName ifAbsent: absentBlock [

	^ self components 
		at: aComponentName 
		ifAbsent: absentBlock
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> components [
	^components
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> components: object [
	components := object
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> componentsWithDoits [
	^ self components select: [ :each | each hasDoits ]
]

{ #category : 'enumerating' }
RwResolvedLoadComponentsV2 >> do: aBlock [
	"For each component in the receiver, evaluates the one-argument block
 aBlock with the value as the argument.  Returns the receiver."

	self components do: aBlock
]

{ #category : 'exporting' }
RwResolvedLoadComponentsV2 >> export: componentsRoot [

	self components values do: [:component|
		component exportToUrl: 'file:',  componentsRoot pathString, '/' ].
	self components isEmpty
		ifTrue: [
			"add README.md as placeholder to ensure that the directory is preserved by git"
			(componentsRoot /  'README', 'md') writeStreamDo: [ :fileStream | ] ]
]

{ #category : 'gemstone support' }
RwResolvedLoadComponentsV2 >> gemstoneSetSymbolDictName: symbolDictName forPackageNamed: packageName [
	"answer true if the package was found in one of the receivers components"

	| foundOne |
	foundOne := false.
	self components
		keysAndValuesDo: [ :componentName :component | 
			(component packageNames includes: packageName)
				ifTrue: [ 
					foundOne := true.
					component
						conditionalPackageMapSpecsAtGemStoneUserId: self _gemstoneAllUsersName
						andPackageName: packageName
						setSymbolDictNameTo: symbolDictName ] ].
	foundOne ifFalse: [ self error: 'No package named ' , packageName printString , ' found.']
]

{ #category : 'gemstone support' }
RwResolvedLoadComponentsV2 >> gemstoneSymbolDictNameForPackageNamed: packageName forUser: userId ifAbsent: absentBlock [
	self components
		keysAndValuesDo: [ :componentName :component | 
			(component packageNames includes: packageName)
				ifTrue: [ 
					| userIdPropertiesMap packagePropertiesMap |
					userIdPropertiesMap := (component
						conditionalPackageMapSpecsAtGemStoneUserId: userId)
						at: #'packageNameToPlatformPropertiesMap'
						ifAbsent: [ 
							"no entries for the specific userId, check if there's an entry for allusers"
							(component
								conditionalPackageMapSpecsAtGemStoneUserId: self _gemstoneAllUsersName)
								at: #'packageNameToPlatformPropertiesMap'
								ifAbsent: absentBlock ].
					packagePropertiesMap := userIdPropertiesMap
						at: packageName
						ifAbsent: absentBlock.
					^ packagePropertiesMap at: 'symbolDictName' ifAbsent: absentBlock ] ].
	^ absentBlock value
]

{ #category : 'initialization' }
RwResolvedLoadComponentsV2 >> initialize [
	components := Dictionary new
]

{ #category : 'testing' }
RwResolvedLoadComponentsV2 >> isEmpty [
	^ self components isEmpty
]

{ #category : 'copying' }
RwResolvedLoadComponentsV2 >> postCopy [
	super postCopy.
	components := self components copy.
	components keysAndValuesDo: [:key :value | components at: key put: value copy ].
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> removeComponentNamed: aComponentName [
	self components
		do: [ :component | component removeComponentNamed: aComponentName ].
	^ self components removeKey: aComponentName ifAbsent: [  ].
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> removePackageNamed: aPackageName [
	self components do: [ :component | component removePackageNamed: aPackageName ]
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> removeProjectNamed: aProjectName [
	self components do: [ :component | component removeProjectNamed: aProjectName ]
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> renameComponentNamed: aComponentPath to: aComponentName [
	| component referencePath componentPath |
	component := self components
		removeKey: aComponentPath
		ifAbsent: [ self error: 'No component named ' , aComponentPath printString , ' found' ].
	referencePath := component referencePath.
	componentPath := referencePath parent segments size = 0
		ifTrue: [ 
			"top-level component, simple rename is sufficient"
			aComponentName ]
		ifFalse: [ 
			"need to preserve the path for the component"
			(referencePath parent / aComponentName) pathString ].
	(self components includesKey: componentPath)
		ifTrue: [ 
			self
				error:
					'A component with the name ' , componentPath printString , ' already exists' ].
	component name: componentPath.
	self components
		do: [ :comp | 
			(comp componentNames includes: aComponentPath)
				ifTrue: [ 
					comp
						removeComponentNamed: aComponentPath;
						addComponentNamed: componentPath ] ].
	self components at: componentPath put: component.
	^ componentPath
]

{ #category : 'vast support' }
RwResolvedLoadComponentsV2 >> vastSetVaPrequisitesTo: prereqArray forPackageNamed: packageName [
	| foundOne |
	foundOne := false.
	self components
		keysAndValuesDo: [ :componentName :component | 
			(component packageNames includes: packageName)
				ifTrue: [ 
					foundOne := true.
					component
						conditionalPackageMapSpecsAtPackageName: packageName
						setVaPrequisitesTo: prereqArray ] ].
	foundOne
		ifFalse: [ self error: 'No package named ' , packageName printString , ' found.' ]
]

{ #category : 'vast support' }
RwResolvedLoadComponentsV2 >> vastSetVaSubApplicationsTo: subApplicationsArray forPackageNamed: packageName [
	| foundOne |
	foundOne := false.
	self components
		keysAndValuesDo: [ :componentName :component | 
			(component packageNames includes: packageName)
				ifTrue: [ 
					foundOne := true.
					component
						conditionalPackageMapSpecsAtPackageName: packageName
						setVaSubApplicationsTo: subApplicationsArray ] ].
	foundOne
		ifFalse: [ self error: 'No package named ' , packageName printString , ' found.' ]
]

{ #category : 'vast support' }
RwResolvedLoadComponentsV2 >> vastVaPrequisitesForPackageNamed: packageName ifAbsent: absentBlock [
	self components
		keysAndValuesDo: [ :componentName :component | 
			(component packageNames includes: packageName)
				ifTrue: [ 
					| packageMap propertiesMap |
					packageMap := component
						conditionalPackageMapSpecsAt: 'vast'
						ifAbsent: absentBlock.
					propertiesMap := packageMap at: packageName ifAbsent: absentBlock.
					^ propertiesMap at: 'vaPrerequisites' ifAbsent: absentBlock ] ].
	^ absentBlock value
]

{ #category : 'vast support' }
RwResolvedLoadComponentsV2 >> vastVaSubApplicationsForPackageNamed: packageName ifAbsent: absentBlock [
	self components
		keysAndValuesDo: [ :componentName :component | 
			(component packageNames includes: packageName)
				ifTrue: [ 
					| packageMap propertiesMap |
					packageMap := component
						conditionalPackageMapSpecsAt: 'vast'
						ifAbsent: absentBlock.
					propertiesMap := packageMap at: packageName ifAbsent: absentBlock.
					^ propertiesMap at: 'vaSubApplications' ifAbsent: absentBlock ] ].
	^ absentBlock value
]
