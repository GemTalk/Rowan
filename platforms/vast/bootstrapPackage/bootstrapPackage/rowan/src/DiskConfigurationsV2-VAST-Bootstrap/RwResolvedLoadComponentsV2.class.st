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

{ #category : 'private' }
RwResolvedLoadComponentsV2 >> _validate [
	"ensure that each of the components is valid and return a list of the package names managed by all components"

	| componentPackageNames |
	componentPackageNames := Set new.
	self components
		keysAndValuesDo: [ :componentName :component | 
			component validate.
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
			component validate.
			componentPackageNames
				addAll:
					(component
						packageNamesForPlatformConfigurationAttributes:
							platformConfigurationAttributes
						groupNames: groupNames) ].
	^ componentPackageNames
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> addComponentNamed: aComponentName comment: commentString [
	| component |
	component := self components
		at: aComponentName
		ifAbsentPut: [ RwProjectLoadComponentV2 newNamed: aComponentName ].
	^ component
		addDefinedGroupName: self defaultGroupName includeGroups: #();
		comment: commentString;
		yourself
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> addComponentNamed: aComponentName definedGroupNames: groupNameDict comment: commentString [
	| component |
	component := self components
		at: aComponentName
		ifAbsentPut: [ RwProjectLoadComponentV2 newNamed: aComponentName ].
	groupNameDict
		keysAndValuesDo: [ :groupName :includeGroups | component defineGroupNamed: groupName toIncludeGroups: includeGroups ].
	component comment: commentString.
	^ component
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> addComponentNamed: componentName toComponentNamed: toComponentName withConditions: conditionArray andGroupName: groupName [
	| component |
	self
		componentNamed: toComponentName
		ifAbsent: [ self error: 'The component ' , componentName printString , ' is undefined' ].
	component := self components
		at: toComponentName
		ifAbsentPut: [ RwProjectLoadComponentV2 newNamed: componentName ].
	component
		conditionalPropertiesAtConditions: conditionArray
		andGroup: groupName
		addComponentNames: {componentName}
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> addNestedComponentNamed: aComponentName comment: commentString [
	| component |
	component := self components
		at: aComponentName
		ifAbsentPut: [ RwNestedProjectLoadComponentV2 newNamed: aComponentName ].
	^ component
		addDefinedGroupName: self defaultGroupName includeGroups: #();
		comment: commentString;
		yourself
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> addNestedComponentNamed: aComponentName definedGroupNames: groupNameDict comment: commentString [
	| component |
	component := self components
		at: aComponentName
		ifAbsentPut: [ RwNestedProjectLoadComponentV2 newNamed: aComponentName ].
	groupNameDict
		keysAndValuesDo: [ :groupName :includeGroups | component defineGroupNamed: groupName toIncludeGroups: includeGroups ].
	component comment: commentString.
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

{ #category : 'exporting' }
RwResolvedLoadComponentsV2 >> export: componentsRoot [

	self components isEmpty ifTrue: [
		self addComponentNamed: self defaultComponentName comment: '' ].
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
