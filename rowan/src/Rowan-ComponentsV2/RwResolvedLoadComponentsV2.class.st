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

{ #category : 'enumerating' }
RwResolvedLoadComponentsV2 >> _conditionalComponentsStartingWith: aComponent platformConditionalAttributes: platformConditionalAttributes visited: visitedComponentNames do: aBlock [
	visitedComponentNames add: aComponent name.
	aComponent conditionalPropertyMatchers
		keysAndValuesDo: [ :platformMatchers :ignored | 
			(self
				_platformAttributeMatchIn: platformMatchers
				using: platformConditionalAttributes)
				ifTrue: [ 
					aBlock value: aComponent.
					aComponent componentNames
						do: [ :cName | 
							(visitedComponentNames includes: cName)
								ifFalse: [ 
									self
										_conditionalComponentsStartingWith: (self componentNamed: cName)
										platformConditionalAttributes: platformConditionalAttributes
										visited: visitedComponentNames
										do: aBlock ] ] ] ]
]

{ #category : 'private' }
RwResolvedLoadComponentsV2 >> _gemstoneAllUsersName [
	^ RwLoadSpecificationV2 _gemstoneAllUsersName
]

{ #category : 'enumerating' }
RwResolvedLoadComponentsV2 >> _matchPlatformAttributes: platformPatternMatcher using: platformConditionalAttributes [
	platformConditionalAttributes
		do: [ :anObject | 
			(platformPatternMatcher match: anObject)
				ifTrue: [ ^ true ] ].
	^ false
]

{ #category : 'enumerating' }
RwResolvedLoadComponentsV2 >> _platformAttributeMatchIn: platformMatchersList using: platformConditionalAttributes [
	platformMatchersList
		do: [ :platformPatternMatcher | 
			(self
				_matchPlatformAttributes: platformPatternMatcher
				using: platformConditionalAttributes)
				ifTrue: [ ^ true ] ].
	^ false
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
RwResolvedLoadComponentsV2 >> _validate: platformConfigurationAttributes [
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
							platformConfigurationAttributes) ].
	^ componentPackageNames
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
RwResolvedLoadComponentsV2 >> addPackageNamed: packageName toComponentNamed: componentName gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc [
	| component |
	component := self
		componentNamed: componentName
		ifAbsent: [ self error: 'The component ' , componentName printString , ' is undefined' ].
	component
		conditionalPackageMapSpecsAtGemStoneUserId: aSymbolDictAssoc key
			andPackageName: packageName
			setSymbolDictNameTo: aSymbolDictAssoc value;
		addPackageNames: {packageName}.
	^ component
]

{ #category : 'accessing' }
RwResolvedLoadComponentsV2 >> addPackagesNamed: packageNames toComponentNamed: aComponentName [
	packageNames
		do: [ :packageName | self addPackageNamed: packageName toComponentNamed: aComponentName ]
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
RwResolvedLoadComponentsV2 >> addSimpleComponentNamed: aComponentName condition: condition [
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
		yourself.
	^ component
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

{ #category : 'querying' }
RwResolvedLoadComponentsV2 >> componentForPackageNamed: packageName [
	self components
		do: [ :component | 
			(component packageNames includes: packageName)
				ifTrue: [ ^ component ] ].
	^ nil
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
RwResolvedLoadComponentsV2 >> conditionalComponentsStartingWith: componentNames platformConditionalAttributes: platformConditionalAttributes do: aBlock [
	| visited |
	visited := Set new.
	componentNames
		do: [ :componentName | 
			| theComponent |
			theComponent := self componentNamed: componentName.

			self
				_conditionalComponentsStartingWith: theComponent
				platformConditionalAttributes: platformConditionalAttributes
				visited: visited
				do: aBlock ]
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
			(componentsRoot /  'README', 'md') ensureCreateFile ]
]

{ #category : 'gemstone support' }
RwResolvedLoadComponentsV2 >> gemstoneSetSymbolDictName: symbolDictName forPackageNamed: packageName [
	self
		gemstoneSetSymbolDictNameForUser: self _gemstoneAllUsersName
		to: symbolDictName
		forPackageNamed: packageName
]

{ #category : 'gemstone support' }
RwResolvedLoadComponentsV2 >> gemstoneSetSymbolDictNameForUser: userId to: symbolDictName forPackageNamed: packageName [
	| foundOne |
	foundOne := false.
	self components
		keysAndValuesDo: [ :componentName :component | 
			(component packageNames includes: packageName)
				ifTrue: [ 
					foundOne := true.
					component
						conditionalPackageMapSpecsAtGemStoneUserId: userId
						andPackageName: packageName
						setSymbolDictNameTo: symbolDictName ] ].
	foundOne
		ifFalse: [ self error: 'No package named ' , packageName printString , ' found.' ]
]

{ #category : 'gemstone support' }
RwResolvedLoadComponentsV2 >> gemstoneSetUseSessionMethodsForExtensions: aBool forPackageNamed: packageName [
	self
		gemstoneSetUseSessionMethodsForExtensionsForUser: self _gemstoneAllUsersName
		to: aBool
		forPackageNamed: packageName
]

{ #category : 'gemstone support' }
RwResolvedLoadComponentsV2 >> gemstoneSetUseSessionMethodsForExtensionsForUser: userId to: aBool forPackageNamed: packageName [
	| foundOne |
	foundOne := false.
	self components
		keysAndValuesDo: [ :componentName :component | 
			(component packageNames includes: packageName)
				ifTrue: [ 
					foundOne := true.
					component
						conditionalPackageMapSpecsAtGemStoneUserId: userId
						andPackageName: packageName
						setUseSessionMethodsForExtensions: aBool ] ].
	foundOne
		ifFalse: [ self error: 'No package named ' , packageName printString , ' found.' ]
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

{ #category : 'querying' }
RwResolvedLoadComponentsV2 >> subcomponentsOf: componentName resolvedProject: resolvedProject matchBlock: matchBlock ifNone: noneBlock [
	| aComponent subcomponents |
	subcomponents := {}.
	aComponent := self
		componentNamed: componentName
		ifAbsent: [ 
			"read from disk if not present?"
			^ noneBlock value ].
	(matchBlock value: aComponent)
		ifFalse: [ 
			"The component is not loadable, so ignore it's subcomponents"
			^ subcomponents ].
	aComponent componentNames
		do: [ :subcomponentName | 
			| subcomponent |
			subcomponent := self
				componentNamed: subcomponentName
				ifAbsent: [ 
					"read from disk if not present?"
					^ noneBlock value ].
			(matchBlock value: subcomponent)
				ifTrue: [ subcomponents add: subcomponent ] ].
	^ subcomponents
]
