Class {
	#name : 'RwResolvedProjectComponentsV2',
	#superclass : 'Object',
	#instVars : [
		'components',
		'packageGroups'
	],
	#category : 'Rowan-DefinitionsV2'
}

{ #category : 'instance creation' }
RwResolvedProjectComponentsV2 class >> new [

	^self basicNew initialize

]

{ #category : 'dispatching' }
RwResolvedProjectComponentsV2 >> _addActiveComponent: aComponent [
	"not sure I like how this is used ... the component structure needs to be kept in sync with packages, so this is not quite the route to go, unless we ensure that the component has an entry for the package"

	"see similar comment in addRawPackageNamed: and addPackages:forComponent: "

	"should be sent from the component visitor ... not unexpected to have a duplicate, but the new
		component --- presumably freshly read from disk --- wins"

	^ self components at: aComponent name put: aComponent
]

{ #category : 'dispatching' }
RwResolvedProjectComponentsV2 >> _addComponent: aComponent [
	"double dispatch, so that _addActiveComponent: or _addPackageGroup: can be called based on class of aComponent"

	^ aComponent _addToResolvedProjectComponents: self
]

{ #category : 'dispatching' }
RwResolvedProjectComponentsV2 >> _addPackageGroup: aPackageGroup [
	"not sure I like how this is used ... the component structure needs to be kept in sync with packages, so this is not quite the route to go, unless we ensure that the component has an entry for the package"

	"see similar comment in addRawPackageNamed: and addPackages:forComponent: "

	"should be sent from the component visitor ... not unexpected to have a duplicate, but the new
		component --- presumably freshly read from disk --- wins"

	^ self packageGroups at: aPackageGroup name put: aPackageGroup
]

{ #category : 'enumerating' }
RwResolvedProjectComponentsV2 >> _conditionalComponentsStartingWith: aComponent customConditionalAttributes: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes visited: visitedComponentNames do: aBlock [
	visitedComponentNames add: aComponent name.
	aComponent conditionalPropertyMatchers
		keysAndValuesDo: [ :platformMatchers :ignored | 
			(self
				_platformAttributeMatchIn: platformMatchers
				using: platformConditionalAttributes, customConditionalAttributes)
				ifTrue: [ 
					aBlock value: aComponent.
					aComponent componentNames
						do: [ :cName | 
							(visitedComponentNames includes: cName)
								ifFalse: [ 
									self
										_conditionalComponentsStartingWith:
											(self componentNamed: cName ifAbsent: [ self packageGroupNamed: cName ])
										customConditionalAttributes: customConditionalAttributes
										platformConditionalAttributes: platformConditionalAttributes
										visited: visitedComponentNames
										do: aBlock ] ] ] ]
]

{ #category : 'private' }
RwResolvedProjectComponentsV2 >> _gemstoneAllUsersName [
	"duplicate of  RwLoadSpecificationV2 class >> _gemstoneAllUsersName BECAUSE
		the LOADER sends this message in the middle of a load (to a previously 
		created INSTANCE of the receiver) and the loader chokes sending the 
		message to RwLoadSpecificationV2 if a NEW version of RwLoadSpecificationV2 is being 
		created ... sending message to instance is safe, because the instance
		will not be redefined during LOAD (see https://github.com/GemTalk/Rowan/issues/611)"

	^ 'allusers'
]

{ #category : 'enumerating' }
RwResolvedProjectComponentsV2 >> _matchPlatformAttributes: platformPatternMatcher using: conditionalAttributes [
	conditionalAttributes
		do: [ :anObject | 
			(platformPatternMatcher match: anObject)
				ifTrue: [ ^ true ] ].
	^ false
]

{ #category : 'enumerating' }
RwResolvedProjectComponentsV2 >> _platformAttributeMatchIn: platformMatchersList using: conditionalAttributes [
	platformMatchersList
		do: [ :platformPatternMatcher | 
			(self
				_matchPlatformAttributes: platformPatternMatcher
				using: conditionalAttributes)
				ifTrue: [ ^ true ] ].
	^ false
]

{ #category : 'private' }
RwResolvedProjectComponentsV2 >> _renameComponentNamed: aComponentOrPackageGroupPath to: aComponentName in: aComponentDictionary label: label [
	"change the basename of aComponentOrPackageGroupPath to <baseName>, i.e., the path is not changed"

	| component referencePath componentPath |
	component := aComponentDictionary
		at: aComponentOrPackageGroupPath
		ifAbsent: [ 
			self
				error:
					'No ' , label , ' named ' , aComponentOrPackageGroupPath printString , ' found' ].
	referencePath := component referencePath.
	componentPath := referencePath parent segments size = 0
		ifTrue: [ 
			"top-level component, simple rename is sufficient"
			aComponentName ]
		ifFalse: [ 
			"need to preserve the path for the component"
			(referencePath parent / aComponentName) pathString ].
	(self componentOrPackageGroupNamed: componentPath ifAbsent: [  ])
		ifNotNil: [ 
			self
				error:
					'A component or package group with the name ' , componentPath printString
						, ' already exists' ].
	component := aComponentDictionary removeKey: aComponentOrPackageGroupPath.
	component name: componentPath.
	self components values
		do: [ :comp | 
			"avoid modifying components during do"
			(comp componentNames includes: aComponentOrPackageGroupPath)
				ifTrue: [ 
					comp
						removeComponentNamed: aComponentOrPackageGroupPath;
						addComponentNamed: componentPath ] ].
	self packageGroups values
		do: [ :comp | 
			"avoid modifying package groups during do"
			(comp componentNames includes: aComponentOrPackageGroupPath)
				ifTrue: [ 
					comp
						removeComponentNamed: aComponentOrPackageGroupPath;
						addComponentNamed: componentPath ] ].
	aComponentDictionary at: componentPath put: component.
	^ componentPath
]

{ #category : 'private' }
RwResolvedProjectComponentsV2 >> _validate [
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
RwResolvedProjectComponentsV2 >> _validate: conditionalAttributes [
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
						packageNamesForConditionalAttributes:
							conditionalAttributes) ].
	^ componentPackageNames
]

{ #category : 'components' }
RwResolvedProjectComponentsV2 >> addComponentNamed: componentName toComponentNamed: toComponentName [
	"add existing component named componentName to component named toComponentName"

	| component |
	component := self
		componentNamed: toComponentName
		ifAbsent: [ self error: 'The component ' , toComponentName printString , ' is undefined' ].
	component addComponentNamed: componentName
]

{ #category : 'components' }
RwResolvedProjectComponentsV2 >> addLoadComponentNamed: aComponentName comment: aString [
	"add a new instance of RwLoadComponent to the project components"

	| component |
	self components
		at: aComponentName
		ifPresent: [ 
			self
				error: 'The component ' , aComponentName printString , ' is already present' ].
	component := self components
		at: aComponentName
		ifAbsentPut: [ RwLoadComponent newNamed: aComponentName ].
	component comment: aString.
	^ component
]

{ #category : 'components' }
RwResolvedProjectComponentsV2 >> addPackageGroupNamed: aPackageGroupName condition: aCondition comment: aString [
	| packageGroup |
	(self packageGroupNamed: aPackageGroupName ifAbsent: [  ])
		ifNotNil: [ 
			self
				error:
					'The package group ' , aPackageGroupName printString , ' is already present' ].
	packageGroup := self packageGroups
		at: aPackageGroupName
		ifAbsentPut: [ RwPackageGroup newNamed: aPackageGroupName ].
	packageGroup
		comment: aString;
		condition: aCondition;
		yourself.
	^ packageGroup
]

{ #category : 'accessing' }
RwResolvedProjectComponentsV2 >> addPackageNamed: packageName toComponentNamed: componentName [ 
	| component |
	component := self
		componentNamed: componentName
		ifAbsent: [ self error: 'The component ' , componentName printString , ' is undefined' ].
	component
		addPackageNames: {packageName}.
	^ component
]

{ #category : 'accessing' }
RwResolvedProjectComponentsV2 >> addPackageNamed: packageName toComponentNamed: componentName gemstoneDefaultSymbolDictionaryForUser: aSymbolDictAssoc [
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
RwResolvedProjectComponentsV2 >> addPackagesNamed: packageNames toComponentNamed: aComponentName [
	packageNames
		do: [ :packageName | self addPackageNamed: packageName toComponentNamed: aComponentName ]
]

{ #category : 'components to be cleaned up' }
RwResolvedProjectComponentsV2 >> addPlatformNestedComponentNamed: aComponentName condition: conditionArray comment: commentString [
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

{ #category : 'components' }
RwResolvedProjectComponentsV2 >> addPlatformSubcomponentNamed: aComponentName condition: aConditionArray comment: aString [
	| component |
	self components
		at: aComponentName
		ifPresent: [ 
			self
				error: 'The component ' , aComponentName printString , ' is already present' ].
	component := self components
		at: aComponentName
		ifAbsentPut: [ RwPlatformSubcomponent newNamed: aComponentName ].
	component
		comment: aString;
		condition: aConditionArray;
		yourself.
	^ component
]

{ #category : 'accessing' }
RwResolvedProjectComponentsV2 >> addProjectNamed: projectName toComponentNamed: toComponentName [
	| component |
	component := self
		componentNamed: toComponentName
		ifAbsent: [ self error: 'The component ' , toComponentName printString , ' is undefined' ].
	component addProjectNamed: projectName
]

{ #category : 'components to be cleaned up' }
RwResolvedProjectComponentsV2 >> addSimpleComponentNamed: aComponentName condition: condition comment: commentString [
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

{ #category : 'components to be cleaned up' }
RwResolvedProjectComponentsV2 >> addSimpleNestedComponentNamed: aComponentName condition: condition comment: commentString [
	| component |
	self components
		at: aComponentName
		ifPresent: [ :ignored | 
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

{ #category : 'components' }
RwResolvedProjectComponentsV2 >> addSubcomponentNamed: aComponentName condition: aCondition comment: aString [
	| component |
	self components
		at: aComponentName
		ifPresent: [ 
			self
				error: 'The component ' , aComponentName printString , ' is already present' ].
	component := self components
		at: aComponentName
		ifAbsentPut: [ RwSubcomponent newNamed: aComponentName ].
	component
		comment: aString;
		condition: aCondition;
		yourself.
	^ component
]

{ #category : 'querying' }
RwResolvedProjectComponentsV2 >> componentForPackageNamed: packageName [
	self components
		do: [ :component | 
			(component packageNames includes: packageName)
				ifTrue: [ ^ component ] ].
	^ nil
]

{ #category : 'accessing' }
RwResolvedProjectComponentsV2 >> componentNamed: aComponentName [
	^ self
		componentNamed: aComponentName
		ifAbsent: [ self error: 'No component named ' , aComponentName printString , ' found' ]
]

{ #category : 'accessing' }
RwResolvedProjectComponentsV2 >> componentNamed: aComponentName ifAbsent: absentBlock [

	^ self components 
		at: aComponentName 
		ifAbsent: absentBlock
]

{ #category : 'accessing' }
RwResolvedProjectComponentsV2 >> componentNames [
	"list of defined components in the receiver"

	^ self components keys asArray
]

{ #category : 'other' }
RwResolvedProjectComponentsV2 >> componentOrPackageGroupNamed: aComponentName [
	^ self
		componentOrPackageGroupNamed: aComponentName
		ifAbsent: [ 
			self
				error:
					'No component or package group named ' , aComponentName printString , ' found' ]
]

{ #category : 'other' }
RwResolvedProjectComponentsV2 >> componentOrPackageGroupNamed: aComponentName ifAbsent: absentBlock [
	^ self
		componentNamed: aComponentName
		ifAbsent: [ self packageGroupNamed: aComponentName ifAbsent: absentBlock ]
]

{ #category : 'accessing' }
RwResolvedProjectComponentsV2 >> components [
	^components
]

{ #category : 'accessing' }
RwResolvedProjectComponentsV2 >> componentsWithDoits [
	^ self components select: [ :each | each hasDoits ]
]

{ #category : 'enumerating' }
RwResolvedProjectComponentsV2 >> conditionalComponentsStartingWith: componentNames customConditionalAttributes: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes do: aBlock [
	| visited |
	visited := Set new.
	componentNames
		do: [ :componentName | 
			| theComponent |
			theComponent := self componentNamed: componentName.

			self
				_conditionalComponentsStartingWith: theComponent
				customConditionalAttributes: customConditionalAttributes
				platformConditionalAttributes: platformConditionalAttributes
				visited: visited
				do: aBlock ]
]

{ #category : 'enumerating' }
RwResolvedProjectComponentsV2 >> do: aBlock [
	"For each component in the receiver, evaluates the one-argument block
 aBlock with the value as the argument.  Returns the receiver."

	self components do: aBlock
]

{ #category : 'exporting' }
RwResolvedProjectComponentsV2 >> export: componentsRoot [
	self components values
		do: [ :component | component exportToUrl: 'file:' , componentsRoot pathString , '/' ].
	self packageGroups values
		do: [ :packgeGroup | packgeGroup exportToUrl: 'file:' , componentsRoot pathString , '/' ].
	(self components isEmpty and: [ self packageGroups isEmpty ])
		ifTrue: [ 
			"add README.md as placeholder to ensure that the directory is preserved by git"
			componentsRoot / 'README' , 'md' writeStreamDo: [ :fileStream |  ] ]
]

{ #category : 'gemstone support' }
RwResolvedProjectComponentsV2 >> gemstonePackagePropertyPropertyMapForPackageNamed: packageName forUser: userId ifAbsent: absentBlock [
	self components
		keysAndValuesDo: [ :componentName :component | 
			(component packageNames includes: packageName)
				ifTrue: [ 
					| gemstonePropertiesMap packagePropertiesMap |
					gemstonePropertiesMap := component conditionalPackageMapSpecs
						at: 'gemstone'
						ifAbsent: [ ^ Dictionary new ].
					{userId.
					(RwLoadSpecificationV2 _gemstoneAllUsersName)}
						do: [ :theUserId | 
							"if we don't get a valid result with userId use allusers"
							(gemstonePropertiesMap at: theUserId ifAbsent: [  ])
								ifNotNil: [ :userIdPropertiesMap | 
									packagePropertiesMap := userIdPropertiesMap
										at: #'packageNameToPlatformPropertiesMap'
										ifAbsent: absentBlock.
									(packagePropertiesMap includesKey: packageName)
										ifTrue: [ ^ packagePropertiesMap ] ] ] ] ].
	^ absentBlock value
]

{ #category : 'gemstone support' }
RwResolvedProjectComponentsV2 >> gemstoneSetSymbolDictName: symbolDictName forPackageNamed: packageName [
	self
		gemstoneSetSymbolDictNameForUser: self _gemstoneAllUsersName
		to: symbolDictName
		forPackageNamed: packageName
]

{ #category : 'gemstone support' }
RwResolvedProjectComponentsV2 >> gemstoneSetSymbolDictNameForUser: userId to: symbolDictName forPackageNamed: packageName [
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
RwResolvedProjectComponentsV2 >> gemstoneSetUseSessionMethodsForExtensions: aBool forPackageNamed: packageName [
	self
		gemstoneSetUseSessionMethodsForExtensionsForUser: self _gemstoneAllUsersName
		to: aBool
		forPackageNamed: packageName
]

{ #category : 'gemstone support' }
RwResolvedProjectComponentsV2 >> gemstoneSetUseSessionMethodsForExtensionsForUser: userId to: aBool forPackageNamed: packageName [
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

{ #category : 'initialization' }
RwResolvedProjectComponentsV2 >> initialize [
	components := Dictionary new.
	packageGroups := Dictionary new
]

{ #category : 'testing' }
RwResolvedProjectComponentsV2 >> isEmpty [
	^ self components isEmpty
]

{ #category : 'other' }
RwResolvedProjectComponentsV2 >> packageGroupNamed: aPackageGroupName [
	^ self
		packageGroupNamed: aPackageGroupName
		ifAbsent: [ self error: 'No package group named ' , aPackageGroupName printString , ' found' ]
]

{ #category : 'accessing' }
RwResolvedProjectComponentsV2 >> packageGroupNamed: aPackageGroupName ifAbsent: absentBlock [
	^ self packageGroups at: aPackageGroupName ifAbsent: absentBlock
]

{ #category : 'accessing' }
RwResolvedProjectComponentsV2 >> packageGroupNames [
	^ self packageGroups keys asArray
]

{ #category : 'accessing' }
RwResolvedProjectComponentsV2 >> packageGroups [
	^packageGroups
]

{ #category : 'copying' }
RwResolvedProjectComponentsV2 >> postCopy [
	| old |
	super postCopy.
	old := self components.
	components := Dictionary new.
	old keysAndValuesDo: [ :key :value | components at: key put: value copy ].
	old := self packageGroups.
	packageGroups := Dictionary new.
	old keysAndValuesDo: [ :key :value | packageGroups at: key put: value copy ]
]

{ #category : 'accessing' }
RwResolvedProjectComponentsV2 >> removeComponentNamed: aComponentName [
	| theComponent |
	theComponent := self components removeKey: aComponentName ifAbsent: [ ^ nil ].
	self components
		do: [ :component | component removeComponentNamed: aComponentName ].
	self packageGroups
		do: [ :component | component removeComponentNamed: aComponentName ].
	^ theComponent
]

{ #category : 'accessing' }
RwResolvedProjectComponentsV2 >> removePackageGroupNamed: aComponentName [
	| theComponent |
	theComponent := self packageGroups removeKey: aComponentName ifAbsent: [ ^ nil ].
	self components
		do: [ :component | component removeComponentNamed: aComponentName ].
	self packageGroups
		do: [ :component | component removeComponentNamed: aComponentName ].
	^ theComponent
]

{ #category : 'accessing' }
RwResolvedProjectComponentsV2 >> removePackageNamed: aPackageName [
	self components do: [ :component | component removePackageNamed: aPackageName ]
]

{ #category : 'accessing' }
RwResolvedProjectComponentsV2 >> removeProjectNamed: aProjectName [
	self components do: [ :component | component removeProjectNamed: aProjectName ]
]

{ #category : 'accessing' }
RwResolvedProjectComponentsV2 >> renameComponentNamed: aComponentPath to: baseName [
	"change the basename of aComponentPath to <baseName>, i.e., the path is not changed"

	^ self
		_renameComponentNamed: aComponentPath
		to: baseName
		in: self components
		label: 'component'
]

{ #category : 'accessing' }
RwResolvedProjectComponentsV2 >> renamePackageGroupNamed: aComponentPath to: baseName [
	"change the basename of aComponentPath to <baseName>, i.e., the path is not changed"

	^ self
		_renameComponentNamed: aComponentPath
		to: baseName
		in: self packageGroups
		label: 'component'
]

{ #category : 'querying' }
RwResolvedProjectComponentsV2 >> subcomponentsOf: componentName matchBlock: matchBlock ifNone: noneBlock [
	| aComponent subcomponents |
	subcomponents := {}.
	aComponent := self
		componentOrPackageGroupNamed: componentName
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
				componentOrPackageGroupNamed: subcomponentName
				ifAbsent: [ 
					"noneBlock, if it returns, should answer a component"
					noneBlock cull: subcomponentName ].
			(matchBlock value: subcomponent)
				ifTrue: [ subcomponents add: subcomponent ] ].
	^ subcomponents
]
