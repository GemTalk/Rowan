Class {
	#name : 'RwAbstractProjectLoadComponentV2',
	#superclass : 'RwBasicProjectLoadComponentV2',
	#instVars : [
		'definedGroupNames',
		'conditionalProperties',
		'conditionalPropertyMatchers',
		'conditionalPackageMapSpecMatchers'
	],
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'private' }
RwAbstractProjectLoadComponentV2 >> _conditionalPropertiesAt: key conditions: conditions andGroup: groupName addNames: names [
	| theNames theConditionalPropertiesMap |
	theConditionalPropertiesMap := (self conditionalProperties
		at: conditions asArray sort
		ifAbsentPut: [ Dictionary new ]) at: groupName ifAbsentPut: [ Dictionary new ].
	theNames := (theConditionalPropertiesMap
		at: key asSymbol
		ifAbsentPut: [ Set new ]) asSet.
	theNames addAll: names.
	theNames := theNames asArray sort.
	theConditionalPropertiesMap at: key asSymbol put: theNames.
	^ theNames
]

{ #category : 'private' }
RwAbstractProjectLoadComponentV2 >> _conditionalPropertiesAt: key conditions: conditions andGroup: groupName removeNames: names [
	| theNames theConditionalPropertiesMap |
	theConditionalPropertiesMap := (self conditionalProperties
		at: conditions asArray sort
		ifAbsentPut: [ Dictionary new ]) at: groupName ifAbsentPut: [ Dictionary new ].
	theNames := (theConditionalPropertiesMap
		at: key asSymbol
		ifAbsentPut: [ Set new ]) asSet.
	theNames removeAll: names.
	theNames := theNames asArray sort.
	theConditionalPropertiesMap at: key asSymbol put: theNames.
	^ theNames
]

{ #category : 'validation' }
RwAbstractProjectLoadComponentV2 >> _validatedPackageNames [
	"answer the validated set of package names"

	| allDefinedPackageNames knownGroupNames |
	allDefinedPackageNames := Set new.
	knownGroupNames := self definedGroupNames keys.
	self definedGroupNames
		keysAndValuesDo: [ :groupName :dependentGroups | 
			dependentGroups
				do: [ :dependentGroupName | 
					(knownGroupNames includes: dependentGroupName)
						ifFalse: [ 
							Error
								signal:
									'The group ' , dependentGroupName printString , ' is not a defined group' ] ] ].
	self conditionalProperties
		keysAndValuesDo: [ :conditions :groupMap | 
			groupMap
				keysAndValuesDo: [ :groupName :propertiesMap | 
					(self definedGroupNames keys includes: groupName)
						ifFalse: [ 
							Error
								signal:
									'Conditional packages includes group name ' , groupName printString
										, ' that is not a defined group' ].
					allDefinedPackageNames
						addAll: (propertiesMap at: #'packageNames' ifAbsent: [ #() ]) ] ].
	^ allDefinedPackageNames
]

{ #category : 'visiting' }
RwAbstractProjectLoadComponentV2 >> acceptNestedVisitor: aVisitor [

	^ self acceptVisitor: aVisitor
]

{ #category : 'visiting' }
RwAbstractProjectLoadComponentV2 >> acceptVisitor: aVisitor [

	^ self subclassResponsibility: #acceptVisitor:

]

{ #category : 'accessing' }
RwAbstractProjectLoadComponentV2 >> conditionalComponentsAtConditions: conditions andGroup: groupName [
	^ self
		conditionalPropertiesAt: #'componentNames'
		conditions: conditions
		andGroup: groupName
]

{ #category : 'private' }
RwAbstractProjectLoadComponentV2 >> conditionalPackageMapSpecMatchers [

	conditionalPackageMapSpecMatchers ifNotNil: [:val | ^ val ]. 
	conditionalPackageMapSpecMatchers := Dictionary new.
	self conditionalPackageMapSpecs keysAndValuesDo: [:platformPattern :packageMapSpecsMap |
		conditionalPackageMapSpecMatchers
			at: (self _platformPatternMatcherFor: platformPattern)
			put: packageMapSpecsMap ].
	^ conditionalPackageMapSpecMatchers

]

{ #category : 'accessing' }
RwAbstractProjectLoadComponentV2 >> conditionalPackagesAtConditions: conditions andGroup: groupName [
	^ self
		conditionalPropertiesAt: #'packageNames'
		conditions: conditions
		andGroup: groupName
]

{ #category : 'accessing' }
RwAbstractProjectLoadComponentV2 >> conditionalProjectsAtConditions: conditions andGroup: groupName [
	^ self
		conditionalPropertiesAt: #'projectNames'
		conditions: conditions
		andGroup: groupName
]

{ #category : 'accessing' }
RwAbstractProjectLoadComponentV2 >> conditionalProperties [
	^ conditionalProperties ifNil: [ conditionalProperties := Dictionary new ]
]

{ #category : 'accessing' }
RwAbstractProjectLoadComponentV2 >> conditionalPropertiesAt: key conditions: conditions andGroup: groupName [

	| thePropertiesMap |
	thePropertiesMap := (self conditionalProperties at: conditions asArray sort ifAbsent: [ ^ Set new])
		at: groupName ifAbsent: [ ^ Set new ].
	^ (thePropertiesMap at: key asSymbol ifAbsent: [ Set new ]) asSet
]

{ #category : 'accessing' }
RwAbstractProjectLoadComponentV2 >> conditionalPropertiesAtConditions: conditions andGroup: groupName addComponentNames: names [
	^ self
		_conditionalPropertiesAt: #'componentNames'
		conditions: conditions
		andGroup: groupName
		addNames: names
]

{ #category : 'accessing' }
RwAbstractProjectLoadComponentV2 >> conditionalPropertiesAtConditions: conditions andGroup: groupName addPackageNames: names [
	^ self
		_conditionalPropertiesAt: #'packageNames'
		conditions: conditions
		andGroup: groupName
		addNames: names
]

{ #category : 'accessing' }
RwAbstractProjectLoadComponentV2 >> conditionalPropertiesAtConditions: conditions andGroup: groupName addProjectNames: names [
	^ self
		_conditionalPropertiesAt: #'projectNames'
		conditions: conditions
		andGroup: groupName
		addNames: names
]

{ #category : 'accessing' }
RwAbstractProjectLoadComponentV2 >> conditionalPropertiesAtConditions: conditions andGroup: groupName removeComponentNames: names [
	^ self
		_conditionalPropertiesAt: #'componentNames'
		conditions: conditions
		andGroup: groupName
		removeNames: names
]

{ #category : 'accessing' }
RwAbstractProjectLoadComponentV2 >> conditionalPropertiesAtConditions: conditions andGroup: groupName removePackageNames: names [
	^ self
		_conditionalPropertiesAt: #'packageNames'
		conditions: conditions
		andGroup: groupName
		removeNames: names
]

{ #category : 'accessing' }
RwAbstractProjectLoadComponentV2 >> conditionalPropertiesAtConditions: conditions andGroup: groupName removeProjectNames: names [
	^ self
		_conditionalPropertiesAt: #'projectNames'
		conditions: conditions
		andGroup: groupName
		removeNames: names
]

{ #category : 'private' }
RwAbstractProjectLoadComponentV2 >> conditionalPropertyMatchers [

	conditionalPropertyMatchers ifNotNil: [:val | ^ val ]. 
	conditionalPropertyMatchers := Dictionary new.
	self conditionalProperties keysAndValuesDo: [:platformPatterns :groupMap |
		conditionalPropertyMatchers
			at: (platformPatterns collect: [:pattern | self _platformPatternMatcherFor: pattern ])
			put: groupMap ].
	^ conditionalPropertyMatchers
]

{ #category : 'accessing' }
RwAbstractProjectLoadComponentV2 >> definedGroupNames [

	^ definedGroupNames 
		ifNil: [ definedGroupNames :=  Dictionary new]
]

{ #category : 'accessing' }
RwAbstractProjectLoadComponentV2 >> definedGroupNames: aColl [

	definedGroupNames := aColl

]

{ #category : 'accessing' }
RwAbstractProjectLoadComponentV2 >> defineGroupNamed: groupName [ 

	"add a group that does not include any other groups"

	self defineGroupNamed: groupName toIncludeGroups: #()
]

{ #category : 'accessing' }
RwAbstractProjectLoadComponentV2 >> defineGroupNamed: groupName toIncludeGroups: includedGroups [

	"add a group that does not include any other groups"

	self definedGroupNames at: groupName put: includedGroups asArray sort
]

{ #category : 'ston' }
RwAbstractProjectLoadComponentV2 >> excludedInstVars [
	^ super excludedInstVars
		, #(#'conditionalPropertyMatchers' #'conditionalPackageMapSpecMatchers')
]

{ #category : 'exporting' }
RwAbstractProjectLoadComponentV2 >> export [

	|  exportUrl exportPath |
	exportPath := self _repositoryRootPath asFileReference / self _configsPath.
	exportPath ensureCreateDirectory.
	exportUrl := exportPath fileSystem isMemoryFileSystem
		ifTrue: [ 'memory:' , exportPath pathString ]
		ifFalse: [ 'file:' , exportPath pathString, '/' ].
	^ self exportToUrl: exportUrl
]

{ #category : 'ston' }
RwAbstractProjectLoadComponentV2 >> fromSton: stonReader [
	"componentNames and projectNames no longer supported ... componentNames and projectnames should be inserted into conditionalProperties under 'common'"

	| instanceVariableNames componentNames projectNames |
	instanceVariableNames := self class allInstVarNames.
	stonReader
		parseMapDo: [ :instVarName :value | 
			instVarName = #'componentNames'
				ifTrue: [ 
					componentNames ifNil: [ componentNames := Set new ].
					componentNames addAll: value ]
				ifFalse: [ 
					instVarName = #'projectNames'
						ifTrue: [ 
							projectNames ifNil: [ projectNames := Set new ].
							projectNames addAll: value ]
						ifFalse: [ 
							instVarName = #'conditionalPackages'
								ifTrue: [ conditionalProperties := value ]
								ifFalse: [ self instVarAt: (instanceVariableNames indexOf: instVarName asSymbol) put: value ] ] ] ].
	componentNames
		ifNotNil: [ 
			self
				conditionalPropertiesAtConditions: {'common'}
				andGroup: 'core'
				addComponentNames: componentNames ].
	projectNames
		ifNotNil: [ 
			self
				conditionalPropertiesAtConditions: {'common'}
				andGroup: 'core'
				addProjectNames: projectNames ]
]

{ #category : 'initialization' }
RwAbstractProjectLoadComponentV2 >> initializeForExport [
	"if spec is to be exported, clear out any of the fields that represent state that should 
	not be shared"

	"for export, the keys in the dictionaries of the structures need to be put into canonical order"

	super initializeForExport.
	conditionalPropertyMatchers := conditionalPackageMapSpecMatchers := nil.
	self conditionalProperties
		ifNotNil: [ :cp | 
			| orderedConditionalProperties |
			orderedConditionalProperties := self class orderedDictionaryClass new.
			(cp keys asSortedCollection: [ :a :b | (a at: 1) <= (b at: 1) ])
				do: [ :ar | 
					| dict orderedPropertyNames |
					dict := cp at: ar.
					orderedPropertyNames := self class orderedDictionaryClass new.
					dict keys asArray sort
						do: [ :group | orderedPropertyNames at: group put: (dict at: group) ].
					orderedConditionalProperties at: ar put: orderedPropertyNames ].
			conditionalProperties := orderedConditionalProperties ]
]

{ #category : 'accessing' }
RwAbstractProjectLoadComponentV2 >> packageNames [
	"Answer the collection of package names defined in the receiver."

	| allDefinedPackageNames |
	allDefinedPackageNames := Set new.
	self conditionalProperties
		keysAndValuesDo: [ :conditions :groupMap | 
			groupMap
				keysAndValuesDo: [ :groupName :propertiesMap | allDefinedPackageNames addAll: (propertiesMap at: #'packageNames') ] ].
	^ allDefinedPackageNames
]

{ #category : 'accessing' }
RwAbstractProjectLoadComponentV2 >> removeComponentNamed: aComponentName [
	"this can create empty componentName lists  ... the export logic _should_ cleanup empty list, which is sufficient"

	self conditionalProperties
		keysAndValuesDo: [ :conditionsArray :conditionMap | 
			conditionMap
				keysAndValuesDo: [ :groupName :propertiesMap | (propertiesMap at: #'componentNames' ifAbsent: [#()]) remove: aComponentName ifAbsent: [  ] ] ]
]

{ #category : 'accessing' }
RwAbstractProjectLoadComponentV2 >> removePackageNamed: aPackageName [
	"this can create empty packageName lists or empty packageMapSpecMaps ... the export logic _should_ cleanup empty list, which is sufficient"

	self conditionalProperties
		keysAndValuesDo: [ :conditionsArray :conditionMap | 
			conditionMap
				keysAndValuesDo: [ :groupName :propertiesMap | (propertiesMap at: #'packageNames' ifAbsent: [#()]) remove: aPackageName ifAbsent: [  ] ] ].
	self conditionalPackageMapSpecs
		keysAndValuesDo: [ :platformPattern :packageMapSpecsMap | 
			packageMapSpecsMap
				keysAndValuesDo: [ :userId :packageMapSpecs | 
					(packageMapSpecs at: #'packageNameToPlatformPropertiesMap')
						removeKey: aPackageName
						ifAbsent: [  ] ] ]
]

{ #category : 'accessing' }
RwAbstractProjectLoadComponentV2 >> removeProjectNamed: aProjectName [
	"this can create empty projectName lists  ... the export logic _should_ cleanup empty list, which is sufficient"

	self conditionalProperties
		keysAndValuesDo: [ :conditionsArray :conditionMap | 
			conditionMap
				keysAndValuesDo: [ :groupName :propertiesMap | (propertiesMap at: #'projectNames' ifAbsent: [#()]) remove: aProjectName ifAbsent: [  ] ] ].
]
