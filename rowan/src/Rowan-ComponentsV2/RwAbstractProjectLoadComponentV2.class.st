Class {
	#name : 'RwAbstractProjectLoadComponentV2',
	#superclass : 'Object',
	#instVars : [
		'name',
		'comment',
		'projectName',
		'definedGroupNames',
		'conditionalProperties',
		'conditionalPropertyMatchers',
		'conditionalPackageMapSpecs',
		'conditionalPackageMapSpecMatchers'
	],
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'private' }
RwAbstractProjectLoadComponentV2 class >> _gemstoneSupportedPackagePropertyNames [
	^ #('methodEnv' 'symbolDictName' 'useSessionMethodsForExtensions')
]

{ #category : 'private' }
RwAbstractProjectLoadComponentV2 class >> _readStonFrom: stream [
	| reader component |
	(reader := STONReader on: stream) allowComplexMapKeys: true.
	component := reader next
		initializeForImport;
		yourself.
	component validate.	"validate when reading from disk, since hand editting could create inconsistencies"
	^ component
]

{ #category : 'instance creation' }
RwAbstractProjectLoadComponentV2 class >> fromUrl: specNameOrUrl [

	"self fromUrl: 'file:/home/dhenrich/rogue/_homes/rogue/_home/shared/repos/RowanSample1/configs/Default.ston'"

	| url |
	url := specNameOrUrl asRwUrl.
	url scheme isNil
		ifTrue: [ self error: 'scheme must be file: or https:' ].
	url scheme = 'file'
		ifTrue: [ ^ self fromFile: url pathForFile ].
	url scheme asString = 'https'
		ifTrue: [ 
self error: 'not yet supported'.
"
			| client response |
			GsSecureSocket disableCertificateVerificationOnClient.
			client := (Rowan globalNamed: 'ZnClient') new.
			response := client
				beOneShot;
				enforceHttpSuccess: true;
				get: url.
			^ self _readStonFrom: response decodeFromUTF8
" ].
	self error: 'Unknown scheme: ' , url scheme printString
]

{ #category : 'instance creation' }
RwAbstractProjectLoadComponentV2 class >> new [

	^self basicNew initialize

]

{ #category : 'instance creation' }
RwAbstractProjectLoadComponentV2 class >> newNamed: aName for: projectName [

	^ self new
		name: aName;
		projectName: projectName;
		yourself

]

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

{ #category : 'private' }
RwAbstractProjectLoadComponentV2 >> _platformPatternMatcherFor: pattern [

	" Returns an instance of RwAbstractConfigurationPlatformAttributeMatcher:
		RwStringConfigurationPlatformAttributeMatcher,
		RwGemStoneVersionConfigurationPlatformAttributeMatcher,
		or RwGemStoneVersionRangeConfigurationPlatformAttributeMatcher
	"

	| versionPattern gsVersion1 gsVersion2 |
	(pattern beginsWith: 'gs')
		ifFalse: [ 
			"simple equality match"
			^ RwStringConfigurationPlatformAttributeMatcher new
					pattern: pattern;
					patternMatchBlock: [:a :b | a = b ];
					yourself ].
	"GemStone version pattern"
	versionPattern := (pattern copyFrom: 3 to: pattern size) substrings: '.'.
	(versionPattern last beginsWith: '[')
		ifTrue: [ 
			| vpSize rangePattern dashIndex |
			"range pattern"
			vpSize := versionPattern size.
			gsVersion1 := RwGemStoneVersionNumber new: vpSize .
			1 to: vpSize - 1
				do: [:index | gsVersion1 at: index put: (versionPattern at: index) asInteger ].
			gsVersion1 at: vpSize put: 0.
			rangePattern := (versionPattern at: vpSize) trimBoth.
			(((rangePattern at: 1) = $[) and: [ (rangePattern at: rangePattern size) = $] ])
				ifFalse: [ self error: 'Poorly formed GemStone version range pattern ', rangePattern printString, ' in ', pattern printString ].
			rangePattern := rangePattern copyFrom: 2 to: rangePattern size -1.
			dashIndex := rangePattern indexOf: $-.
			dashIndex <= 1
				ifTrue: [ self error: 'Invalid version range pattern missing range begin' , rangePattern printString, ' in ', pattern printString ].
			gsVersion1 at: vpSize put: (rangePattern copyFrom: 1 to: dashIndex -1) asInteger.
			dashIndex = rangePattern size
				ifTrue: [
					"open range"
					gsVersion2 := gsVersion1 copyFrom: 1 to: gsVersion1 size -1.
					gsVersion2 at: gsVersion2 size put: (gsVersion2 at: gsVersion2 size) + 1.
					^ RwGemStoneVersionRangeConfigurationPlatformAttributeMatcher new
							pattern: gsVersion1;
							pattern2: gsVersion2;
							patternMatchBlock: [:a :b :c | (a <= b) & (b < c ) ];
							yourself ].
			"closed range"
			gsVersion2 := gsVersion1 copy.
			gsVersion2 at: vpSize put: (rangePattern copyFrom: dashIndex + 1 to: rangePattern size) asInteger.
			^ RwGemStoneVersionRangeConfigurationPlatformAttributeMatcher new
					pattern: gsVersion1;
					pattern2: gsVersion2;
					patternMatchBlock: [:a :b :c | (a <= b) & (b <= c ) ];
					yourself ].
	versionPattern last = 'x' 
		ifFalse: [
			"specific version number match, use ="
			^ RwGemStoneVersionConfigurationPlatformAttributeMatcher new
					pattern: (pattern copyFrom: 3 to: pattern size) asRwGemStoneVersionNumber;
					patternMatchBlock: [:a :b | a = b ];
					yourself ].
	" 'gs', <gemstone-version-number> , '.x'"
	"match all values in x field"
	gsVersion1 := ((pattern copyFrom: 3 to: pattern size - 2), '.0') asRwGemStoneVersionNumber.
	gsVersion2 := gsVersion1 copyFrom: 1 to: gsVersion1 size - 1.
	gsVersion2 at: gsVersion2 size put: (gsVersion2 at: gsVersion2 size) + 1.
	^ RwGemStoneVersionRangeConfigurationPlatformAttributeMatcher new
			pattern: gsVersion1;
			pattern2: gsVersion2;
			patternMatchBlock: [:a :b :c | (a <= b) & (b < c ) ];
			yourself

]

{ #category : 'validation' }
RwAbstractProjectLoadComponentV2 >> _validateGemStonePlatform: allDefinedPackageNames userIdMap: userIdMap [
	"ensure that the data structures within the receiver contain valid information:
		1. only packages defined in the receiver may be referenced in the reciever
		2. platform implementation is responsible for validating platform structures"

	userIdMap
		keysAndValuesDo: [ :userId :platformPropertiesMap | 
			platformPropertiesMap
				keysAndValuesDo: [ :key :packagePropertiesMap | 
					key == #'packageNameToPlatformPropertiesMap'
						ifFalse: [ Error signal: 'Unknown platformPropertiesMap key ' , key printString ].
					packagePropertiesMap
						keysAndValuesDo: [ :packageName :packageProperties | 
							(allDefinedPackageNames includes: packageName)
								ifFalse: [ 
									Error
										signal:
											'Undefined package name ' , packageName printString
												, ' used in plaform properties map' ].
							packageProperties
								keysDo: [ :packagePropertyName | 
									(self class _gemstoneSupportedPackagePropertyNames
										includes: packagePropertyName)
										ifFalse: [ Error signal: 'Unknown package property name ' , packagePropertyName printString ] ] ] ] ]
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
RwAbstractProjectLoadComponentV2 >> comment [

   ^comment

]

{ #category : 'initialization' }
RwAbstractProjectLoadComponentV2 >> comment: anObject [

   comment := anObject

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
RwAbstractProjectLoadComponentV2 >> conditionalPackageMapSpecs [

	^ conditionalPackageMapSpecs ifNil: [ conditionalPackageMapSpecs := Dictionary new ]

]

{ #category : 'accessing' }
RwAbstractProjectLoadComponentV2 >> conditionalPackageMapSpecsAtGemStoneUserId: userId [ 

	^ ((self conditionalPackageMapSpecs at: 'gemstone' ifAbsent: [ ^ Dictionary new ])
		at: userId ifAbsent: [ ^ Dictionary new ])

]

{ #category : 'accessing' }
RwAbstractProjectLoadComponentV2 >> conditionalPackageMapSpecsAtGemStoneUserId: userId andPackageName: packageName [

	^ (((self conditionalPackageMapSpecs at: 'gemstone' ifAbsent: [ ^ Dictionary new ])
		at: userId ifAbsentPut: [ ^ Dictionary new ])
			at: #packageNameToPlatformPropertiesMap ifAbsent: [ ^ Dictionary new ])
				at: packageName ifAbsent: [ ^ Dictionary new ]

]

{ #category : 'accessing' }
RwAbstractProjectLoadComponentV2 >> conditionalPackageMapSpecsAtGemStoneUserId: userId andPackageName: packageName setSymbolDictNameTo: symbolDictName [
	| dict |
	dict := (((self conditionalPackageMapSpecs
		at: 'gemstone'
		ifAbsentPut: [ Dictionary new ]) at: userId ifAbsentPut: [ Dictionary new ])
		at: #'packageNameToPlatformPropertiesMap'
		ifAbsentPut: [ Dictionary new ])
		at: packageName
		ifAbsentPut: [ Dictionary new ].
	symbolDictName
		ifNil: [ dict removeKey: 'symbolDictName' ifAbsent: [  ] ]
		ifNotNil: [ dict at: 'symbolDictName' put: symbolDictName asString ]
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
	^ #(#'conditionalPropertyMatchers' #'conditionalPackageMapSpecMatchers')
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

{ #category : 'exporting' }
RwAbstractProjectLoadComponentV2 >> exportToUrl: directoryUrl [

	^ self copy
		initializeForExport
		_exportToUrl: directoryUrl
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
RwAbstractProjectLoadComponentV2 >> initialize [
	comment := ''
]

{ #category : 'initialization' }
RwAbstractProjectLoadComponentV2 >> initializeForExport [
	"if spec is to be exported, clear out any of the fields that represent state that should 
	not be shared"

	"for export, the keys in the dictionaries of the structures need to be put into canonical order"

	projectName := nil.
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
			conditionalProperties := orderedConditionalProperties ].
	conditionalPackageMapSpecs
		ifNotNil: [ 
			| orderedConditionalPackageMapSpecs |
			orderedConditionalPackageMapSpecs := self class orderedDictionaryClass
				new.
			(conditionalPackageMapSpecs keys asSortedCollection: [ :a :b | a <= b ])
				do: [ :platformName | 
					| orderedUserMap userMap |
					orderedUserMap := self class orderedDictionaryClass new.
					userMap := conditionalPackageMapSpecs at: platformName.
					(userMap keys asSortedCollection: [ :a :b | a <= b ])
						do: [ :userName | 
							| attributeMap orderedAttributeMap |
							attributeMap := userMap at: userName.
							orderedAttributeMap := self class orderedDictionaryClass new.
							(attributeMap keys asSortedCollection: [ :a :b | a <= b ])
								do: [ :attributeName | orderedAttributeMap at: attributeName put: (attributeMap at: attributeName) ].
							orderedUserMap at: userName put: orderedAttributeMap ].
					orderedConditionalPackageMapSpecs at: platformName put: orderedUserMap ].
			conditionalPackageMapSpecs := orderedConditionalPackageMapSpecs ]
]

{ #category : 'initialization' }
RwAbstractProjectLoadComponentV2 >> initializeForImport [

	"if spec has been imported, clear out any of the fields that represent state that should 
	not be shared"

	projectName := nil

]

{ #category : 'accessing' }
RwAbstractProjectLoadComponentV2 >> name [

   ^name

]

{ #category : 'initialization' }
RwAbstractProjectLoadComponentV2 >> name: anObject [

   name := anObject

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
RwAbstractProjectLoadComponentV2 >> packageNamesForPlatformConfigurationAttributes: platformConfigurationAttributes groupNames: groupNames [
	"Answer the collection of package names defined in the receiver."

	| allDefinedPackageNames matchers |
	allDefinedPackageNames := Set new.
	matchers := self conditionalPropertyMatchers.
	self conditionalProperties
		keysAndValuesDo: [ :conditions :ignored | 
			platformConfigurationAttributes
				do: [ :anObject | 
					matchers
						keysAndValuesDo: [ :ar :groupMap | 
							ar
								do: [ :matcher | 
									(matcher match: anObject)
										ifTrue: [ 
											groupMap
												keysAndValuesDo: [ :groupName :packageMap | 
													(groupNames includes: groupName)
														ifTrue: [ allDefinedPackageNames addAll: (packageMap at: #'packageNames') ] ] ] ] ] ] ].
	^ allDefinedPackageNames
]

{ #category : 'accessing' }
RwAbstractProjectLoadComponentV2 >> projectName [

   ^projectName

]

{ #category : 'initialization' }
RwAbstractProjectLoadComponentV2 >> projectName: anObject [

   projectName := anObject

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

{ #category : 'ston' }
RwAbstractProjectLoadComponentV2 >> stonOn: stonWriter [   
	| instanceVariableNames |
	instanceVariableNames := self class allInstVarNames reject: [:iv | self excludedInstVars includes: iv].
	stonWriter writeObject: self
		streamMap: 
			[:dictionary |
			instanceVariableNames do: 
					[:each |
					(self instVarAt: (self class allInstVarNames indexOf: each asSymbol))
						ifNotNil: [:value | dictionary at: each asSymbol put: value]
						ifNil: [self stonShouldWriteNilInstVars ifTrue: [dictionary at: each asSymbol put: nil]]]]
]

{ #category : 'validation' }
RwAbstractProjectLoadComponentV2 >> validate [
	"ensure that the data structures within the receiver contain valid information:
		1. only packages defined in the receiver may be referenced in the reciever
		2. platform implementation is responsible for validating platform structures"

	| allDefinedPackageNames knownGroupNames |
	self name ifNil: [ self error: 'name is nil' ].
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
	self conditionalPackageMapSpecs
		keysAndValuesDo: [ :platformName :userIdMap | 
			(RwSpecification _supportedPlatformNames includes: platformName)
				ifFalse: [ 
					Error
						signal:
							'Unknown platform name ' , platformName printString
								, ' in conditional package map specs' ].
			platformName = 'gemstone'
				ifTrue: [ self _validateGemStonePlatform: allDefinedPackageNames userIdMap: userIdMap ] ].
	^ true
]
