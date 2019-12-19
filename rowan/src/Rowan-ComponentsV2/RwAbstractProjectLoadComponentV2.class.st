Class {
	#name : 'RwAbstractProjectLoadComponentV2',
	#superclass : 'Object',
	#instVars : [
		'name',
		'comment',
		'projectName',
		'componentNames',
		'conditionalPackages',
		'conditionalPackageMatchers',
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

{ #category : 'private' }
RwAbstractProjectLoadComponentV2 class >> _supportedPlatformNames [
	^ #('gemstone')
]

{ #category : 'instance creation' }
RwAbstractProjectLoadComponentV2 class >> fromUrl: specNameOrUrl [

	"self fromUrl: 'file:/home/dhenrich/rogue/_homes/rogue/_home/shared/repos/RowanSample1/configs/Default.ston'"

	| url |
	url := specNameOrUrl asRwUrl.
	url scheme isNil
		ifTrue: [ self error: 'scheme must be file: or https:' ].
	url scheme = 'file'
		ifTrue: [ 
			CypressFileUtilities current
				readStreamFor: url fileName
				in: url pathForDirectory
				do: [ :stream | ^ self _readStonFrom: stream ] ].
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

{ #category : 'exporting' }
RwAbstractProjectLoadComponentV2 >> _exportToUrl: fileUrl [

	| url |
	url := fileUrl asRwUrl.
	url schemeName = 'file'
		ifTrue: [ 
			Rowan fileUtilities
				writeStreamFor: self name , '.ston'
				in: url pathForDirectory
				do: [ :stream | 
					| string |
					string := STON toStringPretty: self.
					stream nextPutAll: string.
					^ self ] ].
  url schemeName = 'memory'
    ifTrue: [ 
		(FileSystem currentMemoryFileSystem workingDirectory / url pathForDirectory / self name , 'ston')
			writeStreamDo: [ :stream | 
			  | string |
			  string := STON toStringPretty: self.
			  stream nextPutAll: string.
			  ^ self ] ].
	^ nil	"otherwise a noop"
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
RwAbstractProjectLoadComponentV2 >> componentNames [
	^ componentNames ifNil: [ Array new ]
]

{ #category : 'accessing' }
RwAbstractProjectLoadComponentV2 >> componentNames: anArray [
	componentNames := anArray
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

{ #category : 'private' }
RwAbstractProjectLoadComponentV2 >> conditionalPackageMatchers [

	conditionalPackageMatchers ifNotNil: [:val | ^ val ]. 
	conditionalPackageMatchers := Dictionary new.
	self conditionalPackages keysAndValuesDo: [:platformPatterns :groupMap |
		conditionalPackageMatchers
			at: (platformPatterns collect: [:pattern | self _platformPatternMatcherFor: pattern ])
			put: groupMap ].
	^ conditionalPackageMatchers

]

{ #category : 'accessing' }
RwAbstractProjectLoadComponentV2 >> conditionalPackages [

	^ conditionalPackages ifNil: [ conditionalPackages := Dictionary new ]

]

{ #category : 'accessing' }
RwAbstractProjectLoadComponentV2 >> conditionalPackagesAtConditions: conditions andGroup: groupName [

	| thePackageNameMap |
	thePackageNameMap := (self conditionalPackages at: conditions asArray sort ifAbsent: [ ^ Set new])
		at: groupName ifAbsent: [ ^ Set new ].
	^ (thePackageNameMap at: #packageNames ifAbsent: [ Set new ]) asSet

]

{ #category : 'accessing' }
RwAbstractProjectLoadComponentV2 >> conditionalPackagesAtConditions: conditions andGroup: groupName addPackageNames: packageNames [

	| thePackageNames thePackageNameMap |
	thePackageNameMap := (self conditionalPackages at: conditions asArray sort ifAbsentPut: [ Dictionary new])
		at: groupName ifAbsentPut: [ Dictionary new ].
	thePackageNames := (thePackageNameMap at: #packageNames ifAbsentPut: [ Set new ]) asSet.
	thePackageNames addAll: packageNames.
	thePackageNameMap at: #packageNames put: thePackageNames asArray sort

]

{ #category : 'accessing' }
RwAbstractProjectLoadComponentV2 >> definedGroupNames [
	"Empty list by default"

	^ #()

]

{ #category : 'ston' }
RwAbstractProjectLoadComponentV2 >> excludedInstVars [
	^ #(#'conditionalPackageMatchers' #'conditionalPackageMapSpecMatchers')
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
RwAbstractProjectLoadComponentV2 >> exportToUrl: fileUrl [

	^ self copy
		initializeForExport
		_exportToUrl: fileUrl
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
	conditionalPackageMatchers := conditionalPackageMapSpecMatchers := nil.
	self conditionalPackages
		ifNotNil: [ :cp | 
			| orderedConditionalPackages |
			orderedConditionalPackages := Rowan platform orderedDictionaryClass new.
			(cp keys asSortedCollection: [ :a :b | (a at: 1) <= (b at: 1) ])
				do: [ :ar | 
					| dict orderedPackageNames |
					dict := cp at: ar.
					orderedPackageNames := Rowan platform orderedDictionaryClass new.
					dict keys asArray sort
						do: [ :group | orderedPackageNames at: group put: (dict at: group) ].
					orderedConditionalPackages at: ar put: orderedPackageNames ].
			conditionalPackages := orderedConditionalPackages ].
	conditionalPackageMapSpecs
		ifNotNil: [ 
			| orderedConditionalPackageMapSpecs |
			orderedConditionalPackageMapSpecs := Rowan platform orderedDictionaryClass
				new.
			(conditionalPackageMapSpecs keys asSortedCollection: [ :a :b | a <= b ])
				do: [ :platformName | 
					| orderedUserMap userMap |
					orderedUserMap := Rowan platform orderedDictionaryClass new.
					userMap := conditionalPackageMapSpecs at: platformName.
					(userMap keys asSortedCollection: [ :a :b | a <= b ])
						do: [ :userName | 
							| attributeMap orderedAttributeMap |
							attributeMap := userMap at: userName.
							orderedAttributeMap := Rowan platform orderedDictionaryClass new.
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
	self conditionalPackages
		keysAndValuesDo: [ :conditions :groupMap | 
			groupMap
				keysAndValuesDo: [ :groupName :packageMap | allDefinedPackageNames addAll: (packageMap at: #'packageNames') ] ].
	^ allDefinedPackageNames
]

{ #category : 'accessing' }
RwAbstractProjectLoadComponentV2 >> packageNamesForPlatformConfigurationAttributes: platformConfigurationAttributes groupNames: groupNames [
	"Answer the collection of package names defined in the receiver."

	| allDefinedPackageNames matchers |
	allDefinedPackageNames := Set new.
	matchers := self conditionalPackageMatchers.
	self conditionalPackages
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
RwAbstractProjectLoadComponentV2 >> removePackageNamed: aPackageName [
	"this can create empty packageName lists or empty packageMapSpecMaps ... the export logic _should_ cleanup empty list, which is sufficient"

	self conditionalPackages
		keysAndValuesDo: [ :conditionsArray :conditionMap | 
			conditionMap
				keysAndValuesDo: [ :groupName :groupMap | (groupMap at: #'packageNames') remove: aPackageName ifAbsent: [  ] ] ].
	self conditionalPackageMapSpecs
		keysAndValuesDo: [ :platformPattern :packageMapSpecsMap | 
			packageMapSpecsMap
				keysAndValuesDo: [ :userId :packageMapSpecs | 
					(packageMapSpecs at: #'packageNameToPlatformPropertiesMap')
						removeKey: aPackageName
						ifAbsent: [  ] ] ]
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
	self conditionalPackages
		keysAndValuesDo: [ :conditions :groupMap | 
			groupMap
				keysAndValuesDo: [ :groupName :packageMap | 
					(self definedGroupNames keys includes: groupName)
						ifFalse: [ 
							Error
								signal:
									'Conditional packages includes group name ' , groupName printString
										, ' that is not a defined group' ].
					allDefinedPackageNames addAll: (packageMap at: #'packageNames') ] ].
	self conditionalPackageMapSpecs
		keysAndValuesDo: [ :platformName :userIdMap | 
			(self class _supportedPlatformNames includes: platformName)
				ifFalse: [ 
					Error
						signal:
							'Unknown platform name ' , platformName printString
								, ' in conditional package map specs' ].
			platformName = 'gemstone'
				ifTrue: [ self _validateGemStonePlatform: allDefinedPackageNames userIdMap: userIdMap ] ].
	^ true
]
