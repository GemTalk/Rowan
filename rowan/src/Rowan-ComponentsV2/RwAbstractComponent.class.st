Class {
	#name : 'RwAbstractComponent',
	#superclass : 'Object',
	#instVars : [
		'name',
		'comment',
		'projectName',
		'conditionalPackageMapSpecs',
		'preloadDoitName',
		'postloadDoitName',
		'doitDict',
		'componentNames',
		'packageNames',
		'projectNames'
	],
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'private' }
RwAbstractComponent class >> _gemstoneSupportedPackagePropertyNames [
	^ #('methodEnv' 'symbolDictName' 'useSessionMethodsForExtensions')
]

{ #category : 'private' }
RwAbstractComponent class >> _readStonFrom: stream [
	| reader component |
	(reader := STONReader on: stream) allowComplexMapKeys: true.
	component := reader next
		initializeForImport;
		yourself.
	component validate.	"validate when reading from disk, since hand editting could create inconsistencies"
	^ component
]

{ #category : 'instance creation' }
RwAbstractComponent class >> fromComponentsDirectory: componentsDirectory named: componentName [
	| component url |
	url := 'file:' , (componentsDirectory / componentName , 'ston') pathString.
	component := self fromUrl: url.
	component _readDoitsFrom: componentsDirectory.
	component _validateDoits.
	^ component
]

{ #category : 'instance creation' }
RwAbstractComponent class >> fromUrl: specNameOrUrl [

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
RwAbstractComponent class >> new [

	^self basicNew initialize

]

{ #category : 'instance creation' }
RwAbstractComponent class >> newNamed: aName [

	^ self new
		name: aName;
		yourself
]

{ #category : 'instance creation' }
RwAbstractComponent class >> newNamed: aName for: projectName [
	"this method should not exist"

	^ (self newNamed: aName)
		projectName: projectName;
		yourself
]

{ #category : 'private' }
RwAbstractComponent >> _canonicalizeGemStonePackageMapSpecs: userMap [
	| orderedUserMap |
	orderedUserMap := self class orderedDictionaryClass new.
	(userMap keys asSortedCollection: [ :a :b | a <= b ])
		do: [ :userName | 
			| attributeMap orderedAttributeMap |
			attributeMap := userMap at: userName.
			orderedAttributeMap := self class orderedDictionaryClass new.
			(attributeMap keys asSortedCollection: [ :a :b | a <= b ])
				do: [ :attributeName | 
					| packageMap orderedPackageMap |
					packageMap := attributeMap at: attributeName.
					orderedPackageMap := self class orderedDictionaryClass new.
					(packageMap keys asSortedCollection: [ :a :b | a <= b ])
						do: [ :packageName | 
							(packageMap at: packageName) isEmpty
								ifFalse: [ orderedPackageMap at: packageName put: (packageMap at: packageName) ] ].
					orderedPackageMap isEmpty
						ifFalse: [ orderedAttributeMap at: attributeName put: orderedPackageMap ] ].
			orderedAttributeMap isEmpty
				ifFalse: [ orderedUserMap at: userName put: orderedAttributeMap ] ].
	^ orderedUserMap
]

{ #category : 'exporting' }
RwAbstractComponent >> _exportToUrl: directoryUrl [
	| url |
	url := directoryUrl asRwUrl.
	url schemeName = 'file'
		ifTrue: [ 
			| fileRef |
			fileRef := url pathForDirectory asFileReference / self name , 'ston'.
			fileRef parent ensureCreateDirectory.
			fileRef
				writeStreamDo: [ :stream | 
					| string |
					string := STON toStringPretty: self.
					stream nextPutAll: string.
					^ self ] ].
	url schemeName = 'memory'
		ifTrue: [ 
			FileSystem currentMemoryFileSystem workingDirectory / url pathForDirectory
				/ self name , 'ston'
				writeStreamDo: [ :stream | 
					| string |
					string := STON toStringPretty: self.
					stream nextPutAll: string.
					^ self ] ].
	^ nil	"otherwise a noop"
]

{ #category : 'matching' }
RwAbstractComponent >> _platformAttributeMatchIn: platformMatchersList for: attributes [
	platformMatchersList
		do: [ :platformPatternMatcher | 
			attributes
				do: [ :anObject | 
					(platformPatternMatcher match: anObject)
						ifTrue: [ ^ true ] ] ].
	^ false
]

{ #category : 'private' }
RwAbstractComponent >> _platformPatternMatcherFor: pattern [

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

{ #category : 'doits' }
RwAbstractComponent >> _readDoitsFrom: componentsRoot [
	| fileRef |
	preloadDoitName
		ifNotNil: [ 
			fileRef := componentsRoot / preloadDoitName , 'st'.
			fileRef exists
				ifTrue: [ 
					fileRef
						readStreamDo: [ :fileStream | self doitDict at: preloadDoitName put: fileStream contents ] ]
				ifFalse: [ self error: '"no doit file ' , preloadDoitName printString , ' found"' ] ].
	postloadDoitName
		ifNotNil: [ 
			fileRef := componentsRoot / postloadDoitName , 'st'.

			fileRef exists
				ifTrue: [ 
					fileRef
						readStreamDo: [ :fileStream | self doitDict at: postloadDoitName put: fileStream contents ] ]
				ifFalse: [ self error: '"no doit file ' , postloadDoitName printString , ' found"' ] ]
]

{ #category : 'validation' }
RwAbstractComponent >> _validateDoits [
	self preloadDoitName
		ifNotNil: [ 
			((self doitDict
				at: self preloadDoitName
				ifAbsent: [ 
					self
						error:
							'Expected doit for ' , self preloadDoitName printString , ' to exist.' ])
				isKindOf: CharacterCollection)
				ifFalse: [ 
					self
						error:
							'Doit for ' , self preloadDoitName printString , ' is not a CharacterCollection' ] ].
	self postloadDoitName
		ifNotNil: [ 
			((self doitDict
				at: self postloadDoitName
				ifAbsent: [ 
					self
						error:
							'Expected doit for ' , self postloadDoitName printString , ' to exist.' ])
				isKindOf: CharacterCollection)
				ifFalse: [ 
					self
						error:
							'Doit for ' , self postloadDoitName printString
								, ' is not a CharacterCollection' ] ]
]

{ #category : 'validation' }
RwAbstractComponent >> _validatedPackageNames [
	"answer the validated set of package names"

	self subclassResponsibility: #'_validatedPackageNames'
]

{ #category : 'validation' }
RwAbstractComponent >> _validateGemStonePlatform: allDefinedPackageNames userIdMap: userIdMap [
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

{ #category : 'comparing' }
RwAbstractComponent >> = aRwProjectLoadComponentV2 [
	(aRwProjectLoadComponentV2 isKindOf: self class)
		ifFalse: [ ^ false ].
	^ ((((self name = aRwProjectLoadComponentV2 name
		and: [ self comment = aRwProjectLoadComponentV2 comment ])
		and: [ 
			self conditionalPackageMapSpecs
				= aRwProjectLoadComponentV2 conditionalPackageMapSpecs ])
		and: [ self condition = aRwProjectLoadComponentV2 condition ])
		and: [ self packageNames = aRwProjectLoadComponentV2 packageNames ])
		and: [ self componentNames = aRwProjectLoadComponentV2 componentNames ]
]

{ #category : 'accessing' }
RwAbstractComponent >> addComponentNamed: aComponentName [ 
	"avoid duplicates and keep the list sorted"
	self componentNames add: aComponentName.
	componentNames := componentNames asSet asArray sort
]

{ #category : 'accessing' }
RwAbstractComponent >> addComponentNames: aComponentNames [
	"add component names"

	self componentNames addAll: aComponentNames.
	componentNames := componentNames asSet asArray sort
]

{ #category : 'accessing' }
RwAbstractComponent >> addPackageNames: aPackageNames [
	"add packages to default conditional/group if applicable"

	self packageNames addAll: aPackageNames.
	packageNames := packageNames asSet asArray sort
]

{ #category : 'accessing' }
RwAbstractComponent >> addProjectNamed: aProjectName [
	self subclassResponsibility: #'addProjectNamed:'
]

{ #category : 'accessing' }
RwAbstractComponent >> basename [
	^ (self name subStrings: $/) last
]

{ #category : 'accessing' }
RwAbstractComponent >> comment [

   ^comment

]

{ #category : 'initialization' }
RwAbstractComponent >> comment: anObject [

   comment := anObject

]

{ #category : 'accessing' }
RwAbstractComponent >> componentNames [

	^ componentNames
]

{ #category : 'accessing' }
RwAbstractComponent >> conditionalPackageMapSpecs [

	^ conditionalPackageMapSpecs ifNil: [ conditionalPackageMapSpecs := Dictionary new ]

]

{ #category : 'accessing' }
RwAbstractComponent >> conditionalPackageMapSpecs: aDictionary [
	conditionalPackageMapSpecs := aDictionary
]

{ #category : 'accessing' }
RwAbstractComponent >> conditionalPackageMapSpecsAt: key ifAbsent: absentBlock [
	conditionalPackageMapSpecs ifNil: [ ^ absentBlock value ].

	^ conditionalPackageMapSpecs at: key ifAbsent: absentBlock
]

{ #category : 'accessing' }
RwAbstractComponent >> conditionalPackageMapSpecsAtGemStoneUserId: userId [ 

	^ ((self conditionalPackageMapSpecs at: 'gemstone' ifAbsent: [ ^ Dictionary new ])
		at: userId ifAbsent: [ ^ Dictionary new ])

]

{ #category : 'accessing' }
RwAbstractComponent >> conditionalPackageMapSpecsAtGemStoneUserId: userId andPackageName: packageName setSymbolDictNameTo: symbolDictName [
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
RwAbstractComponent >> conditionalPackageMapSpecsAtGemStoneUserId: userId andPackageName: packageName setUseSessionMethodsForExtensions: aBool [
	| dict |
	dict := (((self conditionalPackageMapSpecs
		at: 'gemstone'
		ifAbsentPut: [ Dictionary new ]) at: userId ifAbsentPut: [ Dictionary new ])
		at: #'packageNameToPlatformPropertiesMap'
		ifAbsentPut: [ Dictionary new ])
		at: packageName
		ifAbsentPut: [ Dictionary new ].
	aBool
		ifNil: [ dict removeKey: 'useSessionMethodsForExtensions' ifAbsent: [  ] ]
		ifNotNil: [ dict at: 'useSessionMethodsForExtensions' put: aBool ]
]

{ #category : 'private' }
RwAbstractComponent >> conditionalPropertyMatchers [
	self subclassResponsibility: #'conditionalPropertyMatchers'
]

{ #category : 'accessing' }
RwAbstractComponent >> doitDict [
	^doitDict
]

{ #category : 'accessing' }
RwAbstractComponent >> doitDict: object [
	doitDict := object
]

{ #category : 'ston' }
RwAbstractComponent >> excludedInstVars [
	^ #()
]

{ #category : 'doits' }
RwAbstractComponent >> executePostloadDoit [
	self postloadDoitName
		ifNotNil: [ (self doitDict at: self postloadDoitName) evaluate ]
]

{ #category : 'doits' }
RwAbstractComponent >> executePreloadDoit [
	self preloadDoitName
		ifNotNil: [ (self doitDict at: self preloadDoitName) evaluate ]
]

{ #category : 'exporting' }
RwAbstractComponent >> exportDoitsToUrl: directoryUrl [
	| doitsRoot url |
	url := directoryUrl asRwUrl.
	url schemeName = 'file'
		ifTrue: [ 
			doitsRoot := url pathForDirectory asFileReference.
			{(self preloadDoitName).
			(self postloadDoitName)}
				do: [ :doitName | 
					doitName
						ifNotNil: [ 
							(self doitDict at: doitName ifAbsent: [  ])
								ifNotNil: [ :doitString | 
									| fileRef |
									fileRef := doitsRoot / doitName , 'st'.
									fileRef exists
										ifFalse: [ fileRef ensureCreateFile ].
									fileRef
										writeStreamDo: [ :fileStream | fileStream nextPutAll: doitString ] ] ] ] ]
		ifFalse: [ self error: 'unsupported url scheme ' , url schemeName printString ]
]

{ #category : 'exporting' }
RwAbstractComponent >> exportToUrl: directoryUrl [
	self exportDoitsToUrl: directoryUrl.
	^ self copy initializeForExport
		_exportToUrl: directoryUrl;
		yourself
]

{ #category : 'testing' }
RwAbstractComponent >> hasDoits [
	^ preloadDoitName notNil or: [ postloadDoitName notNil ]
]

{ #category : 'comparing' }
RwAbstractComponent >> hash [
	| hashValue |
	hashValue := self name hash.
	hashValue := hashValue bitXor: self comment hash.
	hashValue := hashValue bitXor: self conditionalPackageMapSpecs hash.
	hashValue := hashValue bitXor: self packageNames hash.
	^ hashValue bitXor: self componentNames hash
]

{ #category : 'initialization' }
RwAbstractComponent >> initialize [
	comment := ''.
	doitDict := Dictionary new.
	packageNames := {}.
	componentNames := {}
]

{ #category : 'initialization' }
RwAbstractComponent >> initializeForExport [
	"if spec is to be exported, clear out any of the fields that represent state that should 
	not be shared"

	"for export, the keys in the dictionaries of the structures need to be put into canonical order"

	doitDict := projectName := nil.
	conditionalPackageMapSpecs
		ifNotNil: [ 
			conditionalPackageMapSpecs isEmpty
				ifTrue: [ 
					"don't export this field if it is empty ..."
					conditionalPackageMapSpecs := nil ]
				ifFalse: [ 
					| orderedConditionalPackageMapSpecs |
					orderedConditionalPackageMapSpecs := self class orderedDictionaryClass new.
					(conditionalPackageMapSpecs keys asSortedCollection: [ :a :b | a <= b ])
						do: [ :platformName | 
							| orderedPlatformMap platformMap |
							platformMap := conditionalPackageMapSpecs at: platformName.
							orderedPlatformMap := platformName = 'gemstone'
								ifTrue: [ self _canonicalizeGemStonePackageMapSpecs: platformMap ]
								ifFalse: [ self error: 'Unknown platform name ' , platformName printString ].
							orderedPlatformMap isEmpty
								ifFalse: [ orderedConditionalPackageMapSpecs at: platformName put: orderedPlatformMap ] ].
					conditionalPackageMapSpecs := orderedConditionalPackageMapSpecs ] ]
]

{ #category : 'initialization' }
RwAbstractComponent >> initializeForImport [

	"if spec has been imported, clear out any of the fields that represent state that should 
	not be shared"

	projectName := nil

]

{ #category : 'ston' }
RwAbstractComponent >> instVarNamesInOrderForSton [

	^ self class allInstVarNames
]

{ #category : 'matching' }
RwAbstractComponent >> matchesAttributes: attributes [
	self conditionalPropertyMatchers
		keysAndValuesDo: [ :platformMatchers :ignored | ^ self _platformAttributeMatchIn: platformMatchers for: attributes ]
]

{ #category : 'accessing' }
RwAbstractComponent >> name [

   ^name

]

{ #category : 'initialization' }
RwAbstractComponent >> name: anObject [

   name := anObject

]

{ #category : 'accessing' }
RwAbstractComponent >> packageNames [

	^ packageNames
]

{ #category : 'accessing' }
RwAbstractComponent >> packageNamesForPlatformConfigurationAttributes: platformConfigurationAttributes [
	"Answer the collection of package names defined in the receiver."

	| allDefinedPackageNames matchers |
	allDefinedPackageNames := Set new.
	matchers := self conditionalPropertyMatchers.
	platformConfigurationAttributes
		do: [ :anObject | 
			matchers
				keysAndValuesDo: [ :ar :ignored | 
					ar
						do: [ :matcher | 
							(matcher match: anObject)
								ifTrue: [ allDefinedPackageNames addAll: self packageNames ] ] ] ].
	^ allDefinedPackageNames
]

{ #category : 'accessing' }
RwAbstractComponent >> postloadDoitName [
	^postloadDoitName
]

{ #category : 'accessing' }
RwAbstractComponent >> postloadDoitName: object [
	postloadDoitName := object
]

{ #category : 'accessing' }
RwAbstractComponent >> preloadDoitName [
	^preloadDoitName
]

{ #category : 'accessing' }
RwAbstractComponent >> preloadDoitName: object [
	preloadDoitName := object
]

{ #category : 'printing' }
RwAbstractComponent >> printOn: aStream [
	super printOn: aStream.
	aStream
		space;
		nextPutAll: name.
	projectName ifNotNil: [ aStream nextPutAll: ' for project ' , projectName ]
]

{ #category : 'accessing' }
RwAbstractComponent >> projectName [

   ^projectName

]

{ #category : 'initialization' }
RwAbstractComponent >> projectName: anObject [

   projectName := anObject

]

{ #category : 'accessing' }
RwAbstractComponent >> referencePath [
	^ Path from: self name
]

{ #category : 'accessing' }
RwAbstractComponent >> removeComponentNamed: aComponentName [
	self componentNames remove: aComponentName ifAbsent: [  ]
]

{ #category : 'accessing' }
RwAbstractComponent >> removePackageNamed: aPackageName [
	self conditionalPackageMapSpecs
		keysAndValuesDo: [ :platformPattern :packageMapSpecsMap | 
			packageMapSpecsMap
				keysAndValuesDo: [ :userId :packageMapSpecs | 
					(packageMapSpecs at: #'packageNameToPlatformPropertiesMap')
						removeKey: aPackageName
						ifAbsent: [  ] ] ].
	self packageNames remove: aPackageName ifAbsent: [  ]
]

{ #category : 'accessing' }
RwAbstractComponent >> removeProjectNamed: aProjectName [
	self subclassResponsibility: #'removeProjectNamed:'
]

{ #category : 'ston' }
RwAbstractComponent >> stonOn: stonWriter [
	| instanceVariableNames allInstanceVariableNames |
	instanceVariableNames := self instVarNamesInOrderForSton
		reject: [ :iv | self excludedInstVars includes: iv ].
	allInstanceVariableNames := self class allInstVarNames.
	stonWriter
		writeObject: self
		streamMap: [ :dictionary | 
			instanceVariableNames
				do: [ :each | 
					(self instVarAt: (allInstanceVariableNames indexOf: each asSymbol))
						ifNotNil: [ :value | dictionary at: each asSymbol put: value ]
						ifNil: [ 
							self stonShouldWriteNilInstVars
								ifTrue: [ dictionary at: each asSymbol put: nil ] ] ] ]
]

{ #category : 'validation' }
RwAbstractComponent >> validate [
	"ensure that the data structures within the receiver contain valid information:
		1. only packages defined in the receiver may be referenced in the reciever
		2. platform implementation is responsible for validating platform structures"

	| allDefinedPackageNames |
	self name ifNil: [ self error: 'name is nil' ].
	allDefinedPackageNames := self _validatedPackageNames.
	self conditionalPackageMapSpecs
		keysAndValuesDo: [ :platformName :platformPropertiesMap | 
			(RwSpecification _supportedPlatformNames includes: platformName)
				ifFalse: [ 
					Error
						signal:
							'Unknown platform name ' , platformName printString
								, ' in conditional package map specs' ].
			platformName = 'gemstone'
				ifTrue: [ self _validateGemStonePlatform: allDefinedPackageNames userIdMap: platformPropertiesMap ] ].
	^ true
]
