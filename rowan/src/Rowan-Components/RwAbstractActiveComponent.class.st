Class {
	#name : 'RwAbstractActiveComponent',
	#superclass : 'RwAbstractComponent',
	#instVars : [
		'conditionalPackageMapSpecs',
		'preloadDoitName',
		'postloadDoitName',
		'doitDict',
		'projectNames'
	],
	#category : 'Rowan-Components'
}

{ #category : 'private' }
RwAbstractActiveComponent class >> _gemstoneSupportedPackagePropertyNames [
	^ #('methodEnv' 'symbolDictName' 'useSessionMethodsForExtensions')
]

{ #category : 'dispatching' }
RwAbstractActiveComponent >> _addToResolvedProjectComponents: aRwResolvedProjectComponentsV2 [
	"dispatch to _addActiveComponent: or _addPackageGroup: as appropriate"

	aRwResolvedProjectComponentsV2 _addActiveComponent: self
]

{ #category : 'private' }
RwAbstractActiveComponent >> _canonicalizeGemStonePackageMapSpecs: userMap [
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

{ #category : 'doits' }
RwAbstractActiveComponent >> _readDoitsFrom: componentsRoot [
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
RwAbstractActiveComponent >> _validateDoits [
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
RwAbstractActiveComponent >> _validateGemStonePlatform: allDefinedPackageNames userIdMap: userIdMap [
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
RwAbstractActiveComponent >> = aRwProjectLoadComponentV2 [
	super = aRwProjectLoadComponentV2
		ifFalse: [ ^ false ].
	^ (((self conditionalPackageMapSpecs
		= aRwProjectLoadComponentV2 conditionalPackageMapSpecs
		and: [ self doitDict = aRwProjectLoadComponentV2 doitDict ])
		and: [ self projectNames = aRwProjectLoadComponentV2 projectNames ])
		and: [ self postloadDoitName = aRwProjectLoadComponentV2 postloadDoitName ])
		and: [ self preloadDoitName = aRwProjectLoadComponentV2 preloadDoitName ]
]

{ #category : 'accessing' }
RwAbstractActiveComponent >> addProjectNamed: aProjectName [
	| pn |
	pn := self projectNames.	" returns copy, so cannot add directly to result"
	pn add: aProjectName.
	projectNames := pn asSet asArray sort
]

{ #category : 'accessing' }
RwAbstractActiveComponent >> conditionalPackageMapSpecs [

	^ conditionalPackageMapSpecs ifNil: [ conditionalPackageMapSpecs := Dictionary new ]

]

{ #category : 'accessing' }
RwAbstractActiveComponent >> conditionalPackageMapSpecsAtGemStoneUserId: userId [
	| userIdPropertiesMap |
	userIdPropertiesMap := self conditionalPackageMapSpecs
		at: 'gemstone'
		ifAbsent: [ ^ Dictionary new ].
	^ userIdPropertiesMap
		at: userId
		ifAbsent: [ 
			userIdPropertiesMap
				at: RwLoadSpecificationV2 _gemstoneAllUsersName
				ifAbsent: [ ^ Dictionary new ] ]
]

{ #category : 'accessing' }
RwAbstractActiveComponent >> conditionalPackageMapSpecsAtGemStoneUserId: userId andPackageName: packageName setSymbolDictNameTo: symbolDictName [
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
RwAbstractActiveComponent >> conditionalPackageMapSpecsAtGemStoneUserId: userId andPackageName: packageName setUseSessionMethodsForExtensions: aBool [
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
RwAbstractActiveComponent >> conditionalPropertyMatchers [
	self subclassResponsibility: #'conditionalPropertyMatchers'
]

{ #category : 'accessing' }
RwAbstractActiveComponent >> doitDict [
	^doitDict
]

{ #category : 'accessing' }
RwAbstractActiveComponent >> doitDict: object [
	doitDict := object
]

{ #category : 'doits' }
RwAbstractActiveComponent >> executePostloadDoit [
	self postloadDoitName
		ifNotNil: [ (self doitDict at: self postloadDoitName) evaluate ]
]

{ #category : 'doits' }
RwAbstractActiveComponent >> executePreloadDoit [
	self preloadDoitName
		ifNotNil: [ (self doitDict at: self preloadDoitName) evaluate ]
]

{ #category : 'exporting' }
RwAbstractActiveComponent >> exportDoitsToUrl: directoryUrl [
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
RwAbstractActiveComponent >> exportToUrl: directoryUrl [
	self exportDoitsToUrl: directoryUrl.
	^ super exportToUrl: directoryUrl
]

{ #category : 'testing' }
RwAbstractActiveComponent >> hasDoits [
	^ preloadDoitName notNil or: [ postloadDoitName notNil ]
]

{ #category : 'comparing' }
RwAbstractActiveComponent >> hash [
	| hashValue |
	hashValue := self name hash.
	hashValue := hashValue bitXor: self comment hash.
	hashValue := hashValue bitXor: self conditionalPackageMapSpecs hash.
	hashValue := hashValue bitXor: self packageNames hash.
	^ hashValue bitXor: self componentNames hash
]

{ #category : 'initialization' }
RwAbstractActiveComponent >> initialize [
	super initialize.
	doitDict := Dictionary new.
	projectNames := {}
]

{ #category : 'initialization' }
RwAbstractActiveComponent >> initializeForExport [
	"if spec is to be exported, clear out any of the fields that represent state that should 
	not be shared"

	"for export, the keys in the dictionaries of the structures need to be put into canonical order"

	super initializeForExport.
	doitDict := nil.
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

{ #category : 'ston' }
RwAbstractActiveComponent >> instVarNamesInOrderForSton [
	^ #(#'name' #'preloadDoitName' #'postloadDoitName' #'projectNames' #'componentNames' #'packageNames' #'conditionalPackageMapSpecs' #'comment')
]

{ #category : 'accessing' }
RwAbstractActiveComponent >> packageNamesForConditionalAttributes: conditionalAttributes [
	"Answer the collection of package names defined in the receiver."

	| allDefinedPackageNames matchers |
	allDefinedPackageNames := Set new.
	matchers := self conditionalPropertyMatchers.
	conditionalAttributes
		do: [ :anObject | 
			matchers
				keysAndValuesDo: [ :ar :ignored | 
					ar
						do: [ :matcher | 
							(matcher match: anObject)
								ifTrue: [ allDefinedPackageNames addAll: self packageNames ] ] ] ].
	^ allDefinedPackageNames
]

{ #category : 'copying' }
RwAbstractActiveComponent >> postCopy [
	super postCopy.
	projectNames := projectNames copy.
	doitDict := doitDict copy.
	conditionalPackageMapSpecs
		ifNotNil: [ 
			| packageMapSpecsCopy |
			packageMapSpecsCopy := conditionalPackageMapSpecs copy.
			conditionalPackageMapSpecs
				keysAndValuesDo: [ :platformName :userMap | 
					platformName = 'gemstone'
						ifTrue: [ 
							| userMapCopy |
							userMapCopy := userMap copy.
							packageMapSpecsCopy at: platformName put: userMapCopy.
							userMap
								keysAndValuesDo: [ :userName :attributeMap | 
									| attributeMapCopy |
									attributeMapCopy := attributeMap copy.
									userMapCopy at: userName put: attributeMapCopy.
									attributeMap
										keysAndValuesDo: [ :attributeName :packageMap | 
											| packageMapCopy |
											packageMapCopy := packageMap copy.
											attributeMapCopy at: attributeName put: packageMapCopy.
											packageMap
												keysAndValuesDo: [ :packageName :packageAttribute | packageMapCopy at: packageName put: packageAttribute copy ] ] ] ]
						ifFalse: [ self error: 'Unknown platform name ' , platformName printString ] ].
			conditionalPackageMapSpecs := packageMapSpecsCopy ]
]

{ #category : 'accessing' }
RwAbstractActiveComponent >> postloadDoitName [
	^postloadDoitName
]

{ #category : 'accessing' }
RwAbstractActiveComponent >> postloadDoitName: object [
	postloadDoitName := object
]

{ #category : 'accessing' }
RwAbstractActiveComponent >> preloadDoitName [
	^preloadDoitName
]

{ #category : 'accessing' }
RwAbstractActiveComponent >> preloadDoitName: object [
	preloadDoitName := object
]

{ #category : 'accessing' }
RwAbstractActiveComponent >> projectNames [
	^ projectNames copy
]

{ #category : 'accessing' }
RwAbstractActiveComponent >> removePackageNamed: aPackageName [
	self conditionalPackageMapSpecs
		keysAndValuesDo: [ :platformPattern :packageMapSpecsMap | 
			packageMapSpecsMap
				keysAndValuesDo: [ :userId :packageMapSpecs | 
					(packageMapSpecs at: #'packageNameToPlatformPropertiesMap')
						removeKey: aPackageName
						ifAbsent: [  ] ] ].
	super removePackageNamed: aPackageName
]

{ #category : 'accessing' }
RwAbstractActiveComponent >> removeProjectNamed: aProjectName [
	self subclassResponsibility: #'removeProjectNamed:'
]

{ #category : 'accessing' }
RwAbstractActiveComponent >> renameTo: aString in: aResolvedProject [
	"change the basename of aComponentPath to <baseName>, i.e., the path is not changed"

	"need to change all the references, so not enough to just change my name"

	^ aResolvedProject renameComponentNamed: self name to: aString
]

{ #category : 'validation' }
RwAbstractActiveComponent >> validate [
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
