Class {
	#name : 'RwBasicProjectLoadComponentV2',
	#superclass : 'Object',
	#instVars : [
		'name',
		'comment',
		'projectName',
		'conditionalPackageMapSpecs',
		'preloadDoitName',
		'postloadDoitName'
	],
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'private' }
RwBasicProjectLoadComponentV2 class >> _gemstoneSupportedPackagePropertyNames [
	^ #('methodEnv' 'symbolDictName' 'useSessionMethodsForExtensions')
]

{ #category : 'private' }
RwBasicProjectLoadComponentV2 class >> _readStonFrom: stream [
	| reader component |
	(reader := STONReader on: stream) allowComplexMapKeys: true.
	component := reader next
		initializeForImport;
		yourself.
	component validate.	"validate when reading from disk, since hand editting could create inconsistencies"
	^ component
]

{ #category : 'instance creation' }
RwBasicProjectLoadComponentV2 class >> fromUrl: specNameOrUrl [

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
RwBasicProjectLoadComponentV2 class >> new [

	^self basicNew initialize

]

{ #category : 'instance creation' }
RwBasicProjectLoadComponentV2 class >> newNamed: aName [

	^ self new
		name: aName;
		yourself
]

{ #category : 'instance creation' }
RwBasicProjectLoadComponentV2 class >> newNamed: aName for: projectName [
	"this method should not exist"

	^ (self newNamed: aName)
		projectName: projectName;
		yourself
]

{ #category : 'private' }
RwBasicProjectLoadComponentV2 >> _canonicalizeGemStonePackageMapSpecs: userMap [
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

{ #category : 'private' }
RwBasicProjectLoadComponentV2 >> _canonicalizeVastPackageMapSpecs: propertiesMap [
	| orderedPropertyMap |
	orderedPropertyMap := self class orderedDictionaryClass new.
	(propertiesMap keys asSortedCollection: [ :a :b | a <= b ])
		do: [ :vaPropertyName | 
			propertiesMap at: vaPropertyName.
			propertiesMap isEmpty
				ifFalse: [ orderedPropertyMap at: vaPropertyName put: (propertiesMap at: vaPropertyName) ] ].
	^ orderedPropertyMap
]

{ #category : 'validation' }
RwBasicProjectLoadComponentV2 >> _validatedPackageNames [
	"answer the validated set of package names"

	self subclassResponsibility: #'_validatedPackageNames'
]

{ #category : 'accessing' }
RwBasicProjectLoadComponentV2 >> addComponentNamed: aComponentName [
	self subclassResponsibility: #'addComponentNamed:'
]

{ #category : 'accessing' }
RwBasicProjectLoadComponentV2 >> addPackageNames: aPackageNames [
	"add packages to default conditional/group if applicable"

	self subclassResponsibility: #'addPackageNames:'
]

{ #category : 'accessing' }
RwBasicProjectLoadComponentV2 >> addProjectNamed: aProjectName [
	self subclassResponsibility: #'addProjectNamed:'
]

{ #category : 'accessing' }
RwBasicProjectLoadComponentV2 >> comment [

   ^comment

]

{ #category : 'initialization' }
RwBasicProjectLoadComponentV2 >> comment: anObject [

   comment := anObject

]

{ #category : 'accessing' }
RwBasicProjectLoadComponentV2 >> conditionalPackageMapSpecs [

	^ conditionalPackageMapSpecs ifNil: [ conditionalPackageMapSpecs := Dictionary new ]

]

{ #category : 'accessing' }
RwBasicProjectLoadComponentV2 >> conditionalPackageMapSpecsAt: key ifAbsent: absentBlock [
	conditionalPackageMapSpecs ifNil: [ ^ absentBlock value ].

	^ conditionalPackageMapSpecs at: key ifAbsent: absentBlock
]

{ #category : 'accessing' }
RwBasicProjectLoadComponentV2 >> conditionalPackageMapSpecsAtGemStoneUserId: userId [ 

	^ ((self conditionalPackageMapSpecs at: 'gemstone' ifAbsent: [ ^ Dictionary new ])
		at: userId ifAbsent: [ ^ Dictionary new ])

]

{ #category : 'accessing' }
RwBasicProjectLoadComponentV2 >> conditionalPackageMapSpecsAtGemStoneUserId: userId andPackageName: packageName [

	^ (((self conditionalPackageMapSpecs at: 'gemstone' ifAbsent: [ ^ Dictionary new ])
		at: userId ifAbsentPut: [ ^ Dictionary new ])
			at: #packageNameToPlatformPropertiesMap ifAbsent: [ ^ Dictionary new ])
				at: packageName ifAbsent: [ ^ Dictionary new ]

]

{ #category : 'accessing' }
RwBasicProjectLoadComponentV2 >> conditionalPackageMapSpecsAtGemStoneUserId: userId andPackageName: packageName setSymbolDictNameTo: symbolDictName [
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
RwBasicProjectLoadComponentV2 >> conditionalPackageMapSpecsAtPackageName: packageName setVaPrequisitesTo: prereqsArray [
	| packageDict propertiesDict prereqs |
	packageDict := self conditionalPackageMapSpecs
		at: 'vast'
		ifAbsentPut: [ Dictionary new ].

	propertiesDict := packageDict at: packageName ifAbsentPut: [ Dictionary new ].

	prereqs := propertiesDict at: 'vaPrerequisites' ifAbsent: [ {} ].
	prereqs addAll: prereqsArray.

	propertiesDict at: 'vaPrerequisites' put: prereqs asSet asArray
]

{ #category : 'accessing' }
RwBasicProjectLoadComponentV2 >> conditionalPackageMapSpecsAtPackageName: packageName setVaSubApplicationsTo: subApplicationsArray [
	| packageDict propertiesDict subApplications |
	packageDict := self conditionalPackageMapSpecs
		at: 'vast'
		ifAbsentPut: [ Dictionary new ].

	propertiesDict := packageDict at: packageName ifAbsentPut: [ Dictionary new ].

	subApplications := propertiesDict at: 'vaSubApplications' ifAbsentPut: [ {} ].
	subApplicationsArray do: [ :value | subApplications add: value ]
]

{ #category : 'ston' }
RwBasicProjectLoadComponentV2 >> excludedInstVars [
	^ #()
]

{ #category : 'doits' }
RwBasicProjectLoadComponentV2 >> executePostloadDoit [
	self halt: 'not yet implemented'
]

{ #category : 'doits' }
RwBasicProjectLoadComponentV2 >> executePreloadDoit [
	self halt: 'not yet implemented'
]

{ #category : 'exporting' }
RwBasicProjectLoadComponentV2 >> exportToUrl: directoryUrl [

	^ self copy
		initializeForExport
		_exportToUrl: directoryUrl
]

{ #category : 'testing' }
RwBasicProjectLoadComponentV2 >> hasDoits [
	^ preloadDoitName notNil and: [ postloadDoitName notNil ]
]

{ #category : 'initialization' }
RwBasicProjectLoadComponentV2 >> initialize [
	comment := ''
]

{ #category : 'initialization' }
RwBasicProjectLoadComponentV2 >> initializeForExport [
	"if spec is to be exported, clear out any of the fields that represent state that should 
	not be shared"

	"for export, the keys in the dictionaries of the structures need to be put into canonical order"

	projectName := nil.
	conditionalPackageMapSpecs
		ifNotNil: [ 
			| orderedConditionalPackageMapSpecs |
			orderedConditionalPackageMapSpecs := self class orderedDictionaryClass new.
			(conditionalPackageMapSpecs keys asSortedCollection: [ :a :b | a <= b ])
				do: [ :platformName | 
					| orderedPlatformMap platformMap |
					platformMap := conditionalPackageMapSpecs at: platformName.
					orderedPlatformMap := platformName = 'gemstone'
						ifTrue: [ self _canonicalizeGemStonePackageMapSpecs: platformMap ]
						ifFalse: [ 
							platformName = 'vast'
								ifTrue: [ self _canonicalizeVastPackageMapSpecs: platformMap ]
								ifFalse: [ self error: 'Unknown platform name ' , platformName printString ] ].
					orderedPlatformMap isEmpty
						ifFalse: [ orderedConditionalPackageMapSpecs at: platformName put: orderedPlatformMap ] ].
			conditionalPackageMapSpecs := orderedConditionalPackageMapSpecs ]
]

{ #category : 'initialization' }
RwBasicProjectLoadComponentV2 >> initializeForImport [

	"if spec has been imported, clear out any of the fields that represent state that should 
	not be shared"

	projectName := nil

]

{ #category : 'ston' }
RwBasicProjectLoadComponentV2 >> instVarNamesInOrderForSton [

	^ self class allInstVarNames
]

{ #category : 'accessing' }
RwBasicProjectLoadComponentV2 >> name [

   ^name

]

{ #category : 'initialization' }
RwBasicProjectLoadComponentV2 >> name: anObject [

   name := anObject

]

{ #category : 'accessing' }
RwBasicProjectLoadComponentV2 >> projectName [

   ^projectName

]

{ #category : 'initialization' }
RwBasicProjectLoadComponentV2 >> projectName: anObject [

   projectName := anObject

]

{ #category : 'ston' }
RwBasicProjectLoadComponentV2 >> stonOn: stonWriter [
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
RwBasicProjectLoadComponentV2 >> validate [
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
