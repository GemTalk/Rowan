Class {
	#name : 'RwAbstractComponent',
	#superclass : 'Object',
	#instVars : [
		'name',
		'projectName',
		'comment',
		'componentNames',
		'packageNames'
	],
	#category : 'Rowan-Components'
}

{ #category : 'version pattern matching' }
RwAbstractComponent class >> _platformPatternMatcherFor: pattern [
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
				patternMatchBlock: [ :a :b | a = b ];
				yourself ].	"GemStone version pattern"
	versionPattern := (pattern copyFrom: 3 to: pattern size) substrings: '.'.
	(versionPattern last beginsWith: '[')
		ifTrue: [ 
			| vpSize rangePattern dashIndex |
			"range pattern"
			vpSize := versionPattern size.
			gsVersion1 := RwGemStoneVersionNumber new: vpSize.
			1 to: vpSize - 1 do: [ :index | gsVersion1 at: index put: (versionPattern at: index) asInteger ].
			gsVersion1 at: vpSize put: 0.
			rangePattern := (versionPattern at: vpSize) trimBoth.
			((rangePattern at: 1) = $[ and: [ (rangePattern at: rangePattern size) = $] ])
				ifFalse: [ 
					self
						error:
							'Poorly formed GemStone version range pattern ' , rangePattern printString
								, ' in ' , pattern printString ].
			rangePattern := rangePattern copyFrom: 2 to: rangePattern size - 1.
			dashIndex := rangePattern indexOf: $-.
			dashIndex <= 1
				ifTrue: [ 
					self
						error:
							'Invalid version range pattern missing range begin' , rangePattern printString
								, ' in ' , pattern printString ].
			gsVersion1
				at: vpSize
				put: (rangePattern copyFrom: 1 to: dashIndex - 1) asInteger.
			dashIndex = rangePattern size
				ifTrue: [ 
					"open range"
					gsVersion2 := gsVersion1 copyFrom: 1 to: gsVersion1 size - 1.
					gsVersion2 at: gsVersion2 size put: (gsVersion2 at: gsVersion2 size) + 1.
					^ RwGemStoneVersionRangeConfigurationPlatformAttributeMatcher new
						pattern: gsVersion1;
						pattern2: gsVersion2;
						patternMatchBlock: [ :a :b :c | a <= b & (b < c) ];
						yourself ]
				ifFalse: [ 
					"closed range"
					gsVersion2 := gsVersion1 copy.
					gsVersion2
						at: vpSize
						put:
							(rangePattern copyFrom: dashIndex + 1 to: rangePattern size) asInteger + 1.
					^ RwGemStoneVersionRangeConfigurationPlatformAttributeMatcher new
						pattern: gsVersion1;
						pattern2: gsVersion2;
						patternMatchBlock: [ :a :b :c | a <= b & (b < c) ];
						yourself ] ].
	versionPattern last = 'x'
		ifTrue: [ 
			" 'gs', <gemstone-version-number> , '.x'"
			"match all values in x field"
			gsVersion1 := ((pattern copyFrom: 3 to: pattern size - 2) , '.0')
				asRwGemStoneVersionNumber.
			gsVersion2 := gsVersion1 copyFrom: 1 to: gsVersion1 size - 1.
			gsVersion2 at: gsVersion2 size put: (gsVersion2 at: gsVersion2 size) + 1.
			^ RwGemStoneVersionRangeConfigurationPlatformAttributeMatcher new
				pattern: gsVersion1;
				pattern2: gsVersion2;
				patternMatchBlock: [ :a :b :c | a <= b & (b < c) ];
				yourself ]
		ifFalse: [ 
			"specific version number match, use ="
			^ RwGemStoneVersionConfigurationPlatformAttributeMatcher new
				pattern: (pattern copyFrom: 3 to: pattern size) asRwGemStoneVersionNumber;
				patternMatchBlock: [ :a :b | a = b ];
				yourself ]
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
RwAbstractComponent class >> fromFile: filePath [
	filePath asFileReference
		readStreamDo: [ :fileStream | 
			| stream |
			Rowan projectTools trace trace: '--- reading component ' , filePath asString.
			stream := ZnBufferedReadStream on: fileStream.	"wrap with buffered stream to bypass https://github.com/GemTalk/FileSystemGs/issues/9"
			^ self _readStonFrom: stream ]
]

{ #category : 'instance creation' }
RwAbstractComponent class >> fromUrl: specNameOrUrl [

	"self fromUrl: 'file:/home/dhenrich/rogue/_homes/rogue/_home/shared/repos/RowanSample9/rowan/components/Default.ston'"

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

{ #category : 'accessing' }
RwAbstractComponent class >> orderedDictionaryClass [
	^ Rowan platform orderedDictionaryClass
]

{ #category : 'dispatching' }
RwAbstractComponent >> _addToResolvedProjectComponents: aRwResolvedProjectComponentsV2 [
	"dispatch to _addActiveComponent: or _addPackageGroup: as appropriate"

	self subclassResponsibility: #'_addToResolvedProjectComponents:'
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
					stream 
						truncate;
						nextPutAll: string.
					^ self ] ].
	url schemeName = 'memory'
		ifTrue: [ 
			FileSystem currentMemoryFileSystem workingDirectory / url pathForDirectory
				/ self name , 'ston'
				writeStreamDo: [ :stream | 
					| string |
					string := STON toStringPretty: self.
					stream 
						truncate;
						nextPutAll: string.
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
	^ self class _platformPatternMatcherFor: pattern
]

{ #category : 'doits' }
RwAbstractComponent >> _readDoitsFrom: componentsRoot [
	"noop"
]

{ #category : 'validation' }
RwAbstractComponent >> _validateDoits [
	"noop"
]

{ #category : 'validation' }
RwAbstractComponent >> _validatedPackageNames [
	"answer the validated set of package names"

	^ self packageNames asSet
]

{ #category : 'comparing' }
RwAbstractComponent >> = aRwProjectLoadComponentV2 [
	(aRwProjectLoadComponentV2 isKindOf: self class)
		ifFalse: [ ^ false ].
	^ (((self name = aRwProjectLoadComponentV2 name
		and: [ self comment = aRwProjectLoadComponentV2 comment ])
		and: [ self projectName = aRwProjectLoadComponentV2 projectName])
		and: [ self packageNames = aRwProjectLoadComponentV2 packageNames ])
		and: [ self componentNames = aRwProjectLoadComponentV2 componentNames ]
]

{ #category : 'accessing' }
RwAbstractComponent >> addComponentNamed: aComponentName [
	"avoid duplicates and keep the list sorted"

	| cn |
	cn := self componentNames.	" returns copy, so cannot add directly to result"
	cn add: aComponentName.
	componentNames := cn asSet asArray sort
]

{ #category : 'accessing' }
RwAbstractComponent >> addComponentNames: aComponentNames [
	"add component names"

	| cn |
	cn := self componentNames.	" returns copy, so cannot add directly to result"
	cn addAll: aComponentNames.
	componentNames := cn asSet asArray sort
]

{ #category : 'accessing' }
RwAbstractComponent >> addPackageNames: aPackageNames [
	"add packages to default conditional/group if applicable"

	| pn |
	pn := self packageNames.	" returns copy, so cannot add directly to result"
	pn addAll: aPackageNames.
	packageNames := pn asSet asArray sort
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

	^ componentNames copy
]

{ #category : 'ston' }
RwAbstractComponent >> excludedInstVars [
	"restore full #instVarNamesInOrderForSton - no exclusions (see super implementation)"

	^ #( #projectName )
]

{ #category : 'exporting' }
RwAbstractComponent >> exportToUrl: directoryUrl [
	^ self copy initializeForExport
		_exportToUrl: directoryUrl;
		yourself
]

{ #category : 'initialization' }
RwAbstractComponent >> initialize [
	comment := ''.
	packageNames := {}.
	componentNames := {}
]

{ #category : 'initialization' }
RwAbstractComponent >> initializeForExport [
	"if spec is to be exported, clear out any of the fields that represent state that should 
	not be shared"

	projectName := nil
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

{ #category : 'accessing' }
RwAbstractComponent >> label [

   ^self basename
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

	^ packageNames copy
]

{ #category : 'copying' }
RwAbstractComponent >> postCopy [
	super postCopy.
	componentNames := componentNames copy.
	packageNames := packageNames copy
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
	"The name of the load spec in the projects directory that is used to resolve/load the project"

	projectName := anObject
]

{ #category : 'accessing' }
RwAbstractComponent >> projectNames [
	^ #()
]

{ #category : 'accessing' }
RwAbstractComponent >> referencePath [
	^ Path from: self name
]

{ #category : 'accessing' }
RwAbstractComponent >> removeComponentNamed: aComponentName [
	| cn |
	cn := self componentNames.
	cn remove: aComponentName ifAbsent: [  ].
	componentNames := cn asSet asArray sort
]

{ #category : 'accessing' }
RwAbstractComponent >> removePackageNamed: aPackageName [
	| pn |
	pn := self packageNames.	" returns copy, so cannot add directly to result"
	pn remove: aPackageName ifAbsent: [  ].
	packageNames := pn asSet asArray sort
]

{ #category : 'accessing' }
RwAbstractComponent >> renameTo: newComponentName in: aResolvedProject [

	self subclassResponsibility: #renameTo:in:
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

	self subclassResponsitility: #'validate'
]
