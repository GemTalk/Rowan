Class {
	#name : 'RwAbstractComponent',
	#superclass : 'Object',
	#instVars : [
		'name',
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

{ #category : 'private' }
RwAbstractComponent >> _platformPatternMatcherFor: pattern [
	^ self class _platformPatternMatcherFor: pattern
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

{ #category : 'ston' }
RwAbstractComponent >> excludedInstVars [
	"restore full #instVarNamesInOrderForSton - no exclusions (see super implementation)"

	^ #()
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

	"noop"
]

{ #category : 'initialization' }
RwAbstractComponent >> initializeForImport [
	"if spec has been imported, clear out any of the fields that represent state that should 
	not be shared"

	"noop"
]

{ #category : 'ston' }
RwAbstractComponent >> instVarNamesInOrderForSton [

	^ self class allInstVarNames
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

{ #category : 'printing' }
RwAbstractComponent >> printOn: aStream [
	super printOn: aStream.
	aStream
		space;
		nextPutAll: name.
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
