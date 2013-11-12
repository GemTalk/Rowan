" Package: Cypress-GemStoneFileServer"!


" Class Declarations "!

(Object
	subclass: 'CypressFileUtilities'
	instVarNames: #(  )
	classVars: #( Current )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-GemStoneFileServer';
		comment: ''!

(CypressFileUtilities
	subclass: 'CypressGemStoneDirectoryUtilities'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-GemStoneFileServer';
		comment: ''!

(Object
	subclass: 'CypressAbstractPackageFiler'
	instVarNames: #( repository packageDirectory packageStructure )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-GemStoneFileServer';
		comment: ''!

(CypressAbstractPackageFiler
	subclass: 'CypressAbstractPackageWriter'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #( specials )
	poolDictionaries: #())
		category: 'Cypress-GemStoneFileServer';
		comment: ''!

(CypressAbstractPackageWriter
	subclass: 'CypressStrictFileTreeFormatDoNothingPackageWriter'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-GemStoneFileServer';
		comment: ''!

(CypressAbstractPackageWriter
	subclass: 'CypressFileTreeFormatPackageWriter'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-GemStoneFileServer';
		comment: ''!

(CypressAbstractPackageWriter
	subclass: 'CypressPackageWriter'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-GemStoneFileServer';
		comment: ''!

(CypressAbstractPackageFiler
	subclass: 'CypressAbstractFileoutWriter'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-GemStoneFileServer';
		comment: ''!

(CypressAbstractFileoutWriter
	subclass: 'CypressTopazFileoutWriter'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-GemStoneFileServer';
		comment: ''!

(CypressAbstractFileoutWriter
	subclass: 'CypressSmalltalkFileoutWriter'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-GemStoneFileServer';
		comment: ''!

(CypressAbstractPackageFiler
	subclass: 'CypressAbstractPackageReader'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-GemStoneFileServer';
		comment: ''!

(CypressAbstractPackageReader
	subclass: 'CypressFileTreeFormatPackageReader'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-GemStoneFileServer';
		comment: ''!

(CypressAbstractPackageReader
	subclass: 'CypressDoNothingPackageReader'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-GemStoneFileServer';
		comment: ''!

(CypressAbstractPackageReader
	subclass: 'CypressPackageReader'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-GemStoneFileServer';
		comment: ''!

(CypressAbstractPackageReader
	subclass: 'CypressFlexiblePackageReader'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-GemStoneFileServer';
		comment: ''!

" Class Implementation for CypressFileUtilities"!

" ------------------- Class methods for CypressFileUtilities"!

!CypressFileUtilities class methodsFor: 'accessing'!
current

	^Current! !
!CypressFileUtilities class methodsFor: 'utilities'!
deleteAll: aDirectory

	self subclassResponsibility: #deleteAll:! !
!CypressFileUtilities class methodsFor: 'utilities'!
directoryEntriesFrom: aDirectory

	self subclassResponsibility: #directoryEntriesFrom:! !
!CypressFileUtilities class methodsFor: 'utilities'!
directoryExists: aDirectory

	self subclassResponsibility: #directoryExists:! !
!CypressFileUtilities class methodsFor: 'unknown'!
directoryFileNamesAndContents: aDirectory

	self subclassResponsibility: #directoryFileNamesAndContents:! !
!CypressFileUtilities class methodsFor: 'utilities'!
directoryFromPath: directoryPath relativeTo: aDirectory

	self subclassResponsibility: #directoryFromPath:relativeTo:! !
!CypressFileUtilities class methodsFor: 'utilities'!
ensureDirectoryExists: aDirectory

	self subclassResponsibility: #ensureDirectoryExists:! !
!CypressFileUtilities class methodsFor: 'initializating'!
install

	Current := self! !
!CypressFileUtilities class methodsFor: 'utilities'!
localNameFrom: aDirectory

	self subclassResponsibility: #localNameFrom:! !
!CypressFileUtilities class methodsFor: 'utilities'!
pathNameDelimiter

	self subclassResponsibility: #pathNameDelimiter! !
!CypressFileUtilities class methodsFor: 'utilities'!
readStreamFor: filePath do: aOneArgBlock

	self subclassResponsibility: #readStreamFor:do:! !
!CypressFileUtilities class methodsFor: 'utilities'!
readStreamFor: filePath in: aDirectory do: aOneArgBlock

	self subclassResponsibility: #readStreamFor:in:do:! !
!CypressFileUtilities class methodsFor: 'utilities'!
workingDirectory

	self subclassResponsibility: #workingDirectory! !
!CypressFileUtilities class methodsFor: 'utilities'!
writeStreamFor: filePath in: aDirectory do: aOneArgBlock

	self subclassResponsibility: #writeStreamFor:in:do:! !
" Class Implementation for CypressGemStoneDirectoryUtilities"!

" ------------------- Class methods for CypressGemStoneDirectoryUtilities"!

!CypressGemStoneDirectoryUtilities class methodsFor: 'utilities'!
deleteAll: aDirectory
	"Delete all the files and directories under the named directory.
	 Ensure we don't try to recursively delete . or .."

	| filename isFile |
	(GsFile contentsAndTypesOfDirectory: aDirectory onClient: false)
		doWithIndex: 
			[:each :index |
			index odd
				ifTrue: [filename := each]
				ifFalse: 
					[isFile := each.
					isFile
						ifTrue: [GsFile removeServerFile: filename]
						ifFalse: 
							[(#('/..' '/.' '\..' '\.')
								anySatisfy: [:special | filename endsWith: special])
									ifFalse: 
										[self deleteAll: filename.
										GsFile removeServerDirectory: filename]]]]! !
!CypressGemStoneDirectoryUtilities class methodsFor: 'utilities'!
directoryEntriesFrom: aDirectory
	"Answer fully qualified paths to the contents of aDirectory."

	^(GsFile contentsOfDirectory: aDirectory onClient: false) ifNil: [#()]! !
!CypressGemStoneDirectoryUtilities class methodsFor: 'utilities'!
directoryExists: aDirectory

	^GsFile existsOnServer: aDirectory
! !
!CypressGemStoneDirectoryUtilities class methodsFor: 'unknown'!
directoryFileNamesAndContents: aDirectory
	"Walk the directory tree starting at aDirectory and
	 answer a map of the names of the files in the tree to
	 their contents (which work best when text)."

	| map |
	map := Dictionary new.
	self directoryFileNamesAndContents: aDirectory into: map.
	^map.
! !
!CypressGemStoneDirectoryUtilities class methodsFor: 'unknown'!
directoryFileNamesAndContents: aDirectory into: aDictionary
	"Walk the directory tree starting at aDirectory and
	 answer a map of the names of the files in the tree to
	 their contents (which work best when text)."

	| filename isFile |
	(GsFile contentsAndTypesOfDirectory: aDirectory onClient: false)
		doWithIndex: 
			[:each :index |
			index odd
				ifTrue: [filename := each]
				ifFalse: 
					[isFile := each.
					isFile
						ifTrue: 
							[| file |
							file := GsFile openReadOnServer: filename.
							file isNil
								ifFalse: 
									[aDictionary at: filename put: file contents.
									file close]]
						ifFalse: 
							[(#('/..' '/.' '\..' '\.')
								anySatisfy: [:special | filename endsWith: special])
									ifFalse: [self directoryFileNamesAndContents: filename into: aDictionary]]]]! !
!CypressGemStoneDirectoryUtilities class methodsFor: 'utilities'!
directoryFromPath: directoryPath relativeTo: aDirectory

	^((aDirectory endsWith: self pathNameDelimiter) or: [directoryPath beginsWith: self pathNameDelimiter])
		ifTrue: [aDirectory, directoryPath]
		ifFalse: [aDirectory, self pathNameDelimiter, directoryPath]! !
!CypressGemStoneDirectoryUtilities class methodsFor: 'utilities'!
ensureDirectoryExists: aDirectory

	| lastSeparator |
	(GsFile existsOnServer: aDirectory) ifTrue: [^aDirectory].
	(GsFile createServerDirectory: aDirectory) ifNotNil: [^aDirectory].
	lastSeparator := aDirectory findLastSubString: self pathNameDelimiter startingAt: aDirectory size.
	lastSeparator <= 1 ifTrue: [self error: 'Cannot create directory'].
	self ensureDirectoryExists: (aDirectory copyFrom: 1 to: lastSeparator - 1).
	self ensureDirectoryExists: aDirectory.
! !
!CypressGemStoneDirectoryUtilities class methodsFor: 'initializating'!
initialize
	"self initialize"

	self install! !
!CypressGemStoneDirectoryUtilities class methodsFor: 'utilities'!
localNameFrom: aDirectory

	| endOfPath |
	endOfPath := aDirectory findLastSubString: self pathNameDelimiter startingAt: aDirectory size.
	^aDirectory copyFrom: endOfPath + 1 to: aDirectory size! !
!CypressGemStoneDirectoryUtilities class methodsFor: 'utilities'!
pathNameDelimiter

	^'/'! !
!CypressGemStoneDirectoryUtilities class methodsFor: 'utilities'!
readStreamFor: filePath do: aOneArgBlock

	| file stream |
	GsFile serverErrorString.
	file := GsFile openReadOnServer: filePath.
	GsFile serverErrorString ifNotNil: [:errorMessage | self error: errorMessage].
	[stream := ReadStream on: (String withAll: file contents asByteArray decodeFromUTF8).
	aOneArgBlock value: stream] ensure: [file close]! !
!CypressGemStoneDirectoryUtilities class methodsFor: 'utilities'!
readStreamFor: filePath in: aDirectory do: aOneArgBlock

	self
		readStreamFor: (self directoryFromPath: filePath relativeTo: aDirectory)
		do: aOneArgBlock! !
!CypressGemStoneDirectoryUtilities class methodsFor: 'utilities'!
workingDirectory

	^System gemEnvironmentVariable: 'PWD'! !
!CypressGemStoneDirectoryUtilities class methodsFor: 'utilities'!
writeStreamFor: filePath in: aDirectory do: aOneArgBlock

	| file stream |
	GsFile serverErrorString.
	file := GsFile openWriteOnServer: (self directoryFromPath: filePath relativeTo: aDirectory).
	GsFile serverErrorString ifNotNil: [:errorMessage | self error: errorMessage].
	stream := WriteStream on: String new.
	[aOneArgBlock value: stream] ensure: [file nextPutAll: stream contents encodeAsUTF8; close]! !
" Class Implementation for CypressAbstractPackageFiler"!

" ------------------- Class methods for CypressAbstractPackageFiler"!

!CypressAbstractPackageFiler class methodsFor: 'instance creation'!
forRepository: aCypressFileSystemRepository

	^self new
		initializeForRepository: aCypressFileSystemRepository;
		yourself.
! !
" ------------------- Instance methods for CypressAbstractPackageFiler"!

!CypressAbstractPackageFiler methodsFor: 'private'!
fileUtils

	^CypressFileUtilities current
! !
!CypressAbstractPackageFiler methodsFor: 'initializing - private'!
initializeForRepository: aCypressFileSystemRepository

	repository := aCypressFileSystemRepository! !
!CypressAbstractPackageFiler methodsFor: 'accessing'!
packageDirectory

	^packageDirectory! !
!CypressAbstractPackageFiler methodsFor: 'accessing'!
packageDirectory: aDirectory

	packageDirectory := aDirectory! !
!CypressAbstractPackageFiler methodsFor: 'accessing'!
packageStructure

	^packageStructure! !
!CypressAbstractPackageFiler methodsFor: 'accessing'!
packageStructure: aPackageStructure

	packageStructure := aPackageStructure! !
!CypressAbstractPackageFiler methodsFor: 'accessing'!
propertiesFileNameExtension

	^'.ston'! !
!CypressAbstractPackageFiler methodsFor: 'accessing'!
propertiesFileNameExtensions

	^Array with: self propertiesFileNameExtension! !
!CypressAbstractPackageFiler methodsFor: 'accessing'!
repository

	^repository! !
" Class Implementation for CypressAbstractPackageWriter"!

" ------------------- Class methods for CypressAbstractPackageWriter"!

!CypressAbstractPackageWriter class methodsFor: 'initialization'!
initializeSpecials
	"Valid binarySelector characters  '!!' | '%' | '&' | '*' | '+' | ','' | '/' | '<' | '=' | '>' | '?' | '@' | '\' | '~' | '|' | '-'"

	| map |
	map := Dictionary new.
	map
		at: $!! put: 'bang';
		at: $% put: 'percent';
		at: $& put: 'and';
		at: $* put: 'star';
		at: $+ put: 'plus';
		at: $, put: 'comma';
		at: $- put: 'minus';
		at: $/ put: 'slash';
		at: $< put: 'less';
		at: $= put: 'equals';
		at: $> put: 'more';
		at: $? put: 'wat';
		at: $@ put: 'at';
		at: $\ put: 'backslash';
		at: $| put: 'pipe';
		at: $~ put: 'tilde'.
	map keys do: [:key | map at: (map at: key) put: key].
	^map
! !
!CypressAbstractPackageWriter class methodsFor: 'accessing'!
specials

	^specials ifNil: [specials := self initializeSpecials]
! !
" ------------------- Instance methods for CypressAbstractPackageWriter"!

!CypressAbstractPackageWriter methodsFor: 'private'!
determinePackageDirectory

	^self fileUtils ensureDirectoryExists: (self fileUtils
				directoryFromPath: self packageStructure name
				relativeTo: self repository directoryPath)! !
!CypressAbstractPackageWriter methodsFor: 'private'!
directoryForDirectoryNamed: directoryNameOrPath

	^directoryNameOrPath = '.'
		ifTrue: [self fileUtils ensureDirectoryExists: self packageDirectory]
		ifFalse: [self subPackageFileDirectoryFor: directoryNameOrPath]
! !
!CypressAbstractPackageWriter methodsFor: 'private'!
fileNameForSelector: selector

	^selector last = $:
		ifTrue: [selector copyReplacing: $: with: $.]
		ifFalse: 
			[(selector first isLetter or: [selector first = $_])
				ifTrue: [selector]
				ifFalse: 
					[| specials |
					specials := self class specials.
					String streamContents: 
							[:output |
							output nextPut: $^.
							selector do: [:each | output nextPutAll: (specials at: each)]
								separatedBy: [output nextPut: $.]]]]
! !
!CypressAbstractPackageWriter methodsFor: 'writing'!
removeOldReplacingWithNew

	self fileUtils deleteAll: self packageDirectory.
	self writePropertiesFile.
	self writePackageStructure! !
!CypressAbstractPackageWriter methodsFor: 'private'!
subPackageFileDirectoryFor: directoryNameOrPath

	| dir |
	dir := self fileUtils directoryFromPath: directoryNameOrPath
				relativeTo: self packageDirectory.
	self fileUtils ensureDirectoryExists: dir.
	^dir
! !
!CypressAbstractPackageWriter methodsFor: 'writing'!
writeClassComment: classStructure on: fileStream

	fileStream nextPutAll: classStructure comment withUnixLineEndings
! !
!CypressAbstractPackageWriter methodsFor: 'writing'!
writeClassStructure: classStructure to: classPath

	self
		writeInDirectoryName: classPath
			fileName: 'README'
			extension: '.md'
			visit: [:fileStream | self writeClassComment: classStructure on: fileStream];
		writeInDirectoryName: classPath
			fileName: 'properties'
			extension: self propertiesFileNameExtension
			visit: [:fileStream | classStructure  properties writeCypressJsonOn: fileStream]
! !
!CypressAbstractPackageWriter methodsFor: 'writing'!
writeExtensionClassStructure: classStructure to: classPath

	self
		writeInDirectoryName: classPath
		fileName: 'properties'
		extension: self propertiesFileNameExtension
		visit: 
			[:fileStream |
			(Dictionary with: 'name' -> classStructure className)
				writeCypressJsonOn: fileStream]! !
!CypressAbstractPackageWriter methodsFor: 'private'!
writeInDirectoryName: directoryNameOrPath fileName: fileName extension: ext visit: visitBlock

	| directory |
	directory := self directoryForDirectoryNamed: directoryNameOrPath.
	self fileUtils
		writeStreamFor: fileName , ext
		in: directory
		do: [:fileStream | visitBlock value: fileStream]
! !
!CypressAbstractPackageWriter methodsFor: 'writing'!
writeMethodStructure: methodStructure onStream: fileStream

	self subclassResponsibility: #writeMethodStructure:onStream:! !
!CypressAbstractPackageWriter methodsFor: 'writing'!
writeMethodStructure: methodStructure to: methodPath

	| filename |
	filename := self fileNameForSelector: methodStructure selector.
	self
		writeInDirectoryName: methodPath
		fileName: filename
		extension: '.st'
		visit: [:fileStream | self writeMethodStructure: methodStructure onStream: fileStream]! !
!CypressAbstractPackageWriter methodsFor: 'writing'!
writePackageStructure

	self
		writePackageStructureClasses: self packageStructure classes
			isClassExtension: false;
		writePackageStructureClasses: self packageStructure extensions
			isClassExtension: true
! !
!CypressAbstractPackageWriter methodsFor: 'writing'!
writePackageStructure: aPackageStructure

	self
		packageStructure: aPackageStructure;
		packageDirectory: self determinePackageDirectory;
		removeOldReplacingWithNew
! !
!CypressAbstractPackageWriter methodsFor: 'writing'!
writePackageStructureClasses: classStructures isClassExtension: isClassExtension

	| classDirExtension |
	classDirExtension := isClassExtension
				ifTrue: ['.extension']
				ifFalse: ['.class'].
	classStructures do: 
			[:classStructure |
			| classPath instanceMethodPath classMethodPath |
			classPath := classStructure className , classDirExtension
						, self fileUtils pathNameDelimiter asString.
			isClassExtension
				ifTrue: [self writeExtensionClassStructure: classStructure to: classPath]
				ifFalse: [self writeClassStructure: classStructure to: classPath].
			instanceMethodPath := classPath , 'instance' , self fileUtils pathNameDelimiter asString.
			classStructure instanceMethods
				do: [:methodStructure | self writeMethodStructure: methodStructure to: instanceMethodPath].
			classMethodPath := classPath , 'class' , self fileUtils pathNameDelimiter asString.
			classStructure classMethods
				do: [:methodStructure | self writeMethodStructure: methodStructure to: classMethodPath]]
! !
!CypressAbstractPackageWriter methodsFor: 'writing'!
writePropertiesFile

	self
		writeInDirectoryName: '.'
		fileName: 'properties'
		extension: self propertiesFileNameExtension
		visit: [:fileStream | Dictionary new writeCypressJsonOn: fileStream]! !
" Class Implementation for CypressStrictFileTreeFormatDoNothingPackageWriter"!

" ------------------- Instance methods for CypressStrictFileTreeFormatDoNothingPackageWriter"!

!CypressStrictFileTreeFormatDoNothingPackageWriter methodsFor: 'writing'!
removeOldReplacingWithNew
	"Change nothing, since a Cypress writer has insufficient information
	 for preserving the FileTree details. Strictly read-only."! !
!CypressStrictFileTreeFormatDoNothingPackageWriter methodsFor: 'writing'!
writePropertiesFile
	"Change nothing, since a Cypress writer has insufficient information
	 for preserving the FileTree details. Strictly read-only."! !
" Class Implementation for CypressFileTreeFormatPackageWriter"!

" ------------------- Instance methods for CypressFileTreeFormatPackageWriter"!

!CypressFileTreeFormatPackageWriter methodsFor: 'writing'!
writeMethodStructure: methodStructure onStream: fileStream

	fileStream
		nextPutAll: methodStructure category; lf;
		nextPutAll: methodStructure source withUnixLineEndings
! !
" Class Implementation for CypressPackageWriter"!

" ------------------- Instance methods for CypressPackageWriter"!

!CypressPackageWriter methodsFor: 'accessing - private'!
methodNoticeLine

	^self packageStructure properties
		at: 'copyrightLine'
		ifAbsent: [self repository copyrightProperty]! !
!CypressPackageWriter methodsFor: 'writing'!
writeMethodStructure: methodStructure onStream: fileStream

	fileStream
		nextPutAll: '"'; lf;
		nextPutAll: 'notice: ', self methodNoticeLine; lf;
		nextPutAll: 'category: ', methodStructure category; lf;
		nextPutAll: '"'; lf;
		nextPutAll: methodStructure source withUnixLineEndings! !
" Class Implementation for CypressAbstractFileoutWriter"!

" ------------------- Instance methods for CypressAbstractFileoutWriter"!

!CypressAbstractFileoutWriter methodsFor: 'accessing'!
classesInDependencyOrder

	^(GsGeneralDependencySorter
		on: self packageStructure classes
		dependsOn: [:candidate | candidate superclassName]
		dependent: [:candidate | candidate className])
			inOrder! !
!CypressAbstractFileoutWriter methodsFor: 'accessing'!
classesWithInitializers

	^self classesInDependencyOrder
		select: [:each | each classMethods anySatisfy: [:method | method selector = 'initialize']]! !
!CypressAbstractFileoutWriter methodsFor: 'private'!
determinePackageDirectory

	^self fileUtils ensureDirectoryExists: self repository directoryPath
! !
!CypressAbstractFileoutWriter methodsFor: 'accessing'!
extensions

	^self packageStructure extensions! !
!CypressAbstractFileoutWriter methodsFor: 'writing - private'!
fileOut: aString implementationsFrom: someClassStructures on: aStream

	someClassStructures
		do: [:each | self fileOutType: aString implementationOf: each on: aStream]! !
!CypressAbstractFileoutWriter methodsFor: 'writing - private'!
fileOut: aString methods: someMethodStructures on: aStream

	someMethodStructures isEmpty ifTrue: [^self].
	self
		fileOut: aString methodsPreambleFor: someMethodStructures any classStructure on: aStream;
		fileOutMethods: someMethodStructures on: aStream! !
!CypressAbstractFileoutWriter methodsFor: 'writing - private'!
fileOutClassDeclarationsOn: aStream

	self classesInDependencyOrder
		do: [:classStructure | self fileOutClassDeclaration: classStructure on: aStream]! !
!CypressAbstractFileoutWriter methodsFor: 'writing - private'!
fileOutClassesOn: aStream

	self
		fileOutClassesPreambleOn: aStream;
		fileOutClassDeclarationsOn: aStream;
		fileOutClassImplementationsOn: aStream! !
!CypressAbstractFileoutWriter methodsFor: 'writing - private'!
fileOutClassImplementationsOn: aStream

	self
		fileOut: 'Class Implementation'
		implementationsFrom: self classesInDependencyOrder
		on: aStream
! !
!CypressAbstractFileoutWriter methodsFor: 'writing - private'!
fileOutClassInitializersOn: aStream

	self fileOutClassInitializersPreambleOn: aStream.
	self classesWithInitializers do: [:each | self fileOutClassInitializerFor: each on: aStream].
	self fileOutClassInitializersPostambleOn: aStream.
! !
!CypressAbstractFileoutWriter methodsFor: 'writing - private'!
fileOutExtensionImplementationsOn: aStream

	self
		fileOut: 'Class Extension'
		implementationsFrom: self extensions
		on: aStream
! !
!CypressAbstractFileoutWriter methodsFor: 'writing - private'!
fileOutExtensionsOn: aStream

	self
		fileOutExtensionsPreambleOn: aStream;
		fileOutExtensionImplementationsOn: aStream! !
!CypressAbstractFileoutWriter methodsFor: 'writing - private'!
fileOutMethods: someMethodStructures on: aStream

	(someMethodStructures
		asSortedCollection: [:a :b | a selector <= b selector])
			do: [:methodStructure | self fileOutMethod: methodStructure on: aStream]! !
!CypressAbstractFileoutWriter methodsFor: 'writing - private'!
fileOutPackageOn: aStream

	self
		fileOutPackagePreambleOn: aStream;
		fileOutClassesOn: aStream;
		fileOutExtensionsOn: aStream;
		fileOutClassInitializersOn: aStream;
		fileOutPackagePostambleOn: aStream! !
!CypressAbstractFileoutWriter methodsFor: 'writing - private'!
fileOutType: aString implementationOf: classStructure on: aStream

	self
		fileOutPreambleType: aString
			for: classStructure
			on: aStream;
		fileOut: 'Class'
			methods: classStructure classMethods
			on: aStream;
		fileOut: 'Instance'
			methods: classStructure instanceMethods
			on: aStream! !
!CypressAbstractFileoutWriter methodsFor: 'accessing'!
packageName

	^self packageStructure packageName! !
!CypressAbstractFileoutWriter methodsFor: 'accessing'!
packageNameExtension

	^self subclassResponsibility: #packageNameExtension
! !
!CypressAbstractFileoutWriter methodsFor: 'writing'!
writePackageStructure

	CypressFileUtilities current
		writeStreamFor: self packageStructure packageName, self packageNameExtension
		in: self packageDirectory
		do: [:fileStream | fileStream nextPutAll: (String streamContents: [:stream | self fileOutPackageOn: stream])]
! !
!CypressAbstractFileoutWriter methodsFor: 'writing'!
writePackageStructure: aPackageStructure

	self
		packageStructure: aPackageStructure;
		packageDirectory: self determinePackageDirectory;
		writePackageStructure
! !
" Class Implementation for CypressTopazFileoutWriter"!

" ------------------- Instance methods for CypressTopazFileoutWriter"!

!CypressTopazFileoutWriter methodsFor: 'writing - private'!
fileOut: aString methodsPreambleFor: classStructure on: aStream

	aStream
		nextPutAll: '!! ------------------- ', aString, ' methods for ', classStructure name; lf;
		lf
! !
!CypressTopazFileoutWriter methodsFor: 'writing - private'!
fileOutClassDeclaration: classStructure on: aStream

	aStream
		nextPutAll: 'doit'; lf;
		nextPutAll: '(', classStructure superclassName; lf;
		nextPutAll: '	subclass: ', classStructure className asString printString; lf;
		nextPutAll: '	instVarNames: #( ', classStructure instanceVariablesString, ' )'; lf;
		nextPutAll: '	classVars: #( ', classStructure classVariablesString, ' )'; lf;
		nextPutAll: '	classInstVars: #( ', classStructure classInstanceVariablesString, ' )'; lf;
		nextPutAll: '	poolDictionaries: #()'; lf;
		nextPutAll: '	inDictionary: UserGlobals'; lf;
		nextPutAll: '	options: #())'; lf;
		nextPutAll: '		category: ', classStructure category printString, ';'; lf;
		nextPutAll: '		comment: ', classStructure comment printString, ';'; lf;
		nextPutAll: '		immediateInvariant.'; lf;
		nextPutAll: '%'; lf;
		lf.
! !
!CypressTopazFileoutWriter methodsFor: 'writing - private'!
fileOutClassesPreambleOn: aStream

	aStream
		nextPutAll: '!! Class Declarations'; lf;
		lf
! !
!CypressTopazFileoutWriter methodsFor: 'writing - private'!
fileOutClassInitializerFor: classStructure on: aStream

	aStream
		nextPutAll: classStructure className, ' initialize.'; lf! !
!CypressTopazFileoutWriter methodsFor: 'writing - private'!
fileOutClassInitializersPostambleOn: aStream

	aStream
		nextPutAll: '%'; lf;
		lf! !
!CypressTopazFileoutWriter methodsFor: 'writing - private'!
fileOutClassInitializersPreambleOn: aStream

	aStream
		nextPutAll: '!! Class initializers '; lf;
		lf;
		nextPutAll: 'doit'; lf! !
!CypressTopazFileoutWriter methodsFor: 'writing - private'!
fileOutExtensionsPreambleOn: aStream

	aStream
		nextPutAll: '!! Class Extensions'; lf;
		lf
! !
!CypressTopazFileoutWriter methodsFor: 'writing - private'!
fileOutMethod: methodStructure on: aStream

	aStream
		nextPutAll: 'category: ', methodStructure category printString; lf;
		nextPutAll: 'set compile_env: 0'; lf;
		nextPutAll: (methodStructure isMetaclass ifTrue: ['classmethod: '] ifFalse: ['method: ']), methodStructure classStructure className; lf;
		nextPutAll: methodStructure source.
	methodStructure source last = Character lf
		ifFalse: [aStream lf].
	aStream nextPutAll: '%'; lf;
		lf
! !
!CypressTopazFileoutWriter methodsFor: 'writing - private'!
fileOutPackagePostambleOn: aStream

	aStream
		lf;
		lf;
		nextPutAll: '!! End of Package: ', self packageName; lf;
		lf;
		lf
! !
!CypressTopazFileoutWriter methodsFor: 'writing - private'!
fileOutPackagePreambleOn: aStream

	aStream
		nextPutAll: '!! Package: ', self packageName; lf;
		lf;
		lf;
		nextPutAll: '!! Remove existing behavior from package ', self packageName; lf;
		nextPutAll: '!!!!!!!! This can be cleaned up when some package functionality is moved to the base system.'; lf;
		lf;
		nextPutAll: 'doit'; lf;
		nextPutAll: '| packageName |'; lf;
		nextPutAll: 'packageName := ', self packageName printString, '.'; lf;
		nextPutAll: 'System myUserProfile symbolList do: [:symDict |'; lf;
		nextPutAll: '	symDict do: [:possibleClass |'; lf;
		nextPutAll: '			| toRemove |'; lf;
		nextPutAll: '		possibleClass isBehavior ifTrue: ['; lf;
		nextPutAll: '			{possibleClass. possibleClass class} do: [:aClass |'; lf;
		nextPutAll: '				aClass category = packageName'; lf;
		nextPutAll: '					ifTrue: ['; lf;
		nextPutAll: '							"*anythingbutpackagename[-anything]"'; lf;
		nextPutAll: '						toRemove := aClass categoryNames select: '; lf;
		nextPutAll: '										[:each |'; lf;
		nextPutAll: '										(each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])'; lf;
		nextPutAll: '														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]])'; lf;
		nextPutAll: '										or: [each first ~= $*]]'; lf;
		nextPutAll: '					]'; lf;
		nextPutAll: '					ifFalse: ['; lf;
		nextPutAll: '							"*packagename[-anything]"'; lf;
		nextPutAll: '						toRemove := aClass categoryNames select: '; lf;
		nextPutAll: '										[:each |'; lf;
		nextPutAll: '										each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])'; lf;
		nextPutAll: '														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]]]'; lf;
		nextPutAll: '					].'; lf;
		nextPutAll: '				toRemove do: [:each | aClass removeCategory: each].'; lf;
		nextPutAll: '			]'; lf;
		nextPutAll: '		]'; lf;
		nextPutAll: '	]'; lf;
		nextPutAll: '].'; lf;
		nextPutAll: '%'; lf;
		lf;
		lf
! !
!CypressTopazFileoutWriter methodsFor: 'writing - private'!
fileOutPreambleType: aString for: classStructure on: aStream

	aStream
		nextPutAll: '!! ', aString, ' for ', classStructure name; lf;
		lf
! !
!CypressTopazFileoutWriter methodsFor: 'accessing'!
packageNameExtension

	^'.gs'
! !
" Class Implementation for CypressSmalltalkFileoutWriter"!

" ------------------- Instance methods for CypressSmalltalkFileoutWriter"!

!CypressSmalltalkFileoutWriter methodsFor: 'writing - private'!
fileOut: aString methodsPreambleFor: classStructure on: aStream

	aStream
		nextChunkPut: '" ------------------- ', aString, ' methods for ', classStructure name, '"'; lf;
		lf
! !
!CypressSmalltalkFileoutWriter methodsFor: 'writing - private'!
fileOutClassDeclaration: classStructure on: aStream

	aStream
		nextPutAll: '(', classStructure superclassName; lf;
		nextPutAll: '	subclass: ', classStructure className asString printString; lf;
		nextPutAll: '	instVarNames: #( ', classStructure instanceVariablesString, ' )'; lf;
		nextPutAll: '	classVars: #( ', classStructure classVariablesString, ' )'; lf;
		nextPutAll: '	classInstVars: #( ', classStructure classInstanceVariablesString, ' )'; lf;
		nextPutAll: '	poolDictionaries: #())'; lf;
		nextPutAll: '		category: ', classStructure category printString, ';'; lf;
		nextChunkPut: '		comment: ', classStructure comment printString; lf;
		lf.
! !
!CypressSmalltalkFileoutWriter methodsFor: 'writing - private'!
fileOutClassesPreambleOn: aStream

	aStream
		nextChunkPut: '" Class Declarations "'; lf;
		lf
! !
!CypressSmalltalkFileoutWriter methodsFor: 'writing - private'!
fileOutClassInitializerFor: classStructure on: aStream

	aStream
		nextChunkPut: classStructure className, ' initialize.'; lf
! !
!CypressSmalltalkFileoutWriter methodsFor: 'writing - private'!
fileOutClassInitializersPostambleOn: aStream

	aStream
		lf
! !
!CypressSmalltalkFileoutWriter methodsFor: 'writing - private'!
fileOutClassInitializersPreambleOn: aStream

	aStream
		nextChunkPut: '" Class initializers "'; lf;
		lf
! !
!CypressSmalltalkFileoutWriter methodsFor: 'writing - private'!
fileOutExtensionsPreambleOn: aStream

	aStream
		nextChunkPut: '" Class Extensions "'; lf;
		lf
! !
!CypressSmalltalkFileoutWriter methodsFor: 'writing - private'!
fileOutMethod: methodStructure on: aStream

	aStream
		nextPutAll: '!!';
		nextChunkPut: methodStructure classStructure className, (methodStructure isMetaclass ifTrue: [' class methodsFor: '] ifFalse: [' methodsFor: ']), methodStructure category printString; lf;
		nextChunkPut: methodStructure source;
		nextChunkPut: ' ';
		lf
! !
!CypressSmalltalkFileoutWriter methodsFor: 'writing - private'!
fileOutPackagePostambleOn: aStream

	aStream
		lf;
		lf;
		nextChunkPut: '" End of Package: ', self packageName, '"'; lf;
		lf;
		lf
! !
!CypressSmalltalkFileoutWriter methodsFor: 'writing - private'!
fileOutPackagePreambleOn: aStream

	aStream
		nextChunkPut: '" Package: ', self packageName, '"'; lf;
		lf;
		lf
! !
!CypressSmalltalkFileoutWriter methodsFor: 'writing - private'!
fileOutPreambleType: aString for: classStructure on: aStream

	aStream
		nextChunkPut: '" ', aString, ' for ', classStructure name, '"'; lf;
		lf
! !
!CypressSmalltalkFileoutWriter methodsFor: 'accessing'!
packageNameExtension

	^'.st'
! !
" Class Implementation for CypressAbstractPackageReader"!

" ------------------- Instance methods for CypressAbstractPackageReader"!

!CypressAbstractPackageReader methodsFor: 'private'!
classStructureFrom: classPropertiesDict

	^(CypressClassStructure new)
		packageStructure: self packageStructure;
		isClassExtension: true;
		properties: classPropertiesDict;
		yourself
! !
!CypressAbstractPackageReader methodsFor: 'private'!
classStructureFrom: classPropertiesDict comment: classComment

	^(self classStructureFrom: classPropertiesDict)
		isClassExtension: false;
		comment: classComment;
		yourself
! !
!CypressAbstractPackageReader methodsFor: 'reading'!
isPropertiesFileDirectoryEntry: entry

	^self propertiesFileNameExtensions
		anySatisfy: [:each | entry endsWith: '/properties' , each]! !
!CypressAbstractPackageReader methodsFor: 'accessing'!
packageExtension

	^self packageStructure
		packageExtensionOr: [self repository packageExtension]! !
!CypressAbstractPackageReader methodsFor: 'reading'!
readClassCommentFromDirectoryEntries: entries

	self fileUtils readStreamFor: (entries
				detect: [:entry | entry endsWith: '/README.md']
				ifNone: [^''])
		do: [:fileStream | ^fileStream contents]
! !
!CypressAbstractPackageReader methodsFor: 'reading'!
readClassPropertiesFromDirectoryEntries: entries

	self fileUtils readStreamFor: (entries
				detect: [:entry | self isPropertiesFileDirectoryEntry: entry]
				ifNone: [^Dictionary new])
		do: [:fileStream | ^CypressJsonParser parseStream: fileStream]
! !
!CypressAbstractPackageReader methodsFor: 'reading'!
readClassStructureFromEntry: classEntry

	| classDirectory classPropertiesDict classComment entries classStructure |
	classDirectory := classEntry.
	entries := self fileUtils directoryEntriesFrom: classDirectory.
	classPropertiesDict := self
				readClassPropertiesFromDirectoryEntries: entries.
	classComment := self readClassCommentFromDirectoryEntries: entries.
	classStructure := self classStructureFrom: classPropertiesDict
				comment: classComment.
	self readMethodStructureFor: classStructure in: entries.
	^classStructure! !
!CypressAbstractPackageReader methodsFor: 'reading'!
readCypressFormatMethodStructureFrom: fileStream intoClassStructure: classStructure meta: isMeta methods: methods

	| notice category source selector |
	(fileStream peekFor: $")
		ifTrue: [fileStream nextLine]
		ifFalse: [self error: 'Method does not have valid Cypress format'].
	(fileStream match: 'notice: ')
		ifTrue: [notice := fileStream nextLine trimSeparators]
		ifFalse: [self error: 'Method does not have valid Cypress format'].
	(fileStream match: 'category: ')
		ifTrue: [category := fileStream nextLine trimSeparators]
		ifFalse: [self error: 'Method does not have valid Cypress format'].
	(fileStream peekFor: $")
		ifTrue: [fileStream nextLine]
		ifFalse: [self error: 'Method does not have valid Cypress format'].
	source := fileStream upToEnd.
	selector := UndefinedObject parseSelectorFrom: source.
	methods at: selector
		put: ((CypressMethodStructure new)
				packageStructure: self packageStructure;
				classStructure: classStructure;
				name: selector;
				isMetaclass: isMeta;
				selector: selector;
				category: category;
				source: source;
				yourself)! !
!CypressAbstractPackageReader methodsFor: 'reading'!
readExtensionClassStructureFromEntry: classEntry

	| classPropertiesDict entries classStructure |
	entries := self fileUtils directoryEntriesFrom: classEntry.
	classPropertiesDict := self
				readClassPropertiesFromDirectoryEntries: entries.
	classStructure := self classStructureFrom: classPropertiesDict.
	self readMethodStructureFor: classStructure in: entries.
	^classStructure! !
!CypressAbstractPackageReader methodsFor: 'reading'!
readFileTreeFormatMethodStructureFrom: fileStream intoClassStructure: classStructure meta: isMeta methods: methods

	| category source selector |
	category := fileStream nextLine trimSeparators.
	source := fileStream upToEnd.
	selector := UndefinedObject parseSelectorFrom: source.
	methods at: selector
		put: ((CypressMethodStructure new)
				packageStructure: self packageStructure;
				classStructure: classStructure;
				name: selector;
				isMetaclass: isMeta;
				selector: selector;
				category: category;
				source: source;
				yourself)
! !
!CypressAbstractPackageReader methodsFor: 'reading'!
readMethodStructureFor: classStructure in: entries

	entries do: 
			[:entry |
			| methods isMeta |
			methods := (isMeta := entry endsWith: '/class')
						ifTrue: [classStructure classMethods]
						ifFalse: [classStructure instanceMethods].
			((entry endsWith: '/instance') or: [entry endsWith: '/class'])
				ifTrue: 
					[((self fileUtils directoryEntriesFrom: entry)
						select: [:each | each endsWith: '.st']) do: 
								[:methodEntry |
								self fileUtils readStreamFor: methodEntry
									do: 
										[:fileStream |
										self
											readMethodStructureFrom: fileStream
											intoClassStructure: classStructure
											meta: isMeta
											methods: methods]]]]! !
!CypressAbstractPackageReader methodsFor: 'reading'!
readMethodStructureFrom: fileStream intoClassStructure: classStructure meta: isMeta methods: methods

	self subclassResponsibility: #readMethodStructureFrom:intoClassStructure:meta:methods:! !
!CypressAbstractPackageReader methodsFor: 'reading'!
readPackageStructure

   (self fileUtils directoryEntriesFrom: self packageDirectory)
        do: [ :entry | 
		(self isPropertiesFileDirectoryEntry: entry)
			ifTrue: [ self packageStructure properties: (self readPropertiesFile: entry) ].
            (entry endsWith: '.class')
                ifTrue: [ self packageStructure classes add: (self readClassStructureFromEntry: entry) ].
            (entry endsWith: '.extension')
                ifTrue: [ self packageStructure extensions add: (self readExtensionClassStructureFromEntry: entry) ] ]
! !
!CypressAbstractPackageReader methodsFor: 'reading'!
readPackageStructureForPackageNamed: packageName

	| structureName |
	structureName := packageName , self repository packageExtension.
	self
		packageStructure: (CypressPackageStructure named: structureName);
		packageDirectory: (self fileUtils directoryFromPath: structureName
					relativeTo: self repository directoryPath);
		readPackageStructure! !
!CypressAbstractPackageReader methodsFor: 'reading'!
readPropertiesFile: entry

	self fileUtils
		readStreamFor: entry
		do: [:fileStream | ^CypressJsonParser parseStream: fileStream]
! !
" Class Implementation for CypressFileTreeFormatPackageReader"!

" ------------------- Instance methods for CypressFileTreeFormatPackageReader"!

!CypressFileTreeFormatPackageReader methodsFor: 'accessing'!
propertiesFileNameExtension

	^'.json'! !
!CypressFileTreeFormatPackageReader methodsFor: 'accessing'!
propertiesFileNameExtensions

	^Array
		with: super propertiesFileNameExtension
		with: self propertiesFileNameExtension! !
!CypressFileTreeFormatPackageReader methodsFor: 'reading'!
readMethodStructureFrom: fileStream intoClassStructure: classStructure meta: isMeta methods: methods
	"Strict!!"

	self
		readFileTreeFormatMethodStructureFrom: fileStream
		intoClassStructure: classStructure
		meta: isMeta
		methods: methods! !
" Class Implementation for CypressDoNothingPackageReader"!

" ------------------- Instance methods for CypressDoNothingPackageReader"!

!CypressDoNothingPackageReader methodsFor: 'reading'!
readPackageStructure
! !
" Class Implementation for CypressPackageReader"!

" ------------------- Instance methods for CypressPackageReader"!

!CypressPackageReader methodsFor: 'reading'!
readMethodStructureFrom: fileStream intoClassStructure: classStructure meta: isMeta methods: methods
	"Strict!!"

	self
		readCypressFormatMethodStructureFrom: fileStream
		intoClassStructure: classStructure
		meta: isMeta
		methods: methods! !
" Class Implementation for CypressFlexiblePackageReader"!

" ------------------- Instance methods for CypressFlexiblePackageReader"!

!CypressFlexiblePackageReader methodsFor: 'reading'!
isPropertiesFileDirectoryEntry: entry
	"Expect .ston properties file, but tolerate .json if present."

	^(super isPropertiesFileDirectoryEntry: entry)
		or: [entry endsWith: '/properties.json']
! !
!CypressFlexiblePackageReader methodsFor: 'reading'!
readMethodStructureFrom: fileStream intoClassStructure: classStructure meta: isMeta methods: methods
	"If the stream begins with a double quote, process it on the assumption it is a Cypress-format method.
	 Otherwise, treat it as a FileTree-format method."

	fileStream peek = $"
		ifTrue: 
			[self
				readCypressFormatMethodStructureFrom: fileStream
				intoClassStructure: classStructure
				meta: isMeta
				methods: methods]
		ifFalse: 
			[self
				readFileTreeFormatMethodStructureFrom: fileStream
				intoClassStructure: classStructure
				meta: isMeta
				methods: methods]! !
" Class Extensions "!

" Class Extension for WriteStream"!

" ------------------- Instance methods for WriteStream"!

!WriteStream methodsFor: '*Cypress-GemStoneFileServer-Adding'!
nextChunkPut: aString

	aString do: 
		[:each |
		self nextPut: each.
		each = $!! ifTrue: [self nextPut: each]].
	self nextPut: $!!.
! !
" Class initializers "!

CypressGemStoneDirectoryUtilities initialize.!



" End of Package: Cypress-GemStoneFileServer"!


