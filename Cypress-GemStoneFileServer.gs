! Package: Cypress-GemStoneFileServer


! Remove existing behavior from package Cypress-GemStoneFileServer
!!!! This can be cleaned up when some package functionality is moved to the base system.

doit
| packageName |
packageName := 'Cypress-GemStoneFileServer'.
System myUserProfile symbolList do: [:symDict |
	symDict do: [:possibleClass |
			| toRemove |
		possibleClass isBehavior ifTrue: [
			{possibleClass. possibleClass class} do: [:aClass |
				aClass category = packageName
					ifTrue: [
							"*anythingbutpackagename[-anything]"
						toRemove := aClass categoryNames select: 
										[:each |
										(each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])
														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]])
										or: [each first ~= $*]]
					]
					ifFalse: [
							"*packagename[-anything]"
						toRemove := aClass categoryNames select: 
										[:each |
										each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])
														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]]]
					].
				toRemove do: [:each | aClass removeCategory: each].
			]
		]
	]
].
%


! Class Declarations

doit
(Object
	subclass: 'CypressAbstractPackageFiler'
	instVarNames: #( repository packageDirectory packageStructure )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-GemStoneFileServer';
		comment: '';
		immediateInvariant.
%

doit
(CypressAbstractPackageFiler
	subclass: 'CypressAbstractPackageWriter'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #( specials )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-GemStoneFileServer';
		comment: '';
		immediateInvariant.
%

doit
(CypressAbstractPackageWriter
	subclass: 'CypressFileTreeFormatPackageWriter'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-GemStoneFileServer';
		comment: '';
		immediateInvariant.
%

doit
(CypressAbstractPackageWriter
	subclass: 'CypressPackageWriter'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-GemStoneFileServer';
		comment: '';
		immediateInvariant.
%

doit
(CypressAbstractPackageFiler
	subclass: 'CypressAbstractPackageReader'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-GemStoneFileServer';
		comment: '';
		immediateInvariant.
%

doit
(CypressAbstractPackageReader
	subclass: 'CypressFileTreeFormatPackageReader'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-GemStoneFileServer';
		comment: '';
		immediateInvariant.
%

doit
(CypressAbstractPackageReader
	subclass: 'CypressFlexiblePackageReader'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-GemStoneFileServer';
		comment: '';
		immediateInvariant.
%

doit
(CypressAbstractPackageReader
	subclass: 'CypressPackageReader'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-GemStoneFileServer';
		comment: '';
		immediateInvariant.
%

doit
(Object
	subclass: 'CypressFileUtilities'
	instVarNames: #(  )
	classVars: #( Current )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-GemStoneFileServer';
		comment: '';
		immediateInvariant.
%

doit
(CypressFileUtilities
	subclass: 'CypressGemStoneDirectoryUtilities'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-GemStoneFileServer';
		comment: '';
		immediateInvariant.
%

doit
(Object
	subclass: 'CypressVersionReference'
	instVarNames: #( name package author branch versionNumber )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-GemStoneFileServer';
		comment: 'A CypressVersionReference refers to a specific version of a Monticello package.';
		immediateInvariant.
%

! Class Implementation for CypressAbstractPackageFiler

! ------------------- Class methods for CypressAbstractPackageFiler

category: 'instance creation'
set compile_env: 0
classmethod: CypressAbstractPackageFiler
forRepository: aCypressFileSystemRepository

	^self new
		initializeForRepository: aCypressFileSystemRepository;
		yourself.
%

! ------------------- Instance methods for CypressAbstractPackageFiler

category: 'private'
set compile_env: 0
method: CypressAbstractPackageFiler
fileUtils

	^CypressFileUtilities current
%

category: 'initializing - private'
set compile_env: 0
method: CypressAbstractPackageFiler
initializeForRepository: aCypressFileSystemRepository

	repository := aCypressFileSystemRepository
%

category: 'accessing'
set compile_env: 0
method: CypressAbstractPackageFiler
packageDirectory

	^packageDirectory
%

category: 'accessing'
set compile_env: 0
method: CypressAbstractPackageFiler
packageDirectory: aDirectory

	packageDirectory := aDirectory
%

category: 'accessing'
set compile_env: 0
method: CypressAbstractPackageFiler
packageStructure

	^packageStructure
%

category: 'accessing'
set compile_env: 0
method: CypressAbstractPackageFiler
packageStructure: aPackageStructure

	packageStructure := aPackageStructure
%

category: 'accessing'
set compile_env: 0
method: CypressAbstractPackageFiler
propertiesFileNameExtension

	^'.ston'
%

category: 'accessing'
set compile_env: 0
method: CypressAbstractPackageFiler
repository

	^repository
%

! Class Implementation for CypressAbstractPackageWriter

! ------------------- Class methods for CypressAbstractPackageWriter

category: 'initialization'
set compile_env: 0
classmethod: CypressAbstractPackageWriter
initializeSpecials
	"Valid binarySelector characters  '!' | '%' | '&' | '*' | '+' | ','' | '/' | '<' | '=' | '>' | '?' | '@' | '\' | '~' | '|' | '-'"

	| map |
	map := Dictionary new.
	map
		at: $! put: 'bang';
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
%

category: 'accessing'
set compile_env: 0
classmethod: CypressAbstractPackageWriter
specials

	^specials ifNil: [specials := self initializeSpecials]
%

! ------------------- Instance methods for CypressAbstractPackageWriter

category: 'private'
set compile_env: 0
method: CypressAbstractPackageWriter
directoryForDirectoryNamed: directoryNameOrPath

	^directoryNameOrPath = '.'
		ifTrue: [self fileUtils ensureDirectoryExists: self packageDirectory]
		ifFalse: [self subPackageFileDirectoryFor: directoryNameOrPath]
%

category: 'private'
set compile_env: 0
method: CypressAbstractPackageWriter
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
%

category: 'private'
set compile_env: 0
method: CypressAbstractPackageWriter
subPackageFileDirectoryFor: directoryNameOrPath

	| dir |
	dir := self fileUtils directoryFromPath: directoryNameOrPath
				relativeTo: self packageDirectory.
	self fileUtils ensureDirectoryExists: dir.
	^dir
%

category: 'writing'
set compile_env: 0
method: CypressAbstractPackageWriter
writeClassComment: classStructure on: fileStream

	fileStream nextPutAll: classStructure comment withUnixLineEndings
%

category: 'writing'
set compile_env: 0
method: CypressAbstractPackageWriter
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
%

category: 'writing'
set compile_env: 0
method: CypressAbstractPackageWriter
writeExtensionClassStructure: classStructure to: classPath

	self
		writeInDirectoryName: classPath
		fileName: 'properties'
		extension: self propertiesFileNameExtension
		visit: 
			[:fileStream |
			(Dictionary with: 'name' -> classStructure className)
				writeCypressJsonOn: fileStream]
%

category: 'private'
set compile_env: 0
method: CypressAbstractPackageWriter
writeInDirectoryName: directoryNameOrPath fileName: fileName extension: ext visit: visitBlock

	| directory |
	directory := self directoryForDirectoryNamed: directoryNameOrPath.
	self fileUtils
		writeStreamFor: fileName , ext
		in: directory
		do: [:fileStream | visitBlock value: fileStream]
%

category: 'writing'
set compile_env: 0
method: CypressAbstractPackageWriter
writeMethodStructure: methodStructure onStream: fileStream

	self subclassResponsibility: #writeMethodStructure:onStream:
%

category: 'writing'
set compile_env: 0
method: CypressAbstractPackageWriter
writeMethodStructure: methodStructure to: methodPath

	| filename |
	filename := self fileNameForSelector: methodStructure selector.
	self
		writeInDirectoryName: methodPath
		fileName: filename
		extension: '.st'
		visit: [:fileStream | self writeMethodStructure: methodStructure onStream: fileStream]
%

category: 'writing'
set compile_env: 0
method: CypressAbstractPackageWriter
writePackageStructure

	self
		writePackageStructureClasses: self packageStructure classes
			isClassExtension: false;
		writePackageStructureClasses: self packageStructure extensions
			isClassExtension: true
%

category: 'writing'
set compile_env: 0
method: CypressAbstractPackageWriter
writePackageStructure: aPackageStructure

	self
		packageStructure: aPackageStructure;
		packageDirectory: (self fileUtils ensureDirectoryExists: (self fileUtils
							directoryFromPath: self packageStructure name
							relativeTo: self repository directoryPath)).
	self fileUtils deleteAll: self packageDirectory.
	self writePropertiesFile.
	self writePackageStructure
%

category: 'writing'
set compile_env: 0
method: CypressAbstractPackageWriter
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
%

category: 'writing'
set compile_env: 0
method: CypressAbstractPackageWriter
writePropertiesFile

	self
		writeInDirectoryName: '.'
		fileName: 'properties'
		extension: self propertiesFileNameExtension
		visit: [:fileStream | Dictionary new writeCypressJsonOn: fileStream]
%

! Class Implementation for CypressFileTreeFormatPackageWriter

! ------------------- Instance methods for CypressFileTreeFormatPackageWriter

category: 'accessing'
set compile_env: 0
method: CypressFileTreeFormatPackageWriter
propertiesFileNameExtension

	^'.json'
%

category: 'writing'
set compile_env: 0
method: CypressFileTreeFormatPackageWriter
writeMethodStructure: methodStructure onStream: fileStream

	fileStream
		nextPutAll: methodStructure category; lf;
		nextPutAll: methodStructure source withUnixLineEndings
%

! Class Implementation for CypressPackageWriter

! ------------------- Instance methods for CypressPackageWriter

category: 'accessing - private'
set compile_env: 0
method: CypressPackageWriter
methodNoticeLine

	^self packageStructure properties
		at: 'copyrightLine'
		ifAbsent: [self repository copyrightProperty]
%

category: 'writing'
set compile_env: 0
method: CypressPackageWriter
writeMethodStructure: methodStructure onStream: fileStream

	fileStream
		nextPutAll: '"'; lf;
		nextPutAll: 'notice: ', self methodNoticeLine; lf;
		nextPutAll: 'category: ', methodStructure category; lf;
		nextPutAll: '"'; lf;
		nextPutAll: methodStructure source withUnixLineEndings
%

! Class Implementation for CypressAbstractPackageReader

! ------------------- Instance methods for CypressAbstractPackageReader

category: 'private'
set compile_env: 0
method: CypressAbstractPackageReader
classStructureFrom: classPropertiesDict

	^(CypressClassStructure new)
		packageStructure: self packageStructure;
		isClassExtension: true;
		properties: classPropertiesDict;
		yourself
%

category: 'private'
set compile_env: 0
method: CypressAbstractPackageReader
classStructureFrom: classPropertiesDict comment: classComment

	^(self classStructureFrom: classPropertiesDict)
		isClassExtension: false;
		comment: classComment;
		yourself
%

category: 'reading'
set compile_env: 0
method: CypressAbstractPackageReader
isPropertiesFileDirectoryEntry: entry

	^entry endsWith: '/properties', self propertiesFileNameExtension
%

category: 'accessing'
set compile_env: 0
method: CypressAbstractPackageReader
packageExtension

	^self packageStructure
		packageExtensionOr: [self repository packageExtension]
%

category: 'reading'
set compile_env: 0
method: CypressAbstractPackageReader
readClassCommentFromDirectoryEntries: entries

	self fileUtils readStreamFor: (entries
				detect: [:entry | entry endsWith: '/README.md']
				ifNone: [^''])
		do: [:fileStream | ^fileStream contents]
%

category: 'reading'
set compile_env: 0
method: CypressAbstractPackageReader
readClassPropertiesFromDirectoryEntries: entries

	self fileUtils readStreamFor: (entries
				detect: [:entry | entry endsWith: '/properties', self propertiesFileNameExtension]
				ifNone: [^Dictionary new])
		do: [:fileStream | ^CypressJsonParser parseStream: fileStream]
%

category: 'reading'
set compile_env: 0
method: CypressAbstractPackageReader
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
	^classStructure
%

category: 'reading'
set compile_env: 0
method: CypressAbstractPackageReader
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
				yourself)
%

category: 'reading'
set compile_env: 0
method: CypressAbstractPackageReader
readExtensionClassStructureFromEntry: classEntry

	| classDirectory classPropertiesDict classComment entries classStructure |
	classDirectory := classEntry.
	entries := self fileUtils directoryEntriesFrom: classDirectory.
	classPropertiesDict := self
				readClassPropertiesFromDirectoryEntries: entries.
	classStructure := self classStructureFrom: classPropertiesDict.
	self readMethodStructureFor: classStructure in: entries.
	^classStructure
%

category: 'reading'
set compile_env: 0
method: CypressAbstractPackageReader
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
%

category: 'reading'
set compile_env: 0
method: CypressAbstractPackageReader
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
											methods: methods]]]]
%

category: 'reading'
set compile_env: 0
method: CypressAbstractPackageReader
readMethodStructureFrom: fileStream intoClassStructure: classStructure meta: isMeta methods: methods

	self subclassResponsibility: #readMethodStructureFrom:intoClassStructure:meta:methods:
%

category: 'reading'
set compile_env: 0
method: CypressAbstractPackageReader
readPackageStructure

   (self fileUtils directoryEntriesFrom: self packageDirectory)
        do: [ :entry | 
		(self isPropertiesFileDirectoryEntry: entry)
			ifTrue: [ self packageStructure properties: (self readPropertiesFile: entry) ].
            (entry endsWith: '.class')
                ifTrue: [ self packageStructure classes add: (self readClassStructureFromEntry: entry) ].
            (entry endsWith: '.extension')
                ifTrue: [ self packageStructure extensions add: (self readExtensionClassStructureFromEntry: entry) ] ]
%

category: 'reading'
set compile_env: 0
method: CypressAbstractPackageReader
readPackageStructureForPackageNamed: packageName

	| structureName |
	structureName := packageName , self repository packageExtension.
	self
		packageStructure: (CypressPackageStructure named: structureName);
		packageDirectory: (self fileUtils directoryFromPath: structureName
					relativeTo: self repository directoryPath);
		readPackageStructure
%

category: 'reading'
set compile_env: 0
method: CypressAbstractPackageReader
readPropertiesFile: entry

	self fileUtils
		readStreamFor: entry
		do: [:fileStream | ^CypressJsonParser parseStream: fileStream]
%

! Class Implementation for CypressFileTreeFormatPackageReader

! ------------------- Instance methods for CypressFileTreeFormatPackageReader

category: 'accessing'
set compile_env: 0
method: CypressFileTreeFormatPackageReader
propertiesFileNameExtension

	^'.json'
%

category: 'reading'
set compile_env: 0
method: CypressFileTreeFormatPackageReader
readMethodStructureFrom: fileStream intoClassStructure: classStructure meta: isMeta methods: methods
	"Strict!"

	self
		readFileTreeFormatMethodStructureFrom: fileStream
		intoClassStructure: classStructure
		meta: isMeta
		methods: methods
%

! Class Implementation for CypressFlexiblePackageReader

! ------------------- Instance methods for CypressFlexiblePackageReader

category: 'reading'
set compile_env: 0
method: CypressFlexiblePackageReader
isPropertiesFileDirectoryEntry: entry
	"Expect .ston properties file, but tolerate .json if present."

	^(super isPropertiesFileDirectoryEntry: entry)
		or: [entry endsWith: '/properties.json']
%

category: 'reading'
set compile_env: 0
method: CypressFlexiblePackageReader
readClassPropertiesFromDirectoryEntries: entries
	"Look for Cypress .ston properties file first, but accept a .json version if present."

	self fileUtils readStreamFor: (entries
				detect: [:entry | entry endsWith: '/properties' , self propertiesFileNameExtension]
				ifNone: 
					[entries detect: [:entry | entry endsWith: '/properties.json']
						ifNone: [^Dictionary new]])
		do: [:fileStream | ^CypressJsonParser parseStream: fileStream]
%

category: 'reading'
set compile_env: 0
method: CypressFlexiblePackageReader
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
				methods: methods]
%

! Class Implementation for CypressPackageReader

! ------------------- Instance methods for CypressPackageReader

category: 'reading'
set compile_env: 0
method: CypressPackageReader
readMethodStructureFrom: fileStream intoClassStructure: classStructure meta: isMeta methods: methods
	"Strict!"

	self
		readCypressFormatMethodStructureFrom: fileStream
		intoClassStructure: classStructure
		meta: isMeta
		methods: methods
%

! Class Implementation for CypressFileUtilities

! ------------------- Class methods for CypressFileUtilities

category: 'accessing'
set compile_env: 0
classmethod: CypressFileUtilities
current

	^Current
%

category: 'utilities'
set compile_env: 0
classmethod: CypressFileUtilities
deleteAll: aDirectory

	self subclassResponsibility
%

category: 'utilities'
set compile_env: 0
classmethod: CypressFileUtilities
directoryEntriesFrom: aDirectory

	self subclassResponsibility
%

category: 'utilities'
set compile_env: 0
classmethod: CypressFileUtilities
directoryExists: aDirectory

	self subclassResponsibility
%

category: 'utilities'
set compile_env: 0
classmethod: CypressFileUtilities
directoryFromPath: directoryPath relativeTo: aDirectory

	self subclassResponsibility
%

category: 'utilities'
set compile_env: 0
classmethod: CypressFileUtilities
ensureDirectoryExists: aDirectory

	self subclassResponsibility
%

category: 'initializating'
set compile_env: 0
classmethod: CypressFileUtilities
install

	Current := self
%

category: 'utilities'
set compile_env: 0
classmethod: CypressFileUtilities
localNameFrom: aDirectory

	self subclassResponsibility
%

category: 'utilities'
set compile_env: 0
classmethod: CypressFileUtilities
pathNameDelimiter

	self subclassResponsibility
%

category: 'utilities'
set compile_env: 0
classmethod: CypressFileUtilities
readStreamFor: filePath do: aOneArgBlock

	self subclassResponsibility
%

category: 'utilities'
set compile_env: 0
classmethod: CypressFileUtilities
readStreamFor: filePath in: aDirectory do: aOneArgBlock

	self subclassResponsibility
%

category: 'utilities'
set compile_env: 0
classmethod: CypressFileUtilities
writeStreamFor: filePath in: aDirectory do: aOneArgBlock

	self subclassResponsibility
%

! Class Implementation for CypressGemStoneDirectoryUtilities

! ------------------- Class methods for CypressGemStoneDirectoryUtilities

category: 'utilities'
set compile_env: 0
classmethod: CypressGemStoneDirectoryUtilities
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
									ifFalse: [self deleteAll: filename]]]]
%

category: 'utilities'
set compile_env: 0
classmethod: CypressGemStoneDirectoryUtilities
directoryEntriesFrom: aDirectory
	"Answer fully qualified paths to the contents of aDirectory."

	^(GsFile contentsOfDirectory: aDirectory onClient: false) ifNil: [#()]
%

category: 'utilities'
set compile_env: 0
classmethod: CypressGemStoneDirectoryUtilities
directoryExists: aDirectory

	^GsFile existsOnServer: aDirectory
%

category: 'utilities'
set compile_env: 0
classmethod: CypressGemStoneDirectoryUtilities
directoryFromPath: directoryPath relativeTo: aDirectory

	^((aDirectory endsWith: self pathNameDelimiter) or: [directoryPath beginsWith: self pathNameDelimiter])
		ifTrue: [aDirectory, directoryPath]
		ifFalse: [aDirectory, self pathNameDelimiter, directoryPath]
%

category: 'utilities'
set compile_env: 0
classmethod: CypressGemStoneDirectoryUtilities
ensureDirectoryExists: aDirectory

	| lastSeparator |
	(GsFile existsOnServer: aDirectory) ifTrue: [^aDirectory].
	(GsFile createServerDirectory: aDirectory) ifNotNil: [^aDirectory].
	lastSeparator := aDirectory findLastSubString: self pathNameDelimiter startingAt: aDirectory size.
	lastSeparator <= 1 ifTrue: [self error: 'Cannot create directory'].
	self ensureDirectoryExists: (aDirectory copyFrom: 1 to: lastSeparator - 1).
	self ensureDirectoryExists: aDirectory.
%

category: 'initializating'
set compile_env: 0
classmethod: CypressGemStoneDirectoryUtilities
initialize
	"self initialize"

	self install
%

category: 'utilities'
set compile_env: 0
classmethod: CypressGemStoneDirectoryUtilities
localNameFrom: aDirectory

	| endOfPath |
	endOfPath := aDirectory findLastSubString: self pathNameDelimiter startingAt: aDirectory size.
	^aDirectory copyFrom: endOfPath + 1 to: aDirectory size
%

category: 'utilities'
set compile_env: 0
classmethod: CypressGemStoneDirectoryUtilities
pathNameDelimiter

	^'/'
%

category: 'utilities'
set compile_env: 0
classmethod: CypressGemStoneDirectoryUtilities
readStreamFor: filePath do: aOneArgBlock

	| file stream |
	GsFile serverErrorString.
	file := GsFile openReadOnServer: filePath.
	GsFile serverErrorString ifNotNil: [:errorMessage | self error: errorMessage].
	[stream := ReadStream on: (String withAll: file contents asByteArray decodeFromUTF8).
	aOneArgBlock value: stream] ensure: [file close]
%

category: 'utilities'
set compile_env: 0
classmethod: CypressGemStoneDirectoryUtilities
readStreamFor: filePath in: aDirectory do: aOneArgBlock

	self
		readStreamFor: (self directoryFromPath: filePath relativeTo: aDirectory)
		do: aOneArgBlock
%

category: 'utilities'
set compile_env: 0
classmethod: CypressGemStoneDirectoryUtilities
writeStreamFor: filePath in: aDirectory do: aOneArgBlock

	| file stream |
	GsFile serverErrorString.
	file := GsFile openWriteOnServer: (self directoryFromPath: filePath relativeTo: aDirectory).
	GsFile serverErrorString ifNotNil: [:errorMessage | self error: errorMessage].
	stream := WriteStream on: String new.
	[aOneArgBlock value: stream] ensure: [file nextPutAll: stream contents encodeAsUTF8; close]
%

! Class Implementation for CypressVersionReference

! ------------------- Class methods for CypressVersionReference

category: 'instance creation'
set compile_env: 0
classmethod: CypressVersionReference
name: aString

	^(self basicNew)
		initializeName: aString;
		yourself
%

category: 'instance creation'
set compile_env: 0
classmethod: CypressVersionReference
new

	self error: 'Use #name: to initialize the receiver.'
%

! ------------------- Instance methods for CypressVersionReference

category: 'comparing'
set compile_env: 0
method: CypressVersionReference
= aReference

	^self class = aReference class
		and: [self name = aReference name]
%

category: 'accessing'
set compile_env: 0
method: CypressVersionReference
author
	"Answer the author of the receiver."
	
	^ author
%

category: 'accessing'
set compile_env: 0
method: CypressVersionReference
branch
	"Answer the branch of the receiver."
	
	^ branch
%

category: 'comparing'
set compile_env: 0
method: CypressVersionReference
hash

	^self name hash
%

category: 'initialization'
set compile_env: 0
method: CypressVersionReference
initializeName: aString

	name := aString.
	self parseName: aString
%

category: 'private'
set compile_env: 0
method: CypressVersionReference
matches: aResolvedReference
	^ self name = aResolvedReference name
%

category: 'accessing'
set compile_env: 0
method: CypressVersionReference
name
	"Answer the name of this reference."
	
	^ name
%

category: 'accessing'
set compile_env: 0
method: CypressVersionReference
packageName
	"Answer the package of the receiver."

	^ package
%

category: 'initialization'
set compile_env: 0
method: CypressVersionReference
parseName: aString
	| basicName |
	basicName := (aString isEmpty or: [aString last isDigit or: [(aString includes: $() not]])
		ifTrue: [ aString ]
		ifFalse: [ (aString copyUpToLast: $.) copyUpTo: $( ].
	package := basicName copyUpToLast: $-.
	(package includes: $.)
		ifFalse: [ branch := '' ]
		ifTrue: [
			branch := package copyAfter: $..
			package := package copyUpTo: $. ].
	author := (basicName copyAfterLast: $-) copyUpToLast: $..
	versionNumber := (basicName copyAfterLast: $-) copyAfterLast: $..
	(versionNumber notEmpty and: [ versionNumber allSatisfy: [ :each | each isDigit ] ])
		ifTrue: [ versionNumber := versionNumber asInteger ]
		ifFalse: [ versionNumber := 0 ]
%

category: 'printing'
set compile_env: 0
method: CypressVersionReference
printOn: aStream

	super printOn: aStream.
	aStream nextPutAll: ' name: '.
	self name printOn: aStream
%

category: 'accessing'
set compile_env: 0
method: CypressVersionReference
versionNumber
	"Answer the version of the receiver."

	^ versionNumber
%

! Class Extensions

! Class initializers 

doit
CypressGemStoneDirectoryUtilities initialize.
%



! End of Package: Cypress-GemStoneFileServer


