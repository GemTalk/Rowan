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
	subclass: 'CypressPackageWriter'
	instVarNames: #( packageDirectory packageStructure rootDirectory )
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
(Object
	subclass: 'CypressPackageReader'
	instVarNames: #( packageDirectory packageStructure )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-GemStoneFileServer';
		comment: '';
		immediateInvariant.
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

! Class Implementation for CypressPackageWriter

! ------------------- Class methods for CypressPackageWriter

category: 'initialization'
set compile_env: 0
classmethod: CypressPackageWriter
initializeSpecials

	| map |
	map := Dictionary new.
	map
		at: $+ put: 'plus';
		at: $- put: 'minus';
		at: $= put: 'equals';
		at: $< put: 'less';
		at: $> put: 'more';
		at: $% put: 'percent';
		at: $& put: 'and';
		at: $| put: 'pipe';
		at: $* put: 'star';
		at: $/ put: 'slash';
		at: $\ put: 'backslash';
		at: $~ put: 'tilde';
		at: $? put: 'wat';
		at: $@ put: 'at'.
	map keys do: [:key | map at: (map at: key) put: key].
	^map
%

category: 'accessing'
set compile_env: 0
classmethod: CypressPackageWriter
specials

	^specials ifNil: [specials := self initializeSpecials]
%

category: 'instance creation'
set compile_env: 0
classmethod: CypressPackageWriter
writePackageStructure: aPackageStructure to: aPackagesDirectory

	^(self new)
		packageStructure: aPackageStructure;
		rootDirectory: aPackagesDirectory;
		write
%

! ------------------- Instance methods for CypressPackageWriter

category: 'private'
set compile_env: 0
method: CypressPackageWriter
directoryForDirectoryNamed: directoryNameOrPath

	^directoryNameOrPath = '.'
		ifTrue: [self fileUtils ensureDirectoryExists: self packageDirectory]
		ifFalse: [self subPackageFileDirectoryFor: directoryNameOrPath]
%

category: 'private'
set compile_env: 0
method: CypressPackageWriter
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
method: CypressPackageWriter
fileUtils

	^CypressGemStoneDirectoryUtilities current
%

category: 'accessing'
set compile_env: 0
method: CypressPackageWriter
packageDirectory

	packageDirectory
		ifNil: 
			[packageDirectory := self fileUtils ensureDirectoryExists: (self fileUtils
								directoryFromPath: self packageStructure name
								relativeTo: self rootDirectory)].
	^packageDirectory
%

category: 'accessing'
set compile_env: 0
method: CypressPackageWriter
packageDirectory: aPackageDirectory

	packageDirectory := aPackageDirectory
%

category: 'accessing'
set compile_env: 0
method: CypressPackageWriter
packageStructure

	^packageStructure
%

category: 'accessing'
set compile_env: 0
method: CypressPackageWriter
packageStructure: aCypressPackageStructure

	packageStructure := aCypressPackageStructure
%

category: 'accessing'
set compile_env: 0
method: CypressPackageWriter
rootDirectory

	^rootDirectory
%

category: 'accessing'
set compile_env: 0
method: CypressPackageWriter
rootDirectory: aDirectory

	rootDirectory := aDirectory
%

category: 'private'
set compile_env: 0
method: CypressPackageWriter
subPackageFileDirectoryFor: directoryNameOrPath

	| dir |
	dir := self fileUtils directoryFromPath: directoryNameOrPath
				relativeTo: self packageDirectory.
	self fileUtils ensureDirectoryExists: dir.
	^dir
%

category: 'writing'
set compile_env: 0
method: CypressPackageWriter
write

	(self fileUtils directoryExists: self packageDirectory)
		ifTrue: [self fileUtils deleteAll: self packageDirectory].
	self writePropertiesFile.
	self writePackageStructure
%

category: 'writing'
set compile_env: 0
method: CypressPackageWriter
writeClassComment: classStructure on: fileStream

	fileStream nextPutAll: classStructure comment withUnixLineEndings
%

category: 'writing'
set compile_env: 0
method: CypressPackageWriter
writeClassStructure: classStructure on: fileStream

	classStructure properties writeCypressJsonOn: fileStream
%

category: 'writing'
set compile_env: 0
method: CypressPackageWriter
writeClassStructure: classStructure to: classPath

	self
		writeInDirectoryName: classPath
			fileName: 'README'
			extension: '.md'
			visit: [:fileStream | self writeClassComment: classStructure on: fileStream];
		writeInDirectoryName: classPath
			fileName: 'properties'
			extension: '.json'
			visit: [:fileStream | self writeClassStructure: classStructure on: fileStream]
%

category: 'writing'
set compile_env: 0
method: CypressPackageWriter
writeExtensionClassStructure: classStructure to: classPath

     self
        writeInDirectoryName: classPath
        fileName: 'properties'
        extension: '.json'
        visit: [:fileStream |  | properties |
    		properties := Dictionary new.
    		properties at: 'name' put: classStructure className.
    		properties writeCypressJsonOn: fileStream ]
%

category: 'private'
set compile_env: 0
method: CypressPackageWriter
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
method: CypressPackageWriter
writeMethodStructure: methodStructure to: methodPath

	| filename |
	filename := self fileNameForSelector: methodStructure selector.
	self
		writeInDirectoryName: methodPath
		fileName: filename
		extension: '.st'
		visit: 
			[:fileStream |
			fileStream
				nextPutAll: methodStructure category;
				lf;
				nextPutAll: methodStructure source withUnixLineEndings]
%

category: 'writing'
set compile_env: 0
method: CypressPackageWriter
writePackageStructure

	self
		writePackageStructureClasses: self packageStructure classes
			isClassExtension: false;
		writePackageStructureClasses: self packageStructure extensions
			isClassExtension: true
%

category: 'writing'
set compile_env: 0
method: CypressPackageWriter
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
method: CypressPackageWriter
writePropertiesFile

	self
		writeInDirectoryName: '.'
		fileName: 'properties'
		extension: '.json'
		visit: [:fileStream | Dictionary new writeCypressJsonOn: fileStream]
%

! Class Implementation for CypressPackageReader

! ------------------- Class methods for CypressPackageReader

category: 'unknown'
set compile_env: 0
classmethod: CypressPackageReader
readPackageStructureForPackageNamed: packageName from: aDirectory

	^self readPackageStructureFrom: aDirectory, packageName, '.package'
%

category: 'instance creation'
set compile_env: 0
classmethod: CypressPackageReader
readPackageStructureFrom: aPackageDirectory

	^(self new)
		packageDirectory: aPackageDirectory;
		read;
		yourself
%

! ------------------- Instance methods for CypressPackageReader

category: 'private'
set compile_env: 0
method: CypressPackageReader
classStructureFrom: classPropertiesDict

	^(CypressClassStructure new)
		packageStructure: self packageStructure;
		isClassExtension: true;
		properties: classPropertiesDict;
		yourself
%

category: 'private'
set compile_env: 0
method: CypressPackageReader
classStructureFrom: classPropertiesDict comment: classComment

	^(self classStructureFrom: classPropertiesDict)
		isClassExtension: false;
		comment: classComment;
		yourself
%

category: 'private'
set compile_env: 0
method: CypressPackageReader
fileUtils

	^CypressGemStoneDirectoryUtilities current
%

category: 'accessing'
set compile_env: 0
method: CypressPackageReader
packageDirectory

	^packageDirectory
%

category: 'accessing'
set compile_env: 0
method: CypressPackageReader
packageDirectory: aDirectory

	packageDirectory := aDirectory
%

category: 'accessing'
set compile_env: 0
method: CypressPackageReader
packageStructure

	^packageStructure
%

category: 'accessing'
set compile_env: 0
method: CypressPackageReader
packageStructure: aPackageStructure

	packageStructure := aPackageStructure
%

category: 'reading'
set compile_env: 0
method: CypressPackageReader
read

	self readPackageStructure
%

category: 'reading'
set compile_env: 0
method: CypressPackageReader
readClassCommentFromDirectoryEntries: entries

	self fileUtils readStreamFor: (entries
				detect: [:entry | entry endsWith: '/README.md']
				ifNone: [^''])
		do: [:fileStream | ^fileStream contents]
%

category: 'reading'
set compile_env: 0
method: CypressPackageReader
readClassPropertiesDictFromDirectoryEntries: entries

	self fileUtils readStreamFor: (entries
				detect: [:entry | entry endsWith: '/properties.json']
				ifNone: [^Dictionary new])
		do: [:fileStream | ^CypressJsonParser parseStream: fileStream]
%

category: 'reading'
set compile_env: 0
method: CypressPackageReader
readClassStructureFromEntry: classEntry

	| classDirectory classPropertiesDict classComment entries classStructure |
	classDirectory := classEntry.
	entries := (self fileUtils directoryEntriesFrom: classDirectory).
	classPropertiesDict := self
				readClassPropertiesDictFromDirectoryEntries: entries.
	classComment := self readClassCommentFromDirectoryEntries: entries.
	classStructure := self classStructureFrom: classPropertiesDict
				comment: classComment.
	self readMethodStructureFor: classStructure in: entries.
	^classStructure
%

category: 'reading'
set compile_env: 0
method: CypressPackageReader
readExtensionClassStructureFromEntry: classEntry

	| classDirectory classPropertiesDict classComment entries classStructure |
	classDirectory := classEntry.
	entries := (self fileUtils directoryEntriesFrom: classDirectory).
	classPropertiesDict := self
				readClassPropertiesDictFromDirectoryEntries: entries.
	classStructure := self classStructureFrom: classPropertiesDict.
	self readMethodStructureFor: classStructure in: entries.
	^classStructure
%

category: 'reading'
set compile_env: 0
method: CypressPackageReader
readMethodStructureFor: classStructure in: entries

    entries
        do: [ :entry | 
            | methods isMeta |
 		methods := (isMeta := entry endsWith: '/class')
                ifTrue: [ classStructure classMethods ]
		    ifFalse: [ classStructure instanceMethods ].
            ((entry endsWith: '/instance') or: [entry endsWith: '/class' ])
                ifTrue: [ 
                    ((self fileUtils directoryEntriesFrom: entry) select: [ :each | each endsWith: '.st' ])
                        do: [ :methodEntry | 
                            self fileUtils readStreamFor: methodEntry
                                do: [ :fileStream | 
                                    | category source selector |
                                    category := fileStream nextLine trimSeparators.
                                    source := fileStream upToEnd.
						selector := UndefinedObject parseSelectorFrom: source.
                                     methods 
							at: selector
							put: ((CypressMethodStructure new)
									packageStructure: self packageStructure;
									classStructure: classStructure;
									name: selector;
									isMetaclass: isMeta;
									selector: selector;
									category: category;
									source: source;
									yourself) ] ] ] ]
%

category: 'reading'
set compile_env: 0
method: CypressPackageReader
readPackageStructure

   packageStructure := CypressPackageStructure named: (self fileUtils localNameFrom: self packageDirectory).
   (self fileUtils directoryEntriesFrom: self packageDirectory)
        do: [ :entry | 
		(entry endsWith: '/properties.json')
			ifTrue: [ self packageStructure properties: (self readPropertiesFile: entry) ].
            (entry endsWith: '.class')
                ifTrue: [ self packageStructure classes add: (self readClassStructureFromEntry: entry) ].
            (entry endsWith: '.extension')
                ifTrue: [ self packageStructure extensions add: (self readExtensionClassStructureFromEntry: entry) ] ]
%

category: 'reading'
set compile_env: 0
method: CypressPackageReader
readPropertiesFile: entry

	self fileUtils
		readStreamFor: entry
		do: [:fileStream | ^CypressJsonParser parseStream: fileStream]
%

! Class Extensions

! Class Extension for Behavior

! ------------------- Instance methods for Behavior

category: '*Cypress-GemStoneFileServer'
set compile_env: 0
method: Behavior
parseSelectorFrom: methodString

	| meth |
	meth := self
				_parseMethod: methodString
				category: #'xyzzy'
				using: GsSession currentSession symbolList
				environmentId: 0.
	meth class ~~ GsNMethod
		ifTrue: 
			["if error slot is nil, then the method wasn't compiled because of errors"
			(meth at: 2) == nil ifFalse: [^nil].
			meth := meth at: 1].
	^meth selector asString
%

category: '*Cypress-GemStoneFileServer'
set compile_env: 0
method: Behavior
_parseMethod: source category: cat using: aSymbolList environmentId: anEnvironmentId
	"Compiles the method into disposable dictionaries, if possible.
	 Attempts auto-recompile for undefinedSymbols.
	 Returns the compiled method or signals a CompileError."

	| undefinedSymbolList undefinedSymbols |
	undefinedSymbols := SymbolDictionary new name: #UndefinedSymbols.
	undefinedSymbolList := SymbolList with: undefinedSymbols.
	^
	[self
		compileMethod: source
		dictionaries: aSymbolList
		category: cat
	       intoMethodDict: GsMethodDictionary new
	       intoCategories: GsMethodDictionary new  
	       intoPragmas: nil
		environmentId: anEnvironmentId]
			onSynchronous: (Array with: CompileError with: CompileWarning)
			do: (Array with: 
						[:ex |
						| undefSymbol symbols |
						undefSymbol := true.
						symbols := Array new.
						ex errorDetails do: 
								[:errArray |
								(errArray atOrNil: 1) == 1031
									ifTrue: [symbols add: (errArray atOrNil: 5) asSymbol]
									ifFalse: [undefSymbol := false]].
						undefSymbol
							ifTrue: 
								["attempt auto-define of undefined symbols"
								symbols do: [:sym | undefinedSymbols at: sym put: nil].
								
								[^self
									compileMethod: source
									dictionaries: aSymbolList , undefinedSymbolList
									category: cat
								       intoMethodDict: GsMethodDictionary new
								       intoCategories: GsMethodDictionary new
								       intoPragmas: nil
									environmentId: anEnvironmentId]
										onException: CompileError
										do: [:exb | undefSymbol := false]].
						undefSymbol ifFalse: [ex outer]]
					with: [:ex | ex resume])
%

! Class Extension for GsFile

! ------------------- Instance methods for GsFile

category: '*Cypress-GemStoneFileServer'
set compile_env: 0
method: GsFile
tab: anInteger

	anInteger timesRepeat: [self tab]
%

category: '*Cypress-GemStoneFileServer'
set compile_env: 0
method: GsFile
upToEnd

	^self next: (self fileSize - self positionA)
%

! ------------------- Class initializers 

doit
CypressGemStoneDirectoryUtilities initialize.
%



! End of Package: Cypress-GemStoneFileServer


