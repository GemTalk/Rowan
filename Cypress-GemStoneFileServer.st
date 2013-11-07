SystemOrganization addCategory: 'Cypress-GemStoneFileServer'!

Object subclass: #CypressAbstractPackageFiler
	instanceVariableNames: 'repository packageDirectory packageStructure'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-GemStoneFileServer'!

!CypressAbstractPackageFiler class methodsFor: 'instance creation' stamp: ''!
forRepository: aCypressFileSystemRepository	^self new		initializeForRepository: aCypressFileSystemRepository;		yourself.! !

!CypressAbstractPackageFiler methodsFor: 'private' stamp: ''!
fileUtils	^CypressFileUtilities current! !

!CypressAbstractPackageFiler methodsFor: 'initializing - private' stamp: ''!
initializeForRepository: aCypressFileSystemRepository	repository := aCypressFileSystemRepository! !

!CypressAbstractPackageFiler methodsFor: 'accessing' stamp: ''!
packageDirectory	^packageDirectory! !

!CypressAbstractPackageFiler methodsFor: 'accessing' stamp: ''!
packageDirectory: aDirectory	packageDirectory := aDirectory! !

!CypressAbstractPackageFiler methodsFor: 'accessing' stamp: ''!
packageStructure	^packageStructure! !

!CypressAbstractPackageFiler methodsFor: 'accessing' stamp: ''!
packageStructure: aPackageStructure	packageStructure := aPackageStructure! !

!CypressAbstractPackageFiler methodsFor: 'accessing' stamp: ''!
propertiesFileNameExtension	^'.ston'! !

!CypressAbstractPackageFiler methodsFor: 'accessing' stamp: ''!
propertiesFileNameExtensions	^Array with: self propertiesFileNameExtension! !

!CypressAbstractPackageFiler methodsFor: 'accessing' stamp: ''!
repository	^repository! !

CypressAbstractPackageFiler subclass: #CypressAbstractPackageReader
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-GemStoneFileServer'!

!CypressAbstractPackageReader methodsFor: 'private' stamp: ''!
classStructureFrom: classPropertiesDict	^(CypressClassStructure new)		packageStructure: self packageStructure;		isClassExtension: true;		properties: classPropertiesDict;		yourself! !

!CypressAbstractPackageReader methodsFor: 'private' stamp: ''!
classStructureFrom: classPropertiesDict comment: classComment	^(self classStructureFrom: classPropertiesDict)		isClassExtension: false;		comment: classComment;		yourself! !

!CypressAbstractPackageReader methodsFor: 'reading' stamp: ''!
isPropertiesFileDirectoryEntry: entry	^self propertiesFileNameExtensions		anySatisfy: [:each | entry endsWith: '/properties' , each]! !

!CypressAbstractPackageReader methodsFor: 'accessing' stamp: ''!
packageExtension	^self packageStructure		packageExtensionOr: [self repository packageExtension]! !

!CypressAbstractPackageReader methodsFor: 'reading' stamp: ''!
readClassCommentFromDirectoryEntries: entries	self fileUtils readStreamFor: (entries				detect: [:entry | entry endsWith: '/README.md']				ifNone: [^''])		do: [:fileStream | ^fileStream contents]! !

!CypressAbstractPackageReader methodsFor: 'reading' stamp: ''!
readClassPropertiesFromDirectoryEntries: entries	self fileUtils readStreamFor: (entries				detect: [:entry | self isPropertiesFileDirectoryEntry: entry]				ifNone: [^Dictionary new])		do: [:fileStream | ^CypressJsonParser parseStream: fileStream]! !

!CypressAbstractPackageReader methodsFor: 'reading' stamp: ''!
readClassStructureFromEntry: classEntry	| classDirectory classPropertiesDict classComment entries classStructure |	classDirectory := classEntry.	entries := self fileUtils directoryEntriesFrom: classDirectory.	classPropertiesDict := self				readClassPropertiesFromDirectoryEntries: entries.	classComment := self readClassCommentFromDirectoryEntries: entries.	classStructure := self classStructureFrom: classPropertiesDict				comment: classComment.	self readMethodStructureFor: classStructure in: entries.	^classStructure! !

!CypressAbstractPackageReader methodsFor: 'reading' stamp: ''!
readCypressFormatMethodStructureFrom: fileStream intoClassStructure: classStructure meta: isMeta methods: methods	| notice category source selector |	(fileStream peekFor: $")		ifTrue: [fileStream nextLine]		ifFalse: [self error: 'Method does not have valid Cypress format'].	(fileStream match: 'notice: ')		ifTrue: [notice := fileStream nextLine trimSeparators]		ifFalse: [self error: 'Method does not have valid Cypress format'].	(fileStream match: 'category: ')		ifTrue: [category := fileStream nextLine trimSeparators]		ifFalse: [self error: 'Method does not have valid Cypress format'].	(fileStream peekFor: $")		ifTrue: [fileStream nextLine]		ifFalse: [self error: 'Method does not have valid Cypress format'].	source := fileStream upToEnd.	selector := UndefinedObject parseSelectorFrom: source.	methods at: selector		put: ((CypressMethodStructure new)				packageStructure: self packageStructure;				classStructure: classStructure;				name: selector;				isMetaclass: isMeta;				selector: selector;				category: category;				source: source;				yourself)! !

!CypressAbstractPackageReader methodsFor: 'reading' stamp: ''!
readExtensionClassStructureFromEntry: classEntry	| classDirectory classPropertiesDict classComment entries classStructure |	classDirectory := classEntry.	entries := self fileUtils directoryEntriesFrom: classDirectory.	classPropertiesDict := self				readClassPropertiesFromDirectoryEntries: entries.	classStructure := self classStructureFrom: classPropertiesDict.	self readMethodStructureFor: classStructure in: entries.	^classStructure! !

!CypressAbstractPackageReader methodsFor: 'reading' stamp: ''!
readFileTreeFormatMethodStructureFrom: fileStream intoClassStructure: classStructure meta: isMeta methods: methods	| category source selector |	category := fileStream nextLine trimSeparators.	source := fileStream upToEnd.	selector := UndefinedObject parseSelectorFrom: source.	methods at: selector		put: ((CypressMethodStructure new)				packageStructure: self packageStructure;				classStructure: classStructure;				name: selector;				isMetaclass: isMeta;				selector: selector;				category: category;				source: source;				yourself)! !

!CypressAbstractPackageReader methodsFor: 'reading' stamp: ''!
readMethodStructureFor: classStructure in: entries	entries do: 			[:entry |			| methods isMeta |			methods := (isMeta := entry endsWith: '/class')						ifTrue: [classStructure classMethods]						ifFalse: [classStructure instanceMethods].			((entry endsWith: '/instance') or: [entry endsWith: '/class'])				ifTrue: 					[((self fileUtils directoryEntriesFrom: entry)						select: [:each | each endsWith: '.st']) do: 								[:methodEntry |								self fileUtils readStreamFor: methodEntry									do: 										[:fileStream |										self											readMethodStructureFrom: fileStream											intoClassStructure: classStructure											meta: isMeta											methods: methods]]]]! !

!CypressAbstractPackageReader methodsFor: 'reading' stamp: ''!
readMethodStructureFrom: fileStream intoClassStructure: classStructure meta: isMeta methods: methods	self subclassResponsibility: #readMethodStructureFrom:intoClassStructure:meta:methods:! !

!CypressAbstractPackageReader methodsFor: 'reading' stamp: ''!
readPackageStructure   (self fileUtils directoryEntriesFrom: self packageDirectory)        do: [ :entry | 		(self isPropertiesFileDirectoryEntry: entry)			ifTrue: [ self packageStructure properties: (self readPropertiesFile: entry) ].            (entry endsWith: '.class')                ifTrue: [ self packageStructure classes add: (self readClassStructureFromEntry: entry) ].            (entry endsWith: '.extension')                ifTrue: [ self packageStructure extensions add: (self readExtensionClassStructureFromEntry: entry) ] ]! !

!CypressAbstractPackageReader methodsFor: 'reading' stamp: ''!
readPackageStructureForPackageNamed: packageName	| structureName |	structureName := packageName , self repository packageExtension.	self		packageStructure: (CypressPackageStructure named: structureName);		packageDirectory: (self fileUtils directoryFromPath: structureName					relativeTo: self repository directoryPath);		readPackageStructure! !

!CypressAbstractPackageReader methodsFor: 'reading' stamp: ''!
readPropertiesFile: entry	self fileUtils		readStreamFor: entry		do: [:fileStream | ^CypressJsonParser parseStream: fileStream]! !

CypressAbstractPackageReader subclass: #CypressFileTreeFormatPackageReader
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-GemStoneFileServer'!

!CypressFileTreeFormatPackageReader methodsFor: 'accessing' stamp: ''!
propertiesFileNameExtension	^'.json'! !

!CypressFileTreeFormatPackageReader methodsFor: 'accessing' stamp: ''!
propertiesFileNameExtensions	^Array		with: super propertiesFileNameExtension		with: self propertiesFileNameExtension! !

!CypressFileTreeFormatPackageReader methodsFor: 'reading' stamp: ''!
readMethodStructureFrom: fileStream intoClassStructure: classStructure meta: isMeta methods: methods	"Strict!!"	self		readFileTreeFormatMethodStructureFrom: fileStream		intoClassStructure: classStructure		meta: isMeta		methods: methods! !

CypressAbstractPackageReader subclass: #CypressFlexiblePackageReader
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-GemStoneFileServer'!

!CypressFlexiblePackageReader methodsFor: 'reading' stamp: ''!
isPropertiesFileDirectoryEntry: entry	"Expect .ston properties file, but tolerate .json if present."	^(super isPropertiesFileDirectoryEntry: entry)		or: [entry endsWith: '/properties.json']! !

!CypressFlexiblePackageReader methodsFor: 'reading' stamp: ''!
readMethodStructureFrom: fileStream intoClassStructure: classStructure meta: isMeta methods: methods	"If the stream begins with a double quote, process it on the assumption it is a Cypress-format method.	 Otherwise, treat it as a FileTree-format method."	fileStream peek = $"		ifTrue: 			[self				readCypressFormatMethodStructureFrom: fileStream				intoClassStructure: classStructure				meta: isMeta				methods: methods]		ifFalse: 			[self				readFileTreeFormatMethodStructureFrom: fileStream				intoClassStructure: classStructure				meta: isMeta				methods: methods]! !

CypressAbstractPackageReader subclass: #CypressPackageReader
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-GemStoneFileServer'!

!CypressPackageReader methodsFor: 'reading' stamp: ''!
readMethodStructureFrom: fileStream intoClassStructure: classStructure meta: isMeta methods: methods	"Strict!!"	self		readCypressFormatMethodStructureFrom: fileStream		intoClassStructure: classStructure		meta: isMeta		methods: methods! !

CypressAbstractPackageFiler subclass: #CypressAbstractPackageWriter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-GemStoneFileServer'!

CypressAbstractPackageWriter class
	instanceVariableNames: 'specials'!

!CypressAbstractPackageWriter class methodsFor: 'initialization' stamp: ''!
initializeSpecials	"Valid binarySelector characters  '!!' | '%' | '&' | '*' | '+' | ','' | '/' | '<' | '=' | '>' | '?' | '@' | '\' | '~' | '|' | '-'"	| map |	map := Dictionary new.	map		at: $!! put: 'bang';		at: $% put: 'percent';		at: $& put: 'and';		at: $* put: 'star';		at: $+ put: 'plus';		at: $, put: 'comma';		at: $- put: 'minus';		at: $/ put: 'slash';		at: $< put: 'less';		at: $= put: 'equals';		at: $> put: 'more';		at: $? put: 'wat';		at: $@ put: 'at';		at: $\ put: 'backslash';		at: $| put: 'pipe';		at: $~ put: 'tilde'.	map keys do: [:key | map at: (map at: key) put: key].	^map! !

!CypressAbstractPackageWriter class methodsFor: 'accessing' stamp: ''!
specials	^specials ifNil: [specials := self initializeSpecials]! !

!CypressAbstractPackageWriter methodsFor: 'private' stamp: ''!
determinePackageDirectory	^self fileUtils ensureDirectoryExists: (self fileUtils				directoryFromPath: self packageStructure name				relativeTo: self repository directoryPath)! !

!CypressAbstractPackageWriter methodsFor: 'private' stamp: ''!
directoryForDirectoryNamed: directoryNameOrPath	^directoryNameOrPath = '.'		ifTrue: [self fileUtils ensureDirectoryExists: self packageDirectory]		ifFalse: [self subPackageFileDirectoryFor: directoryNameOrPath]! !

!CypressAbstractPackageWriter methodsFor: 'private' stamp: ''!
fileNameForSelector: selector	^selector last = $:		ifTrue: [selector copyReplacing: $: with: $.]		ifFalse: 			[(selector first isLetter or: [selector first = $_])				ifTrue: [selector]				ifFalse: 					[| specials |					specials := self class specials.					String streamContents: 							[:output |							output nextPut: $^.							selector do: [:each | output nextPutAll: (specials at: each)]								separatedBy: [output nextPut: $.]]]]! !

!CypressAbstractPackageWriter methodsFor: 'writing' stamp: ''!
removeOldReplacingWithNew	self fileUtils deleteAll: self packageDirectory.	self writePropertiesFile.	self writePackageStructure! !

!CypressAbstractPackageWriter methodsFor: 'private' stamp: ''!
subPackageFileDirectoryFor: directoryNameOrPath	| dir |	dir := self fileUtils directoryFromPath: directoryNameOrPath				relativeTo: self packageDirectory.	self fileUtils ensureDirectoryExists: dir.	^dir! !

!CypressAbstractPackageWriter methodsFor: 'writing' stamp: ''!
writeClassComment: classStructure on: fileStream	fileStream nextPutAll: classStructure comment withUnixLineEndings! !

!CypressAbstractPackageWriter methodsFor: 'writing' stamp: ''!
writeClassStructure: classStructure to: classPath	self		writeInDirectoryName: classPath			fileName: 'README'			extension: '.md'			visit: [:fileStream | self writeClassComment: classStructure on: fileStream];		writeInDirectoryName: classPath			fileName: 'properties'			extension: self propertiesFileNameExtension			visit: [:fileStream | classStructure  properties writeCypressJsonOn: fileStream]! !

!CypressAbstractPackageWriter methodsFor: 'writing' stamp: ''!
writeExtensionClassStructure: classStructure to: classPath	self		writeInDirectoryName: classPath		fileName: 'properties'		extension: self propertiesFileNameExtension		visit: 			[:fileStream |			(Dictionary with: 'name' -> classStructure className)				writeCypressJsonOn: fileStream]! !

!CypressAbstractPackageWriter methodsFor: 'private' stamp: ''!
writeInDirectoryName: directoryNameOrPath fileName: fileName extension: ext visit: visitBlock	| directory |	directory := self directoryForDirectoryNamed: directoryNameOrPath.	self fileUtils		writeStreamFor: fileName , ext		in: directory		do: [:fileStream | visitBlock value: fileStream]! !

!CypressAbstractPackageWriter methodsFor: 'writing' stamp: ''!
writeMethodStructure: methodStructure onStream: fileStream	self subclassResponsibility: #writeMethodStructure:onStream:! !

!CypressAbstractPackageWriter methodsFor: 'writing' stamp: ''!
writeMethodStructure: methodStructure to: methodPath	| filename |	filename := self fileNameForSelector: methodStructure selector.	self		writeInDirectoryName: methodPath		fileName: filename		extension: '.st'		visit: [:fileStream | self writeMethodStructure: methodStructure onStream: fileStream]! !

!CypressAbstractPackageWriter methodsFor: 'writing' stamp: ''!
writePackageStructure	self		writePackageStructureClasses: self packageStructure classes			isClassExtension: false;		writePackageStructureClasses: self packageStructure extensions			isClassExtension: true! !

!CypressAbstractPackageWriter methodsFor: 'writing' stamp: ''!
writePackageStructure: aPackageStructure	self		packageStructure: aPackageStructure;		packageDirectory: (self determinePackageDirectory);		removeOldReplacingWithNew! !

!CypressAbstractPackageWriter methodsFor: 'writing' stamp: ''!
writePackageStructureClasses: classStructures isClassExtension: isClassExtension	| classDirExtension |	classDirExtension := isClassExtension				ifTrue: ['.extension']				ifFalse: ['.class'].	classStructures do: 			[:classStructure |			| classPath instanceMethodPath classMethodPath |			classPath := classStructure className , classDirExtension						, self fileUtils pathNameDelimiter asString.			isClassExtension				ifTrue: [self writeExtensionClassStructure: classStructure to: classPath]				ifFalse: [self writeClassStructure: classStructure to: classPath].			instanceMethodPath := classPath , 'instance' , self fileUtils pathNameDelimiter asString.			classStructure instanceMethods				do: [:methodStructure | self writeMethodStructure: methodStructure to: instanceMethodPath].			classMethodPath := classPath , 'class' , self fileUtils pathNameDelimiter asString.			classStructure classMethods				do: [:methodStructure | self writeMethodStructure: methodStructure to: classMethodPath]]! !

!CypressAbstractPackageWriter methodsFor: 'writing' stamp: ''!
writePropertiesFile	self		writeInDirectoryName: '.'		fileName: 'properties'		extension: self propertiesFileNameExtension		visit: [:fileStream | Dictionary new writeCypressJsonOn: fileStream]! !

CypressAbstractPackageWriter subclass: #CypressFileTreeFormatPackageWriter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-GemStoneFileServer'!

!CypressFileTreeFormatPackageWriter methodsFor: 'writing' stamp: ''!
writeMethodStructure: methodStructure onStream: fileStream	fileStream		nextPutAll: methodStructure category; lf;		nextPutAll: methodStructure source withUnixLineEndings! !

CypressAbstractPackageWriter subclass: #CypressPackageWriter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-GemStoneFileServer'!

!CypressPackageWriter methodsFor: 'accessing - private' stamp: ''!
methodNoticeLine	^self packageStructure properties		at: 'copyrightLine'		ifAbsent: [self repository copyrightProperty]! !

!CypressPackageWriter methodsFor: 'writing' stamp: ''!
writeMethodStructure: methodStructure onStream: fileStream	fileStream		nextPutAll: '"'; lf;		nextPutAll: 'notice: ', self methodNoticeLine; lf;		nextPutAll: 'category: ', methodStructure category; lf;		nextPutAll: '"'; lf;		nextPutAll: methodStructure source withUnixLineEndings! !

CypressAbstractPackageWriter subclass: #CypressStrictFileTreeFormatDoNothingPackageWriter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-GemStoneFileServer'!

!CypressStrictFileTreeFormatDoNothingPackageWriter methodsFor: 'writing' stamp: ''!
removeOldReplacingWithNew	"Change nothing, since a Cypress writer has insufficient information	 for preserving the FileTree details. Strictly read-only."! !

!CypressStrictFileTreeFormatDoNothingPackageWriter methodsFor: 'writing' stamp: ''!
writePropertiesFile	"Change nothing, since a Cypress writer has insufficient information	 for preserving the FileTree details. Strictly read-only."! !

Object subclass: #CypressFileUtilities
	instanceVariableNames: ''
	classVariableNames: 'Current'
	poolDictionaries: ''
	category: 'Cypress-GemStoneFileServer'!

!CypressFileUtilities class methodsFor: 'accessing' stamp: ''!
current	^Current! !

!CypressFileUtilities class methodsFor: 'utilities' stamp: ''!
deleteAll: aDirectory	self subclassResponsibility: #deleteAll:! !

!CypressFileUtilities class methodsFor: 'utilities' stamp: ''!
directoryEntriesFrom: aDirectory	self subclassResponsibility: #directoryEntriesFrom:! !

!CypressFileUtilities class methodsFor: 'utilities' stamp: ''!
directoryExists: aDirectory	self subclassResponsibility: #directoryExists:! !

!CypressFileUtilities class methodsFor: 'unknown' stamp: ''!
directoryFileNamesAndContents: aDirectory	self subclassResponsibility: #directoryFileNamesAndContents:! !

!CypressFileUtilities class methodsFor: 'utilities' stamp: ''!
directoryFromPath: directoryPath relativeTo: aDirectory	self subclassResponsibility: #directoryFromPath:relativeTo:! !

!CypressFileUtilities class methodsFor: 'utilities' stamp: ''!
ensureDirectoryExists: aDirectory	self subclassResponsibility: #ensureDirectoryExists:! !

!CypressFileUtilities class methodsFor: 'initializating' stamp: ''!
install	Current := self! !

!CypressFileUtilities class methodsFor: 'utilities' stamp: ''!
localNameFrom: aDirectory	self subclassResponsibility: #localNameFrom:! !

!CypressFileUtilities class methodsFor: 'utilities' stamp: ''!
pathNameDelimiter	self subclassResponsibility: #pathNameDelimiter! !

!CypressFileUtilities class methodsFor: 'utilities' stamp: ''!
readStreamFor: filePath do: aOneArgBlock	self subclassResponsibility: #readStreamFor:do:! !

!CypressFileUtilities class methodsFor: 'utilities' stamp: ''!
readStreamFor: filePath in: aDirectory do: aOneArgBlock	self subclassResponsibility: #readStreamFor:in:do:! !

!CypressFileUtilities class methodsFor: 'utilities' stamp: ''!
workingDirectory	self subclassResponsibility: #workingDirectory! !

!CypressFileUtilities class methodsFor: 'utilities' stamp: ''!
writeStreamFor: filePath in: aDirectory do: aOneArgBlock	self subclassResponsibility: #writeStreamFor:in:do:! !

CypressFileUtilities subclass: #CypressGemStoneDirectoryUtilities
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-GemStoneFileServer'!

!CypressGemStoneDirectoryUtilities class methodsFor: 'utilities' stamp: ''!
deleteAll: aDirectory	"Delete all the files and directories under the named directory.	 Ensure we don't try to recursively delete . or .."	| filename isFile |	(GsFile contentsAndTypesOfDirectory: aDirectory onClient: false)		doWithIndex: 			[:each :index |			index odd				ifTrue: [filename := each]				ifFalse: 					[isFile := each.					isFile						ifTrue: [GsFile removeServerFile: filename]						ifFalse: 							[(#('/..' '/.' '\..' '\.')								anySatisfy: [:special | filename endsWith: special])									ifFalse: 										[self deleteAll: filename.										GsFile removeServerDirectory: filename]]]]! !

!CypressGemStoneDirectoryUtilities class methodsFor: 'utilities' stamp: ''!
directoryEntriesFrom: aDirectory	"Answer fully qualified paths to the contents of aDirectory."	^(GsFile contentsOfDirectory: aDirectory onClient: false) ifNil: [#()]! !

!CypressGemStoneDirectoryUtilities class methodsFor: 'utilities' stamp: ''!
directoryExists: aDirectory	^GsFile existsOnServer: aDirectory! !

!CypressGemStoneDirectoryUtilities class methodsFor: 'unknown' stamp: ''!
directoryFileNamesAndContents: aDirectory	"Walk the directory tree starting at aDirectory and	 answer a map of the names of the files in the tree to	 their contents (which work best when text)."	| map |	map := Dictionary new.	self directoryFileNamesAndContents: aDirectory into: map.	^map.! !

!CypressGemStoneDirectoryUtilities class methodsFor: 'unknown' stamp: ''!
directoryFileNamesAndContents: aDirectory into: aDictionary	"Walk the directory tree starting at aDirectory and	 answer a map of the names of the files in the tree to	 their contents (which work best when text)."	| filename isFile |	(GsFile contentsAndTypesOfDirectory: aDirectory onClient: false)		doWithIndex: 			[:each :index |			index odd				ifTrue: [filename := each]				ifFalse: 					[isFile := each.					isFile						ifTrue: 							[| file |							file := GsFile openReadOnServer: filename.							file isNil								ifFalse: 									[aDictionary at: filename put: file contents.									file close]]						ifFalse: 							[(#('/..' '/.' '\..' '\.')								anySatisfy: [:special | filename endsWith: special])									ifFalse: [self directoryFileNamesAndContents: filename into: aDictionary]]]]! !

!CypressGemStoneDirectoryUtilities class methodsFor: 'utilities' stamp: ''!
directoryFromPath: directoryPath relativeTo: aDirectory	^((aDirectory endsWith: self pathNameDelimiter) or: [directoryPath beginsWith: self pathNameDelimiter])		ifTrue: [aDirectory, directoryPath]		ifFalse: [aDirectory, self pathNameDelimiter, directoryPath]! !

!CypressGemStoneDirectoryUtilities class methodsFor: 'utilities' stamp: ''!
ensureDirectoryExists: aDirectory	| lastSeparator |	(GsFile existsOnServer: aDirectory) ifTrue: [^aDirectory].	(GsFile createServerDirectory: aDirectory) ifNotNil: [^aDirectory].	lastSeparator := aDirectory findLastSubString: self pathNameDelimiter startingAt: aDirectory size.	lastSeparator <= 1 ifTrue: [self error: 'Cannot create directory'].	self ensureDirectoryExists: (aDirectory copyFrom: 1 to: lastSeparator - 1).	self ensureDirectoryExists: aDirectory.! !

!CypressGemStoneDirectoryUtilities class methodsFor: 'initializating' stamp: ''!
initialize	"self initialize"	self install! !

!CypressGemStoneDirectoryUtilities class methodsFor: 'utilities' stamp: ''!
localNameFrom: aDirectory	| endOfPath |	endOfPath := aDirectory findLastSubString: self pathNameDelimiter startingAt: aDirectory size.	^aDirectory copyFrom: endOfPath + 1 to: aDirectory size! !

!CypressGemStoneDirectoryUtilities class methodsFor: 'utilities' stamp: ''!
pathNameDelimiter	^'/'! !

!CypressGemStoneDirectoryUtilities class methodsFor: 'utilities' stamp: ''!
readStreamFor: filePath do: aOneArgBlock	| file stream |	GsFile serverErrorString.	file := GsFile openReadOnServer: filePath.	GsFile serverErrorString ifNotNil: [:errorMessage | self error: errorMessage].	[stream := ReadStream on: (String withAll: file contents asByteArray decodeFromUTF8).	aOneArgBlock value: stream] ensure: [file close]! !

!CypressGemStoneDirectoryUtilities class methodsFor: 'utilities' stamp: ''!
readStreamFor: filePath in: aDirectory do: aOneArgBlock	self		readStreamFor: (self directoryFromPath: filePath relativeTo: aDirectory)		do: aOneArgBlock! !

!CypressGemStoneDirectoryUtilities class methodsFor: 'utilities' stamp: ''!
workingDirectory	^System gemEnvironmentVariable: 'PWD'! !

!CypressGemStoneDirectoryUtilities class methodsFor: 'utilities' stamp: ''!
writeStreamFor: filePath in: aDirectory do: aOneArgBlock	| file stream |	GsFile serverErrorString.	file := GsFile openWriteOnServer: (self directoryFromPath: filePath relativeTo: aDirectory).	GsFile serverErrorString ifNotNil: [:errorMessage | self error: errorMessage].	stream := WriteStream on: String new.	[aOneArgBlock value: stream] ensure: [file nextPutAll: stream contents encodeAsUTF8; close]! !
CypressGemStoneDirectoryUtilities initialize!
