"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressAbstractPackageReader',
	#superclass : 'CypressAbstractPackageFiler',
	#category : 'Cypress-GemStoneFileServer'
}

{ #category : 'parsing' }
CypressAbstractPackageReader >> _parseMethod: source category: cat using: aSymbolList environmentId: anEnvironmentId [
	"Compiles the method into disposable dictionaries, if possible.
	 Attempts auto-recompile for undefinedSymbols.
	 Returns the compiled method or signals a CompileError."

	| undefinedSymbolList undefinedSymbols |
	undefinedSymbols := SymbolDictionary new name: #UndefinedSymbols.
	undefinedSymbolList := SymbolList with: undefinedSymbols.
	^
	[UndefinedObject
		compileMethod: source
		dictionaries: aSymbolList
		category: cat
		intoMethodDict: GsMethodDictionary new
		intoCategories: GsMethodDictionary new
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
								
								[^UndefinedObject
									compileMethod: source
									dictionaries: aSymbolList , undefinedSymbolList
									category: cat
									intoMethodDict: GsMethodDictionary new
									intoCategories: GsMethodDictionary new
									environmentId: anEnvironmentId]
										onException: CompileError
										do: [:exb | undefSymbol := false]].
						undefSymbol ifFalse: [ex outer]]
					with: [:ex | ex resume])
]

{ #category : 'private' }
CypressAbstractPackageReader >> classStructureFrom: classPropertiesDict [

	^(CypressClassStructure new)
		packageStructure: self packageStructure;
		isClassExtension: true;
		properties: classPropertiesDict;
		yourself

]

{ #category : 'private' }
CypressAbstractPackageReader >> classStructureFrom: classPropertiesDict comment: classComment [

	^(self classStructureFrom: classPropertiesDict)
		isClassExtension: false;
		comment: classComment;
		yourself

]

{ #category : 'reading' }
CypressAbstractPackageReader >> isPropertiesFileDirectoryEntry: entry [

	^self propertiesFileNameExtensions
		anySatisfy: [:each | self string: entry endsWith: '/properties' , each]

]

{ #category : 'accessing' }
CypressAbstractPackageReader >> packageExtension [

	^self packageStructure
		packageExtensionOr: [self repository packageExtension]

]

{ #category : 'parsing' }
CypressAbstractPackageReader >> parseSelectorFrom: methodString [

	| meth |
	^
	[meth := self
				_parseMethod: methodString
				category: #xyzzy
				using: GsSession currentSession symbolList
				environmentId: 0.
	meth class ~~ GsNMethod
		ifTrue: 
			["if error slot is nil, then the method wasn't compiled because of errors"
			(meth at: 2) == nil ifFalse: [^nil].
			meth := meth at: 1].
	meth selector asString]
			on: CompileError
			do: [:ex | ex return: '_____could_not_parse_selector_from_method_source_____']

]

{ #category : 'reading' }
CypressAbstractPackageReader >> readClassCommentFromDirectoryEntries: entries [

	self fileUtils readStreamFor: (entries
				detect: [:entry | self string: entry endsWith: '/README.md']
				ifNone: [^''])
		do: [:fileStream | ^fileStream contents]

]

{ #category : 'reading' }
CypressAbstractPackageReader >> readClassPropertiesFromDirectoryEntries: entries [

	self fileUtils readStreamFor: (entries
				detect: [:entry | self isPropertiesFileDirectoryEntry: entry]
				ifNone: [^Dictionary new])
		do: [:fileStream | ^CypressJsonParser parseStream: fileStream]

]

{ #category : 'reading' }
CypressAbstractPackageReader >> readClassStructureFromEntry: classEntry [

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

]

{ #category : 'reading' }
CypressAbstractPackageReader >> readCypressFormatMethodStructureFrom: fileStream intoClassStructure: classStructure meta: isMeta methods: methods [

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
	selector := self parseSelectorFrom: source.
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

]

{ #category : 'reading' }
CypressAbstractPackageReader >> readExtensionClassStructureFromEntry: classEntry [

	| classPropertiesDict entries classStructure |
	entries := self fileUtils directoryEntriesFrom: classEntry.
	classPropertiesDict := self
				readClassPropertiesFromDirectoryEntries: entries.
	classStructure := self classStructureFrom: classPropertiesDict.
	self readMethodStructureFor: classStructure in: entries.
	^classStructure

]

{ #category : 'reading' }
CypressAbstractPackageReader >> readFileTreeFormatMethodStructureFrom: fileStream intoClassStructure: classStructure meta: isMeta methods: methods [

	| category source selector |
	category := fileStream nextLine trimSeparators.
	source := fileStream upToEnd.
	selector := self parseSelectorFrom: source.
	selector = '_____could_not_parse_selector_from_method_source_____'
		ifTrue: [ 
			self
				error:
					'compile error while attempting to extract selector from method: '
						, source printString ].
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
]

{ #category : 'reading' }
CypressAbstractPackageReader >> readMethodStructureFor: classStructure in: entries [

	entries do: 
			[:entry |
			| methods isMeta |
			methods := (isMeta := self string: entry endsWith: '/class')
						ifTrue: [classStructure classMethods]
						ifFalse: [classStructure instanceMethods].
			((self string: entry endsWith: '/instance')
				or: [self string: entry endsWith: '/class'])
					ifTrue: 
						[((self fileUtils directoryEntriesFrom: entry)
							select: [:each | self string: each endsWith: '.st']) do: 
									[:methodEntry |
									self fileUtils readStreamFor: methodEntry
										do: 
											[:fileStream |
											self
												readMethodStructureFrom: fileStream
												intoClassStructure: classStructure
												meta: isMeta
												methods: methods]]]]

]

{ #category : 'reading' }
CypressAbstractPackageReader >> readMethodStructureFrom: fileStream intoClassStructure: classStructure meta: isMeta methods: methods [

	self subclassResponsibility: #readMethodStructureFrom:intoClassStructure:meta:methods:

]

{ #category : 'reading' }
CypressAbstractPackageReader >> readPackageStructure [

	(self fileUtils directoryEntriesFrom: self packageDirectory) do: 
			[:entry |
			(self isPropertiesFileDirectoryEntry: entry)
				ifTrue: [self packageStructure properties: (self readPropertiesFile: entry)].
			(self string: entry endsWith: '.class')
				ifTrue: 
					[self packageStructure classes
						add: (self readClassStructureFromEntry: entry)].
			(self string: entry endsWith: '.extension')
				ifTrue: 
					[self packageStructure extensions
						add: (self readExtensionClassStructureFromEntry: entry)]]

]

{ #category : 'reading' }
CypressAbstractPackageReader >> readPackageStructureForPackageNamed: packageName [

	| structureName |
	structureName := packageName , self repository packageExtension.
	self
		packageStructure: (CypressPackageStructure named: structureName);
		packageDirectory: (self fileUtils directoryFromPath: structureName
					relativeTo: self repository directoryPath);
		readPackageStructure

]

{ #category : 'reading' }
CypressAbstractPackageReader >> readPropertiesFile: entry [

	self fileUtils
		readStreamFor: entry
		do: [:fileStream | ^CypressJsonParser parseStream: fileStream]

]
