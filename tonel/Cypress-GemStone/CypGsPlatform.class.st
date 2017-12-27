"
This class is private to Cypress.
A GsPlatform is the gateway object for requesting platform-specific actions.
"
Class {
	#name : 'CypGsPlatform',
	#superclass : 'CypPlatform',
	#category : 'Cypress-GemStone'
}

{ #category : 'private' }
CypGsPlatform >> _parseMethod: source category: cat using: aSymbolList environmentId: anEnvironmentId [
	"Compiles the method into disposable dictionaries, if possible.
	 Attempts auto-recompile for undefinedSymbols.
	 Returns the compiled method or signals a CompileError."

	| undefinedSymbolList undefinedSymbols |
	undefinedSymbols := SymbolDictionary new name: #UndefinedSymbols.
	undefinedSymbolList := SymbolList with: undefinedSymbols.
	^
	[self class
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
								
								[^self class
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
]

{ #category : 'defaults' }
CypGsPlatform >> defaultConfiguration [

	^(CypConfiguration new)
		packageInfoSource: #Category;
		yourself
]

{ #category : 'queries' }
CypGsPlatform >> fileUtilities [
	"Answer the platform-specific object for accessing files and directories"

	^ CypGsFileUtilities
]

{ #category : 'queries' }
CypGsPlatform >> image [
	"Answer the platform-specific object for querying the local image"

	^CypGsImage
]

{ #category : 'initialization' }
CypGsPlatform >> initialize [

	CypGsLoadedPackage initialize.
	CypGsLoadedClass initialize.
	CypGsLoadedMethod initialize
]

{ #category : 'queries' }
CypGsPlatform >> loadedPackageClass [
	"Answer the platform-specific class for LoadedPackage"

	^CypGsLoadedPackage
]

{ #category : 'queries' }
CypGsPlatform >> loadedPackageOfClass: aClass [
	"If the given class is in a package, answer that LoadedPackage. If not, answer nil."

	^self loadedPackageClass loadedPackageOfClass: aClass
]

{ #category : 'parsing' }
CypGsPlatform >> parseSelectorFrom: methodString [

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

{ #category : 'actions' }
CypGsPlatform >> updateLoadedPackagesFrom: source [
	"For GemStone, valid sources are #Category and #SymbolDictionary"

	source == #Category
		ifTrue: [CypGsLoadedPackage updateAllFromCategories]
		ifFalse: 
			[source == #SymbolDictionary
				ifTrue: [CypGsLoadedPackage updateAllFromSymbolDictionaries]
				ifFalse: [self error: 'Invalid packageInfoSource for GemStone.']]
]
