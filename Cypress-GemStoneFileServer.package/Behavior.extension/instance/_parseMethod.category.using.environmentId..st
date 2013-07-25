*Cypress-GemStoneFileServer
_parseMethod: source category: cat using: aSymbolList environmentId: anEnvironmentId
	"Attempts auto-recompile for undefinedSymbols.
	 Returns the compiled method or signals a CompileError"

	| undefinedSymbolList undefinedSymbols |
	undefinedSymbols := SymbolDictionary new name: #UndefinedSymbols.
	undefinedSymbolList := SymbolList with: undefinedSymbols.
	^
	[self
		compileMethod: source
		dictionaries: aSymbolList
		category: cat
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
									environmentId: anEnvironmentId]
										onException: CompileError
										do: [:exb | undefSymbol := false]].
						undefSymbol ifFalse: [ex outer]]
					with: [:ex | ex resume])