"
This class is private to Cypress.
A GsPlatform is the gateway object for requesting platform-specific actions.
"
Class {
	#name : 'RwGsPlatform',
	#superclass : 'RwPlatform',
	#instVars : [
		'alternateImageClass',
		'enableInstanceMigration',
		'instanceMigrator'
	],
	#category : 'Rowan-GemStone-Core'
}

{ #category : 'private' }
RwGsPlatform >> _alternateImageClass [

	^ alternateImageClass
]

{ #category : 'private' }
RwGsPlatform >> _alternateImageClass: anImageClass [

	alternateImageClass := anImageClass
]

{ #category : 'private' }
RwGsPlatform >> _parseMethod: source category: cat using: aSymbolList environmentId: anEnvironmentId [
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
								
								[^UndefinedObject
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

{ #category : 'queries' }
RwGsPlatform >> basePlatformAttribute [
	"Answer the generic configuration attribute for the platform"


	^ 'gemstone'

]

{ #category : 'defaults' }
RwGsPlatform >> defaultConfiguration [

	^ RwConfiguration new
		packageInfoSource: #'SymbolDictionary';
		yourself
]

{ #category : 'queries' }
RwGsPlatform >> fileUtilities [
  "Answer the platform-specific object for accessing files and directories"

  ^ RwGsFileUtilities
]

{ #category : 'queries' }
RwGsPlatform >> globalNamed: aString [

	"Answer a global object with the given name.  If no object with the given name is found, returns nil."

	^ Rowan image objectNamed: aString
]

{ #category : 'queries' }
RwGsPlatform >> image [

	"Answer the platform-specific object for querying the local image"

	^ alternateImageClass ifNil: [ RwGsImage ]
]

{ #category : 'initialization' }
RwGsPlatform >> initialize [

]

{ #category : 'queries' }
RwGsPlatform >> instanceMigrator [

	^ instanceMigrator
		ifNil: [ instanceMigrator := RwGsInstanceMigrator noMigration ]
]

{ #category : 'queries' }
RwGsPlatform >> instanceMigrator: anInstanceMigrator [

	instanceMigrator := anInstanceMigrator
]

{ #category : 'parsing' }
RwGsPlatform >> parseSelectorFrom: methodString [

	| meth |
	^ [ 
	meth := self
		_parseMethod: methodString
		category: #'xyzzy'
		using: self image symbolList
		environmentId: 0.
	meth class ~~ GsNMethod
		ifTrue: [ 
			"if error slot is nil, then the method wasn't compiled because of errors"
			(meth at: 2) == nil
				ifFalse: [ ^ nil ].
			meth := meth at: 1 ].
	meth selector asString ]
		on: CompileError
		do: [ :ex | ex return: '_____could_not_parse_selector_from_method_source_____' ]
]

{ #category : 'queries' }
RwGsPlatform >> platformConfigurationAttributes [
	"Answer the configuration attributes for the current platform."

	^ super platformConfigurationAttributes, {self basePlatformAttribute. (System stoneVersionReport at: 'gsVersion') asRwGemStoneVersionNumber}

]
