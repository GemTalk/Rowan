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
RwGsPlatform >> _globalPreferenceDict [

	^ (self class _userPlatformDictionaryForUser: 'SystemUser') at: #RwGlobalPlatform_Preferences ifAbsentPut: [ Dictionary new ]
]

{ #category : 'private' }
RwGsPlatform >> _parseMethod: source category: cat using: aSymbolList environmentId: anEnvironmentId [
	"Compiles the method into disposable dictionaries, if possible.
	 Attempts auto-recompile for undefinedSymbols.
	 Returns the compiled method or signals a CompileError.
   Only used to parse a method to determine the selector.  "

	| undefinedSymbolList undefinedSymbols |
	undefinedSymbols := SymbolDictionary new name: #'UndefinedSymbols'.
	undefinedSymbolList := SymbolList with: undefinedSymbols.
	^ [ 
	UndefinedObject
		compileMethod: source
		dictionaries: aSymbolList
		category: cat
		intoMethodDict: GsMethodDictionary new
		intoCategories: GsMethodDictionary new
		environmentId: anEnvironmentId ]
		onSynchronous: (Array with: CompileError with: CompileWarning)
		do:
			(Array
				with: [ :ex | 
					| undefSymbol symbols |
					undefSymbol := true.
					symbols := Array new.
					ex errorDetails
						do: [ :errArray | 
							(errArray atOrNil: 1) == 1031
								ifTrue: [ symbols add: (errArray atOrNil: 5) asSymbol ]
								ifFalse: [ undefSymbol := false ] ].
					undefSymbol
						ifTrue: [ 
							"attempt auto-define of undefined symbols"
							symbols do: [ :sym | undefinedSymbols at: sym put: nil ].

							[ 
							^ UndefinedObject
								compileMethod: source
								dictionaries: aSymbolList , undefinedSymbolList
								category: cat
								intoMethodDict: GsMethodDictionary new
								intoCategories: GsMethodDictionary new
								environmentId: anEnvironmentId ]
								onSynchronous: (Array with: CompileError with: CompileWarning)
								do:
									(Array with: [ :exb | undefSymbol := false ] with: [ :exc | exc resume ]) ].
					undefSymbol
						ifFalse: [ ex outer ] ]
				with: [ :ex | ex resume ])
]

{ #category : 'private' }
RwGsPlatform >> _sessionPreferenceDict [

	^ SessionTemps current at: #RwSessionPlatform_Preferences ifAbsentPut: [ Dictionary new ]
]

{ #category : 'private' }
RwGsPlatform >> _userPreferenceDict [

	^ self class _userPlatformDictionary at: #RwUserPlatform_Preferences ifAbsentPut: [ Dictionary new ]
]

{ #category : 'automatic class initialization' }
RwGsPlatform >> automaticClassInitializationBlackList_session [

	"Answer session list of project names for which automatic class initialiation should be disabled."

	| preferenceSymbol |
	preferenceSymbol := self _automaticClassInitializationBlackList_symbol.
	^ self 
		sessionPreferenceFor: preferenceSymbol 
		ifAbsent: [
			| list |
			list := OrderedCollection new.
			self setSessionPreferenceFor: preferenceSymbol to: list.
			list ]
]

{ #category : 'automatic class initialization' }
RwGsPlatform >> automaticClassInitializationBlackList_user [

	"Answer session list of project names for which automatic class initialiation should be disabled."

	| preferenceSymbol |
	preferenceSymbol := self _automaticClassInitializationBlackList_symbol.
	^ self 
		userPreferenceFor: preferenceSymbol 
		ifAbsent: [
			| list |
			list := OrderedCollection new.
			self setUserPreferenceFor: preferenceSymbol to: list.
			list ]
]

{ #category : 'queries' }
RwGsPlatform >> basePlatformConditionalAttribute [
	"Answer the generic conditional attribute for the platform"

	"Examples include: 'gemstone', 'pharo', 'squeak', 'vast' 
		(see RwSpecification class >> _supportedPlatformNames)"

	^ 'gemstone'
]

{ #category : 'preferences' }
RwGsPlatform >> clearAllPreferencesFor: preferenceSymbol [


	self 
		clearSessionPreferenceFor: preferenceSymbol;
		clearUserPreferenceFor: preferenceSymbol;
		clearGlobalPreferenceFor: preferenceSymbol;
		yourself
]

{ #category : 'automatic class initialization' }
RwGsPlatform >> clearAutomaticClassInitializationBlackList_global [

	"Answer global list of project names for which automatic class initialiation should be disabled."

	| preferenceSymbol |
	preferenceSymbol := self _automaticClassInitializationBlackList_symbol.
	self clearGlobalPreferenceFor: preferenceSymbol
]

{ #category : 'automatic class initialization' }
RwGsPlatform >> clearAutomaticClassInitializationBlackList_session [

	"Answer session list of project names for which automatic class initialiation should be disabled."

	| preferenceSymbol |
	preferenceSymbol := self _automaticClassInitializationBlackList_symbol.
	self clearSessionPreferenceFor: preferenceSymbol
]

{ #category : 'automatic class initialization' }
RwGsPlatform >> clearAutomaticClassInitializationBlackList_user [

	"Answer session list of project names for which automatic class initialiation should be disabled."

	| preferenceSymbol |
	preferenceSymbol := self _automaticClassInitializationBlackList_symbol.
	self clearUserPreferenceFor: preferenceSymbol
]

{ #category : 'preferences' }
RwGsPlatform >> clearDefaultPreferenceFor: preferenceSymbol [
	"global preferences implements default preference"

	self clearGlobalPreferenceFor: preferenceSymbol
]

{ #category : 'preferences - gemstone' }
RwGsPlatform >> clearGlobalPreferenceFor: preferenceSymbol [

	self _globalPreferenceDict removeKey: preferenceSymbol ifAbsent: []
]

{ #category : 'preferences' }
RwGsPlatform >> clearPreferenceFor: preferenceSymbol [
	"clear sessoin and userPreferences - preserve non-gemstone semantics"

	self 
		clearSessionPreferenceFor: preferenceSymbol;
		clearUserPreferenceFor: preferenceSymbol;
		yourself
]

{ #category : 'preferences - gemstone' }
RwGsPlatform >> clearSessionPreferenceFor: preferenceSymbol [

	self _sessionPreferenceDict removeKey: preferenceSymbol ifAbsent: []
]

{ #category : 'preferences - gemstone' }
RwGsPlatform >> clearUserPreferenceFor: preferenceSymbol [

	self _userPreferenceDict removeKey: preferenceSymbol ifAbsent: []
]

{ #category : 'defaults' }
RwGsPlatform >> defaultConfiguration [

	^ RwConfiguration new
		packageInfoSource: #'SymbolDictionary';
		yourself
]

{ #category : 'preferences' }
RwGsPlatform >> defaultPreferenceFor: preferenceSymbol ifAbsent: aBlock [

	"global preferences implements default preference"

	^self globalPreferenceFor: preferenceSymbol ifAbsent: aBlock
]

{ #category : 'queries' }
RwGsPlatform >> globalNamed: aString [

	"Answer a global object with the given name.  If no object with the given name is found, returns nil."

	^ Rowan image objectNamed: aString
]

{ #category : 'preferences - gemstone' }
RwGsPlatform >> globalPreferenceFor: preferenceSymbol [

	^ self globalPreferenceFor: preferenceSymbol ifAbsent: [ self error: 'Preference for ', preferenceSymbol printString, ' not found' ].
]

{ #category : 'preferences - gemstone' }
RwGsPlatform >> globalPreferenceFor: preferenceSymbol ifAbsent: aBlock [

	^ self _globalPreferenceDict 
		at: preferenceSymbol 
		ifAbsent: aBlock
]

{ #category : 'queries' }
RwGsPlatform >> image [

	"Answer the platform-specific object for querying the local image"

	^ alternateImageClass ifNil: [ RwGsImage ]
]

{ #category : 'initialization' }
RwGsPlatform >> initialize [

	self automaticClassInitializationBlackList
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

{ #category : 'queries' }
RwGsPlatform >> orderedDictionaryClass [
  "Answer the platform-specific OrderedDictionary-compatible class"

  ^ GsTonelOrderedDictionary
]

{ #category : 'parsing' }
RwGsPlatform >> parseSelectorFrom: methodString [
	^ self parseSelectorFrom: methodString passCompileError: false
]

{ #category : 'parsing' }
RwGsPlatform >> parseSelectorFrom: methodString passCompileError: passCompileError [
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
		do: [ :ex | 
			passCompileError
				ifTrue: [ ex pass ]
				ifFalse: [ ex return: '_____could_not_parse_selector_from_method_source_____' ] ]
]

{ #category : 'queries' }
RwGsPlatform >> platformConditionalAttributes [
	"Answer the conditional attributes for the current platform."

	^ super platformConditionalAttributes
		,
			{(self basePlatformConditionalAttribute).
			'gemstone-kernel'.
			((System gemVersionReport at: 'gsVersion') asRwGemStoneVersionNumber)}
]

{ #category : 'preferences' }
RwGsPlatform >> preferenceFor: preferenceSymbol ifAbsent: aBlock [

	^ self _sessionPreferenceDict 
		at: preferenceSymbol 
		ifAbsent: [ 
			self 
				_userPreferenceDict at: preferenceSymbol 
				ifAbsent: [
					self _globalPreferenceDict 
						at: preferenceSymbol
						ifAbsent:aBlock ] ]
]

{ #category : 'preferences - gemstone' }
RwGsPlatform >> sessionPreferenceFor: preferenceSymbol [

	^ self sessionPreferenceFor: preferenceSymbol ifAbsent: [ self error: 'Preference for ', preferenceSymbol printString, ' not found' ].
]

{ #category : 'preferences - gemstone' }
RwGsPlatform >> sessionPreferenceFor: preferenceSymbol ifAbsent: aBlock [

	^ self _sessionPreferenceDict 
		at: preferenceSymbol 
		ifAbsent: aBlock
]

{ #category : 'preferences' }
RwGsPlatform >> setDefaultPreferenceFor: preferenceSymbol to: anObject [

	"global preferences implements default preference"

	^self setGlobalPreferenceFor: preferenceSymbol to: anObject
]

{ #category : 'preferences - gemstone' }
RwGsPlatform >> setGlobalPreferenceFor: preferenceSymbol to: anObject [

	self _globalPreferenceDict at: preferenceSymbol put: anObject
]

{ #category : 'preferences' }
RwGsPlatform >> setPreferenceFor: preferenceSymbol to: anObject [

	"clear session; set userPreferences - preserve non-gemstone semantics"

	self clearSessionPreferenceFor: preferenceSymbol.
	^self setUserPreferenceFor: preferenceSymbol to: anObject
]

{ #category : 'preferences - gemstone' }
RwGsPlatform >> setSessionPreferenceFor: preferenceSymbol to: anObject [

	self _sessionPreferenceDict at: preferenceSymbol put: anObject
]

{ #category : 'preferences - gemstone' }
RwGsPlatform >> setUserPreferenceFor: preferenceSymbol to: anObject [

	self _userPreferenceDict at: preferenceSymbol put: anObject
]

{ #category : 'preferences - gemstone' }
RwGsPlatform >> userPreferenceFor: preferenceSymbol [

	^ self userPreferenceFor: preferenceSymbol ifAbsent: [ self error: 'Preference for ', preferenceSymbol printString, ' not found' ].
]

{ #category : 'preferences - gemstone' }
RwGsPlatform >> userPreferenceFor: preferenceSymbol ifAbsent: aBlock [

	^ self _userPreferenceDict 
		at: preferenceSymbol 
		ifAbsent: aBlock
]
