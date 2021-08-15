"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressLoader',
	#superclass : 'CypressObject',
	#instVars : [
		'additions',
		'defaultSymbolDictionaryName',
		'errors',
		'exceptionClass',
		'methodAdditions',
		'obsoletions',
		'provisions',
		'removals',
		'requirements',
		'unloadable'
	],
	#category : 'Cypress-Definitions'
}

{ #category : 'accessing' }
CypressLoader class >> defaultSymbolDictionaryName [
  "Name of the SymbolDictionary where new classes should be installed"

  ^ (SessionTemps current 
      at: #'Cypress_Loader_Default_Symbol_Dictionary_Name' 
      ifAbsent: [] ) 
        ifNil: [
          System myUserProfile userId = 'SystemUser'
          ifTrue: [ #Globals ]
          ifFalse: [ #'UserGlobals' ] ]

]

{ #category : 'accessing' }
CypressLoader class >> defaultSymbolDictionaryName: aSymbol [
  SessionTemps current 
      at: #'Cypress_Loader_Default_Symbol_Dictionary_Name'
      put: aSymbol

]

{ #category : 'unloading' }
CypressLoader class >> unloadSnapshot: aSnapshot [
  ^ self new
    unloadSnapshot: aSnapshot;
    load

]

{ #category : 'loading' }
CypressLoader class >> updatePackage: aPackage defaultSymbolDictionaryName: defaultSymbolDictionaryName withSnapshot: aSnapshot [
  "Answer the loader used to apply the update."

  ^ self new
    defaultSymbolDictionaryName: defaultSymbolDictionaryName;
    updatePackage: aPackage withSnapshot: aSnapshot;
    load

]

{ #category : 'loading' }
CypressLoader class >> updatePackage: aPackage withSnapshot: aSnapshot [
  "Answer the loader used to apply the update."

  ^ self
    updatePackage: aPackage
    defaultSymbolDictionaryName: nil
    withSnapshot: aSnapshot

]

{ #category : 'updating' }
CypressLoader >> addFailedPatchOperation: aPatchOperation [

	self errors add: aPatchOperation

]

{ #category : 'accessing' }
CypressLoader >> additions [

	additions ifNil: [ additions := OrderedCollection new ].
	^additions

]

{ #category : 'loading' }
CypressLoader >> analyze [
  self
    analyzeRemovalOfAdditions;
    analyzeAdditions;
    analyzeRemovals

]

{ #category : 'loading' }
CypressLoader >> analyzeAdditions [

	| sorter |
	sorter := CypressDependencySorter new 
		addAll: self additions;
		addExternalProvisions: self provisions;
		yourself.
	additions := sorter orderedItems.
	requirements := sorter externalRequirements.
	unloadable := sorter required.

]

{ #category : 'loading' }
CypressLoader >> analyzeRemovalOfAdditions [
  "if there is an addition and a removal for the same definition, the addition wins ... needed when loading multiple packages and a defintion has been moved from one package to another --- see atomic loads for Metacello"

  | index |
  index := CypressDefinitionIndex
    definitions: (self additions collect: [ :each | each definition ]).
  self removals
    removeAllSuchThat: [ :removal | 
      (index
        definitionLike: removal definition
        ifPresent: [ :additionDefinition | self obsoletions at: additionDefinition description put: removal definition ]
        ifAbsent: [  ]) notNil ]

]

{ #category : 'loading' }
CypressLoader >> analyzeRemovals [

	| sorter |
	sorter := CypressDependencySorter new 
		addAll: self removals;
		yourself.
	removals := sorter orderedItems reverse.

]

{ #category : 'applying' }
CypressLoader >> applyAddition: aCypressPatchOperation [

	self additions add: aCypressPatchOperation

]

{ #category : 'applying' }
CypressLoader >> applyModification: aCypressPatchOperation [
  self additions add: aCypressPatchOperation.
  self obsoletions
    at: aCypressPatchOperation modification description
    put: aCypressPatchOperation obsoletion

]

{ #category : 'applying' }
CypressLoader >> applyRemoval: aCypressPatchOperation [

	self removals add: aCypressPatchOperation

]

{ #category : 'loading' }
CypressLoader >> attemptInitialLoad [
  ^ self attemptInitialLoad: true

]

{ #category : 'loading' }
CypressLoader >> attemptInitialLoad: doUnloads [

	self
		resetErrors;
		notifyOnFailedPatchOperations;
		loadAdditions: self additions .
  doUnloads ifTrue:[ self unloadRemovals: self removals].

]

{ #category : 'accessing' }
CypressLoader >> defaultSymbolDictionaryName [
  ^ defaultSymbolDictionaryName ifNil: [ self class defaultSymbolDictionaryName ]

]

{ #category : 'accessing' }
CypressLoader >> defaultSymbolDictionaryName: aSymbol [
  defaultSymbolDictionaryName := aSymbol

]

{ #category : 'loading' }
CypressLoader >> errorOnFailedPatchOperations [

	exceptionClass := CypressLoaderError.

]

{ #category : 'accessing' }
CypressLoader >> errors [
	errors ifNil: [self resetErrors].
	^errors

]

{ #category : 'loading' }
CypressLoader >> handleCompileError: aCompileError from: aPatchOperation [

	| undefinedSymbolErrors otherErrors |
	undefinedSymbolErrors := aCompileError errorDetails
				select: [:each | each first = 1031].
	otherErrors := aCompileError errorDetails
				reject: [:each | each first = 1031].
	undefinedSymbolErrors do: [:each | self requirements add: each last].
	aCompileError pass

]

{ #category : 'loading' }
CypressLoader >> handlePatchOperation: aPatchOperation failure: anException [
	"Signal the loader exception appropriate to the current phase.
	 Note that a handler may suppress the #addFailedPatchOperation: by
	 sending #return or #return: to the resignaled exception. Otherwise,
	 resumption from a resumable resignalled exception will continue through
	 this method."

	(exceptionClass patchOperation: aPatchOperation exception: anException) signal.
	self addFailedPatchOperation: aPatchOperation.

]

{ #category : 'loading' }
CypressLoader >> load [
  ^self load: true

]

{ #category : 'loading' }
CypressLoader >> load: doUnloads [
	self analyze .
  doUnloads ifTrue:[ self reportUnloadableDefinitions ].
	self attemptInitialLoad: doUnloads  ;
		retryFailedLoads;
		postLoad.

]

{ #category : 'loading' }
CypressLoader >> loadAdditions: somePatchOperations [
	"Load class definitions first, then method definitions."

	somePatchOperations
		do: [:each | self loadClassDefinition: each];
		do: [:each | self loadMethodDefinition: each].

]

{ #category : 'operations' }
CypressLoader >> loadClassDefinition: aPatchOperation [

	[ aPatchOperation loadClassDefinition: self defaultSymbolDictionaryName ]
		on: Error
		do: [:ex | self handlePatchOperation: aPatchOperation failure: ex].

]

{ #category : 'operations' }
CypressLoader >> loadMethodDefinition: aPatchOperation [

	
	[[aPatchOperation loadMethodDefinition]
		on: CompileError
		do: [:ex | self handleCompileError: ex from: aPatchOperation]]
			on: Error
			do: [:ex | self handlePatchOperation: aPatchOperation failure: ex]

]

{ #category : 'accessing' }
CypressLoader >> methodAdditions [

	^#()

]

{ #category : 'loading' }
CypressLoader >> notifyOnFailedPatchOperations [

	exceptionClass := CypressLoaderErrorNotification.

]

{ #category : 'accessing' }
CypressLoader >> obsoletions [
  obsoletions ifNil: [ obsoletions := Dictionary new ].
  ^ obsoletions

]

{ #category : 'loading' }
CypressLoader >> postLoad [
	"This is where the obsoletion is taken into account ..."

	self additions do: [:each | self postLoad: each].

]

{ #category : 'operations' }
CypressLoader >> postLoad: aPatchOperation [

	[aPatchOperation postLoadDefinition]
		on: Error
		do: [:ex | self handlePatchOperation: aPatchOperation failure: ex].

]

{ #category : 'accessing' }
CypressLoader >> provisions [
	^ provisions ifNil: [provisions := (self allClasses collect: [:cl | cl name asString]) asSet ]

]

{ #category : 'accessing' }
CypressLoader >> removals [

	removals ifNil: [ removals := OrderedCollection new ].
	^removals

]

{ #category : 'loading' }
CypressLoader >> reportUnloadableDefinitions [

	self unloadable isEmpty ifTrue: [^self].
	(CypressLoaderMissingClasses missingRequirementsMap: unloadable) signal.

]

{ #category : 'accessing' }
CypressLoader >> requirements [

	^requirements

]

{ #category : 'loading' }
CypressLoader >> resetErrors [

	errors := OrderedCollection new.

]

{ #category : 'loading' }
CypressLoader >> retryFailedLoads [
	"In case any of the failed loads were resolved by subsequent
	 patch operations after the initial attempt or by editting of the
	 failed patch operations by exception handling during the notification
	 phase (initial attempt)."

	| failed |
	failed := self errors.
	self
		resetErrors;
		errorOnFailedPatchOperations;
		loadAdditions: (self elementsInBoth: self additions and: failed);
		unloadRemovals: (self elementsInBoth: self removals and: failed)

]

{ #category : 'accessing' }
CypressLoader >> unloadable [

	unloadable ifNil: [ unloadable := OrderedCollection new ].
	^unloadable

]

{ #category : 'operations' }
CypressLoader >> unloadDefinition: aPatchOperation [

	[aPatchOperation unloadDefinition]
		on: Error
		do: [:ex | self handlePatchOperation: aPatchOperation failure: ex].

]

{ #category : 'loading' }
CypressLoader >> unloadRemovals: somePatchOperations [
	"Removals need to be done after adding classes and methods."

	somePatchOperations
		do: [:each | self unloadDefinition: each].

]

{ #category : 'unloading' }
CypressLoader >> unloadSnapshot: aSnapshot [
  |  patch |
  patch := CypressSnapshot empty patchRelativeToBase: aSnapshot.
  patch applyTo: self

]

{ #category : 'loading' }
CypressLoader >> updatePackage: aPackage withSnapshot: aSnapshot [

	| patch snapshot |
	snapshot := aPackage snapshot.
	patch := aSnapshot patchRelativeToBase: snapshot.
	patch applyTo: self.
	snapshot definitions do: [:ea | self provisions addAll: ea provisions]

]
