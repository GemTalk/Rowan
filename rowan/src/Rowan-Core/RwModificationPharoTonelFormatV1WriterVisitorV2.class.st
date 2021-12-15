Class {
	#name : 'RwModificationPharoTonelFormatV1WriterVisitorV2',
	#superclass : 'RwModificationTonelWriterVisitorV2',
	#category : 'Rowan-Core'
}

{ #category : 'class writing' }
RwModificationPharoTonelFormatV1WriterVisitorV2 >> _methodDefinitionOf: aMethodDefinition [
	| excludedMethodProperties methodProperties exportedProperties |
	excludedMethodProperties := #(#'_gsFileOffset' #'_gsFileName' #'category' #'protocol' #'selector').
	exportedProperties := self class orderedDictionaryClass new
		at: #'category' put: aMethodDefinition protocol asSymbol;
		yourself.
	methodProperties := aMethodDefinition properties.
	(methodProperties keys
		reject: [ :each | excludedMethodProperties includes: each ]) asArray sort
		do: [ :key | exportedProperties at: key asSymbol put: (methodProperties at: key) asSymbol].
	^ self _toSTON: exportedProperties
]

{ #category : 'private' }
RwModificationPharoTonelFormatV1WriterVisitorV2 >> _toSTON: anObject [
	^ (String streamContents: [ :stream | 
		(PharoTonelV1STONWriter on: stream) nextPut: anObject ])
		withLineEndings: self _newLine
]

{ #category : 'class writing' }
RwModificationPharoTonelFormatV1WriterVisitorV2 >> _typeClassDefinitionOf: aClassDefinition [
	| definition |
	
	definition := self class orderedDictionaryClass new 
		at: #name put: aClassDefinition name asSymbol; 
		at: #superclass put: aClassDefinition superclassName asSymbol;
		yourself.

	aClassDefinition classType = 'normal' ifFalse: [ 
		definition at: #type put: aClassDefinition classType asSymbol ].
	
	(aClassDefinition instVarNames)
		ifNotEmpty: [ :vars | definition at: #instVars put: (vars asArray collect: [:each | each asSymbol]) ].

	(aClassDefinition classVarNames)
		ifNotEmpty: [ :vars | definition at: #classVars put: (vars asArray collect: [:each | each asSymbol]) ].
		
	((aClassDefinition poolDictionaryNames) collect: [:each | each asString])
		ifNotEmpty: [ :vars | definition at: #pools put: vars asArray ].
		
	(aClassDefinition classInstVarNames)
		ifNotEmpty: [ :vars | definition at: #classInstVars put: (vars asArray collect: [:each | each asSymbol])].

	(aClassDefinition gs_constraints)
		ifNotEmpty: [:gs_constraints | definition at: #'gs_constraints' put: gs_constraints asArray ].

	(aClassDefinition gs_options)
		ifNotEmpty: [:gs_options | definition at: #'gs_options' put: gs_options asArray ].

	(aClassDefinition gs_reservedOop)
		ifNotEmpty: [:gs_reservedOop | definition at: #'gs_reservedoop' put: gs_reservedOop asString ].

	definition 		
		at: #category put: aClassDefinition category asSymbol.
	
	^ self _toSTON: definition
]

{ #category : 'class extension writing' }
RwModificationPharoTonelFormatV1WriterVisitorV2 >> _writeClassExtension: aClassExtension on: aStream [

	aStream << 'Extension '
				<< (self _toSTON: {(#'name' -> aClassExtension name)} asDictionary) << self _newLine.
]

{ #category : 'actions' }
RwModificationPharoTonelFormatV1WriterVisitorV2 >> addedPackage: aPackageModification [
	| packageProperties exportedPackageProperties |
	currentPackageDefinition := aPackageModification after.

	packageProperties := currentPackageDefinition properties.
	exportedPackageProperties := self class orderedDictionaryClass new.
	exportedPackageProperties at: #'name' put: (packageProperties at: 'name') asSymbol.
	(packageProperties at: 'comment' ifAbsent: [  ])
		ifNotNil: [ :comment | exportedPackageProperties at: #'comment' put: comment ].
	(packageProperties keys
		reject: [ :key | key = 'name' or: [ key = 'gs_SymbolDictionary' ] ]) asArray
		sort
		do: [ :key | exportedPackageProperties at: key asSymbol put: (packageProperties at: key) ].

	self _packageSourceDir ensureCreateDirectory.
	self _packageSourceDir / 'package.st'
		writeStreamDo: [ :aStream | 
			aStream << 'Package ' << (self _toSTON: exportedPackageProperties)
				<< self _newLine ].

	self processPackage: aPackageModification
]
