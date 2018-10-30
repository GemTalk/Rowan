Class {
	#name : 'RwModificationFiletreeWriterVisitor',
	#superclass : 'RwModificationCypressFamilyWriterVisitor',
	#category : 'Rowan-Core'
}

{ #category : 'class writing' }
RwModificationFiletreeWriterVisitor >> _classSourceDir [

	^ self _packageSourceDir / self currentClassDefinition name, 'class'
]

{ #category : 'class writing' }
RwModificationFiletreeWriterVisitor >> _typeClassDefinitionOf: aClassDefinition [
	| definition |
	
	definition := self class orderedDictionaryClass new 
		at: #name put: aClassDefinition name; 
		at: #superclass put: aClassDefinition superclassName;
		yourself.

	aClassDefinition classType = 'normal' ifFalse: [ 
		definition at: #type put: aClassDefinition classType ].
	
	(aClassDefinition instVarNames)
		ifNotEmpty: [ :vars | definition at: #instVars put: vars asArray ].

	(aClassDefinition classVarNames)
		ifNotEmpty: [ :vars | definition at: #classVars put: vars asArray ].
		
	((aClassDefinition poolDictionaryNames) collect: [:each | each asString])
		ifNotEmpty: [ :vars | definition at: #pools put: vars asArray ].
		
	(aClassDefinition classInstVarNames)
		ifNotEmpty: [ :vars | definition at: #classInstVars put: vars asArray ].

	(aClassDefinition gs_constraints)
		ifNotEmpty: [:gs_constraints | definition at: #'gs_constraints' put: gs_constraints asArray ].

	(aClassDefinition gs_options)
		ifNotEmpty: [:gs_options | definition at: #'gs_options' put: gs_options asArray ].

	definition 		
		at: #category put: aClassDefinition category.
	
	^ self _toJSON: definition
]

{ #category : 'actions' }
RwModificationFiletreeWriterVisitor >> addedPackage: aPackageModification [

	currentPackageDefinition := aPackageModification after.

	"create package directory and properties.json file"
	self _packageSourceDir ensureCreateDirectory.
	(self _packageSourceDir /  'properties.json')
		writeStreamDo: [:aStream |
		aStream 
			<< (self _toJSON: self class orderedDictionaryClass new)
			<< self  _newLine ].

	(self _packageSourceDir /  '.filetree')
		writeStreamDo: [:aStream |
		aStream 
			<< (self _toJSON: {
				'separateMethodMetaAndSource' -> false.
				'noMethodMetaData' -> true.
				'useCypressPropertiesFile' -> true.
			} asDictionary)
			<< self  _newLine ].

	self processPackage: aPackageModification
]

{ #category : 'actions' }
RwModificationFiletreeWriterVisitor >> processClass: aClassModification [

	"create class directory and class defintion structure"

	self _classSourceDir ensureCreateDirectory.

	(self _classSourceDir /  'README.md')
		writeStreamDo: [:aStream |
		aStream 
			<< (self _commentOf: self currentClassDefinition) ].

	(self _classSourceDir /  'properties.json')
		writeStreamDo: [:aStream |
			aStream 
				<< (self _typeClassDefinitionOf: self currentClassDefinition)
				<< self _newLine ].

	aClassModification instanceMethodsModification acceptVisitor: self.
	aClassModification classMethodsModification acceptVisitor: self
]

{ #category : 'project writing' }
RwModificationFiletreeWriterVisitor >> processProject: aProjectModification [
	"confirm that the project source is written in Tonel format"

	(self _repositoryFormatFor: aProjectModification) = 'filetree' ifFalse: [ self error: 'expected tonel format repository' ].

	(self _projectSourceDir /  '.filetree')
		writeStreamDo: [:aStream |
		| dict |
		dict := (self class orderedDictionaryClass new)
			at: 'packageExtension' put: '.package';
			yourself.
		aStream 
			<< (self _toJSON: dict )
			<< self  _newLine ].

	super processProject: aProjectModification.
]

{ #category : 'visiting' }
RwModificationFiletreeWriterVisitor >> visitMethodModification: aMethodModification [

self error: 'not yet implemented'.
]

{ #category : 'visiting' }
RwModificationFiletreeWriterVisitor >> visitMethodsModification: aMethodsModification [

	aMethodsModification elementsModified
		do: [:each | each acceptVisitor: self]
]
