Class {
	#name : 'RwModificationFiletreeWriterVisitor',
	#superclass : 'RwModificationCypressFamilyWriterVisitor',
	#instVars : [
		'packageExtension',
		'separateMethodMetaAndSource',
		'noMethodMetaData',
		'useCypressPropertiesFile',
		'monticelloMetadata'
	],
	#classInstVars : [
		'specials'
	],
	#category : 'Rowan-Core'
}

{ #category : 'initialization' }
RwModificationFiletreeWriterVisitor class >> initialize [
	"self initialize"
    specials := self initializeSpecials
]

{ #category : 'initialization' }
RwModificationFiletreeWriterVisitor class >> initializeSpecials [

    | map |
    map := Dictionary new.
    map
		at: $! put: 'bang';
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
        at: $, put: 'comma';
        at: $@ put: 'at'.
    map keys do: [ :key | map at: (map at: key) put: key ].
    ^ map
]

{ #category : 'accessing' }
RwModificationFiletreeWriterVisitor class >> selectorSpecials [

	^ specials ifNil: [ specials := self initializeSpecials ]
]

{ #category : 'class writing' }
RwModificationFiletreeWriterVisitor >> _classSourceDir [

	^ self _packageSourceDir / self currentClassDefinition name, 'class'
]

{ #category : 'package writing' }
RwModificationFiletreeWriterVisitor >> _filetreePackagePropertyDictFor: aPackageModification [

	| propertiesFile |
	propertiesFile := self _packageSourceDir /  '.filetree'.
	propertiesFile exists
		ifFalse: [
			"create default properties file"
			propertiesFile
				writeStreamDo: [:aStream |
				aStream 
					<< (self _toJSON:
 					(self class orderedDictionaryClass new
						add: 'noMethodMetaData' -> self noMethodMetaData;
						add: 'separateMethodMetaAndSource' -> self separateMethodMetaAndSource;
						add: 'useCypressPropertiesFile' -> self useCypressPropertiesFile;
						yourself))
					<< self  _newLine ] ].
	^ STON fromStream: (ZnBufferedReadStream on: propertiesFile  readStream)
]

{ #category : 'project writing' }
RwModificationFiletreeWriterVisitor >> _filetreeProjectPropertyDictFor: aProjectModification [

	| propertiesFile |
	propertiesFile := self _projectSourceDir /  '.filetree'.
	propertiesFile exists
		ifFalse: [
			"create default properties file"
			propertiesFile
				writeStreamDo: [:aStream |
				| dict |
				dict := (self class orderedDictionaryClass new)
					at: 'packageExtension' put: '.', self packageExtension;
					at: 'Metadata' put: self monticelloMetadata;
					yourself.
				aStream 
					<< (self _toJSON: dict )
					<< self  _newLine ] ].
	^ STON fromStream: (ZnBufferedReadStream on: propertiesFile  readStream)
]

{ #category : 'method writing' }
RwModificationFiletreeWriterVisitor >> _methodClassDir [

	^ self _classSourceDir / 'class'
]

{ #category : 'method writing' }
RwModificationFiletreeWriterVisitor >> _methodFileNameFor: aMethodDefinition [
	| selector |
	selector := aMethodDefinition selector.
	^ (selector includes: $:)
	ifTrue: [ 
	  selector
		collect: [ :each | 
		  each = $:
			ifTrue: [ $. ]
			ifFalse: [ each ] ] ]
	ifFalse: [ 
	  (self class selectorSpecials includes: selector first)
		ifFalse: [ selector ]
		ifTrue: [ 
		  | output specials |
		  specials := self class seletorSpecials.
		  output := String new writeStream.
		  output nextPut: $^.
		  selector
			do: [ :each | 
			  output
				nextPutAll:
				  ((specials includes: each)
					ifTrue: [ specials at: each ]
					ifFalse: [ each asString ]) ]
			separatedBy: [ output nextPut: $. ].
		  output contents ] ]
]

{ #category : 'method writing' }
RwModificationFiletreeWriterVisitor >> _methodInstanceDir [

	^ self _classSourceDir / 'instance'
]

{ #category : 'class writing' }
RwModificationFiletreeWriterVisitor >> _typeClassDefinitionOf: aClassDefinition [
	"order is alphabetical"

	| definition |
	definition := self class orderedDictionaryClass new.

	definition 		
		at: #category put: aClassDefinition category.

	(aClassDefinition classInstVarNames)
		ifNotEmpty: [ :vars | definition at: #classinstvars put: vars asArray ].

	(aClassDefinition classVarNames)
		ifNotEmpty: [ :vars | definition at: #classvars put: vars asArray ].

	(aClassDefinition gs_constraints)
		ifNotEmpty: [:gs_constraints | definition at: #'gs_constraints' put: gs_constraints asArray ].

	(aClassDefinition gs_options)
		ifNotEmpty: [:gs_options | definition at: #'gs_options' put: gs_options asArray ].

	(aClassDefinition instVarNames)
		ifNotEmpty: [ :vars | definition at: #instvars put: vars asArray ].

	definition at: #name put: aClassDefinition name.

	((aClassDefinition poolDictionaryNames) collect: [:each | each asString])
		ifNotEmpty: [ :vars | definition at: #pools put: vars asArray ].

	definition at: #super put: aClassDefinition superclassName.

	definition at: #type put: aClassDefinition classType.

	^ self _toJSON: definition
]

{ #category : 'actions' }
RwModificationFiletreeWriterVisitor >> addedMethod: aMethodModification [

	| methodDefinition methodDir |

	"create instance/class directory as needed ... write method source file"
	methodDir := (aMethodModification isMeta
		ifTrue: [ self _methodClassDir ]
		ifFalse: [ self _methodInstanceDir ]) ensureCreateDirectory.

	methodDefinition := aMethodModification after.

	(methodDir / (self _methodFileNameFor: methodDefinition))
		writeStreamDo: [:aStream |
		aStream 
			<< methodDefinition protocol
			<< self _newLine
			<< (methodDefinition source withLineEndings: self _newLine) ]
]

{ #category : 'actions' }
RwModificationFiletreeWriterVisitor >> addedPackage: aPackageModification [

	| packageProperties |
	currentPackageDefinition := aPackageModification after.

	"create package directory and properties.json file"
	self _packageSourceDir ensureCreateDirectory.
	(self _packageSourceDir /  'properties.json')
		writeStreamDo: [:aStream |
		aStream 
			<< '{'
			<< self  _newLine
			<< '	 }'].

	packageProperties := self _filetreePackagePropertyDictFor: aPackageModification.
	(packageProperties at:  'noMethodMetaData' ifAbsent: []) = self noMethodMetaData
		ifFalse: [
			"Rowan can read filetree repositories with methodMetaData, but does write method metadata"
			self error: 'method metadata not supported.' ].
	(packageProperties at:  'separateMethodMetaAndSource' ifAbsent: []) = self separateMethodMetaAndSource
		ifFalse: [
			"Rowan can read filetree repositories with separateMethodMetaAndSource, but does write separateMethodMetaAndSource"
			self error: 'separateMethodMetaAndSource not supported.' ].
	(packageProperties at:  'useCypressPropertiesFile' ifAbsent: []) = self useCypressPropertiesFile
		ifFalse: [
			"Rowan can read filetree repositories without CypressPropertiesFile, but does write without useCypressPropertiesFile"
			self error: 'only useCypressPropertiesFile not supported.' ].

	self processPackage: aPackageModification
]

{ #category : 'package writing' }
RwModificationFiletreeWriterVisitor >> monticelloMetadata [
	"monticello metadata  --- for writing metadataless is the only supported format"

	^ monticelloMetadata ifNil: [ monticelloMetadata := false ]
]

{ #category : 'package writing' }
RwModificationFiletreeWriterVisitor >> noMethodMetaData [

	^ noMethodMetaData ifNil: [ noMethodMetaData := true ]
]

{ #category : 'package writing' }
RwModificationFiletreeWriterVisitor >> packageExtension [

	^ packageExtension ifNil: [ packageExtension := 'package' ]
]

{ #category : 'class writing' }
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

	| projectProperties |

	"confirm that the project source is written in Tonel format"
	(self _repositoryFormatFor: aProjectModification) = 'filetree' ifFalse: [ self error: 'expected tonel format repository' ].

	projectProperties := self _filetreeProjectPropertyDictFor: aProjectModification.
	packageExtension := projectProperties at: 'packageExtension' ifAbsent: [ self packageExtension ].
	(packageExtension beginsWith: '.')
		ifTrue: [
			"properties file includes leading $., so strip it"
			packageExtension := packageExtension copyFrom: 2 to: packageExtension size ].
	(projectProperties at: 'Metadata' ifAbsent: [ true ])
		ifTrue: [
			"Rowan can read filetree repositories with Monticello metadata, but cannot write Monticello metadata"
			self error: 'Monticello metadata not supported.' ].

	super processProject: aProjectModification.
]

{ #category : 'package writing' }
RwModificationFiletreeWriterVisitor >> separateMethodMetaAndSource [

	^ separateMethodMetaAndSource ifNil: [ separateMethodMetaAndSource := false ]
]

{ #category : 'package writing' }
RwModificationFiletreeWriterVisitor >> useCypressPropertiesFile [

	^ useCypressPropertiesFile ifNil: [ useCypressPropertiesFile := true ]
]

{ #category : 'visiting' }
RwModificationFiletreeWriterVisitor >> visitMethodModification: aMethodModification [

	aMethodModification isAddition
		ifTrue: [ ^ self addedMethod: aMethodModification ] .
	aMethodModification isDeletion
		ifTrue: [ ^ self deletedMethod: aMethodModification ].
	^ self changedMethod: aMethodModification
]

{ #category : 'visiting' }
RwModificationFiletreeWriterVisitor >> visitMethodsModification: aMethodsModification [

	aMethodsModification elementsModified
		do: [:each | each acceptVisitor: self]
]
