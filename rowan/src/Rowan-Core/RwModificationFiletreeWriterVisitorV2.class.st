Class {
	#name : 'RwModificationFiletreeWriterVisitorV2',
	#superclass : 'RwModificationCypressFamilyWriterVisitorV2',
	#instVars : [
		'instanceFileNameMap',
		'classFileNameMap',
		'instanceBeforeFileNameMap',
		'classBeforeFileNameMap',
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
RwModificationFiletreeWriterVisitorV2 class >> initialize [
	"self initialize"
    specials := self initializeSpecials
]

{ #category : 'initialization' }
RwModificationFiletreeWriterVisitorV2 class >> initializeSpecials [

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
RwModificationFiletreeWriterVisitorV2 class >> selectorSpecials [

	^ specials ifNil: [ specials := self initializeSpecials ]
]

{ #category : 'class writing' }
RwModificationFiletreeWriterVisitorV2 >> _classExtensionOf: aClassExtension [
	"order is alphabetical"

	| definition |
	definition := self class orderedDictionaryClass new.

	definition at: #name put: aClassExtension name.

	^ self _toJSON: definition
]

{ #category : 'class writing' }
RwModificationFiletreeWriterVisitorV2 >> _classExtensionSourceDir [

	| filename |
	filename := classExtFileNameMap 
		at:  self currentClassExtension name
		ifAbsent: [
			"in the case of class removal, need to use the before class def map"
			classExtBeforeFileNameMap at: self currentClassExtension name ].
	^ self _packageSourceDir / filename, 'extension'
]

{ #category : 'class writing' }
RwModificationFiletreeWriterVisitorV2 >> _classSourceDir [

	| filename |
	filename := classDefFileNameMap 
		at:  self currentClassDefinition name
		ifAbsent: [
			"in the case of class removal, need to use the before class def map"
			classDefBeforeFileNameMap at: self currentClassDefinition name ].
	^ self _packageSourceDir / filename, 'class'
]

{ #category : 'method writing' }
RwModificationFiletreeWriterVisitorV2 >> _createFileNameMapForMethods: aMethodDefinitionDictionary [
  "https://github.com/dalehenrich/filetree/issues/92"

  "answer a dictionary that maps each definition selector to a filename that is guaranteed unique on case insensitive file systems"

  | map filenameMap |
  map := Dictionary new.
  aMethodDefinitionDictionary
    do: [ :mDef | 
      | sel col |
      "sort into bins by lowercase selector. "
      sel := mDef selector asLowercase.
      col := map
        at: sel
        ifAbsentPut: [ OrderedCollection new ].
      col add: mDef ].
  filenameMap := Dictionary new.
  map values
	do: [ :col | 
	  | sortedCol |
	  col size = 1
		ifTrue: [ 
		  | def |
		  "no need to distinguish filename"
		  def := col at: 1.
		  filenameMap
			at: def selector
			put: (self _methodFileNameFor: def) ]
		ifFalse: [ 
		  "tack on postfix to guarantee  file names are unique on case insensitive file systems"
		  sortedCol := col sort: [ :a :b | a selector asString _unicodeLessThan: b selector asString ].
		  (1 to: sortedCol size) do: [ :index | 
			| def filename |
			def := sortedCol at: index.
			filename := self _methodFileNameFor: def.
			filename := filename , '..' , index printString.
			filenameMap at: def selector put: filename ] ] ].
  ^ filenameMap
]

{ #category : 'package writing' }
RwModificationFiletreeWriterVisitorV2 >> _filetreePackagePropertyDictFor: aPackageModification [

	| propertiesFile |
	propertiesFile := self _packageSourceDir /  '.filetree'.
	propertiesFile exists
		ifFalse: [
			"create default properties file"
			propertiesFile
				writeStreamDo: [:aStream |
				aStream truncate.
				aStream 
					<< (self _toJSON:
 					(self class orderedDictionaryClass new
						add: 'noMethodMetaData' -> self noMethodMetaData;
						add: 'separateMethodMetaAndSource' -> self separateMethodMetaAndSource;
						add: 'useCypressPropertiesFile' -> self useCypressPropertiesFile;
						yourself)) ] ].
	^ self _readObjectFrom: propertiesFile
]

{ #category : 'project writing' }
RwModificationFiletreeWriterVisitorV2 >> _filetreeProjectPropertyDictFor: aProjectModification [

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
				aStream truncate.
				aStream 
					<< (self _toJSON: dict )
					<< self  _newLine ] ].
	^ self _readObjectFrom: propertiesFile
]

{ #category : 'method writing' }
RwModificationFiletreeWriterVisitorV2 >> _methodClassDir [

	^ self _classSourceDir / 'class'
]

{ #category : 'method writing' }
RwModificationFiletreeWriterVisitorV2 >> _methodClassExtensionDir [

	^ self _classExtensionSourceDir / 'class'
]

{ #category : 'method writing' }
RwModificationFiletreeWriterVisitorV2 >> _methodFileNameFor: aMethodDefinition [
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
		  specials := self class selectorSpecials.
		  output := WriteStream on: String new.
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
RwModificationFiletreeWriterVisitorV2 >> _methodInstanceDir [

	^ self _classSourceDir / 'instance'
]

{ #category : 'method writing' }
RwModificationFiletreeWriterVisitorV2 >> _methodInstanceExtensionDir [

	^ self _classExtensionSourceDir / 'instance'
]

{ #category : 'private' }
RwModificationFiletreeWriterVisitorV2 >> _toJSON: anObject [
	| str |
	str := WriteStreamPortable on: String new.
	anObject _writeCypressJsonOn: str.
	^ str contents
]

{ #category : 'class writing' }
RwModificationFiletreeWriterVisitorV2 >> _typeClassDefinitionOf: aClassDefinition [
	"order is alphabetical"

	| definition |
	definition := self class orderedDictionaryClass new.

	definition at: #commentStamp put: (aClassDefinition propertyAt: #commentStamp ifAbsent: ['']).

	definition at: #super put: aClassDefinition superclassName.

	definition 		
		at: #category put: aClassDefinition category.

	definition at: #classinstvars put: (aClassDefinition classInstVarNames) asArray.

	definition at: #pools put: ((aClassDefinition poolDictionaryNames) collect: [:each | each asString]) asArray.

	definition at: #classvars put: (aClassDefinition classVarNames) asArray.

	definition at: #instvars put: (aClassDefinition instVarNames) asArray.

	definition at: #name put: aClassDefinition name.

	definition at: #type put: aClassDefinition classType.

	(aClassDefinition gs_constraints)
		ifNotEmpty: [:gs_constraints | definition at: #'gs_constraints' put: gs_constraints asArray ].

	(aClassDefinition gs_options)
		ifNotEmpty: [:gs_options | definition at: #'gs_options' put: gs_options asArray ].

	(aClassDefinition gs_reservedOop)
		ifNotEmpty: [:gs_reservedOop | definition at: #'gs_reservedoop' put: gs_reservedOop asString ].

	^ self _toJSON: definition
]

{ #category : 'actions' }
RwModificationFiletreeWriterVisitorV2 >> addedMethod: aMethodModification [

	| methodDefinition methodDir methodFileName |

	methodDefinition := aMethodModification after.

	"create instance/class directory as needed ... write method source file"
	 (aMethodModification isMeta
		ifTrue: [
			methodFileName := classFileNameMap at: methodDefinition selector.
			methodDir := self _methodClassDir ensureCreateDirectory ]
		ifFalse: [ 
			methodFileName := instanceFileNameMap at: methodDefinition selector.
			methodDir := self _methodInstanceDir ensureCreateDirectory ]) .


	(methodDir / methodFileName, 'st')
		writeStreamDo: [:aStream |
		aStream truncate.
		aStream 
			<< methodDefinition protocol
			<< self _newLine
			<< (methodDefinition source withLineEndings: self _newLine) ]
]

{ #category : 'actions' }
RwModificationFiletreeWriterVisitorV2 >> addedMethodExtension: aMethodExtensionModification [

	| methodDefinition methodDir methodFileName |

	methodDefinition := aMethodExtensionModification after.

	"create instance/class directory as needed ... write method source file"
	 (aMethodExtensionModification isMeta
		ifTrue: [
			methodFileName := classFileNameMap at: methodDefinition selector.
			methodDir := self _methodClassExtensionDir ensureCreateDirectory ]
		ifFalse: [ 
			methodFileName := instanceFileNameMap at: methodDefinition selector.
			methodDir := self _methodInstanceExtensionDir ensureCreateDirectory ]) .


	(methodDir / methodFileName, 'st')
		writeStreamDo: [:aStream |
		aStream truncate.
		aStream 
			<< methodDefinition protocol
			<< self _newLine
			<< (methodDefinition source withLineEndings: self _newLine) ]
]

{ #category : 'actions' }
RwModificationFiletreeWriterVisitorV2 >> addedPackage: aPackageModification [

	| packageProperties |
	currentPackageDefinition := aPackageModification after.

	"create package directory and properties.json file"
	self _packageSourceDir ensureCreateDirectory.
	(self _packageSourceDir /  'properties.json')
		writeStreamDo: [:aStream |
		aStream truncate.
		aStream 
			<< '{ }'  ].

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

{ #category : 'actions' }
RwModificationFiletreeWriterVisitorV2 >> changedMethod: aMethodModification [

	| methodDefinition methodDir methodFileName |

	methodDefinition := aMethodModification after.

	"create instance/class directory as needed ... write method source file"
	 (aMethodModification isMeta
		ifTrue: [
			methodFileName := classFileNameMap at: methodDefinition selector.
			methodDir := self _methodClassDir ensureCreateDirectory ]
		ifFalse: [ 
			methodFileName := instanceFileNameMap at: methodDefinition selector.
			methodDir := self _methodInstanceDir ensureCreateDirectory ]) .


	(methodDir / methodFileName, 'st')
		writeStreamDo: [:aStream |
		aStream truncate.
		aStream 
			<< methodDefinition protocol
			<< self _newLine
			<< (methodDefinition source withLineEndings: self _newLine) ]
]

{ #category : 'actions' }
RwModificationFiletreeWriterVisitorV2 >> changedMethodExtension: aMethodExtensionModification [

	self addedMethodExtension: aMethodExtensionModification

]

{ #category : 'accessing' }
RwModificationFiletreeWriterVisitorV2 >> defaultPackageFormat [

	^ 'filetree'
]

{ #category : 'actions' }
RwModificationFiletreeWriterVisitorV2 >> deletedClass: aClassModification [

	currentClassDefinition := aClassModification before.

	self _classSourceDir ensureDeleteAll
]

{ #category : 'actions' }
RwModificationFiletreeWriterVisitorV2 >> deletedClassExtension: aClassExtensionModification [

	currentClassExtension := aClassExtensionModification before.

	self _classExtensionSourceDir ensureDeleteAll
]

{ #category : 'actions' }
RwModificationFiletreeWriterVisitorV2 >> deletedMethod: aMethodModification [

	| methodDefinition methodDir methodFileName |

	methodDefinition := aMethodModification before.

	"delete method source file"
	 (aMethodModification isMeta
		ifTrue: [
			methodFileName := classFileNameMap at: methodDefinition selector
				ifAbsent: [ classBeforeFileNameMap at: methodDefinition selector ].
			methodDir := self _methodClassDir ensureCreateDirectory ]
		ifFalse: [ 
			methodFileName := instanceFileNameMap at: methodDefinition selector
				ifAbsent: [ instanceBeforeFileNameMap at: methodDefinition selector ].
			methodDir := self _methodInstanceDir ensureCreateDirectory ]) .


	(methodDir / methodFileName, 'st') ensureDelete
]

{ #category : 'actions' }
RwModificationFiletreeWriterVisitorV2 >> deletedMethodExtension: aMethodExtensionModification [

	| methodDefinition methodDir methodFileName |

	methodDefinition := aMethodExtensionModification before.

	"create instance/class directory as needed ... write method source file"
	 (aMethodExtensionModification isMeta
		ifTrue: [
			methodFileName := classBeforeFileNameMap at: methodDefinition selector.
			methodDir := self _methodClassExtensionDir ensureCreateDirectory ]
		ifFalse: [ 
			methodFileName := instanceBeforeFileNameMap at: methodDefinition selector.
			methodDir := self _methodInstanceExtensionDir ensureCreateDirectory ]) .


	(methodDir / methodFileName, 'st') ensureDelete
]

{ #category : 'package writing' }
RwModificationFiletreeWriterVisitorV2 >> monticelloMetadata [
	"monticello metadata  --- for writing metadataless is the only supported format"

	^ monticelloMetadata ifNil: [ monticelloMetadata := 'false' ]
]

{ #category : 'package writing' }
RwModificationFiletreeWriterVisitorV2 >> noMethodMetaData [

	^ noMethodMetaData ifNil: [ noMethodMetaData := true ]
]

{ #category : 'package writing' }
RwModificationFiletreeWriterVisitorV2 >> packageExtension [

	^ packageExtension ifNil: [ packageExtension := 'package' ]
]

{ #category : 'class writing' }
RwModificationFiletreeWriterVisitorV2 >> processClass: aClassModification [

	"create class directory and class defintion structure"

	self _classSourceDir ensureCreateDirectory.

	(self _classSourceDir /  'README.md')
		writeStreamDo: [:aStream |
		aStream truncate.
		aStream 
			<< (self _commentOf: self currentClassDefinition) ].

	(self _classSourceDir /  'properties.json')
		writeStreamDo: [:aStream |
			aStream truncate.
			aStream << (self _typeClassDefinitionOf: self currentClassDefinition) ].

	instanceFileNameMap  := self _createFileNameMapForMethods: aClassModification after instanceMethodDefinitions.
	classFileNameMap := self _createFileNameMapForMethods: aClassModification after classMethodDefinitions.
	instanceBeforeFileNameMap  := self _createFileNameMapForMethods: aClassModification before instanceMethodDefinitions.
	classBeforeFileNameMap := self _createFileNameMapForMethods: aClassModification before classMethodDefinitions.

	currentClassDefinition instanceMethodDefinitions isEmpty
		ifTrue: [  self _methodInstanceDir ensureDeleteAll ]
		ifFalse: [ aClassModification instanceMethodsModification acceptVisitor: self ].
	currentClassDefinition classMethodDefinitions isEmpty
		ifTrue: [  self _methodClassDir ensureDeleteAll ]
		ifFalse: [ aClassModification classMethodsModification acceptVisitor: self ]
]

{ #category : 'class writing' }
RwModificationFiletreeWriterVisitorV2 >> processClassExtension: aClassExtensionModification [

	"create class extension directory and defintion structure"

	(currentClassExtension instanceMethodDefinitions isEmpty and: [ currentClassExtension classMethodDefinitions isEmpty ])
		ifTrue: [ self _classExtensionSourceDir ensureDeleteAll ]
		ifFalse: [ 
			self _classExtensionSourceDir ensureCreateDirectory.
			(self _classExtensionSourceDir /  'properties.json')
				writeStreamDo: [:aStream |
					aStream truncate.
					aStream << (self _classExtensionOf: self currentClassExtension) ].

			instanceFileNameMap  := self _createFileNameMapForMethods: aClassExtensionModification after instanceMethodDefinitions.
			classFileNameMap := self _createFileNameMapForMethods: aClassExtensionModification after classMethodDefinitions.
			instanceBeforeFileNameMap  := self _createFileNameMapForMethods: aClassExtensionModification before instanceMethodDefinitions.
			classBeforeFileNameMap := self _createFileNameMapForMethods: aClassExtensionModification before classMethodDefinitions.

			aClassExtensionModification instanceMethodsModification acceptVisitor: self.
			aClassExtensionModification classMethodsModification acceptVisitor: self ]
]

{ #category : 'project writing' }
RwModificationFiletreeWriterVisitorV2 >> processProject: aProjectModification [

	| projectProperties |

	packageDefFileNameMap := self _createFileNameMapForClassesOrPackages:  aProjectModification after packages.
	packageDefBeforeFileNameMap := self _createFileNameMapForClassesOrPackages:  aProjectModification before packages.

	"confirm that the project source is written in filetree format"
	(self _repositoryFormatFor: self packagesRoot) = 'filetree' ifFalse: [ self error: 'expected filetree format repository' ].

	projectProperties := self _filetreeProjectPropertyDictFor: aProjectModification.
	packageExtension := projectProperties at: 'packageExtension' ifAbsent: [ self packageExtension ].
	(packageExtension beginsWith: '.')
		ifTrue: [
			"properties file includes leading $., so strip it"
			packageExtension := packageExtension copyFrom: 2 to: packageExtension size ].
	(projectProperties at: 'Metadata' ifAbsent: [ 'true' ])  = 'true'
		ifTrue: [
			"Rowan can read filetree repositories with Monticello metadata, but cannot write Monticello metadata"
			self error: 'Monticello metadata not supported.' ].

	super processProject: aProjectModification.
]

{ #category : 'package writing' }
RwModificationFiletreeWriterVisitorV2 >> separateMethodMetaAndSource [

	^ separateMethodMetaAndSource ifNil: [ separateMethodMetaAndSource := false ]
]

{ #category : 'package writing' }
RwModificationFiletreeWriterVisitorV2 >> useCypressPropertiesFile [

	^ useCypressPropertiesFile ifNil: [ useCypressPropertiesFile := true ]
]

{ #category : 'visiting' }
RwModificationFiletreeWriterVisitorV2 >> visitExtensionMethodModification: aMethodExtensionModification [

	aMethodExtensionModification isAddition
		ifTrue: [ ^ self addedMethodExtension: aMethodExtensionModification ] .
	aMethodExtensionModification isDeletion
		ifTrue: [ ^ self deletedMethodExtension: aMethodExtensionModification ].
	^ self changedMethodExtension: aMethodExtensionModification
]

{ #category : 'visiting' }
RwModificationFiletreeWriterVisitorV2 >> visitExtensionMethodsModification: aMethodExtensionsModification [

	aMethodExtensionsModification elementsModified
		do: [ :each | each acceptVisitor: self ]
]

{ #category : 'visiting' }
RwModificationFiletreeWriterVisitorV2 >> visitMethodModification: aMethodModification [

	aMethodModification isAddition
		ifTrue: [ ^ self addedMethod: aMethodModification ] .
	aMethodModification isDeletion
		ifTrue: [ ^ self deletedMethod: aMethodModification ].
	^ self changedMethod: aMethodModification
]

{ #category : 'visiting' }
RwModificationFiletreeWriterVisitorV2 >> visitMethodsModification: aMethodsModification [

	aMethodsModification elementsModified
		do: [:each | each acceptVisitor: self]
]
