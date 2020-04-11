"
  Example script that produces topaz bootstrap files for the Rowan projects:

	| repositoryRootPath projectSetDefinition projectSetModification visitor |
	repositoryRootPath := FileLocator dbfScratchDir / 'rowanBootstrap'.
	repositoryRootPath ensureCreateDirectory.

	projectSetDefinition := RwProjectSetDefinition new.
	Rowan image loadedProjects do: [:lp | projectSetDefinition addProject: lp asDefinition ].

	projectSetModification := projectSetDefinition compareAgainstBase: RwProjectSetDefinition new.
	visitor := RwGsModificationTopazWriterVisitor new
		repositoryRootPath: repositoryRootPath;
		yourself.
	false 
		ifTrue: [ visitor topazFilename: 'rowan_bootstrap' ]
		ifFalse: [
			| fileNameConfigsMap |
			fileNameConfigsMap := Dictionary new
				at: 'rowan_bootstrap_app' 
					put: (Dictionary new 
							at: 'Rowan' put: {{'Kernel'}. {}};
							at: 'FileSystemGs' put: nil;
							at: 'Cypress' put: nil;
							at: 'STON' put: nil;
							at: 'Tonel' put: nil;
							yourself);
				at: 'rowan_bootstrap_globals' 
					put: (Dictionary new 
							at: 'Rowan' put: {{'Globals'}. {}};
							at: 'FileSystemGs' put: {{'Default'}. {}};
							at: 'Cypress' put: {{'Default'}. {}};
							at: 'STON' put: {{'Bootstrap'}. {}};
							at: 'Tonel' put: {{'Bootstrap'}. {}};
							yourself);
				at: 'rowan_bootstrap_user_globals' 
					put: (Dictionary new 
							at: 'Rowan' put: {{'UserGlobals'}. {}};
							at: 'FileSystemGs' put: nil;
							at: 'Cypress' put: nil;
							at: 'STON' put: nil;
							at: 'Tonel' put: nil;
							yourself);
				yourself.
			visitor topazFilenameConfigsMap: fileNameConfigsMap].
	visitor visit: projectSetModification.
	visitor export.
"
Class {
	#name : 'RwGsModificationTopazWriterVisitor',
	#superclass : 'RwModificationWriterVisitor',
	#instVars : [
		'topazFilenameConfigsMap',
		'topazFilename',
		'topazFileHeader',
		'filenameExtension',
		'classSymbolDictionaryNames',
		'classDefinitions',
		'classExtensions',
		'bufferedStream',
		'topazFilenamePackageNamesMap',
		'classDefPackageNameMap',
		'classExtPackageNameMap',
		'classInitializationDefinitions',
		'buildPackageNamesMap',
		'repositoryRootPath'
	],
	#category : 'Rowan-GemStone-Core'
}

{ #category : 'private exporting' }
RwGsModificationTopazWriterVisitor >> _fileOutClassDeclaration: classDefinition on: aStream [
	| optionsString reservedOopString hasClassInstVars |
	aStream
		nextPutAll: 'doit';
		lf;
		nextPutAll: '(' , classDefinition superclassName;
		lf.
	hasClassInstVars := self
		_writeClassTypeMessage: classDefinition
		on: aStream
		hasInstanceVariables: [ aStream
			nextPutAll:
			'	instVarNames: #( ' , (self _stringForVariables: classDefinition instVarNames) , ' )';
			lf ].
	optionsString := ''.
	classDefinition gs_options isEmpty
		ifFalse: [ optionsString :=  ' ', (self _symbolsForVariables: classDefinition gs_options), ' ' ].
	reservedOopString := ''.
	classDefinition gs_reservedOop isEmpty
		ifFalse: [
			| strm |
			strm := WriteStream on: String new.
			strm nextPutAll: '	reservedOop: ', classDefinition gs_reservedOop; lf.
			reservedOopString := strm contents ].
	aStream
		nextPutAll: '	classVars: #( ' ,  (self _stringForVariables: classDefinition classVarNames) , ' )';
		lf.
	hasClassInstVars
		ifTrue: [ 
		aStream
			nextPutAll:
			'	classInstVars: #( ' ,  (self _stringForVariables: classDefinition classInstVarNames) , ' )';
			lf ].
	aStream
		nextPutAll: '	poolDictionaries: #()';
		lf;
		nextPutAll: '	inDictionary: ', (self classSymbolDictionaryNames at: classDefinition name);
		lf;
		nextPutAll: '	options: #(', optionsString, ')';
		lf;
		nextPutAll: reservedOopString;
		nextPutAll: ')';
		lf;
		nextPutAll: '		category: ' , classDefinition category printString , ';';
		lf;
		nextPutAll: '		comment: ' , classDefinition comment printString , ';';
		lf;
		nextPutAll: '		immediateInvariant.';
		lf;
		nextPutAll: 'true.';
		lf;
		nextPutAll: '%';
		lf;
		lf
]

{ #category : 'private exporting' }
RwGsModificationTopazWriterVisitor >> _fileOutMethod: methodDefinition forClass: className isMeta: isMeta on: aStream [

	aStream
		nextPutAll: 'category: ', methodDefinition protocol printString; lf;
		nextPutAll: (isMeta ifTrue: ['classmethod: '] ifFalse: ['method: ']), className; lf;
		nextPutAll: methodDefinition source.
	methodDefinition source last = Character lf
		ifFalse: [aStream lf].
	aStream nextPutAll: '%'; lf;
		lf
]

{ #category : 'private exporting' }
RwGsModificationTopazWriterVisitor >> _setBufferedStreamFor: filename [

	^ self _setBufferedStreamFor: filename extension: self filenameExtension
]

{ #category : 'private exporting' }
RwGsModificationTopazWriterVisitor >> _setBufferedStreamFor: filename extension: extension [

	| encodedStream |
	encodedStream := (self repositoryRootPath / filename, extension) writeStreamEncoded: 'utf8'.
	bufferedStream := ZnBufferedWriteStream on: encodedStream
]

{ #category : 'private exporting' }
RwGsModificationTopazWriterVisitor >> _stringForVariables: variableList [

	| stream |
	stream := WriteStreamPortable on: (String new: 100).
	variableList do: [:each | stream nextPutAll: each]
		separatedBy: [stream space].
	^stream contents
]

{ #category : 'private exporting' }
RwGsModificationTopazWriterVisitor >> _symbolsForVariables: variableList [

	| stream |
	stream := WriteStreamPortable on: (String new: 100).
	variableList do: [:each | stream nextPutAll: each asSymbol printString ]
		separatedBy: [stream space].
	^stream contents
]

{ #category : 'private exporting' }
RwGsModificationTopazWriterVisitor >> _writeClassTypeMessage: classDefinition on: aStream hasInstanceVariables: instanceVariableBlock [
	| classType classTypeMessage hasInstanceVariables hasReservedOop hasClassInstVars |
	hasInstanceVariables := true.
	hasReservedOop := classDefinition gs_reservedOop isEmpty not.
	hasClassInstVars := true.
	classType := classDefinition subclassType.
	classType = 'variable'
		ifTrue: [ 
			classTypeMessage :=  hasReservedOop
				ifTrue: [ '_newKernelIndexableSubclass:' ]
				ifFalse: [ 'indexableSubclass: ' ] ]
		ifFalse: [ classType = 'byteSubclass'
			ifTrue: [ 
				classTypeMessage :=  hasReservedOop
					ifTrue: [ '_newKernelByteSubclass:' ]
					ifFalse: [ 'byteSubclass: ' ].
				hasClassInstVars := hasInstanceVariables := false ]
			ifFalse: [ (classType = '' or: [classType = 'immediate'])
				ifTrue: [ 
					classTypeMessage :=  hasReservedOop
						ifTrue: [ '_newKernelSubclass:' ]
						ifFalse: [ 'subclass: ' ] ]
				ifFalse: [ self error: 'unknown subclass type: ' , classType ] ] ].
	aStream
		tab;
		nextPutAll: classTypeMessage , classDefinition name asString printString;
		lf.
	hasInstanceVariables
		ifTrue: [ instanceVariableBlock value ].
	^ hasClassInstVars
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> bufferedStream [

	^ bufferedStream ifNil: [ 
		| encodedStream |
		encodedStream := (self repositoryRootPath / self topazFilename, self filenameExtension) writeStreamEncoded: 'utf8'.
		bufferedStream := ZnBufferedWriteStream on: encodedStream ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> buildPackageNamesMap [

	"If true, topazFilenamePackageNamesMap will be built from topazFilenameConfigsMap.
		If false, existing topazFilenamePackageNamesMap will be used"

	^ buildPackageNamesMap ifNil: [ buildPackageNamesMap := true ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> classDefinitions [

	^ classDefinitions ifNil: [ classDefinitions := Dictionary new ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> classDefPackageNameMap [

	^ classDefPackageNameMap ifNil: [ classDefPackageNameMap := Dictionary new ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> classExtensions [

	^ classExtensions ifNil: [ classExtensions := Dictionary new ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> classExtPackageNameMap [

	^ classExtPackageNameMap ifNil: [ classExtPackageNameMap := IdentityKeyValueDictionary new ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> classInitializationDefinitions [

	^ classInitializationDefinitions
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> classSymbolDictionaryNames [

	^ classSymbolDictionaryNames ifNil: [ classSymbolDictionaryNames := Dictionary new ]
]

{ #category : 'actions' }
RwGsModificationTopazWriterVisitor >> deletededClass: aClassModification [

	"a deleted class is ignored for topaz ... the deleted class is simply not written"
]

{ #category : 'actions' }
RwGsModificationTopazWriterVisitor >> deletededClassExtension: aClassExtensionModification [

	"a deleted class extension is ignored for topaz ... the deleted methods are simply not written"
]

{ #category : 'actions' }
RwGsModificationTopazWriterVisitor >> deletededPackage: aPackageModification [

	"a deleted package is ignored for topaz ... the deleted classes and methods are simply not written out"
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> excludeClassInitializers [
  self dynamicInstVarAt: #excludeClassInitializers put: true
]

{ #category : 'exporting' }
RwGsModificationTopazWriterVisitor >> export [

	self topazFilenamePackageNamesMap keysAndValuesDo: [:filename :packageNames |
		| classDefinitionsInOrder classExtensionsInOrder classExtensionsList classDefinitionsList |

		self _setBufferedStreamFor: filename.
		self bufferedStream nextPutAll: self topazFileHeader.

		classInitializationDefinitions := Set new. "per file record"
		classDefinitionsList := Set new.

		self classDefPackageNameMap keysAndValuesDo: [:className :packageName |
			(packageNames includes: packageName)
				ifTrue: [ classDefinitionsList add: (self classDefinitions at: className) ] ].

		classDefinitionsInOrder := (RowanGsGeneralDependencySorter
			on: classDefinitionsList
			dependsOn: [:candidate | candidate superclassName]
			dependent: [:candidate | candidate name]) inOrder.
		self exportClassDefinitions: classDefinitionsInOrder;
			exportMethodDefinitions: classDefinitionsInOrder.

		"consolidate the classExtensions for a class from multiple packages into a single definition"
		classExtensionsList := Set new. 
		self classExtensions keysAndValuesDo: [:classExtName :extSet |
			| extsInConfig |
			extsInConfig := Set new.
			extSet do: [:ext |
				| packageName |
				packageName := self classExtPackageNameMap at: ext.
				(packageNames includes: packageName)
					ifTrue: [ extsInConfig add: ext ] ].

			extsInConfig size <= 1 
				ifTrue: [ classExtensionsList addAll: extsInConfig ]
				ifFalse: [
					| ar def |
					ar := extsInConfig asArray.
					def := (ar at: 1) copy.
					2 to: ar size do: [:index |
						| d |
						d := (ar at: index).
						d classMethodDefinitions values do: [:m | def addClassMethodDefinition: m ].
						d instanceMethodDefinitions values do: [:m | def addInstanceMethodDefinition: m ] ].
					classExtensionsList add: def ] ].

		classExtensionsInOrder := classExtensionsList sort: [:a :b | a name <= b name ].
		self
			exportExtensionMethodDefinitions: classExtensionsInOrder.

		self exportClassInitializations.

		self bufferedStream flush; close ].
]

{ #category : 'exporting' }
RwGsModificationTopazWriterVisitor >> exportClassDefinitions: classDefinitionsInOrder [

	| stream |
	stream := self bufferedStream.
	classDefinitionsInOrder isEmpty not
		ifTrue: [ 
			stream
				nextPutAll: '! Class Declarations'; lf;
				nextPutAll: '! Generated file, do not Edit'; lf;
				lf ].
	classDefinitionsInOrder do: [:classDef | 
		self _fileOutClassDeclaration: classDef on: stream ].
]

{ #category : 'exporting' }
RwGsModificationTopazWriterVisitor >> exportClassInitializations [
	| stream exclude |
	self classInitializationDefinitions isEmpty ifTrue: [ ^ self ].
	stream := self bufferedStream.
  exclude := (self dynamicInstVarAt: #excludeClassInitializers) ~~ nil  .
  exclude ifTrue:[
    stream nextPutAll: '! Class Initialization Excluded by export visitor'; lf .
  ] ifFalse:[
	  stream 
		  nextPutAll: '! Class Initialization'; lf;
      lf;
		  nextPutAll: 'run'; lf.
  ].
	(self classInitializationDefinitions sort: [:a :b | a name <= b name ]) do: [ :classDef |
    exclude ifTrue:[ stream nextPutAll:'!  ' ].
		stream nextPutAll: classDef name, ' initialize.'; lf 
  ].
  exclude ifFalse:[
	  stream nextPutAll: 'true'; lf;
	  nextPutAll: '%'; lf .
  ].
]

{ #category : 'exporting' }
RwGsModificationTopazWriterVisitor >> exportExtensionMethodDefinitions: classDefinitionsInOrder [

	self exportMethodDefinitions: classDefinitionsInOrder labeled: 'Class extensions'
]

{ #category : 'exporting' }
RwGsModificationTopazWriterVisitor >> exportMethodDefinitions: classDefinitionsInOrder [

	self exportMethodDefinitions: classDefinitionsInOrder labeled: 'Class implementation'
]

{ #category : 'exporting' }
RwGsModificationTopazWriterVisitor >> exportMethodDefinitions: classDefinitionsInOrder labeled: label [
	| stream |
	stream := self bufferedStream.
	classDefinitionsInOrder do: [:classDef | 
		| className |
		className := classDef name.
		(classDef classMethodDefinitions isEmpty not or: [ classDef instanceMethodDefinitions isEmpty not])
			ifTrue: [ 
				stream nextPutAll: '! ', label, ' for ', className printString; lf;
				lf ].
		classDef classMethodDefinitions isEmpty not
			ifTrue: [ 
				stream nextPutAll: '!		Class methods for ', className printString; lf;
				lf ].
		(classDef classMethodDefinitions values sort: [:a :b | a selector <= b selector ])
			do: [:methodDef |
				methodDef selector == #initialize
					ifTrue: [ self classInitializationDefinitions add: classDef ].
				self _fileOutMethod: methodDef forClass: className isMeta: true on: stream ].
		classDef instanceMethodDefinitions isEmpty not
			ifTrue: [ 
				stream nextPutAll: '!		Instance methods for ', className printString; lf;
				lf ].
		(classDef instanceMethodDefinitions values sort: [:a :b | a selector <= b selector ])
			do: [:methodDef |
				self _fileOutMethod: methodDef forClass: className isMeta: false on: stream ] ].
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> filenameExtension [

	^ filenameExtension ifNil: ['gs' ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> filenameExtension: aString [

	filenameExtension := aString
]

{ #category : 'class writing' }
RwGsModificationTopazWriterVisitor >> processClass: aClassModification [

	| classDefinition symbolDictName clsName |
	classDefinition := aClassModification after.
	(self classDefinitions at: (clsName := classDefinition name) ifAbsent: []) ifNotNil: [ 
   self error: 'duplicate class definition for ', clsName printString, ' encountered.'].

	symbolDictName := self currentProjectDefinition symbolDictNameForPackageNamed: self currentPackageDefinition name.
	self classSymbolDictionaryNames at: classDefinition name put: symbolDictName.
	self classDefinitions at: classDefinition name put: classDefinition.

	self classDefPackageNameMap at: classDefinition name put: currentPackageDefinition name.

	"no need to visit any further as the class definition records the instance and class methods"
]

{ #category : 'class writing' }
RwGsModificationTopazWriterVisitor >> processClassExtension: aClassExtensionModification [

	| classExtension |
	classExtension := aClassExtensionModification after.
	(self classExtensions at: classExtension name ifAbsentPut: [ Set new ])
		add: classExtension.

	self classExtPackageNameMap at: classExtension put: currentPackageDefinition name.

	"no need to visit any further as the class etension records the instance and class methods"
]

{ #category : 'project writing' }
RwGsModificationTopazWriterVisitor >> processProject: aProjectModification [

	self buildPackageNamesMap 
		ifTrue: [
			| readTool |
			topazFilenamePackageNamesMap := self topazFilenamePackageNamesMap.
			readTool := Rowan projectTools read.
			self topazFilenameConfigsMap keysAndValuesDo: [:filename :configAndGroupNamesMap | 
				| configNames groupNames visitor packageNames configAndGroupNamesArray |
				configAndGroupNamesArray := configAndGroupNamesMap at: currentProjectDefinition name ifAbsent: [ {{}.{}} ].
				configAndGroupNamesArray
					ifNotNil: [
						configNames := configAndGroupNamesArray at: 1.
						groupNames := configAndGroupNamesArray at: 2.
						visitor := readTool 
							readConfigurationsForProjectDefinition: currentProjectDefinition 
							withConfigurations: configNames 
							groupNames: groupNames 
							forLoad: false.
						packageNames := visitor
							ifNil: [ 
								self topazFilenameConfigsMap size > 1
									ifTrue: [ self error: 'unable to read project configurations for project ', currentProjectDefinition name printString, '. Multiple output files likely to have the same contents.'].
								currentProjectDefinition packageNames ]
							ifNotNil: [  visitor packageNames ].
						(topazFilenamePackageNamesMap at: filename ifAbsentPut: [ Set new ]) addAll: packageNames ] ] ].
	aProjectModification packagesModification acceptVisitor: self
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> repositoryRootPath [

	^ repositoryRootPath
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> repositoryRootPath: aStringOrFileReference [ 

	repositoryRootPath := aStringOrFileReference asFileReference.
	repositoryRootPath exists ifFalse: [ self error: 'The repository root path must exist: ', repositoryRootPath pathString printString ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> topazFileHeader [

	^ topazFileHeader ifNil: [ '' ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> topazFileHeader: aString [

	topazFileHeader := aString
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> topazFilename [

	^ topazFilename
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> topazFilename: aString [

	topazFilename := aString
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> topazFilename: aNameString filenameExtension: anExtensionString [

	topazFilename := aNameString.
	filenameExtension := anExtensionString
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> topazFilenameConfigsMap [

	^ topazFilenameConfigsMap
		ifNil: [ topazFilenameConfigsMap := Dictionary new at: self topazFilename put: Dictionary new; yourself ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> topazFilenameConfigsMap: aDictionary [

	"keys are topaz file names, values are a dictionary:
		whose keys are project names and values are two slot array with config name list and group name list. 
			The config names and group names are resolved to a list of packages that will be written to the named file.

			If the config/group name list is empty, the default config and group names for the project will be used.
			If either (or both) of the slots contain empty array, then the corresponding project default will be used."

	"If topazFilenamePackageNamesMap is explicitly set, then the contents of topazFilenameConfigsMap will be ignored."

	buildPackageNamesMap := true.
	topazFilenameConfigsMap := aDictionary
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> topazFilenamePackageNamesMap [

	^ topazFilenamePackageNamesMap ifNil: [ topazFilenamePackageNamesMap := Dictionary new ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> topazFilenamePackageNamesMap: aDictionary [

	"keys are topaz file names, values are a collection of package names"

	"if topazFilenameConfigsMap is being used, then the topazFilenamePackageNamesMap is generated automatically,
		based on the project configurations."

	"If you explicitly set topazFilenamePackageNamesMap then contents of topazFilenameConfigsMap will be ignored."

	buildPackageNamesMap := false.
	topazFilenamePackageNamesMap := aDictionary
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> topazFileReference [

	^ self repositoryRootPath / self topazFilename, self filenameExtension
]
