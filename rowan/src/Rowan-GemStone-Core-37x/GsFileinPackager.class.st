Class {
	#name : 'GsFileinPackager',
	#superclass : 'GsFileIn',
	#instVars : [
		'definedProject',
		'packageNameToComponentNameMap',
		'defaultComponentName',
		'packageDefinition',
		'packageCount',
		'onDoitBlock',
		'packageConvention'
	],
	#category : 'Rowan-GemStone-Core-37x'
}

{ #category : 'instance creation' }
GsFileinPackager class >> toPackage: packageName fromServerPath: aString packageConvention: packageConvention onDoitBlock: aZeroOneOrTwoArgBlockOrNil [
	| fileStream gsFilein |
	fileStream := FileStreamPortable read: aString type: #'serverText'.
	[ 
	(gsFilein := self newFromStream: fileStream)
		packageConvention: packageConvention;
		setSession: nil .
	aZeroOneOrTwoArgBlockOrNil
		ifNotNil: [ gsFilein onDoitBlock: aZeroOneOrTwoArgBlockOrNil ].
	gsFilein currentPackage: packageName.
	gsFilein doFileIn ]
		ensure: [ fileStream close ]
]

{ #category : 'instance creation' }
GsFileinPackager class >> toPackage: packageName packageConvention: packageConvention fromServerPath: aString [
	self toPackage: packageName fromServerPath: aString packageConvention: packageConvention onDoitBlock: nil
]

{ #category : 'instance creation' }
GsFileinPackager class >> toPackagesForDefinedProject: definedProject componentName: componentName fromServerPath: aString [
	self
		toPackagesForDefinedProject: definedProject
		packageNameToComponentNameMap: Dictionary new
		defaultComponentName: componentName
		fromServerPath: aString
		onDoitBlock: [ :chunk :fileinPackager | fileinPackager parseRBDoitChunkForDefinition: chunk ]
]

{ #category : 'instance creation' }
GsFileinPackager class >> toPackagesForDefinedProject: definedProject componentName: componentName fromStream: aStream [
	self
		toPackagesForDefinedProject: definedProject
		packageNameToComponentNameMap: Dictionary new
		defaultComponentName: componentName
		fromStream: aStream
		onDoitBlock: [ :chunk :fileinPackager | fileinPackager parseRBDoitChunkForDefinition: chunk ]
]

{ #category : 'instance creation' }
GsFileinPackager class >> toPackagesForDefinedProject: definedProject fromServerPath: aString [
	self
		toPackagesForDefinedProject: definedProject
		componentName: 'Core'
		fromServerPath: aString
]

{ #category : 'instance creation' }
GsFileinPackager class >> toPackagesForDefinedProject: definedProject fromStream: aStream [
	self
		toPackagesForDefinedProject: definedProject
		componentName: 'Core'
		fromStream: aStream
]

{ #category : 'instance creation' }
GsFileinPackager class >> toPackagesForDefinedProject: definedProject packageNameToComponentNameMap: packageNameToComponentNameMap defaultComponentName: defaultComponentName fromServerPath: aString onDoitBlock: aZeroOneOrTwoArgBlockOrNil [
	| fileStream |
	fileStream := FileStreamPortable read: aString type: #'serverText'.
	[ 
	self
		toPackagesForDefinedProject: definedProject
		packageNameToComponentNameMap: packageNameToComponentNameMap
		defaultComponentName: defaultComponentName
		fromStream: fileStream
		onDoitBlock: aZeroOneOrTwoArgBlockOrNil ]
		ensure: [ fileStream close ]
]

{ #category : 'instance creation' }
GsFileinPackager class >> toPackagesForDefinedProject: definedProject packageNameToComponentNameMap: packageNameToComponentNameMap defaultComponentName: defaultComponentName fromStream: aStream onDoitBlock: aZeroOneOrTwoArgBlockOrNil [
	|  gsFilein |
	(gsFilein := self newFromStream: aStream)
		setSession: nil;
		setEnableRemoveAll: false.
	aZeroOneOrTwoArgBlockOrNil
		ifNotNil: [ gsFilein onDoitBlock: aZeroOneOrTwoArgBlockOrNil ].
	gsFilein 
		definedProject: definedProject;
		defaultComponentName: defaultComponentName;
		packageNameToComponentNameMap: packageNameToComponentNameMap .
	gsFilein doFileIn
]

{ #category : 'private' }
GsFileinPackager >> _correctClassCategoryFor: classDef [
	(self packageConvention = 'RowanHybrid'
		or: [ self packageConvention = 'Monticello' ])
		ifTrue: [ classDef category: self packageDefinition name ]
		ifFalse: [ 
			self
				error:
					'Unexpected invalid class category for package convention '
						, self packageConvention printString ]
]

{ #category : 'private' }
GsFileinPackager >> _correctMethodProtocolFor: methodDef [
	(self packageConvention = 'RowanHybrid'
		or: [ self packageConvention = 'Monticello' ])
		ifTrue: [ methodDef protocol: '*' , self packageDefinition name asLowercase ]
		ifFalse: [ 
			self
				error:
					'Unexpected invalid method protocol for package convention '
						, self packageConvention printString ]
]

{ #category : 'private' }
GsFileinPackager >> _setClass: aString [
	"GsFileinPackager does not use currentClassObj, since we're building definitions not classes"

	self _setCurrentClassName: aString
]

{ #category : 'processing' }
GsFileinPackager >> abort [
	^ GsSession isSolo
		ifFalse: [ super abort ]
]

{ #category : 'processing' }
GsFileinPackager >> abortTransaction [
	^ GsSession isSolo
		ifFalse: [ super abortTransaction ]
]

{ #category : 'processing' }
GsFileinPackager >> classMethod [
  | className |
  super parseClassmethodLine .
	(className := self currentClass) ifNil: [ self error: 'current class not defined' ].
	self packageDefinition
		ifNotNil: [ :packageDef | 
			| classDef methodDef |
			methodDef := ((classDef := packageDef
				classDefinitionNamed: className
				ifAbsent: [  ])
				ifNil: [ 
					packageDef
						classExtensionDefinitionNamed: className
						ifAbsent: [ packageDef addClassExtensionNamed: className ] ])
				addClassMethod: self nextChunk
				protocol: category.
			[ 
			RwAbstractReaderWriterVisitor
				validatePackageConvention: self packageConvention
				forClassDefinition: classDef
				forMethodDefinitionProtocol: methodDef
				className: className
				isMeta: true
				forPackageNamed: packageDef name ]
				on: RwInvalidMethodProtocolConventionErrorNotification
				do: [ :ex | 
					"opportunity to automatically correct method protocol"
					self _correctMethodProtocolFor: methodDef ] ]
		ifNil: [ super classMethodBody ]
]

{ #category : 'processing' }
GsFileinPackager >> commit [
	^ GsSession isSolo
		ifFalse: [ super commit ]
]

{ #category : 'processing' }
GsFileinPackager >> commitTransaction [
	^ GsSession isSolo
		ifFalse: [ super commitTransaction ]
]

{ #category : 'class definition creation' }
GsFileinPackager >> createClassDefinitionFromCommentCategoryImmediateInvariantCascadeNode: cascadeNode [
	| messages classCreationMessageNode args cat comment superclassName classDef |
	self packageDefinition ifNil: [ self error: 'current package not defined' ].
	messages := cascadeNode messages.
	classCreationMessageNode := messages first receiver.
	args := classCreationMessageNode arguments.
	superclassName := classCreationMessageNode receiver token value.

	(messages detect: [ :each | each selector == #'category:' ] ifNone: [  ])
		ifNotNil: [ :messageNode | cat := messageNode arguments first token value ].
	(messages detect: [ :each | each selector == #'comment:' ] ifNone: [  ])
		ifNotNil: [ :messageNode | comment := messageNode arguments first token value ].

	(classCreationMessageNode selector
		==
			#'subclass:instVarNames:classVars:classInstVars:poolDictionaries:inDictionary:options:'
		or: [ 
			classCreationMessageNode selector
				==
					#'_newKernelSubclass:instVarNames:classVars:classInstVars:poolDictionaries:inDictionary:options:reservedOop:'
				or: [ 
					classCreationMessageNode selector
						==
							#'_newKernelIndexableSubclass:instVarNames:classVars:classInstVars:poolDictionaries:inDictionary:options:reservedOop:'
						or: [ 
							classCreationMessageNode selector
								==
									#'indexableSubclass:instVarNames:classVars:classInstVars:poolDictionaries:inDictionary:options:' ] ] ])
		ifTrue: [ 
			| type |
			type := 'normal'.
			(classCreationMessageNode selector
				==
					#'_newKernelIndexableSubclass:instVarNames:classVars:classInstVars:poolDictionaries:inDictionary:options:reservedOop:'
				or: [ 
					classCreationMessageNode selector
						==
							#'indexableSubclass:instVarNames:classVars:classInstVars:poolDictionaries:inDictionary:options:' ])
				ifTrue: [ type := 'variable' ].
			classDef := self packageDefinition
				addClassNamed: (args at: 1) token value
				super: superclassName
				instvars: (args at: 2) value
				classinstvars: (args at: 4) value
				classvars: (args at: 3) value
				category: cat
				comment: comment
				pools: (args at: 5) value
				type: type.
			classDef gs_options: (args at: 7) value.
			classCreationMessageNode selector
				==
					#'_newKernelSubclass:instVarNames:classVars:classInstVars:poolDictionaries:inDictionary:options:reservedOop:'
				ifTrue: [ classDef gs_reservedOop: (args at: 8) token value asString ] ]
		ifFalse: [ 
			(classCreationMessageNode selector
				== #'byteSubclass:classVars:poolDictionaries:inDictionary:options:'
				or: [ 
					classCreationMessageNode selector
						==
							#'_newKernelByteSubclass:classVars:poolDictionaries:inDictionary:options:reservedOop:' ])
				ifTrue: [ 
					| type |
					type := 'byteSubclass'.
					classDef := self packageDefinition
						addClassNamed: (args at: 1) token value
						super: superclassName
						instvars: #()
						classinstvars: #()
						classvars: (args at: 2) value
						category: cat
						comment: comment
						pools: (args at: 3) value
						type: type.
					classDef gs_options: (args at: 5) value.
					classCreationMessageNode selector
						==
							#'_newKernelByteSubclass:classVars:poolDictionaries:inDictionary:options:reservedOop:'
						ifTrue: [ classDef gs_reservedOop: (args at: 6) token value asString ] ] ].
	[ 
	RwAbstractReaderWriterVisitor
		validatePackageConvention: self packageConvention
		forClassCategory: classDef
		inPackageNamed: self packageDefinition name ]
		on: RwInvalidClassCategoryConventionErrorNotification
		do: [ :ex | 
			"opportunity to automatically correct class category"
			self _correctClassCategoryFor: classDef ]
]

{ #category : 'class definition creation' }
GsFileinPackager >> createClassDefinitionFromCommentCategoryImmediateInvariantGsComCascadeNode: cascadeNode [
	| messages classCreationMessageNode args cat comment superclassName classDef |
	self packageDefinition ifNil: [ self error: 'current package not defined' ].
	messages := cascadeNode messages.
	classCreationMessageNode := cascadeNode receiver.
	args := classCreationMessageNode arguments.
	superclassName := classCreationMessageNode receiver leaf litValue asString.

	(messages detect: [ :each | each selector == #'category:' ] ifNone: [  ])
		ifNotNil: [ :messageNode | cat := messageNode arguments first leaf litValue ].
	(messages detect: [ :each | each selector == #'comment:' ] ifNone: [  ])
		ifNotNil: [ :messageNode | comment := messageNode arguments first leaf litValue ].

	(classCreationMessageNode selector
		==
			#'subclass:instVarNames:classVars:classInstVars:poolDictionaries:inDictionary:options:'
		or: [ 
			classCreationMessageNode selector
				==
					#'_newKernelSubclass:instVarNames:classVars:classInstVars:poolDictionaries:inDictionary:options:reservedOop:'
				or: [ 
					classCreationMessageNode selector
						==
							#'_newKernelIndexableSubclass:instVarNames:classVars:classInstVars:poolDictionaries:inDictionary:options:reservedOop:'
						or: [ 
							classCreationMessageNode selector
								==
									#'indexableSubclass:instVarNames:classVars:classInstVars:poolDictionaries:inDictionary:options:' ] ] ])
		ifTrue: [ 
			| type |
			type := 'normal'.
			(classCreationMessageNode selector
				==
					#'_newKernelIndexableSubclass:instVarNames:classVars:classInstVars:poolDictionaries:inDictionary:options:reservedOop:'
				or: [ 
					classCreationMessageNode selector
						==
							#'indexableSubclass:instVarNames:classVars:classInstVars:poolDictionaries:inDictionary:options:' ])
				ifTrue: [ type := 'variable' ].
			classDef := self packageDefinition
				addClassNamed: (args at: 1) leaf litValue
				super: superclassName
				instvars: (args at: 2) leaf litValue
				classinstvars: (args at: 4) leaf litValue
				classvars: (args at: 3) leaf litValue
				category: cat
				comment: comment
				pools: (args at: 5) leaf litValue
				type: type.
			classDef gs_options: (args at: 7) leaf litValue.
			classCreationMessageNode selector
				==
					#'_newKernelSubclass:instVarNames:classVars:classInstVars:poolDictionaries:inDictionary:options:reservedOop:'
				ifTrue: [ classDef gs_reservedOop: (args at: 8) leaf litValue asString ] ]
		ifFalse: [ 
			(classCreationMessageNode selector
				== #'byteSubclass:classVars:poolDictionaries:inDictionary:options:'
				or: [ 
					classCreationMessageNode selector
						==
							#'_newKernelByteSubclass:classVars:poolDictionaries:inDictionary:options:reservedOop:' ])
				ifTrue: [ 
					| type |
					type := 'byteSubclass'.
					classDef := self packageDefinition
						addClassNamed: (args at: 1) leaf litValue
						super: superclassName
						instvars: #()
						classinstvars: #()
						classvars: (args at: 2) leaf litValue
						category: cat
						comment: comment
						pools: (args at: 3) leaf litValue
						type: type.
					classDef gs_options: (args at: 5) leaf litValue.
					classCreationMessageNode selector
						==
							#'_newKernelByteSubclass:classVars:poolDictionaries:inDictionary:options:reservedOop:'
						ifTrue: [ classDef gs_reservedOop: (args at: 6) leaf litValue asString ] ] ].
	[ 
	RwAbstractReaderWriterVisitor
		validatePackageConvention: self packageConvention
		forClassCategory: classDef
		inPackageNamed: self packageDefinition name ]
		on: RwInvalidClassCategoryConventionErrorNotification
		do: [ :ex | 
			"opportunity to automatically correct class category"
			self _correctClassCategoryFor: classDef ]
]

{ #category : 'private' }
GsFileinPackager >> createGsComMethodForWorkspace: chunk [
	| lastNonSeparatorCharacter |
	lastNonSeparatorCharacter := nil.
	chunk size to: 1 do: [ :index | 
		lastNonSeparatorCharacter
			ifNil: [ 
				| ch |
				(ch := chunk at: index) isSeparator
					ifFalse: [ lastNonSeparatorCharacter := ch ] ] ].
	^ lastNonSeparatorCharacter = $.
		ifTrue: [ ^ 'xxx ' , chunk , ' ^ true ' ]
		ifFalse: [ ^ 'xxx ' , chunk , '. ^ true ' ]
]

{ #category : 'processing' }
GsFileinPackager >> currentPackage: aPackageName [
	(self definedProject notNil and: [ aPackageName notNil ])
		ifTrue: [ 
			| componentName packageDef |
			self packageCount: self packageCount + 1.
			self packageCount > 1
				ifTrue: [ 
					self
						error:
							'Only one CURRENTPACKAGE: command allowed when creating package definitions' ].
			componentName := self packageNameToComponentNameMap
				at: aPackageName
				ifAbsent: [ self defaultComponentName ].
			packageDef := self definedProject
				packageNamed: aPackageName
				ifAbsent: [ 
					self definedProject
						addPackageNamed: aPackageName
						toComponentNamed: componentName ].
			self packageDefinition: packageDef ]
		ifFalse: [ Rowan gemstoneTools topaz currentTopazPackageName: aPackageName ]
]

{ #category : 'accessing' }
GsFileinPackager >> defaultComponentName [
	^ defaultComponentName ifNil: [ 'Core' ]
]

{ #category : 'accessing' }
GsFileinPackager >> defaultComponentName: object [
	defaultComponentName := object
]

{ #category : 'accessing' }
GsFileinPackager >> definedProject [
	^definedProject
]

{ #category : 'accessing' }
GsFileinPackager >> definedProject: object [
	definedProject := object
]

{ #category : 'processing' }
GsFileinPackager >> doit [
	self onDoitBlock cull: self nextChunk cull: self
]

{ #category : 'processing' }
GsFileinPackager >> ignoreList [
	^ super ignoreList , #('IFERROR' 'DEFINE' 'SEND' 'OBJ')
]

{ #category : 'testing' }
GsFileinPackager >> isSupportedClassCreationSelector: sel [
	^ #(#'subclass:instVarNames:classVars:classInstVars:poolDictionaries:inDictionary:options:' #'_newKernelSubclass:instVarNames:classVars:classInstVars:poolDictionaries:inDictionary:options:reservedOop:' #'_newKernelIndexableSubclass:instVarNames:classVars:classInstVars:poolDictionaries:inDictionary:options:reservedOop:' #'indexableSubclass:instVarNames:classVars:classInstVars:poolDictionaries:inDictionary:options:' #'byteSubclass:classVars:poolDictionaries:inDictionary:options:' #'_newKernelByteSubclass:classVars:poolDictionaries:inDictionary:options:reservedOop:')
		includes: sel
]

{ #category : 'processing' }
GsFileinPackager >> method [
  | className |
  super parseMethodLine .
	(className := self currentClass) ifNil: [ self error: 'current class not defined' ].
	self packageDefinition
		ifNotNil: [ :packageDef | 
			| classDef methodDef |
			methodDef := ((classDef := packageDef
				classDefinitionNamed: className
				ifAbsent: [  ])
				ifNil: [ 
					packageDef
						classExtensionDefinitionNamed: className
						ifAbsent: [ packageDef addClassExtensionNamed: className ] ])
				addInstanceMethod: self nextChunk
				protocol: category.
			[ 
			RwAbstractReaderWriterVisitor
				validatePackageConvention: self packageConvention
				forClassDefinition: classDef
				forMethodDefinitionProtocol: methodDef
				className: className
				isMeta: false
				forPackageNamed: packageDef name ]
				on: RwInvalidMethodProtocolConventionErrorNotification
				do: [ :ex | 
					"opportunity to automatically correct method protocol"
					self _correctMethodProtocolFor: methodDef ] ]
		ifNil: [ super methodBody ]
]

{ #category : 'accessing' }
GsFileinPackager >> onDoitBlock [
	"give user ability to track doits in .gs file ... by default execute the doitSrc"

	^ onDoitBlock ifNil: [ ^ [ :doitSrc | self execute: doitSrc ] ]
]

{ #category : 'accessing' }
GsFileinPackager >> onDoitBlock: aZeroOneOrTwoArgBlock [
	onDoitBlock := aZeroOneOrTwoArgBlock
]

{ #category : 'accessing' }
GsFileinPackager >> packageConvention [
	^ packageConvention
		ifNil: [ packageConvention := self definedProject packageConvention ]
]

{ #category : 'accessing' }
GsFileinPackager >> packageConvention: object [
	packageConvention := object
]

{ #category : 'accessing' }
GsFileinPackager >> packageCount [
	^ packageCount ifNil: [ packageCount := 0 ]
]

{ #category : 'accessing' }
GsFileinPackager >> packageCount: object [
	packageCount := object
]

{ #category : 'accessing' }
GsFileinPackager >> packageDefinition [
	^packageDefinition
]

{ #category : 'accessing' }
GsFileinPackager >> packageDefinition: aPackageDefinition [
	packageDefinition := aPackageDefinition
]

{ #category : 'accessing' }
GsFileinPackager >> packageNameToComponentNameMap [
	^packageNameToComponentNameMap
]

{ #category : 'accessing' }
GsFileinPackager >> packageNameToComponentNameMap: object [
	packageNameToComponentNameMap := object
]

{ #category : 'doit parser' }
GsFileinPackager >> parseRBDoitChunkForDefinition: chunk [
	"The doit chunk produced by topaz fileout (via Rowan) is:
		(<class-creation-message>)
			category: #categoryOrNil;
			comment: 'xxx';
			immediateInvariant.
	Will attempt to recognize as many standard class creation patterns as is required ..."

	| workspaceNode |
	workspaceNode := RBParser parseWorkspace: chunk.
	workspaceNode body
		do: [ :sequenceNode | 
			sequenceNode isSequence
				ifTrue: [ 
					sequenceNode statements
						do: [ :cascadeNode | 
							cascadeNode isCascade
								ifTrue: [ 
									| messages cascadeMessagePattern |
									"start matching class creation cascade message patterns"
									messages := cascadeNode messages.
									(messages size = 3
										and: [ 
											(cascadeMessagePattern := (messages collect: [ :message | message selector ])
												asArray sort)
												= #(#'comment:' #'category:' #'immediateInvariant') sort ])
										ifTrue: [ 
											| classCreationSelector |
											classCreationSelector := messages first receiver selector.
											(self isSupportedClassCreationSelector: classCreationSelector)
												ifTrue: [ 
													^ self
														createClassDefinitionFromCommentCategoryImmediateInvariantCascadeNode:
															cascadeNode ]
												ifFalse: [ 
													self
														error:
															'Unrecognized class creation selector: ' , classCreationSelector printString ] ]
										ifFalse: [ 
											((messages size = 3
												and: [ 
													(cascadeMessagePattern := (messages collect: [ :message | message selector ])
														asArray sort)
														= #(#'comment:' #'category:' #'immediateInvariant') sort ])
												or: [ 
													messages size = 2
														and: [ 
															(cascadeMessagePattern := (messages collect: [ :message | message selector ])
																asArray sort) = #(#'category:' #'immediateInvariant') sort ] ])
												ifTrue: [ 
													| classCreationSelector |
													classCreationSelector := messages first receiver selector.
													(self isSupportedClassCreationSelector: classCreationSelector)
														ifTrue: [ 
															^ self
																createClassDefinitionFromCommentCategoryImmediateInvariantCascadeNode:
																	cascadeNode ]
														ifFalse: [ 
															self
																error:
																	'Unrecognized class creation selector: ' , classCreationSelector printString ] ]
												ifFalse: [ 
													self
														error:
															'Unrecognized class creation message pattern: '
																, cascadeMessagePattern printString ] ] ]
								ifFalse: [ 
									((cascadeNode isMessage and: [ cascadeNode selector == #'initialize' ])
										or: [ cascadeNode isLiteralNode and: [ cascadeNode value == true ] ])
										ifTrue: [ 
											"initialize message sends and literal true in doits should be ignored"
											 ]
										ifFalse: [ self unexpectedNode: cascadeNode expectedNode: 'RBCascadeNode' ] ] ] ]
				ifFalse: [ self unexpectedNode: sequenceNode expectedNode: 'RBSequenceNode' ] ]
]

{ #category : 'errors' }
GsFileinPackager >> unexpectedNode: node expectedNode: expectedNode [
	self
		error:
			'Unrecognized class creation pattern. Expected a ' , expectedNode , ' not '
				, node class name asString , '.'
]
