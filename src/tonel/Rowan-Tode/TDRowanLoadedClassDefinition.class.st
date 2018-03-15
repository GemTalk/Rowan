Class {
	#name : 'TDRowanLoadedClassDefinition',
	#superclass : 'TDClassDefinition',
	#instVars : [
		'loadedClass',
		'loadedClassExtensions'
	],
	#category : 'Rowan-Tode'
}

{ #category : 'initialization' }
TDRowanLoadedClassDefinition class >> initialize [

	"self initialize"

	TDTopezServer
		registerEditorClass: TDRowanClassHierarchyListElementBuilder
			for: self
			aspect: #'rowanHierarchyList';
		registerEditorClass: TDRowanClassDefinitionClientElementBuilder
			for: self
			aspect: #'rowanClassDefinition';
		registerEditorClass: TDRowanClassProtocolListElementBuilder
			for: self
			aspect: #'rowanProtocolList';
		registerEditorClass: TDRowanClassProtocolListElementBuilder
			for: TDRowanBehaviorDefinition
			aspect: #'rowanProtocolList';
		registerEditorClass: TDRowanClassSelectorListElementBuilder
			for: TDProtocolDefinition
			aspect: #'rowanProtocolSelectors';
		registerEditorClass: TDRowanMethodDefinitionElementBuilder
			for: TDRowanLoadedMethodDefinition
			aspect: #'method';
		yourself
]

{ #category : 'accessing' }
TDRowanLoadedClassDefinition >> addLoadedClassExtension: anObject [

	self loadedClassExtensions add: anObject
]

{ #category : 'converting' }
TDRowanLoadedClassDefinition >> asTDProtocolDefinition: protocol [

	^ (TDRowanProtocolDefinition
		className: self className
		isMeta: self isMeta
		protocol: protocol)
		appEnv: self appEnv;
		theClassDefinition: self;
		yourself
]

{ #category : 'instance/class button bar support' }
TDRowanLoadedClassDefinition >> browseClassProtocol: buttonBarClientElement [

	((TDRowanBehaviorDefinition className: self className isMeta: true)
		appEnv: self appEnv;
		theClassDefinition: self;
		yourself)
		editUsing:
			((TDEditorSpec
				topez: buttonBarClientElement topez
				editorAspect: #'rowanProtocolList')
				windowName: #'classicProtocolList';
				windowLocation: #'classicProtocol';
				selectionIndex: 1;
				yourself).
	^ true
]

{ #category : 'instance/class button bar support' }
TDRowanLoadedClassDefinition >> browseInstanceProtocol: buttonBarClientElement [

	self
		editUsing:
			((TDEditorSpec
				topez: buttonBarClientElement topez
				editorAspect: #'rowanProtocolList')
				windowName: #'classicProtocolList';
				windowLocation: #'classicProtocol';
				selectionIndex: 1;
				yourself).
	^ true
]

{ #category : 'class definition' }
TDRowanLoadedClassDefinition >> classDefinitionFor: aClass inContext: aUserProfile superClassName: superClassName [

	"Returns a description of the receiver using object names taken from the given
 UserProfile. Crafted for OG-Standard browsers"

	| result anArray lfsp newByteSubclass firstElement poolDicts civs |
	result := String new.
	lfsp := Character lf asString
		addAll: String tab;
		yourself.
	loadedClass
		ifNotNil: [ 
			result
				addAll:
						'"' , loadedClass loadedProject name , ':' , loadedClass loadedPackage name
								, '"';
				addAll: Character lf asString ].
	result addAll: superClassName.
	newByteSubclass := false.
	(aClass isBytes _and: [ aClass superClass isBytes not ])
		ifTrue: [ 
			result
				addAll: ' byteSubclass: ''';
				addAll: className;
				addLast: $'.
			newByteSubclass := true ]
		ifFalse: [ 
			aClass _portableIsIndexable
				ifTrue: [ 
					result
						addAll: ' indexableSubclass: ''';
						addAll: className;
						addLast: $' ]
				ifFalse: [ 
					result
						addAll: ' subclass: ''';
						addAll: className;
						addLast: $' ] ].
	newByteSubclass
		ifFalse: [ 
			result
				addAll: lfsp;
				addAll: 'instVarNames: #(';
				addAll: (aClass _instVarNamesWithSeparator: lfsp , '                 ');
				add: $) ].
	result
		addAll: lfsp;
		addLast: 'classVars: #('.
	aClass _sortedClassVarNames
		do: [ :aKey | 
			result addLast: $ .
			(aKey includesValue: $')
				ifTrue: [ result addAll: aKey _asSource ]
				ifFalse: [ result addAll: aKey ] ].
	result addLast: $).
	result
		addAll: lfsp;
		addLast: 'classInstVars: #('.
	civs := aClass class allInstVarNames.
	civs removeFrom: 1 to: aClass class superClass instSize.
	civs
		do: [ :civName | 
			result addLast: $ .
			(civName includesValue: $')
				ifTrue: [ result addAll: civName _asSource ]
				ifFalse: [ result addAll: civName ] ].
	result addLast: $).
	result
		addAll: lfsp;
		addAll: 'poolDictionaries: '.
	(poolDicts := aClass sharedPools) size > 0
		ifTrue: [ 
			result addAll: ' {'.
			firstElement := true.
			poolDicts
				do: [ :each | 
					firstElement
						ifFalse: [ result addAll: ' . ' ].
					(anArray := aUserProfile dictionaryAndSymbolOf: each)
						ifNil: [ 
							| d |
							(d := aUserProfile objectNamed: each name)
								ifNotNil: [ 
									((d isKindOf: Class) and: [ d isSubclassOf: SharedPool ])
										ifTrue: [ result addAll: each name , ' _classVars' ]
										ifFalse: [ result addAll: '"(not named)"' ] ]
								ifNil: [ result addAll: '"(not named)"' ] ]
						ifNotNil: [ result addAll: (anArray at: 2) ].
					firstElement := false ].
			result addAll: ' }' ]
		ifFalse: [ result addAll: '#()' ].
	result
		addAll: lfsp;
		addAll: 'inDictionary: '.
	anArray := System myUserProfile dictionaryAndSymbolOf: aClass.
	(anArray == nil
		or: [ (anArray at: 1) == GsPackagePolicy current homeSymbolDict ])
		ifTrue: [ result addAll: '''''' ]
		ifFalse: [ 
			(anArray := System myUserProfile dictionaryAndSymbolOf: (anArray at: 1))
				ifNil: [ result addAll: '''''' ]
				ifNotNil: [ result addAll: (anArray at: 2) asString printString ] ].
	result
		addAll: lfsp;
		addAll: 'category: '.
	result addAll: aClass category printString.
	result
		add: lfsp;
		add: aClass _optionsForDefinition.
	result add: Character lf.
	^ result
]

{ #category : 'accessing' }
TDRowanLoadedClassDefinition >> emphasis [

	"use emphasis to indicate Rowan project/package membership:
		bold				- class is defined in the specified project
		normal				- class is not defined the specified project and contains no extension methods
							  defined in the project
		italic/underline	- class is not defined in the specified project, but does contain
							  extension methods defined in the project
"

	loadedClass
		ifNil: [ 
			loadedClassExtensions isEmpty
				ifTrue: [ ^ {(TextEmphasis normal)} ].
			^ {(TextEmphasis italic).
			(TextEmphasis underlined)} ].
	^ super emphasis
]

{ #category : 'tools' }
TDRowanLoadedClassDefinition >> itemSelected: topez [

	(topez toolInstanceFor: 'browse')
		browseRowanClassHierarchy: self asTDClassDefinition
		label: 'Hierarchy of ' , self label.
	^ nil
]

{ #category : 'accessing' }
TDRowanLoadedClassDefinition >> loadedClass [

   ^loadedClass

]

{ #category : 'accessing' }
TDRowanLoadedClassDefinition >> loadedClass: anObject [

   loadedClass := anObject

]

{ #category : 'accessing' }
TDRowanLoadedClassDefinition >> loadedClassExtensions [

	^ loadedClassExtensions ifNil: [ loadedClassExtensions := IdentitySet new ]
]

{ #category : 'as yet unclassified' }
TDRowanLoadedClassDefinition >> methodDefinitionFor: selector isMeta: isMeta [

	^ (TDRowanLoadedMethodDefinition
		className: self className
		isMeta: isMeta
		selector: selector)
		appEnv: self appEnv;
		theClassDefinition: self;
		yourself
]
