class browsing
classCreationTemplateForClass: aClass hybridBrowser: hybridBrowser

	| result anArray lfsp newByteSubclass civs superClass className thePackageName nonRowanClass |
	result := String new.
	superClass := aClass superclass.
	className := aClass name asString.
	superClass
		ifNil: [ result addAll: 'nil' ]
		ifNotNil: [ result addAll: superClass name asString ].
	lfsp := Character lf asString tab.
	newByteSubclass := false.
	thePackageName := aClass rowanPackageName.
	nonRowanClass := thePackageName = Rowan unpackagedName.
	(aClass isBytes _and: [ superClass isBytes not ])
		ifTrue: [ 
			nonRowanClass
				ifTrue: [ result addAll: ' byteSubclass: ''' ]
				ifFalse: [ result addAll: ' rwByteSubclass: ''' ].
			result
				addAll: className;
				addLast: $'.
			newByteSubclass := true ]
		ifFalse: [ 
			(aClass isIndexable and: [superClass isIndexable not])
				ifTrue: [ 
					nonRowanClass
						ifTrue: [ result addAll: ' indexableSubclass: ''' ]
						ifFalse: [ result addAll: ' rwIndexableSubclass: ''' ].
					result
						addAll: className;
						addLast: $' ]
				ifFalse: [ 
					nonRowanClass
						ifTrue: [ result addAll: ' subclass: ''' ]
						ifFalse: [ result addAll: ' rwSubclass: ''' ].
					result
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
	result addAll: '#()'.	"ignored for now"
	nonRowanClass
		ifTrue: [ 
			"if the class is unpackaged, then we need to provide for the specification of symbol dictionary into which the class will be installed"
			result
				addAll: lfsp;
				addAll: 'inDictionary: '.
			anArray := Rowan image symbolList dictionariesAndSymbolsOf: aClass.
			anArray isEmpty
				ifTrue: [ result addAll: '''''' ]
				ifFalse: [ result addAll: ((anArray at: 1) at: 1) name asString ] ]
		ifFalse: [ 
			result
				addAll: lfsp;
				addAll: 'category: '.
			result addAll: aClass category printString.
			(hybridBrowser and: [ thePackageName = aClass category ])
				ifFalse: [ 
					result
						addAll: lfsp;
						addAll: 'packageName: '.
					result addAll: thePackageName printString ] ].
	aClass _hasConstraints
		ifTrue: [ 
			result
				add: lfsp;
				add: aClass _rwDefinitionOfConstraints ].
	result
		add: lfsp;
		add: aClass _rwOptionsForDefinition.
	result add: Character lf.
	^ result
