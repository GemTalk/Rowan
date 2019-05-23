class browsing
classCreationTemplateForSubclassOf: superclassName className: className type: classType category: category packageName: packageNameOrNil

	"classType may be 'normal', 'bytes', or 'variable'"

	| result lfsp superClass |
	result := String new.
	superClass := Rowan image objectNamed: superclassName.
	result addAll: superclassName.
	lfsp := Character lf asString tab.
	classType = 'normal'
		ifTrue: [ result addAll: ' rwSubclass: ' ].
	classType = 'variable'
		ifTrue: [ result addAll: ' rwIndexableSubclass: ' ].
	classType = 'bytes'
		ifTrue: [ result addAll: ' rwByteSubclass: ' ].
	result addAll: className printString.
	classType ~= 'bytes'
		ifTrue: [ 
			result
				addAll: lfsp;
				addAll: 'instVarNames: #()' ].
	result
		addAll: lfsp;
		addLast: 'classVars: #()';
		addAll: lfsp;
		addLast: 'classInstVars: #()';
		addAll: lfsp;
		addAll: 'poolDictionaries: #()'.
	result
		addAll: lfsp;
		addAll: 'category: ';
		addAll: category printString.
	packageNameOrNil
		ifNotNil: [ 
			result
				addAll: lfsp;
				addAll: 'packageName: '.
			result addAll: packageNameOrNil printString ].
	result
		add: lfsp;
		add: 'options: #()';
		add: Character lf.
	^ result
