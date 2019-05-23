class browsing
classCreationTemplateForSubclassOf: superclassName className: className type: classType instanceVariablesString: instanceVariablesString classVariablesString: classVariablesString classInstanceVariablesString: classInstanceVariablesString poolDictionariesString: poolDictionariesString 
comment: comment category: category

	"classType may be 'normal', 'bytes', or 'variable'"

	| result lfsp |
	result := String new.
	lfsp := Character lf asString tab.
	comment isEmpty 
		ifFalse: [ 
			result 
				addAll: '(';
				addAll: lfsp ].
	result
		addAll: superclassName.
	classType = 'normal'
		ifTrue: [ result addAll: ' rwSubclass: ' ].
	classType = 'variable'
		ifTrue: [ result addAll: ' rwIndexableSubclass: ' ].
	classType = 'bytes'
		ifTrue: [ result addAll: ' rwByteSubclass: ' ].
	result addAll: className.
	classType ~= 'bytes'
		ifTrue: [ 
			result
				addAll: lfsp;
				addAll: 'instVarNames: #(' , instanceVariablesString , ')' ].
	result
		addAll: lfsp;
		addLast: 'classVars: #(' , classVariablesString , ')';
		addAll: lfsp;
		addLast: 'classInstVars: #(' , classInstanceVariablesString , ')';
		addAll: lfsp;
		addAll: 'poolDictionaries: #(' , poolDictionariesString , ')'.
	result
		addAll: lfsp;
		addAll: 'category: ';
		addAll: category.
	result
		add: lfsp;
		add: 'options: #()';
		add: Character lf.
	comment isEmpty 
		ifFalse: [ 
			result 
				addAll: ') ';
				addAll: lfsp;
				addAll: 'comment: ';
				addAll: comment;
				addAll: lfsp ].
	^ result