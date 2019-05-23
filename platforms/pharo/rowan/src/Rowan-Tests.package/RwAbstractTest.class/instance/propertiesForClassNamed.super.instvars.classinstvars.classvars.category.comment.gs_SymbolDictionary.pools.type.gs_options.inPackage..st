support
propertiesForClassNamed: className super: superClassName instvars: instvars classinstvars: classinstvars classvars: classvars category: category comment: comment gs_SymbolDictionary: gs_SymbolDictionary pools: pools type: type gs_options: gs_options inPackage: packageName

	| dict |
	dict := Dictionary new.
	category
		ifNotNil: [ 
			dict
				at: 'category' put: category;
				yourself ].
	dict
		at: 'classinstvars' put: classinstvars;
		at: 'classvars' put: classvars;
		at: 'gs_SymbolDictionary' put: gs_SymbolDictionary;
		at: 'instvars' put: instvars;
		at: 'name' put: className;
		at: 'pools' put: pools;
		at: 'superclass' put: superClassName;
		at: 'type' put: type.
	comment ifNotNil: [ dict at: 'comment' put: comment ].
	(gs_options _isArray and: [ gs_options isEmpty not ])
		ifTrue: [ 
			dict
				at: 'gs_options'
				put:
					(gs_options collect: [ :each | each asString ]) asSortedCollection asArray ].
	^ dict