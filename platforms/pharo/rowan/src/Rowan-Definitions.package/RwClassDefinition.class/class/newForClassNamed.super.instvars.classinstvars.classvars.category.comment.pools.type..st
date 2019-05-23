instance creation
newForClassNamed: className super: superClassName instvars: instvars classinstvars: classinstvars classvars: classvars category: category comment: comment pools: pools type: type

	| propertiesDictionary |
	propertiesDictionary := Dictionary new.
	category
		ifNotNil: [ 
			propertiesDictionary
				at: 'category' put: category;
				yourself ].
	propertiesDictionary
		at: 'classinstvars' put: classinstvars;
		at: 'classvars' put: classvars;
		at: 'instvars' put: instvars;
		at: 'name' put: className;
		at: 'pools' put: pools;
		at: 'superclass' put: superClassName;
		at: 'type' put: type asString.
	comment ifNotNil: [ propertiesDictionary at: 'comment' put: comment ].
	^ self
		withProperties: propertiesDictionary
		classMethodDefinitions: Dictionary new
		instanceMethodDefinitions: Dictionary new