Class {
	#name : 'RwAbstractTest',
	#superclass : 'TestCase',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwAbstractTest >> _classvars [

	^ #('ClassVar1')
]

{ #category : 'private' }
RwAbstractTest >> _symbolDictionaryName [

	^ #'CypressTestData'
]

{ #category : 'support' }
RwAbstractTest >> classDefinition: className super: superClassName category: category inPackage: packageName [

	^ Dictionary new
		at: className
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: className
								super: superClassName
								category: category
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself
]

{ #category : 'support' }
RwAbstractTest >> classDefinition: className super: superClassName classVars: classVars inPackage: packageName [

	^ Dictionary new
		at: className
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: className
								super: superClassName
								classVars: classVars
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself
]

{ #category : 'support' }
RwAbstractTest >> classDefinition: className super: superClassName comment: comment inPackage: packageName [

	^ Dictionary new
		at: className
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: className
								super: superClassName
								comment: comment
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself
]

{ #category : 'support' }
RwAbstractTest >> classDefinition: className super: superClassName inPackage: packageName [

	^ Dictionary new
		at: className
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: className
								super: superClassName
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself
]

{ #category : 'support' }
RwAbstractTest >> classDefinition: className super: superClassName instanceMethods: instanceMethods classMethods: classMethods inPackage: packageName [

	^ Dictionary new
		at: className
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: className
								super: superClassName
								inPackage: packageName)
						classMethodDefinitions: classMethods
						instanceMethodDefinitions: instanceMethods);
		yourself
]

{ #category : 'support' }
RwAbstractTest >> classDefinition: className super: superClassName instVars: instVars inPackage: packageName [

	^ Dictionary new
		at: className
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: className
								super: superClassName
								instVars: instVars
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself
]

{ #category : 'support' }
RwAbstractTest >> classExtensionDefinition: className instanceMethods: instanceMethods classMethods: classMethods [

	^ Dictionary new
		at: className
			put:
				(RwClassExtensionDefinition
						withProperties: (self propertiesForClassExtensionNamed: className)
						classMethodDefinitions: classMethods
						instanceMethodDefinitions: instanceMethods);
		yourself
]

{ #category : 'support' }
RwAbstractTest >> definitionsOfLoadedPackages: packageNames [

	^ Rowan loadedPackageDefinitionsNamed: packageNames
]

{ #category : 'support' }
RwAbstractTest >> methodDefsFromSpec: specArray [
  | dict |
  dict := Dictionary new.
  specArray
    do: [ :spec | 
      | selector protocol source |
      selector := (spec at: 1) asSymbol.
      protocol := (spec at: 2) asString.
      source := (spec at: 3) asString.
      dict
        at: selector
        put:
          (RwMethodDefinition
            withProperties:
              (Dictionary with: 'selector' -> selector with: 'protocol' -> protocol)
            source: source) ].
  ^ dict
]

{ #category : 'support' }
RwAbstractTest >> packageDefinition: packageName [

	^ RwPackageDefinition
		withProperties: (Dictionary with: 'name' -> packageName)
		classDefinitions: Dictionary new
		classExtensions: Dictionary new
]

{ #category : 'support' }
RwAbstractTest >> packageDefinition: packageName classNamed: className super: superClassName [

	^ RwPackageDefinition
		withProperties: (Dictionary with: 'name' -> packageName)
		classDefinitions:
			(self classDefinition: className super: superClassName inPackage: packageName)
		classExtensions: Dictionary new
]

{ #category : 'support' }
RwAbstractTest >> packageDefinition: packageName classNamed: className super: superClassName category: category [

	^ RwPackageDefinition
		withProperties: (Dictionary with: 'name' -> packageName)
		classDefinitions:
			(self
				classDefinition: className
				super: superClassName
				category: category
				inPackage: packageName)
		classExtensions: Dictionary new
]

{ #category : 'support' }
RwAbstractTest >> packageDefinition: packageName classNamed: className super: superClassName classVars: classVars [

	^ RwPackageDefinition
		withProperties: (Dictionary with: 'name' -> packageName)
		classDefinitions:
			(self
				classDefinition: className
				super: superClassName
				classVars: classVars
				inPackage: packageName)
		classExtensions: Dictionary new
]

{ #category : 'support' }
RwAbstractTest >> packageDefinition: packageName classNamed: className super: superClassName comment: comment [

	^ RwPackageDefinition
		withProperties: (Dictionary with: 'name' -> packageName)
		classDefinitions:
			(self
				classDefinition: className
				super: superClassName
				comment: comment
				inPackage: packageName)
		classExtensions: Dictionary new
]

{ #category : 'support' }
RwAbstractTest >> packageDefinition: packageName classNamed: className super: superClassName instanceMethods: instanceMethods classMethods: classMethods [

	^ RwPackageDefinition
		withProperties: (Dictionary with: 'name' -> packageName)
		classDefinitions:
			(self
				classDefinition: className
				super: superClassName
				instanceMethods: instanceMethods
				classMethods: classMethods
				inPackage: packageName)
		classExtensions: Dictionary new
]

{ #category : 'support' }
RwAbstractTest >> packageDefinition: packageName classNamed: className super: superClassName instVars: instVars [

	^ RwPackageDefinition
		withProperties: (Dictionary with: 'name' -> packageName)
		classDefinitions:
			(self
				classDefinition: className
				super: superClassName
				instVars: instVars
				inPackage: packageName)
		classExtensions: Dictionary new
]

{ #category : 'support' }
RwAbstractTest >> packageDefinition: packageName extendClassNamed: className instanceMethods: instanceMethods classMethods: classMethods [

	^ RwPackageDefinition
		withProperties: (Dictionary with: 'name' -> packageName)
		classDefinitions: Dictionary new
		classExtensions:
			(self
				classExtensionDefinition: className
				instanceMethods: instanceMethods
				classMethods: classMethods)
]

{ #category : 'support' }
RwAbstractTest >> packageSetDefinition: packageName [

	^ (RwPackageSetDefinition new addPackage: (self packageDefinition: packageName))
		asDefinition
]

{ #category : 'support' }
RwAbstractTest >> packageSetDefinition: packageName classNamed: className super: superClassName [

	^ (RwPackageSetDefinition new
		addPackage:
			(self packageDefinition: packageName classNamed: className super: superClassName))
		asDefinition
]

{ #category : 'support' }
RwAbstractTest >> packageSetDefinition: packageName classNamed: className super: superClassName category: category [

	^ (RwPackageSetDefinition new
		addPackage:
			(self
				packageDefinition: packageName
				classNamed: className
				super: superClassName
				category: category))
		asDefinition
]

{ #category : 'support' }
RwAbstractTest >> packageSetDefinition: packageName classNamed: className super: superClassName classVars: classVars [

	^ (RwPackageSetDefinition new
		addPackage:
			(self
				packageDefinition: packageName
				classNamed: className
				super: superClassName
				classVars: classVars))
		asDefinition
]

{ #category : 'support' }
RwAbstractTest >> packageSetDefinition: packageName classNamed: className super: superClassName comment: comment [

	^ (RwPackageSetDefinition new
		addPackage:
			(self
				packageDefinition: packageName
				classNamed: className
				super: superClassName
				comment: comment))
		asDefinition
]

{ #category : 'support' }
RwAbstractTest >> packageSetDefinition: packageName classNamed: className super: superClassName instanceMethods: instanceMethods classMethods: classMethods [

	^ (RwPackageSetDefinition new
		addPackage:
			(self
				packageDefinition: packageName
				classNamed: className
				super: superClassName
				instanceMethods: instanceMethods
				classMethods: classMethods))
		asDefinition
]

{ #category : 'support' }
RwAbstractTest >> packageSetDefinition: packageName classNamed: className super: superClassName instVars: instVars [

	^ (RwPackageSetDefinition new
		addPackage:
			(self
				packageDefinition: packageName
				classNamed: className
				super: superClassName
				instVars: instVars))
		asDefinition
]

{ #category : 'support' }
RwAbstractTest >> packageSetDefinition: packageName extendClassNamed: className instanceMethods: instanceMethods classMethods: classMethods [

	^ (RwPackageSetDefinition new
		addPackage:
			(self
				packageDefinition: packageName
				extendClassNamed: className
				instanceMethods: instanceMethods
				classMethods: classMethods))
		asDefinition
]

{ #category : 'support' }
RwAbstractTest >> propertiesForClassExtensionNamed: className [

	| dict |
	dict := Dictionary new.
	dict
		at: 'name' put: className;
		yourself.
	^ dict
]

{ #category : 'support' }
RwAbstractTest >> propertiesForClassNamed: className super: superClassName category: category inPackage: packageName [

	^ self
		propertiesForClassNamed: className
		super: superClassName
		instvars: {}
		classinstvars: {}
		classvars: {}
		category: category
		comment: nil
		gs_SymbolDictionary: self _symbolDictionaryName asString
		pools: {}
		type: 'normal'
		gs_options: {}
		inPackage: packageName
]

{ #category : 'support' }
RwAbstractTest >> propertiesForClassNamed: className super: superClassName classVars: classVars inPackage: packageName [

	^ self
		propertiesForClassNamed: className
		super: superClassName
		instvars: {}
		classinstvars: {}
		classvars: classVars
		category: nil
		comment: nil
		gs_SymbolDictionary: self _symbolDictionaryName asString
		pools: {}
		type: 'normal'
		gs_options: {}
		inPackage: packageName
]

{ #category : 'support' }
RwAbstractTest >> propertiesForClassNamed: className super: superClassName comment: comment inPackage: packageName [

	^ self
		propertiesForClassNamed: className
		super: superClassName
		instvars: {}
		classinstvars: {}
		classvars: {}
		category: nil
		comment: comment
		gs_SymbolDictionary: self _symbolDictionaryName asString
		pools: {}
		type: 'normal'
		gs_options: {}
		inPackage: packageName
]

{ #category : 'support' }
RwAbstractTest >> propertiesForClassNamed: className super: superClassName inPackage: packageName [

	^ self
		propertiesForClassNamed: className
		super: superClassName
		instvars: {}
		classinstvars: {}
		classvars: {}
		category: nil
		comment: nil
		gs_SymbolDictionary: self _symbolDictionaryName asString
		pools: {}
		type: 'normal'
		gs_options: {}
		inPackage: packageName
]

{ #category : 'support' }
RwAbstractTest >> propertiesForClassNamed: className super: superClassName instvars: instvars classinstvars: classinstvars classvars: classvars category: category comment: comment gs_SymbolDictionary: gs_SymbolDictionary pools: pools type: type gs_options: gs_options inPackage: packageName [

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
]

{ #category : 'support' }
RwAbstractTest >> propertiesForClassNamed: className super: superClassName instvars: instvars classinstvars: classinstvars classvars: classvars comment: comment gs_SymbolDictionary: gs_SymbolDictionary pools: pools type: type gs_options: gs_options inPackage: packageName [

	^ self
		propertiesForClassNamed: className
		super: superClassName
		instvars: instvars
		classinstvars: classinstvars
		classvars: classvars
		category: packageName
		comment: comment
		gs_SymbolDictionary: gs_SymbolDictionary
		pools: pools
		type: type
		gs_options: gs_options
		inPackage: packageName
]

{ #category : 'support' }
RwAbstractTest >> propertiesForClassNamed: className super: superClassName instVars: instVars inPackage: packageName [

	^ self
		propertiesForClassNamed: className
		super: superClassName
		instvars: instVars
		classinstvars: {}
		classvars: {}
		category: nil
		comment: nil
		gs_SymbolDictionary: self _symbolDictionaryName asString
		pools: {}
		type: 'normal'
		gs_options: {}
		inPackage: packageName
]

{ #category : 'running' }
RwAbstractTest >> setUp [

	"Remove the CypressTestData symbol dictionary, if present --- gets persistent and session symbol list"

	| user symList dict index |
	user := System myUserProfile.
	symList := user symbolList.

	dict := symList objectNamed: self _symbolDictionaryName.
	dict
		ifNotNil: [ 
			index := symList indexOf: dict.
			index > 0
				ifTrue: [ user removeDictionaryAt: index ] ]
]
