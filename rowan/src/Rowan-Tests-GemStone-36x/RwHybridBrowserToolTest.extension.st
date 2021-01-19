Extension { #name : 'RwHybridBrowserToolTest' }

{ #category : '*rowan-tests-gemstone-36x' }
RwHybridBrowserToolTest >> testHybridCompileMethodRemove_315 [

	"https://github.com/dalehenrich/Rowan/issues/315"

	| normalClass projectName packageNames packageName1 packageName2 fooMethod barMethod 
		normalInstance symDict registry project audit  |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageName2 := 'HybridA-Extensions'.
	packageNames := {packageName1.
	packageName2}.
	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.

	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.

	normalClass := Object
		rwSubclass: 'SimpleHybridNormal3'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: normalClass rowanPackageName = packageName1.

"add methods"
	fooMethod := normalClass rwCompileMethod: 'foo ^''foo''' category: 'accessing'.
	barMethod := normalClass class
		rwCompileMethod: 'bar ^''bar'''
		category: '*' , packageName2 asLowercase.

"validate"
	self assert: (normalClass perform: #bar) = 'bar'.
	normalInstance := normalClass new.
	self assert: (normalInstance perform: #foo) = 'foo'.

	self assert: fooMethod rowanPackageName = packageName1.
	self assert: barMethod rowanPackageName = packageName2.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: (audit := project audit) isEmpty.
	self assert: registry packageRegistry size = 2.

"remove methods"
	normalClass rwRemoveSelector: #'foo'.
	normalClass class rwRemoveSelector: #'bar'.

"validate"
	self should: [ normalClass perform: #bar ] raise: MessageNotUnderstood.
	self should: [ normalInstance perform: #foo ] raise: MessageNotUnderstood.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: (audit := project audit) isEmpty.
	self assert: registry packageRegistry size = 2.

"re-add methods"
	fooMethod := normalClass
		rwCompileMethod: 'foo ^''foo'''
		category: '*' , packageName2 asLowercase.
	barMethod := normalClass class
		rwCompileMethod: 'bar ^''bar'''
		category: 'accessing'.

"validate"
	self assert: fooMethod rowanPackageName = packageName2.
	self assert: barMethod rowanPackageName = packageName1.

	self assert: (normalClass perform: #bar) = 'bar'.
	self assert: (normalInstance perform: #foo) = 'foo'.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: (audit := project audit) isEmpty.
	self assert: registry packageRegistry size = 2.

"remove methods"
	normalClass rwRemoveSelector: #'foo'.
	normalClass class rwRemoveSelector: #'bar'.

"validate"
	self should: [ normalClass perform: #bar ] raise: MessageNotUnderstood.
	self should: [ normalInstance perform: #foo ] raise: MessageNotUnderstood.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: (audit := project audit) isEmpty.
	self assert: registry packageRegistry size = 2.
]

{ #category : '*rowan-tests-gemstone-36x' }
RwHybridBrowserToolTest >> testHybridMoveClassToPackageInAlternaterSymbolDict [

	| normalClass projectName packageNames packageName1 packageName2 movedNormalClass 
		packageNameMap symDict registry project audit |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageName2 := 'HybridA-Extensions'.
	packageNames := {packageName1.
	packageName2}.
	packageNameMap := Dictionary new
		at: packageName1 put: self _symbolDictionaryName1;
		at: packageName2 put: self _symbolDictionaryName2;
		yourself.

	self
		_loadPackageMappedProjectDefinition: projectName 
		packageNameMap: packageNameMap 
		defaultSymbolDictName: self _symbolDictionaryName1.

	project := Rowan projectNamed: projectName.

	normalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: normalClass rowanPackageName = packageName1.
	self assert: normalClass category= packageName1.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: (audit := project audit) isEmpty.
	self assert: registry packageRegistry size = 1.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.

	movedNormalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName2
		options: #().

"validate"
	self assert: (audit := project audit) isEmpty.
	self assert: movedNormalClass == normalClass.
	self assert: movedNormalClass rowanPackageName = packageName2.
	self assert: movedNormalClass category= packageName2.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
]

{ #category : '*rowan-tests-gemstone-36x' }
RwHybridBrowserToolTest >> testHybridMoveClassToPackageInAlternaterSymbolDict_class_vars [

	| normalClass projectName packageNames packageName1 packageName2 movedNormalClass 
		packageNameMap symDict registry project audit |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageName2 := 'HybridA-Extensions'.
	packageNames := {packageName1.
	packageName2}.
	packageNameMap := Dictionary new
		at: packageName1 put: self _symbolDictionaryName1;
		at: packageName2 put: self _symbolDictionaryName2;
		yourself.

	self
		_loadPackageMappedProjectDefinition: projectName 
		packageNameMap: packageNameMap 
		defaultSymbolDictName: self _symbolDictionaryName1.

	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.

	normalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: normalClass rowanPackageName = packageName1.
	self assert: normalClass category= packageName1.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: (audit := project audit) isEmpty.
	self assert: registry packageRegistry size = 1.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.

	movedNormalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #()
		classVars: #(Cvar1)
		classInstVars: #()
		poolDictionaries: #()
		category: packageName2
		options: #().

"validate"
	self assert: (audit := project audit) isEmpty.
	self assert: movedNormalClass == normalClass.
	self assert: movedNormalClass rowanPackageName = packageName2.
	self assert: movedNormalClass category= packageName2.
	self assert: (movedNormalClass classVarNames includes: #Cvar1).

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
]

{ #category : '*rowan-tests-gemstone-36x' }
RwHybridBrowserToolTest >> testHybridMoveClassToPackageInAlternaterSymbolDict_class_vars_constraints [

	| normalClass projectName packageNames packageName1 packageName2 movedNormalClass packageNameMap symDict registry project audit |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageName2 := 'HybridA-Extensions'.
	packageNames := {packageName1.
	packageName2}.
	packageNameMap := Dictionary new
		at: packageName1 put: self _symbolDictionaryName1;
		at: packageName2 put: self _symbolDictionaryName2;
		yourself.

	self
		_loadPackageMappedProjectDefinition: projectName 
		packageNameMap: packageNameMap 
		defaultSymbolDictName: self _symbolDictionaryName1.

	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.

	normalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #(ivar1)
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: normalClass rowanPackageName = packageName1.
	self assert: normalClass category= packageName1.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: (audit := project audit) isEmpty.
	self assert: registry packageRegistry size = 1.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.

	movedNormalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #(ivar1)
		classVars: #(Cvar1)
		classInstVars: #()
		poolDictionaries: #()
		category: packageName2
		constraints: {{#ivar1 . Association}}
		options: #().

"validate"
	self assert: (audit := project audit) isEmpty.
	self assert: movedNormalClass == normalClass.
	self assert: movedNormalClass rowanPackageName = packageName2.
	self assert: movedNormalClass category= packageName2.
	self assert: (movedNormalClass classVarNames includes: #Cvar1).
	self assert: (movedNormalClass _constraintOn: #ivar1) = Association.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
]

{ #category : '*rowan-tests-gemstone-36x' }
RwHybridBrowserToolTest >> testHybridMoveClassToPackageInAlternaterSymbolDict_class_vars_constraints_new_class_version [

	| normalClass projectName packageNames packageName1 packageName2 movedNormalClass packageNameMap symDict registry project audit |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageName2 := 'HybridA-Extensions'.
	packageNames := {packageName1.
	packageName2}.
	packageNameMap := Dictionary new
		at: packageName1 put: self _symbolDictionaryName1;
		at: packageName2 put: self _symbolDictionaryName2;
		yourself.

	self
		_loadPackageMappedProjectDefinition: projectName 
		packageNameMap: packageNameMap 
		defaultSymbolDictName: self _symbolDictionaryName1.

	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.

	normalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #(ivar1)
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: normalClass rowanPackageName = packageName1.
	self assert: normalClass category= packageName1.
	self assert: (normalClass instVarNames includes: #ivar1).
	self deny: (normalClass instVarNames includes: #ivar2).

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: (audit := project audit) isEmpty.
	self assert: registry packageRegistry size = 1.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.

	movedNormalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #(ivar1 ivar2)
		classVars: #(Cvar1)
		classInstVars: #()
		poolDictionaries: #()
		category: packageName2
		constraints: {{#ivar1 . Association}}
		options: #().

"validate"
	self assert: movedNormalClass ~~ normalClass.
	self assert: movedNormalClass rowanPackageName = packageName2.
	self assert: movedNormalClass category= packageName2.
	self assert: (movedNormalClass classVarNames includes: #Cvar1).
	self assert: (movedNormalClass _constraintOn: #ivar1) = Association.
	self assert: (movedNormalClass instVarNames includes: #ivar1).
	self assert: (movedNormalClass instVarNames includes: #ivar2).

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: (audit := project audit) isEmpty.
	self assert: registry packageRegistry size = 1.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
]

{ #category : '*rowan-tests-gemstone-36x' }
RwHybridBrowserToolTest >> testHybridMoveMethod_309 [

	"https://github.com/dalehenrich/Rowan/issues/309"

	|  normalClass projectName packageNames packageName1 packageName2 packageName3 className symDict registry
		project audit |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageName2 := 'HybridA-Extensions1'.
	packageName3 := 'HybridA-Extensions2'.
	packageNames := {packageName1.
	packageName2.
	packageName3}.
	className :=  'SimpleHybridNormal1'.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.

	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.

	normalClass := Object
		rwSubclass: className
		instVarNames: #(foo)
		classVars: #(Bar)
		classInstVars: #(baz)
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: normalClass rowanPackageName = packageName1.

	normalClass
		rwCompileMethod: 'foo ^foo'
		category: '*' , packageName2 asLowercase.
	normalClass
		rwCompileMethod: 'bar ^Bar'
		category: 'accessing'.
	normalClass class
		rwCompileMethod: 'baz ^baz'
		category: 'accessing'.

"validate"
	self assert: (normalClass categoryOfSelector: #foo) = ( '*' , packageName2 asLowercase) asSymbol.
	self assert: (normalClass categoryOfSelector: #bar) = #accessing.
	self assert: (normalClass class categoryOfSelector: #baz) = #accessing.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: (audit := project audit) isEmpty.
	self assert: registry packageRegistry size = 3.

	self
		_assert: [ :classExtensionDef :packageDef :projectDef | 
			packageDef name = packageName1
				ifTrue: [self assert: false description: 'no class extensions expected in package ', packageDef name printString ].
			packageDef name = packageName2
				ifTrue: [
					| classExtension |
					classExtension := packageDef classExtensions at: className.
					self assert: (classExtension instanceMethodDefinitions includesKey: #foo).
					self assert: (classExtension classMethodDefinitions isEmpty) ].
			packageDef name = packageName3
				ifTrue: [self assert: false description: 'no class extensions expected in package ', packageDef name printString ] ]
		forClassExtensionsIn: className.
	self
		_assert: [ :classDef :packageDef :projectDef | 
			packageDef name = packageName1
				ifTrue: [ 
					| instanceMethodDict |
					self assert: classDef name = className.
					instanceMethodDict := classDef instanceMethodDefinitions.
					self deny: (instanceMethodDict includesKey: #foo).
					self assert: (instanceMethodDict includesKey: #bar).
					self assert: (classDef classMethodDefinitions includesKey: #baz) ]
				ifFalse: [ self assert: false description: 'no class definitions expected in package ', packageDef name printString ]  ]
		forClassNamed: className.

"load"
	Rowan projectTools browser
		moveMethod: #'foo' forClassNamed: className isMeta: false toProtocol: 'accessing';
		moveMethod: #'bar' forClassNamed: className isMeta: false toProtocol: ( '*' , packageName2 asLowercase);
		moveMethod: #'baz' forClassNamed: className isMeta: true toProtocol: ( '*' , packageName2 asLowercase);
		yourself.

"validate"
	self assert: (normalClass categoryOfSelector: #foo) = #accessing.
	self assert: (normalClass categoryOfSelector: #bar) = ( '*' , packageName2 asLowercase) asSymbol.
	self assert: (normalClass class categoryOfSelector: #baz) = ( '*' , packageName2 asLowercase) asSymbol.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: (audit := project audit) isEmpty.
	self assert: registry packageRegistry size = 3.

	self
		_assert: [ :classExtensionDef :packageDef :projectDef | 
			packageDef name = packageName1
				ifTrue: [ self assert: false description: 'no class extensions expected in package ', packageDef name printString ].
			packageDef name = packageName2
				ifTrue: [
					| classExtension |
					classExtension := packageDef classExtensions at: className.
					self deny: (classExtension instanceMethodDefinitions includesKey: #foo).
					self assert: (classExtension instanceMethodDefinitions includesKey: #bar).
					self assert: (classExtension classMethodDefinitions includesKey: #baz) ].
			packageDef name = packageName3
				ifTrue: [ self assert: false description: 'no class extensions expected in package ', packageDef name printString ] ]
		forClassExtensionsIn: className.
	self
		_assert: [ :classDef :packageDef :projectDef | 
			packageDef name = packageName1
				ifTrue: [ 
					self assert: classDef name = className.
					self assert: (classDef instanceMethodDefinitions includesKey: #foo).
					self deny: (classDef instanceMethodDefinitions includesKey: #bar).
					self assert: classDef classMethodDefinitions isEmpty ]
				ifFalse: [ self assert: false description: 'no class definitions expected in package ', packageDef name printString ] ]
		forClassNamed: className.

"load"
	Rowan projectTools browser
		moveMethod: #'foo' forClassNamed: className isMeta: false toProtocol: ( '*' , packageName3 asLowercase);
		moveMethod: #'bar' forClassNamed: className isMeta: false toProtocol: ( '*' , packageName3 asLowercase);
		moveMethod: #'baz' forClassNamed: className isMeta: true toProtocol: ( '*' , packageName3 asLowercase);
		yourself.
"validate"
	self assert: (normalClass categoryOfSelector: #foo) = ( '*' , packageName3 asLowercase) asSymbol.
	self assert: (normalClass categoryOfSelector: #bar) = ( '*' , packageName3 asLowercase) asSymbol.
	self assert: (normalClass class categoryOfSelector: #baz) = ( '*' , packageName3 asLowercase) asSymbol.

	self
		_assert: [ :classExtensionDef :packageDef :projectDef | 
			(classExtensionDef instanceMethodDefinitions isEmpty and: [classExtensionDef classMethodDefinitions isEmpty])
				ifFalse: [ 
					packageDef name = packageName1
						ifTrue: [ self assert: false description: 'no class extensions expected in package ', packageDef name printString ].
					packageDef name = packageName2
						ifTrue: [ self assert: false description: 'no class extensions expected in package ', packageDef name printString ].
					packageDef name = packageName3
						ifTrue: [
							| classExtension |
							classExtension := packageDef classExtensions at: className.
							self assert: (classExtension instanceMethodDefinitions includesKey: #foo).
							self assert: (classExtension instanceMethodDefinitions includesKey: #bar).
							self assert: (classExtension classMethodDefinitions includesKey: #baz) ] ] ]
		forClassExtensionsIn: className.
	self
		_assert: [ :classDef :packageDef :projectDef | 
					self assert: packageDef name = packageName1.
					self assert: classDef name = className.
					self assert: classDef instanceMethodDefinitions isEmpty.
					self assert: classDef classMethodDefinitions isEmpty ]
		forClassNamed: className.
]
