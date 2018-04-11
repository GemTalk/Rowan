Extension { #name : 'RwHybridBrowserToolTest' }

{ #category : '*rowan-tode' }
RwHybridBrowserToolTest >> testHybridPackagePatch [

	"Write project to disk, make a few modifications and then check patch for correctness"

	| normalClass projectName packageNames packageName1 packageName2 fooMethod barMethod normalInstance projectTools bazMethod ivar1Method packageTools patch1 patch2 operations |
	projectName := 'HybridPatchProjectA'.
	packageName1 := 'Hybrid-Patch-Core'.
	packageName2 := 'Hybrid-Patch-Extensions'.
	packageNames := {packageName1.
	packageName2}.
	self
		_loadDiskProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project package patch test'
		format: 'tonel'
		root: '/tmp/rowanHybridPatchProject/'
		repoPath: 'src'.

	normalClass := Object
		rwSubclass: 'SimpleHybridNormalPatch'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: normalClass rowanPackageName = packageName1.

	fooMethod := normalClass rwCompileMethod: 'foo ^''foo''' category: 'accessing'.
	barMethod := normalClass class
		rwCompileMethod: 'bar ^''bar'''
		category: '*' , packageName2 asLowercase.

	self assert: normalClass bar = 'bar'.
	normalInstance := normalClass new.
	self assert: normalInstance foo = 'foo'.

	projectTools := Rowan projectTools.
	projectTools spec exportProjectNamed: projectName.
	projectTools write writeProjectNamed: projectName.
	projectTools commit
		commitProjectNamed: projectName
		message: 'Added Simple class and extension methods'.

	normalClass := Object
		rwSubclass: 'SimpleHybridNormalPatch'
		instVarNames: #('ivar1')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().	"add new instance variable to class"

	normalInstance := normalClass new.

	fooMethod := normalClass
		rwCompileMethod: 'foo "comment" ^''foo'''
		category: 'accessing'.	"change method"
	ivar1Method := normalClass
		rwCompileMethod: 'ivar1 ^ivar1'
		category: 'accessing'.	"add new instance method"
	bazMethod := normalClass class
		rwCompileMethod: 'baz ^''baz'''
		category: '*' , packageName2 asLowercase.	"add new class-side method"
	normalClass class rwRemoveSelector: #'bar'.	"remove existing method"

	self assert: normalInstance foo = 'foo'.
	self assert: normalInstance ivar1 = nil.
	self assert: normalClass baz = 'baz'.
	self should: [ normalClass bar ] raise: MessageNotUnderstood.

	packageTools := Rowan packageTools.
	patch1 := packageTools diff patchForPackageName: packageName1.
	patch2 := packageTools diff patchForPackageName: packageName2.

	operations := patch1 operations.
	self assert: operations size = 3.
	self assert: (operations select: [ :each | each isAddition ]) size = 1.
	self assert: (operations select: [ :each | each isModification ]) size = 2.
	self assert: (operations select: [ :each | each isRemoval ]) size = 0.
	operations
		do: [ :patchOperation | 
			patchOperation isAddition
				ifTrue: [ 
					patchOperation definition class == RwCypressMethodDefinition.
					patchOperation definition source = 'ivar1 ^ivar1' ].
			patchOperation isModification
				ifTrue: [ 
					self
						assert:
							(patchOperation definition class == RwCypressMethodDefinition
								or: [ patchOperation definition class == CypressClassDefinition ]).
					patchOperation definition class == RwCypressMethodDefinition
						ifTrue: [ patchOperation definition source = 'foo "comment" ^''foo''' ].
					patchOperation definition class == CypressClassDefinition
						ifTrue: [ 
							self assert: patchOperation modification className = 'SimpleHybridNormalPatch'.
							self assert: patchOperation obsoletion instVarNames = #().
							self assert: patchOperation modification instVarNames = #('ivar1') ] ].
			patchOperation isRemoval
				ifTrue: [ self assert: false description: 'no removal expected' ] ].

	operations := patch2 operations.
	self assert: operations size = 2.
	self assert: (operations select: [ :each | each isAddition ]) size = 1.
	self assert: (operations select: [ :each | each isModification ]) size = 0.
	self assert: (operations select: [ :each | each isRemoval ]) size = 1.
	operations
		do: [ :patchOperation | 
			patchOperation isAddition
				ifTrue: [ 
					patchOperation definition class == RwCypressMethodDefinition.
					patchOperation definition source = 'baz ^''baz''' ].
			patchOperation isModification
				ifTrue: [ self assert: false description: 'no modification expected' ].
			patchOperation isRemoval
				ifTrue: [ self assert: patchOperation definition class == CypressMethodDefinition ] ]
]
