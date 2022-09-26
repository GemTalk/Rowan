Class {
	#name : 'RwCloneSymbolDictionaryTest',
	#superclass : 'RwRowanProjectIssuesTest',
	#category : 'Rowan-Tests-ComponentsV2-OnlyV2'
}

{ #category : 'private' }
RwCloneSymbolDictionaryTest class >> _clonedLoaderSymbolDictionaryName [
	^ #'RowanLoader_cloned'
]

{ #category : 'private' }
RwCloneSymbolDictionaryTest class >> _symbolDictionaryNames [
	^ super _symbolDictionaryNames
		,
			{((self _symbolDictionaryName2 , '_CLONE') asSymbol).
			(self _clonedLoaderSymbolDictionaryName)}
]

{ #category : 'Testing' }
RwCloneSymbolDictionaryTest class >> shouldInheritSelectors [

	^false

]

{ #category : 'private' }
RwCloneSymbolDictionaryTest >> _clonedLoaderSymbolDictionaryName [
	^ self class _clonedLoaderSymbolDictionaryName
]

{ #category : 'tests' }
RwCloneSymbolDictionaryTest >> testClassVarSystemDictionaryClone [

	| projectName packageName1 className symDictName theOriginalClass theSymbolDict validationBlock clonedSymbolDict 
		clonedClass clonedSymDictName | 

	projectName := 'SystemDictionaryClone_Project'.
	packageName1 := 'SystemDictionaryClone-Core'.
	className := 'SystemDictionaryCloneClass'.
	symDictName := self _symbolDictionaryName2.
	clonedSymDictName :=  (symDictName, '_CLONE') asSymbol.

	(Rowan image loadedProjectNamed:projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

	self 
		_createLoadedProjectNamed: projectName 
		root: self _testRowanProjectsSandbox pathString, '/rowanIssuesProject/' 
		symbolDictionaryName: symDictName 
		validate: false.

	self _addPackageNamed: packageName1 toComponentNamed: 'Core' andProjectNamed: projectName validate: false.

	theOriginalClass := Object
		rwSubclass: className
		instVarNames: #(iv1)
		classVars: #(CV1)
		classInstVars: #(civ1)
		poolDictionaries: #()
		category: packageName1
		options: #().

	theOriginalClass class rwCompileMethod: 'cv1 ^CV1' category: 'accessing'.
	theOriginalClass class rwCompileMethod: 'cv1: x CV1 := x' category: 'accessing'.

	validationBlock := [:aClass :expectedValue |
		| x |
		self assert: (x := aClass perform: #cv1) = expectedValue.
	].

	theOriginalClass perform: #cv1: with: #one.

	validationBlock value: theOriginalClass value: #one.

	theSymbolDict := Rowan globalNamed: symDictName.

	clonedSymbolDict := theSymbolDict _rowanCloneSymbolDictionaryNamed: clonedSymDictName symbolList: GsCurrentSession currentSession symbolList.
	self assert: clonedSymbolDict name == clonedSymDictName.

	clonedClass := clonedSymbolDict at: className asSymbol.

	self assert: clonedClass ~~ theOriginalClass.
	self assert: clonedClass classHistory ~~ theOriginalClass classHistory.

	validationBlock value: clonedClass value: #one.
	validationBlock value: theOriginalClass value: #one.

	clonedClass perform: #cv1: with: #ONE.

	validationBlock value: clonedClass value: #ONE.
	validationBlock value: theOriginalClass value: #one.
]

{ #category : 'tests' }
RwCloneSymbolDictionaryTest >> testMultiDependentClassSystemDictionaryClone [

	| projectName packageName1 className1 className2 className3 symDictName theOriginalClass1 theOriginalClass2 theOriginalClass3 theSymbolDict validationBlock clonedSymbolDict 
		clonedClass1 clonedClass2 clonedClass3 clonedSymDictName | 

	projectName := 'SystemDictionaryClone_Project'.
	packageName1 := 'SystemDictionaryClone-Core'.
	className1 := 'SystemDictionaryCloneClass_1'.
	className2 := 'SystemDictionaryCloneClass_2'.
	className3 := 'SystemDictionaryCloneClass_3'.
	symDictName := self _symbolDictionaryName2.
	clonedSymDictName :=  (symDictName, '_CLONE') asSymbol.

	(Rowan image loadedProjectNamed:projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

	self 
		_createLoadedProjectNamed: projectName 
		root: self _testRowanProjectsSandbox pathString, '/rowanIssuesProject/' 
		symbolDictionaryName: symDictName 
		validate: false.

	self _addPackageNamed: packageName1 toComponentNamed: 'Core' andProjectNamed: projectName validate: false.

	theOriginalClass1 := Object
		rwSubclass: className1
		instVarNames: #(iv1 )
		classVars: #(CV1)
		classInstVars: #(civ1)
		poolDictionaries: #()
		category: packageName1
		options: #().
	theOriginalClass2 := theOriginalClass1
		rwSubclass: className2
		instVarNames: #(iv2)
		classVars: #(CV2)
		classInstVars: #(civ2)
		poolDictionaries: #()
		category: packageName1
		options: #().
	theOriginalClass3 := Object
		rwSubclass: className3
		instVarNames: #(iv3)
		classVars: #(CV3)
		classInstVars: #(civ3)
		poolDictionaries: #()
		category: packageName1
		options: #().
	theOriginalClass1 rwCompileMethod: 'iv1 ^iv1' category: 'accessing'.
	theOriginalClass1 rwCompileMethod: 'iv2 ^#none' category: 'accessing'.
	theOriginalClass1 rwCompileMethod: 'cv1 ^', className1, ' cv1' category: 'accessing'.
	theOriginalClass1 rwCompileMethod: 'cv2 ^', className2, ' cv2' category: 'accessing'.
	theOriginalClass1 rwCompileMethod: 'cv3 ^', className3, ' cv3' category: 'accessing'.
	theOriginalClass1 rwCompileMethod: 'civ1 ^', className1, ' civ1' category: 'accessing'.
	theOriginalClass1 rwCompileMethod: 'civ2 ^', className2, ' civ2' category: 'accessing'.
	theOriginalClass1 rwCompileMethod: 'civ3 ^', className3, ' civ3' category: 'accessing'.
	theOriginalClass1 class rwCompileMethod: 'cv1 ^CV1' category: 'accessing'.
	theOriginalClass1 class rwCompileMethod: 'cv1: x CV1 := x' category: 'accessing'.
	theOriginalClass1 class rwCompileMethod: 'civ1 ^civ1' category: 'accessing'.
	theOriginalClass1 class rwCompileMethod: 'civ1: x civ1 := x' category: 'accessing'.

	theOriginalClass2 rwCompileMethod: 'iv1 ^iv1' category: 'accessing'.
	theOriginalClass2 rwCompileMethod: 'iv2 ^iv2' category: 'accessing'.
	theOriginalClass2 rwCompileMethod: 'cv1 ^', className1, ' cv1' category: 'accessing'.
	theOriginalClass2 rwCompileMethod: 'cv2 ^', className2, ' cv2' category: 'accessing'.
	theOriginalClass2 rwCompileMethod: 'cv3 ^', className3, ' cv3' category: 'accessing'.
	theOriginalClass2 rwCompileMethod: 'civ1 ^', className1, ' civ1' category: 'accessing'.
	theOriginalClass2 rwCompileMethod: 'civ2 ^', className2, ' civ2' category: 'accessing'.
	theOriginalClass2 rwCompileMethod: 'civ3 ^', className3, ' civ3' category: 'accessing'.
	theOriginalClass2 class rwCompileMethod: 'cv2 ^CV2' category: 'accessing'.
	theOriginalClass2 class rwCompileMethod: 'cv2: x CV2 := x' category: 'accessing'.
	theOriginalClass2 class rwCompileMethod: 'civ2 ^civ2' category: 'accessing'.
	theOriginalClass2 class rwCompileMethod: 'civ2: x civ2 := x' category: 'accessing'.

	theOriginalClass3 rwCompileMethod: 'iv1 ^#none' category: 'accessing'.
	theOriginalClass3 rwCompileMethod: 'iv2 ^#none' category: 'accessing'.
	theOriginalClass3 rwCompileMethod: 'cv1 ^', className1, ' cv1' category: 'accessing'.
	theOriginalClass3 rwCompileMethod: 'cv2 ^', className2, ' cv2' category: 'accessing'.
	theOriginalClass3 rwCompileMethod: 'cv3 ^', className3, ' cv3' category: 'accessing'.
	theOriginalClass3 rwCompileMethod: 'civ1 ^', className1, ' civ1' category: 'accessing'.
	theOriginalClass3 rwCompileMethod: 'civ2 ^', className2, ' civ2' category: 'accessing'.
	theOriginalClass3 rwCompileMethod: 'civ3 ^', className3, ' civ3' category: 'accessing'.
	theOriginalClass3 class rwCompileMethod: 'cv3 ^CV3' category: 'accessing'.
	theOriginalClass3 class rwCompileMethod: 'cv3: x CV3 := x' category: 'accessing'.
	theOriginalClass3 class rwCompileMethod: 'civ3 ^civ3' category: 'accessing'.
	theOriginalClass3 class rwCompileMethod: 'civ3: x civ3 := x' category: 'accessing'.

	theOriginalClass1 perform: #cv1: with: #one; perform: #civ1: with: #one.
	theOriginalClass2 perform: #cv2: with: #two; perform: #civ2: with: #two.
	theOriginalClass3 perform: #cv3: with: #three; perform: #civ3: with: #three.

	validationBlock := [:aClass :cv1 :cv2 :cv3|
		| x |
		aClass new perform: #iv1; perform: #iv2.	"no MNU is sufficient"
		self assert: (x := aClass new perform: #civ1) = cv1.
		self assert: (x := aClass new perform: #civ2) = cv2.
		self assert: (x := aClass new perform: #civ3) = cv3.
		self assert: (x := aClass new perform: #cv1) = cv1.
		self assert: (x := aClass new perform: #cv2) = cv2.
		self assert: (x := aClass new perform: #cv3) = cv3.
	].

	validationBlock value: theOriginalClass1 value: #one value: #two value: #three.
	validationBlock value: theOriginalClass2 value: #one value: #two value: #three.
	validationBlock value: theOriginalClass3 value: #one value: #two value: #three.

	theSymbolDict := Rowan globalNamed: symDictName.
	clonedSymbolDict := theSymbolDict _rowanCloneSymbolDictionaryNamed: clonedSymDictName symbolList: GsCurrentSession currentSession symbolList.
	self assert: clonedSymbolDict name == clonedSymDictName.

	clonedClass1 := clonedSymbolDict at: className1 asSymbol.
	clonedClass2 := clonedSymbolDict at: className2 asSymbol.
	clonedClass3 := clonedSymbolDict at: className3 asSymbol.

	self assert: clonedClass2 superclass == clonedClass1.

	self assert: clonedClass1 ~~ theOriginalClass1.
	self assert: clonedClass1 classHistory ~~ theOriginalClass1 classHistory.
	self assert: clonedClass2 ~~ theOriginalClass2.
	self assert: clonedClass2 classHistory ~~ theOriginalClass2 classHistory.
	self assert: clonedClass3 ~~ theOriginalClass3.
	self assert: clonedClass3 classHistory ~~ theOriginalClass3 classHistory.

	validationBlock value: clonedClass1 value: #one value: #two value: #three.
	validationBlock value: clonedClass2 value: #one value: #two value: #three.
	validationBlock value: clonedClass3 value: #one value: #two value: #three.

	clonedClass1 perform: #cv1: with: #ONE; perform: #civ1: with: #ONE.
	clonedClass2 perform: #cv2: with: #TWO; perform: #civ2: with: #TWO.
	clonedClass3 perform: #cv3: with: #THREE; perform: #civ3: with: #THREE.

	validationBlock value: theOriginalClass1 value: #one value: #two value: #three.
	validationBlock value: theOriginalClass2 value: #one value: #two value: #three.
	validationBlock value: theOriginalClass3 value: #one value: #two value: #three.

	validationBlock value: clonedClass1 value: #ONE value: #TWO value: #THREE.
	validationBlock value: clonedClass2 value: #ONE value: #TWO value: #THREE.
	validationBlock value: clonedClass3 value: #ONE value: #TWO value: #THREE.
]

{ #category : 'tests' }
RwCloneSymbolDictionaryTest >> testRowanLoaderSystemDictionaryClone [

	| rowanLoaderSymbolDict clonedSymDictName clonedSymbolDict |
	rowanLoaderSymbolDict := Rowan globalNamed: 'RowanLoader'.
	clonedSymDictName := self _clonedLoaderSymbolDictionaryName.
	clonedSymbolDict := rowanLoaderSymbolDict 
		_rowanCloneSymbolDictionaryNamed: clonedSymDictName 
		symbolList: GsCurrentSession currentSession symbolList.
]

{ #category : 'tests' }
RwCloneSymbolDictionaryTest >> testSimpleSystemDictionaryClone [

	| projectName packageName1 className symDictName theOriginalClass theSymbolDict validationBlock clonedSymbolDict 
		clonedClass clonedSymDictName | 

	projectName := 'SystemDictionaryClone_Project'.
	packageName1 := 'SystemDictionaryClone-Core'.
	className := 'SystemDictionaryCloneClass'.
	symDictName := self _symbolDictionaryName2.
	clonedSymDictName :=  (symDictName, '_CLONE') asSymbol.

	(Rowan image loadedProjectNamed:projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

	self
		_createLoadedProjectNamed: projectName 
		root: self _testRowanProjectsSandbox pathString, '/rowanIssuesProject/' 
		symbolDictionaryName: symDictName 
		validate: false.

	self _addPackageNamed: packageName1 toComponentNamed: 'Core' andProjectNamed: projectName validate: false.

	theOriginalClass := Object
		rwSubclass: className
		instVarNames: #(iv1)
		classVars: #(CV1)
		classInstVars: #(civ1)
		poolDictionaries: #()
		category: packageName1
		options: #().
	theOriginalClass rwCompileMethod: 'foo ^''foo''' category: 'accessing'.
	theOriginalClass rwCompileMethod: 'iv1 ^iv1' category: 'accessing'.
	theOriginalClass rwCompileMethod: 'initialize iv1:=3' category: 'initialization'.

	theOriginalClass class rwCompileMethod: 'bar ^''bar''' category: 'accessing'.
	theOriginalClass class rwCompileMethod: 'cv1 ^CV1' category: 'accessing'.
	theOriginalClass class rwCompileMethod: 'civ1 ^civ1' category: 'accessing'.
	theOriginalClass class rwCompileMethod: 'initialize civ1:=1. CV1:=2.' category: 'initialization'.
	theOriginalClass class rwCompileMethod: 'new ^ self basicNew initialize; yourself' category: 'instance creation'.

	theOriginalClass initialize.
	validationBlock := [:aClass |
		self assert: (aClass perform: #cv1) = 2.
		self assert: (aClass perform: #civ1) = 1.
		self assert: (aClass perform: #bar) = 'bar'.
		self assert: (aClass new perform: #iv1) = 3.
		self assert: (aClass new perform: #foo) = 'foo'.
	].

	validationBlock value: theOriginalClass.
	theSymbolDict := Rowan globalNamed: symDictName.

	clonedSymbolDict := theSymbolDict _rowanCloneSymbolDictionaryNamed: clonedSymDictName symbolList: GsCurrentSession currentSession symbolList.
	self assert: clonedSymbolDict name == clonedSymDictName.

	clonedClass := clonedSymbolDict at: className asSymbol.

	self assert: clonedClass ~~ theOriginalClass.
	self assert: clonedClass classHistory ~~ theOriginalClass classHistory.

	validationBlock value: clonedClass.
]

{ #category : 'tests' }
RwCloneSymbolDictionaryTest >> testSubclassSystemDictionaryClone [

	| projectName packageName1 className1 className2 symDictName theOriginalClass theSymbolDict clonedSymbolDict 
		clonedClass1 clonedClass2 clonedSymDictName theOriginalSubclass x | 

	projectName := 'SystemDictionaryClone_Project'.
	packageName1 := 'SystemDictionaryClone-Core'.
	className1 := 'SystemDictionaryCloneClass_1'.
	className2 := 'SystemDictionaryCloneClass_2'.
	symDictName := self _symbolDictionaryName2.
	clonedSymDictName :=  (symDictName, '_CLONE') asSymbol.

	(Rowan image loadedProjectNamed:projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

	self 
		_createLoadedProjectNamed: projectName 
		root: self _testRowanProjectsSandbox pathString, '/rowanIssuesProject/' 
		symbolDictionaryName: symDictName 
		validate: false.

	self _addPackageNamed: packageName1 toComponentNamed: 'Core' andProjectNamed: projectName validate: false.

	theOriginalClass := Object
		rwSubclass: className1
		instVarNames: #(iv1)
		classVars: #(CV1)
		classInstVars: #(civ1)
		poolDictionaries: #()
		category: packageName1
		options: #().
	theOriginalSubclass := theOriginalClass
		rwSubclass: className2
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	theSymbolDict := Rowan globalNamed: symDictName.

	clonedSymbolDict := theSymbolDict _rowanCloneSymbolDictionaryNamed: clonedSymDictName symbolList: GsCurrentSession currentSession symbolList.
	self assert: clonedSymbolDict name == clonedSymDictName.

	clonedClass1 := clonedSymbolDict at: className1 asSymbol.

	self assert: clonedClass1 ~~ theOriginalClass.
	self assert: clonedClass1 classHistory ~~ theOriginalClass classHistory.

	clonedClass2 := clonedSymbolDict at: className2 asSymbol.

	self assert: clonedClass2 ~~ theOriginalSubclass.
	self assert: clonedClass2 classHistory ~~ theOriginalSubclass classHistory.

	self assert: (x := clonedClass2 superclass) == clonedClass1
]
