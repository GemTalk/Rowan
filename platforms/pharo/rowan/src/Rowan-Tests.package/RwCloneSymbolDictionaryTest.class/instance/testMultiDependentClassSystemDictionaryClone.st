tests
testMultiDependentClassSystemDictionaryClone

	| projectName packageName1 className1 className2 className3 symDictName theOriginalClass1 theOriginalClass2 theOriginalClass3 theSymbolDict validationBlock clonedSymbolDict 
		clonedClass1 clonedClass2 clonedClass3 clonedSymDictName | 

	projectName := 'SystemDictionaryClone_Project'.
	packageName1 := 'SystemDictionaryClone-Core'.
	className1 := 'SystemDictionaryCloneClass_1'.
	className2 := 'SystemDictionaryCloneClass_2'.
	className3 := 'SystemDictionaryCloneClass_3'.
	symDictName := self _symbolDictionaryName2.
	clonedSymDictName :=  (symDictName, '_CLONE') asSymbol.

	self 
		_createLoadedProjectNamed: projectName 
		root: '/tmp/rowanIssuesProject/' 
		symbolDictionaryName: symDictName 
		validate: false.

	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.

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

	theOriginalClass1 cv1: #one; civ1: #one.
	theOriginalClass2 cv2: #two; civ2: #two.
	theOriginalClass3 cv3: #three; civ3: #three.

	validationBlock := [:aClass :cv1 :cv2 :cv3|
		| x |
		aClass new iv1; iv2.	"no MNU is sufficient"
		self assert: (x := aClass new civ1) = cv1.
		self assert: (x := aClass new civ2) = cv2.
		self assert: (x := aClass new civ3) = cv3.
		self assert: (x := aClass new cv1) = cv1.
		self assert: (x := aClass new cv2) = cv2.
		self assert: (x := aClass new cv3) = cv3.
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

	clonedClass1 cv1: #ONE; civ1: #ONE.
	clonedClass2 cv2: #TWO; civ2: #TWO.
	clonedClass3 cv3: #THREE; civ3: #THREE.

	validationBlock value: theOriginalClass1 value: #one value: #two value: #three.
	validationBlock value: theOriginalClass2 value: #one value: #two value: #three.
	validationBlock value: theOriginalClass3 value: #one value: #two value: #three.

	validationBlock value: clonedClass1 value: #ONE value: #TWO value: #THREE.
	validationBlock value: clonedClass2 value: #ONE value: #TWO value: #THREE.
	validationBlock value: clonedClass3 value: #ONE value: #TWO value: #THREE.
