tests
testHybridComplicatedClassCopy

	| projectNames projectName1 projectName2 packageNames project1PackageName1 project1PackageName2 project2PackageName1
		project2PackageName2 className1 className2 classNames classPackageNames1 classPackageNames2 classPackageNames 
		defaultSymbolDictNames comments newClassName testClass meth newClass |

	projectName1 := 'HybridClassCopy1'.
	projectName2 := 'HybridClassCopy2'.
	projectNames := {projectName1.
	projectName2}.
	project1PackageName1 := 'ClassCopy1-Core'.
	project1PackageName2 := 'ClassCopy12-Extensions'.
	project2PackageName1 := 'ClassCopy2-Core'.
	project2PackageName2 := 'ClassCopy2-Extensions'.
	packageNames := {{project1PackageName1.
	project1PackageName2}.
	{project2PackageName1.
	project2PackageName2}}.
	className1 := 'HybridCopyClass1'.
	className2 := 'HybridCopyClass2'.
	newClassName := 'HybridCopyClass1Copy'.
	classNames := {className1.
	className2}.
	classPackageNames1 := 'ClassCopy1-Core'.
	classPackageNames2 := 'ClassCopy2-Core'.
	classPackageNames := {classPackageNames1.
	classPackageNames2}.
	defaultSymbolDictNames := {(self _symbolDictionaryName1).
	(self _symbolDictionaryName1)}.
	comments := {'new class version project 1'.
	'new class version project2'}.
	self
		_loadMultiProjectDefinition: projectNames
		packageNames: packageNames
		defaultSymbolDictName: defaultSymbolDictNames
		comment: comments
		className: classNames
		inPackageName: classPackageNames.

	testClass := Rowan globalNamed: className1.

	meth := testClass
		rwCompileMethod: 'foo ^1'
		category: '*' , project1PackageName2 asLowercase.
	self assert: meth rowanPackageName = project1PackageName2. 
	meth := testClass class
		rwCompileMethod: 'fooClass ^1'
		category: '*' , project1PackageName2 asLowercase.
	self assert: meth rowanPackageName = project1PackageName2. 
	meth := testClass
		rwCompileMethod: 'bar ^1'
		category: '*' , project2PackageName2 asLowercase.
	self assert: meth rowanPackageName = project2PackageName2. 
	meth := testClass class
		rwCompileMethod: 'barClass ^1'
		category: '*' , project2PackageName2 asLowercase.
	self assert: meth rowanPackageName = project2PackageName2. 

	self assert: testClass notNil.
	self assert: testClass cvar1 = 2.
	self assert: testClass civar1 = 1.
	self assert: testClass fooClass = 1.
	self assert: testClass barClass = 1.

	self assert: testClass new ivar1 isNil.
	self assert: testClass new foo = 1.
	self assert: testClass new bar = 1.

	self assert: (testClass categoryOfSelector: #foo) = ( '*' , project1PackageName2 asLowercase) asSymbol.
	self assert: (testClass class categoryOfSelector: #fooClass) = ( '*' , project1PackageName2 asLowercase) asSymbol.
	self assert: (testClass categoryOfSelector: #bar) = ( '*' , project2PackageName2 asLowercase) asSymbol.
	self assert: (testClass class categoryOfSelector: #barClass) = ( '*' , project2PackageName2 asLowercase) asSymbol.

	newClass := Rowan projectTools browser copyClassNamed: className1 to: newClassName.
	newClass initialize.

	self assert: newClass == (Rowan globalNamed: newClassName).

	self assert: newClass ~~ testClass.
	self assert: newClass cvar1 = 2.
	self assert: newClass civar1 = 1.
	self assert: newClass fooClass = 1.
	self assert: newClass barClass = 1.

	self assert: newClass new ivar1 isNil.
	self assert: newClass new foo = 1.
	self assert: newClass new bar = 1.

	self assert: (newClass categoryOfSelector: #foo) = ( '*' , project1PackageName2 asLowercase) asSymbol.
	self assert: (newClass class categoryOfSelector: #fooClass) = ( '*' , project1PackageName2 asLowercase) asSymbol.
	self assert: (newClass categoryOfSelector: #bar) = ( '*' , project2PackageName2 asLowercase) asSymbol.
	self assert: (newClass class categoryOfSelector: #barClass) = ( '*' , project2PackageName2 asLowercase) asSymbol.
