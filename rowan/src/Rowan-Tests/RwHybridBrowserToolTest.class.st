Class {
	#name : 'RwHybridBrowserToolTest',
	#superclass : 'RwBrowserToolTest',
	#category : 'Rowan-Tests'
}

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridClassComment [

	| normalClass projectName packageNames packageName comment |
	projectName := 'Hybrid Project A'.
	packageName := 'HybridA-Core'.
	packageNames := {packageName}.
	comment := 'a comment'.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.

	normalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName
		options: #().
	self assert: normalClass rowanPackageName = packageName.

	normalClass rwComment: comment.
	self assert: normalClass comment = comment

]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridClassCopy [

	|  normalClass projectName packageNames packageName1 packageName2 comment newClassName newClass className |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageName2 := 'HybridA-Extensions'.
	packageNames := {packageName1.
	packageName2}.
	comment := 'a comment'.
	className :=  'SimpleHybridNormal1'.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.

	normalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
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
	normalClass class
		rwCompileMethod: 'bar ^Bar'
		category: 'accessing'.
	normalClass class
		rwCompileMethod: 'baz ^baz'
		category: 'accessing'.
	normalClass rwComment: comment.

	self assert: (normalClass new perform: #foo) isNil.
	self assert: (normalClass  perform: #bar) isNil.
	self assert: (normalClass  perform: #baz) isNil.
	self assert: normalClass comment = comment.

	newClassName := 'SimpleHybridNormal1Copy'.
	newClass := Rowan projectTools browser copyClassNamed: className to: newClassName.

	self assert: (newClass new  perform: #foo) isNil.
	self assert: (newClass  perform: #bar) isNil.
	self assert: (newClass  perform: #baz) isNil.
	self assert: newClass comment = comment.

	self assert: (Rowan globalNamed: className) == normalClass
]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridClassCreation [

	| byteClass indexableClass normalClass projectName packageNames packageName1 packageName2 |
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

	byteClass := Object
		rwByteSubclass: 'SimpleHybridByte1'
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: byteClass rowanPackageName = packageName1.

	indexableClass := Object
		rwIndexableSubclass: 'SimpleHybridIndexable1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: indexableClass rowanPackageName = packageName1.

	normalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: normalClass rowanPackageName = packageName1
]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridClassCreationWithClassCreationTemplateA [

	"https://github.com/dalehenrich/Rowan/issues/17"

	"make sure that we actually evaluate the class creation templates"

	| normalClass projectName packageNames packageName1 packageName2 browserTool template |
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
	browserTool := Rowan projectTools browser.

	template := browserTool
		classCreationTemplateForSubclassOf: 'Object'
		className: 'SimpleHybridNormal1'
		category: packageName1.
	normalClass := template evaluate.
	self assert: normalClass rowanPackageName = packageName1.
	self assert: normalClass category = packageName1.

	template := browserTool
		classCreationTemplateForSubclassOf: 'Object'
		className: 'SimpleHybridNormal1'
		category: packageName1
		packageName: packageName1.
	normalClass := template evaluate.
	self assert: normalClass rowanPackageName = packageName1.
	self assert: normalClass category = packageName1.

	template := browserTool
		classCreationTemplateForClass: normalClass
		hybridBrowser: false.
	normalClass := template evaluate.
	self assert: normalClass rowanPackageName = packageName1.
	self assert: normalClass category = packageName1.

	template := browserTool
		classCreationTemplateForClass: normalClass
		hybridBrowser: true.	" packageName does not equal category, so hyrbridBrowser form cannot be used"
	normalClass := template evaluate.
	self assert: normalClass rowanPackageName = packageName1.
	self assert: normalClass category = packageName1.

	normalClass category: packageName1.	"now we can use hybridBrowser form again"
	template := browserTool
		classCreationTemplateForClass: normalClass
		hybridBrowser: true.
	normalClass := template evaluate.
	self assert: normalClass rowanPackageName = packageName1.
	self assert: normalClass category = packageName1
]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridClassCreationWithClassCreationTemplateB [

	"https://github.com/dalehenrich/Rowan/issues/17"

	"make sure that we actually evaluate the class creation templates"

	| byteClass variableClass projectName packageNames packageName1 packageName2 browserTool template |
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
	browserTool := Rowan projectTools browser.

	template := browserTool
		classCreationTemplateForSubclassOf: 'Object'
		className: 'SimpleHybridByte1'
		type: 'byteSubclass'
		category: packageName1
		packageName: nil.
	byteClass := template evaluate.
	self assert: byteClass rowanPackageName = packageName1.
	self assert: byteClass category = packageName1.

	template := browserTool
		classCreationTemplateForSubclassOf: 'Object'
		className: 'SimpleHybridIndexable1'
		type: 'variable'
		category: packageName1
		packageName: packageName1.
	variableClass := template evaluate.
	self assert: variableClass rowanPackageName = packageName1.
	self assert: variableClass category = packageName1
]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridClassRename [

	|  normalClass projectName packageNames packageName1 packageName2 comment newClassName newClass className |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageName2 := 'HybridA-Extensions'.
	packageNames := {packageName1.
	packageName2}.
	comment := 'a comment'.
	className :=  'SimpleHybridNormal1'.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.

	normalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
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
	normalClass class
		rwCompileMethod: 'bar ^Bar'
		category: 'accessing'.
	normalClass class
		rwCompileMethod: 'baz ^baz'
		category: 'accessing'.
	normalClass rwComment: comment.

	self assert: (normalClass new perform: #foo) isNil.
	self assert: (normalClass perform: #bar) isNil.
	self assert: (normalClass perform: #baz) isNil.
	self assert: normalClass comment = comment.

	newClassName := 'SimpleHybridNormal1Copy'.
	newClass := Rowan projectTools browser renameClassNamed: className to: newClassName.

	self assert: (newClass new perform: #foo) isNil.
	self assert: (newClass perform: #bar) isNil.
	self assert: (newClass perform: #baz) isNil.
	self assert: newClass comment = comment.

	self assert: (Rowan globalNamed: className) isNil
]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridCompileMethod_01 [
	| normalClass projectName packageNames packageName1 packageName2 fooMethod barMethod normalInstance project audit |
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
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: (audit := project audit) isEmpty.
	self assert: normalClass rowanPackageName = packageName1.

	fooMethod := normalClass rwCompileMethod: 'foo ^''foo''' category: 'accessing'.
	self assert: (audit := project audit) isEmpty.

	barMethod := normalClass class
		rwCompileMethod: 'bar ^''bar'''
		category: '*' , packageName2
		packageName: packageName2.

	self assert: (audit := project audit) isEmpty.
	self assert: (normalClass perform: #'bar') = 'bar'.
	normalInstance := normalClass new.
	self assert: (normalInstance perform: #'foo') = 'foo'.

	self assert: fooMethod rowanPackageName = packageName1.
	self assert: barMethod rowanPackageName = packageName2
]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridCompileMethod_02 [
	| normalClass projectName packageNames packageName1 packageName2 fooMethod barMethod normalInstance project audit |
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
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: (audit := project audit) isEmpty.
	self assert: normalClass rowanPackageName = packageName1.

	fooMethod := normalClass rwCompileMethod: 'foo ^''foo''' category: 'accessing'.
	self assert: (audit := project audit) isEmpty.

	barMethod := normalClass class
		rwCompileMethod: 'bar ^''bar'''
		dictionaries: Rowan image symbolList
		category: '*' , packageName2
		packageName: packageName2.

	self assert: (audit := project audit) isEmpty.
	self assert: (normalClass perform: #'bar') = 'bar'.
	normalInstance := normalClass new.
	self assert: (normalInstance perform: #'foo') = 'foo'.

	self assert: fooMethod rowanPackageName = packageName1.
	self assert: barMethod rowanPackageName = packageName2
]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridCompileMethodMoveToProtocol [

	| normalClass projectName packageNames packageName1 packageName2 fooMethod barMethod normalInstance x |
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
	normalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: normalClass rowanPackageName = packageName1.

	fooMethod := normalClass
		rwCompileMethod: 'foo ^''foo'''
		category: '*' , packageName2 asLowercase.
	barMethod := normalClass class
		rwCompileMethod: 'bar ^''bar'''
		category: 'accessing'.

	self assert: (normalClass perform: #bar) = 'bar'.
	normalInstance := normalClass new.
	self assert: (normalInstance perform: #foo) = 'foo'.
	self
		assert:
			(x := normalClass class categoryOfSelector: #'bar') asString = 'accessing'.
	self assert: fooMethod rowanPackageName = packageName2.
	self assert: barMethod rowanPackageName = packageName1.

	barMethod := normalClass class
		rwCompileMethod: 'bar "" ^''bar'''
		category: 'new category'.

	self assert: (normalClass perform: #bar) = 'bar'.
	self
		assert:
			(x := normalClass class categoryOfSelector: #'bar') asString = 'new category'.
	self assert: barMethod rowanPackageName = packageName1
]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridCompileMethodRemove [

	| normalClass projectName packageNames packageName1 packageName2 fooMethod barMethod normalInstance |
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
	normalClass := Object
		rwSubclass: 'SimpleHybridNormal3'
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

	self assert: (normalClass perform: #bar) = 'bar'.
	normalInstance := normalClass new.
	self assert: (normalInstance perform: #foo) = 'foo'.

	self assert: fooMethod rowanPackageName = packageName1.
	self assert: barMethod rowanPackageName = packageName2.

	normalClass rwRemoveSelector: #'foo'.
	normalClass class rwRemoveSelector: #'bar'.

	self should: [ normalClass perform: #bar ] raise: MessageNotUnderstood.
	self should: [ normalInstance perform: #foo ] raise: MessageNotUnderstood.

	fooMethod := normalClass
		rwCompileMethod: 'foo ^''foo'''
		category: '*' , packageName2 asLowercase.
	barMethod := normalClass class
		rwCompileMethod: 'bar ^''bar'''
		category: 'accessing'.

	self assert: fooMethod rowanPackageName = packageName2.
	self assert: barMethod rowanPackageName = packageName1.

	self assert: (normalClass perform: #bar) = 'bar'.
	self assert: (normalInstance perform: #foo) = 'foo'.

	normalClass rwRemoveSelector: #'foo'.
	normalClass class rwRemoveSelector: #'bar'.

	self should: [ normalClass perform: #bar ] raise: MessageNotUnderstood.
	self should: [ normalInstance perform: #foo ] raise: MessageNotUnderstood
]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridComplicatedClassCopy [

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
	self assert: (testClass perform: #cvar1) = 2.
	self assert: (testClass perform: #civar1) = 1.
	self assert: (testClass perform: #fooClass) = 1.
	self assert: (testClass perform: #barClass) = 1.

	self assert: (testClass new perform: #ivar1) isNil.
	self assert: (testClass new perform: #foo) = 1.
	self assert: (testClass new perform: #bar) = 1.

	self assert: (testClass categoryOfSelector: #foo) = ( '*' , project1PackageName2 asLowercase) asSymbol.
	self assert: (testClass class categoryOfSelector: #fooClass) = ( '*' , project1PackageName2 asLowercase) asSymbol.
	self assert: (testClass categoryOfSelector: #bar) = ( '*' , project2PackageName2 asLowercase) asSymbol.
	self assert: (testClass class categoryOfSelector: #barClass) = ( '*' , project2PackageName2 asLowercase) asSymbol.

	newClass := Rowan projectTools browser copyClassNamed: className1 to: newClassName.
	newClass initialize.

	self assert: newClass == (Rowan globalNamed: newClassName).

	self assert: newClass ~~ testClass.
	self assert: (newClass perform: #cvar1) = 2.
	self assert: (newClass perform: #civar1) = 1.
	self assert: (newClass perform: #fooClass) = 1.
	self assert: (newClass perform: #barClass) = 1.

	self assert: (newClass new perform: #ivar1) isNil.
	self assert: (newClass new perform: #foo) = 1.
	self assert: (newClass new perform: #bar) = 1.

	self assert: (newClass categoryOfSelector: #foo) = ( '*' , project1PackageName2 asLowercase) asSymbol.
	self assert: (newClass class categoryOfSelector: #fooClass) = ( '*' , project1PackageName2 asLowercase) asSymbol.
	self assert: (newClass categoryOfSelector: #bar) = ( '*' , project2PackageName2 asLowercase) asSymbol.
	self assert: (newClass class categoryOfSelector: #barClass) = ( '*' , project2PackageName2 asLowercase) asSymbol.
]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridDeletePackage [

	"add 2 classes in 2 packages, extend each class from the other package ... remove the package"

	| normalClass projectName packageNames packageName1 packageName2 otherClass className1 className2 theLoadedProject theLoadedPackage theLoadedClassOrClassExtension classNames |
	className1 := 'SimpleHybridNormal1'.
	className2 := 'SimpleHybridOther1'.
	classNames := {className1.
	className2}.
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

	normalClass := Object
		rwSubclass: className1
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: normalClass rowanPackageName = packageName1.

	normalClass
		rwCompileMethod: 'foo ^''foo'''
		category: '*' , packageName2 asLowercase.
	self assert: (normalClass new perform: #foo) = 'foo'.

	otherClass := Object
		rwSubclass: className2
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName2
		options: #().
	self assert: otherClass rowanPackageName = packageName2.

	otherClass
		rwCompileMethod: 'foo ^''foo'''
		category: '*' , packageName1 asLowercase.
	self assert: (otherClass new perform: #foo) = 'foo'.

	Rowan projectTools browser removePackageNamed: packageName1.

	self assert: (Rowan globalNamed: className1) isNil.
	self assert: (Rowan globalNamed: className2) == otherClass.
	self should: [ otherClass new perform: #foo ] raise: MessageNotUnderstood.

	theLoadedProject := Rowan image loadedProjectNamed: projectName.	"Traverse the loaded thing structure and verify that it matches the expected structure"

	theLoadedProject
		loadedPackagesDo: [ :loadedProject :loadedPackage | 
			self assert: theLoadedProject == loadedProject.
			theLoadedPackage := loadedPackage.
			self
				assert:
					(Rowan image loadedPackageNamed: loadedPackage name) == loadedPackage.
			self assert: (packageNames includes: loadedPackage name) ]
		loadedClassedDo: [ :loadedProject :loadedPackage :loadedClass | 
			self assert: theLoadedProject == loadedProject.
			self assert: theLoadedPackage == loadedPackage.
			self
				assert:
					(Rowan image loadedPackageNamed: loadedPackage name) == loadedPackage.
			theLoadedClassOrClassExtension := loadedClass.

			loadedPackage name = packageName1
				ifTrue: [ 
					self
						assert: false
						description:
							'unexpected class ' , loadedClass name , ' in package ' , loadedPackage name ]
				ifFalse: [ 
					loadedPackage name = packageName2
						ifTrue: [ self assert: loadedClass name = className2 ]
						ifFalse: [ self assert: false description: 'unexpected package ' , loadedPackage name ] ] ]
		loadedClassExtensionsDo: [ :loadedProject :loadedPackage :loadedClassExtension | 
			self assert: false description: 'no class extensions expected in package '.
			loadedPackage name ]
		loadedInstanceMethodsDo: [ :loadedProject :loadedPackage :loadedClassOrClassExtension :loadedMethod | 
			self assert: false description: 'no instance methods expected in class'.
			loadedClassOrClassExtension name ]
		loadedClassMethodsDo: [ :loadedProject :loadedPackage :loadedClassOrClassExtension :loadedMethod | 
			self assert: false description: 'no class methods expected in class'.
			loadedClassOrClassExtension name ]
		loadedTraitsDo: [  :loadedProject :loadedPackage :loadedTrait |   ]
]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridModifyExtensionCompileMethod [

	| normalClass projectName packageNames packageName1 packageName2 fooMethod normalInstance protocol x barMethod |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageName2 := 'HybridA-Extensions'.
	packageNames := {packageName1.
	packageName2}.
	self
		_loadSessionMethodExtensionProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.

	normalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: normalClass rowanPackageName = packageName1.

	protocol := '*' , packageName2 asLowercase.

	fooMethod := normalClass rwCompileMethod: 'foo ^''foo''' category: protocol.
	barMethod := normalClass class
		rwCompileMethod: 'bar ^''bar'''
		category: protocol.

	self assert: normalClass bar = 'bar'.
	self
		assert:
			(x := normalClass class categoryOfSelector: #'bar') asString = protocol.
	self assert: barMethod rowanPackageName = packageName2.

	normalInstance := normalClass new.
	self assert: (normalInstance perform: #foo) = 'foo'.
	self assert: (x := normalClass categoryOfSelector: #'foo') asString = protocol.
	self assert: fooMethod rowanPackageName = packageName2.

	fooMethod := normalClass rwCompileMethod: 'foo ^''bar''' category: protocol.

	self assert: (x := normalInstance perform: #foo) = 'bar'.
	self assert: (x := normalClass categoryOfSelector: #'foo') asString = protocol.
	self assert: fooMethod rowanPackageName = packageName2.

	barMethod := normalClass class
		rwCompileMethod: 'bar ^''foo'''
		category: protocol.

	self assert: (x := normalClass perform: #bar) = 'foo'.
	self
		assert:
			(x := normalClass class categoryOfSelector: #'bar') asString = protocol.
	self assert: barMethod rowanPackageName = packageName2
]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridMoveClassToPackage [

	| normalClass projectName packageNames packageName1 packageName2 movedNormalClass |
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
	normalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: normalClass rowanPackageName = packageName1.

	movedNormalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName2
		options: #().
	self assert: movedNormalClass == normalClass.
	self assert: movedNormalClass rowanPackageName = packageName2
]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridMoveClassToPackageWithClassModifications [

	| normalClass projectName packageNames packageName1 packageName2 movedNormalClass |
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
	normalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: normalClass rowanPackageName = packageName1.

	movedNormalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #('ivar1')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName2
		options: #().
	self assert: movedNormalClass ~= normalClass.
	self assert: movedNormalClass rowanPackageName = packageName2

]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridMoveClassWithMethodsAndExtensionMethodsToExtensionPackage [

	| normalClass projectName packageNames packageName1 packageName2 packageName3 movedNormalClass fooMethod barMethod ext1Method ext2Method |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core1'.
	packageName2 := 'HybridA-Core2'.
	packageName3 := 'HybridA-Extensions'.
	packageNames := {packageName1. packageName2. packageName3}.
	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.

	normalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	fooMethod := normalClass
		rwCompileMethod: 'foo ^''foo'''
		category: 'accessing'.
	barMethod := normalClass class
		rwCompileMethod: 'bar ^''bar'''
		category: 'accessing'.
	ext1Method := normalClass
		rwCompileMethod: 'ext1 ^''ext1'''
		category: '*', packageName3 asLowercase.
	ext2Method := normalClass class
		rwCompileMethod: 'ext2 ^''ext2'''
		category: '*', packageName3 asLowercase.

	self assert: (normalClass new perform: #foo) = 'foo'.
	self assert: (normalClass new perform: #ext1) = 'ext1'.
	self assert: (normalClass perform: #bar) = 'bar'.
	self assert: (normalClass perform: #ext2) = 'ext2'.

	self assert: normalClass rowanPackageName = packageName1.
	self assert: fooMethod rowanPackageName = packageName1.
	self assert: barMethod rowanPackageName = packageName1.
	self assert: ext1Method rowanPackageName = packageName3.
	self assert: ext2Method rowanPackageName = packageName3.

	movedNormalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName2
		options: #().

	self assert: (normalClass new perform: #foo) = 'foo'.
	self assert: (normalClass new perform: #ext1) = 'ext1'.
	self assert: (normalClass perform: #bar) = 'bar'.
	self assert: (normalClass perform: #ext2) = 'ext2'.

	fooMethod := movedNormalClass compiledMethodAt: #foo.
	barMethod := movedNormalClass class compiledMethodAt: #bar.
	ext1Method := movedNormalClass compiledMethodAt: #ext1.
	ext2Method := movedNormalClass class compiledMethodAt: #ext2.

	self assert: movedNormalClass == normalClass.
	self assert: movedNormalClass rowanPackageName = packageName2.
	self assert: fooMethod rowanPackageName = packageName2.
	self assert: barMethod rowanPackageName = packageName2.
	self assert: ext1Method rowanPackageName = packageName3.
	self assert: ext2Method rowanPackageName = packageName3.
]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridMoveClassWithMethodsAndExtensionMethodsToPackage [

	| normalClass projectName packageNames packageName1 packageName2 packageName3 movedNormalClass fooMethod barMethod ext1Method ext2Method |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core1'.
	packageName2 := 'HybridA-Core2'.
	packageName3 := 'HybridA-Extensions'.
	packageNames := {packageName1. packageName2. packageName3}.
	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.

	normalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	fooMethod := normalClass
		rwCompileMethod: 'foo ^''foo'''
		category: 'accessing'.
	barMethod := normalClass class
		rwCompileMethod: 'bar ^''bar'''
		category: 'accessing'.
	ext1Method := normalClass
		rwCompileMethod: 'ext1 ^''ext1'''
		category: '*', packageName3 asLowercase.
	ext2Method := normalClass class
		rwCompileMethod: 'ext2 ^''ext2'''
		category: '*', packageName3 asLowercase.

	self assert: (normalClass new perform: #foo) = 'foo'.
	self assert: (normalClass new perform: #ext1) = 'ext1'.
	self assert: (normalClass perform: #bar) = 'bar'.
	self assert: (normalClass perform: #ext2) = 'ext2'.

	self assert: normalClass rowanPackageName = packageName1.
	self assert: fooMethod rowanPackageName = packageName1.
	self assert: barMethod rowanPackageName = packageName1.
	self assert: ext1Method rowanPackageName = packageName3.
	self assert: ext2Method rowanPackageName = packageName3.

	movedNormalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName2
		options: #().

	self assert: (normalClass new perform: #foo) = 'foo'.
	self assert: (normalClass new perform: #ext1) = 'ext1'.
	self assert: (normalClass perform: #bar) = 'bar'.
	self assert: (normalClass perform: #ext2) = 'ext2'.

	fooMethod := movedNormalClass compiledMethodAt: #foo.
	barMethod := movedNormalClass class compiledMethodAt: #bar.
	ext1Method := movedNormalClass compiledMethodAt: #ext1.
	ext2Method := movedNormalClass class compiledMethodAt: #ext2.

	self assert: movedNormalClass == normalClass.
	self assert: movedNormalClass rowanPackageName = packageName2.
	self assert: fooMethod rowanPackageName = packageName2.
	self assert: barMethod rowanPackageName = packageName2.
	self assert: ext1Method rowanPackageName = packageName3.
	self assert: ext2Method rowanPackageName = packageName3.
]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridMoveClassWithMethodsToPackage [

	| normalClass projectName packageNames packageName1 packageName2 movedNormalClass fooMethod barMethod |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core1'.
	packageName2 := 'HybridA-Core2'.
	packageNames := {packageName1.
	packageName2}.
	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.
	normalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	fooMethod := normalClass
		rwCompileMethod: 'foo ^''foo'''
		category: 'accessing'.
	barMethod := normalClass class
		rwCompileMethod: 'bar ^''bar'''
		category: 'accessing'.

	self assert: normalClass rowanPackageName = packageName1.
	self assert: fooMethod rowanPackageName = packageName1.
	self assert: barMethod rowanPackageName = packageName1.

	movedNormalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName2
		options: #().

	fooMethod := movedNormalClass compiledMethodAt: #foo.
	barMethod := movedNormalClass class compiledMethodAt: #bar.


	self assert: movedNormalClass == normalClass.
	self assert: movedNormalClass rowanPackageName = packageName2.
	self assert: fooMethod rowanPackageName = packageName2.
	self assert: barMethod rowanPackageName = packageName2.

]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridMoveMethod [

	|  normalClass projectName packageNames packageName1 packageName2 packageName3 className |
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

	normalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
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

	self assert: (normalClass categoryOfSelector: #foo) = ( '*' , packageName2 asLowercase) asSymbol.
	self assert: (normalClass categoryOfSelector: #bar) = #accessing.
	self assert: (normalClass class categoryOfSelector: #baz) = #accessing.

	Rowan projectTools browser
		moveMethod: #'foo' forClassNamed: 'SimpleHybridNormal1' isMeta: false toProtocol: 'accessing';
		moveMethod: #'bar' forClassNamed: 'SimpleHybridNormal1' isMeta: false toProtocol: ( '*' , packageName2 asLowercase);
		moveMethod: #'baz' forClassNamed: 'SimpleHybridNormal1' isMeta: true toProtocol: ( '*' , packageName2 asLowercase);
		yourself.

	self assert: (normalClass categoryOfSelector: #foo) = #accessing.
	self assert: (normalClass categoryOfSelector: #bar) = ( '*' , packageName2 asLowercase) asSymbol.
	self assert: (normalClass class categoryOfSelector: #baz) = ( '*' , packageName2 asLowercase) asSymbol.

	Rowan projectTools browser
		moveMethod: #'foo' forClassNamed: 'SimpleHybridNormal1' isMeta: false toProtocol: ( '*' , packageName3 asLowercase);
		moveMethod: #'bar' forClassNamed: 'SimpleHybridNormal1' isMeta: false toProtocol: ( '*' , packageName3 asLowercase);
		moveMethod: #'baz' forClassNamed: 'SimpleHybridNormal1' isMeta: true toProtocol: ( '*' , packageName3 asLowercase);
		yourself.

	self assert: (normalClass categoryOfSelector: #foo) = ( '*' , packageName3 asLowercase) asSymbol.
	self assert: (normalClass categoryOfSelector: #bar) = ( '*' , packageName3 asLowercase) asSymbol.
	self assert: (normalClass class categoryOfSelector: #baz) = ( '*' , packageName3 asLowercase) asSymbol.

]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridMoveMethod_185 [

	"tighten down on the bare minimum set of changes needed to reproduce an #185 issue exposed byt RwBrowserTooApiTest>>testMoveMethod"

	|  normalClass projectName packageNames packageName1 packageName2 packageName3 className fooMethod |
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

	normalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #(foo)
		classVars: #(Bar)
		classInstVars: #(baz)
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: normalClass rowanPackageName = packageName1.

	fooMethod := normalClass
		rwCompileMethod: 'foo ^foo'
		category: '*' , packageName2 asLowercase.

	self assert: (normalClass categoryOfSelector: #foo) = ( '*' , packageName2 asLowercase) asSymbol.
	self assert: fooMethod rowanPackageName = packageName2.

	fooMethod := normalClass
		rwCompileMethod: 'foo ^foo'
		category: '*' , packageName3 asLowercase.

	self assert: (normalClass categoryOfSelector: #foo) = ( '*' , packageName3 asLowercase) asSymbol.
	self assert: fooMethod rowanPackageName = packageName3.

	fooMethod := normalClass
		rwCompileMethod: 'foo ^foo'
		category: '*' , packageName2 asLowercase.

	self assert: (normalClass categoryOfSelector: #foo) = ( '*' , packageName2 asLowercase) asSymbol.
	self assert: fooMethod rowanPackageName = packageName2.

]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridMoveMethod_simple_A [
	"move a method from the package of the class to an extension package"

	|  normalClass projectName packageNames packageName1 packageName2 className audit project |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageName2 := 'HybridA-Extensions1'.
	packageNames := {packageName1.
	packageName2.}.
	className :=  'SimpleHybridNormal1'.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.

	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.

	normalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #(foo)
		classVars: #(Bar)
		classInstVars: #(baz)
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: normalClass rowanPackageName = packageName1.

	normalClass
		rwCompileMethod: 'foo ^foo'
		category: 'accessing'.
	self assert: (audit := project audit) isEmpty.

	Rowan projectTools browser
		moveMethod: #'foo' forClassNamed: 'SimpleHybridNormal1' isMeta: false toProtocol: '*' , packageName2 asLowercase;
		yourself.
	self assert: (audit := project audit) isEmpty.
]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridMoveMethod_simple_B [
	"move an extension method from extension package into the package of the class"

	|  normalClass projectName packageNames packageName1 packageName2 className audit project |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageName2 := 'HybridA-Extensions1'.
	packageNames := {packageName1.
	packageName2.}.
	className :=  'SimpleHybridNormal1'.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.

	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.

	normalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #(foo)
		classVars: #(Bar)
		classInstVars: #(baz)
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: normalClass rowanPackageName = packageName1.

	normalClass
		rwCompileMethod: 'foo ^foo'
		category:  '*' , packageName2 asLowercase.
	self assert: (audit := project audit) isEmpty.

	Rowan projectTools browser
		moveMethod: #'foo' forClassNamed: 'SimpleHybridNormal1' isMeta: false toProtocol: 'accessing';
		yourself.
	self assert: (audit := project audit) isEmpty.
]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridMoveMethodFromSessionMethodsIntoSessionMethods [

	|  normalClass projectName packageNames packageName1 packageName2 packageName3 className |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageName2 := 'HybridA-Extensions1'.
	packageName3 := 'HybridA-Extensions2'.
	packageNames := {packageName1.
	packageName2.
	packageName3}.
	className :=  'SimpleHybridNormal1'.

	self
		_loadSessionMethodExtensionProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.

	normalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
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

	self assert: (normalClass categoryOfSelector: #foo) = ( '*' , packageName2 asLowercase) asSymbol.
	self assert: (normalClass categoryOfSelector: #bar) = #accessing.
	self assert: (normalClass class categoryOfSelector: #baz) = #accessing.

	Rowan projectTools browser
		moveMethod: #'bar' forClassNamed: 'SimpleHybridNormal1' isMeta: false toProtocol: ( '*' , packageName2 asLowercase);
		moveMethod: #'baz' forClassNamed: 'SimpleHybridNormal1' isMeta: true toProtocol: ( '*' , packageName2 asLowercase);
		moveMethod: #'foo' forClassNamed: 'SimpleHybridNormal1' isMeta: false toProtocol: 'accessing';
		yourself.

	self assert: (normalClass categoryOfSelector: #foo) = #accessing.
	self assert: (normalClass categoryOfSelector: #bar) = ( '*' , packageName2 asLowercase) asSymbol.
	self assert: (normalClass class categoryOfSelector: #baz) = ( '*' , packageName2 asLowercase) asSymbol.

	Rowan projectTools browser
		moveMethod: #'bar' forClassNamed: 'SimpleHybridNormal1' isMeta: false toProtocol: ( '*' , packageName3 asLowercase);
		moveMethod: #'baz' forClassNamed: 'SimpleHybridNormal1' isMeta: true toProtocol: ( '*' , packageName3 asLowercase);
		moveMethod: #'foo' forClassNamed: 'SimpleHybridNormal1' isMeta: false toProtocol: ( '*' , packageName3 asLowercase);
		yourself.

	self assert: (normalClass categoryOfSelector: #foo) = ( '*' , packageName3 asLowercase) asSymbol.
	self assert: (normalClass categoryOfSelector: #bar) = ( '*' , packageName3 asLowercase) asSymbol.
	self assert: (normalClass class categoryOfSelector: #baz) = ( '*' , packageName3 asLowercase) asSymbol.

]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridMoveMethodIntoSessionMethods [

	|  normalClass projectName packageNames packageName1 packageName2 className |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageName2 := 'HybridA-Extensions'.
	packageNames := {packageName1.
	packageName2}.
	className :=  'SimpleHybridNormal1'.

	self
		_loadSessionMethodExtensionProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.

	normalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
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

	self assert: (normalClass categoryOfSelector: #foo) = ( '*' , packageName2 asLowercase) asSymbol.
	self assert: (normalClass categoryOfSelector: #bar) = #accessing.
	self assert: (normalClass class categoryOfSelector: #baz) = #accessing.

	Rowan projectTools browser
		moveMethod: #'bar' forClassNamed: 'SimpleHybridNormal1' isMeta: false toProtocol: ( '*' , packageName2 asLowercase);
		moveMethod: #'baz' forClassNamed: 'SimpleHybridNormal1' isMeta: true toProtocol: ( '*' , packageName2 asLowercase);
		moveMethod: #'foo' forClassNamed: 'SimpleHybridNormal1' isMeta: false toProtocol: 'accessing';
		yourself.

	self assert: (normalClass categoryOfSelector: #foo) = #accessing.
	self assert: (normalClass categoryOfSelector: #bar) = ( '*' , packageName2 asLowercase) asSymbol.
	self assert: (normalClass class categoryOfSelector: #baz) = ( '*' , packageName2 asLowercase) asSymbol.

]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridNewClassVersionWithSubclassesA [

	| class subclass projectName packageNames packageName1 newClassVersion newSubclassVersion audit |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageNames := {packageName1}.
	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.
	class := Object
		rwSubclass: 'SimpleHybrid1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	subclass := class
		rwSubclass: 'SimpleHybridSubclass1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: subclass superClass == class.

	newClassVersion := Object
		rwSubclass: 'SimpleHybrid1'
		instVarNames: #('ivar1')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	audit := (Rowan projectNamed: projectName) audit.
	self assert: audit isEmpty.
	self assert: newClassVersion ~~ class.
	newSubclassVersion := Rowan globalNamed: subclass name.
	self assert: newSubclassVersion ~~ subclass.
	self assert: newSubclassVersion superClass == newClassVersion
]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridNewClassVersionWithSubclassesAndMethodsA [
	"we've been illegally moving methods into extension methods in the same package as the class... should throw errors"

	"https://github.com/GemTalk/Rowan/issues/802"

	| class  projectName packageNames packageName1 hadError |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageNames := {packageName1}.
	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.
	class := Object
		rwSubclass: 'SimpleHybrid1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	hadError := false.
	[class rwCompileMethod: 'foo ^''foo''' category: '*' , packageName1 asLowercase] 
		on: Error do: [:ex |
			hadError := true.
			self 
				assert: ex description 
				equals:  'UserDefinedError: extension category name ''*hybrida-core''  must not match class package name (HybridA-Core)' ].
	self assert: hadError description: 'Expected an error'

]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridNewClassVersionWithSubclassesAndMethodsB [

	| class subclass projectName packageNames packageName1 newClassVersion newSubclassVersion audit |
	projectName := 'Hybrid Project B'.
	packageName1 := 'HybridB-Core'.
	packageNames := {packageName1}.
	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.
	class := Object
		rwSubclass: 'SimpleHybrid1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	class rwCompileMethod: 'foo ^''foo''' category: 'accessing'.
	class class rwCompileMethod: 'foo ^''foo''' category: 'accessing'.

	audit := (Rowan projectNamed: projectName) audit.
	self assert: audit isEmpty.

	subclass := class
		rwSubclass: 'SimpleHybridSubclass1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	subclass rwCompileMethod: 'foo1 ^''foo''' category: 'accessing'.
	subclass class rwCompileMethod: 'foo1 ^''foo''' category: 'accessing'.

	self assert: subclass superClass == class.
	audit := (Rowan projectNamed: projectName) audit.
	self assert: audit isEmpty.

	newClassVersion := Object
		rwSubclass: 'SimpleHybrid1'
		instVarNames: #('ivar1')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	audit := (Rowan projectNamed: projectName) audit.
	self assert: audit isEmpty.
	self assert: newClassVersion ~~ class.
	newSubclassVersion := Rowan globalNamed: subclass name.
	self assert: newSubclassVersion ~~ subclass.
	self assert: newSubclassVersion superClass == newClassVersion.
	self assert: (newSubclassVersion perform: #foo) = 'foo'.
	self assert: (newSubclassVersion perform: #foo1) = 'foo'.
	self assert: (newSubclassVersion new perform: #foo) = 'foo'.
	self assert: (newSubclassVersion new perform: #foo1) = 'foo'
]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridNewClassVersionWithSubclassesAndMethodsC [

	| class subclass projectName packageNames packageName1 packageName2 newClassVersion newSubclassVersion audit |
	projectName := 'Hybrid Project C'.
	packageName1 := 'HybridC-Core'.
	packageName2 := 'HybridC-Extensions'.
	packageNames := {packageName1.
	packageName2}.
	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.
	class := Object
		rwSubclass: 'SimpleHybrid1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	class rwCompileMethod: 'foo ^''foo''' category: '*' , packageName2 asLowercase.
	class class rwCompileMethod: 'foo ^''foo''' category: 'accessing'.

	audit := (Rowan projectNamed: projectName) audit.
	self assert: audit isEmpty.

	subclass := class
		rwSubclass: 'SimpleHybridSubclass1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	subclass rwCompileMethod: 'foo1 ^''foo''' category: 'accessing'.
	subclass class
		rwCompileMethod: 'foo1 ^''foo'''
		category: '*' , packageName2 asLowercase.

	self assert: subclass superClass == class.
	audit := (Rowan projectNamed: projectName) audit.
	self assert: audit isEmpty.

	newClassVersion := Object
		rwSubclass: 'SimpleHybrid1'
		instVarNames: #('ivar1')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	audit := (Rowan projectNamed: projectName) audit.
	self assert: audit isEmpty.
	self assert: newClassVersion ~~ class.
	newSubclassVersion := Rowan globalNamed: subclass name.
	self assert: newSubclassVersion ~~ subclass.
	self assert: newSubclassVersion superClass == newClassVersion.
	self assert: (newSubclassVersion perform: #foo) = 'foo'.
	self assert: (newSubclassVersion perform: #foo1) = 'foo'.
	self assert: (newSubclassVersion new perform: #foo) = 'foo'.
	self assert: (newSubclassVersion new perform: #foo1) = 'foo'
]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridNewClassVersionWithSubclassesAndMethodsD [
	"Create new class version of class in new package ... where the class already has an 
		extension method for that package. Recompile succeeds, but audit fails ... recompile 
		should throw the error"

	"https://github.com/GemTalk/Rowan/issues/802"

	| class subclass projectName packageNames packageName1 packageName2 newClassVersion audit hadError |
	projectName := 'Hybrid Project C'.
	packageName1 := 'HybridC-Core'.
	packageName2 := 'HybridC-Extensions'.
	packageNames := {packageName1.
	packageName2}.
	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.
	class := Object
		rwSubclass: 'SimpleHybrid1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	class rwCompileMethod: 'foo ^''foo''' category: '*' , packageName2 asLowercase.
	class class rwCompileMethod: 'foo ^''foo''' category: 'accessing'.

	audit := (Rowan projectNamed: projectName) audit.
	self assert: audit isEmpty.

	subclass := class
		rwSubclass: 'SimpleHybridSubclass1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName2
		options: #().
	subclass rwCompileMethod: 'foo1 ^''foo''' category: 'accessing'.
	subclass class
		rwCompileMethod: 'foo1 ^''foo'''
		category: '*' , packageName1 asLowercase.

	audit := (Rowan projectNamed: projectName) audit.
	self assert: audit isEmpty.	self assert: subclass superClass == class.

	hadError := false.
	[
		newClassVersion := Object
			rwSubclass: 'SimpleHybrid1'
			instVarNames: #('ivar1')
			classVars: #()
			classInstVars: #()
			poolDictionaries: #()
			category: packageName2
			options: #()
	] on: Error do: [:ex |
		self 
			assert: ex description 
			equals: 'UserDefinedError: The class SimpleHybrid1 has extension methods for the package HybridC-Extensions, so the class cannot be added to the package HybridC-Extensions without violating the restriction that extension category names must not match the class package name.'.
		hadError := true ].
	self assert: hadError description: 'Expected and error'.
]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridNewClassVersionWithSubclassesAndMethodsE [
	"Create new class version of class in new package ... where the class already has an 
		extension method for that package. Recompile succeeds, but audit fails ... recompile 
		should throw the error"

	"https://github.com/GemTalk/Rowan/issues/802"

	| class subclass projectName packageNames packageName1 packageName2 newClassVersion audit hadError |
	projectName := 'Hybrid Project C'.
	packageName1 := 'HybridC-Core'.
	packageName2 := 'HybridC-Extensions'.
	packageNames := {packageName1.
	packageName2}.
	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.
	class := Object
		rwSubclass: 'SimpleHybrid1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	class rwCompileMethod: 'foo ^''foo''' category: '*' , packageName2 asLowercase.
	class class rwCompileMethod: 'foo ^''foo''' category: 'accessing'.

	audit := (Rowan projectNamed: projectName) audit.
	self assert: audit isEmpty.

	subclass := class
		rwSubclass: 'SimpleHybridSubclass1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName2
		options: #().
	subclass rwCompileMethod: 'foo1 ^''foo''' category: 'accessing'.
	subclass class
		rwCompileMethod: 'foo1 ^''foo'''
		category: '*' , packageName1 asLowercase.

	audit := (Rowan projectNamed: projectName) audit.
	self assert: audit isEmpty.	self assert: subclass superClass == class.

	hadError := false.
	[
		newClassVersion := Object
			rwSubclass: 'SimpleHybrid1'
			instVarNames: #('ivar1')
			classVars: #()
			classInstVars: #()
			poolDictionaries: #()
			category: packageName2
			packageName: packageName2
			options: #()
	] on: Error do: [:ex |
		self 
			assert: ex description 
			equals: 'UserDefinedError: The class SimpleHybrid1 has extension methods for the package HybridC-Extensions, so the class cannot be added to the package HybridC-Extensions without violating the restriction that extension category names must not match the class package name.'.
		hadError := true ].
	self assert: hadError description: 'Expected and error'.
]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridNewClassVersionWithSubclassesB [

	| class subclass1 subclass2 subclass3 subclass4 projectName packageNames packageName1 newClassVersion newSubclassVersion1 newSubclassVersion2 newSubclassVersion3 newSubclassVersion4 audit |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageNames := {packageName1}.
	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.
	class := Object
		rwSubclass: 'SimpleHybrid1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	subclass1 := class
		rwSubclass: 'SimpleHybridSubclass1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: subclass1 superClass == class.

	subclass2 := subclass1
		rwSubclass: 'SimpleHybridSubclass2'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: subclass2 superClass == subclass1.

	subclass3 := subclass2
		rwSubclass: 'SimpleHybridSubclass3'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: subclass3 superClass == subclass2.

	subclass4 := subclass3
		rwSubclass: 'SimpleHybridSubclass4'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: subclass4 superClass == subclass3.

	newClassVersion := Object
		rwSubclass: 'SimpleHybrid1'
		instVarNames: #('ivar1')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	audit := (Rowan projectNamed: projectName) audit.
	self assert: audit isEmpty.
	self assert: newClassVersion ~~ class.
	newSubclassVersion1 := Rowan globalNamed: subclass1 name.
	self assert: newSubclassVersion1 ~~ subclass1.
	self assert: newSubclassVersion1 superClass == newClassVersion.

	newSubclassVersion2 := Rowan globalNamed: subclass2 name.
	self assert: newSubclassVersion2 ~~ subclass2.
	self assert: newSubclassVersion2 superClass == newSubclassVersion1.

	newSubclassVersion3 := Rowan globalNamed: subclass3 name.
	self assert: newSubclassVersion3 ~~ subclass3.
	self assert: newSubclassVersion3 superClass == newSubclassVersion2.

	newSubclassVersion4 := Rowan globalNamed: subclass4 name.
	self assert: newSubclassVersion4 ~~ subclass4.
	self assert: newSubclassVersion4 superClass == newSubclassVersion3
]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridRemoveCategory [

	|  normalClass projectName packageNames packageName1 packageName2 className |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageName2 := 'HybridA-Extensions1'.
	packageNames := {packageName1.
	packageName2}.
	className :=  'SimpleHybridNormal1'.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.

	normalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #(foo fah)
		classVars: #(Bar)
		classInstVars: #(baz)
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: normalClass rowanPackageName = packageName1.

	normalClass
		rwCompileMethod: 'foo1 ^foo'
		category: '*' , packageName2 asLowercase.
	normalClass
		rwCompileMethod: 'foo2 ^foo'
		category: '*' , packageName2 asLowercase.
	normalClass class
		rwCompileMethod: 'fah1 ^baz'
		category: '*' , packageName2 asLowercase.
	normalClass class
		rwCompileMethod: 'fah2 ^Bar'
		category: '*' , packageName2 asLowercase.
	normalClass
		rwCompileMethod: 'bar1 ^Bar'
		category: 'accessing'.
	normalClass class
		rwCompileMethod: 'bar2 ^baz'
		category: 'accessing'.
	normalClass
		rwCompileMethod: 'bee1 ^Bar'
		category: 'accessing'.
	normalClass class
		rwCompileMethod: 'bee2 ^baz'
		category: 'accessing'.

	self assert: (normalClass categoryOfSelector: #foo1) = ( '*' , packageName2 asLowercase) asSymbol.
	self assert: (normalClass categoryOfSelector: #foo2) = ( '*' , packageName2 asLowercase) asSymbol.
	self assert: (normalClass class categoryOfSelector: #fah1) = ( '*' , packageName2 asLowercase) asSymbol.
	self assert: (normalClass class categoryOfSelector: #fah2) = ( '*' , packageName2 asLowercase) asSymbol.
	self assert: (normalClass categoryOfSelector: #bar1) = #accessing.
	self assert: (normalClass class categoryOfSelector: #bar2) = #accessing.
	self assert: (normalClass categoryOfSelector: #bee1) = #accessing.
	self assert: (normalClass class categoryOfSelector: #bee2) = #accessing.

	self assert: normalClass categoryNames size = 2.
	self assert: normalClass class categoryNames size = 2.

	normalClass
		rwRemoveCategory: 'accessing';
		rwRemoveCategory: ( '*' , packageName2 asLowercase);
		yourself.

	normalClass class
		rwRemoveCategory: 'accessing';
		rwRemoveCategory: ( '*' , packageName2 asLowercase);
		yourself.

	self assert: normalClass categoryNames size = 0.
	self assert: normalClass class categoryNames size = 0.

]
