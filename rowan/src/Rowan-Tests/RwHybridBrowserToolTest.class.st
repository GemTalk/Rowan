Class {
	#name : 'RwHybridBrowserToolTest',
	#superclass : 'RwBrowserToolTest',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwHybridBrowserToolTest >> _standardProjectDefinition: projectName packageNameMap: packageNameMap defaultSymbolDictName: defaultSymbolDictName [

	| projectDefinition |
	projectDefinition := RwProjectDefinition
		newForGitBasedProjectNamed: projectName.
	packageNameMap keysAndValuesDo: [:packageName :symDictName |
		projectDefinition
			addPackageNamed: packageName;
			setSymbolDictName: symDictName forPackageNamed: packageName
			yourself ].
	projectDefinition defaultSymbolDictName: defaultSymbolDictName.

	^ projectDefinition

]

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

	self assert: normalClass new foo isNil.
	self assert: normalClass bar isNil.
	self assert: normalClass baz isNil.
	self assert: normalClass comment = comment.

	newClassName := 'SimpleHybridNormal1Copy'.
	newClass := Rowan projectTools browser copyClassNamed: className to: newClassName.

	self assert: newClass new foo isNil.
	self assert: newClass bar isNil.
	self assert: newClass baz isNil.
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
RwHybridBrowserToolTest >> testHybridClassCreationWithClassCreationTemplate_292 [

	"https://github.com/dalehenrich/Rowan/issues/292"

	|  class projectName packageNames packageName1 template expectedTemplate oldClass |
	projectName := 'Hybrid Project A'.
	packageName1 := 'HybridA-Core'.
	packageNames := {packageName1}.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.
"1 create class - indexable"
	class := Object
		rwIndexableSubclass: 'TestVariableClass'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	template := Rowan projectTools browser
		classCreationTemplateForClass: class
		hybridBrowser: true.
	expectedTemplate := 'Object rwIndexableSubclass: ''TestVariableClass''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	category: ', packageName1 printString, '
	options: #()
'.
	self assert: template = expectedTemplate.

"2 create subclass - indexable"
	oldClass := class.
	class := oldClass
		rwIndexableSubclass: 'TestVariableSubclass'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	template := Rowan projectTools browser
		classCreationTemplateForClass: class
		hybridBrowser: true.
	expectedTemplate := 'TestVariableClass rwSubclass: ''TestVariableSubclass''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	category: ', packageName1 printString, '
	options: #()
'.
	self assert: template = expectedTemplate.

"1 create class - bytes"
	class := Object
		rwByteSubclass: 'TestByteClass'
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	template := Rowan projectTools browser
		classCreationTemplateForClass: class
		hybridBrowser: true.
	expectedTemplate := 'Object rwByteSubclass: ''TestByteClass''
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	category: ', packageName1 printString, '
	options: #()
'.
	self assert: template = expectedTemplate.

"2 create subclass - bytes"
	oldClass := class.
	class := oldClass
		rwSubclass: 'TestByteSubclass'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	template := Rowan projectTools browser
		classCreationTemplateForClass: class
		hybridBrowser: true.
	expectedTemplate := 'TestByteClass rwSubclass: ''TestByteSubclass''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	category: ', packageName1 printString, '
	options: #()
'.
	self assert: template = expectedTemplate.

"1 create class - disallowGciStore"
	class := Error
		rwSubclass: 'TestErrorClass'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	template := Rowan projectTools browser
		classCreationTemplateForClass: class
		hybridBrowser: true.
	expectedTemplate := 'Error rwSubclass: ''TestErrorClass''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	category: ', packageName1 printString, '
	options: #()
'.
	self assert: template = expectedTemplate.

"1 create class - traverseByCallback"
	class := RcIdentityBag
		rwSubclass: 'TestRcIdentityBagClass'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	template := Rowan projectTools browser
		classCreationTemplateForClass: class
		hybridBrowser: true.
	expectedTemplate := 'RcIdentityBag rwSubclass: ''TestRcIdentityBagClass''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	category: ', packageName1 printString, '
	options: #()
'.
	self assert: template = expectedTemplate.

]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridClassCreationWithClassCreationTemplateA [

	"https://github.com/dalehenrich/Rowan/issues/17"

	"make sure that we actually evaluate the class creation templates"

	| category normalClass projectName packageNames packageName1 packageName2 browserTool template |
	category := 'Category'.
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
		category: category
		packageName: packageName1.
	normalClass := template evaluate.
	self assert: normalClass rowanPackageName = packageName1.
	self assert: normalClass category = category.

	template := browserTool
		classCreationTemplateForClass: normalClass
		hybridBrowser: false.
	normalClass := template evaluate.
	self assert: normalClass rowanPackageName = packageName1.
	self assert: normalClass category = category.

	template := browserTool
		classCreationTemplateForClass: normalClass
		hybridBrowser: true.	" packageName does not equal category, so hyrbridBrowser form cannot be used"
	normalClass := template evaluate.
	self assert: normalClass rowanPackageName = packageName1.
	self assert: normalClass category = category.

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

	| category byteClass variableClass projectName packageNames packageName1 packageName2 browserTool template |
	category := 'Category'.
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
		type: 'bytes'
		category: packageName1
		packageName: nil.
	byteClass := template evaluate.
	self assert: byteClass rowanPackageName = packageName1.
	self assert: byteClass category = packageName1.

	template := browserTool
		classCreationTemplateForSubclassOf: 'Object'
		className: 'SimpleHybridIndexable1'
		type: 'variable'
		category: category
		packageName: packageName1.
	variableClass := template evaluate.
	self assert: variableClass rowanPackageName = packageName1.
	self assert: variableClass category = category
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
		category: '*' , packageName1 asLowercase.
	normalClass class
		rwCompileMethod: 'bar ^Bar'
		category: 'accessing'.
	normalClass class
		rwCompileMethod: 'baz ^baz'
		category: 'accessing'.
	normalClass rwComment: comment.

	self assert: normalClass new foo isNil.
	self assert: normalClass bar isNil.
	self assert: normalClass baz isNil.
	self assert: normalClass comment = comment.

	newClassName := 'SimpleHybridNormal1Copy'.
	newClass := Rowan projectTools browser renameClassNamed: className to: newClassName.

	self assert: newClass new foo isNil.
	self assert: newClass bar isNil.
	self assert: newClass baz isNil.
	self assert: newClass comment = comment.

	self assert: (Rowan globalNamed: className) isNil

]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridCompileMethod [

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
		category: '*' , packageName1 asLowercase.
	barMethod := normalClass class
		rwCompileMethod: 'bar ^''bar'''
		category: 'accessing'
		packageName: packageName2.

	self assert: normalClass bar = 'bar'.
	normalInstance := normalClass new.
	self assert: normalInstance foo = 'foo'.

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
		category: '*' , packageName1 asLowercase.
	barMethod := normalClass class
		rwCompileMethod: 'bar ^''bar'''
		category: 'accessing'.

	self assert: normalClass bar = 'bar'.
	normalInstance := normalClass new.
	self assert: normalInstance foo = 'foo'.
	self
		assert:
			(x := normalClass class categoryOfSelector: #'bar') asString = 'accessing'.
	self assert: fooMethod rowanPackageName = packageName1.
	self assert: barMethod rowanPackageName = packageName1.

	barMethod := normalClass class
		rwCompileMethod: 'bar "" ^''bar'''
		category: 'new category'.

	self assert: normalClass bar = 'bar'.
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

	self assert: normalClass bar = 'bar'.
	normalInstance := normalClass new.
	self assert: normalInstance foo = 'foo'.

	self assert: fooMethod rowanPackageName = packageName1.
	self assert: barMethod rowanPackageName = packageName2.

	normalClass rwRemoveSelector: #'foo'.
	normalClass class rwRemoveSelector: #'bar'.

	self should: [ normalClass bar ] raise: MessageNotUnderstood.
	self should: [ normalInstance foo ] raise: MessageNotUnderstood.

	fooMethod := normalClass
		rwCompileMethod: 'foo ^''foo'''
		category: '*' , packageName2 asLowercase.
	barMethod := normalClass class
		rwCompileMethod: 'bar ^''bar'''
		category: 'accessing'.

	self assert: fooMethod rowanPackageName = packageName2.
	self assert: barMethod rowanPackageName = packageName1.

	self assert: normalClass bar = 'bar'.
	self assert: normalInstance foo = 'foo'.

	normalClass rwRemoveSelector: #'foo'.
	normalClass class rwRemoveSelector: #'bar'.

	self should: [ normalClass bar ] raise: MessageNotUnderstood.
	self should: [ normalInstance foo ] raise: MessageNotUnderstood

]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridCompileMethodRemove_315 [

	"https://github.com/dalehenrich/Rowan/issues/315"

	| normalClass projectName packageNames packageName1 packageName2 fooMethod barMethod normalInstance
		symDict registry |
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

"add methods"
	fooMethod := normalClass rwCompileMethod: 'foo ^''foo''' category: 'accessing'.
	barMethod := normalClass class
		rwCompileMethod: 'bar ^''bar'''
		category: '*' , packageName2 asLowercase.

"validate"
	self assert: normalClass bar = 'bar'.
	normalInstance := normalClass new.
	self assert: normalInstance foo = 'foo'.

	self assert: fooMethod rowanPackageName = packageName1.
	self assert: barMethod rowanPackageName = packageName2.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 1.
	self assert: registry methodRegistry size = 2.

"remove methods"
	normalClass rwRemoveSelector: #'foo'.
	normalClass class rwRemoveSelector: #'bar'.

"validate"
	self should: [ normalClass bar ] raise: MessageNotUnderstood.
	self should: [ normalInstance foo ] raise: MessageNotUnderstood.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

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

	self assert: normalClass bar = 'bar'.
	self assert: normalInstance foo = 'foo'.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 1.
	self assert: registry methodRegistry size = 2.

"remove methods"
	normalClass rwRemoveSelector: #'foo'.
	normalClass class rwRemoveSelector: #'bar'.

"validate"
	self should: [ normalClass bar ] raise: MessageNotUnderstood.
	self should: [ normalInstance foo ] raise: MessageNotUnderstood.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

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

]

{ #category : 'tests - packages' }
RwHybridBrowserToolTest >> testHybridComplicatedProjectLoad [

	"Write project to disk, make a few modifications and then reload the project from disk"

	| normalClass1 normalClass2 projectName packageNames packageName1 packageName2 normalInstance1 normalInstance2 projectTools className1 className2 theLoadedProject theLoadedPackage theLoadedClassOrClassExtension writtenStateValidationBlock classNames oldNormalClass2 |
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
		root: '/tmp/rowanHybridPatchProject_01/'.
	className1 := 'SimpleHybridNormalReload1'.
	className2 := 'SimpleHybridNormalReload2'.
	classNames := {className1.
	className2}.

	normalClass1 := Object
		rwSubclass: className1
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: normalClass1 rowanPackageName = packageName1.

	normalClass1 
		rwCompileMethod:  'foo ^''foo''' 
		category: 'accessing'.
	normalClass1 class
		rwCompileMethod: 'bar ^''bar'''
		category: '*' , packageName2 asLowercase.

	self assert: normalClass1 bar = 'bar'.
	normalInstance1 := normalClass1 new.
	self assert: normalInstance1 foo = 'foo'.

	normalClass2 := Object
		rwSubclass: className2
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: normalClass2 rowanPackageName = packageName1.

	normalInstance2 := normalClass2 new.
	self should: [ normalInstance2 biz ] raise: MessageNotUnderstood.
	self should: [ normalInstance2 biff ] raise: MessageNotUnderstood.

	projectTools := Rowan projectTools.
	projectTools spec exportProjectNamed: projectName.
	projectTools write writeProjectNamed: projectName.
	projectTools commit
		commitProjectNamed: projectName
		message:
			'Added SimpleHybridNormalReload1 and SimpleHybridNormalReload2 classes and extension methods'.

	writtenStateValidationBlock := [ 
	theLoadedProject := Rowan image loadedProjectNamed: projectName.	"Traverse the loaded package structure and verify that it matches the original expected structure"

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
					loadedClass name = className1
						ifTrue: [ self assert: (loadedClass propertyAt: 'instvars') isEmpty ]
						ifFalse: [ 
							loadedClass name = className2
								ifTrue: [ self assert: (loadedClass propertyAt: 'instvars') isEmpty ]
								ifFalse: [ 
									self
										assert: false
										description:
											'Unexpected loaded class ' , loadedClass name printString , ' in '
												, packageName2 ] ] ]
				ifFalse: [ self assert: false description: 'No class expected in ' , packageName2 ] ]
		loadedClassExtensionsDo: [ :loadedProject :loadedPackage :loadedClassExtension | 
			self assert: theLoadedProject == loadedProject.
			self assert: theLoadedPackage == loadedPackage.
			self
				assert:
					(Rowan image loadedPackageNamed: loadedPackage name) == loadedPackage.
			theLoadedClassOrClassExtension := loadedClassExtension.

			loadedPackage name = packageName1
				ifTrue: [ 
					self
						assert: false
						description: 'no classes expected in ' , packageName1 printString ].
			loadedPackage name = packageName2
				ifTrue: [ self assert: (classNames includes: loadedClassExtension name) ] ]
		loadedInstanceMethodsDo: [ :loadedProject :loadedPackage :loadedClassOrClassExtension :loadedMethod | 
			self assert: theLoadedProject == loadedProject.
			self assert: theLoadedPackage == loadedPackage.
			self
				assert:
					(Rowan image loadedPackageNamed: loadedPackage name) == loadedPackage.
			self assert: theLoadedClassOrClassExtension == loadedClassOrClassExtension.

			loadedPackage name = packageName1
				ifTrue: [ self assert: (#(#'foo') includes: loadedMethod selector) ].
			loadedPackage name = packageName2
				ifTrue: [ 
					self assert: false description: 'no instance methods expected in '.
					packageName2 ] ]
		loadedClassMethodsDo: [ :loadedProject :loadedPackage :loadedClassOrClassExtension :loadedMethod | 
			loadedPackage name = packageName1
				ifTrue: [ 
					self
						assert: false
						description: 'no instance methods expected in ' , packageName1 ].
			loadedPackage name = packageName2
				ifTrue: [ self assert: (#(#'bar') includes: loadedMethod selector) ] ] ].

	writtenStateValidationBlock value.

	normalClass1 := Object
		rwSubclass: className1
		instVarNames: #('ivar1')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().	"add new instance variable to class"

	normalInstance1 := normalClass1 new.

	normalClass1 rwCompileMethod: 'foo "comment" ^''foo''' category: 'accessing'.	"change method"
	normalClass1 rwCompileMethod: 'ivar1 ^ivar1' category: 'accessing'.	"add new instance method"
	normalClass1 class
		rwCompileMethod: 'baz ^''baz'''
		category: '*' , packageName2 asLowercase.	"add new class-side method"
	normalClass1 class rwRemoveSelector: #'bar'.	"remove existing method"

	self assert: normalInstance1 foo = 'foo'.
	self assert: normalInstance1 ivar1 = nil.
	self assert: normalClass1 baz = 'baz'.
	self should: [ normalClass1 bar ] raise: MessageNotUnderstood.

	normalClass2 rwCompileMethod: 'biz ^''biz''' category: 'accessing'.	"add new instance method"
	normalClass2
		rwCompileMethod: 'biff ^''biff'''
		category: '*' , packageName2 asLowercase.	"add new class-side method"

	normalInstance2 := normalClass2 new.
	self assert: normalInstance2 biz = 'biz'.
	self assert: normalInstance2 biff = 'biff'.

	theLoadedProject := Rowan image loadedProjectNamed: projectName.	"Traverse the loaded package structure and verify that it matches the expected structure"

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
					loadedClass name = className1
						ifTrue: [ self assert: (loadedClass propertyAt: 'instvars') = #('ivar1') ]
						ifFalse: [ 
							loadedClass name = className2
								ifTrue: [ self assert: (loadedClass propertyAt: 'instvars') isEmpty ]
								ifFalse: [ 
									self
										assert: false
										description:
											'Unexpected loaded class ' , loadedClass name printString , ' in '
												, packageName2 ] ] ]
				ifFalse: [ self assert: false description: 'No class expected in ' , packageName2 ] ]
		loadedClassExtensionsDo: [ :loadedProject :loadedPackage :loadedClassExtension | 
			self assert: theLoadedProject == loadedProject.
			self assert: theLoadedPackage == loadedPackage.
			self
				assert:
					(Rowan image loadedPackageNamed: loadedPackage name) == loadedPackage.
			theLoadedClassOrClassExtension := loadedClassExtension.

			loadedPackage name = packageName1
				ifTrue: [ 
					self
						assert: false
						description: 'no classes expected in ' , packageName1 printString ].
			loadedPackage name = packageName2
				ifTrue: [ self assert: (classNames includes: loadedClassExtension name) ] ]
		loadedInstanceMethodsDo: [ :loadedProject :loadedPackage :loadedClassOrClassExtension :loadedMethod | 
			self assert: theLoadedProject == loadedProject.
			self assert: theLoadedPackage == loadedPackage.
			self
				assert:
					(Rowan image loadedPackageNamed: loadedPackage name) == loadedPackage.
			self assert: theLoadedClassOrClassExtension == loadedClassOrClassExtension.

			loadedPackage name = packageName1
				ifTrue: [ 
					loadedMethod loadedClass name = className1
						ifTrue: [ self assert: (#(#'foo' #'ivar1') includes: loadedMethod selector) ]
						ifFalse: [ 
							loadedMethod loadedClass name = className2
								ifTrue: [ self assert: (#(#'biz') includes: loadedMethod selector) ]
								ifFalse: [ 
									self
										assert: false
										description:
											'Unexpected loaded method ' , loadedMethod selector printString , ' in '
												, packageName1 ] ] ].
			loadedPackage name = packageName2
				ifTrue: [ 
					loadedMethod loadedClass name = className2
						ifTrue: [ self assert: (#(#'biff') includes: loadedMethod selector) ]
						ifFalse: [ 
							self
								assert: false
								description:
									'Unexpected loaded method ' , loadedMethod selector printString , ' in '
										, packageName2 ] ] ]
		loadedClassMethodsDo: [ :loadedProject :loadedPackage :loadedClassOrClassExtension :loadedMethod | 
			loadedPackage name = packageName1
				ifTrue: [ 
					self
						assert: false
						description: 'no instance methods expected in ' , packageName1 ].
			loadedPackage name = packageName2
				ifTrue: [ self assert: (#(#'baz') includes: loadedMethod selector) ] ].

	projectTools load loadProjectNamed: projectName.	"restore original state of project"

	normalClass1 := Rowan globalNamed: className1.
	normalInstance1 := normalClass1 new.

	self assert: normalClass1 bar = 'bar'.
	self assert: normalInstance1 foo = 'foo'.
	self should: [ normalInstance1 ivar1 ] raise: MessageNotUnderstood.
	self should: [ normalClass1 baz = 'baz' ] raise: MessageNotUnderstood.

	oldNormalClass2 := normalClass2.
	normalClass2 := Rowan globalNamed: className2.
	self assert: oldNormalClass2 == normalClass2.
	normalInstance2 := normalClass2 new.
	self should: [ normalInstance2 biz ] raise: MessageNotUnderstood.
	self should: [ normalInstance2 biff ] raise: MessageNotUnderstood.

	writtenStateValidationBlock value	"verify that original state is restored"
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
	self assert: normalClass new foo = 'foo'.

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
	self assert: otherClass new foo = 'foo'.

	Rowan projectTools browser removePackageNamed: packageName1.

	self assert: (Rowan globalNamed: className1) isNil.
	self assert: (Rowan globalNamed: className2) == otherClass.
	self should: [ otherClass new foo ] raise: MessageNotUnderstood.

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
	self assert: normalInstance foo = 'foo'.
	self assert: (x := normalClass categoryOfSelector: #'foo') asString = protocol.
	self assert: fooMethod rowanPackageName = packageName2.

	fooMethod := normalClass rwCompileMethod: 'foo ^''bar''' category: protocol.

	self assert: (x := normalInstance foo) = 'bar'.
	self assert: (x := normalClass categoryOfSelector: #'foo') asString = protocol.
	self assert: fooMethod rowanPackageName = packageName2.

	barMethod := normalClass class
		rwCompileMethod: 'bar ^''foo'''
		category: protocol.

	self assert: (x := normalClass bar) = 'foo'.
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
RwHybridBrowserToolTest >> testHybridMoveClassToPackageInAlternaterSymbolDict [

	| normalClass projectName packageNames packageName1 packageName2 movedNormalClass packageNameMap symDict registry |
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

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

	movedNormalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName2
		options: #().

	self assert: movedNormalClass == normalClass.
	self assert: movedNormalClass rowanPackageName = packageName2.
	self assert: movedNormalClass category= packageName2.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridMoveClassToPackageInAlternaterSymbolDict_class_vars [

	| normalClass projectName packageNames packageName1 packageName2 movedNormalClass packageNameMap symDict registry |
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

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

	movedNormalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #()
		classVars: #(Cvar1)
		classInstVars: #()
		poolDictionaries: #()
		category: packageName2
		options: #().

	self assert: movedNormalClass == normalClass.
	self assert: movedNormalClass rowanPackageName = packageName2.
	self assert: movedNormalClass category= packageName2.
	self assert: (movedNormalClass classVarNames includes: #Cvar1).

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridMoveClassToPackageInAlternaterSymbolDict_class_vars_constraints [

	| normalClass projectName packageNames packageName1 packageName2 movedNormalClass packageNameMap symDict registry |
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

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

	movedNormalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #(ivar1)
		classVars: #(Cvar1)
		classInstVars: #()
		poolDictionaries: #()
		category: packageName2
		constraints: {{#ivar1 . Association}}
		options: #().

	self assert: movedNormalClass == normalClass.
	self assert: movedNormalClass rowanPackageName = packageName2.
	self assert: movedNormalClass category= packageName2.
	self assert: (movedNormalClass classVarNames includes: #Cvar1).
	self assert: (movedNormalClass _constraintOn: #ivar1) = Association.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridMoveClassToPackageInAlternaterSymbolDict_class_vars_constraints_new_class_version [

	| normalClass projectName packageNames packageName1 packageName2 movedNormalClass packageNameMap symDict registry |
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

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

	movedNormalClass := Object
		rwSubclass: 'SimpleHybridNormal1'
		instVarNames: #(ivar1 ivar2)
		classVars: #(Cvar1)
		classInstVars: #()
		poolDictionaries: #()
		category: packageName2
		constraints: {{#ivar1 . Association}}
		options: #().

	self assert: movedNormalClass ~~ normalClass.
	self assert: movedNormalClass rowanPackageName = packageName2.
	self assert: movedNormalClass category= packageName2.
	self assert: (movedNormalClass classVarNames includes: #Cvar1).
	self assert: (movedNormalClass _constraintOn: #ivar1) = Association.
	self assert: (movedNormalClass instVarNames includes: #ivar1).
	self assert: (movedNormalClass instVarNames includes: #ivar2).

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

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

	self assert: normalClass new foo = 'foo'.
	self assert: normalClass new ext1 = 'ext1'.
	self assert: normalClass bar = 'bar'.
	self assert: normalClass ext2 = 'ext2'.

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
		category: packageName3
		options: #().

	self assert: normalClass new foo = 'foo'.
	self assert: normalClass new ext1 = 'ext1'.
	self assert: normalClass bar = 'bar'.
	self assert: normalClass ext2 = 'ext2'.

	fooMethod := movedNormalClass compiledMethodAt: #foo.
	barMethod := movedNormalClass class compiledMethodAt: #bar.
	ext1Method := movedNormalClass compiledMethodAt: #ext1.
	ext2Method := movedNormalClass class compiledMethodAt: #ext2.

	self assert: movedNormalClass == normalClass.
	self assert: movedNormalClass rowanPackageName = packageName3.
	self assert: fooMethod rowanPackageName = packageName3.
	self assert: barMethod rowanPackageName = packageName3.
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

	self assert: normalClass new foo = 'foo'.
	self assert: normalClass new ext1 = 'ext1'.
	self assert: normalClass bar = 'bar'.
	self assert: normalClass ext2 = 'ext2'.

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

	self assert: normalClass new foo = 'foo'.
	self assert: normalClass new ext1 = 'ext1'.
	self assert: normalClass bar = 'bar'.
	self assert: normalClass ext2 = 'ext2'.

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
RwHybridBrowserToolTest >> testHybridMoveMethod_309 [

	"https://github.com/dalehenrich/Rowan/issues/309"

	|  normalClass projectName packageNames packageName1 packageName2 packageName3 className symDict registry |
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

	self assert: registry packageRegistry size = 3.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 1.
	self assert: registry methodRegistry size = 3.

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

	self assert: registry packageRegistry size = 3.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 1.
	self assert: registry methodRegistry size = 3.

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

	| class subclass projectName packageNames packageName1 newClassVersion newSubclassVersion |
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

	self assert: newClassVersion ~~ class.
	newSubclassVersion := Rowan globalNamed: subclass name.
	self assert: newSubclassVersion ~~ subclass.
	self assert: newSubclassVersion superClass == newClassVersion
]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridNewClassVersionWithSubclassesAndMethodsA [

	| class subclass projectName packageNames packageName1 newClassVersion newSubclassVersion |
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
	class rwCompileMethod: 'foo ^''foo''' category: '*' , packageName1 asLowercase.
	class class
		rwCompileMethod: 'foo ^''foo'''
		category: '*' , packageName1 asLowercase.

	subclass := class
		rwSubclass: 'SimpleHybridSubclass1'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	subclass
		rwCompileMethod: 'foo1 ^''foo'''
		category: '*' , packageName1 asLowercase.
	subclass class
		rwCompileMethod: 'foo1 ^''foo'''
		category: '*' , packageName1 asLowercase.

	self assert: subclass superClass == class.

	newClassVersion := Object
		rwSubclass: 'SimpleHybrid1'
		instVarNames: #('ivar1')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	self assert: newClassVersion ~~ class.
	newSubclassVersion := Rowan globalNamed: subclass name.
	self assert: newSubclassVersion ~~ subclass.
	self assert: newSubclassVersion superClass == newClassVersion.
	self assert: newSubclassVersion foo = 'foo'.
	self assert: newSubclassVersion foo1 = 'foo'.
	self assert: newSubclassVersion new foo = 'foo'.
	self assert: newSubclassVersion new foo1 = 'foo'
]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridNewClassVersionWithSubclassesAndMethodsB [

	| class subclass projectName packageNames packageName1 newClassVersion newSubclassVersion |
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

	newClassVersion := Object
		rwSubclass: 'SimpleHybrid1'
		instVarNames: #('ivar1')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	self assert: newClassVersion ~~ class.
	newSubclassVersion := Rowan globalNamed: subclass name.
	self assert: newSubclassVersion ~~ subclass.
	self assert: newSubclassVersion superClass == newClassVersion.
	self assert: newSubclassVersion foo = 'foo'.
	self assert: newSubclassVersion foo1 = 'foo'.
	self assert: newSubclassVersion new foo = 'foo'.
	self assert: newSubclassVersion new foo1 = 'foo'
]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridNewClassVersionWithSubclassesAndMethodsC [

	| class subclass projectName packageNames packageName1 packageName2 newClassVersion newSubclassVersion |
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

	newClassVersion := Object
		rwSubclass: 'SimpleHybrid1'
		instVarNames: #('ivar1')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	self assert: newClassVersion ~~ class.
	newSubclassVersion := Rowan globalNamed: subclass name.
	self assert: newSubclassVersion ~~ subclass.
	self assert: newSubclassVersion superClass == newClassVersion.
	self assert: newSubclassVersion foo = 'foo'.
	self assert: newSubclassVersion foo1 = 'foo'.
	self assert: newSubclassVersion new foo = 'foo'.
	self assert: newSubclassVersion new foo1 = 'foo'
]

{ #category : 'tests' }
RwHybridBrowserToolTest >> testHybridNewClassVersionWithSubclassesB [

	| class subclass1 subclass2 subclass3 subclass4 projectName packageNames packageName1 newClassVersion newSubclassVersion1 newSubclassVersion2 newSubclassVersion3 newSubclassVersion4 |
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

{ #category : 'tests - packages' }
RwHybridBrowserToolTest >> testHybridProjectLoad [

	"Write project to disk, make a few modifications and then reload the project from disk"

	| normalClass projectName packageNames packageName1 packageName2 fooMethod barMethod normalInstance projectTools bazMethod ivar1Method className theLoadedProject theLoadedPackage theLoadedClassOrClassExtension writtenStateValidationBlock |
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
		root: '/tmp/rowanHybridPatchProject_02/'.
	className := 'SimpleHybridNormalReload'.

	normalClass := Object
		rwSubclass: className
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

	writtenStateValidationBlock := [ 
	theLoadedProject := Rowan image loadedProjectNamed: projectName.	"Traverse the loaded package structure and verify that it matches the original expected structure"

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
					self assert: loadedClass name = className.
					self assert: (loadedClass propertyAt: 'instvars') isEmpty ]
				ifFalse: [ self assert: false description: 'No class expected in ' , packageName2 ] ]
		loadedClassExtensionsDo: [ :loadedProject :loadedPackage :loadedClassExtension | 
			self assert: theLoadedProject == loadedProject.
			self assert: theLoadedPackage == loadedPackage.
			self
				assert:
					(Rowan image loadedPackageNamed: loadedPackage name) == loadedPackage.
			theLoadedClassOrClassExtension := loadedClassExtension.

			loadedPackage name = packageName1
				ifTrue: [ 
					self
						assert: false
						description: 'no classes expected in ' , packageName1 printString ].
			loadedPackage name = packageName2
				ifTrue: [ self assert: loadedClassExtension name = className ] ]
		loadedInstanceMethodsDo: [ :loadedProject :loadedPackage :loadedClassOrClassExtension :loadedMethod | 
			self assert: theLoadedProject == loadedProject.
			self assert: theLoadedPackage == loadedPackage.
			self
				assert:
					(Rowan image loadedPackageNamed: loadedPackage name) == loadedPackage.
			self assert: theLoadedClassOrClassExtension == loadedClassOrClassExtension.

			loadedPackage name = packageName1
				ifTrue: [ self assert: (#(#'foo') includes: loadedMethod selector) ].
			loadedPackage name = packageName2
				ifTrue: [ 
					self assert: false description: 'no instance methods expected in '.
					packageName2 ] ]
		loadedClassMethodsDo: [ :loadedProject :loadedPackage :loadedClassOrClassExtension :loadedMethod | 
			loadedPackage name = packageName1
				ifTrue: [ 
					self
						assert: false
						description: 'no instance methods expected in ' , packageName1 ].
			loadedPackage name = packageName2
				ifTrue: [ self assert: (#(#'bar') includes: loadedMethod selector) ] ] ].

	writtenStateValidationBlock value.

	normalClass := Object
		rwSubclass: className
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

	theLoadedProject := Rowan image loadedProjectNamed: projectName.	"Traverse the loaded package structure and verify that it matches the expected structure"

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
					self assert: loadedClass name = className.
					self assert: (loadedClass propertyAt: 'instvars') = #('ivar1') ]
				ifFalse: [ self assert: false description: 'No class expected in ' , packageName2 ] ]
		loadedClassExtensionsDo: [ :loadedProject :loadedPackage :loadedClassExtension | 
			self assert: theLoadedProject == loadedProject.
			self assert: theLoadedPackage == loadedPackage.
			self
				assert:
					(Rowan image loadedPackageNamed: loadedPackage name) == loadedPackage.
			theLoadedClassOrClassExtension := loadedClassExtension.

			loadedPackage name = packageName1
				ifTrue: [ 
					self
						assert: false
						description: 'no classes expected in ' , packageName1 printString ].
			loadedPackage name = packageName2
				ifTrue: [ self assert: loadedClassExtension name = className ] ]
		loadedInstanceMethodsDo: [ :loadedProject :loadedPackage :loadedClassOrClassExtension :loadedMethod | 
			self assert: theLoadedProject == loadedProject.
			self assert: theLoadedPackage == loadedPackage.
			self
				assert:
					(Rowan image loadedPackageNamed: loadedPackage name) == loadedPackage.
			self assert: theLoadedClassOrClassExtension == loadedClassOrClassExtension.

			loadedPackage name = packageName1
				ifTrue: [ self assert: (#(#'foo' #'ivar1') includes: loadedMethod selector) ].
			loadedPackage name = packageName2
				ifTrue: [ 
					self assert: false description: 'no instance methods expected in '.
					packageName2 ] ]
		loadedClassMethodsDo: [ :loadedProject :loadedPackage :loadedClassOrClassExtension :loadedMethod | 
			loadedPackage name = packageName1
				ifTrue: [ 
					self
						assert: false
						description: 'no instance methods expected in ' , packageName1 ].
			loadedPackage name = packageName2
				ifTrue: [ self assert: (#(#'baz') includes: loadedMethod selector) ] ].

	projectTools load loadProjectNamed: projectName.	"restore original state of project"

	normalClass := Rowan globalNamed: className.
	normalInstance := normalClass new.

	self assert: normalClass bar = 'bar'.
	self assert: normalInstance foo = 'foo'.
	self should: [ normalInstance ivar1 ] raise: MessageNotUnderstood.
	self should: [ normalClass baz = 'baz' ] raise: MessageNotUnderstood.

	writtenStateValidationBlock value	"verify that original state is restored"
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
