Class {
	#name : 'RwUnmanagedClassesV2Test',
	#superclass : 'RwAbstractV2Test',
	#category : 'Rowan-TestsV2'
}

{ #category : 'private' }
RwUnmanagedClassesV2Test >> _createLoadedProject: projectName packageName: packageName symbolDictionaryName: symbolDictionaryName [
	| project |
	project := self
		_createProject: projectName
		packageName: packageName
		symbolDictionaryName: symbolDictionaryName.
	^ project load at: 1
]

{ #category : 'private' }
RwUnmanagedClassesV2Test >> _createProject: projectName packageName: packageName symbolDictionaryName: symbolDictionaryName [
	| componentName project |
	componentName := 'Core'.
	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	project := Rowan newProjectNamed: projectName.
	project
		addNewComponentNamed: 'Core';
		addPackageNamed: packageName toComponentNamed: componentName;
		gemstoneSetSymbolDictName: symbolDictionaryName forPackageNamed: packageName;
		packageConvention: 'Rowan';
		yourself.
	^ project
]

{ #category : 'private' }
RwUnmanagedClassesV2Test >> _removeUnmanagedClass [
	| className |
	className := self _unmanagedClassName.

	((Rowan globalNamed: 'UserGlobals') at: className asSymbol ifAbsent: [])
		ifNotNil: [ 
			[ Rowan projectTools browser removeClassNamed: className ]
				on: RwPerformingUnpackagedEditNotification
				do: [ :ex | ex resume ] ]
]

{ #category : 'private' }
RwUnmanagedClassesV2Test >> _unmanagedClassName [
	^ 'UnmanagedTestClass'
]

{ #category : 'private' }
RwUnmanagedClassesV2Test >> _validateAuditFailure: audit forPackage: packageName class: className method: selector [
	"right now there are unrelated auditissues in UnPackaged project, so filter them out"

	self
		assert:
			((audit at: packageName) at: className) first message
				= ('Missing loaded method>>' , selector asString)
]

{ #category : 'private' }
RwUnmanagedClassesV2Test >> _validateAuditResults: audit [
	"right now there are unrelated auditissues in UnPackaged project, so filter them out"

	audit removeKey: 'UnPackaged-Globals' ifAbsent: [  ].
	self assert: audit isEmpty
]

{ #category : 'running' }
RwUnmanagedClassesV2Test >> setUp [
	super setUp.
	self _removeUnmanagedClass
]

{ #category : 'running' }
RwUnmanagedClassesV2Test >> tearDown [
	self _removeUnmanagedClass.
	super tearDown.
]

{ #category : 'tests' }
RwUnmanagedClassesV2Test >> testMoveManagedClass_01 [
	"Move packaged class with packaged and unpackaged methods to a package in another project"

	| className managedClass project projectName packageName symbolDictionary unmanagedMethod managedMethod x anotherProject anotherProjectName anotherPackageName |
	projectName := 'PackagedClass_project'.
	packageName := projectName , '-Core'.
	anotherProjectName := 'AnotherPackagedClass_project'.
	anotherPackageName := anotherProjectName , '-Core'.
	symbolDictionary := Rowan image
		newOrExistingSymbolDictionaryNamed: self _sampleSymbolDictionaryName1.

	className := self _unmanagedClassName.

	project := self
		_createLoadedProject: projectName
		packageName: packageName
		symbolDictionaryName: symbolDictionary name.

	anotherProject := self
		_createLoadedProject: anotherProjectName
		packageName: anotherPackageName
		symbolDictionaryName: symbolDictionary name.

	managedClass := Object
		rwSubclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName
		options: #().
	unmanagedMethod := managedClass
		compileMethod: 'foo ^1'
		dictionaries: Rowan image symbolList
		category: 'unmanaged'
		environmentId: 0.
	managedMethod := managedClass
		rwCompileMethod: 'bar ^2'
		category: 'managed'
		packageName: packageName.

	self assert: managedClass notNil.
	self assert: (Rowan globalNamed: className) notNil.
	self assert: ((Rowan globalNamed: className) new perform: #'foo') = 1.
	self assert: ((Rowan globalNamed: className) new perform: #'bar') = 2.
	self assert: (x := managedClass rowanPackageName) = packageName.
	self assert: (x := unmanagedMethod rowanPackageName) = Rowan unpackagedName.
	self assert: (x := managedMethod rowanPackageName) = packageName.

	managedClass rwMoveClassToPackage: anotherPackageName.

	self assert: ((Rowan globalNamed: className) new perform: #'foo') = 1.
	self assert: ((Rowan globalNamed: className) new perform: #'bar') = 2.

	self assert: (x := managedClass rowanPackageName) = anotherPackageName.
	self assert: (x := managedMethod rowanPackageName) = anotherPackageName.
	self assert: (x := unmanagedMethod rowanPackageName) = Rowan unpackagedName.
	self _validateAuditResults: (Rowan projectNamed: projectName) audit.
	self
		_validateAuditFailure: (Rowan projectNamed: anotherProjectName) audit
		forPackage: anotherPackageName
		class: className
		method: unmanagedMethod selector.
	managedClass
		rwMoveMethod: unmanagedMethod selector
		toPackage: anotherPackageName.	"repair the audit failure, so all audits run clean"
	self _validateAuditResults: (Rowan projectNamed: projectName) audit.
	self _validateAuditResults: (Rowan projectNamed: anotherProjectName) audit
]

{ #category : 'tests' }
RwUnmanagedClassesV2Test >> testMoveManagedMethod_01 [
	"Move packaged method in unpackaged class to another package"

	| className unmanagedClass project projectName packageName symbolDictionary managedMethod x anotherProject anotherProjectName anotherPackageName |
	projectName := 'PackagedClass_project'.
	packageName := projectName , '-Core'.
	anotherProjectName := 'AnotherPackagedClass_project'.
	anotherPackageName := anotherProjectName , '-Core'.
	symbolDictionary := Rowan image
		newOrExistingSymbolDictionaryNamed: self _sampleSymbolDictionaryName1.

	className := self _unmanagedClassName.

	project := self
		_createLoadedProject: projectName
		packageName: packageName
		symbolDictionaryName: symbolDictionary name.

	anotherProject := self
		_createLoadedProject: anotherProjectName
		packageName: anotherPackageName
		symbolDictionaryName: symbolDictionary name.

	unmanagedClass := Object
		subclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symbolDictionary
		options: #().
	managedMethod := unmanagedClass
		rwCompileExtensionMethod: 'bar ^2'
		category: 'managed'
		packageName: packageName.

	self assert: unmanagedClass notNil.
	self assert: (Rowan globalNamed: className) notNil.
	self assert: ((Rowan globalNamed: className) new perform: #'bar') = 2.
	self assert: (x := managedMethod rowanPackageName) = packageName.

	unmanagedClass
		rwMoveMethod: managedMethod selector
		toPackage: anotherPackageName.

	self
		_validateAuditResults: project audit;
		_validateAuditResults: anotherProject audit.
	self assert: (x := managedMethod rowanPackageName) = anotherPackageName
]

{ #category : 'tests' }
RwUnmanagedClassesV2Test >> testMoveUnmanagedClass_01 [
	"Move unpackaged class with packaged and unpackaged methods to a package in another project"

	| className unmanagedClass project projectName packageName symbolDictionary unmanagedMethod managedMethod x anotherProject anotherProjectName anotherPackageName |
	projectName := 'PackagedClass_project'.
	packageName := projectName , '-Core'.
	anotherProjectName := 'AnotherPackagedClass_project'.
	anotherPackageName := anotherProjectName , '-Core'.
	symbolDictionary := Rowan image
		newOrExistingSymbolDictionaryNamed: self _sampleSymbolDictionaryName1.

	className := self _unmanagedClassName.

	project := self
		_createLoadedProject: projectName
		packageName: packageName
		symbolDictionaryName: symbolDictionary name.

	anotherProject := self
		_createLoadedProject: anotherProjectName
		packageName: anotherPackageName
		symbolDictionaryName: symbolDictionary name.

	unmanagedClass := Object
		subclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symbolDictionary
		options: #().
	unmanagedMethod := unmanagedClass
		compileMethod: 'foo ^1'
		dictionaries: Rowan image symbolList
		category: 'unmanaged'
		environmentId: 0.
	managedMethod := unmanagedClass
		rwCompileExtensionMethod: 'bar ^2'
		category: 'managed'
		packageName: packageName.

	self assert: unmanagedClass notNil.
	self assert: (Rowan globalNamed: className) notNil.
	self assert: ((Rowan globalNamed: className) new perform: #'foo') = 1.
	self assert: ((Rowan globalNamed: className) new perform: #'bar') = 2.
	self assert: (x := unmanagedClass rowanPackageName) = Rowan unpackagedName.
	self assert: (x := unmanagedMethod rowanPackageName) = Rowan unpackagedName.
	self assert: (x := managedMethod rowanPackageName) = packageName.
	self
		_validateAuditResults: project audit;
		_validateAuditResults: anotherProject audit.

	unmanagedClass rwMoveClassToPackage: anotherPackageName.

	self assert: ((Rowan globalNamed: className) new perform: #'foo') = 1.
	self assert: ((Rowan globalNamed: className) new perform: #'bar') = 2.

	self
		_validateAuditResults: project audit;
		_validateAuditResults: anotherProject audit.
	self assert: (x := unmanagedClass rowanPackageName) = anotherPackageName.
	self assert: (x := unmanagedMethod rowanPackageName) = anotherPackageName.
	self assert: (x := managedMethod rowanPackageName) = packageName
]

{ #category : 'tests' }
RwUnmanagedClassesV2Test >> testMoveUnmanagedMethod_01 [
	"Move unpackaged method in unpackaged class to a package"

	| className unmanagedClass project projectName packageName symbolDictionary unmanagedMethod |
	projectName := 'PackagedClass_project'.
	packageName := projectName , '-Core'.
	symbolDictionary := Rowan image
		newOrExistingSymbolDictionaryNamed: self _sampleSymbolDictionaryName1.

	className := self _unmanagedClassName.

	project := self
		_createLoadedProject: projectName
		packageName: packageName
		symbolDictionaryName: symbolDictionary name.

	unmanagedClass := Object
		subclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symbolDictionary
		options: #().
	unmanagedMethod := unmanagedClass
		compileMethod: 'foo ^1'
		dictionaries: Rowan image symbolList
		category: 'unmanaged'
		environmentId: 0.

	self assert: unmanagedClass notNil.
	self assert: (Rowan globalNamed: className) notNil.
	self assert: ((Rowan globalNamed: className) new perform: #'foo') = 1.
	self assert: unmanagedMethod rowanPackageName = Rowan unpackagedName.

	unmanagedClass rwMoveMethod: unmanagedMethod selector toPackage: packageName.

	self _validateAuditResults: project audit.
	self assert: ((Rowan globalNamed: className) new perform: #'foo') = 1.
	self assert: unmanagedMethod rowanPackageName = packageName
]

{ #category : 'tests' }
RwUnmanagedClassesV2Test >> testPackagedClass_01 [
	"Use filein class creation against packaged class with no modifications.
		(inverse of testUnManagedClass01)"

	| className unmanagedClass managedClass project projectName packageName symbolDictionary |
	projectName := 'PackagedClass_project'.
	packageName := projectName , '-Core'.
	symbolDictionary := Rowan image
		newOrExistingSymbolDictionaryNamed: self _sampleSymbolDictionaryName1.

	className := self _unmanagedClassName.

	project := self
		_createLoadedProject: projectName
		packageName: packageName
		symbolDictionaryName: symbolDictionary name.

	managedClass := Object
		rwSubclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: nil
		packageName: packageName
		constraints: #()
		options: #().
	self assert: managedClass notNil.
	self assert: (Rowan globalNamed: className) notNil.

	unmanagedClass := Object
		subclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symbolDictionary
		options: #().
	self assert: unmanagedClass notNil.

	self assert: unmanagedClass == managedClass.
	self assert: unmanagedClass rowanPackageName = packageName.
	self _validateAuditResults: project audit
]

{ #category : 'tests' }
RwUnmanagedClassesV2Test >> testPackagedClassNewVersion_01 [
	"Use filein class creation against packaged class and create new version of class (inverse of testUnManagedClassNewVersion01"

	| className unmanagedClass managedClass project projectName packageName symbolDictionary |
	projectName := 'PackagedClass_project'.
	packageName := projectName , '-Core'.
	symbolDictionary := Rowan image
		newOrExistingSymbolDictionaryNamed: self _sampleSymbolDictionaryName1.

	className := self _unmanagedClassName.

	project := self
		_createLoadedProject: projectName
		packageName: packageName
		symbolDictionaryName: symbolDictionary name.

	managedClass := Object
		rwSubclass: className
		instVarNames: #(#'xxx' #'yyy')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: nil
		packageName: packageName
		constraints: #()
		options: #().
	self assert: managedClass notNil.
	self assert: (Rowan globalNamed: className) notNil.

	unmanagedClass := Object
		subclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symbolDictionary
		options: #().
	self assert: unmanagedClass notNil.

	self assert: unmanagedClass ~~ managedClass.
	self _validateAuditResults: (Rowan projectNamed: projectName) audit.
	self assert: unmanagedClass rowanPackageName = packageName
]

{ #category : 'tests' }
RwUnmanagedClassesV2Test >> testRemoveAllMethods [
	"Remove all methods, some packaged and some unpackaged"

	| className unmanagedClass project projectName packageName symbolDictionary unmanagedMethod managedMethod |
	projectName := 'PackagedClass_project'.
	packageName := projectName , '-Core'.
	symbolDictionary := Rowan image
		newOrExistingSymbolDictionaryNamed: self _sampleSymbolDictionaryName1.

	className := self _unmanagedClassName.

	project := self
		_createLoadedProject: projectName
		packageName: packageName
		symbolDictionaryName: symbolDictionary name.

	unmanagedClass := Object
		subclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symbolDictionary
		options: #().
	unmanagedMethod := unmanagedClass
		compileMethod: 'foo ^1'
		dictionaries: Rowan image symbolList
		category: 'unmanaged'
		environmentId: 0.
	managedMethod := unmanagedClass
		rwCompileExtensionMethod: 'bar ^2'
		category: 'managed'
		packageName: packageName.

	self assert: unmanagedClass notNil.
	self assert: (Rowan globalNamed: className) notNil.
	self assert: ((Rowan globalNamed: className) new perform: #'foo') = 1.
	self assert: ((Rowan globalNamed: className) new perform: #'bar') = 2.
	self assert: unmanagedMethod rowanPackageName = Rowan unpackagedName.
	self assert: managedMethod rowanPackageName = packageName.

	unmanagedClass removeAllMethods.

	self assert: project audit isEmpty.
	self assert: (unmanagedClass compiledMethodAt: #foo otherwise: nil) isNil.
	self assert: (unmanagedClass compiledMethodAt: #bar otherwise: nil) isNil.
]

{ #category : 'tests' }
RwUnmanagedClassesV2Test >> testUnmanagedClass_01 [
	"Use project browser class creation expression to package an unmanaged class"

	| className unmanagedClass managedClass project projectName packageName symbolDictionary |
	projectName := 'PackagedClass_project'.
	packageName := projectName , '-Core'.
	symbolDictionary := Rowan image
		newOrExistingSymbolDictionaryNamed: self _sampleSymbolDictionaryName1.

	className := self _unmanagedClassName.

	project := self
		_createLoadedProject: projectName
		packageName: packageName
		symbolDictionaryName: symbolDictionary name.

	unmanagedClass := Object
		subclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symbolDictionary
		options: #().
	self assert: unmanagedClass notNil.
	self assert: (Rowan globalNamed: className) notNil.

	managedClass := Object
		rwSubclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: nil
		packageName: packageName
		constraints: #()
		options: #().

	self assert: unmanagedClass == managedClass.
	self _validateAuditResults: project audit.
	self assert: unmanagedClass rowanPackageName = packageName
]

{ #category : 'tests' }
RwUnmanagedClassesV2Test >> testUnmanagedClass_02 [
	"Use project load to package the class"

	| className unmanagedClass managedClass project projectName packageName symbolDictionary |
	projectName := 'PackagedClass_project'.
	packageName := projectName , '-Core'.
	symbolDictionary := Rowan image
		newOrExistingSymbolDictionaryNamed: self _sampleSymbolDictionaryName1.

	className := self _unmanagedClassName.

	project := self
		_createProject: projectName
		packageName: packageName
		symbolDictionaryName: symbolDictionary name.

	unmanagedClass := Object
		subclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symbolDictionary
		options: #().
	self assert: unmanagedClass notNil.
	self assert: (Rowan globalNamed: className) notNil.

	(project packageNamed: packageName)
		addClassNamed: className
		super: 'Object'
		instvars: #('xxx').
	project load.

	managedClass := Rowan globalNamed: className.
	self assert: unmanagedClass == managedClass.
	self _validateAuditResults: (Rowan projectNamed: projectName) audit.
	self assert: unmanagedClass rowanPackageName = packageName
]

{ #category : 'tests' }
RwUnmanagedClassesV2Test >> testUnmanagedClass_03 [
	"Use project browser class creation expression to package an unmanaged class with a superclass of nil"

	| className unmanagedClass managedClass project projectName packageName symbolDictionary |
	projectName := 'PackagedClass_project'.
	packageName := projectName , '-Core'.
	symbolDictionary := Rowan image
		newOrExistingSymbolDictionaryNamed: self _sampleSymbolDictionaryName1.

	className := self _unmanagedClassName.

	project := self
		_createLoadedProject: projectName
		packageName: packageName
		symbolDictionaryName: symbolDictionary name.

	unmanagedClass := nil
		subclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symbolDictionary
		options: #().
	self assert: unmanagedClass notNil.
	self assert: (Rowan globalNamed: className) notNil.

	managedClass := nil
		rwSubclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: nil
		packageName: packageName
		constraints: #()
		options: #().

	self assert: unmanagedClass == managedClass.
	self _validateAuditResults: project audit.
	self assert: unmanagedClass rowanPackageName = packageName
]

{ #category : 'tests' }
RwUnmanagedClassesV2Test >> testUnmanagedClass_04 [
	"Use class definition in a different project to package an unmanaged class"

	| className unmanagedClass managedClass project projectName packageName symbolDictionary |
	projectName := 'PackagedClass_project'.
	packageName := projectName , '-Core'.
	symbolDictionary := Rowan image
		newOrExistingSymbolDictionaryNamed: self _sampleSymbolDictionaryName1.

	className := self _unmanagedClassName.

	project := self
		_createProject: projectName
		packageName: packageName
		symbolDictionaryName: symbolDictionary name.

	unmanagedClass := Object
		subclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symbolDictionary
		options: #().

	self assert: unmanagedClass notNil.
	self assert: (Rowan globalNamed: className) notNil.

	(project packageNamed: packageName)
		addClassNamed: className super: 'Object' instvars: #(#'xxx');
		yourself.
	project load.

	managedClass := Rowan globalNamed: className.
	self assert: unmanagedClass == managedClass.
	self _validateAuditResults: (Rowan projectNamed: projectName) audit.
	self assert: managedClass rowanPackageName = packageName
]

{ #category : 'tests' }
RwUnmanagedClassesV2Test >> testUnmanagedClass_05 [
	"Use class definition that creates new version in a different project to package an unmanaged class"

	| className unmanagedClass managedClass project projectName packageName symbolDictionary |
	projectName := 'PackagedClass_project'.
	packageName := projectName , '-Core'.
	symbolDictionary := Rowan image
		newOrExistingSymbolDictionaryNamed: self _sampleSymbolDictionaryName1.

	className := self _unmanagedClassName.

	project := self
		_createProject: projectName
		packageName: packageName
		symbolDictionaryName: symbolDictionary name.

	unmanagedClass := Object
		subclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symbolDictionary
		options: #().

	self assert: unmanagedClass notNil.
	self assert: (Rowan globalNamed: className) notNil.

	(project packageNamed: packageName)
		addClassNamed: className super: 'Object' instvars: #('xxx' 'yyy');
		yourself.
	project load.

	managedClass := Rowan globalNamed: className.
	self assert: unmanagedClass ~~ managedClass.
	self _validateAuditResults: (Rowan projectNamed: projectName) audit.
	self assert: managedClass rowanPackageName = packageName
]

{ #category : 'tests' }
RwUnmanagedClassesV2Test >> testUnmanagedClassNewVersion_01 [
	"Use project browser class creation expression to package and create a new version of an unmanaged class"

	| className unmanagedClass managedClass project projectName packageName symbolDictionary |
	projectName := 'PackagedClass_project'.
	packageName := projectName , '-Core'.
	symbolDictionary := Rowan image
		newOrExistingSymbolDictionaryNamed: self _sampleSymbolDictionaryName1.

	className := self _unmanagedClassName.

	project := self
		_createLoadedProject: projectName
		packageName: packageName
		symbolDictionaryName: symbolDictionary name.

	unmanagedClass := Object
		subclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symbolDictionary
		options: #().
	self assert: unmanagedClass notNil.
	self assert: (Rowan globalNamed: className) notNil.

	managedClass := Object
		rwSubclass: className
		instVarNames: #(#'xxx' #'yyy')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: nil
		packageName: packageName
		constraints: #()
		options: #().

	self assert: unmanagedClass ~~ managedClass.
	self _validateAuditResults: project audit.
	self assert: managedClass rowanPackageName = packageName
]

{ #category : 'tests' }
RwUnmanagedClassesV2Test >> testUnmanagedClassNewVersion_02 [
	"Use project load to package and create a new version of an unmanaged class"

	| className unmanagedClass managedClass project projectName packageName symbolDictionary |
	projectName := 'PackagedClass_project'.
	packageName := projectName , '-Core'.
	symbolDictionary := Rowan image
		newOrExistingSymbolDictionaryNamed: self _sampleSymbolDictionaryName1.

	className := self _unmanagedClassName.

	project := self
		_createProject: projectName
		packageName: packageName
		symbolDictionaryName: symbolDictionary name.

	unmanagedClass := Object
		subclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symbolDictionary
		options: #().
	self assert: unmanagedClass notNil.
	self assert: (Rowan globalNamed: className) notNil.

	(project packageNamed: packageName)
		addClassNamed: className
		super: 'Object'
		instvars: #('xxx' 'yyy').
	project load.

	managedClass := Rowan globalNamed: className.

	self assert: unmanagedClass ~~ managedClass.
	self _validateAuditResults: (Rowan projectNamed: projectName) audit.
	self assert: managedClass rowanPackageName = packageName
]
