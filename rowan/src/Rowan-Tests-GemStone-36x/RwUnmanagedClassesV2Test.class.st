Class {
	#name : 'RwUnmanagedClassesV2Test',
	#superclass : 'RwAbstractV2Test',
	#category : 'Rowan-Tests-GemStone-36x'
}

{ #category : 'private' }
RwUnmanagedClassesV2Test >> _createLoadedProject: projectName packageName: packageName symbolDictionaryName: symbolDictionaryName [
	| project |
	project := self
		_createProject: projectName
		packageName: packageName
		symbolDictionaryName: symbolDictionaryName.
	^ project load projectNamed: projectName
]

{ #category : 'private' }
RwUnmanagedClassesV2Test >> _createLoadedProject: projectName packageNames: packageNames symbolDictionaryName: symbolDictionaryName [
	| project |
	project := self
		_createProject: projectName
		packageNames: packageNames
		symbolDictionaryName: symbolDictionaryName.
	^ project load projectNamed: projectName
]

{ #category : 'private' }
RwUnmanagedClassesV2Test >> _createProject: projectName packageName: packageName symbolDictionaryName: symbolDictionaryName [
	^ self
		_createProject: projectName
		packageNames: {packageName}
		symbolDictionaryName: symbolDictionaryName
]

{ #category : 'private' }
RwUnmanagedClassesV2Test >> _createProject: projectName packageNames: packageNames symbolDictionaryName: symbolDictionaryName [
	| componentName project |
	componentName := 'Core'.
	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	project := Rowan newProjectNamed: projectName.
	project
		addLoadComponentNamed: 'Core';
		packageConvention: 'Rowan';
		yourself.

	packageNames
		do: [ :packageName | 
			project
				addPackageNamed: packageName toComponentNamed: componentName;
				gemstoneSetSymbolDictName: symbolDictionaryName
					forPackageNamed: packageName;
				yourself ].
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
RwUnmanagedClassesV2Test >> _simpleGsFilein_foo_UnManaged: className instVarNames: instVarNamesString symbolDictionaryName: symbolDictionaryName [
	^ 'expectvalue /String
run
(Object subclass: ''' , className
		,
			'''
  instVarNames: ' , instVarNamesString
		,
			'
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: ' , symbolDictionaryName
		,
			'
  options:#() ) definition
%
set class ' , className
		,
			'
removeallmethods
removeallclassmethods
set category accessing

run
', className, '
		compileMethod: ''foo ^1''
		dictionaries: Rowan image symbolList
		category: #''accessing''
		intoMethodDict: nil
		intoCategories: nil
		environmentId: 0.
%

method
bar ^2
%
'
]

{ #category : 'private' }
RwUnmanagedClassesV2Test >> _simpleGsFilein: className instVarNames: instVarNamesString symbolDictionaryName: symbolDictionaryName [
	^ 'expectvalue /String
run
(Object subclass: ''' , className
		,
			'''
  instVarNames: ' , instVarNamesString
		,
			'
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: ' , symbolDictionaryName
		,
			'
  options:#() ) definition
%
set class ' , className
		,
			'
removeallmethods
removeallclassmethods
set category accessing

method
foo ^1
%

method
bar ^2
%
'
]

{ #category : 'private' }
RwUnmanagedClassesV2Test >> _simpleGsFileinUnManagedMethods: className instVarNames: instVarNamesString symbolDictionaryName: symbolDictionaryName [
	^ 'expectvalue /String
run
(Object subclass: ''' , className
		,
			'''
  instVarNames: ' , instVarNamesString
		,
			'
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: ' , symbolDictionaryName
		,
			'
  options:#() ) definition
%
set class ' , className
		,
			'
removeallmethods
removeallclassmethods

run
', className, '
		compileMethod: ''foo ^1''
		dictionaries: Rowan image symbolList
		category: #''accessing''
		intoMethodDict: nil
		intoCategories: nil
		environmentId: 0.
', className, '
		compileMethod: ''bar ^2''
		dictionaries: Rowan image symbolList
		category: #''accessing''
		intoMethodDict: nil
		intoCategories: nil
		environmentId: 0.
%
'
]

{ #category : 'private' }
RwUnmanagedClassesV2Test >> _unmanagedClassName [
	^ 'UnmanagedTestClass'
]

{ #category : 'private' }
RwUnmanagedClassesV2Test >> _validateAuditFailure: audit forPackage: packageName class: className method: selector [
	"right now there are unrelated auditissues in UnPackaged project, so filter them out"

	self
		assert: ((audit at: packageName) at: className) first reason
		equals: #'missingLoadedMethod'.
	self
		assert: ((audit at: packageName) at: className) first selector
		equals: selector.

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
	self _removeUnmanagedClass.
	Rowan gemstoneTools topaz currentTopazPackageName: nil.
]

{ #category : 'running' }
RwUnmanagedClassesV2Test >> tearDown [
	self _removeUnmanagedClass.
	Rowan gemstoneTools topaz currentTopazPackageName: nil.
	super tearDown.
]

{ #category : 'tests' }
RwUnmanagedClassesV2Test >> testCurrentTopazPackageClass_01 [
	"Package class based on current topaz package name"

	| className managedClass project projectName packageName symbolDictionary |
	projectName := 'PackagedClass_project'.
	packageName := projectName , '-Core'.
	symbolDictionary := Rowan image
		newOrExistingSymbolDictionaryNamed: self _sampleSymbolDictionaryName1.

	className := self _unmanagedClassName.

	project := self
		_createLoadedProject: projectName
		packageName: packageName
		symbolDictionaryName: symbolDictionary name.

	[ 
	Rowan gemstoneTools topaz currentTopazPackageName: packageName.
	managedClass := Object
		subclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symbolDictionary
		options: #() ]
		ensure: [ Rowan gemstoneTools topaz currentTopazPackageName: nil ].

	self assert: managedClass notNil.
	self assert: (Rowan globalNamed: className) notNil.

	self _validateAuditResults: project audit.
	self assert: managedClass rowanPackageName = packageName.
]

{ #category : 'tests' }
RwUnmanagedClassesV2Test >> testCurrentTopazPackageFilein_01 [
	"File in an unmanaged class and methods (includes removeAllMethods), using `set package`"

	| className managedClass project projectName packageName symbolDictionary stream |
	projectName := 'PackagedClass_project'.
	packageName := projectName , '-Core'.
	symbolDictionary := Rowan image
		newOrExistingSymbolDictionaryNamed: self _sampleSymbolDictionaryName1.

	className := self _unmanagedClassName.

	project := self
		_createLoadedProject: projectName
		packageName: packageName
		symbolDictionaryName: symbolDictionary name.

	stream := ReadStreamPortable
		on:
			'set package ' , packageName
				,
					'
'
				,
					(self
						_simpleGsFilein: className
						instVarNames: '#(#xxx)'
						symbolDictionaryName: symbolDictionary name asString).

	((Rowan globalNamed: 'GsFileIn') newFromStream: stream) doFileIn.

	managedClass := Rowan globalNamed: className.
	self assert: managedClass notNil.
	self assert: ((Rowan globalNamed: className) new perform: #'foo') = 1.
	self assert: ((Rowan globalNamed: className) new perform: #'bar') = 2.
	self assert: managedClass rowanPackageName = packageName.
	self
		assert: (managedClass compiledMethodAt: #'foo') rowanPackageName = packageName.
	self
		assert: (managedClass compiledMethodAt: #'bar') rowanPackageName = packageName.
	self assert: project audit isEmpty
]

{ #category : 'tests' }
RwUnmanagedClassesV2Test >> testCurrentTopazPackageFilein_02 [
	"Package a method in an unpackaged class load a .gs filein using `set package`
		removeAll enabled"

	| className unmanagedClass project projectName packageName symbolDictionary managedMethod x anotherProject anotherProjectName anotherPackageName stream |
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

	stream := ReadStreamPortable
		on:
			'set package ' , anotherPackageName
				,
					'		
'
				,
					(self
						_simpleGsFilein: className
						instVarNames: '#(#xxx)'
						symbolDictionaryName: symbolDictionary name asString).

	((Rowan globalNamed: 'GsFileIn') newFromStream: stream) doFileIn.


	self
		_validateAuditResults: project audit;
		_validateAuditResults: anotherProject audit.
	self assert: ((Rowan globalNamed: className) new perform: #'foo') = 1.
	self assert: ((Rowan globalNamed: className) new perform: #'bar') = 2.

	self
		assert:
			(unmanagedClass compiledMethodAt: #'foo') rowanPackageName = anotherPackageName.
	self
		assert:
			(x := (unmanagedClass compiledMethodAt: #'bar') rowanPackageName) = anotherPackageName
]

{ #category : 'tests' }
RwUnmanagedClassesV2Test >> testCurrentTopazPackageFilein_03 [
	"Package a method in an unpackaged class load a .gs filein using `set package`
		removeAll disabled"

	| className unmanagedClass project projectName packageName symbolDictionary managedMethod x anotherProject anotherProjectName anotherPackageName stream |
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

	stream := ReadStreamPortable
		on:
			'set package ' , anotherPackageName
				,
					'		
'
				,
					(self
						_simpleGsFilein: className
						instVarNames: '#(#xxx)'
						symbolDictionaryName: symbolDictionary name asString).

	((Rowan globalNamed: 'GsFileIn') newFromStream: stream)
		setEnableRemoveAll: false;
		doFileIn.


	self
		_validateAuditResults: project audit;
		_validateAuditResults: anotherProject audit.
	self assert: ((Rowan globalNamed: className) new perform: #'foo') = 1.
	self assert: ((Rowan globalNamed: className) new perform: #'bar') = 2.

	self
		assert:
			(unmanagedClass compiledMethodAt: #'foo') rowanPackageName = anotherPackageName.
	self
		assert:
			(x := (unmanagedClass compiledMethodAt: #'bar') rowanPackageName) = packageName
]

{ #category : 'tests' }
RwUnmanagedClassesV2Test >> testCurrentTopazPackageMethod_01 [
	"Package method based on current topaz package name"

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

	[ 
	Rowan gemstoneTools topaz currentTopazPackageName: packageName.
	unmanagedMethod := unmanagedClass
		compileMethod: 'foo ^1'
		dictionaries: Rowan image symbolList
		category: 'managed'
		environmentId: 0 ]
		ensure: [ Rowan gemstoneTools topaz currentTopazPackageName: nil ].

	self _validateAuditResults: project audit.
	self assert: ((Rowan globalNamed: className) new perform: #'foo') = 1.
	self assert: unmanagedMethod rowanPackageName = packageName.
	self
		assert:
			((Rowan globalNamed: className) categoryOfSelector: #'foo') = #'managed'
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
		category: #'unmanaged'
		intoMethodDict: nil
		intoCategories: nil
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

{ #category : 'tests' }
RwUnmanagedClassesV2Test >> testUnmanagedFilein_01 [
	"File in an unmanaged class and methods (includes removeAllMethods)"

	| className unmanagedClass project projectName packageName symbolDictionary stream |
	projectName := 'PackagedClass_project'.
	packageName := projectName , '-Core'.
	symbolDictionary := Rowan image
		newOrExistingSymbolDictionaryNamed: self _sampleSymbolDictionaryName1.

	className := self _unmanagedClassName.

	project := self
		_createLoadedProject: projectName
		packageName: packageName
		symbolDictionaryName: symbolDictionary name.

	stream := ReadStreamPortable
		on:
			(self
				_simpleGsFilein: className
				instVarNames: '#(#xxx)'
				symbolDictionaryName: symbolDictionary name asString).

	((Rowan globalNamed: 'GsFileIn') newFromStream: stream) doFileIn.

	unmanagedClass := Rowan globalNamed: className.
	self assert: unmanagedClass notNil.
	self assert: ((Rowan globalNamed: className) new perform: #'foo') = 1.
	self assert: ((Rowan globalNamed: className) new perform: #'bar') = 2.
	self assert: unmanagedClass rowanPackageName = Rowan unpackagedName.
	self
		assert:
			(unmanagedClass compiledMethodAt: #'foo') rowanPackageName
				= Rowan unpackagedName.
	self
		assert:
			(unmanagedClass compiledMethodAt: #'bar') rowanPackageName
				= Rowan unpackagedName.
	self assert: project audit isEmpty
]

{ #category : 'tests' }
RwUnmanagedClassesV2Test >> testUnmanagedFilein_02 [
	"File in a managed class and unmanaged methods (includes removeAllMethods)"

	| className managedClass project projectName packageName symbolDictionary stream audit details |
	projectName := 'PackagedClass_project'.
	packageName := projectName , '-Core'.
	symbolDictionary := Rowan image
		newOrExistingSymbolDictionaryNamed: self _sampleSymbolDictionaryName1.

	className := self _unmanagedClassName.

	project := self
		_createLoadedProject: projectName
		packageName: packageName
		symbolDictionaryName: symbolDictionary name.

	stream := ReadStreamPortable
		on:
			(self
				_simpleGsFileinUnManagedMethods: className
				instVarNames: '#(#xxx)'
				symbolDictionaryName: symbolDictionary name asString).

	managedClass := Object
		rwSubclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName
		options: #().

	self assert: managedClass notNil.

	((Rowan globalNamed: 'GsFileIn') newFromStream: stream) doFileIn.

	self assert: managedClass == (Rowan globalNamed: className).
	self assert: ((Rowan globalNamed: className) new perform: #'foo') = 1.
	self assert: ((Rowan globalNamed: className) new perform: #'bar') = 2.
	self assert: managedClass rowanPackageName = packageName.
	self
		assert:
			(managedClass compiledMethodAt: #'foo') rowanPackageName = Rowan unpackagedName.
	self
		assert:
			(managedClass compiledMethodAt: #'bar') rowanPackageName = Rowan unpackagedName.
	self deny: (audit := project audit) isEmpty.
	self assert: ((audit at: packageName) at: className) size = 2.
	details := ((audit at: packageName) at: className)
		collect: [ :each | each message ].
	self assert: (details includes: 'Missing loaded method: UnmanagedTestClass>>foo').
	self assert: (details includes: 'Missing loaded method: UnmanagedTestClass>>bar')
]

{ #category : 'tests' }
RwUnmanagedClassesV2Test >> testUnmanagedFilein_03 [
	"File in a managed class and unmanaged methods (without removeAllMethods)"

	| className managedClass project projectName packageName symbolDictionary stream audit details |
	projectName := 'PackagedClass_project'.
	packageName := projectName , '-Core'.
	symbolDictionary := Rowan image
		newOrExistingSymbolDictionaryNamed: self _sampleSymbolDictionaryName1.

	className := self _unmanagedClassName.

	project := self
		_createLoadedProject: projectName
		packageName: packageName
		symbolDictionaryName: symbolDictionary name.

	stream := ReadStreamPortable
		on:
			(self
				_simpleGsFileinUnManagedMethods: className
				instVarNames: '#(#xxx)'
				symbolDictionaryName: symbolDictionary name asString).

	managedClass := Object
		rwSubclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName
		options: #().

	self assert: managedClass notNil.

	((Rowan globalNamed: 'GsFileIn') newFromStream: stream)
		setEnableRemoveAll: false;
		doFileIn.

	self assert: managedClass == (Rowan globalNamed: className).
	self assert: ((Rowan globalNamed: className) new perform: #'foo') = 1.
	self assert: ((Rowan globalNamed: className) new perform: #'bar') = 2.
	self assert: managedClass rowanPackageName = packageName.
	self
		assert:
			(managedClass compiledMethodAt: #'foo') rowanPackageName = Rowan unpackagedName.
	self
		assert:
			(managedClass compiledMethodAt: #'bar') rowanPackageName = Rowan unpackagedName.
	self deny: (audit := project audit) isEmpty.
	self assert: ((audit at: packageName) at: className) size = 2.
	details := ((audit at: packageName) at: className)
		collect: [ :each | each message ].
	self assert: (details includes: 'Missing loaded method: UnmanagedTestClass>>foo').
	self assert: (details includes: 'Missing loaded method: UnmanagedTestClass>>bar')
]

{ #category : 'tests' }
RwUnmanagedClassesV2Test >> testUnmanagedFilein_04 [
	"File in a managed class with a managed method and an unmanaged method (without removeAllMethods)"

	| className managedClass project projectName packageName symbolDictionary stream audit details managedMethod x |
	projectName := 'PackagedClass_project'.
	packageName := projectName , '-Core'.
	symbolDictionary := Rowan image
		newOrExistingSymbolDictionaryNamed: self _sampleSymbolDictionaryName1.

	className := self _unmanagedClassName.

	project := self
		_createLoadedProject: projectName
		packageName: packageName
		symbolDictionaryName: symbolDictionary name.

	stream := ReadStreamPortable
		on:
			(self
				_simpleGsFilein_foo_UnManaged: className
				instVarNames: '#(#xxx)'
				symbolDictionaryName: symbolDictionary name asString).

	managedClass := Object
		rwSubclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName
		options: #().

	self assert: managedClass notNil.

	managedMethod := managedClass
		rwCompileExtensionMethod: 'bar ^2'
		category: 'managed'
		packageName: packageName.

	self assert: (managedClass categoryOfSelector: #'bar') = #'managed'.
	self assert: managedMethod rowanPackageName equals: packageName.

	((Rowan globalNamed: 'GsFileIn') newFromStream: stream)
		setEnableRemoveAll: false;
		doFileIn.

	self assert: managedClass == (Rowan globalNamed: className).
	self assert: ((Rowan globalNamed: className) new perform: #'foo') = 1.
	self assert: ((Rowan globalNamed: className) new perform: #'bar') = 2.
	self assert: managedClass rowanPackageName = packageName.
	self
		assert:
			(managedClass compiledMethodAt: #'foo') rowanPackageName = Rowan unpackagedName.
	self
		assert:
			(x := (managedClass compiledMethodAt: #'bar') rowanPackageName) = packageName.
	self
		assert:
			(x := (Rowan globalNamed: className) categoryOfSelector: #'foo') = #'accessing'.
	self
		assert:
			((Rowan globalNamed: className) categoryOfSelector: #'bar') = #'accessing'.
	self deny: (audit := project audit) isEmpty.
	self assert: ((audit at: packageName) at: className) size = 1.
	details := ((audit at: packageName) at: className)
		collect: [ :each | each message ].
	self assert: (details includes: 'Missing loaded method: UnmanagedTestClass>>foo')
]

{ #category : 'tests' }
RwUnmanagedClassesV2Test >> testUnmanagedFilein_05 [
	"File in a managed class with a managed method, an extension method and an unmanaged method (without removeAllMethods)"

	| className managedClass project projectName packageName1 packageName2 symbolDictionary stream audit details fileinString |
	projectName := 'PackagedClass_project'.
	packageName1 := projectName , '-Core'.
	packageName2 := projectName , '-Extensions'.
	symbolDictionary := Rowan image
		newOrExistingSymbolDictionaryNamed: self _sampleSymbolDictionaryName1.

	className := self _unmanagedClassName.

	project := self
		_createLoadedProject: projectName
		packageNames:
			{packageName1.
			packageName2}
		symbolDictionaryName: symbolDictionary name.

	fileinString := (self
		_simpleGsFilein_foo_UnManaged: className
		instVarNames: '#(#xxx)'
		symbolDictionaryName: symbolDictionary name asString)
		,
			'
category: accessing
method
baz ^3
%
'.
	stream := ReadStreamPortable on: fileinString.

	managedClass := Object
		rwSubclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	self assert: managedClass notNil.

	managedClass
		rwCompileExtensionMethod: 'bar ^2'
			category: 'managed'
			packageName: packageName1;
		rwCompileExtensionMethod: 'baz ^3'
			category: 'extension'
			packageName: packageName2;
		yourself.

	self assert: (managedClass categoryOfSelector: #'bar') = #'managed'.
	self assert: (managedClass categoryOfSelector: #'baz') = #'extension'.

	((Rowan globalNamed: 'GsFileIn') newFromStream: stream)
		setEnableRemoveAll: false;
		doFileIn.

	self assert: managedClass == (Rowan globalNamed: className).
	self assert: ((Rowan globalNamed: className) new perform: #'foo') = 1.
	self assert: ((Rowan globalNamed: className) new perform: #'bar') = 2.
	self assert: ((Rowan globalNamed: className) new perform: #'baz') = 3.
	self assert: managedClass rowanPackageName = packageName1.
	self
		assert:
			(managedClass compiledMethodAt: #'foo') rowanPackageName = Rowan unpackagedName.
	self
		assert:
			((Rowan globalNamed: className) categoryOfSelector: #'foo') = #'accessing'.
	self
		assert:
			(managedClass compiledMethodAt: #'bar') rowanPackageName = packageName1.
	self
		assert:
			((Rowan globalNamed: className) categoryOfSelector: #'bar') = #'accessing'.
	self
		assert:
			(managedClass compiledMethodAt: #'baz') rowanPackageName = packageName2.
	self
		assert:
			((Rowan globalNamed: className) categoryOfSelector: #'bar') = #'accessing'.
	self deny: (audit := project audit) isEmpty.
	self assert: (audit at: packageName2 ifAbsent: [  ]) isNil.
	self assert: ((audit at: packageName1) at: className) size = 1.
	details := ((audit at: packageName1) at: className)
		collect: [ :each | each message ].
	self assert: (details includes: 'Missing loaded method: UnmanagedTestClass>>foo')
]

{ #category : 'tests' }
RwUnmanagedClassesV2Test >> testUnpackageManagedClass_01 [
	"unpackage a managed class with packaged and unpackaged methods"

	| className managedClass project projectName packageName symbolDictionary unmanagedMethod managedMethod x  |
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
		category: packageName
		options: #().
	unmanagedMethod := managedClass
		compileMethod: 'foo ^1'
		dictionaries: Rowan image symbolList
		category: #'unmanaged'
	   intoMethodDict: nil
	   intoCategories: nil
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

	managedClass rwUnpackageClass.

	self assert: ((Rowan globalNamed: className) new perform: #'foo') = 1.
	self assert: ((Rowan globalNamed: className) new perform: #'bar') = 2.

	self assert: (x := managedClass rowanPackageName) = Rowan unpackagedName.
	self assert: (x := managedMethod rowanPackageName) = Rowan unpackagedName.
	self assert: (x := unmanagedMethod rowanPackageName) = Rowan unpackagedName.
	self assert: (x := (Rowan projectNamed: projectName) audit) isEmpty.
]

{ #category : 'tests' }
RwUnmanagedClassesV2Test >> testUnpackageManagedMethod_01 [
	"unpackage the method in unpackaged class"

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

	unmanagedClass rwUnpackageMethod: #bar.

	self
		_validateAuditResults: project audit;
		_validateAuditResults: anotherProject audit.
	self assert: (Rowan globalNamed: className) notNil.
	self assert: ((Rowan globalNamed: className) new perform: #'bar') = 2.
	self assert: (x := managedMethod rowanPackageName) = Rowan unpackagedName
]
