Class {
	#name : 'RwUnmanagedClassesV2Test',
	#superclass : 'RwAbstractV2Test',
	#category : 'Rowan-TestsV2'
}

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
RwUnmanagedClassesV2Test >> testPackagedClass_01 [
	"Use filein class creation against packaged class with no modifications (inverse of testUnManagedClass01"

	| unmanagedPackageName className unmanagedClass managedClass x |
	unmanagedPackageName := Rowan unpackagedPackagePrefix , 'UserGlobals'.

	className := self _unmanagedClassName.

	(Rowan globalNamed: className)
		ifNotNil: [ 
			[ Rowan projectTools browse removeClassNamed: className ]
				on: RwPerformingUnpackagedEditNotification
				do: [ :ex | ex resume ] ].

	managedClass := Object
		rwSubclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: nil
		packageName: unmanagedPackageName
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
		inDictionary: (Rowan globalNamed: 'UserGlobals')
		options: #().
	self assert: unmanagedClass notNil.

	self assert: unmanagedClass == managedClass.
	self assert: unmanagedClass rowanPackageName = unmanagedPackageName.
	self
		_validateAuditResults: (Rowan projectNamed: Rowan unpackagedProjectName) audit.
	self assert: (x := unmanagedClass rowanPackageName) = unmanagedPackageName
]

{ #category : 'tests' }
RwUnmanagedClassesV2Test >> testPackagedClassNewVersion_01 [
	"Use filein class creation against packaged class and create new version of class (inverse of testUnManagedClassNewVersion01"

	| unmanagedPackageName className unmanagedClass managedClass x |
	unmanagedPackageName := Rowan unpackagedPackagePrefix , 'UserGlobals'.

	className := self _unmanagedClassName.

	(Rowan globalNamed: className)
		ifNotNil: [ 
			[ Rowan projectTools browse removeClassNamed: className ]
				on: RwPerformingUnpackagedEditNotification
				do: [ :ex | ex resume ] ].

	managedClass := Object
		rwSubclass: className
		instVarNames: #(#'xxx' #'yyy')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: nil
		packageName: unmanagedPackageName
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
		inDictionary: (Rowan globalNamed: 'UserGlobals')
		options: #().
	self assert: unmanagedClass notNil.

	self assert: unmanagedClass ~~ managedClass.
	self
		_validateAuditResults: (Rowan projectNamed: Rowan unpackagedProjectName) audit.
	self assert: (x := unmanagedClass rowanPackageName) = unmanagedPackageName
]

{ #category : 'tests' }
RwUnmanagedClassesV2Test >> testUnmanagedClass_01 [
	"Use project browser class creation expression to package an unmanaged class"

	| unmanagedPackageName className unmanagedClass managedClass x |
	unmanagedPackageName := Rowan unpackagedPackagePrefix , 'UserGlobals'.

	className := self _unmanagedClassName.

	(Rowan globalNamed: className)
		ifNotNil: [ 
			[ Rowan projectTools browser removeClassNamed: className ]
				on: RwPerformingUnpackagedEditNotification
				do: [ :ex | ex resume ] ].

	unmanagedClass := Object
		subclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: (Rowan globalNamed: 'UserGlobals')
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
		packageName: unmanagedPackageName
		constraints: #()
		options: #().

	self assert: unmanagedClass == managedClass.
	self
		_validateAuditResults: (Rowan projectNamed: Rowan unpackagedProjectName) audit.
	self assert: (x := unmanagedClass rowanPackageName) = unmanagedPackageName
]

{ #category : 'tests' }
RwUnmanagedClassesV2Test >> testUnmanagedClass_02 [
	"Use project load to package the class"

	| unmanagedPackageName className unmanagedClass managedClass project x |
	unmanagedPackageName := Rowan unpackagedPackagePrefix , 'UserGlobals'.

	className := self _unmanagedClassName.

	(Rowan globalNamed: className)
		ifNotNil: [ 
			[ Rowan projectTools browser removeClassNamed: className ]
				on: RwPerformingUnpackagedEditNotification
				do: [ :ex | ex resume ] ].

	unmanagedClass := Object
		subclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: (Rowan globalNamed: 'UserGlobals')
		options: #().
	self assert: unmanagedClass notNil.
	self assert: (Rowan globalNamed: className) notNil.

	project := (Rowan projectNamed: Rowan unpackagedProjectName) defined.
	(project packageNamed: unmanagedPackageName)
		addClassNamed: className
		super: 'Object'
		instvars: #('xxx').
	project load.

	managedClass := Rowan globalNamed: className.
	self assert: unmanagedClass == managedClass.
	self
		_validateAuditResults: (Rowan projectNamed: Rowan unpackagedProjectName) audit.
	self assert: (x := unmanagedClass rowanPackageName) = unmanagedPackageName
]

{ #category : 'tests' }
RwUnmanagedClassesV2Test >> testUnmanagedClass_03 [
	"Use project browser class creation expression to package an unmanaged class with a superclass of nil"

	| unmanagedPackageName className unmanagedClass managedClass x |
	unmanagedPackageName := Rowan unpackagedPackagePrefix , 'UserGlobals'.

	className := self _unmanagedClassName.

	(Rowan globalNamed: className)
		ifNotNil: [ 
			[ Rowan projectTools browser removeClassNamed: className ]
				on: RwPerformingUnpackagedEditNotification
				do: [ :ex | ex resume ] ].

	unmanagedClass := nil
		subclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: (Rowan globalNamed: 'UserGlobals')
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
		packageName: unmanagedPackageName
		constraints: #()
		options: #().

	self assert: unmanagedClass == managedClass.
	self
		_validateAuditResults: (Rowan projectNamed: Rowan unpackagedProjectName) audit.
	self assert: (x := unmanagedClass rowanPackageName) = unmanagedPackageName
]

{ #category : 'tests' }
RwUnmanagedClassesV2Test >> testUnmanagedClass_04 [
	"Use class definition in a different project to package an unmanaged class"

	| unmanagedPackageName className unmanagedClass managedClass x projectName componentName packageName project |
	unmanagedPackageName := Rowan unpackagedPackagePrefix , 'UserGlobals'.

	className := self _unmanagedClassName.
	projectName := 'ManagedProject_test'.
	packageName := projectName , '-Core'.
	componentName := 'Core'.

	(Rowan globalNamed: className)
		ifNotNil: [ 
			[ Rowan projectTools browser removeClassNamed: className ]
				on: RwPerformingUnpackagedEditNotification
				do: [ :ex | ex resume ] ].

	unmanagedClass := Object
		subclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: (Rowan globalNamed: 'UserGlobals')
		options: #().

	self assert: unmanagedClass notNil.
	self assert: (Rowan globalNamed: className) notNil.

	project := Rowan newProjectNamed: projectName.
	(project
		addNewComponentNamed: 'Core';
		addPackageNamed: packageName toComponentNamed: componentName)
		addClassNamed: className super: 'Object' instvars: #(#'xxx');
		yourself.

	project load.

	managedClass := Rowan globalNamed: className.
	self assert: unmanagedClass == managedClass.
	self
		_validateAuditResults: (Rowan projectNamed: Rowan unpackagedProjectName) audit.
	self assert: (x := managedClass rowanPackageName) = packageName
]

{ #category : 'tests' }
RwUnmanagedClassesV2Test >> testUnmanagedClass_05 [
	"Use class definition that creates new version in a different project to package an unmanaged class"

	| unmanagedPackageName className unmanagedClass managedClass x projectName componentName packageName project |
	(Rowan globalNamed: 'UserGlobals') at: #'ConditionalHalt' put: false.
	unmanagedPackageName := Rowan unpackagedPackagePrefix , 'UserGlobals'.

	className := self _unmanagedClassName.
	projectName := 'ManagedProject_test'.
	packageName := projectName , '-Core'.
	componentName := 'Core'.

	(Rowan globalNamed: className)
		ifNotNil: [ 
			[ Rowan projectTools browser removeClassNamed: className ]
				on: RwPerformingUnpackagedEditNotification
				do: [ :ex | ex resume ] ].

	unmanagedClass := Object
		subclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: (Rowan globalNamed: 'UserGlobals')
		options: #().

	self assert: unmanagedClass notNil.
	self assert: (Rowan globalNamed: className) notNil.

	project := Rowan newProjectNamed: projectName.
	(project
		addNewComponentNamed: 'Core';
		addPackageNamed: packageName toComponentNamed: componentName)
		addClassNamed: className super: 'Object' instvars: #('xxx' 'yyy');
		yourself.

	(Rowan globalNamed: 'UserGlobals') at: #'ConditionalHalt' put: false.
	project load.

	managedClass := Rowan globalNamed: className.
	self assert: unmanagedClass ~~ managedClass.
	self
		_validateAuditResults: (Rowan projectNamed: Rowan unpackagedProjectName) audit.
	self assert: (x := managedClass rowanPackageName) = packageName
]

{ #category : 'tests' }
RwUnmanagedClassesV2Test >> testUnmanagedClassNewVersion_01 [
	"Use project browser class creation expression to package and create a new version of an unmanaged class"

	| unmanagedPackageName className unmanagedClass managedClass x |
	unmanagedPackageName := Rowan unpackagedPackagePrefix , 'UserGlobals'.

	className := self _unmanagedClassName.

	(Rowan globalNamed: className)
		ifNotNil: [ 
			[ Rowan projectTools browser removeClassNamed: className ]
				on: RwPerformingUnpackagedEditNotification
				do: [ :ex | ex resume ] ].

	unmanagedClass := Object
		subclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: (Rowan globalNamed: 'UserGlobals')
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
		packageName: unmanagedPackageName
		constraints: #()
		options: #().

	self assert: unmanagedClass ~~ managedClass.
	self
		_validateAuditResults: (Rowan projectNamed: Rowan unpackagedProjectName) audit.
	self assert: (x := managedClass rowanPackageName) = unmanagedPackageName
]

{ #category : 'tests' }
RwUnmanagedClassesV2Test >> testUnmanagedClassNewVersion_02 [
	"Use project load to package and create a new version of an unmanaged class"

	| unmanagedPackageName className unmanagedClass managedClass project x |
	unmanagedPackageName := Rowan unpackagedPackagePrefix , 'UserGlobals'.

	className := self _unmanagedClassName.

	(Rowan globalNamed: className)
		ifNotNil: [ 
			[ Rowan projectTools browser removeClassNamed: className ]
				on: RwPerformingUnpackagedEditNotification
				do: [ :ex | ex resume ] ].

	unmanagedClass := Object
		subclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: (Rowan globalNamed: 'UserGlobals')
		options: #().
	self assert: unmanagedClass notNil.
	self assert: (Rowan globalNamed: className) notNil.

	project := (Rowan projectNamed: Rowan unpackagedProjectName) defined.
	(project packageNamed: unmanagedPackageName)
		addClassNamed: className
		super: 'Object'
		instvars: #('xxx' 'yyy').
	project load.

	managedClass := Rowan globalNamed: className.

	self assert: unmanagedClass ~~ managedClass.
	self
		_validateAuditResults: (Rowan projectNamed: Rowan unpackagedProjectName) audit.
	self assert: (x := managedClass rowanPackageName) = unmanagedPackageName
]
