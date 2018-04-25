Class {
	#name : 'RwRowanProjectIssuesTest',
	#superclass : 'RwBrowserToolTest',
	#category : 'Rowan-Tests'
}

{ #category : 'private-issue 24' }
RwRowanProjectIssuesTest >> _addClassNamed: className toPackageNamed: packageName inProjectNamed: projectName [

	^ self _addClassNamed: className toPackageNamed: packageName inProjectNamed: projectName validate: true

]

{ #category : 'private-issue 24' }
RwRowanProjectIssuesTest >> _addClassNamed: className toPackageNamed: packageName inProjectNamed: projectName validate: validate [

	"validate the dirty state of project before and after class is added"

	| project package theClass |

	project := RwProject newNamed: projectName.
	package := RwPackage newNamed: packageName.

	validate ifTrue: [ 
		self deny: project isDirty.
		self deny: package isDirty ].

	theClass := Object
		rwSubclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName
		options: #().

	validate ifTrue: [ 
		self assert: project isDirty.
		self assert: package isDirty ].

	^theClass

]

{ #category : 'private-issue 24' }
RwRowanProjectIssuesTest >> _addOrUpdateMethod: methodSource forBehavior: theBehavior inPackage: packageName inProjectNamed: projectName [

	^ self _addOrUpdateMethod: methodSource forBehavior: theBehavior inPackage: packageName inProjectNamed: projectName validate: true

]

{ #category : 'private-issue 24' }
RwRowanProjectIssuesTest >> _addOrUpdateMethod: methodSource forBehavior: theBehavior inPackage: packageName inProjectNamed: projectName validate: validate [

	"validate the dirty state of project before and after method is added or updated"

	| project package |

	project := RwProject newNamed: projectName.
	package := RwPackage newNamed: packageName.

	validate ifTrue: [ 
		self deny: project isDirty.
		self deny: package isDirty].

	theBehavior
		rwCompileMethod: methodSource
		category:  '*' , packageName asLowercase.

	validate ifTrue: [ 
		self assert: project isDirty.
		self assert: package isDirty ].

]

{ #category : 'private-issue 24' }
RwRowanProjectIssuesTest >> _addPackageNamed: packageName toProjectNamed: projectName [

	^ self _addPackageNamed: packageName toProjectNamed: projectName validate: true

]

{ #category : 'private-issue 24' }
RwRowanProjectIssuesTest >> _addPackageNamed: packageName toProjectNamed: projectName validate: validate [

	"validate the dirty state of project before and after add package call"

	| project |

	project := RwProject newNamed: projectName.

	validate ifTrue: [ self deny: project isDirty ].

	Rowan projectTools browser addPackageNamed: packageName toProjectNamed: projectName.

	validate ifTrue: [ self assert: project isDirty ].

]

{ #category : 'private-issue 24' }
RwRowanProjectIssuesTest >> _createLoadedProjectNamed: projectName root: rootPath [

	^ self _createLoadedProjectNamed: projectName root: rootPath validate: true

]

{ #category : 'private-issue 24' }
RwRowanProjectIssuesTest >> _createLoadedProjectNamed: projectName root: rootPath validate: validate [

	| projectDefinition project |

	(Rowan loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	projectDefinition := self
		_standardProjectDefinition: projectName
		packageNames: #()
		defaultSymbolDictName:self _symbolDictionaryName
		comment:
			'Basic project ', projectName printString.

	self
		handleConfirmationDuring: [
			Rowan projectTools create 
				createPackageProjectFor: projectDefinition 
				format: 'tonel' 
				root:rootPath 
				repoPath: 'src' ].

	Rowan projectTools load loadProjectDefinition: projectDefinition.

	project := RwProject newNamed: projectName.

	validate ifTrue: [ self assert: project isDirty ]. "a project is dirty if it has changes that are not written to disk"

]

{ #category : 'private-issue 24' }
RwRowanProjectIssuesTest >> _removeClassNamed: className fromProjectNamed: projectName [

	^ self _removeClassNamed: className fromProjectNamed: projectName validate: true

]

{ #category : 'private-issue 24' }
RwRowanProjectIssuesTest >> _removeClassNamed: className fromProjectNamed: projectName validate: validate [

	"validate the dirty state of project before and after class is added"

	| project package |

	project := RwProject newNamed: projectName.
	package := RwPackage newNamed: (Rowan globalNamed: className) rowanPackageName.

	validate ifTrue: [ 
		self deny: project isDirty.
		self deny: package isDirty ].

	Rowan projectTools browser removeClassNamed: className.

	validate ifTrue: [ 
		self assert: project isDirty.
		self assert: package isDirty ].

]

{ #category : 'private-issue 24' }
RwRowanProjectIssuesTest >> _removePackageNamed: packageName from: projectName [

	^ self _removePackageNamed: packageName from: projectName validate: true

]

{ #category : 'private-issue 24' }
RwRowanProjectIssuesTest >> _removePackageNamed: packageName from: projectName validate: validate [

	"validate the dirty state of project before and after remove package call"

	| project |

	project := RwProject newNamed: projectName.

	validate ifTrue: [ self deny: project isDirty ].

	Rowan projectTools browser removePackageNamed: packageName.

	(Rowan image loadedPackageNamed: packageName ifAbsent: [])
		ifNotNil: [:loadedPackage | self assert: false description: 'unexpected loaded package ', packageName printString ].

	validate ifTrue: [ self assert: project isDirty ].

]

{ #category : 'private-issue 24' }
RwRowanProjectIssuesTest >> _removeSelector: selector fromBehavior: theBehavior inProjectNamed: projectName [

	^ self _removeSelector: selector fromBehavior: theBehavior inProjectNamed: projectName validate: true

]

{ #category : 'private-issue 24' }
RwRowanProjectIssuesTest >> _removeSelector: selector fromBehavior: theBehavior inProjectNamed: projectName validate: validate [

	"validate the dirty state of project before and after method is removed"

	| project |

	project := RwProject newNamed: projectName.

	validate ifTrue: [ self deny: project isDirty ].

	theBehavior rwRemoveSelector: selector.

	validate ifTrue: [ self assert: project isDirty ].

]

{ #category : 'private-issue 24' }
RwRowanProjectIssuesTest >> _updateClassNamed: className toPackageNamed: packageName inProjectNamed: projectName [

	^ self _updateClassNamed: className toPackageNamed: packageName inProjectNamed: projectName validate: true

]

{ #category : 'private-issue 24' }
RwRowanProjectIssuesTest >> _updateClassNamed: className toPackageNamed: packageName inProjectNamed: projectName validate: validate [

	"validate the dirty state of project before and after class is added"

	| project package theClass |

	project := RwProject newNamed: projectName.
	package := RwPackage newNamed: packageName.

	validate ifTrue: [ 
		self deny: project isDirty.
		self deny: package isDirty ].

	theClass := Object
		rwSubclass: className
		instVarNames: #(ivar1)
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName
		options: #().

	validate ifTrue: [ 
		self assert: project isDirty.
		self assert: package isDirty ].

	^theClass

]

{ #category : 'private-issue 24' }
RwRowanProjectIssuesTest >> _writeProjectNamed: projectName [

	^ self _writeProjectNamed: projectName validate: true

]

{ #category : 'private-issue 24' }
RwRowanProjectIssuesTest >> _writeProjectNamed: projectName validate: validate [

	"validate the dirty state of project before and after write"

	| project |

	project := RwProject newNamed: projectName.

	validate ifTrue: [ self assert: project isDirty ]. "a project is dirty if it has changes that are not written to disk"

	Rowan projectTools write writeProjectNamed: projectName.

	validate ifTrue: [ self deny: project isDirty ]. "a project is not dirty if it has been written to disk"

]

{ #category : 'tests-issue 24' }
RwRowanProjectIssuesTest >> testIssue24_addProject [

	| projectName |
	projectName := 'Issue24_addProject'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject/'.

	self _writeProjectNamed: projectName

]

{ #category : 'tests-issue 24' }
RwRowanProjectIssuesTest >> testIssue24_addRemovePackage [

	| projectName packageName |
	projectName := 'Issue24_addRemovePackage'.
	packageName := 'Issue24-Core'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject/'.

	self _writeProjectNamed: projectName.

	self _addPackageNamed: packageName toProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	self _removePackageNamed: packageName from: projectName.

	self _writeProjectNamed: projectName.

]

{ #category : 'tests-issue 24' }
RwRowanProjectIssuesTest >> testIssue24_addUpdateRemoveClass [

	| projectName packageName className  |
	projectName := 'Issue24_addUpdateRemoveClass'.
	packageName := 'Issue24-Core'.
	className := 'Issue24Class'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject/'.

	self _writeProjectNamed: projectName.

	self _addPackageNamed: packageName toProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	self _addClassNamed: className toPackageNamed: packageName inProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	self _updateClassNamed: className toPackageNamed: packageName inProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	self _removeClassNamed: className fromProjectNamed: projectName.

	self _writeProjectNamed: projectName.

]

{ #category : 'tests-issue 24' }
RwRowanProjectIssuesTest >> testIssue24_addUpdateRemoveMethods [

	| projectName packageName className theClass |
	projectName := 'Issue24_addUpdateRemoveMethods'.
	packageName := 'Issue24-Core'.
	className := 'Issue24Class'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject/'.

	self _writeProjectNamed: projectName.

	self _addPackageNamed: packageName toProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	theClass := self _addClassNamed: className toPackageNamed: packageName inProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	self _addOrUpdateMethod:  'foo ^''foo''' forBehavior: theClass inPackage: packageName inProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	self _addOrUpdateMethod:  'foo ^''foo''' forBehavior: theClass class inPackage: packageName inProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	self _addOrUpdateMethod:  'foo ^''bar''' forBehavior: theClass inPackage: packageName inProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	self _addOrUpdateMethod:  'foo ^''bar''' forBehavior: theClass class inPackage: packageName inProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	self _removeSelector: #foo fromBehavior: theClass inProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	self _removeSelector: #foo fromBehavior: theClass class inProjectNamed: projectName.

	self _writeProjectNamed: projectName.

]

{ #category : 'tests-issue 24' }
RwRowanProjectIssuesTest >> testIssue24_loadProject [

	| projectName packageName className theClass project |
	projectName := 'Issue24_loadProject'.
	packageName := 'Issue24-Core'.
	className := 'Issue24Class'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject/'  validate: false.
	self _addPackageNamed: packageName toProjectNamed: projectName  validate: false.
	theClass := self _addClassNamed: className toPackageNamed: packageName inProjectNamed: projectName  validate: false.
	self _addOrUpdateMethod:  'foo ^''foo''' forBehavior: theClass inPackage: packageName inProjectNamed: projectName  validate: false.
	self _addOrUpdateMethod:  'foo ^''foo''' forBehavior: theClass class inPackage: packageName inProjectNamed: projectName  validate: false.
	self _writeProjectNamed: projectName  validate: false.

	project := RwProject newNamed: projectName.

	self deny: project isDirty.

	Rowan projectTools load loadProjectNamed: projectName.

	self deny: project isDirty.

	self _addOrUpdateMethod:  'foo ^''bar''' forBehavior: theClass inPackage: packageName inProjectNamed: projectName validate: false.
	self _addOrUpdateMethod:  'foo ^''bar''' forBehavior: theClass class inPackage: packageName inProjectNamed: projectName validate: false.

	self assert: project isDirty.
	self assert: theClass new foo = 'bar'.

	Rowan projectTools load loadProjectNamed: projectName.

	self deny: project isDirty.

	theClass := Rowan image objectNamed: className.
	self assert: theClass notNil.

	self _removeClassNamed: className fromProjectNamed: projectName.

	theClass := Rowan image objectNamed: className.
	self assert: theClass isNil.

	self assert: project isDirty.

	Rowan projectTools load loadProjectNamed: projectName.

	self deny: project isDirty.

	theClass := Rowan image objectNamed: className.
	self assert: theClass notNil.
	self assert: theClass new foo = 'foo'.

]

{ #category : 'tests - issue 72' }
RwRowanProjectIssuesTest >> testIssue72_addMethod [
	"https://github.com/dalehenrich/Rowan/issues/72"

	| x |
	self 
		should: [
			"add an unpackaged method - expect error"
			Object
				rwCompileMethod: 'bar ^''bar'''
				category: 'accessing' ]
		raise: Error.

	(Object compiledMethodAt: #bar environmentId: 0 otherwise: nil)
		ifNotNil: [ Object removeSelector: #'bar' ].

	[
		"handle notification and abort the operation"
		Object
			rwCompileMethod: 'bar ^''bar'''
			category: 'accessing' ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | "abort"].
	self assert: (x := Object compiledMethodAt: #bar environmentId: 0 otherwise: nil) isNil.

	[
		"handle notification and complete operation"
		Object
			rwCompileMethod: 'bar ^''bar'''
			category: 'accessing' ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | ex resume ].

	"ensure that the new method is not packaged"
	Rowan image 
		loadedMethod: #bar 
		inClassNamed: 'Object' 
		isMeta: false 
		ifFound: [self assert: false description: 'unexpected loaded method' ] 
		ifAbsent: ["expected"].

	self assert: Object new bar = 'bar'.
	Object removeSelector: #'bar'

]

{ #category : 'tests - issue 72' }
RwRowanProjectIssuesTest >> testIssue72_removeClass [
	"https://github.com/dalehenrich/Rowan/issues/72"

	"nothing special really needed when adding/updating an uppackaged class, just use the standard class creation protocol"

	| className browser |
	className := 'Issue72TestClass'.
	Object subclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: self _symbolDictionary
		options: #().
	self assert: (Rowan image objectNamed: className) notNil.

	browser := Rowan projectTools browser.

	self 
		should: [ browser removeClassNamed: className ]
		raise: Error.
	self assert: (Rowan image objectNamed: className) notNil.

	[
		"handle notification and abort the remove"
		browser removeClassNamed: className ]
			on: RwPerformingUnpackagedEditNotification
			do: [:ex | "abort"].
	self assert: (Rowan image objectNamed: className) notNil.

	[
		"handle notification and complete removal"
		browser removeClassNamed: className  ]
			on: RwPerformingUnpackagedEditNotification
			do: [:ex | ex resume ].
	self assert: (Rowan image objectNamed: className) isNil.

]

{ #category : 'tests - issue 72' }
RwRowanProjectIssuesTest >> testIssue72_removeClassWithExtensions [
	"https://github.com/dalehenrich/Rowan/issues/72"

	"Need to worry about removing an unpackaged class that has extension methods"

	| className theClass projectName packageName packageNames |

	className := 'Issue72TestClass'.
	theClass := Object subclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: self _symbolDictionary
		options: #().
	self assert: (Rowan image objectNamed: className) notNil.
	self assert: (Rowan image loadedClassNamed: className ifAbsent: []) isNil.
	self assert: ((Rowan image loadedClassExtensionsForClass: theClass) select: [:each | each isEmpty not ]) isEmpty.

	projectName := 'Issue72Project'.
	packageName := 'Issue72Project-Extensions'.
	packageNames := {packageName}.
	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName
		comment: 'issue 72 project'.

	theClass rwCompileMethod: 'bar ^''bar'''
				category: '*', packageName asLowercase.

	self assert: (Rowan image objectNamed: className) notNil.
	self assert: (Rowan image loadedClassNamed: className ifAbsent: []) isNil.
	self assert: ((Rowan image loadedClassExtensionsForClass: theClass) select: [:each | each isEmpty not ]) notEmpty.

	[
		"handle notification and complete removal"
		Rowan projectTools browser removeClassNamed: className  ]
			on: RwPerformingUnpackagedEditNotification
			do: [:ex | ex resume ].
	self assert: (Rowan image objectNamed: className) isNil.
	self assert: (Rowan image loadedClassNamed: className ifAbsent: []) isNil.
	self assert: ((Rowan image loadedClassExtensionsForClass: theClass) select: [:each | each isEmpty not ]) isEmpty.

]

{ #category : 'tests - issue 72' }
RwRowanProjectIssuesTest >> testIssue72_removeMethod [
	"https://github.com/dalehenrich/Rowan/issues/72"

	[
		"add unpackaged method"
		Object
			rwCompileMethod: 'bar ^''bar'''
			category: 'accessing' ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | ex resume ].
	self assert: Object new bar = 'bar'.

	self 
		should: [
			"attempt to remove existing unpackaged method - expect error"
			Object rwRemoveSelector: #bar ]
		raise: Error.
	self assert: Object new bar = 'bar'.

	[
		"handle notification and abort the remove"
		Object rwRemoveSelector: #bar ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | "abort"].
	self assert: Object new bar = 'bar'.

	[
		"handle notification and complete removal"
		Object rwRemoveSelector: #bar  ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | ex resume ].
	self should: [ Object new bar ] raise: MessageNotUnderstood.

]

{ #category : 'tests - issue 72' }
RwRowanProjectIssuesTest >> testIssue72_updateMethod [
	"https://github.com/dalehenrich/Rowan/issues/72"

	[
		"add unpackaged method"
		Object
			rwCompileMethod: 'bar ^''bar'''
			category: 'accessing' ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | ex resume ].
	self assert: Object new bar = 'bar'.

	self 
		should: [
			"attempt to update existing unpackaged method - expect error"
			Object
				rwCompileMethod: 'bar ^''foo'''
				category: 'accessing' ]
		raise: Error.
	self assert: Object new bar = 'bar'.

	[
		"handle notification and abort the operation"
		Object
			rwCompileMethod: 'bar ^''foo'''
			category: 'accessing' ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | "abort"].
	self assert: Object new bar = 'bar'.

	[
		"handle notification and complete operation"
		Object
			rwCompileMethod: 'bar ^''foo'''
			category: 'accessing' ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | ex resume ].
	self assert: Object new bar = 'foo'.

	Object removeSelector: #'bar'

]

{ #category : 'tests-issue 91' }
RwRowanProjectIssuesTest >> testIssue91 [
	"add class in one package, add extension in another package, new version of class, 
		update extension method - boom?"

	"https://github.com/dalehenrich/Rowan/issues/91"

	| projectName packageNames packageName1 packageName2 className testClass testInstance newClass fooMethod |
	projectName := 'Issue91'.
	packageName1 := 'Issue91-Core'.
	packageName2 := 'Issue91-Extensions'.
	packageNames := {packageName1. packageName2}.
	className := 'Issue91Class'.

	{projectName}
		do: [ :name | 
			(Rowan loadedProjectNamed: name ifAbsent: [  ])
				ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

	self
		_loadStandardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment:
			'This project is being used to reproduce the problem reported in Issue #91.'
		className: className
		inPackageName: packageName1.

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	self assert: testClass civar1 = 1.
	self assert: testClass cvar1 = 2.
	testInstance := testClass new.
	self assert: testInstance ivar1 isNil.

	testClass
		rwCompileMethod: 'foo ^''foo'''
		category: '*' , packageName2 asLowercase. "create extension method"

	newClass := Object rwSubclass: 'Issue91Class'
		instVarNames: #( ivar1 ivar2)
		classVars: #( Cvar1)
		classInstVars: #( civar1)
		poolDictionaries: #()
		category: 'Simple Things'
		packageName: 'Issue91-Core'
		options: #().

	self assert: newClass ~~ testClass.

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	self assert: testClass civar1 = 1.
	self assert: testClass cvar1 = 2.
	testInstance := testClass new.
	self assert: testInstance ivar1 isNil.
	self assert: testInstance foo = 'foo'.

	fooMethod := testClass
		rwCompileMethod: 'foo ^''bar'''
		category: '*' , packageName2 asLowercase. "create extension method"

	self assert: testInstance foo = 'bar'.
	self assert: fooMethod rowanPackageName = packageName2.

]
