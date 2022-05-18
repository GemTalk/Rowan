Class {
	#name : 'RwRowanProjectIssuesTest',
	#superclass : 'RwBrowserToolTest',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwRowanProjectIssuesTest >> _addClassNamed: className superclassName: superclassName classVars: classVars toPackageNamed: packageName inProjectNamed: projectName validate: validate [

	"validate the dirty state of project before and after class is added"

	| project package theClass |

	project := RwProject newNamed: projectName.
	package := RwPackage newNamed: packageName.

	validate ifTrue: [ 
		self deny: project isDirty.
		self deny: package isDirty ].

	theClass := (Rowan globalNamed: superclassName)
		rwSubclass: className
		instVarNames: #()
		classVars: classVars
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

{ #category : 'private' }
RwRowanProjectIssuesTest >> _addOrUpdateMethod: methodSource forBehavior: theBehavior inPackage: packageName inProjectNamed: projectName validate: validate [

	"validate the dirty state of project before and after method is added or updated"

	| project package |

	project := RwProject newNamed: projectName.
	package := RwPackage newNamed: packageName.

	validate ifTrue: [ 
		self deny: project isDirty.
		self deny: package isDirty].

	theBehavior rowanPackageName = packageName
		ifTrue: [
			theBehavior
				rwCompileMethod: methodSource
				category:  'other' ]
		ifFalse: [
			theBehavior
				rwCompileMethod: methodSource
				category:  '*' , packageName asLowercase ].

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

{ #category : 'private-issue 150' }
RwRowanProjectIssuesTest >> _cloneGitRepositoryFor: projectName projectUrlString: projectUrlString [

	| rowanSpec projectTools gitRootPath |
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/issues/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: projectUrlString
		gitRootPath: rowanSpec repositoryRootPath , '/test/testRepositories/repos/'
		useSsh: true.

]

{ #category : 'private-issue 24' }
RwRowanProjectIssuesTest >> _createLoadedProjectNamed: projectName packageNames: packageNames root: rootPath symbolDictionaryName: symbolDictionaryName validate: validate [

	| projectDefinition project |

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	projectDefinition := self
		_standardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: symbolDictionaryName
		comment:
			'Basic project ', projectName printString.

	self
		handleConfirmationDuring: [
			Rowan projectTools create 
				createProjectFor: projectDefinition 
				format: 'tonel' 
				root:rootPath 
				configsPath: 'configs'
				repoPath: 'src' 
				specsPath: 'specs' ].

	Rowan projectTools load loadProjectDefinition: projectDefinition.

	project := RwProject newNamed: projectName.

	validate ifTrue: [ self assert: project isDirty ]. "a project is dirty if it has changes that are not written to disk"

]

{ #category : 'private-issue 24' }
RwRowanProjectIssuesTest >> _createLoadedProjectNamed: projectName packageNames: packageNames root: rootPath validate: validate [

	^ self 
		_createLoadedProjectNamed: projectName
		packageNames: packageNames
		root: rootPath 
		symbolDictionaryName: self _symbolDictionaryName 
		validate: validate

]

{ #category : 'private-issue 24' }
RwRowanProjectIssuesTest >> _createLoadedProjectNamed: projectName root: rootPath [

	^ self _createLoadedProjectNamed: projectName root: rootPath validate: true

]

{ #category : 'private-issue 24' }
RwRowanProjectIssuesTest >> _createLoadedProjectNamed: projectName root: rootPath symbolDictionaryName: symbolDictionaryName validate: validate [

	^ self _createLoadedProjectNamed: projectName packageNames: #() root: rootPath symbolDictionaryName: symbolDictionaryName validate: validate

]

{ #category : 'private-issue 24' }
RwRowanProjectIssuesTest >> _createLoadedProjectNamed: projectName root: rootPath validate: validate [

	^ self 
		_createLoadedProjectNamed: projectName 
		root: rootPath 
		symbolDictionaryName: self _symbolDictionaryName 
		validate: validate

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
		classVars: #(CVar1 CVar2)
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

{ #category : 'tests-issue 114' }
RwRowanProjectIssuesTest >> testIssue114_bothModificationsMustShareCommonAfter_1 [

	"https://github.com/dalehenrich/Rowan/issues/114"

	"new class version of superclass and new method in subclass"

	| projectName packageName1 packageName2 theClass1 theClass2 projectDefinition classDefinition |
	projectName := 'Issue114_project'.
	packageName1 := 'Issue114-Core1'.
	packageName2 := 'Issue114-Core2'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject_05/' validate: false.
	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.

	theClass1 := self 
		_addClassNamed: 'Issue114Class_1' 
		superclassName: 'Object' 
		classVars: #(  )
		toPackageNamed: packageName1 
		inProjectNamed: projectName 
		validate: false.

	theClass2 := self 
		_addClassNamed: 'Issue114Class_2' 
		superclassName: 'Issue114Class_1' 
		classVars: #(  )
		toPackageNamed: packageName2
		inProjectNamed: projectName
		validate: false.

	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	classDefinition := (projectDefinition packageNamed: packageName1) classDefinitions at:  'Issue114Class_1'.
	classDefinition instVarNames: #(ivar1).	"new class version"
	classDefinition := (projectDefinition packageNamed: packageName2) classDefinitions at:  'Issue114Class_2'.
	classDefinition addInstanceMethodDefinition:  (RwMethodDefinition
						newForSelector: #'ivar1'
						protocol: 'accessing'
						source: 'ivar1 ^ivar1') .
	[ Rowan projectTools load loadProjectDefinition: projectDefinition ]
			on: RwExecuteClassInitializeMethodsAfterLoadNotification
			do: [:ex | ex resume: true ].

	theClass2 := Rowan globalNamed: 'Issue114Class_2'.
	self assert: theClass2 new ivar1 isNil.
]

{ #category : 'tests-issue 114' }
RwRowanProjectIssuesTest >> testIssue114_bothModificationsMustShareCommonAfter_2 [

	"https://github.com/dalehenrich/Rowan/issues/114"

	"new class version of superclass and changed comment in subclass"

	| projectName packageName1 packageName2 theClass1 theClass2 projectDefinition classDefinition comment |
	projectName := 'Issue114_project'.
	packageName1 := 'Issue114-Core1'.
	packageName2 := 'Issue114-Core2'.
	comment := 'a class comment'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject_06/' validate: false.
	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.

	theClass1 := self 
		_addClassNamed: 'Issue114Class_1' 
		superclassName: 'Object' 
		classVars: #(  )
		toPackageNamed: packageName1 
		inProjectNamed: projectName 
		validate: false.

	theClass2 := self 
		_addClassNamed: 'Issue114Class_2' 
		superclassName: 'Issue114Class_1' 
		classVars: #(  )
		toPackageNamed: packageName2
		inProjectNamed: projectName
		validate: false.

	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	classDefinition := (projectDefinition packageNamed: packageName1) classDefinitions at:  'Issue114Class_1'.
	classDefinition instVarNames: #(ivar1).	"new class version"
	classDefinition := (projectDefinition packageNamed: packageName2) classDefinitions at:  'Issue114Class_2'.
	classDefinition comment: comment.
	[ Rowan projectTools load loadProjectDefinition: projectDefinition ]
			on: RwExecuteClassInitializeMethodsAfterLoadNotification
			do: [:ex | ex resume: true ].

	theClass2 := Rowan globalNamed: 'Issue114Class_2'.
	self assert: theClass2 comment = comment.
]

{ #category : 'tests-issue 114' }
RwRowanProjectIssuesTest >> testIssue114_bothModificationsMustShareCommonAfter_3 [

	"https://github.com/dalehenrich/Rowan/issues/114"

	"existing method in subclass, new class version of superclass and new method in subclass"

	| projectName packageName1 packageName2 theClass1 theClass2 projectDefinition classDefinition |
	projectName := 'Issue114_project'.
	packageName1 := 'Issue114-Core1'.
	packageName2 := 'Issue114-Core2'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject_07/' validate: false.
	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.

	theClass1 := self 
		_addClassNamed: 'Issue114Class_1' 
		superclassName: 'Object' 
		classVars: #(  )
		toPackageNamed: packageName1 
		inProjectNamed: projectName 
		validate: false.

	theClass2 := self 
		_addClassNamed: 'Issue114Class_2' 
		superclassName: 'Issue114Class_1' 
		classVars: #(  )
		toPackageNamed: packageName2
		inProjectNamed: projectName
		validate: false.
	self _addOrUpdateMethod: 'foo ^1' forBehavior: theClass1  inPackage: packageName1 inProjectNamed: projectName validate: false.
	self _addOrUpdateMethod: 'foo ^1' forBehavior: theClass2  inPackage: packageName2 inProjectNamed: projectName validate: false.

	self assert: theClass1 new foo = 1.
	self assert: theClass2 new foo = 1.

	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	classDefinition := (projectDefinition packageNamed: packageName1) classDefinitions at:  'Issue114Class_1'.
	classDefinition instVarNames: #(ivar1).	"new class version"
	classDefinition := (projectDefinition packageNamed: packageName2) classDefinitions at:  'Issue114Class_2'.
	classDefinition addInstanceMethodDefinition:  (RwMethodDefinition
						newForSelector: #'ivar1'
						protocol: 'accessing'
						source: 'ivar1 ^ivar1') .
	[ Rowan projectTools load loadProjectDefinition: projectDefinition ]
			on: RwExecuteClassInitializeMethodsAfterLoadNotification
			do: [:ex | ex resume: true ].

	theClass1 := Rowan globalNamed: 'Issue114Class_1'.
	theClass2 := Rowan globalNamed: 'Issue114Class_2'.
	self assert: theClass1 new foo = 1.
	self assert: theClass2 new foo = 1.
	self assert: theClass2 new ivar1 isNil.
]

{ #category : 'tests-issue 114' }
RwRowanProjectIssuesTest >> testIssue114_bothModificationsMustShareCommonAfter_4 [

	"https://github.com/dalehenrich/Rowan/issues/114"

	"existing method in subclass, new class version of superclass and change of existing method in subclass, new method in subclass"

	| projectName packageName1 packageName2 theClass1 theClass2 projectDefinition classDefinition |
	projectName := 'Issue114_project'.
	packageName1 := 'Issue114-Core1'.
	packageName2 := 'Issue114-Core2'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject_08/' validate: false.
	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.

	theClass1 := self 
		_addClassNamed: 'Issue114Class_1' 
		superclassName: 'Object' 
		classVars: #(  )
		toPackageNamed: packageName1 
		inProjectNamed: projectName 
		validate: false.

	theClass2 := self 
		_addClassNamed: 'Issue114Class_2' 
		superclassName: 'Issue114Class_1' 
		classVars: #(  )
		toPackageNamed: packageName2
		inProjectNamed: projectName
		validate: false.
	self _addOrUpdateMethod: 'foo ^1' forBehavior: theClass1  inPackage: packageName1 inProjectNamed: projectName validate: false.
	self _addOrUpdateMethod: 'foo ^1' forBehavior: theClass2  inPackage: packageName2 inProjectNamed: projectName validate: false.

	self assert: theClass1 new foo = 1.
	self assert: theClass2 new foo = 1.

	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	classDefinition := (projectDefinition packageNamed: packageName1) classDefinitions at:  'Issue114Class_1'.
	classDefinition instVarNames: #(ivar1).	"new class version"
	classDefinition := (projectDefinition packageNamed: packageName2) classDefinitions at:  'Issue114Class_2'.
	classDefinition 
		addInstanceMethodDefinition:  (RwMethodDefinition
						newForSelector: #'ivar1'
						protocol: 'accessing'
						source: 'ivar1 ^ivar1');
		updateInstanceMethodDefinition:  (RwMethodDefinition
						newForSelector: #'foo'
						protocol: 'accessing'
						source: 'foo ^2').

	[ Rowan projectTools load loadProjectDefinition: projectDefinition ]
			on: RwExecuteClassInitializeMethodsAfterLoadNotification
			do: [:ex | ex resume: true ].

	theClass1 := Rowan globalNamed: 'Issue114Class_1'.
	theClass2 := Rowan globalNamed: 'Issue114Class_2'.
	self assert: theClass1 new foo = 1.
	self assert: theClass2 new foo = 2.
	self assert: theClass2 new ivar1 isNil.
]

{ #category : 'tests-issue 114' }
RwRowanProjectIssuesTest >> testIssue114_bothModificationsMustShareCommonAfter_5 [

	"https://github.com/dalehenrich/Rowan/issues/114"

	"existing method in subclass, new class version of superclass and change of existing method in subclass, new method in subclass and change comment in subclass"

	| projectName packageName1 packageName2 theClass1 theClass2 projectDefinition classDefinition |
	projectName := 'Issue114_project'.
	packageName1 := 'Issue114-Core1'.
	packageName2 := 'Issue114-Core2'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject_09/' validate: false.
	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.

	theClass1 := self 
		_addClassNamed: 'Issue114Class_1' 
		superclassName: 'Object' 
		classVars: #(  )
		toPackageNamed: packageName1 
		inProjectNamed: projectName 
		validate: false.

	theClass2 := self 
		_addClassNamed: 'Issue114Class_2' 
		superclassName: 'Issue114Class_1' 
		classVars: #(  )
		toPackageNamed: packageName2
		inProjectNamed: projectName
		validate: false.
	self _addOrUpdateMethod: 'foo ^1' forBehavior: theClass1  inPackage: packageName1 inProjectNamed: projectName validate: false.
	self _addOrUpdateMethod: 'foo ^1' forBehavior: theClass2  inPackage: packageName2 inProjectNamed: projectName validate: false.
	theClass2 comment: 'a comment'.

	self assert: theClass1 new foo = 1.
	self assert: theClass2 new foo = 1.

	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	classDefinition := (projectDefinition packageNamed: packageName1) classDefinitions at:  'Issue114Class_1'.
	classDefinition instVarNames: #(ivar1).	"new class version"
	classDefinition := (projectDefinition packageNamed: packageName2) classDefinitions at:  'Issue114Class_2'.
	classDefinition 
		addInstanceMethodDefinition:  (RwMethodDefinition
						newForSelector: #'ivar1'
						protocol: 'accessing'
						source: 'ivar1 ^ivar1');
		updateInstanceMethodDefinition:  (RwMethodDefinition
						newForSelector: #'foo'
						protocol: 'accessing'
						source: 'foo ^2');
		comment: 'a new comment'.

	[ Rowan projectTools load loadProjectDefinition: projectDefinition ]
			on: RwExecuteClassInitializeMethodsAfterLoadNotification
			do: [:ex | ex resume: true ].

	theClass1 := Rowan globalNamed: 'Issue114Class_1'.
	theClass2 := Rowan globalNamed: 'Issue114Class_2'.
	self assert: theClass1 new foo = 1.
	self assert: theClass2 new foo = 2.
	self assert: theClass2 new ivar1 isNil.
]

{ #category : 'tests-issue 114' }
RwRowanProjectIssuesTest >> testIssue114_classComment [

	"https://github.com/dalehenrich/Rowan/issues/114"

	"new class version of superclass, where both superclass and subclass have a class comment"

	| projectName packageName1 packageName2 theClass1 theClass2 projectDefinition classDefinition |
	projectName := 'Issue114_project'.
	packageName1 := 'Issue114-Core1'.
	packageName2 := 'Issue114-Core2'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject_10/' validate: false.
	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.

	theClass1 := self 
		_addClassNamed: 'Issue114Class_1' 
		superclassName: 'Object' 
		classVars: #(  )
		toPackageNamed: packageName1 
		inProjectNamed: projectName 
		validate: false.
	theClass1 rwComment: 'theClass1 comment'.

	theClass2 := self 
		_addClassNamed: 'Issue114Class_2' 
		superclassName: 'Issue114Class_1' 
		classVars: #(  )
		toPackageNamed: packageName2
		inProjectNamed: projectName
		validate: false.
	theClass2 rwComment: 'theClass2 comment'.

	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	classDefinition := (projectDefinition packageNamed: packageName1) classDefinitions at:  'Issue114Class_1'.
	classDefinition instVarNames: #(ivar1).	"new class version"
	classDefinition := (projectDefinition packageNamed: packageName2) classDefinitions at:  'Issue114Class_2'.

	[ Rowan projectTools load loadProjectDefinition: projectDefinition ]
			on: RwExecuteClassInitializeMethodsAfterLoadNotification
			do: [:ex | ex resume: true ].

	theClass1 := Rowan globalNamed: 'Issue114Class_1'.
	theClass2 := Rowan globalNamed: 'Issue114Class_2'.
	self assert: theClass1 comment = 'theClass1 comment'.
	self assert: theClass2 comment = 'theClass2 comment'.
]

{ #category : 'tests-issue 114' }
RwRowanProjectIssuesTest >> testIssue114_classCommentChange_1 [

	"https://github.com/dalehenrich/Rowan/issues/114"

	"new class version of superclass, where both superclass and subclass have a class comment, change superclass 
		comment"

	| projectName packageName1 packageName2 theClass1 theClass2 projectDefinition classDefinition |
	projectName := 'Issue114_project'.
	packageName1 := 'Issue114-Core1'.
	packageName2 := 'Issue114-Core2'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject_11/' validate: false.
	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.

	theClass1 := self 
		_addClassNamed: 'Issue114Class_1' 
		superclassName: 'Object' 
		classVars: #(  )
		toPackageNamed: packageName1 
		inProjectNamed: projectName 
		validate: false.
	theClass1 rwComment: 'theClass1 comment'.

	theClass2 := self 
		_addClassNamed: 'Issue114Class_2' 
		superclassName: 'Issue114Class_1' 
		classVars: #(  )
		toPackageNamed: packageName2
		inProjectNamed: projectName
		validate: false.
	theClass2 rwComment: 'theClass2 comment'.

	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	classDefinition := (projectDefinition packageNamed: packageName1) classDefinitions at:  'Issue114Class_1'.
	classDefinition 
		instVarNames: #(ivar1);	"new class version"
		comment: 'a new comment'.
	classDefinition := (projectDefinition packageNamed: packageName2) classDefinitions at:  'Issue114Class_2'.

	[ Rowan projectTools load loadProjectDefinition: projectDefinition ]
			on: RwExecuteClassInitializeMethodsAfterLoadNotification
			do: [:ex | ex resume: true ].

	theClass1 := Rowan globalNamed: 'Issue114Class_1'.
	theClass2 := Rowan globalNamed: 'Issue114Class_2'.
	self assert: theClass1 comment = 'a new comment'.
	self assert: theClass2 comment = 'theClass2 comment'.
]

{ #category : 'tests-issue 114' }
RwRowanProjectIssuesTest >> testIssue114_classCommentChange_2 [

	"https://github.com/dalehenrich/Rowan/issues/114"

	"new class version of superclass, where both superclass and subclass have a class comment, change subclass 
		comment"

	| projectName packageName1 packageName2 theClass1 theClass2 projectDefinition classDefinition |
	projectName := 'Issue114_project'.
	packageName1 := 'Issue114-Core1'.
	packageName2 := 'Issue114-Core2'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject_12/' validate: false.
	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.

	theClass1 := self 
		_addClassNamed: 'Issue114Class_1' 
		superclassName: 'Object' 
		classVars: #(  )
		toPackageNamed: packageName1 
		inProjectNamed: projectName 
		validate: false.
	theClass1 rwComment: 'theClass1 comment'.

	theClass2 := self 
		_addClassNamed: 'Issue114Class_2' 
		superclassName: 'Issue114Class_1' 
		classVars: #(  )
		toPackageNamed: packageName2
		inProjectNamed: projectName
		validate: false.
	theClass2 rwComment: 'theClass2 comment'.

	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	classDefinition := (projectDefinition packageNamed: packageName1) classDefinitions at:  'Issue114Class_1'.
	classDefinition 
		instVarNames: #(ivar1).	"new class version"
	classDefinition := (projectDefinition packageNamed: packageName2) classDefinitions at:  'Issue114Class_2'.
	classDefinition
				comment: 'a new comment'.
	[ Rowan projectTools load loadProjectDefinition: projectDefinition ]
			on: RwExecuteClassInitializeMethodsAfterLoadNotification
			do: [:ex | ex resume: true ].

	theClass1 := Rowan globalNamed: 'Issue114Class_1'.
	theClass2 := Rowan globalNamed: 'Issue114Class_2'.
	self assert: theClass1 comment = 'theClass1 comment'.
	self assert: theClass2 comment = 'a new comment'.
]

{ #category : 'tests-issue 114' }
RwRowanProjectIssuesTest >> testIssue114_classCommentChange_3 [

	"https://github.com/dalehenrich/Rowan/issues/114"

	"new class version of superclass, where both superclass and subclass have a class comment, change superclass and change subclass 
		comment"

	| projectName packageName1 packageName2 theClass1 theClass2 projectDefinition classDefinition |
	projectName := 'Issue114_project'.
	packageName1 := 'Issue114-Core1'.
	packageName2 := 'Issue114-Core2'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject_13/' validate: false.
	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.

	theClass1 := self 
		_addClassNamed: 'Issue114Class_1' 
		superclassName: 'Object' 
		classVars: #(  )
		toPackageNamed: packageName1 
		inProjectNamed: projectName 
		validate: false.
	theClass1 rwComment: 'theClass1 comment'.

	theClass2 := self 
		_addClassNamed: 'Issue114Class_2' 
		superclassName: 'Issue114Class_1' 
		classVars: #(  )
		toPackageNamed: packageName2
		inProjectNamed: projectName
		validate: false.
	theClass2 rwComment: 'theClass2 comment'.

	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	classDefinition := (projectDefinition packageNamed: packageName1) classDefinitions at:  'Issue114Class_1'.
	classDefinition 
		instVarNames: #(ivar1);	"new class version"
		comment: 'a new comment'.
	classDefinition := (projectDefinition packageNamed: packageName2) classDefinitions at:  'Issue114Class_2'.
	classDefinition
			comment: 'a new comment'.
	[ Rowan projectTools load loadProjectDefinition: projectDefinition ]
			on: RwExecuteClassInitializeMethodsAfterLoadNotification
			do: [:ex | ex resume: true ].

	theClass1 := Rowan globalNamed: 'Issue114Class_1'.
	theClass2 := Rowan globalNamed: 'Issue114Class_2'.
	self assert: theClass1 comment = 'a new comment'.
	self assert: theClass2 comment = 'a new comment'.
]

{ #category : 'tests-issue 114' }
RwRowanProjectIssuesTest >> testIssue114_methodSourceIsNotTheSame_0 [

	"https://github.com/dalehenrich/Rowan/issues/114"

	"existing method and comment in subclass, new class version of superclass, new method, update old method,
		and change method protocol"

	| projectName packageName1 packageName2 theClass1 theClass2 projectDefinition classDefinition |
	projectName := 'Issue114_project'.
	packageName1 := 'Issue114-Core1'.
	packageName2 := 'Issue114-Core2'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject_14/' validate: false.
	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.

	theClass1 := self 
		_addClassNamed: 'Issue114Class_1' 
		superclassName: 'Object' 
		classVars: #(  )
		toPackageNamed: packageName1 
		inProjectNamed: projectName 
		validate: false.

	theClass2 := self 
		_addClassNamed: 'Issue114Class_2' 
		superclassName: 'Issue114Class_1' 
		classVars: #(  )
		toPackageNamed: packageName2
		inProjectNamed: projectName
		validate: false.
	theClass2 rwComment: 'a comment'.
	self _addOrUpdateMethod: 'foo ^1' forBehavior: theClass1  inPackage: packageName1 inProjectNamed: projectName validate: false.
	self _addOrUpdateMethod: 'foo ^1' forBehavior: theClass2  inPackage: packageName2 inProjectNamed: projectName validate: false.

	self assert: theClass1 new foo = 1.
	self assert: theClass2 new foo = 1.
	self assert: theClass2 comment = 'a comment'.

	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	classDefinition := (projectDefinition packageNamed: packageName1) classDefinitions at:  'Issue114Class_1'.
	classDefinition instVarNames: #(ivar1).	"new class version"
	classDefinition := (projectDefinition packageNamed: packageName2) classDefinitions at:  'Issue114Class_2'.
	classDefinition 
		addInstanceMethodDefinition:  (RwMethodDefinition
						newForSelector: #'ivar1'
						protocol: 'accessing'
						source: 'ivar1 ^ivar1');
		updateInstanceMethodDefinition:  (RwMethodDefinition
						newForSelector: #'foo'
						protocol: 'accessing'
						source: 'foo ^2').
	[ Rowan projectTools load loadProjectDefinition: projectDefinition ]
			on: RwExecuteClassInitializeMethodsAfterLoadNotification
			do: [:ex | ex resume: true ].

	theClass1 := Rowan globalNamed: 'Issue114Class_1'.
	theClass2 := Rowan globalNamed: 'Issue114Class_2'.
	self assert: theClass1 new foo = 1.
	self assert: theClass2 new foo = 2.
	self assert: theClass2 new ivar1 isNil.
]

{ #category : 'tests-issue 114' }
RwRowanProjectIssuesTest >> testIssue114_methodSourceIsNotTheSame_1 [

	"https://github.com/dalehenrich/Rowan/issues/114"

	"existing method and comment in subclass, new class version of superclass, new method, update old method,
		change method protocol and update comment in subclass -- combo"

	| projectName packageName1 packageName2 theClass1 theClass2 projectDefinition classDefinition |
	projectName := 'Issue114_project'.
	packageName1 := 'Issue114-Core1'.
	packageName2 := 'Issue114-Core2'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject_15/' validate: false.
	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.

	theClass1 := self 
		_addClassNamed: 'Issue114Class_1' 
		superclassName: 'Object' 
		classVars: #(  )
		toPackageNamed: packageName1 
		inProjectNamed: projectName 
		validate: false.

	theClass2 := self 
		_addClassNamed: 'Issue114Class_2' 
		superclassName: 'Issue114Class_1' 
		classVars: #(  )
		toPackageNamed: packageName2
		inProjectNamed: projectName
		validate: false.
	theClass2 rwComment: 'a comment'.
	self _addOrUpdateMethod: 'foo ^1' forBehavior: theClass1  inPackage: packageName1 inProjectNamed: projectName validate: false.
	self _addOrUpdateMethod: 'foo ^1' forBehavior: theClass2  inPackage: packageName2 inProjectNamed: projectName validate: false.

	self assert: theClass1 new foo = 1.
	self assert: theClass2 new foo = 1.
	self assert: theClass2 comment = 'a comment'.

	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	classDefinition := (projectDefinition packageNamed: packageName1) classDefinitions at:  'Issue114Class_1'.
	classDefinition instVarNames: #(ivar1).	"new class version"
	classDefinition := (projectDefinition packageNamed: packageName2) classDefinitions at:  'Issue114Class_2'.
	classDefinition 
		addInstanceMethodDefinition:  (RwMethodDefinition
						newForSelector: #'ivar1'
						protocol: 'accessing'
						source: 'ivar1 ^ivar1');
		updateInstanceMethodDefinition:  (RwMethodDefinition
						newForSelector: #'foo'
						protocol: 'accessing'
						source: 'foo ^2');
		comment: 'a new comment'.
	[ Rowan projectTools load loadProjectDefinition: projectDefinition ]
			on: RwExecuteClassInitializeMethodsAfterLoadNotification
			do: [:ex | ex resume: true ].

	theClass1 := Rowan globalNamed: 'Issue114Class_1'.
	theClass2 := Rowan globalNamed: 'Issue114Class_2'.
	self assert: theClass1 new foo = 1.
	self assert: theClass2 new foo = 2.
	self assert: theClass2 new ivar1 isNil.
	self assert: theClass2 comment = 'a new comment'.
]

{ #category : 'tests-issue 114' }
RwRowanProjectIssuesTest >> testIssue114_methodSourceIsNotTheSame_2 [

	"https://github.com/dalehenrich/Rowan/issues/114"

	"existing method and comment in subclass, new class version of superclass, new method, update 
		old method in superclass and subclass,change method protocol and update comment in subclass -- combo"

	| projectName packageName1 packageName2 theClass1 theClass2 projectDefinition classDefinition |
	projectName := 'Issue114_project'.
	packageName1 := 'Issue114-Core1'.
	packageName2 := 'Issue114-Core2'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject_16/' validate: false.
	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.

	theClass1 := self 
		_addClassNamed: 'Issue114Class_1' 
		superclassName: 'Object' 
		classVars: #(  )
		toPackageNamed: packageName1 
		inProjectNamed: projectName 
		validate: false.

	theClass2 := self 
		_addClassNamed: 'Issue114Class_2' 
		superclassName: 'Issue114Class_1' 
		classVars: #(  )
		toPackageNamed: packageName2
		inProjectNamed: projectName
		validate: false.
	theClass2 rwComment: 'a comment'.
	self _addOrUpdateMethod: 'foo ^1' forBehavior: theClass1  inPackage: packageName1 inProjectNamed: projectName validate: false.
	self _addOrUpdateMethod: 'foo ^1' forBehavior: theClass2  inPackage: packageName2 inProjectNamed: projectName validate: false.

	self assert: theClass1 new foo = 1.
	self assert: theClass2 new foo = 1.
	self assert: theClass2 comment = 'a comment'.

	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	classDefinition := (projectDefinition packageNamed: packageName1) classDefinitions at:  'Issue114Class_1'.
	classDefinition 
		instVarNames: #(ivar1);	"new class version"
		updateInstanceMethodDefinition:  (RwMethodDefinition
						newForSelector: #'foo'
						protocol: 'accessing'
						source: 'foo ^2').
	classDefinition := (projectDefinition packageNamed: packageName2) classDefinitions at:  'Issue114Class_2'.
	classDefinition 
		addInstanceMethodDefinition:  (RwMethodDefinition
						newForSelector: #'ivar1'
						protocol: 'accessing'
						source: 'ivar1 ^ivar1');
		updateInstanceMethodDefinition:  (RwMethodDefinition
						newForSelector: #'foo'
						protocol: 'accessing'
						source: 'foo ^2');
		comment: 'a new comment'.
	[ Rowan projectTools load loadProjectDefinition: projectDefinition ]
			on: RwExecuteClassInitializeMethodsAfterLoadNotification
			do: [:ex | ex resume: true ].

	theClass1 := Rowan globalNamed: 'Issue114Class_1'.
	theClass2 := Rowan globalNamed: 'Issue114Class_2'.
	self assert: theClass1 new foo = 2.
	self assert: theClass2 new foo = 2.
	self assert: theClass2 new ivar1 isNil.
	self assert: theClass2 comment = 'a new comment'.
]

{ #category : 'tests-issue 114' }
RwRowanProjectIssuesTest >> testIssue114_missingClassModification [

	"https://github.com/dalehenrich/Rowan/issues/114"

	"RwGsClassVersioningSymbolDictPatch>>updatePatchesForNewClassVersion:"

	| projectName packageName1 packageName2 theClass1 theClass2 projectDefinition classDefinition |
	projectName := 'Issue114_project'.
	packageName1 := 'Issue114-Core1'.
	packageName2 := 'Issue114-Core2'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject_17/' validate: false.
	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.

	theClass1 := self 
		_addClassNamed: 'Issue114Class_1' 
		superclassName: 'Object' 
		classVars: #(  )
		toPackageNamed: packageName1 
		inProjectNamed: projectName 
		validate: false.

	theClass2 := self 
		_addClassNamed: 'Issue114Class_2' 
		superclassName: 'Object' 
		classVars: #(  )
		toPackageNamed: packageName2
		inProjectNamed: projectName
		validate: false.
	self _addOrUpdateMethod: 'foo ^1' forBehavior: theClass1  inPackage: packageName1 inProjectNamed: projectName validate: false.
	self _addOrUpdateMethod: 'foo ^1' forBehavior: theClass2  inPackage: packageName2 inProjectNamed: projectName validate: false.

	self assert: theClass1 new foo = 1.
	self assert: theClass2 new foo = 1.

	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	classDefinition := (projectDefinition packageNamed: packageName1) classDefinitions at:  'Issue114Class_1'.
	classDefinition instVarNames: #(ivar1).	"new class version"
	classDefinition updateInstanceMethodDefinition:  (RwMethodDefinition
						newForSelector: #'foo'
						protocol: 'accessing'
						source: 'foo ^2').
	classDefinition updateInstanceMethodDefinition:  (RwMethodDefinition
						newForSelector: #'ivar1'
						protocol: 'accessing'
						source: 'ivar1 ^ivar1').
	classDefinition := (projectDefinition packageNamed: packageName2) classDefinitions at:  'Issue114Class_2'.
	classDefinition instVarNames: #().
	classDefinition updateInstanceMethodDefinition:  (RwMethodDefinition
						newForSelector: #'foo'
						protocol: 'accessing'
						source: 'foo ^ 2') .
	[ Rowan projectTools load loadProjectDefinition: projectDefinition ]
			on: RwExecuteClassInitializeMethodsAfterLoadNotification
			do: [:ex | ex resume: true ].

	theClass1 := Rowan globalNamed: 'Issue114Class_1'.
	theClass2 := Rowan globalNamed: 'Issue114Class_2'.
	self assert: theClass1 new foo = 2.
	self assert: theClass1 new ivar1 isNil.
	self assert: theClass2 new foo = 2.
]

{ #category : 'tests-issue 123' }
RwRowanProjectIssuesTest >> testIssue123_definition_based_moveExistingClassWithMethodsAndSubclassesToNewPackageAndNewClassVersion [
	"https://github.com/dalehenrich/Rowan/issues/123"

"
    Create class Issue91 in Package RowanSample1-XXX.
    Add non-extension method .
     Using the class definition template, create a new version of Issue91 with new inst var 'abc' in RowanSample1-Extensions
"

	| projectName packageName1 packageName2 className1 className2 projectDefinition classDefinition1 classDefinition2 packageDefinition
		projectSetDefinition theClass1 theClass2 ivNames oldClass1 oldClass2 |
	projectName := 'Issue123_Project'.
	packageName1 := 'Issue123-Core'.
	packageName2 := 'Issue123-Extensions'.
	className1 := 'Issue123Class'.
	className2 := 'Issue123SubClass'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project definitions"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	classDefinition1 := RwClassDefinition
		newForClassNamed: className1
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition1
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'foo ^1').
	classDefinition2 := RwClassDefinition
		newForClassNamed: className2
		super: className1
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: ''
		pools: #()
		type: 'normal'.

	packageDefinition := projectDefinition packageNamed: packageName1.
	packageDefinition 
		addClassDefinition: classDefinition1;
		addClassDefinition: classDefinition2.

"load project1"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validation"
	theClass1 := Rowan globalNamed: className1.
	theClass2 := Rowan globalNamed: className2.
	self assert: theClass1 new foo = 1.
	self assert: theClass1 category = packageName1.
	self assert: theClass1 rowanPackageName = packageName1.
	self assert: (theClass1 categoryOfSelector: #foo) asString = 'accessing'.
	self assert: (theClass1 compiledMethodAt: #foo) rowanPackageName = packageName1.
	self assert: theClass2 new foo = 1.
	self assert: theClass2 category = packageName1.
	self assert: theClass2 rowanPackageName = packageName1.

"create new version of project"
	classDefinition1 := packageDefinition classDefinitions at: className1.
	packageDefinition removeClassDefinition: classDefinition1.

	ivNames := classDefinition1 instVarNames copy.
	ivNames add: 'ivar1'.
	classDefinition1 instVarNames: ivNames.
	classDefinition1 category: packageName2.

	packageDefinition := projectDefinition packageNamed: packageName2.
	packageDefinition addClassDefinition: classDefinition1.

"load project1"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validation"
	oldClass1 := theClass1.
	oldClass2 := theClass2.
	theClass1 := Rowan globalNamed: className1.
	theClass2 := Rowan globalNamed: className2.
	self assert: oldClass1 ~~ theClass1.
	self assert: oldClass2 ~~ theClass2.
	self assert: theClass1 new foo = 1.
	self assert: theClass1 new foo = 1.
	self assert: theClass1 category = packageName2.
	self assert: theClass1 rowanPackageName = packageName2.
	self assert: (theClass1 categoryOfSelector: #foo) asString = 'accessing'.
	self assert: (theClass1 compiledMethodAt: #foo) rowanPackageName = packageName2.
	self assert: theClass2 new foo = 1.
	self assert: theClass2 category = packageName1.
	self assert: theClass2 rowanPackageName = packageName1.

]

{ #category : 'tests-issue 123' }
RwRowanProjectIssuesTest >> testIssue123_moveExistingClassWithExtensionMethodsAndSubclassesToNewPackageAndNewClassVersion [
	"https://github.com/dalehenrich/Rowan/issues/123"

"
    Create class Issue91 in Package RowanSample1-XXX.
    Add non-extension method .
     Using the class definition template, create a new version of Issue91 with new inst var 'abc' in RowanSample1-Extensions
"

	| projectName packageName1 packageName2 className1 className2 theClass1  theClass2 |
	projectName := 'Issue123_Project'.
	packageName1 := 'Issue123-Core'.
	packageName2 := 'Issue123-Extensions'.
	className1 := 'Issue123Class'.
	className2 := 'Issue123SubClass'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject_18/'.

	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.

	theClass1 := Object
		rwSubclass: className1
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	theClass2 := (Rowan globalNamed: className1)
		rwSubclass: className2
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	self _addOrUpdateMethod: 'foo ^1' forBehavior: theClass1  inPackage: packageName2 inProjectNamed: projectName validate: false.

	self assert: theClass1 new foo = 1.
	self assert: theClass1 category = packageName1.
	self assert: theClass1 rowanPackageName = packageName1.
	self assert: (theClass1 categoryOfSelector: #foo) asString = ('*' , packageName2 asLowercase).
	self assert: (theClass1 compiledMethodAt: #foo) rowanPackageName = packageName2.
	self assert: theClass2 new foo = 1.
	self assert: theClass2 category = packageName1.
	self assert: theClass2 rowanPackageName = packageName1.

	theClass1 := Object
		rwSubclass: className1
		instVarNames: #( ivar1 )
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName2
		options: #().

	self assert: theClass1 new foo = 1.
	self assert: theClass1 new foo = 1.
	self assert: theClass1 category = packageName2.
	self assert: theClass1 rowanPackageName = packageName2.
	self assert: (theClass1 categoryOfSelector: #foo) asString = ('*' , packageName2 asLowercase).
	self assert: (theClass1 compiledMethodAt: #foo) rowanPackageName = packageName2.
	self assert: (Rowan globalNamed: className2) ~~ theClass2.
	theClass2 := Rowan globalNamed: className2.
	self assert: theClass2 new foo = 1.
	self assert: theClass2 category = packageName1.
	self assert: theClass2 rowanPackageName = packageName1.
]

{ #category : 'tests-issue 123' }
RwRowanProjectIssuesTest >> testIssue123_moveExistingClassWithExtensionMethodsToNewPackage [
	"https://github.com/dalehenrich/Rowan/issues/123"

"
    Create class Issue91 in Package RowanSample1-XXX.
    Add extension method category, *rowansample1-extensions, in class Issue91
    Add method extendedMethod in new category.
    Verify by Show Changes that RowanSample1-Extensions package shows the new method was added.
    Using the class definition template, move the class to RowanSample1-Extensions
"

	| projectName packageName1 packageName2 className theClass  |
	projectName := 'Issue123_Project'.
	packageName1 := 'Issue123-Core'.
	packageName2 := 'Issue123-Extensions'.
	className := 'Issue123Class'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject_19/'.

	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.

	theClass := Object
		rwSubclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	self _addOrUpdateMethod: 'foo ^1' forBehavior: theClass  inPackage: packageName2 inProjectNamed: projectName validate: false.

	self assert: theClass new foo = 1.
	self assert: theClass category = packageName1.
	self assert: theClass rowanPackageName = packageName1.
	self assert: (theClass categoryOfSelector: #foo) asString = ('*' , packageName2 asLowercase).
	self assert: (theClass compiledMethodAt: #foo) rowanPackageName = packageName2.

	theClass := Object
		rwSubclass: className
		instVarNames: #( )
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName2
		options: #().

	self assert: theClass category = packageName2.
	self assert: theClass rowanPackageName = packageName2.
	self assert: theClass new foo = 1.
	self assert: (theClass categoryOfSelector: #foo) asString = ('*' , packageName2 asLowercase).
	self assert: (theClass compiledMethodAt: #foo) rowanPackageName = packageName2.
]

{ #category : 'tests-issue 123' }
RwRowanProjectIssuesTest >> testIssue123_moveExistingClassWithExtensionMethodsToNewPackageAndNewClassVersion [
	"https://github.com/dalehenrich/Rowan/issues/123"

"
    Create class Issue91 in Package RowanSample1-XXX.
    Add extension method category, *rowansample1-extensions, in class Issue91
    Add method extendedMethod in new category.
    Verify by Show Changes that RowanSample1-Extensions package shows the new method was added.
    Using the class definition template, create a new version of Issue91 with new inst var 'abc' in RowanSample1-Extensions
    Verify Issue91 has moved to RowanSample1-Extensions package.
    Bug is that method extendedMethod was not brought into the new class version. No methods are shown.
"

	| projectName packageName1 packageName2 className theClass  |
	projectName := 'Issue123_Project'.
	packageName1 := 'Issue123-Core'.
	packageName2 := 'Issue123-Extensions'.
	className := 'Issue123Class'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject_20/'.

	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.

	theClass := Object
		rwSubclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	self _addOrUpdateMethod: 'foo ^1' forBehavior: theClass  inPackage: packageName2 inProjectNamed: projectName validate: false.

	self assert: theClass new foo = 1.
	self assert: theClass category = packageName1.
	self assert: theClass rowanPackageName = packageName1.
	self assert: (theClass categoryOfSelector: #foo) asString = ('*' , packageName2 asLowercase).
	self assert: (theClass compiledMethodAt: #foo) rowanPackageName = packageName2.

	theClass := Object
		rwSubclass: className
		instVarNames: #( ivar1 )
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName2
		options: #().

	self assert: theClass new foo = 1.
	self assert: theClass category = packageName2.
	self assert: theClass rowanPackageName = packageName2.
	self assert: (theClass categoryOfSelector: #foo) asString = ('*' , packageName2 asLowercase).
	self assert: (theClass compiledMethodAt: #foo) rowanPackageName = packageName2.
]

{ #category : 'tests-issue 123' }
RwRowanProjectIssuesTest >> testIssue123_moveExistingClassWithMethodsAndSubclassesToNewPackageAndNewClassVersion [
	"https://github.com/dalehenrich/Rowan/issues/123"

"
    Create class Issue91 in Package RowanSample1-XXX.
    Add non-extension method .
     Using the class definition template, create a new version of Issue91 with new inst var 'abc' in RowanSample1-Extensions
"

	| projectName packageName1 packageName2 className1 className2 theClass1  theClass2 x oldClass |
	projectName := 'Issue123_Project'.
	packageName1 := 'Issue123-Core'.
	packageName2 := 'Issue123-Extensions'.
	className1 := 'Issue123Class'.
	className2 := 'Issue123SubClass'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject_21/'.

	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.

	theClass1 := Object
		rwSubclass: className1
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	theClass2 := (Rowan globalNamed: className1)
		rwSubclass: className2
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	self _addOrUpdateMethod: 'foo ^1' forBehavior: theClass1  inPackage: packageName1 inProjectNamed: projectName validate: false.

	self assert: theClass1 new foo = 1.
	self assert: theClass1 category = packageName1.
	self assert: theClass1 rowanPackageName = packageName1.
	self assert: (theClass1 categoryOfSelector: #foo) asString = 'other'.
	self assert: (theClass1 compiledMethodAt: #foo) rowanPackageName = packageName1.
	self assert: theClass2 new foo = 1.
	self assert: theClass2 category = packageName1.
	self assert: theClass2 rowanPackageName = packageName1.

	oldClass := theClass1.
	theClass1 := Object
		rwSubclass: className1
		instVarNames: #( ivar1 )
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName2
		options: #().

	self assert: oldClass ~~ theClass1.
	self assert: theClass1 new foo = 1.
	self assert: theClass1 new foo = 1.
	self assert: theClass1 category = packageName2.
	self assert: theClass1 rowanPackageName = packageName2.
	self assert: (theClass1 categoryOfSelector: #foo) asString = 'other'.
	self assert: (theClass1 compiledMethodAt: #foo) rowanPackageName = packageName2.
	self assert: (x := Rowan globalNamed: className2) ~~ theClass2.
	theClass2 := Rowan globalNamed: className2.
	self assert: theClass2 new foo = 1.
	self assert: theClass2 category = packageName1.
	self assert: theClass2 rowanPackageName = packageName1
]

{ #category : 'tests-issue 123' }
RwRowanProjectIssuesTest >> testIssue123_moveExistingClassWithMethodsToNewPackage [
	"https://github.com/dalehenrich/Rowan/issues/123"

"
    Create class Issue91 in Package RowanSample1-XXX.
    Add non-extension method .
     Using the class definition template, move the class to RowanSample1-Extensions
"

	| projectName packageName1 packageName2 className theClass  |
	projectName := 'Issue123_Project'.
	packageName1 := 'Issue123-Core'.
	packageName2 := 'Issue123-Extensions'.
	className := 'Issue123Class'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject_22/'.

	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.

	theClass := Object
		rwSubclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	self _addOrUpdateMethod: 'foo ^1' forBehavior: theClass  inPackage: packageName1 inProjectNamed: projectName validate: false.

	self assert: theClass new foo = 1.
	self assert: theClass category = packageName1.
	self assert: theClass rowanPackageName = packageName1.
	self assert: (theClass categoryOfSelector: #foo) asString = 'other'.
	self assert: (theClass compiledMethodAt: #foo) rowanPackageName = packageName1.

	theClass := Object
		rwSubclass: className
		instVarNames: #( )
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName2
		options: #().

	self assert: theClass new foo = 1.
	self assert: theClass category = packageName2.
	self assert: theClass rowanPackageName = packageName2.
	self assert: (theClass categoryOfSelector: #foo) asString = 'other'.
	self assert: (theClass compiledMethodAt: #foo) rowanPackageName = packageName2.
]

{ #category : 'tests-issue 123' }
RwRowanProjectIssuesTest >> testIssue123_moveExistingClassWithMethodsToNewPackageAndNewClassVersion [
	"https://github.com/dalehenrich/Rowan/issues/123"

"
    Create class Issue91 in Package RowanSample1-XXX.
    Add non-extension method .
     Using the class definition template, create a new version of Issue91 with new inst var 'abc' in RowanSample1-Extensions
"

	| projectName packageName1 packageName2 className theClass oldClass |
	projectName := 'Issue123_Project'.
	packageName1 := 'Issue123-Core'.
	packageName2 := 'Issue123-Extensions'.
	className := 'Issue123Class'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject_23/'.

	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.

	theClass := Object
		rwSubclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	self _addOrUpdateMethod: 'foo ^1' forBehavior: theClass  inPackage: packageName1 inProjectNamed: projectName validate: false.

	self assert: theClass new foo = 1.
	self assert: theClass category = packageName1.
	self assert: theClass rowanPackageName = packageName1.
	self assert: (theClass categoryOfSelector: #foo) asString = 'other'.
	self assert: (theClass compiledMethodAt: #foo) rowanPackageName = packageName1.

	oldClass := theClass.
	theClass := Object
		rwSubclass: className
		instVarNames: #( ivar1 )
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName2
		options: #().

	self assert: oldClass ~= theClass.
	self assert: theClass new foo = 1.
	self assert: theClass new foo = 1.
	self assert: theClass category = packageName2.
	self assert: theClass rowanPackageName = packageName2.
	self assert: (theClass categoryOfSelector: #foo) asString = 'other'.
	self assert: (theClass compiledMethodAt: #foo) rowanPackageName = packageName2.
]

{ #category : 'tests-issue 125' }
RwRowanProjectIssuesTest >> testIssue125_1 [

	"https://github.com/dalehenrich/Rowan/issues/125"

	"move a method to an extension package ... unchanged"

	"RwGsPatchSetBasic does not understand:
		addPatchedExtendedClassMethodProperties:inClass:inPackage:inProject:
		addPatchedExtendedInstanceMethodProperties:inClass:inPackage:inProject:
	"
	| projectName packageName1 packageName2 className theClass  | 
	projectName := 'Issue125_Project'.
	packageName1 := 'Issue125-Core'.
	packageName2 := 'Issue125-Extensions'.
	className := 'Issue125Class'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject_24/'.

	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.

	theClass := Object
		rwSubclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	theClass rwCompileMethod: 'foo ^1' category: 'accessing'.

	self assert: theClass new foo = 1.
	self assert: (theClass categoryOfSelector: #foo) asString = 'accessing'.

	theClass rwCompileMethod: 'foo ^1' category: '*' , packageName2 asLowercase.

	self assert: theClass new foo = 1.
	self assert: (theClass categoryOfSelector: #foo) asString = ('*' , packageName2 asLowercase)
]

{ #category : 'tests-issue 125' }
RwRowanProjectIssuesTest >> testIssue125_2 [

	"https://github.com/dalehenrich/Rowan/issues/125"

	"move a method from an extension method ... unchanged"

	"RwGsPatchSetBasic does not understand:
		addPatchedExtendedClassMethodProperties:inClass:inPackage:inProject:
		addPatchedExtendedInstanceMethodProperties:inClass:inPackage:inProject:
	"
	| projectName packageName1 packageName2 className theClass  x |
	projectName := 'Issue125_Project'.
	packageName1 := 'Issue125-Core'.
	packageName2 := 'Issue125-Extensions'.
	className := 'Issue125Class'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject_25/'.

	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.

	theClass := Object
		rwSubclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	theClass rwCompileMethod: 'foo ^1' category: '*' , packageName2 asLowercase.

	self assert: theClass new foo = 1.
	self assert: (theClass categoryOfSelector: #foo) asString = ('*' , packageName2 asLowercase).

	theClass rwCompileMethod: 'foo ^1' category: 'accessing'.

	self assert: theClass new foo = 1.
	self assert: (x := (theClass categoryOfSelector: #foo) asString) = 'accessing'
]

{ #category : 'tests-issue 125' }
RwRowanProjectIssuesTest >> testIssue125_3 [

	"https://github.com/dalehenrich/Rowan/issues/125"

	"move a method from an extension method in one package ... unchanged to an extension method in another package"

	"RwGsPatchSetBasic does not understand:
		addPatchedExtendedClassMethodProperties:inClass:inPackage:inProject:
		addPatchedExtendedInstanceMethodProperties:inClass:inPackage:inProject:
	"
	| projectName packageName1 packageName2 packageName3 className theClass  |
	projectName := 'Issue125_Project'.
	packageName1 := 'Issue125-Core'.
	packageName2 := 'Issue125-Extensions1'.
	packageName3 := 'Issue125-Extensions2'.
	className := 'Issue125Class'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject_26/'.

	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName3 toProjectNamed: projectName validate: false.

	theClass := Object
		rwSubclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	theClass rwCompileMethod: 'foo ^1' category: '*' , packageName2 asLowercase.

	self assert: theClass new foo = 1.
	self assert: (theClass categoryOfSelector: #foo) asString = ('*' , packageName2 asLowercase).

	theClass rwCompileMethod: 'foo ^1' category:'*' , packageName3 asLowercase.

	self assert: theClass new foo = 1.
	self assert: (theClass categoryOfSelector: #foo) asString = ('*' , packageName3 asLowercase).
]

{ #category : 'tests-issue 165' }
RwRowanProjectIssuesTest >> testIssue165 [

	"https://github.com/dalehenrich/Rowan/issues/165"

	| projectName packageName1 className symDictName theClass theSymbolDict myUserProfile mySymbolList theSymDictIndex | 
	projectName := 'Issue165_Project'.
	packageName1 := 'Issue165-Core'.
	className := 'Issue165Class'.
	symDictName := self _symbolDictionaryName2.

	self 
		_createLoadedProjectNamed: projectName 
		root: '/tmp/rowanIssuesProject_27/' 
		symbolDictionaryName: symDictName 
		validate: false.

	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.

	theClass := Object
		rwSubclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	theClass rwCompileMethod: 'foo ^1' category: 'accessing'.

	theSymbolDict := Rowan globalNamed: symDictName.
	self assert: theSymbolDict class == SymbolDictionary.			"symbol dictionary is accessible in my symbol list"

	self assert: (Rowan projectNames includes: projectName).		"project is visible"
	self assert: (Rowan packageNames includes: packageName1).	"package is visible"
	self assert: (Rowan globalNamed: className) notNil.				"class is visible"

	myUserProfile := System myUserProfile.
	mySymbolList := System myUserProfile symbolList.
	theSymDictIndex := mySymbolList indexOf: theSymbolDict.
	myUserProfile removeDictionaryAt: theSymDictIndex.				"remove symbol dictionary from my symbol list"

	self assert: (Rowan projectNames includes: projectName).		"project is visible"
	self deny: (Rowan packageNames includes: packageName1).	"package is NOT visible"
	self deny: (Rowan globalNamed: className) notNil.				"class is NOT visible"
]

{ #category : 'tests-issue 185' }
RwRowanProjectIssuesTest >> testIssue185_254_move_class_to_package [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"https://github.com/dalehenrich/Rowan/issues/254"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition packageDefinition className projectSetDefinition class |
	projectName := 'Issue185'.
	packageName1 := 'Issue185-Core1'.
	packageName2 := 'Issue185-Core2'.
	className := 'Issue185Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].


	projectDefinition1 := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition_254: projectSetDefinition.

	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.

	projectSetDefinition := RwProjectSetDefinition new.
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName1.
	packageDefinition classDefinitions removeKey: classDefinition key.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition_254: projectSetDefinition.

	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName2.

]

{ #category : 'tests-issue 185' }
RwRowanProjectIssuesTest >> testIssue185_254_move_extension_method_to_new_package_1 [

	"https://github.com/dalehenrich/Rowan/issues/185"
	"https://github.com/dalehenrich/Rowan/issues/254"

	"simplified version of RwRowanProjectIssuesTest>>testIssue41_interactiveMoveInitializeExtensionMethodToPackage"

	"use RwClassExtensionDefinition>>removeKey: to do final update (source changed)--- definition comparison bug (see testIssue206_move_extension_method_to_new_package_1)"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition packageDefinition className projectSetDefinition class
		classExtensionDefinition packageName3 loadedPackage loadedClassExtensions x |
	projectName := 'Issue185'.
	packageName1 := 'Issue185-Core1'.
	packageName2 := 'Issue185-Core2'.
	packageName3 := 'Issue185-Core3'.
	className := 'Issue185Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	projectDefinition1 := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		addPackageNamed: packageName3;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	"create extension method in different package"
	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'mover'
					protocol: '*', packageName2 asLowercase
					source: 'mover ^2').

	packageDefinition := projectDefinition1 packageNamed: packageName2.
	packageDefinition addClassExtension: classExtensionDefinition.

	"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition_254: projectSetDefinition.

	"validate"
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: class new method1 = 1.
	self assert: class new mover = 2.

	self assert: (class categoryOfSelector: #mover) asString = ('*', packageName2 asLowercase).
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName2.

	loadedPackage := Rowan image loadedPackageNamed: packageName2.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: ((loadedClassExtensions at: className) instanceMethodDefinitions includesKey: #mover).

	loadedPackage := Rowan image loadedPackageNamed: packageName3.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: (loadedClassExtensions at: className ifAbsent: []) isNil.

	"move the method to new package"
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition classExtensions removeKey: className.

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'mover'
					protocol: '*', packageName3 asLowercase
					source: 'mover ^3').

	packageDefinition := projectDefinition2 packageNamed: packageName3.
	packageDefinition addClassExtension: classExtensionDefinition.

	"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition_254: projectSetDefinition.

	"validate"
	self assert: class rowanPackageName = packageName1.
	self assert: class new method1 = 1.
	self assert: class new mover = 3.

	self assert: (class categoryOfSelector: #mover) asString = ('*', packageName3 asLowercase).
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName3.

	loadedPackage := Rowan image loadedPackageNamed: packageName3.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: ((loadedClassExtensions at: className) instanceMethodDefinitions includesKey: #mover).

	loadedPackage := Rowan image loadedPackageNamed: packageName2.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self deny: ((x := loadedClassExtensions at: className ifAbsent: [RwGsLoadedSymbolDictClassExtension new initialize]) instanceMethodDefinitions includesKey: #mover).

]

{ #category : 'tests-issue 185' }
RwRowanProjectIssuesTest >> testIssue185_254_move_extension_method_to_new_package_3 [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"simplified version of RwRowanProjectIssuesTest>>testIssue41_interactiveMoveInitializeExtensionMethodToPackage"

	"use RwClassExtensionDefinition>>removeInstanceMethod: to do final update (testIssue185_move_extension_method_to_new_package_1 exposes a bug)"


	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition packageDefinition className projectSetDefinition class
		classExtensionDefinition packageName3 loadedPackage loadedClassExtensions x |
	projectName := 'Issue185'.
	packageName1 := 'Issue185-Core1'.
	packageName2 := 'Issue185-Core2'.
	packageName3 := 'Issue185-Core3'.
	className := 'Issue185Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	projectDefinition1 := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		addPackageNamed: packageName3;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	"create extension method in different package"
	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'mover'
					protocol: '*', packageName2 asLowercase
					source: 'mover ^2').

	packageDefinition := projectDefinition1 packageNamed: packageName2.
	packageDefinition addClassExtension: classExtensionDefinition.

	"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition_254: projectSetDefinition.

	"validate"
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: class new method1 = 1.
	self assert: class new mover = 2.

	self assert: (class categoryOfSelector: #mover) asString = ('*', packageName2 asLowercase).
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName2.

	loadedPackage := Rowan image loadedPackageNamed: packageName2.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: ((loadedClassExtensions at: className) instanceMethodDefinitions includesKey: #mover).

	loadedPackage := Rowan image loadedPackageNamed: packageName3.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: (loadedClassExtensions at: className ifAbsent: []) isNil.

	"move the method to new package"
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	(packageDefinition classExtensions at: className) removeInstanceMethod: #mover.

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'mover'
					protocol: '*', packageName3 asLowercase
					source: 'mover ^2').

	packageDefinition := projectDefinition2 packageNamed: packageName3.
	packageDefinition addClassExtension: classExtensionDefinition.

	"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition_254: projectSetDefinition.

	"validate"
	self assert: class rowanPackageName = packageName1.
	self assert: class new method1 = 1.
	self assert: class new mover = 2.

	self assert: (class categoryOfSelector: #mover) asString = ('*', packageName3 asLowercase).
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName3.

	loadedPackage := Rowan image loadedPackageNamed: packageName3.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: ((loadedClassExtensions at: className) instanceMethodDefinitions includesKey: #mover).

	loadedPackage := Rowan image loadedPackageNamed: packageName2.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self deny: ((x := loadedClassExtensions at: className ifAbsent: [RwGsLoadedSymbolDictClassExtension new initialize]) instanceMethodDefinitions includesKey: #mover).

]

{ #category : 'tests-issue 185' }
RwRowanProjectIssuesTest >> testIssue185_254_move_extension_method_to_new_package_4 [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"simplified version of RwRowanProjectIssuesTest>>testIssue41_interactiveMoveInitializeExtensionMethodToPackage"

	"use RwClassExtensionDefinition>>removeKey: to do final update (source not changed) --- definition comparison bug?"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition packageDefinition className projectSetDefinition class
		classExtensionDefinition packageName3 loadedPackage loadedClassExtensions x |
	projectName := 'Issue185'.
	packageName1 := 'Issue185-Core1'.
	packageName2 := 'Issue185-Core2'.
	packageName3 := 'Issue185-Core3'.
	className := 'Issue185Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	projectDefinition1 := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		addPackageNamed: packageName3;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	"create extension method in different package"
	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'mover'
					protocol: '*', packageName2 asLowercase
					source: 'mover ^2').

	packageDefinition := projectDefinition1 packageNamed: packageName2.
	packageDefinition addClassExtension: classExtensionDefinition.

	"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition_254: projectSetDefinition.

	"validate"
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: class new method1 = 1.
	self assert: class new mover = 2.

	self assert: (class categoryOfSelector: #mover) asString = ('*', packageName2 asLowercase).
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName2.

	loadedPackage := Rowan image loadedPackageNamed: packageName2.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: ((loadedClassExtensions at: className) instanceMethodDefinitions includesKey: #mover).

	loadedPackage := Rowan image loadedPackageNamed: packageName3.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: (loadedClassExtensions at: className ifAbsent: []) isNil.

	"move the method to new package"
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition classExtensions removeKey: className.

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'mover'
					protocol: '*', packageName3 asLowercase
					source: 'mover ^2').

	packageDefinition := projectDefinition2 packageNamed: packageName3.
	packageDefinition addClassExtension: classExtensionDefinition.

	"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition_254: projectSetDefinition.

	"validate"
	self assert: class rowanPackageName = packageName1.
	self assert: class new method1 = 1.
	self assert: class new mover = 2.

	self assert: (class categoryOfSelector: #mover) asString = ('*', packageName3 asLowercase).
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName3.

	loadedPackage := Rowan image loadedPackageNamed: packageName3.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: ((loadedClassExtensions at: className) instanceMethodDefinitions includesKey: #mover).

	loadedPackage := Rowan image loadedPackageNamed: packageName2.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self deny: ((x := loadedClassExtensions at: className ifAbsent: [RwGsLoadedSymbolDictClassExtension new initialize]) instanceMethodDefinitions includesKey: #mover).

]

{ #category : 'tests-issue 185' }
RwRowanProjectIssuesTest >> testIssue185_254_rename_package_move_class_to_symbolDict_3 [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"initial load uses custom symbol dictionary, second load uses default symbol dictionary -- change category of class"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition packageDefinition className projectSetDefinition class
		ar oldClass x y |
	projectName := 'Issue185'.
	packageName1 := 'Issue185-Core'.
	packageName2 := 'Issue185-RenamedCore'.
	className := 'Issue185Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	projectDefinition1 := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		defaultSymbolDictName: self _symbolDictionaryName1;
		setSymbolDictName: self _symbolDictionaryName2 forPackageNamed: packageName1;
		yourself.

	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: ''
		pools: #()
		type: 'normal'.

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition_254: projectSetDefinition.

	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName2 asSymbol.

	projectSetDefinition := RwProjectSetDefinition new.

	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition
		removePackageNamed: packageName1;
		addPackageNamed: packageName2;
		setSymbolDictName: nil forPackageNamed: packageName2.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	classDefinition category: packageName2.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition_254: projectSetDefinition.

	oldClass := class.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName2.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (x := (ar first at: 1) name) = (y := self _symbolDictionaryName1 asSymbol).

	self assert: class == oldClass.

]

{ #category : 'tests-issue 185' }
RwRowanProjectIssuesTest >> testIssue185_254_rename_package_move_class_to_symbolDict_add_remove_and_remove_methods_1 [

	"https://github.com/dalehenrich/Rowan/issues/185"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition packageDefinition className projectSetDefinition class
		ar oldClass x y |
	projectName := 'Issue185'.
	packageName1 := 'Issue185-Core'.
	packageName2 := 'Issue185-RenamedCore'.
	className := 'Issue185Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	projectDefinition1 := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		defaultSymbolDictName: self _symbolDictionaryName1;
		setSymbolDictName: self _symbolDictionaryName2 forPackageNamed: packageName1;
		yourself.

	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1');
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method2'
					protocol: 'accessing'
					source: 'method2 ^2');
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method3'
					protocol: 'accessing'
					source: 'method3 ^3').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition_254: projectSetDefinition.

	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName2 asSymbol.
	self assert: class new method1 = 1.
	self assert: class new method2 = 2.

	projectSetDefinition := RwProjectSetDefinition new.

	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition
		removePackageNamed: packageName1;
		addPackageNamed: packageName2;
		setSymbolDictName: nil forPackageNamed: packageName2.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	classDefinition category: packageName2.
	packageDefinition addClassDefinition: classDefinition.

	classDefinition
		removeInstanceMethod: #method2;
		removeInstanceMethod: #method3;
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method3'
					protocol: 'accessing'
					source: 'method3 ^5');
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method4'
					protocol: 'accessing'
					source: 'method4 ^4').

	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition_254: projectSetDefinition.

	oldClass := class.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName2.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (x := (ar first at: 1) name) = (y := self _symbolDictionaryName1 asSymbol).
	self assert: class new method1 = 1.
	self should: [ class new method2 = 2 ] raise: MessageNotUnderstood.
	self assert: (x := class new method3) = 5.
	self assert: class new method4 = 4.

	self assert: class == oldClass.

]

{ #category : 'tests-issue 185' }
RwRowanProjectIssuesTest >> testIssue185_254_rename_package_move_class_to_symbolDict_add_remove_and_remove_methods_2 [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"minimal version of #testIssue185_rename_package_move_class_to_symbolDict_add_remove_and_remove_methods_1 isolating a particular failure
		that I'm trying to resolve"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition packageDefinition 
		className projectSetDefinition class x |
	projectName := 'Issue185'.
	packageName1 := 'Issue185-Core'.
	packageName2 := 'Issue185-RenamedCore'.
	className := 'Issue185Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	projectDefinition1 := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		defaultSymbolDictName: self _symbolDictionaryName1;
		setSymbolDictName: self _symbolDictionaryName2 forPackageNamed: packageName1;
		yourself.

	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method3'
					protocol: 'accessing'
					source: 'method3 ^3').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition_254: projectSetDefinition.

	projectSetDefinition := RwProjectSetDefinition new.
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition
		removePackageNamed: packageName1;
		addPackageNamed: packageName2;
		setSymbolDictName: nil forPackageNamed: packageName2.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	classDefinition category: packageName2.
	packageDefinition addClassDefinition: classDefinition.

	classDefinition
		removeInstanceMethod: #method3;
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method3'
					protocol: 'accessing'
					source: 'method3 ^5').

	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition_254: projectSetDefinition.

	class := Rowan globalNamed: className.

	self assert: (x := class new method3) = 5.

]

{ #category : 'tests-issue 185' }
RwRowanProjectIssuesTest >> testIssue185_move_class_to_package [

	"https://github.com/dalehenrich/Rowan/issues/185"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition packageDefinition className projectSetDefinition class |
	projectName := 'Issue185'.
	packageName1 := 'Issue185-Core1'.
	packageName2 := 'Issue185-Core2'.
	className := 'Issue185Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].


	projectDefinition1 := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.

	projectSetDefinition := RwProjectSetDefinition new.
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName1.
	packageDefinition classDefinitions removeKey: classDefinition key.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName2.

]

{ #category : 'tests-issue 185' }
RwRowanProjectIssuesTest >> testIssue185_move_extension_method_to_new_package_1 [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"simplified version of RwRowanProjectIssuesTest>>testIssue41_interactiveMoveInitializeExtensionMethodToPackage"

	"use RwClassExtensionDefinition>>removeKey: to do final update (source changed)--- definition comparison bug (see testIssue206_move_extension_method_to_new_package_1)"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition packageDefinition className projectSetDefinition class
		classExtensionDefinition packageName3 loadedPackage loadedClassExtensions x |
	projectName := 'Issue185'.
	packageName1 := 'Issue185-Core1'.
	packageName2 := 'Issue185-Core2'.
	packageName3 := 'Issue185-Core3'.
	className := 'Issue185Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	projectDefinition1 := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		addPackageNamed: packageName3;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	"create extension method in different package"
	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'mover'
					protocol: '*', packageName2 asLowercase
					source: 'mover ^2').

	packageDefinition := projectDefinition1 packageNamed: packageName2.
	packageDefinition addClassExtension: classExtensionDefinition.

	"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	"validate"
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: class new method1 = 1.
	self assert: class new mover = 2.

	self assert: (class categoryOfSelector: #mover) asString = ('*', packageName2 asLowercase).
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName2.

	loadedPackage := Rowan image loadedPackageNamed: packageName2.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: ((loadedClassExtensions at: className) instanceMethodDefinitions includesKey: #mover).

	loadedPackage := Rowan image loadedPackageNamed: packageName3.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: (loadedClassExtensions at: className ifAbsent: []) isNil.

	"move the method to new package"
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition classExtensions removeKey: className.

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'mover'
					protocol: '*', packageName3 asLowercase
					source: 'mover ^3').

	packageDefinition := projectDefinition2 packageNamed: packageName3.
	packageDefinition addClassExtension: classExtensionDefinition.

	"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	"validate"
	self assert: class rowanPackageName = packageName1.
	self assert: class new method1 = 1.
	self assert: class new mover = 3.

	self assert: (class categoryOfSelector: #mover) asString = ('*', packageName3 asLowercase).
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName3.

	loadedPackage := Rowan image loadedPackageNamed: packageName3.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: ((loadedClassExtensions at: className) instanceMethodDefinitions includesKey: #mover).

	loadedPackage := Rowan image loadedPackageNamed: packageName2.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self deny: ((x := loadedClassExtensions at: className ifAbsent: [RwGsLoadedSymbolDictClassExtension new initialize]) instanceMethodDefinitions includesKey: #mover).

]

{ #category : 'tests-issue 185' }
RwRowanProjectIssuesTest >> testIssue185_move_extension_method_to_new_package_2 [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"simplified version of RwRowanProjectIssuesTest>>testIssue41_interactiveMoveInitializeExtensionMethodToPackage"

	"use Behavior>>rwCompileMethod:category: to do final update (testIssue185_move_extension_method_to_new_package_1 exposes a bug)"

	| projectName  packageName1 packageName2 projectDefinition1 classDefinition packageDefinition className projectSetDefinition class
		classExtensionDefinition packageName3 loadedPackage loadedClassExtensions x |
	projectName := 'Issue185'.
	packageName1 := 'Issue185-Core1'.
	packageName2 := 'Issue185-Core2'.
	packageName3 := 'Issue185-Core3'.
	className := 'Issue185Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	projectDefinition1 := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		addPackageNamed: packageName3;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	"create extension method in different package"
	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'mover'
					protocol: '*', packageName2 asLowercase
					source: 'mover ^2').

	packageDefinition := projectDefinition1 packageNamed: packageName2.
	packageDefinition addClassExtension: classExtensionDefinition.

	"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	"validate"
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: class new method1 = 1.
	self assert: class new mover = 2.

	self assert: (class categoryOfSelector: #mover) asString = ('*', packageName2 asLowercase).
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName2.

	loadedPackage := Rowan image loadedPackageNamed: packageName2.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: ((loadedClassExtensions at: className) instanceMethodDefinitions includesKey: #mover).

	loadedPackage := Rowan image loadedPackageNamed: packageName3.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: (loadedClassExtensions at: className ifAbsent: []) isNil.

	"move the method to new package"
	class 
		rwCompileMethod: 'mover ^2'
		category:  '*' , packageName3 asLowercase.

	"validate"
	self assert: class rowanPackageName = packageName1.
	self assert: class new method1 = 1.
	self assert: class new mover = 2.

	self assert: (class categoryOfSelector: #mover) asString = ('*', packageName3 asLowercase).
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName3.

	loadedPackage := Rowan image loadedPackageNamed: packageName3.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: ((loadedClassExtensions at: className) instanceMethodDefinitions includesKey: #mover).

	loadedPackage := Rowan image loadedPackageNamed: packageName2.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self deny: ((x := loadedClassExtensions at: className ifAbsent: [RwGsLoadedSymbolDictClassExtension new initialize]) instanceMethodDefinitions includesKey: #mover).

]

{ #category : 'tests-issue 185' }
RwRowanProjectIssuesTest >> testIssue185_move_extension_method_to_new_package_3 [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"simplified version of RwRowanProjectIssuesTest>>testIssue41_interactiveMoveInitializeExtensionMethodToPackage"

	"use RwClassExtensionDefinition>>removeInstanceMethod: to do final update (testIssue185_move_extension_method_to_new_package_1 exposes a bug)"


	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition packageDefinition className projectSetDefinition class
		classExtensionDefinition packageName3 loadedPackage loadedClassExtensions x |
	projectName := 'Issue185'.
	packageName1 := 'Issue185-Core1'.
	packageName2 := 'Issue185-Core2'.
	packageName3 := 'Issue185-Core3'.
	className := 'Issue185Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	projectDefinition1 := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		addPackageNamed: packageName3;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	"create extension method in different package"
	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'mover'
					protocol: '*', packageName2 asLowercase
					source: 'mover ^2').

	packageDefinition := projectDefinition1 packageNamed: packageName2.
	packageDefinition addClassExtension: classExtensionDefinition.

	"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	"validate"
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: class new method1 = 1.
	self assert: class new mover = 2.

	self assert: (class categoryOfSelector: #mover) asString = ('*', packageName2 asLowercase).
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName2.

	loadedPackage := Rowan image loadedPackageNamed: packageName2.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: ((loadedClassExtensions at: className) instanceMethodDefinitions includesKey: #mover).

	loadedPackage := Rowan image loadedPackageNamed: packageName3.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: (loadedClassExtensions at: className ifAbsent: []) isNil.

	"move the method to new package"
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	(packageDefinition classExtensions at: className) removeInstanceMethod: #mover.

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'mover'
					protocol: '*', packageName3 asLowercase
					source: 'mover ^2').

	packageDefinition := projectDefinition2 packageNamed: packageName3.
	packageDefinition addClassExtension: classExtensionDefinition.

	"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	"validate"
	self assert: class rowanPackageName = packageName1.
	self assert: class new method1 = 1.
	self assert: class new mover = 2.

	self assert: (class categoryOfSelector: #mover) asString = ('*', packageName3 asLowercase).
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName3.

	loadedPackage := Rowan image loadedPackageNamed: packageName3.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: ((loadedClassExtensions at: className) instanceMethodDefinitions includesKey: #mover).

	loadedPackage := Rowan image loadedPackageNamed: packageName2.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self deny: ((x := loadedClassExtensions at: className ifAbsent: [RwGsLoadedSymbolDictClassExtension new initialize]) instanceMethodDefinitions includesKey: #mover).

]

{ #category : 'tests-issue 185' }
RwRowanProjectIssuesTest >> testIssue185_move_extension_method_to_new_package_4 [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"simplified version of RwRowanProjectIssuesTest>>testIssue41_interactiveMoveInitializeExtensionMethodToPackage"

	"use RwClassExtensionDefinition>>removeKey: to do final update (source not changed) --- definition comparison bug?"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition packageDefinition className projectSetDefinition class
		classExtensionDefinition packageName3 loadedPackage loadedClassExtensions x |
	projectName := 'Issue185'.
	packageName1 := 'Issue185-Core1'.
	packageName2 := 'Issue185-Core2'.
	packageName3 := 'Issue185-Core3'.
	className := 'Issue185Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	projectDefinition1 := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		addPackageNamed: packageName3;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	"create extension method in different package"
	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'mover'
					protocol: '*', packageName2 asLowercase
					source: 'mover ^2').

	packageDefinition := projectDefinition1 packageNamed: packageName2.
	packageDefinition addClassExtension: classExtensionDefinition.

	"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	"validate"
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: class new method1 = 1.
	self assert: class new mover = 2.

	self assert: (class categoryOfSelector: #mover) asString = ('*', packageName2 asLowercase).
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName2.

	loadedPackage := Rowan image loadedPackageNamed: packageName2.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: ((loadedClassExtensions at: className) instanceMethodDefinitions includesKey: #mover).

	loadedPackage := Rowan image loadedPackageNamed: packageName3.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: (loadedClassExtensions at: className ifAbsent: []) isNil.

	"move the method to new package"
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition classExtensions removeKey: className.

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'mover'
					protocol: '*', packageName3 asLowercase
					source: 'mover ^2').

	packageDefinition := projectDefinition2 packageNamed: packageName3.
	packageDefinition addClassExtension: classExtensionDefinition.

	"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	"validate"
	self assert: class rowanPackageName = packageName1.
	self assert: class new method1 = 1.
	self assert: class new mover = 2.

	self assert: (class categoryOfSelector: #mover) asString = ('*', packageName3 asLowercase).
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName3.

	loadedPackage := Rowan image loadedPackageNamed: packageName3.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: ((loadedClassExtensions at: className) instanceMethodDefinitions includesKey: #mover).

	loadedPackage := Rowan image loadedPackageNamed: packageName2.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self deny: ((x := loadedClassExtensions at: className ifAbsent: [RwGsLoadedSymbolDictClassExtension new initialize]) instanceMethodDefinitions includesKey: #mover).

]

{ #category : 'tests-issue 185' }
RwRowanProjectIssuesTest >> testIssue185_rename_package [

	"https://github.com/dalehenrich/Rowan/issues/185"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition packageDefinition className projectSetDefinition class |
	projectName := 'Issue185'.
	packageName1 := 'Issue185-Core'.
	packageName2 := 'Issue185-RenamedCore'.
	className := 'Issue185Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].


	projectDefinition1 := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.

	projectSetDefinition := RwProjectSetDefinition new.
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition
		removePackageNamed: packageName1;
		addPackageNamed: packageName2.
	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition addClassDefinition: classDefinition.
	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName2.

]

{ #category : 'tests-issue 185' }
RwRowanProjectIssuesTest >> testIssue185_rename_package_move_class_to_symbolDict_1 [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"initial load uses default symbol dictionary, second load uses custom symbol dictionary"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition packageDefinition className projectSetDefinition class
		ar oldClass |
	projectName := 'Issue185'.
	packageName1 := 'Issue185-Core'.
	packageName2 := 'Issue185-RenamedCore'.
	className := 'Issue185Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	projectDefinition1 := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName1 asSymbol.


	projectSetDefinition := RwProjectSetDefinition new.

	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition
		removePackageNamed: packageName1;
		addPackageNamed: packageName2;
		setSymbolDictName: self _symbolDictionaryName2 forPackageNamed: packageName2.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	oldClass := class.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName2.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName2 asSymbol.

	self assert: class == oldClass.

]

{ #category : 'tests-issue 185' }
RwRowanProjectIssuesTest >> testIssue185_rename_package_move_class_to_symbolDict_2 [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"initial load uses custom symbol dictionary, second load uses default symbol dictionary"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition packageDefinition className projectSetDefinition class
		ar oldClass x y |
	projectName := 'Issue185'.
	packageName1 := 'Issue185-Core'.
	packageName2 := 'Issue185-RenamedCore'.
	className := 'Issue185Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	projectDefinition1 := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		defaultSymbolDictName: self _symbolDictionaryName1;
		setSymbolDictName: self _symbolDictionaryName2 forPackageNamed: packageName1;
		yourself.

	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName2 asSymbol.

	projectSetDefinition := RwProjectSetDefinition new.

	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition
		removePackageNamed: packageName1;
		addPackageNamed: packageName2;
		setSymbolDictName: nil forPackageNamed: packageName2.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	oldClass := class.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName2.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (x := (ar first at: 1) name) = (y := self _symbolDictionaryName1 asSymbol).

	self assert: class == oldClass.

]

{ #category : 'tests-issue 185' }
RwRowanProjectIssuesTest >> testIssue185_rename_package_move_class_to_symbolDict_3 [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"initial load uses custom symbol dictionary, second load uses default symbol dictionary -- change category of class"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition packageDefinition className projectSetDefinition class
		ar oldClass x y |
	projectName := 'Issue185'.
	packageName1 := 'Issue185-Core'.
	packageName2 := 'Issue185-RenamedCore'.
	className := 'Issue185Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	projectDefinition1 := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		defaultSymbolDictName: self _symbolDictionaryName1;
		setSymbolDictName: self _symbolDictionaryName2 forPackageNamed: packageName1;
		yourself.

	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: ''
		pools: #()
		type: 'normal'.

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName2 asSymbol.

	projectSetDefinition := RwProjectSetDefinition new.

	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition
		removePackageNamed: packageName1;
		addPackageNamed: packageName2;
		setSymbolDictName: nil forPackageNamed: packageName2.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	classDefinition category: packageName2.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	oldClass := class.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName2.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (x := (ar first at: 1) name) = (y := self _symbolDictionaryName1 asSymbol).

	self assert: class == oldClass.

]

{ #category : 'tests-issue 185' }
RwRowanProjectIssuesTest >> testIssue185_rename_package_move_class_to_symbolDict_add_remove_and_remove_methods_1 [

	"https://github.com/dalehenrich/Rowan/issues/185"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition packageDefinition className projectSetDefinition class
		ar oldClass x y |
	projectName := 'Issue185'.
	packageName1 := 'Issue185-Core'.
	packageName2 := 'Issue185-RenamedCore'.
	className := 'Issue185Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	projectDefinition1 := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		defaultSymbolDictName: self _symbolDictionaryName1;
		setSymbolDictName: self _symbolDictionaryName2 forPackageNamed: packageName1;
		yourself.

	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1');
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method2'
					protocol: 'accessing'
					source: 'method2 ^2');
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method3'
					protocol: 'accessing'
					source: 'method3 ^3').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName2 asSymbol.
	self assert: class new method1 = 1.
	self assert: class new method2 = 2.

	projectSetDefinition := RwProjectSetDefinition new.

	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition
		removePackageNamed: packageName1;
		addPackageNamed: packageName2;
		setSymbolDictName: nil forPackageNamed: packageName2.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	classDefinition category: packageName2.
	packageDefinition addClassDefinition: classDefinition.

	classDefinition
		removeInstanceMethod: #method2;
		removeInstanceMethod: #method3;
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method3'
					protocol: 'accessing'
					source: 'method3 ^5');
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method4'
					protocol: 'accessing'
					source: 'method4 ^4').

	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	oldClass := class.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName2.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (x := (ar first at: 1) name) = (y := self _symbolDictionaryName1 asSymbol).
	self assert: class new method1 = 1.
	self should: [ class new method2 = 2 ] raise: MessageNotUnderstood.
	self assert: (x := class new method3) = 5.
	self assert: class new method4 = 4.

	self assert: class == oldClass.

]

{ #category : 'tests-issue 185' }
RwRowanProjectIssuesTest >> testIssue185_rename_package_move_class_to_symbolDict_add_remove_and_remove_methods_2 [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"minimal version of #testIssue185_rename_package_move_class_to_symbolDict_add_remove_and_remove_methods_1 isolating a particular failure
		that I'm trying to resolve"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition packageDefinition 
		className projectSetDefinition class x |
	projectName := 'Issue185'.
	packageName1 := 'Issue185-Core'.
	packageName2 := 'Issue185-RenamedCore'.
	className := 'Issue185Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	projectDefinition1 := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		defaultSymbolDictName: self _symbolDictionaryName1;
		setSymbolDictName: self _symbolDictionaryName2 forPackageNamed: packageName1;
		yourself.

	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method3'
					protocol: 'accessing'
					source: 'method3 ^3').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	projectSetDefinition := RwProjectSetDefinition new.
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition
		removePackageNamed: packageName1;
		addPackageNamed: packageName2;
		setSymbolDictName: nil forPackageNamed: packageName2.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	classDefinition category: packageName2.
	packageDefinition addClassDefinition: classDefinition.

	classDefinition
		removeInstanceMethod: #method3;
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method3'
					protocol: 'accessing'
					source: 'method3 ^5').

	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	class := Rowan globalNamed: className.

	self assert: (x := class new method3) = 5.

]

{ #category : 'tests-issue 198' }
RwRowanProjectIssuesTest >> testIssue198A [

	"https://github.com/dalehenrich/Rowan/issues/198"

	"in attempting to reproduce issue #198, hit this issue. See https://github.com/dalehenrich/Rowan/issues/198#issuecomment-391831224
		for characterization."

	"subclass of class with new version, which is referenced by a method in a third class, is moved to a different package ... 
		failure occurs when the new superclass version and move to different package occurs in one atomic load. "

	| projectName projectNames packageName1 packageName2 className1 className2 theClass 
		theSubclass projectDefinition theClassDefinition theSubclassDefinition 
		packageDefinition1 packageDefinition2 |

	projectName := 'Issue198Project'.
	projectNames := {projectName}.
	packageName1 := 'Issue198-Core'.
	packageName2 := 'Issue198-Deprecated'.
	className1 := 'Issue198Class'.
	className2 := 'Issue198Subclass'.

	self
		_loadProjectDefinition: projectName
		packageNames: {packageName1. packageName2}
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: ''.

	theClass := Object
		rwSubclass: className1
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	theSubclass := theClass
		rwSubclass: className2
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	packageDefinition1 := projectDefinition packageNamed: packageName1.
	packageDefinition2 := projectDefinition packageNamed: packageName2.

	theClassDefinition := packageDefinition1 classDefinitions at: className1.
	theClassDefinition instVarNames: #( 'ivar1').																	"force new version of className1 and className2"

	theSubclassDefinition := packageDefinition1 classDefinitions at: className2.
	packageDefinition1 classDefinitions removeKey: theSubclassDefinition key.
	packageDefinition2 classDefinitions at: theSubclassDefinition key put: theSubclassDefinition.	"move subclass to different package"

	Rowan projectTools load loadProjectDefinition: projectDefinition.

	self assert: theSubclass rowanPackageName = packageName2.

]

{ #category : 'tests-issue 198' }
RwRowanProjectIssuesTest >> testIssue198B [

	"https://github.com/dalehenrich/Rowan/issues/198"

	"subclass of class with new version, which is referenced by a method in a third class, is moved to a different package ... 
		failure occurs when the new superclass version and move to different package occurs in one atomic load. "

	| projectName projectNames packageName1 packageName2 className1 className2 className3 theClass 
		theSubclass theReferenceClass projectDefinition theClassDefinition theSubclassDefinition 
		packageDefinition1 packageDefinition2 theReferenceClassDefinition |

	projectName := 'Issue198Project'.
	projectNames := {projectName}.
	packageName1 := 'Issue198-Core'.
	packageName2 := 'Issue198-Deprecated'.
	className1 := 'Issue198Class'.
	className2 := 'Issue198Subclass'.
	className3 := 'Issue198ReferenceClass'.

	self
		_loadProjectDefinition: projectName
		packageNames: {packageName1. packageName2}
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: ''.

	theClass := Object
		rwSubclass: className1
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	theSubclass := theClass
		rwSubclass: className2
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	theReferenceClass := Object
		rwSubclass: className3
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	packageDefinition1 := projectDefinition packageNamed: packageName1.
	packageDefinition2 := projectDefinition packageNamed: packageName2.

	theClassDefinition := packageDefinition1 classDefinitions at: className1.
	theClassDefinition instVarNames: #( 'ivar1').																	"force new version of className1 and className2"

	theSubclassDefinition := packageDefinition1 classDefinitions at: className2.
	packageDefinition1 classDefinitions removeKey: theSubclassDefinition key.
	packageDefinition2 classDefinitions at: theSubclassDefinition key put: theSubclassDefinition.	"move subclass to different package"

	theReferenceClassDefinition := packageDefinition1 classDefinitions at: className3.
	theReferenceClassDefinition addInstanceMethodDefinition: 
		(RwMethodDefinition
						newForSelector: #'boom'
						protocol: 'accessing'
						source: 'boom ^ ', className2).																"add method referencing moved class"

	Rowan projectTools load loadProjectDefinition: projectDefinition.

	self assert: (Rowan globalNamed: className3) new boom == (Rowan globalNamed: className2).
	self assert: theSubclass rowanPackageName = packageName2.

]

{ #category : 'tests-issue 198' }
RwRowanProjectIssuesTest >> testIssue198C [

	"https://github.com/dalehenrich/Rowan/issues/198"

	"Need to make sure that classes moved between packages (unrelated to new class versions) are processed correctly"

	| projectName projectNames packageName1 packageName2 className1 className2 className3 theClass 
		theSubclass theUnrelatedMovedClass projectDefinition theClassDefinition 
		packageDefinition1 packageDefinition2 theUnrelatedMovedClassDefinition |

	projectName := 'Issue198Project'.
	projectNames := {projectName}.
	packageName1 := 'Issue198-Core'.
	packageName2 := 'Issue198-Deprecated'.
	className1 := 'Issue198Class'.
	className2 := 'Issue198Subclass'.
	className3 := 'Issue198UnrelatedMovedClass'.

	self
		_loadProjectDefinition: projectName
		packageNames: {packageName1. packageName2}
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: ''.

	theClass := Object
		rwSubclass: className1
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	theSubclass := theClass
		rwSubclass: className2
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	theUnrelatedMovedClass := Object
		rwSubclass: className3
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	packageDefinition1 := projectDefinition packageNamed: packageName1.
	packageDefinition2 := projectDefinition packageNamed: packageName2.

	theClassDefinition := packageDefinition1 classDefinitions at: className1.
	theClassDefinition instVarNames: #( 'ivar1').																	"force new version of className1 and className2"

	theUnrelatedMovedClassDefinition := packageDefinition1 classDefinitions at: className3.
	packageDefinition1 classDefinitions removeKey: theUnrelatedMovedClassDefinition key.
	packageDefinition2 classDefinitions at: theUnrelatedMovedClassDefinition key put: theUnrelatedMovedClassDefinition.	"move unrelated moved class to different package"

	Rowan projectTools load loadProjectDefinition: projectDefinition.

]

{ #category : 'tests-issue 199' }
RwRowanProjectIssuesTest >> testIssue199 [

	"https://github.com/dalehenrich/Rowan/issues/199"

	"rename superclass of a class"

	| projectName projectNames packageName1  className1 className2 className3 theClass theSubclass projectDefinition theClassDefinition 
		theSubclassDefinition packageDefinition1 |

	projectName := 'Issue199Project'.
	projectNames := {projectName}.
	packageName1 := 'Issue199-Core'.
	className1 := 'Issue199Class'.
	className2 := 'Issue199Subclass'.
	className3 := 'Issue199ClassNew'.

	self
		_loadProjectDefinition: projectName
		packageNames: {packageName1}
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: ''.

	theClass := Object
		rwSubclass: className1
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	theSubclass := theClass
		rwSubclass: className2
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	packageDefinition1 := projectDefinition packageNamed: packageName1.

	theClassDefinition := packageDefinition1 classDefinitions at: className1.
	packageDefinition1 classDefinitions removeKey: theClassDefinition key.
	theClassDefinition name: className3.
	packageDefinition1 addClassDefinition: theClassDefinition.										"rename className1 to className3"

	theSubclassDefinition := packageDefinition1 classDefinitions at: className2.
	theSubclassDefinition superclassName: className3.												"change className2 superclass to className3"

	Rowan projectTools load loadProjectDefinition: projectDefinition.

]

{ #category : 'tests-issue 206' }
RwRowanProjectIssuesTest >> testIssue206_move_extension_method_to_new_package [

	"https://github.com/dalehenrich/Rowan/issues/206"

	"based on testIssue185_move_extension_method_to_new_package_1"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition packageDefinition className projectSetDefinition class
		classExtensionDefinition packageName3 |
	projectName := 'Issue185'.
	packageName1 := 'Issue185-Core1'.
	packageName2 := 'Issue185-Core2'.
	packageName3 := 'Issue185-Core3'.
	className := 'Issue185Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	projectDefinition1 := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		addPackageNamed: packageName3;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: ''
		pools: #()
		type: 'normal'.

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	"create extension method in different package"
	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'mover'
					protocol: '*', packageName2 asLowercase
					source: 'mover ^2').

	packageDefinition := projectDefinition1 packageNamed: packageName2.
	packageDefinition addClassExtension: classExtensionDefinition.

	"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	"validate"
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: class new mover = 2.

	"move the method to new package"
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition classExtensions removeKey: className.										"this is the source of the bug ... as a new class extensions without the proper class name is created
																															during the comparison"

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'mover'
					protocol: '*', packageName3 asLowercase
					source: 'mover ^2').

	packageDefinition := projectDefinition2 packageNamed: packageName3.
	packageDefinition addClassExtension: classExtensionDefinition.

	"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	"validate"
	self assert: class new mover = 2. "method not installed when class extension does not have a proper class name"

]

{ #category : 'tests-issue 208' }
RwRowanProjectIssuesTest >> testIssue208_move_method_from_project_to_project [

	"create class and method in same project/package"

	"move class to one package in new project and method to another package in new project"

	"simulating extension method moves during an adopt"

	"simplification of RwRowanSample4Test>>testIssue208_adopt_load"

	| projectName1 projectName2 packageName1 packageName2 packageName3 className projectDefinition1 classDefinition
		packageDefinition projectSetDefinition projectDefinition2 classExtensionDefinition class ar symDict registry oldClass |
	projectName1 := 'Issue208_moveMethodBetweenProjects1'.
	projectName2 := 'Issue208_moveMethodBetweenProjects2'.
	packageName1 := 'Issue208-Core1'.
	packageName2 := 'Issue208-Core2'.
	packageName3 := 'Issue208-Extension2'.
	className := 'Issue208Class'.

	{projectName1. projectName2}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project 1"
	projectDefinition1 := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName1)
		addPackageNamed: packageName1;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

"load project1"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validation"
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName1.
	self assert: class new method1 = 1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName1 asSymbol.

"create project2"
	projectDefinition2 := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName2)
		addPackageNamed: packageName2;
		addPackageNamed: packageName3;
		defaultSymbolDictName: self _symbolDictionaryName2;
		yourself.

	packageDefinition := projectDefinition2 packageNamed: packageName2.

	classDefinition 
		category: packageName2;
		removeInstanceMethod: #method1.
	packageDefinition addClassDefinition: classDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName3.
	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: '*', packageName3 asLowercase
					source: 'method1 ^1').
	packageDefinition addClassExtension: classExtensionDefinition.

"load project2"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validation"
	oldClass := class.
	class := Rowan globalNamed: className.
	self assert: oldClass == class.
	self assert: class rowanPackageName = packageName2.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName3.
	self assert: class new method1 = 1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName2 asSymbol.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 1.
	self assert: registry methodRegistry size = 1.

]

{ #category : 'tests-issue 215' }
RwRowanProjectIssuesTest >> testIssue215_move_class_and_extension_method_to_new_symbol_dict [

	"https://github.com/dalehenrich/Rowan/issues/215"

	| projectName  packageName1 packageName2 packageName3 projectDefinition1 projectDefinition2 classDefinition packageDefinition className1 className2 projectSetDefinition class
		classExtensionDefinition oldClass |
	projectName := 'Issue215'.
	packageName1 := 'Issue215-Core1'.
	packageName2 := 'Issue215-Tools'.
	packageName3 := 'Issue215-Tools-Extensions'.
	className1 := 'Issue215Class1'.
	className2 := 'Issue215Class2'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	projectDefinition1 := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		setSymbolDictName: self _symbolDictionaryName1
			forPackageNamed: packageName1;
		setSymbolDictName: self _symbolDictionaryName1
			forPackageNamed: packageName2;
		yourself.

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	classDefinition := RwClassDefinition
		newForClassNamed: className1
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: ''
		pools: #()
		type: 'normal'.
	packageDefinition addClassDefinition: classDefinition.

	packageDefinition := projectDefinition1 packageNamed: packageName2.
	classDefinition := RwClassDefinition
		newForClassNamed: className2
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName2
		comment: ''
		pools: #()
		type: 'normal'.
	packageDefinition addClassDefinition: classDefinition.

	"create extension method in different package"
	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className1.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'mover'
				protocol: 'accessing'
			source: 'mover ^2').
	packageDefinition addClassExtension: classExtensionDefinition.

	"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	"validate"
	class := Rowan globalNamed: className1.
	self assert: class rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName2.
	self assert: class new mover = 2.

	"move the class to different symbol dictionary and move extension methods to new package"
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.
	projectDefinition2
		addPackageNamed: packageName3;
		setSymbolDictName: self _symbolDictionaryName2
			forPackageNamed: packageName2;
		setSymbolDictName: self _symbolDictionaryName1
			forPackageNamed: packageName3.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	(packageDefinition classExtensions at: className1) removeInstanceMethod: #mover.

	packageDefinition := projectDefinition2 packageNamed: packageName3.
	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className1.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'mover'
				protocol: 'accessing'
			source: 'mover ^2').
	packageDefinition addClassExtension: classExtensionDefinition.
	
	"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	"validate"
	oldClass := class.
	class := Rowan globalNamed: className1.
	self assert: class == oldClass.
	self assert: class rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName3.
	self assert: class new mover = 2.

]

{ #category : 'tests-issue 217' }
RwRowanProjectIssuesTest >> testIssue217 [

	"https://github.com/dalehenrich/Rowan/issues/217"

	| spec specExportString |
	spec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specExportString := STON toStringPretty: spec.
	self assert: (specExportString includes: Character lf).

]

{ #category : 'tests-issue 24' }
RwRowanProjectIssuesTest >> testIssue24_addProject [

	| projectName |
	projectName := 'Issue24_addProject'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject_32/'.

	self _writeProjectNamed: projectName
]

{ #category : 'tests-issue 24' }
RwRowanProjectIssuesTest >> testIssue24_addRemovePackage [

	| projectName packageName |
	projectName := 'Issue24_addRemovePackage'.
	packageName := 'Issue24-Core'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject_33/'.

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

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject_34/'.

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

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject_35/'.

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

	self _createLoadedProjectNamed: projectName packageNames: {packageName} root: '/tmp/rowanIssuesProject_36/'  validate: false.
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

{ #category : 'tests-issue 24' }
RwRowanProjectIssuesTest >> testIssue24_moveClassBetweenPackages [

	"move a class from one package to another"

	| projectName packageName1 packageName2 className testClass |
	projectName := 'Issue24_moveClassBetweenPackages'.
	packageName1 := 'Issue24-Core1'.
	packageName2 := 'Issue24-Core2'.
	className := 'Issue24Class'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject_37/'.

	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.

	self _writeProjectNamed: projectName.

	self _addClassNamed: className toPackageNamed: packageName1 inProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	testClass := Rowan globalNamed: className.
	self assert: testClass rowanPackageName = packageName1.

	self _updateClassNamed: className toPackageNamed: packageName2 inProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	testClass := Rowan globalNamed: className.
	self assert: testClass rowanPackageName = packageName2
]

{ #category : 'tests-issue 24' }
RwRowanProjectIssuesTest >> testIssue24_moveClassBetweenProjects [

	| projectName1 projectName2 packageName1 packageName2 className testClass |
	projectName1 := 'Issue24_moveClassBetweenProjects1'.
	projectName2 := 'Issue24_moveClassBetweenProjects2'.
	packageName1 := 'Issue24-Core1'.
	packageName2 := 'Issue24-Core2'.
	className := 'Issue24Class'.

	self _createLoadedProjectNamed: projectName1 root: '/tmp/rowanIssuesProject1/'.
	self _createLoadedProjectNamed: projectName2 root: '/tmp/rowanIssuesProject2/'.

	self _addPackageNamed: packageName1 toProjectNamed: projectName1 validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName2 validate: false.

	self _writeProjectNamed: projectName1.
	self _writeProjectNamed: projectName2.

	self _addClassNamed: className toPackageNamed: packageName1 inProjectNamed: projectName1.

	self _writeProjectNamed: projectName1.

	testClass := Rowan globalNamed: className.
	self assert: testClass rowanPackageName = packageName1.

	self _updateClassNamed: className toPackageNamed: packageName2 inProjectNamed: projectName2.

	self _writeProjectNamed: projectName1.
	self _writeProjectNamed: projectName2.

	testClass := Rowan globalNamed: className.
	self assert: testClass rowanPackageName = packageName2

]

{ #category : 'tests-issue 24' }
RwRowanProjectIssuesTest >> testIssue24_moveMethodBetweenPackages [

	"move a method from one package to another"

	| projectName packageName1 packageName2 className theClass |

	projectName := 'Issue24_moveMethodBetweenPackages'.
	packageName1 := 'Issue24-Core1'.
	packageName2 := 'Issue24-Core2'.
	className := 'Issue24Class'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject_38/'.

	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.

	self _writeProjectNamed: projectName.

	theClass := self _addClassNamed: className toPackageNamed: packageName1 inProjectNamed: projectName.

	self _addOrUpdateMethod:  'foo ^''foo''' forBehavior: theClass inPackage: packageName1 inProjectNamed: projectName validate: false.

	self _writeProjectNamed: projectName.

	self _addOrUpdateMethod:  'foo ^''foo''' forBehavior: theClass inPackage: packageName2 inProjectNamed: projectName.

	self _writeProjectNamed: projectName.
]

{ #category : 'tests-issue 24' }
RwRowanProjectIssuesTest >> testIssue24_moveMethodBetweenProjects [

	"move a method from one package to another"

	| projectName1 projectName2 packageName1 packageName2 className theClass |
	projectName1 := 'Issue24_moveMethodBetweenProjects1'.
	projectName2 := 'Issue24_moveMethodBetweenProjects2'.
	packageName1 := 'Issue24-Core1'.
	packageName2 := 'Issue24-Core2'.
	className := 'Issue24Class'.

	self _createLoadedProjectNamed: projectName1 root: '/tmp/rowanIssuesProject1_33/'.
	self _createLoadedProjectNamed: projectName2 root: '/tmp/rowanIssuesProject2_33/'.

	self _addPackageNamed: packageName1 toProjectNamed: projectName1 validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName2 validate: false.

	self _writeProjectNamed: projectName1.
	self _writeProjectNamed: projectName2.

	theClass := self _addClassNamed: className toPackageNamed: packageName1 inProjectNamed: projectName1.

	self _addOrUpdateMethod:  'foo ^''foo''' forBehavior: theClass inPackage: packageName1 inProjectNamed: projectName1 validate: false.

	self _writeProjectNamed: projectName1.

	self _addOrUpdateMethod:  'foo ^''foo''' forBehavior: theClass inPackage: packageName2 inProjectNamed: projectName2.

	self _writeProjectNamed: projectName1.
	self _writeProjectNamed: projectName2.
]

{ #category : 'tests-issue 255' }
RwRowanProjectIssuesTest >> testIssue255_class_modification_constraints [

	"test modifying constraints"
	"https://github.com/dalehenrich/Rowan/issues/255"

	| projectName  packageName projectDefinition classDefinition1 packageDefinition className1 
		projectSetDefinition class1 oldClass x |

	projectName := 'Issue255'.
	packageName := 'Issue255-Core'.
	className1 := 'Issue255Class1'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	classDefinition1 := RwClassDefinition
		newForClassNamed: className1
		super: 'Object'
		instvars: #(ivar1)
		classinstvars: #()
		classvars: #()
		category: 'category'
		comment: 'comment'
		pools: #()
		type: 'normal'.

	packageDefinition := projectDefinition packageNamed: packageName.
	packageDefinition 
		addClassDefinition: classDefinition1.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class1 := Rowan globalNamed: className1.
	self assert: class1 instVarNames = #(ivar1).
	self assert: (x := class1 classVarNames asSortedCollection) = #() asSortedCollection.
	self assert: class1 category = 'category'.
	self assert: class1 comment = 'comment'.

"modify class -- no new version"
	classDefinition1
		classVarNames: #(Cvar1 Cvar2 Cvar3);
		category: 'newer category';
		comment: 'newer comment';
		gs_constraints: { { 'ivar1' . 'Association' } }.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	oldClass := class1.
	class1 := Rowan globalNamed: className1.
	self assert: oldClass == class1.
	self assert: class1 instVarNames = #(ivar1).
	self assert: (x := class1 classVarNames asSortedCollection) = #(Cvar1 Cvar2 Cvar3) asSortedCollection.
	self assert: class1 category = 'newer category'.
	self assert: class1 comment = 'newer comment'.
	self assert: (class1 _constraintsEqual: { { #'ivar1' . Association } }).

"modify class -- new version"
	classDefinition1
		instVarNames: #(ivar1 ivar2);
		classVarNames: #(Cvar1 Cvar2 Cvar3);
		category: 'new category';
		comment: 'new comment';
		gs_constraints: { { 'ivar1' . 'Association' } . { 'ivar2' . 'Number' } }.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	oldClass := class1.
	class1 := Rowan globalNamed: className1.
	self assert: oldClass ~~ class1.
	self assert: class1 instVarNames = #(ivar1 ivar2).
	self assert: (x := class1 classVarNames asSortedCollection) = #(Cvar1 Cvar2 Cvar3) asSortedCollection.
	self assert: class1 category = 'new category'.
	self assert: class1 comment = 'new comment'.
	self assert: (class1 _constraintsEqual: { { #'ivar1' . Association } . { #'ivar2' . Number } }).

]

{ #category : 'tests-issue 255' }
RwRowanProjectIssuesTest >> testIssue255_class_modification_no_constraints [

	"test modifying constraints"
	"https://github.com/dalehenrich/Rowan/issues/255"

	| projectName  packageName projectDefinition classDefinition1 packageDefinition className1 
		projectSetDefinition class1 oldClass x |

	projectName := 'Issue255'.
	packageName := 'Issue255-Core'.
	className1 := 'Issue255Class1'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	classDefinition1 := RwClassDefinition
		newForClassNamed: className1
		super: 'Object'
		instvars: #(ivar1)
		classinstvars: #()
		classvars: #()
		category: 'category'
		comment: 'comment'
		pools: #()
		type: 'normal'.

	packageDefinition := projectDefinition packageNamed: packageName.
	packageDefinition 
		addClassDefinition: classDefinition1.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class1 := Rowan globalNamed: className1.
	self assert: class1 instVarNames = #(ivar1).
	self assert: (x := class1 classVarNames asSortedCollection) = #() asSortedCollection.
	self assert: class1 category = 'category'.
	self assert: class1 comment = 'comment'.

"modify class -- no new version"
	classDefinition1
		classVarNames: #(Cvar1 Cvar2 Cvar3);
		category: 'newer category';
		comment: 'newer comment'.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	oldClass := class1.
	class1 := Rowan globalNamed: className1.
	self assert: oldClass == class1.
	self assert: class1 instVarNames = #(ivar1).
	self assert: (x := class1 classVarNames asSortedCollection) = #(Cvar1 Cvar2 Cvar3) asSortedCollection.
	self assert: class1 category = 'newer category'.
	self assert: class1 comment = 'newer comment'.

"modify class -- new version"
	classDefinition1
		instVarNames: #(ivar1 ivar2);
		classVarNames: #(Cvar1 Cvar2 Cvar3);
		category: 'new category';
		comment: 'new comment'.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	oldClass := class1.
	class1 := Rowan globalNamed: className1.
	self assert: oldClass ~~ class1.
	self assert: class1 instVarNames = #(ivar1 ivar2).
	self assert: (x := class1 classVarNames asSortedCollection) = #(Cvar1 Cvar2 Cvar3) asSortedCollection.
	self assert: class1 category = 'new category'.
	self assert: class1 comment = 'new comment'.

]

{ #category : 'tests-issue 255' }
RwRowanProjectIssuesTest >> testIssue255_constraints [

	"test modifying constraints"
	"https://github.com/dalehenrich/Rowan/issues/255"

	| projectName  packageName projectDefinition classDefinition1 packageDefinition className1 projectSetDefinition class1 |
	projectName := 'Issue255'.
	packageName := 'Issue255-Core'.
	className1 := 'Issue255Class1'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	classDefinition1 := RwClassDefinition
		newForClassNamed: className1
		super: 'Object'
		instvars: #(ivar1)
		classinstvars: #()
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition1 gs_constraints: { { 'ivar1' . 'Association' } }.


	packageDefinition := projectDefinition packageNamed: packageName.
	packageDefinition 
		addClassDefinition: classDefinition1.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class1 := Rowan globalNamed: className1.
	self assert: (class1 _constraintsEqual: { { #'ivar1' . Association } })

]

{ #category : 'tests-issue 255' }
RwRowanProjectIssuesTest >> testIssue255_external_constraints [

	"basic test case"
	"https://github.com/dalehenrich/Rowan/issues/255"

	| projectName  packageName projectDefinition classDefinition1 classDefinition2 classDefinition3 packageDefinition className1 className2 projectSetDefinition 
		class1 class2 className3 class3 |
	projectName := 'Issue255'.
	packageName := 'Issue255-Core'.
	className1 := 'Issue255Class1'.
	className2 := 'Issue255Class2'.
	className3 := 'Issue255Array3'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	classDefinition1 := RwClassDefinition
		newForClassNamed: className1
		super: 'Object'
		instvars: #(ivar1)
		classinstvars: #()
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition1 gs_constraints: { { 'ivar1' . 'Association' } }.

	classDefinition2 := RwClassDefinition
		newForClassNamed: className2
		super: 'Object'
		instvars: #(ivar2)
		classinstvars: #()
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition2 gs_constraints: { { 'ivar2' . 'Array' } }.

	classDefinition3 := RwClassDefinition
		newForClassNamed: className3
		super: 'Array'
		instvars: #()
		classinstvars: #( )
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition3 gs_constraints: { 'SmallInteger' }.

	packageDefinition := projectDefinition packageNamed: packageName.
	packageDefinition 
		addClassDefinition: classDefinition1;
		addClassDefinition: classDefinition2;
		addClassDefinition: classDefinition3.

"load"	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class1 := Rowan globalNamed: className1.
	class2 := Rowan globalNamed: className2.
	class3 := Rowan globalNamed: className3.
	self assert: class1 rowanPackageName = packageName.
	self assert: class2 rowanPackageName = packageName.
	self assert: (class1 _constraintOn: #ivar1) == Association.
	self assert: (class2 _constraintOn: #ivar2) == Array.
	self assert: class3 _varyingConstraint == SmallInteger

]

{ #category : 'tests-issue 255' }
RwRowanProjectIssuesTest >> testIssue255_internal_constraints_1 [

	"reproduce bug"
	"https://github.com/dalehenrich/Rowan/issues/255"

	| projectName  packageName projectDefinition classDefinition1 classDefinition2 packageDefinition className1 className2 projectSetDefinition 
		class1 class2 |
	projectName := 'Issue255'.
	packageName := 'Issue255-Core'.
	className1 := 'Issue255Class1'.
	className2 := 'Issue255Class2'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	classDefinition1 := RwClassDefinition
		newForClassNamed: className1
		super: 'Object'
		instvars: #(ivar1)
		classinstvars: #()
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition1 gs_constraints: { { 'ivar1' . className2 } }.

	classDefinition2 := RwClassDefinition
		newForClassNamed: className2
		super: 'Object'
		instvars: #(ivar2)
		classinstvars: #()
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition2 gs_constraints: { { 'ivar2' . className1 } }.

	packageDefinition := projectDefinition packageNamed: packageName.
	packageDefinition 
		addClassDefinition: classDefinition1;
		addClassDefinition: classDefinition2.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class1 := Rowan globalNamed: className1.
	class2 := Rowan globalNamed: className2.
	self assert: class1 rowanPackageName = packageName.
	self assert: class2 rowanPackageName = packageName.

]

{ #category : 'tests-issue 255' }
RwRowanProjectIssuesTest >> testIssue255_internal_constraints_2 [

	"varian of bug"
	"https://github.com/dalehenrich/Rowan/issues/255"

	| projectName  packageName projectDefinition classDefinition1 classDefinition2 classDefinition3 packageDefinition className1 className2 projectSetDefinition 
		class1 class2 className3 |
	projectName := 'Issue255'.
	packageName := 'Issue255-Core'.
	className1 := 'Issue255Class1'.
	className2 := 'Issue255Class2'.
	className3 := 'Issue255Array3'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	classDefinition1 := RwClassDefinition
		newForClassNamed: className1
		super: 'Object'
		instvars: #(ivar1)
		classinstvars: #()
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition1 gs_constraints: { { 'ivar1' . className2 } }.

	classDefinition2 := RwClassDefinition
		newForClassNamed: className2
		super: 'Object'
		instvars: #(ivar2)
		classinstvars: #()
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition2 gs_constraints: { { 'ivar2' . className3 } }.

	classDefinition3 := RwClassDefinition
		newForClassNamed: className3
		super: 'Array'
		instvars: #()
		classinstvars: #( )
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition3 gs_constraints: { className2 }.

	packageDefinition := projectDefinition packageNamed: packageName.
	packageDefinition 
		addClassDefinition: classDefinition1;
		addClassDefinition: classDefinition2;
		addClassDefinition: classDefinition3.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class1 := Rowan globalNamed: className1.
	class2 := Rowan globalNamed: className2.
	self assert: class1 rowanPackageName = packageName.
	self assert: class2 rowanPackageName = packageName.

]

{ #category : 'tests-issue 255' }
RwRowanProjectIssuesTest >> testIssue255_internal_constraints_3 [

	"reproduce bug encountered at client site - constraints on subclasses"
	"https://github.com/dalehenrich/Rowan/issues/255"

	| projectName  packageName projectDefinition classDefinition1 classDefinition2 classDefinition3 packageDefinition className1 className2 projectSetDefinition 
		class1 class2 className3 |
	projectName := 'Issue255'.
	packageName := 'Issue255-Core'.
	className1 := 'Issue255Class1'.
	className2 := 'Issue255Class2'.
	className3 := 'Issue255Array3'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	classDefinition1 := RwClassDefinition
		newForClassNamed: className1
		super: 'Object'
		instvars: #(ivar1)
		classinstvars: #()
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition1 gs_constraints: { { 'ivar1' . className2 } }.

	classDefinition2 := RwClassDefinition
		newForClassNamed: className2
		super: className1
		instvars: #(ivar2)
		classinstvars: #()
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition2 gs_constraints: { { 'ivar2' . className3 } }.

	classDefinition3 := RwClassDefinition
		newForClassNamed: className3
		super: 'Array'
		instvars: #()
		classinstvars: #( )
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition3 gs_constraints: { className2 }.

	packageDefinition := projectDefinition packageNamed: packageName.
	packageDefinition 
		addClassDefinition: classDefinition1;
		addClassDefinition: classDefinition2;
		addClassDefinition: classDefinition3.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class1 := Rowan globalNamed: className1.
	class2 := Rowan globalNamed: className2.
	self assert: class1 rowanPackageName = packageName.
	self assert: class2 rowanPackageName = packageName.

]

{ #category : 'tests-issue 255' }
RwRowanProjectIssuesTest >> testIssue255_modification [

	"test modifying constraints"
	"https://github.com/dalehenrich/Rowan/issues/255"

	| projectName  packageName projectDefinition classDefinition1 packageDefinition className1 projectSetDefinition class1 |

	projectName := 'Issue255'.
	packageName := 'Issue255-Core'.
	className1 := 'Issue255Class1'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	classDefinition1 := RwClassDefinition
		newForClassNamed: className1
		super: 'Object'
		instvars: #(ivar1 ivar2)
		classinstvars: #()
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition1 gs_constraints: { { 'ivar1' . 'Association' } }.

	packageDefinition := projectDefinition packageNamed: packageName.
	packageDefinition 
		addClassDefinition: classDefinition1.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class1 := Rowan globalNamed: className1.
	self assert: class1 rowanPackageName = packageName.
	self assert: (class1 _constraintOn: #ivar1) = Association.

"modify constraints"
	classDefinition1 gs_constraints: { { 'ivar1' . 'Association' } . { 'ivar2' . 'SmallInteger' } }.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class1 := Rowan globalNamed: className1.
	self assert: class1 rowanPackageName = packageName.
	self assert: (class1 _constraintOn: #ivar1) = Association.
	self assert: (class1 _constraintOn: #ivar2) = SmallInteger.

]

{ #category : 'tests-issue 263' }
RwRowanProjectIssuesTest >> testIssue263 [
	"https://github.com/dalehenrich/Rowan/issues/263"

	| projectName packageName projectDefinition projectSetDefinition |
	projectName := 'Issue263_01'.
	packageName := 'Issue263-Kernel'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: 'Globals';
		yourself.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	[
"test"
		"add an extension method to a kernel class (unpackaged class)"
		Object
			rwCompileMethod: 'bar ^''bar'''
			category: '*', packageName asLowercase.

		Boolean
			rwCompileMethod: 'bar ^''bar'''
			category: '*', packageName asLowercase.

"validate"
		(Rowan image loadedClassExtensionsForClass: Object) 
			detect: [:ce |  ce loadedProject name = projectName ] ifNone: [ self assert: false description: 'no class extension found' ].

		(Rowan image loadedClassExtensionsForClass: Boolean) 
			detect: [:ce |  ce loadedProject name = projectName ] ifNone: [ self assert: false description: 'no class extension found' ] ]
				ensure: [ 
					Object rwRemoveSelector: #'bar'.
					Boolean rwRemoveSelector: #'bar']
]

{ #category : 'tests-issue 275' }
RwRowanProjectIssuesTest >> testIssue275_delete_class_handle_keep_in_system [

	"https://github.com/dalehenrich/Rowan/issues/275"

	| projectName  packageName projectDefinition classDefinition1 packageDefinition className1 
		projectSetDefinition class1 oldClass |

	projectName := 'Issue275'.
	packageName := 'Issue275-Core'.
	className1 := 'Issue275Class1'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	classDefinition1 := RwClassDefinition
		newForClassNamed: className1
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: 'category'
		comment: 'comment'
		pools: #()
		type: 'normal'.

	packageDefinition := projectDefinition packageNamed: packageName.
	packageDefinition 
		addClassDefinition: classDefinition1.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class1 := Rowan globalNamed: className1.
	self assert: class1 isBehavior.
	self assert: class1 name asString = className1.

"remove class from system"
	packageDefinition
		removeClassDefinition: classDefinition1.

"load -- remove from loaded things, but leave in system"
	[ Rowan projectTools load loadProjectSetDefinition: projectSetDefinition ]
		on: RwDeleteClassFromSystemNotification
		do: [:ex | 
			ex candidateClass == class1 
				ifTrue: [ ex resume: false ]
				ifFalse: [ ex pass ] ].

"validate"
	oldClass := class1.
	class1 := Rowan globalNamed: className1.
	self assert: class1 == oldClass.
	self assert: class1 rowanProjectName = Rowan unpackagedName.

]

{ #category : 'tests-issue 275' }
RwRowanProjectIssuesTest >> testIssue275_delete_class_handle_remove_from_system [

	"https://github.com/dalehenrich/Rowan/issues/275"

	| projectName  packageName projectDefinition classDefinition1 packageDefinition className1 
		projectSetDefinition class1 oldClass handled |

	projectName := 'Issue275'.
	packageName := 'Issue275-Core'.
	className1 := 'Issue275Class1'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	classDefinition1 := RwClassDefinition
		newForClassNamed: className1
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: 'category'
		comment: 'comment'
		pools: #()
		type: 'normal'.

	packageDefinition := projectDefinition packageNamed: packageName.
	packageDefinition 
		addClassDefinition: classDefinition1.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class1 := Rowan globalNamed: className1.
	self assert: class1 isBehavior.
	self assert: class1 name asString = className1.

"remove class from system"
	packageDefinition
		removeClassDefinition: classDefinition1.

"load"
	handled := false.
	[ Rowan projectTools load loadProjectSetDefinition: projectSetDefinition ]
		on: RwDeleteClassFromSystemNotification
		do: [:ex | 
			ex candidateClass == class1 
				ifTrue: [ 
					handled := true.
					ex resume: true ]
				ifFalse: [ ex pass ] ].
	self assert: handled.

"validate"
	oldClass := class1.
	class1 := Rowan globalNamed: className1.
	self assert: class1 isNil.

]

{ #category : 'tests-issue 275' }
RwRowanProjectIssuesTest >> testIssue275_delete_class_remove_from_system [

	"https://github.com/dalehenrich/Rowan/issues/275"

	| projectName  packageName projectDefinition classDefinition1 packageDefinition className1 
		projectSetDefinition class1 oldClass |

	projectName := 'Issue275'.
	packageName := 'Issue275-Core'.
	className1 := 'Issue275Class1'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	classDefinition1 := RwClassDefinition
		newForClassNamed: className1
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: 'category'
		comment: 'comment'
		pools: #()
		type: 'normal'.

	packageDefinition := projectDefinition packageNamed: packageName.
	packageDefinition 
		addClassDefinition: classDefinition1.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class1 := Rowan globalNamed: className1.
	self assert: class1 isBehavior.
	self assert: class1 name asString = className1.

"remove class from system"
	packageDefinition
		removeClassDefinition: classDefinition1.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	oldClass := class1.
	class1 := Rowan globalNamed: className1.
	self assert: class1 isNil.

]

{ #category : 'tests-issue 275' }
RwRowanProjectIssuesTest >> testIssue275_delete_subclass_handle_keep_in_system [

	"https://github.com/dalehenrich/Rowan/issues/275"

	| projectName  packageName projectDefinition classDefinition1 classDefinition2 packageDefinition className1 className2 
		projectSetDefinition class1 class2 oldClass1 oldClass2 handled |

	projectName := 'Issue275'.
	packageName := 'Issue275-Core'.
	className1 := 'Issue275Class1'.
	className2 := 'Issue275Subclass1'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	classDefinition1 := RwClassDefinition
		newForClassNamed: className1
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: 'category'
		comment: 'comment'
		pools: #()
		type: 'normal'.

	classDefinition2 := RwClassDefinition
		newForClassNamed: className2
		super: className1
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: 'category'
		comment: 'comment'
		pools: #()
		type: 'normal'.

	packageDefinition := projectDefinition packageNamed: packageName.
	packageDefinition 
		addClassDefinition: classDefinition1;
		addClassDefinition: classDefinition2.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class1 := Rowan globalNamed: className1.
	class2 := Rowan globalNamed: className2.
	self assert: class1 isBehavior.
	self assert: class1 name asString = className1.
	self assert: class2 isBehavior.
	self assert: class2 name asString = className2.
	self assert: class1 rowanPackageName = packageName.
	self assert: class2 rowanPackageName = packageName.

"remove class2 from system new version of class1"
	classDefinition1 instVarNames: #(ivar1).
	packageDefinition
		removeClassDefinition: classDefinition2.

"load -- remove from loaded things, but leave in system"
	handled := false.
	[ Rowan projectTools load loadProjectSetDefinition: projectSetDefinition ]
		on: RwDeleteClassFromSystemNotification
		do: [:ex | 
			ex candidateClass name asString = className2 
				ifTrue: [ 
					handled := true.
					ex resume: false ]
				ifFalse: [ ex pass ] ].
	self assert: handled.

"validate"
	oldClass1 := class1.
	oldClass2 := class2.
	class1 := Rowan globalNamed: className1.
	class2 := Rowan globalNamed: className2.
	self assert: class1 ~~ oldClass1.
	self assert: class1 rowanPackageName = packageName.
	self assert: class2 notNil.
	self assert: class2 superclass == oldClass1.
	self assert: class2 == oldClass2.
	self assert: class2 rowanPackageName = Rowan unpackagedName.

]

{ #category : 'tests-issue 291' }
RwRowanProjectIssuesTest >> testIssue291_new_version_class_with_subclass_constraints_1 [

	"https://github.com/dalehenrich/Rowan/issues/291"

	| projectName  packageName projectDefinition classDefinition1 classDefinition packageDefinition className1 className2 className3 
		projectSetDefinition class1 class2 class3 oldClass1 oldClass2 oldClass3 |

	projectName := 'Issue291'.
	packageName := 'Issue255-Core'.
	className1 := 'Issue255Class1'.
	className2 := 'Issue255Class2'.
	className3 := 'Issue255Class3'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	packageDefinition := projectDefinition packageNamed: packageName.

	classDefinition1 := (RwClassDefinition
		newForClassNamed: className1
			super: 'Object'
			instvars: #(ivar1)
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		gs_constraints: { { 'ivar1' . 'Association' } };
		yourself.
	packageDefinition 
		addClassDefinition: classDefinition1.

	classDefinition := (RwClassDefinition
		newForClassNamed: className2
			super: className1
			instvars: #(ivar2)
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		gs_constraints: { { 'ivar2' . 'Association' } };
		yourself.
	packageDefinition 
		addClassDefinition: classDefinition.

	classDefinition := (RwClassDefinition
		newForClassNamed: className3
			super: className2
			instvars: #(ivar4 ivar3)
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		gs_constraints: { { 'ivar3' . 'Association' }. { 'ivar4' . 'Association' }. };
		yourself.
	packageDefinition 
		addClassDefinition: classDefinition.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class1 := Rowan globalNamed: className1.
	self assert: class1 instVarNames = #(ivar1).
	self assert: class1 category = 'category'.
	self assert: class1 comment = 'comment'.
	self assert: (class1 _constraintsEqual: { { #'ivar1' . Association } }).
	class2 := Rowan globalNamed: className2.
	self assert: class2 instVarNames = #(ivar2).
	self assert: (class2 _constraintsEqual: { { #'ivar2' . Association } }).
	class3 := Rowan globalNamed: className3.
	self assert: class3 instVarNames = #(ivar4 ivar3).
	self assert: (class3 _constraintsEqual: { { #'ivar4' . Association }. { #'ivar3' . Association }. }).



"modify class -- new version"
	classDefinition1
		instVarNames: #(ivar5 ivar1);
		gs_constraints: { { 'ivar1' . 'Association' } . { 'ivar5' . 'Number' } }.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	oldClass1 := class1.
	class1 := Rowan globalNamed: className1.
	self assert: oldClass1 ~~ class1.
	self assert: class1 instVarNames = #(ivar5 ivar1).
	self assert: class1 category = 'category'.
	self assert: class1 comment = 'comment'.
	self assert: (class1 _constraintsEqual: { { #'ivar5' . Number }. { #'ivar1' . Association }. }).
	oldClass2 := class2.
	class2 := Rowan globalNamed: className2.
	self assert: oldClass2 ~~ class2.
	self assert: class2 instVarNames = #(ivar2).
	self assert: (class2 _constraintsEqual: { { #'ivar2' . Association } }).
	oldClass3 := class3.
	class3 := Rowan globalNamed: className3.
	self assert: oldClass3 ~~ class3.
	self assert: class3 instVarNames = #(ivar4 ivar3).
	self assert: (class3 _constraintsEqual: { { #'ivar4' . Association }. { #'ivar3' . Association }. }).

]

{ #category : 'tests-issue 291' }
RwRowanProjectIssuesTest >> testIssue291_new_version_class_with_subclass_constraints_2 [

	"https://github.com/dalehenrich/Rowan/issues/291"

	| projectName  packageName projectDefinition classDefinition1 classDefinition packageDefinition className1 className2 className3 
		projectSetDefinition class1 class2 class3 oldClass1 oldClass2 oldClass3 newClass1 |

	projectName := 'Issue291'.
	packageName := 'Issue255-Core'.
	className1 := 'Issue255Class1'.
	className2 := 'Issue255Class2'.
	className3 := 'Issue255Class3'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	packageDefinition := projectDefinition packageNamed: packageName.

	classDefinition1 := (RwClassDefinition
		newForClassNamed: className1
			super: 'Object'
			instvars: #(ivar1)
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		gs_constraints: { { 'ivar1' . 'Association' } };
		yourself.
	packageDefinition 
		addClassDefinition: classDefinition1.

	classDefinition := (RwClassDefinition
		newForClassNamed: className2
			super: className1
			instvars: #(ivar2)
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		gs_constraints: { { 'ivar2' . 'Association' } };
		yourself.
	packageDefinition 
		addClassDefinition: classDefinition.

	classDefinition := (RwClassDefinition
		newForClassNamed: className3
			super: className2
			instvars: #(ivar4 ivar3)
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		gs_constraints: { { 'ivar3' . 'Association' }. { 'ivar4' . 'Association' }. };
		yourself.
	packageDefinition 
		addClassDefinition: classDefinition.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class1 := Rowan globalNamed: className1.
	self assert: class1 instVarNames = #(ivar1).
	self assert: class1 category = 'category'.
	self assert: class1 comment = 'comment'.
	self assert: (class1 _constraintsEqual: { { #'ivar1' . Association } }).
	class2 := Rowan globalNamed: className2.
	self assert: class2 instVarNames = #(ivar2).
	self assert: (class2 _constraintsEqual: { { #'ivar2' . Association } }).
	class3 := Rowan globalNamed: className3.
	self assert: class3 instVarNames = #(ivar4 ivar3).
	self assert: (class3 _constraintsEqual: { { #'ivar4' . Association }. { #'ivar3' . Association }. }).



"create new version"
	newClass1 := Object 
		rwSubclass: className1 
		instVarNames: #('ivar5' 'ivar1') 
		classVars: #() 
		classInstVars: #() 
		poolDictionaries: #() 
		category: 'category' 
		packageName:  packageName
		constraints: { { #'ivar1' . Association } . { #'ivar5' . Number } } 
		options: #().

"validate"
	oldClass1 := class1.
	class1 := Rowan globalNamed: className1.
	self assert: class1 == newClass1.
	self assert: oldClass1 ~~ class1.
	self assert: class1 instVarNames = #(ivar5 ivar1).
	self assert: class1 category = 'category'.
	self assert: class1 comment = 'comment'.
	self assert: (class1 _constraintsEqual: { { #'ivar5' . Number }. { #'ivar1' . Association }. }).
	oldClass2 := class2.
	class2 := Rowan globalNamed: className2.
	self assert: oldClass2 ~~ class2.
	self assert: class2 instVarNames = #(ivar2).
	self assert: (class2 _constraintsEqual: { { #'ivar2' . Association } }).
	oldClass3 := class3.
	class3 := Rowan globalNamed: className3.
	self assert: oldClass3 ~~ class3.
	self assert: class3 instVarNames = #(ivar4 ivar3).
	self assert: (class3 _constraintsEqual: { { #'ivar4' . Association }. { #'ivar3' . Association }. }).

]

{ #category : 'tests-issue 291' }
RwRowanProjectIssuesTest >> testIssue291_new_version_class_with_subclass_constraints_3 [

	"https://github.com/dalehenrich/Rowan/issues/291"

	"super class has constraint on instance of subclass"

	| projectName  packageName projectDefinition classDefinition1 classDefinition packageDefinition className1 className2 className3 
		projectSetDefinition class1 class2 class3 oldClass1 oldClass2 oldClass3 newClass1 |

	projectName := 'Issue291'.
	packageName := 'Issue255-Core'.
	className1 := 'Issue255Class1'.
	className2 := 'Issue255Class2'.
	className3 := 'Issue255Class3'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	packageDefinition := projectDefinition packageNamed: packageName.

	classDefinition1 := (RwClassDefinition
		newForClassNamed: className1
			super: 'Object'
			instvars: #(ivar1)
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		gs_constraints: { { 'ivar1' . className2 } };
		yourself.
	packageDefinition 
		addClassDefinition: classDefinition1.

	classDefinition := (RwClassDefinition
		newForClassNamed: className2
			super: className1
			instvars: #(ivar2)
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		gs_constraints: { { 'ivar2' . 'Association' } };
		yourself.
	packageDefinition 
		addClassDefinition: classDefinition.

	classDefinition := (RwClassDefinition
		newForClassNamed: className3
			super: className2
			instvars: #(ivar4 ivar3)
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		gs_constraints: { { 'ivar3' . 'Association' }. { 'ivar4' . 'Association' }. };
		yourself.
	packageDefinition 
		addClassDefinition: classDefinition.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class1 := Rowan globalNamed: className1.
	class2 := Rowan globalNamed: className2.
	class3 := Rowan globalNamed: className3.
	self assert: class1 instVarNames = #(ivar1).
	self assert: class1 category = 'category'.
	self assert: class1 comment = 'comment'.
	self assert: (class1 _constraintsEqual: { { #'ivar1' . class2 } }).
	self assert: class2 instVarNames = #(ivar2).
	self assert: (class2 _constraintsEqual: { { #'ivar2' . Association } }).
	self assert: class3 instVarNames = #(ivar4 ivar3).
	self assert: (class3 _constraintsEqual: { { #'ivar4' . Association }. { #'ivar3' . Association }. }).



"create new version"
	newClass1 := Object 
		rwSubclass: className1 
		instVarNames: #('ivar5' 'ivar1') 
		classVars: #() 
		classInstVars: #() 
		poolDictionaries: #() 
		category: 'category' 
		packageName:  packageName
		constraints: { { #'ivar1' . class2 } . { #'ivar5' . Number } } 
		options: #().

"validate"
	oldClass1 := class1.
	class1 := Rowan globalNamed: className1.
	oldClass2 := class2.
	class2 := Rowan globalNamed: className2.
	oldClass3 := class3.
	class3 := Rowan globalNamed: className3.
	self assert: class1 == newClass1.
	self assert: oldClass1 ~~ class1.
	self assert: class1 instVarNames = #(ivar5 ivar1).
	self assert: class1 category = 'category'.
	self assert: class1 comment = 'comment'.
	self assert: (class1 _constraintsEqual: { { #'ivar5' . Number }. { #'ivar1' . oldClass2 }. }).
	self assert: oldClass2 ~~ class2.
	self assert: class2 instVarNames = #(ivar2).
	self assert: (class2 _constraintsEqual: { { #'ivar2' . Association } }).
	self assert: oldClass3 ~~ class3.
	self assert: class3 instVarNames = #(ivar4 ivar3).
	self assert: (class3 _constraintsEqual: { { #'ivar4' . Association }. { #'ivar3' . Association }. }).

]

{ #category : 'tests-issue 291' }
RwRowanProjectIssuesTest >> testIssue291_new_version_class_with_subclass_constraints_4 [

	"https://github.com/dalehenrich/Rowan/issues/291"

	"subclass overrides constraint of superclass (ivar1 has different constraint for each of classes)"

	| projectName  packageName projectDefinition classDefinition1 classDefinition packageDefinition className1 className2 className3 
		projectSetDefinition class1 class2 class3 oldClass1 oldClass2 oldClass3 newClass1 |

	projectName := 'Issue291'.
	packageName := 'Issue255-Core'.
	className1 := 'Issue255Class1'.
	className2 := 'Issue255Class2'.
	className3 := 'Issue255Class3'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	packageDefinition := projectDefinition packageNamed: packageName.

	classDefinition1 := (RwClassDefinition
		newForClassNamed: className1
			super: 'Object'
			instvars: #(ivar1)
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		gs_constraints: { { 'ivar1' . 'Number' } };
		yourself.
	packageDefinition 
		addClassDefinition: classDefinition1.

	classDefinition := (RwClassDefinition
		newForClassNamed: className2
			super: className1
			instvars: #(ivar2)
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		gs_constraints: { { 'ivar2' . 'Association' } . { 'ivar1' . 'Integer' } };
		yourself.
	packageDefinition 
		addClassDefinition: classDefinition.

	classDefinition := (RwClassDefinition
		newForClassNamed: className3
			super: className2
			instvars: #(ivar4 ivar3)
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		gs_constraints: { { 'ivar3' . 'Association' }. { 'ivar4' . 'Association' }.  { 'ivar1' . 'SmallInteger' } };
		yourself.
	packageDefinition 
		addClassDefinition: classDefinition.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class1 := Rowan globalNamed: className1.
	class2 := Rowan globalNamed: className2.
	class3 := Rowan globalNamed: className3.
	self assert: class1 instVarNames = #(ivar1).
	self assert: class1 category = 'category'.
	self assert: class1 comment = 'comment'.
	self assert: (class1 _constraintsEqual: { { #'ivar1' . Number } }).
	self assert: class2 instVarNames = #(ivar2).
	self assert: (class2 _constraintsEqual: {  { #'ivar1' . Integer } . { #'ivar2' . Association } }).
	self assert: class3 instVarNames = #(ivar4 ivar3).
	self assert: (class3 _constraintsEqual: { { #'ivar1' . SmallInteger } . { #'ivar4' . Association }. { #'ivar3' . Association }. }).



"create new version"
	newClass1 := Object 
		rwSubclass: className1 
		instVarNames: #('ivar5' 'ivar1') 
		classVars: #() 
		classInstVars: #() 
		poolDictionaries: #() 
		category: 'category' 
		packageName:  packageName
		constraints: { { #'ivar1' . Number } . { #'ivar5' . Number } } 
		options: #().

"validate"
	oldClass1 := class1.
	class1 := Rowan globalNamed: className1.
	oldClass2 := class2.
	class2 := Rowan globalNamed: className2.
	oldClass3 := class3.
	class3 := Rowan globalNamed: className3.
	self assert: class1 == newClass1.
	self assert: oldClass1 ~~ class1.
	self assert: class1 instVarNames = #(ivar5 ivar1).
	self assert: class1 category = 'category'.
	self assert: class1 comment = 'comment'.
	self assert: (class1 _constraintsEqual: { { #'ivar5' . Number }. { #'ivar1' . Number }. }).
	self assert: oldClass2 ~~ class2.
	self assert: class2 instVarNames = #(ivar2).
	self assert: (class2 _constraintsEqual: { { #'ivar1' . Integer } . { #'ivar2' . Association } }).
	self assert: oldClass3 ~~ class3.
	self assert: class3 instVarNames = #(ivar4 ivar3).
	self assert: (class3 _constraintsEqual: { { #'ivar1' . SmallInteger } . { #'ivar4' . Association }. { #'ivar3' . Association }. }).

]

{ #category : 'tests-issue 293' }
RwRowanProjectIssuesTest >> testIssue293_constraint_ordering [

	"https://github.com/dalehenrich/Rowan/issues/293"

	"constraints should be displayed in inst var order, not alphabetical order"

	| projectName  packageName projectDefinition classDefinition packageDefinition className1 className2 className3 projectSetDefinition class |
	projectName := 'Issue255'.
	packageName := 'Issue255-Core'.
	className1 := 'Issue255Class1'.
	className2 := 'Issue255Subclass2'.
	className3 := 'Issue255Subclass3'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	packageDefinition := projectDefinition packageNamed: packageName.

	classDefinition := RwClassDefinition
		newForClassNamed: className1
		super: 'Object'
		instvars: #( ivar2 ivar3 ivar4 ivar1)
		classinstvars: #()
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition gs_constraints: { { 'ivar4' . 'Association' }. { 'ivar3' . 'Association' }. { 'ivar2' . 'Association' }. { 'ivar1' . 'Association' }. }.
	packageDefinition 
		addClassDefinition: classDefinition.

	classDefinition := RwClassDefinition
		newForClassNamed: className2
		super: className1
		instvars: #( ivar7 ivar6)
		classinstvars: #()
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition gs_constraints: { { 'ivar7' . 'Association' }. { 'ivar6' . 'Association' }. }.
	packageDefinition 
		addClassDefinition: classDefinition.

	classDefinition := RwClassDefinition
		newForClassNamed: className3
		super: className2
		instvars: #( ivar8 ivar9)
		classinstvars: #()
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition gs_constraints: { { 'ivar9' . 'Association' }. { 'ivar8' . 'Association' }. }.
	packageDefinition 
		addClassDefinition: classDefinition.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class := Rowan globalNamed: className1.
	self assert: (class _constraintsEqual: {  { #'ivar1' . Association }.  { #'ivar2' . Association }. { #'ivar3' . Association }. { #'ivar4' . Association }.  }).

	class := Rowan globalNamed: className2.
	self assert: (class _constraintsEqual: {  { #'ivar7' . Association }.  { #'ivar6' . Association }. }).

	class := Rowan globalNamed: className3.
	self assert: (class _constraintsEqual: {  { #'ivar8' . Association }.  { #'ivar9' . Association }. }).

]

{ #category : 'tests-issue 305' }
RwRowanProjectIssuesTest >> testIssue305_delete_package_with_class [

	"https://github.com/dalehenrich/Rowan/issues/305"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition packageDefinition className projectSetDefinition class |
	projectName := 'Issue305'.
	packageName1 := 'Issue305-Core1'.
	packageName2 := 'Issue305-Core2'.
	className := 'Issue305Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	projectDefinition1 := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.

	packageDefinition := projectDefinition1 packageNamed: packageName2.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName2.

	projectSetDefinition := RwProjectSetDefinition new.
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.

	projectDefinition2 removePackageNamed: packageName2.

	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition_254: projectSetDefinition.

	class := Rowan globalNamed: className.
	self assert: class isNil.

]

{ #category : 'tests-issue 305' }
RwRowanProjectIssuesTest >> testIssue305_delete_package_with_extension_method [

	"https://github.com/dalehenrich/Rowan/issues/305"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition 
		classExtension packageDefinition className projectSetDefinition class oldClass |
	projectName := 'Issue305'.
	packageName1 := 'Issue305-Core1'.
	packageName2 := 'Issue305-Core2'.
	className := 'Issue305Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	projectDefinition1 := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	classExtension := (RwClassExtensionDefinition newForClassNamed: className)
		addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'foo'
						protocol: '*', packageName2 asLowercase
						source: 'foo ^ ''foo''');
		addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'foo'
						protocol: '*', packageName2 asLowercase
						source: 'foo ^ ''foo''');
		yourself.

	packageDefinition := projectDefinition1 packageNamed: packageName2.
	packageDefinition addClassExtension: classExtension.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: class foo = 'foo'.
	self assert: class new foo = 'foo'.

"remove package"
	projectSetDefinition := RwProjectSetDefinition new.
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.

	projectDefinition2 removePackageNamed: packageName2.

"load"
	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition_254: projectSetDefinition.

"validate"
	oldClass := class.
	class := Rowan globalNamed: className.
	self assert: class == oldClass.
	self should: [ class foo = 'foo' ] raise: MessageNotUnderstood.
	self should: [ class new foo = 'foo' ] raise: MessageNotUnderstood.

]

{ #category : 'tests-issue 310' }
RwRowanProjectIssuesTest >> testIssue310 [

       "https://github.com/dalehenrich/Rowan/issues/310"

       "original test case for HR9880 - testing interactive api load and unpackaged (disowned) subclasses"

		"unpackaged subclasses of a packaged class are to be ignored when a new version of the packaged class is created"

       | projectName  packageName projectDefinition classDefinition1 classDefinition packageDefinition className1 
               projectSetDefinition baseClass oldClass |

       projectName := 'HR9880'.
       packageName := 'HR9880-Core'.
       className1 := 'HR9880'.
       {projectName}
               do: [ :pn | 
                       (Rowan image loadedProjectNamed: pn ifAbsent: [  ])
                               ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
       projectDefinition := (RwProjectDefinition
               newForGitBasedProjectNamed: projectName)
               addPackageNamed: packageName;
               defaultSymbolDictName: self _symbolDictionaryName1;
               yourself.

       packageDefinition := projectDefinition packageNamed: packageName.

       classDefinition1 := RwClassDefinition
               newForClassNamed: className1 
                       super: 'Object'
                       instvars: #(iv1 iv2)
                       classinstvars: #()
                       classvars: #()
                       category:packageName
                       comment: 'comment'
                       pools: #()
                       type: 'normal'.
       packageDefinition 
               addClassDefinition: classDefinition1.

       1 to: 10 do: [:index | 

               classDefinition := RwClassDefinition
                       newForClassNamed: className1, '_', index printString
                               super: className1
                               instvars: #()
                               classinstvars: #()
                               classvars: #()
                               category: packageName
                               comment: 'comment'
                               pools: #()
                               type: 'normal'.
               packageDefinition 
                       addClassDefinition: classDefinition.

               classDefinition
                       addInstanceMethodDefinition:
                               (RwMethodDefinition
                                               newForSelector: #'mover'
                                               protocol: 'accessing'
                                               source: 'foo ^1') ].


"load"
       projectSetDefinition := RwProjectSetDefinition new.
       projectSetDefinition addDefinition: projectDefinition.
       Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"

       baseClass := Rowan globalNamed: className1.
       1 to: 10 do: [:index | 
               | theClass |

               theClass := Rowan globalNamed: className1 , '_', index printString.

               self assert: theClass superclass == baseClass.
               self assert: (theClass compiledMethodAt: #foo) rowanPackageName = packageName ].

"disown the subclasses"
       1 to: 10 do: [:index | 
               Rowan packageTools disown disownClassNamed: className1 , '_', index printString ].
 
"modify class -- new version"

       TestCase rwSubclass:className1
       instVarNames: #( iv1 iv2)
       classVars: #()
       classInstVars: #()
       poolDictionaries: #()
       category: packageName
       options: #().

"validate"
       oldClass := baseClass.
       baseClass := Rowan globalNamed: className1.
       self assert: oldClass ~~ baseClass.
       1 to: 10 do: [:index | 
               | theClass |

               theClass := Rowan globalNamed: className1 , '_', index printString.

               self assert: theClass superclass == oldClass.
               self assert: (theClass compiledMethodAt: #foo) rowanPackageName = Rowan unpackagedName ].

]

{ #category : 'tests-issue 313' }
RwRowanProjectIssuesTest >> testIssue313_1 [

	"https://github.com/dalehenrich/Rowan/issues/313"

	"initial test case for HR9880 - testing definition based load"

	| projectName  packageName projectDefinition classDefinition1 classDefinition packageDefinition className1 
		projectSetDefinition baseClass oldClass |

	projectName := 'HR9880'.
	packageName := 'HR9880-Core'.
	className1 := 'HR9880'.
	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	packageDefinition := projectDefinition packageNamed: packageName.

	classDefinition1 := RwClassDefinition
		newForClassNamed: className1 
			super: 'Object'
			instvars: #(iv1 iv2)
			classinstvars: #()
			classvars: #()
			category:packageName
			comment: 'comment'
			pools: #()
			type: 'normal'.
	packageDefinition 
		addClassDefinition: classDefinition1.

	1 to: 10 do: [:index | 

		classDefinition := RwClassDefinition
			newForClassNamed: className1, '_', index printString
				super: className1
				instvars: #()
				classinstvars: #()
				classvars: #()
				category: packageName
				comment: 'comment'
				pools: #()
				type: 'normal'.
		packageDefinition 
			addClassDefinition: classDefinition.

		classDefinition
			addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'mover'
						protocol: 'accessing'
						source: 'foo ^1') ].


"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"

	baseClass := Rowan globalNamed: className1.
	1 to: 10 do: [:index | 
		| theClass |

		theClass := Rowan globalNamed: className1 , '_', index printString.

		self assert: theClass superclass == baseClass.
		self assert: (theClass compiledMethodAt: #foo) rowanPackageName = packageName ].

"modify class -- new version"
	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	packageDefinition := projectDefinition packageNamed: packageName.
	classDefinition1 := packageDefinition classDefinitions at: className1.
	classDefinition1 instVarNames: #(iv3).

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	oldClass := baseClass.
	baseClass := Rowan globalNamed: className1.
	self assert: oldClass ~~ baseClass.
	1 to: 10 do: [:index | 
		| theClass |

		theClass := Rowan globalNamed: className1 , '_', index printString.

		self assert: theClass superclass == baseClass.
		self assert: (theClass compiledMethodAt: #foo) rowanPackageName = packageName ].

]

{ #category : 'tests-issue 313' }
RwRowanProjectIssuesTest >> testIssue313_2 [

	"https://github.com/dalehenrich/Rowan/issues/313"

	"test case for HR9880 - testing definition based load"

	"change superclass to TestCase to create new version ... instead of inst var addition"

	| projectName  packageName projectDefinition classDefinition1 classDefinition packageDefinition className1 
		projectSetDefinition baseClass oldClass |

	projectName := 'HR9880'.
	packageName := 'HR9880-Core'.
	className1 := 'HR9880'.
	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	packageDefinition := projectDefinition packageNamed: packageName.

	classDefinition1 := RwClassDefinition
		newForClassNamed: className1 
			super: 'Object'
			instvars: #(iv1 iv2)
			classinstvars: #()
			classvars: #()
			category:packageName
			comment: 'comment'
			pools: #()
			type: 'normal'.
	packageDefinition 
		addClassDefinition: classDefinition1.

	1 to: 10 do: [:index | 

		classDefinition := RwClassDefinition
			newForClassNamed: className1, '_', index printString
				super: className1
				instvars: #()
				classinstvars: #()
				classvars: #()
				category: packageName
				comment: 'comment'
				pools: #()
				type: 'normal'.
		packageDefinition 
			addClassDefinition: classDefinition.

		classDefinition
			addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'mover'
						protocol: 'accessing'
						source: 'foo ^1') ].


"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"

	baseClass := Rowan globalNamed: className1.
	1 to: 10 do: [:index | 
		| theClass |

		theClass := Rowan globalNamed: className1 , '_', index printString.

		self assert: theClass superclass == baseClass.
		self assert: (theClass compiledMethodAt: #foo) rowanPackageName = packageName ].

"modify class -- new version"
	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	packageDefinition := projectDefinition packageNamed: packageName.
	classDefinition1 := packageDefinition classDefinitions at: className1.
	classDefinition1 superclassName: 'TestCase'.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	oldClass := baseClass.
	baseClass := Rowan globalNamed: className1.
	self assert: oldClass ~~ baseClass.
	1 to: 10 do: [:index | 
		| theClass |

		theClass := Rowan globalNamed: className1 , '_', index printString.

		self assert: theClass superclass == baseClass.
		self assert: (theClass compiledMethodAt: #foo) rowanPackageName = packageName ].

]

{ #category : 'tests-issue 313' }
RwRowanProjectIssuesTest >> testIssue313_3 [

	"https://github.com/dalehenrich/Rowan/issues/313"

	"test case for HR9880 - testing interactive api load"

	| projectName  packageName projectDefinition classDefinition1 classDefinition packageDefinition className1 
		projectSetDefinition baseClass oldClass |

	projectName := 'HR9880'.
	packageName := 'HR9880-Core'.
	className1 := 'HR9880'.
	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	packageDefinition := projectDefinition packageNamed: packageName.

	classDefinition1 := RwClassDefinition
		newForClassNamed: className1 
			super: 'Object'
			instvars: #(iv1 iv2)
			classinstvars: #()
			classvars: #()
			category:packageName
			comment: 'comment'
			pools: #()
			type: 'normal'.
	packageDefinition 
		addClassDefinition: classDefinition1.

	1 to: 10 do: [:index | 

		classDefinition := RwClassDefinition
			newForClassNamed: className1, '_', index printString
				super: className1
				instvars: #()
				classinstvars: #()
				classvars: #()
				category: packageName
				comment: 'comment'
				pools: #()
				type: 'normal'.
		packageDefinition 
			addClassDefinition: classDefinition.

		classDefinition
			addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'mover'
						protocol: 'accessing'
						source: 'foo ^1') ].


"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"

	baseClass := Rowan globalNamed: className1.
	1 to: 10 do: [:index | 
		| theClass |

		theClass := Rowan globalNamed: className1 , '_', index printString.

		self assert: theClass superclass == baseClass.
		self assert: (theClass compiledMethodAt: #foo) rowanPackageName = packageName ].

"modify class -- new version"

	TestCase rwSubclass:className1
	instVarNames: #( iv1 iv2)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	category: packageName
	options: #().

"validate"
	oldClass := baseClass.
	baseClass := Rowan globalNamed: className1.
	self assert: oldClass ~~ baseClass.
	1 to: 10 do: [:index | 
		| theClass |

		theClass := Rowan globalNamed: className1 , '_', index printString.

		self assert: theClass superclass == baseClass.
		self assert: (theClass compiledMethodAt: #foo) rowanPackageName = packageName ].

]

{ #category : 'tests-issue 313' }
RwRowanProjectIssuesTest >> testIssue313_4 [

	"https://github.com/dalehenrich/Rowan/issues/313"

	"test case for HR9880 - testing interactive api load and test methods as extension methods"

	| projectName  packageName1 packageName2 projectDefinition classDefinition1 classDefinition 
		packageDefinition1 packageDefinition2 className1 projectSetDefinition baseClass oldClass 
		classExtensionDefinition |

	projectName := 'HR9880'.
	packageName1 := 'HR9880-Core'.
	packageName2 := 'HR9880-Extension'.
	className1 := 'HR9880'.
	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	packageDefinition1 := projectDefinition packageNamed: packageName1.
	packageDefinition2 := projectDefinition packageNamed: packageName2.

	classDefinition1 := RwClassDefinition
		newForClassNamed: className1 
			super: 'Object'
			instvars: #(iv1 iv2)
			classinstvars: #()
			classvars: #()
			category:packageName1
			comment: 'comment'
			pools: #()
			type: 'normal'.
	packageDefinition1
		addClassDefinition: classDefinition1.

	1 to: 10 do: [:index | 
		| className |

		className := className1, '_', index printString.

		classDefinition := RwClassDefinition
			newForClassNamed: className
				super: className1
				instvars: #()
				classinstvars: #()
				classvars: #()
				category: packageName1
				comment: 'comment'
				pools: #()
				type: 'normal'.
		packageDefinition1 addClassDefinition: classDefinition.

		classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className.
		classExtensionDefinition
			addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'mover'
						protocol: '*', packageName2 asLowercase
						source: 'foo ^1').
		packageDefinition2 addClassExtension: classExtensionDefinition ].


"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"

	baseClass := Rowan globalNamed: className1.
	1 to: 10 do: [:index | 
		| theClass |

		theClass := Rowan globalNamed: className1 , '_', index printString.

		self assert: theClass superclass == baseClass.
		self assert: (theClass compiledMethodAt: #foo) rowanPackageName = packageName2 ].

"modify class -- new version"

	TestCase rwSubclass:className1
	instVarNames: #( iv1 iv2)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	category: packageName1
	options: #().

"validate"
	oldClass := baseClass.
	baseClass := Rowan globalNamed: className1.
	self assert: oldClass ~~ baseClass.
	1 to: 10 do: [:index | 
		| theClass |

		theClass := Rowan globalNamed: className1 , '_', index printString.

		self assert: theClass superclass == baseClass.
		self assert: (theClass compiledMethodAt: #foo) rowanPackageName = packageName2 ].

]

{ #category : 'tests-issue 313' }
RwRowanProjectIssuesTest >> testIssue313_5 [

	"https://github.com/dalehenrich/Rowan/issues/313"

	"test case for HR9880 - testing interactive api load and subclasses in different packages"

	| projectName  packageName projectDefinition classDefinition1 classDefinition packageDefinition className1 
		projectSetDefinition baseClass oldClass |

	projectName := 'HR9880'.
	packageName := 'HR9880-Core'.
	className1 := 'HR9880'.
	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.
	1 to: 10 do: [:index | 
		projectDefinition addPackageNamed: packageName, '-', index printString ].


	packageDefinition := projectDefinition packageNamed: packageName.

	classDefinition1 := RwClassDefinition
		newForClassNamed: className1 
			super: 'Object'
			instvars: #(iv1 iv2)
			classinstvars: #()
			classvars: #()
			category:packageName
			comment: 'comment'
			pools: #()
			type: 'normal'.
	packageDefinition 
		addClassDefinition: classDefinition1.

	1 to: 10 do: [:index | 
		| thePackageName thePackageDefinition |
		thePackageName := packageName, '-', index printString.
		thePackageDefinition := projectDefinition packageNamed: thePackageName.

		classDefinition := RwClassDefinition
			newForClassNamed: className1, '_', index printString
				super: className1
				instvars: #()
				classinstvars: #()
				classvars: #()
				category: packageName
				comment: 'comment'
				pools: #()
				type: 'normal'.
		thePackageDefinition 
			addClassDefinition: classDefinition.

		classDefinition
			addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'mover'
						protocol: 'accessing'
						source: 'foo ^1') ].


"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"

	baseClass := Rowan globalNamed: className1.
	1 to: 10 do: [:index | 
		| theClass thePackageName |

		theClass := Rowan globalNamed: className1 , '_', index printString.
		thePackageName := packageName, '-', index printString.

		self assert: theClass superclass == baseClass.
		self assert: (theClass compiledMethodAt: #foo) rowanPackageName = thePackageName ].

"modify class -- new version"

	TestCase rwSubclass:className1
	instVarNames: #( iv1 iv2)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	category: packageName
	options: #().

"validate"
	oldClass := baseClass.
	baseClass := Rowan globalNamed: className1.
	self assert: oldClass ~~ baseClass.
	1 to: 10 do: [:index | 
		| theClass thePackageName |

		theClass := Rowan globalNamed: className1 , '_', index printString.
		thePackageName := packageName, '-', index printString.

		self assert: theClass superclass == baseClass.
		self assert: (theClass compiledMethodAt: #foo) rowanPackageName = thePackageName ].

]

{ #category : 'tests-issue 313' }
RwRowanProjectIssuesTest >> testIssue313_6 [

	"https://github.com/dalehenrich/Rowan/issues/313"

	"test case for HR9880 - testing interactive api load and subclasses in different projects"

	| projectName1 projectName2  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition1 
		classDefinition packageDefinition className1 projectSetDefinition baseClass oldClass |

	projectName1 := 'HR9880_1'.
	projectName2 := 'HR9880_2'.
	packageName1 := 'HR9880_1-Core'.
	packageName2 := 'HR9880_2-Core'.
	className1 := 'HR9880'.
	{projectName1 . projectName2 }
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create projects"
	projectDefinition1 := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName1)
		addPackageNamed: packageName1;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.
	projectDefinition2 := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName2)
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	1 to: 10 do: [:index | 
		projectDefinition2 addPackageNamed: packageName2, '-', index printString ].


	packageDefinition := projectDefinition1 packageNamed: packageName1.

	classDefinition1 := RwClassDefinition
		newForClassNamed: className1 
			super: 'Object'
			instvars: #(iv1 iv2)
			classinstvars: #()
			classvars: #()
			category:packageName1
			comment: 'comment'
			pools: #()
			type: 'normal'.
	packageDefinition 
		addClassDefinition: classDefinition1.

	1 to: 10 do: [:index | 
		| thePackageName thePackageDefinition |
		thePackageName := packageName2, '-', index printString.
		thePackageDefinition := projectDefinition2 packageNamed: thePackageName.

		classDefinition := RwClassDefinition
			newForClassNamed: className1, '_', index printString
				super: className1
				instvars: #()
				classinstvars: #()
				classvars: #()
				category: thePackageName
				comment: 'comment'
				pools: #()
				type: 'normal'.
		thePackageDefinition 
			addClassDefinition: classDefinition.

		classDefinition
			addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'mover'
						protocol: 'accessing'
						source: 'foo ^1') ].

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition 
		addDefinition: projectDefinition1;
		addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"

	baseClass := Rowan globalNamed: className1.
	self assert: baseClass rowanPackageName = packageName1.
	1 to: 10 do: [:index | 
		| theClass thePackageName |

		theClass := Rowan globalNamed: className1 , '_', index printString.
		thePackageName := packageName2, '-', index printString.

		self assert: theClass superclass == baseClass.
		self assert: theClass rowanPackageName = thePackageName.
		self assert: (theClass compiledMethodAt: #foo) rowanPackageName = thePackageName ].

"modify class -- new version"

	TestCase rwSubclass:className1
	instVarNames: #( iv1 iv2)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	category: packageName1
	options: #().

"validate"
	oldClass := baseClass.
	baseClass := Rowan globalNamed: className1.
	self assert: baseClass rowanPackageName = packageName1.
	self assert: oldClass ~~ baseClass.
	1 to: 10 do: [:index | 
		| theClass thePackageName |

		theClass := Rowan globalNamed: className1 , '_', index printString.
		thePackageName := packageName2, '-', index printString.

		self assert: theClass superclass == baseClass.
		self assert: theClass rowanPackageName = thePackageName.
		self assert: (theClass compiledMethodAt: #foo) rowanPackageName = thePackageName ].

]

{ #category : 'tests-issue 325' }
RwRowanProjectIssuesTest >> testIssue325 [

	"https://github.com/dalehenrich/Rowan/issues/325"

	| projectName  packageName1 packageName2 packageName3 projectDefinition1 classDefinition packageDefinition 
		className1 className2 className3 projectSetDefinition theSymbolDict myUserProfile mySymbolList theSymDictIndex |

	projectName := 'Issue325'.
	packageName1 := 'Issue325-Core1'.
	packageName2 := 'Issue325-Core2'.
	packageName3 := 'Issue325-Core3'.
	className1 := 'Issue325Class1'. 
	className2 := 'Issue325Class2'. 
	className3 := 'Issue325Class3'. 

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create definitions"
	projectDefinition1 := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		addPackageNamed: packageName3;
		setSymbolDictName: self _symbolDictionaryName forPackageNamed: packageName1;
		setSymbolDictName: self _symbolDictionaryName1 forPackageNamed: packageName2;
		setSymbolDictName: self _symbolDictionaryName2 forPackageNamed: packageName3;
		yourself.

	classDefinition := RwClassDefinition
		newForClassNamed: className1
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName1
		comment: ''
		pools: #()
		type: 'normal'.
	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	classDefinition := RwClassDefinition
		newForClassNamed: className2
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName2
		comment: ''
		pools: #()
		type: 'normal'.
	packageDefinition := projectDefinition1 packageNamed: packageName2.
	packageDefinition addClassDefinition: classDefinition.

	classDefinition := RwClassDefinition
		newForClassNamed: className3
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: packageName3
		comment: ''
		pools: #()
		type: 'normal'.
	packageDefinition := projectDefinition1 packageNamed: packageName3.
	packageDefinition addClassDefinition: classDefinition.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"test"
	theSymbolDict := Rowan globalNamed: self _symbolDictionaryName.
	self assert: theSymbolDict class == SymbolDictionary.			"symbol dictionary is accessible in my symbol list"

	self assert: (Rowan image packageNames includes: packageName1).	"package 1 is visible"
	self assert: (Rowan globalNamed: className1) notNil.							"and class1 is visible"
	self assert: (Rowan image packageNames includes: packageName2).	"package 2 is visible"
	self assert: (Rowan globalNamed: className2) notNil.							"and class1 3 is visible"
	self assert: (Rowan image packageNames includes: packageName3).	"package is visible"
	self assert: (Rowan globalNamed: className3) notNil.							"and class1 is visible"

	myUserProfile := System myUserProfile.
	mySymbolList := System myUserProfile symbolList.
	theSymDictIndex := mySymbolList indexOf: theSymbolDict.
	myUserProfile removeDictionaryAt: theSymDictIndex.				"remove symbol dictionary from my symbol list"

	self deny: (Rowan image packageNames includes: packageName1).	"package 1 is visible"
	self deny: (Rowan globalNamed: className1) notNil.							"and class1 is visible"
	self assert: (Rowan image packageNames includes: packageName2).	"package 2 is visible"
	self assert: (Rowan globalNamed: className2) notNil.							"and class1 3 is visible"
	self assert: (Rowan image packageNames includes: packageName3).	"package is visible"
	self assert: (Rowan globalNamed: className3) notNil.							"and class1 is visible"

	self assert: (Rowan image packageNamesForLoadedProjectNamed: projectName) sort = {packageName2. packageName3} sort

]

{ #category : 'tests-issue 326' }
RwRowanProjectIssuesTest >> testIssue326_class_modification_remove_constraint [

	"test modifying constraints remove constraint"
	"https://github.com/dalehenrich/Rowan/issues/326"

	| projectName  packageName projectDefinition classDefinition1 packageDefinition className1 
		projectSetDefinition class1 oldClass x |

	projectName := 'Issue326'.
	packageName := 'Issue326-Core'.
	className1 := 'Issue326Class1'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project - class with constraints"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	classDefinition1 := RwClassDefinition
		newForClassNamed: className1
		super: 'Object'
		instvars: #(ivar1 ivar2)
		classinstvars: #()
		classvars: #()
		category: 'category'
		comment: 'comment'
		pools: #()
		type: 'normal'.
	classDefinition1
		gs_constraints: { { 'ivar1' . 'Association' } . { 'ivar2' . 'Number' } }.

	packageDefinition := projectDefinition packageNamed: packageName.
	packageDefinition 
		addClassDefinition: classDefinition1.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class1 := Rowan globalNamed: className1.
	self assert: class1 instVarNames = #(ivar1  ivar2).
	self assert: (x := class1 classVarNames asSortedCollection) = #() asSortedCollection.
	self assert: class1 category = 'category'.
	self assert: class1 comment = 'comment'.
	self assert: (class1 _constraintsEqual: { { #'ivar2' . Number } . { #'ivar1' . Association } }).

"remove a constraint"
	classDefinition1
		gs_constraints: { { 'ivar2' . 'Number' } }.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	oldClass := class1.
	class1 := Rowan globalNamed: className1.
	self assert: oldClass == class1.
	self assert: class1 instVarNames = #(ivar1 ivar2).
	self assert: (class1 _constraintsEqual: { { #'ivar2' . Number } }).

]

{ #category : 'tests-issue 326' }
RwRowanProjectIssuesTest >> testIssue326_new_version_class_with_subclass_constraints_remove_constraint [

	"https://github.com/dalehenrich/Rowan/issues/326"

	"subclass overrides constraint of superclass (ivar1 has different constraint for each of classes)"

	| projectName  packageName projectDefinition classDefinition1 classDefinition packageDefinition className1 className2 className3 
		projectSetDefinition class1 class2 class3 oldClass1 oldClass2 oldClass3 newClass1 |

	projectName := 'Issue326'.
	packageName := 'Issue326-Core'.
	className1 := 'Issue326Class1'.
	className2 := 'Issue326Class2'.
	className3 := 'Issue326Class3'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	packageDefinition := projectDefinition packageNamed: packageName.

	classDefinition1 := (RwClassDefinition
		newForClassNamed: className1
			super: 'Object'
			instvars: #(ivar1 ivar5)
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		gs_constraints: { { 'ivar1' . 'Number' } . { 'ivar5' . 'Number' } };
		yourself.
	packageDefinition 
		addClassDefinition: classDefinition1.

	classDefinition := (RwClassDefinition
		newForClassNamed: className2
			super: className1
			instvars: #(ivar2)
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		gs_constraints: { { 'ivar2' . 'Association' }  };
		yourself.
	packageDefinition 
		addClassDefinition: classDefinition.

	classDefinition := (RwClassDefinition
		newForClassNamed: className3
			super: className2
			instvars: #(ivar4 ivar3)
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		gs_constraints: { { 'ivar3' . 'Association' }. { 'ivar4' . 'Association' } };
		yourself.
	packageDefinition 
		addClassDefinition: classDefinition.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class1 := Rowan globalNamed: className1.
	class2 := Rowan globalNamed: className2.
	class3 := Rowan globalNamed: className3.
	self assert: class1 instVarNames = #(ivar1 ivar5).
	self assert: class1 category = 'category'.
	self assert: class1 comment = 'comment'.
	self assert: (class1 _constraintsEqual: { { #'ivar1' . Number } . { #'ivar5' . Number } }).
	self assert: class2 instVarNames = #(ivar2).
	self assert: (class2 _constraintsEqual: {  { #'ivar1' . Number } . { #'ivar5' . Number } . { #'ivar2' . Association } }).
	self assert: class3 instVarNames = #(ivar4 ivar3).
	self assert: (class3 _constraintsEqual: { { #'ivar1' . Number } . { #'ivar5' . Number } . { #'ivar4' . Association }. { #'ivar3' . Association }. }).

"remove constraints in className1 (superclass)"
	newClass1 := Object 
		rwSubclass: className1 
		instVarNames: #('ivar1' 'ivar5') 
		classVars: #() 
		classInstVars: #() 
		poolDictionaries: #() 
		category: 'category' 
		packageName:  packageName
		constraints: { { 'ivar5' . Number } } 
		options: #().

"validate"
	oldClass1 := class1.
	class1 := Rowan globalNamed: className1.
	oldClass2 := class2.
	class2 := Rowan globalNamed: className2.
	oldClass3 := class3.
	class3 := Rowan globalNamed: className3.
	self assert: class1 == newClass1.
	self assert: oldClass1 == class1.
	self assert: class1 instVarNames = #(ivar1 ivar5).
	self assert: (class1 _constraintsEqual: { { #'ivar5' . Number } }).
	self assert: oldClass2 == class2.
	self assert: class2 instVarNames = #(ivar2).
	self assert: (class2 _constraintsEqual: {  { #'ivar5' . Number } . { #'ivar2' . Association } }).
	self assert: oldClass3 == class3.
	self assert: class3 instVarNames = #(ivar4 ivar3).
	self assert: (class3 _constraintsEqual: { { #'ivar5' . Number } . { #'ivar4' . Association }. { #'ivar3' . Association }. }).

]

{ #category : 'tests-issue 326' }
RwRowanProjectIssuesTest >> testIssue326_new_version_class_with_subclass_constraints_remove_subclass_constraint [

	"https://github.com/dalehenrich/Rowan/issues/326"

	"subclass overrides constraint of superclass (ivar1 has different constraint for each of classes)"

	| projectName  packageName projectDefinition classDefinition1 classDefinition packageDefinition className1 className2 className3 
		projectSetDefinition class1 class2 class3 oldClass1 oldClass2 oldClass3 newClass |

	projectName := 'Issue326'.
	packageName := 'Issue326-Core'.
	className1 := 'Issue326Class1'.
	className2 := 'Issue326Class2'.
	className3 := 'Issue326Class3'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	packageDefinition := projectDefinition packageNamed: packageName.

	classDefinition1 := (RwClassDefinition
		newForClassNamed: className1
			super: 'Object'
			instvars: #(ivar1 ivar5)
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		gs_constraints: { { 'ivar1' . 'Number' } . { 'ivar5' . 'Number' } };
		yourself.
	packageDefinition 
		addClassDefinition: classDefinition1.

	classDefinition := (RwClassDefinition
		newForClassNamed: className2
			super: className1
			instvars: #(ivar2)
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		gs_constraints: { { 'ivar2' . 'Association' }  };
		yourself.
	packageDefinition 
		addClassDefinition: classDefinition.

	classDefinition := (RwClassDefinition
		newForClassNamed: className3
			super: className2
			instvars: #(ivar4 ivar3)
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		gs_constraints: { { 'ivar3' . 'Association' }. { 'ivar4' . 'Association' } };
		yourself.
	packageDefinition 
		addClassDefinition: classDefinition.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class1 := Rowan globalNamed: className1.
	class2 := Rowan globalNamed: className2.
	class3 := Rowan globalNamed: className3.
	self assert: class1 instVarNames = #(ivar1 ivar5).
	self assert: class1 category = 'category'.
	self assert: class1 comment = 'comment'.
	self assert: (class1 _constraintsEqual: { { #'ivar1' . Number } . { #'ivar5' . Number } }).
	self assert: class2 instVarNames = #(ivar2).
	self assert: (class2 _constraintsEqual: {  { #'ivar1' . Number } . { #'ivar5' . Number } . { #'ivar2' . Association } }).
	self assert: class3 instVarNames = #(ivar4 ivar3).
	self assert: (class3 _constraintsEqual: { { #'ivar1' . Number } . { #'ivar5' . Number } . { #'ivar4' . Association }. { #'ivar3' . Association }. }).

"remove constraints in className1 (superclass)"
	newClass := class2 
		rwSubclass: className3 
		instVarNames: #('ivar4' 'ivar3') 
		classVars: #() 
		classInstVars: #() 
		poolDictionaries: #() 
		category: 'category' 
		packageName:  packageName
		constraints: { { 'ivar3' . Association } } 
		options: #().

"validate"
	oldClass1 := class1.
	class1 := Rowan globalNamed: className1.
	oldClass2 := class2.
	class2 := Rowan globalNamed: className2.
	oldClass3 := class3.
	class3 := Rowan globalNamed: className3.
	self assert: class3 == newClass.
	self assert: oldClass1 == class1.
	self assert: class1 instVarNames = #(ivar1 ivar5).
	self assert: (class1 _constraintsEqual: {  { #'ivar1' . Number } . { #'ivar5' . Number } }).
	self assert: oldClass2 == class2.
	self assert: class2 instVarNames = #(ivar2).
	self assert: (class2 _constraintsEqual: {   { #'ivar1' . Number } . { #'ivar5' . Number } . { #'ivar2' . Association } }).
	self assert: oldClass3 == class3.
	self assert: class3 instVarNames = #(ivar4 ivar3).
	self assert: (class3 _constraintsEqual: {  { #'ivar1' . Number } . { #'ivar5' . Number } . { #'ivar3' . Association }. }).

]

{ #category : 'tests-issue 353' }
RwRowanProjectIssuesTest >> testIssue353_new_version_class_with_subclass_with_method_deletion_A [

	"reproduce bug"
	"https://github.com/dalehenrich/Rowan/issues/353"

	| projectName  packageName projectDefinition classDefinition packageDefinition className1 className2 className3 
		projectSetDefinition audit |

	projectName := 'Issue326'.
	packageName := 'Issue326-Core'.
	className1 := 'Issue326Class1'.
	className2 := 'Issue326Class2'.
	className3 := 'Issue326Class3'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	packageDefinition := projectDefinition packageNamed: packageName.

	classDefinition := (RwClassDefinition
		newForClassNamed: 'SuperMan'
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		yourself.

	packageDefinition 
		addClassDefinition: classDefinition.

	classDefinition := (RwClassDefinition
		newForClassNamed: className1
			super: 'SuperMan'
			instvars: #(ivar1 ivar5)
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		yourself.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method3'
					protocol: 'accessing'
					source: 'method3 ^3').
	classDefinition
		addClassMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method2'
					protocol: 'accessing'
					source: 'method2 ^2').

	packageDefinition 
		addClassDefinition: classDefinition.


"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.


"new class version of SuperMan and remove methods"
	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	classDefinition := (projectDefinition packageNamed: packageName) classDefinitions at: className1.
	classDefinition
		removeInstanceMethod: #method3;
		removeClassMethod: #method2;
		yourself.

	classDefinition := (projectDefinition packageNamed: packageName) classDefinitions at: 'SuperMan'.
	classDefinition instVarNames: #(ivar0).

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
]

{ #category : 'tests-issue 353' }
RwRowanProjectIssuesTest >> testIssue353_new_version_class_with_subclass_with_method_deletion_B [

	"(passing) companion test case"
	"https://github.com/dalehenrich/Rowan/issues/353"

	| projectName  packageName projectDefinition classDefinition packageDefinition className1 className2 className3 
		projectSetDefinition audit |

	projectName := 'Issue326'.
	packageName := 'Issue326-Core'.
	className1 := 'Issue326Class1'.
	className2 := 'Issue326Class2'.
	className3 := 'Issue326Class3'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	packageDefinition := projectDefinition packageNamed: packageName.

	classDefinition := (RwClassDefinition
		newForClassNamed: 'SuperMan'
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		yourself.

	packageDefinition 
		addClassDefinition: classDefinition.

	classDefinition := (RwClassDefinition
		newForClassNamed: className1
			super: 'SuperMan'
			instvars: #(ivar1 ivar5)
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		yourself.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method3'
					protocol: 'accessing'
					source: 'method3 ^3').
	classDefinition
		addClassMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method2'
					protocol: 'accessing'
					source: 'method2 ^2').

	packageDefinition 
		addClassDefinition: classDefinition.


"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.


"new class version of className1 and remove methods"
	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	classDefinition := (projectDefinition packageNamed: packageName) classDefinitions at: className1.
	classDefinition instVarNames: #(ivar0 ivar1 ivar5).
	classDefinition
		removeInstanceMethod: #method3;
		removeClassMethod: #method2;
		yourself.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
]

{ #category : 'tests-issue 353' }
RwRowanProjectIssuesTest >> testIssue353_new_version_class_with_subclass_with_method_deletion_C [

	"additional test case (fails as well)"
	"https://github.com/dalehenrich/Rowan/issues/353"

	| projectName  packageName projectDefinition classDefinition packageDefinition className1 className2 className3 
		projectSetDefinition audit |

	projectName := 'Issue326'.
	packageName := 'Issue326-Core'.
	className1 := 'Issue326Class1'.
	className2 := 'Issue326Class2'.
	className3 := 'Issue326Class3'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	packageDefinition := projectDefinition packageNamed: packageName.

	classDefinition := (RwClassDefinition
		newForClassNamed: 'SuperMan'
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		yourself.

	packageDefinition 
		addClassDefinition: classDefinition.

	classDefinition := (RwClassDefinition
		newForClassNamed: className1
			super: 'SuperMan'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		yourself.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method4'
					protocol: 'accessing'
					source: 'method4 ^3').
	classDefinition
		addClassMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method5'
					protocol: 'accessing'
					source: 'method5 ^2').

	packageDefinition 
		addClassDefinition: classDefinition.

	classDefinition := (RwClassDefinition
		newForClassNamed: className2
			super:className1
			instvars: #(ivar1 ivar5)
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		yourself.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method3'
					protocol: 'accessing'
					source: 'method3 ^3').
	classDefinition
		addClassMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method2'
					protocol: 'accessing'
					source: 'method2 ^2').

	packageDefinition 
		addClassDefinition: classDefinition.


"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.


"new class version of SuperMan and remove methods"
	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	classDefinition := (projectDefinition packageNamed: packageName) classDefinitions at: className1.
	classDefinition
		removeInstanceMethod: #method4;
		removeClassMethod: #method5;
		yourself.
	classDefinition := (projectDefinition packageNamed: packageName) classDefinitions at: className2.
	classDefinition
		removeInstanceMethod: #method3;
		removeClassMethod: #method2;
		yourself.

	classDefinition := (projectDefinition packageNamed: packageName) classDefinitions at: 'SuperMan'.
	classDefinition instVarNames: #(ivar0).

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
]

{ #category : 'tests-issue 353' }
RwRowanProjectIssuesTest >> testIssue353_new_version_class_with_subclass_with_method_deletion_D [

	"new test case: both superclass and subclass have new class versions and method removed from each sublclass"
	"https://github.com/dalehenrich/Rowan/issues/353"

	| projectName  packageName projectDefinition classDefinition packageDefinition className1 className2 className3 
		projectSetDefinition audit |

	projectName := 'Issue326'.
	packageName := 'Issue326-Core'.
	className1 := 'Issue326Class1'.
	className2 := 'Issue326Class2'.
	className3 := 'Issue326Class3'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	packageDefinition := projectDefinition packageNamed: packageName.

	classDefinition := (RwClassDefinition
		newForClassNamed: 'SuperMan'
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		yourself.

	packageDefinition 
		addClassDefinition: classDefinition.

	classDefinition := (RwClassDefinition
		newForClassNamed: className1
			super: 'SuperMan'
			instvars: #(ivar4 ivar6)
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		yourself.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method4'
					protocol: 'accessing'
					source: 'method4 ^3').
	classDefinition
		addClassMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method5'
					protocol: 'accessing'
					source: 'method5 ^2').

	packageDefinition 
		addClassDefinition: classDefinition.

	classDefinition := (RwClassDefinition
		newForClassNamed: className2
			super: 'SuperMan'
			instvars: #(ivar1 ivar5 ivar3)
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		yourself.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method3'
					protocol: 'accessing'
					source: 'method3 ^3').
	classDefinition
		addClassMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method2'
					protocol: 'accessing'
					source: 'method2 ^2').

	packageDefinition 
		addClassDefinition: classDefinition.


"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.


"new class version of SuperMan and remove methods"
	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	classDefinition := (projectDefinition packageNamed: packageName) classDefinitions at: className1.
	classDefinition instVarNames: #(ivar4).
	classDefinition
		removeInstanceMethod: #method4;
		removeClassMethod: #method5;
		yourself.
	classDefinition := (projectDefinition packageNamed: packageName) classDefinitions at: className2.
	classDefinition instVarNames: #(ivar1 ivar5).
	classDefinition
		removeInstanceMethod: #method3;
		removeClassMethod: #method2;
		yourself.

	classDefinition := (projectDefinition packageNamed: packageName) classDefinitions at: 'SuperMan'.
	classDefinition instVarNames: #(ivar0).

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
]

{ #category : 'tests-issue 393' }
RwRowanProjectIssuesTest >> testIssue393_new_version_class_with_subclass_with_extension_method_deletion_A [

	"new test case:superclass has new class version ... the subclass has an extension method removed --- simplest case"
	"https://github.com/dalehenrich/Rowan/issues/393"

	| projectName  packageName1 packageName2 projectDefinition classDefinition packageDefinition className1  
		projectSetDefinition audit classExtensionDefinition |

	projectName := 'Issue326'.
	packageName1 := 'Issue326-Core'.
	packageName2 := 'Issue326-Extension'.
	className1 := 'Issue326Class1'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	packageDefinition := projectDefinition packageNamed: packageName1.

	classDefinition := (RwClassDefinition
		newForClassNamed: 'SuperMan'
			super: 'Object'
			instvars: #(ivar0)
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		yourself.

	packageDefinition 
		addClassDefinition: classDefinition.

	classDefinition := (RwClassDefinition
		newForClassNamed: className1
			super: 'SuperMan'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		yourself.

	packageDefinition 
		addClassDefinition: classDefinition.

	packageDefinition := projectDefinition packageNamed: packageName2.

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className1.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: '*', packageName2 asLowercase
					source: 'method1 ^1').

	packageDefinition 
		addClassExtension: classExtensionDefinition.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.


"new class version of SuperMan and remove extension method"
	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	classDefinition := (projectDefinition packageNamed: packageName1) classDefinitions at: className1.

	classDefinition := (projectDefinition packageNamed: packageName1) classDefinitions at: 'SuperMan'.
	classDefinition instVarNames: #().

	classExtensionDefinition := (projectDefinition packageNamed: packageName2) classExtensions at:className1.
	classExtensionDefinition
		removeInstanceMethod: #method1;
		yourself.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
]

{ #category : 'tests-issue 393' }
RwRowanProjectIssuesTest >> testIssue393_new_version_class_with_subclass_with_method_deletion_E [

	"new test case:superclass has new class version ... each subclass has methods removed"
	"https://github.com/dalehenrich/Rowan/issues/393"

	| projectName  packageName projectDefinition classDefinition packageDefinition className1 className2 className3 
		projectSetDefinition audit |

	projectName := 'Issue326'.
	packageName := 'Issue326-Core'.
	className1 := 'Issue326Class1'.
	className2 := 'Issue326Class2'.
	className3 := 'Issue326Class3'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	packageDefinition := projectDefinition packageNamed: packageName.

	classDefinition := (RwClassDefinition
		newForClassNamed: 'SuperMan'
			super: 'Object'
			instvars: #(ivar0)
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		yourself.

	packageDefinition 
		addClassDefinition: classDefinition.

	classDefinition := (RwClassDefinition
		newForClassNamed: className1
			super: 'SuperMan'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		yourself.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method4'
					protocol: 'accessing'
					source: 'method4 ^3').
	classDefinition
		addClassMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method5'
					protocol: 'accessing'
					source: 'method5 ^2').

	packageDefinition 
		addClassDefinition: classDefinition.

	classDefinition := (RwClassDefinition
		newForClassNamed: className2
			super: 'SuperMan'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		yourself.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method3'
					protocol: 'accessing'
					source: 'method3 ^3').
	classDefinition
		addClassMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method2'
					protocol: 'accessing'
					source: 'method2 ^2').

	packageDefinition 
		addClassDefinition: classDefinition.


"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.


"new class version of SuperMan and remove methods"
	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	classDefinition := (projectDefinition packageNamed: packageName) classDefinitions at: className1.
	classDefinition
		removeInstanceMethod: #method4;
		removeClassMethod: #method5;
		yourself.
	classDefinition := (projectDefinition packageNamed: packageName) classDefinitions at: className2.
	classDefinition
		removeInstanceMethod: #method3;
		removeClassMethod: #method2;
		yourself.

	classDefinition := (projectDefinition packageNamed: packageName) classDefinitions at: 'SuperMan'.
	classDefinition instVarNames: #().

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.

"validate"
	self assert: ((Rowan image loadedClassNamed: className1) loadedInstanceMethods values select: [:loadedMethod | loadedMethod selector == #method4 ]) isEmpty
]

{ #category : 'tests-issue 393' }
RwRowanProjectIssuesTest >> testIssue393_new_version_class_with_subclass_with_method_deletion_F [

	"new test case:superclass has new class version ... each subclass has SOME methods removed"
	"https://github.com/dalehenrich/Rowan/issues/393"

	| projectName  packageName projectDefinition classDefinition packageDefinition className1 className2 className3 
		projectSetDefinition audit |

	projectName := 'Issue326'.
	packageName := 'Issue326-Core'.
	className1 := 'Issue326Class1'.
	className2 := 'Issue326Class2'.
	className3 := 'Issue326Class3'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	packageDefinition := projectDefinition packageNamed: packageName.

	classDefinition := (RwClassDefinition
		newForClassNamed: 'SuperMan'
			super: 'Object'
			instvars: #(ivar0)
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		yourself.

	packageDefinition 
		addClassDefinition: classDefinition.

	classDefinition := (RwClassDefinition
		newForClassNamed: className1
			super: 'SuperMan'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		yourself.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method4'
					protocol: 'accessing'
					source: 'method4 ^3').
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method5'
					protocol: 'accessing'
					source: 'method5 ^2').

	packageDefinition 
		addClassDefinition: classDefinition.

	classDefinition := (RwClassDefinition
		newForClassNamed: className2
			super: 'SuperMan'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		yourself.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method3'
					protocol: 'accessing'
					source: 'method3 ^3').
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method2'
					protocol: 'accessing'
					source: 'method2 ^2').

	packageDefinition 
		addClassDefinition: classDefinition.


"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.


"new class version of SuperMan and remove methods"
	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	classDefinition := (projectDefinition packageNamed: packageName) classDefinitions at: className1.
	classDefinition
		removeInstanceMethod: #method4;
		yourself.
	classDefinition := (projectDefinition packageNamed: packageName) classDefinitions at: className2.
	classDefinition
		removeInstanceMethod: #method3;
		yourself.

	classDefinition := (projectDefinition packageNamed: packageName) classDefinitions at: 'SuperMan'.
	classDefinition instVarNames: #().

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.

"validate"
	self assert: ((Rowan image loadedClassNamed: className1) loadedInstanceMethods values select: [:loadedMethod | loadedMethod selector == #method4 ]) isEmpty
]

{ #category : 'tests-issue 393' }
RwRowanProjectIssuesTest >> testIssue393_new_version_class_with_subclass_with_method_deletion_G [

	"new test case:superclass has new class version ... the subclass has a method removed --- simplest case"
	"https://github.com/dalehenrich/Rowan/issues/393"

	| projectName  packageName projectDefinition classDefinition packageDefinition className1  
		projectSetDefinition audit |

	projectName := 'Issue326'.
	packageName := 'Issue326-Core'.
	className1 := 'Issue326Class1'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	packageDefinition := projectDefinition packageNamed: packageName.

	classDefinition := (RwClassDefinition
		newForClassNamed: 'SuperMan'
			super: 'Object'
			instvars: #(ivar0)
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		yourself.

	packageDefinition 
		addClassDefinition: classDefinition.

	classDefinition := (RwClassDefinition
		newForClassNamed: className1
			super: 'SuperMan'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		yourself.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition 
		addClassDefinition: classDefinition.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.


"new class version of SuperMan and remove method"
	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	classDefinition := (projectDefinition packageNamed: packageName) classDefinitions at: className1.
	classDefinition
		removeInstanceMethod: #method1;
		yourself.

	classDefinition := (projectDefinition packageNamed: packageName) classDefinitions at: 'SuperMan'.
	classDefinition instVarNames: #().

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
]

{ #category : 'tests-issue 40' }
RwRowanProjectIssuesTest >> testIssue40 [

	"https://github.com/dalehenrich/Rowan/issues/40"

	| projectName packageName theClass1 theClass2 projectDefinition classDefinition |
	projectName := 'Issue40_project'.
	packageName := 'Issue40-Core'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject_39/' validate: false.
	self _addPackageNamed: packageName toProjectNamed: projectName validate: false.

	theClass1 := self 
		_addClassNamed: 'Issue40Class_1' 
		superclassName: 'Object' 
		classVars: #( CVar1 )
		toPackageNamed: packageName 
		inProjectNamed: projectName 
		validate: false.

	theClass2 := self 
		_addClassNamed: 'Issue40Class_2' 
		superclassName: 'Issue40Class_1' 
		classVars: #( CVar2 )
		toPackageNamed: packageName 
		inProjectNamed: projectName 
		validate: false.

	self _addOrUpdateMethod: 'cvar1 ^CVar1' forBehavior: theClass2 class inPackage: packageName inProjectNamed: projectName validate: false.
	self _addOrUpdateMethod: 'cvar2 ^CVar2' forBehavior: theClass2 class inPackage: packageName inProjectNamed: projectName validate: false.

	"add both class initializationMethods at once, so that order will be important - probably random order, unfortunately"
	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	classDefinition := (projectDefinition packageNamed: packageName) classDefinitions at:  'Issue40Class_1'.
	classDefinition addClassMethodDefinition: (RwMethodDefinition
						newForSelector: #'initialize'
						protocol: 'initialization'
						source: 'initialize CVar1 := 1.').
	classDefinition := (projectDefinition packageNamed: packageName) classDefinitions at:  'Issue40Class_2'.
	classDefinition addClassMethodDefinition: (RwMethodDefinition
						newForSelector: #'initialize'
						protocol: 'initialization'
						source: 'initialize CVar2 := CVar1 + 1.').
	[ Rowan projectTools load loadProjectDefinition: projectDefinition ]
			on: RwExecuteClassInitializeMethodsAfterLoadNotification
			do: [:ex | ex resume: true ].

	self assert: theClass2 cvar1 = 1.
	self assert: theClass2 cvar2 = 2.
]

{ #category : 'tests-issue 41' }
RwRowanProjectIssuesTest >> testIssue41_addUpdateInitializeExtensionMethods [

	"https://github.com/dalehenrich/Rowan/issues/41"

	"Are extension method initialization methods run correctly for add and update"

	| projectName packageName1 packageName2 className theClass initializeRun |
	projectName := 'Issue41_project'.
	packageName1 := 'Issue41-Core'.
	packageName2 := 'Issue41-Extensions'.
	className := 'Issue41Class'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject_40/' validate: false.
	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.
	theClass := self _updateClassNamed: className toPackageNamed: packageName1 inProjectNamed: projectName validate: false.

	self _addOrUpdateMethod: 'cvar1 ^CVar1' forBehavior: theClass class inPackage: packageName2 inProjectNamed: projectName validate: false.
	self _addOrUpdateMethod: 'cvar2 ^CVar2' forBehavior: theClass class inPackage: packageName2 inProjectNamed: projectName validate: false.
	
	initializeRun := false.
	[ self _addOrUpdateMethod: 'initialize CVar1 := 1' forBehavior: theClass class inPackage: packageName2 inProjectNamed: projectName validate: false ]
		on: RwExecuteClassInitializeMethodsAfterLoadNotification
		do: [:ex | 
			initializeRun := true.
			ex resume: true ].

	self assert: initializeRun.
	self assert: theClass cvar1 = 1.
	self assert: theClass cvar2 isNil.

	initializeRun := false.
	[ self _addOrUpdateMethod: 'initialize CVar1 := 1. CVar2 := 2' forBehavior: theClass class inPackage: packageName2 inProjectNamed: projectName validate: false ]
		on: RwExecuteClassInitializeMethodsAfterLoadNotification
		do: [:ex | 
			initializeRun := true.
			ex resume: true ].

	self assert: initializeRun.
	self assert: theClass cvar1 = 1.
	self assert: theClass cvar2 = 2.
]

{ #category : 'tests-issue 41' }
RwRowanProjectIssuesTest >> testIssue41_interactiveAddUpdateInitializeExtensionMethods [

	"https://github.com/dalehenrich/Rowan/issues/41"

	"Are extension method initialization methods run correctly for add and update - interactive edits should not trigger updates "

	| projectName packageName1 packageName2 className theClass  |
	projectName := 'Issue41_project'.
	packageName1 := 'Issue41-Core'.
	packageName2 := 'Issue41-Extensions'.
	className := 'Issue41Class'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject_41/' validate: false.
	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.
	theClass := self _updateClassNamed: className toPackageNamed: packageName1 inProjectNamed: projectName validate: false.

	self _addOrUpdateMethod: 'cvar1 ^CVar1' forBehavior: theClass class inPackage: packageName2 inProjectNamed: projectName validate: false.
	self _addOrUpdateMethod: 'cvar2 ^CVar2' forBehavior: theClass class inPackage: packageName2 inProjectNamed: projectName validate: false.
	
	self _addOrUpdateMethod: 'initialize CVar1 := 1' forBehavior: theClass class inPackage: packageName2 inProjectNamed: projectName validate: false.

	self assert: theClass cvar1 = 1.
	self assert: theClass cvar2 isNil.

	self _addOrUpdateMethod: 'initialize CVar1 := 1. CVar2 := 2' forBehavior: theClass class inPackage: packageName2 inProjectNamed: projectName validate: false.

	self assert: theClass cvar1 = 1.
	self assert: theClass cvar2 = 2.
]

{ #category : 'tests-issue 41' }
RwRowanProjectIssuesTest >> testIssue41_interactiveMoveInitializeExtensionMethodToPackage [

	"https://github.com/dalehenrich/Rowan/issues/41"

	"Are extension method initialization methods run correctly for add and update"

	| projectName packageName1 packageName2 packageName3 className theClass loadedPackage loadedClassExtensions |
	projectName := 'Issue41_project'.
	packageName1 := 'Issue41-Core'.
	packageName2 := 'Issue41-Extensions1'.
	packageName3 := 'Issue41-Extensions2'.
	className := 'Issue41Class'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject_42/' validate: false.
	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName3 toProjectNamed: projectName validate: false.
	theClass := self _updateClassNamed: className toPackageNamed: packageName1 inProjectNamed: projectName validate: false.

	self _addOrUpdateMethod: 'cvar1 ^CVar1' forBehavior: theClass class inPackage: packageName2 inProjectNamed: projectName validate: false.
	self _addOrUpdateMethod: 'cvar2 ^CVar2' forBehavior: theClass class inPackage: packageName2 inProjectNamed: projectName validate: false.
	self _addOrUpdateMethod: 'cvar1: anInt CVar1 := anInt' forBehavior: theClass class inPackage: packageName2 inProjectNamed: projectName validate: false.
	self _addOrUpdateMethod: 'cvar2: anInt CVar2 := anInt' forBehavior: theClass class inPackage: packageName2 inProjectNamed: projectName validate: false.
	
	"create initialize package as an extension method"
	self _addOrUpdateMethod: 'initialize CVar1 := 1' forBehavior: theClass class inPackage: packageName2 inProjectNamed: projectName validate: false.

	self assert: theClass cvar1 = 1.
	self assert: theClass cvar2 isNil.

	theClass 
		cvar1: 3;
		cvar2: 4.
	self assert: theClass cvar1 = 3.
	self assert: theClass cvar2 = 4.

	"initialize method changed in same package"
	self _addOrUpdateMethod: 'initialize "" CVar1 := 1. CVar2 := 2' forBehavior: theClass class inPackage: packageName2 inProjectNamed: projectName validate: false.

	self assert: theClass cvar1 = 1.
	self assert: theClass cvar2 = 2.

	loadedPackage := Rowan image loadedPackageNamed: packageName2.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: ((loadedClassExtensions at: className) classMethodDefinitions includesKey: #initialize).

	loadedPackage := Rowan image loadedPackageNamed: packageName3.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: (loadedClassExtensions at: className ifAbsent: []) isNil.

	theClass 
		cvar1: 5;
		cvar2: 6.
	self assert: theClass cvar1 = 5.
	self assert: theClass cvar2 = 6.

	"initialize method not changed and moved to new package"
	self _addOrUpdateMethod: 'initialize "" CVar1 := 1. CVar2 := 2' forBehavior: theClass class inPackage: packageName3 inProjectNamed: projectName validate: false.

	self assert: theClass cvar1 = 5.
	self assert: theClass cvar2 = 6.

	loadedPackage := Rowan image loadedPackageNamed: packageName3.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: ((loadedClassExtensions at: className) classMethodDefinitions includesKey: #initialize).

	loadedPackage := Rowan image loadedPackageNamed: packageName2.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self deny: ((loadedClassExtensions at: className) classMethodDefinitions includesKey: #initialize).

	theClass 
		cvar1: 5;
		cvar2: 6.
	self assert: theClass cvar1 = 5.
	self assert: theClass cvar2 = 6.

	"initialize method not changed and left in same package"
	self _addOrUpdateMethod: 'initialize "" CVar1 := 1. CVar2 := 2' forBehavior: theClass class inPackage: packageName3 inProjectNamed: projectName validate: false.

	self assert: theClass cvar1 = 5.
	self assert: theClass cvar2 = 6.

	"initialize method not changed and moved to new package"
	self _addOrUpdateMethod: 'initialize "" CVar1 := 1. CVar2 := 2' forBehavior: theClass class inPackage: packageName2 inProjectNamed: projectName validate: false.

	self assert: theClass cvar1 = 5.
	self assert: theClass cvar2 = 6.

	theClass initialize.

	self assert: theClass cvar1 = 1.
	self assert: theClass cvar2 = 2.

	theClass 
		cvar1: 5;
		cvar2: 6.
	self assert: theClass cvar1 = 5.
	self assert: theClass cvar2 = 6.

	"initialize method changed and moved to new package"
	self _addOrUpdateMethod: 'initialize CVar1 := 1. CVar2 := 2' forBehavior: theClass class inPackage: packageName3 inProjectNamed: projectName validate: false.

	self assert: theClass cvar1 = 1.
	self assert: theClass cvar2 = 2.
]

{ #category : 'tests-issue 41' }
RwRowanProjectIssuesTest >> testIssue41_moveInitializeExtensionMethodToPackage [

	"https://github.com/dalehenrich/Rowan/issues/41"

	"Are extension method initialization methods run correctly for add and update"

	| projectName packageName1 packageName2 packageName3 className theClass initializeRun |
	projectName := 'Issue41_project'.
	packageName1 := 'Issue41-Core'.
	packageName2 := 'Issue41-Extensions1'.
	packageName3 := 'Issue41-Extensions2'.
	className := 'Issue41Class'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject_43/' validate: false.
	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName3 toProjectNamed: projectName validate: false.
	theClass := self _updateClassNamed: className toPackageNamed: packageName1 inProjectNamed: projectName validate: false.

	self _addOrUpdateMethod: 'cvar1 ^CVar1' forBehavior: theClass class inPackage: packageName2 inProjectNamed: projectName validate: false.
	self _addOrUpdateMethod: 'cvar2 ^CVar2' forBehavior: theClass class inPackage: packageName2 inProjectNamed: projectName validate: false.
	self _addOrUpdateMethod: 'cvar1: anInt CVar1 := anInt' forBehavior: theClass class inPackage: packageName2 inProjectNamed: projectName validate: false.
	self _addOrUpdateMethod: 'cvar2: anInt CVar2 := anInt' forBehavior: theClass class inPackage: packageName2 inProjectNamed: projectName validate: false.
	
	initializeRun := false.
	[ self _addOrUpdateMethod: 'initialize CVar1 := 1' forBehavior: theClass class inPackage: packageName2 inProjectNamed: projectName validate: false ]
		on: RwExecuteClassInitializeMethodsAfterLoadNotification
		do: [:ex | 
			initializeRun := true.
			ex resume: true ].

	self assert: initializeRun.
	self assert: theClass cvar1 = 1.
	self assert: theClass cvar2 isNil.

	theClass 
		cvar1: 3;
		cvar2: 4.
	self assert: theClass cvar1 = 3.
	self assert: theClass cvar2 = 4.

	initializeRun := false.
	[	"initialize method changed in same package"
		self _addOrUpdateMethod: 'initialize "" CVar1 := 1. CVar2 := 2' forBehavior: theClass class inPackage: packageName2 inProjectNamed: projectName validate: false ]
			on: RwExecuteClassInitializeMethodsAfterLoadNotification
			do: [:ex | 
				initializeRun := true.
				ex resume: true ].

	self assert: initializeRun.
	self assert: theClass cvar1 = 1.
	self assert: theClass cvar2 = 2.

	theClass 
		cvar1: 5;
		cvar2: 6.
	self assert: theClass cvar1 = 5.
	self assert: theClass cvar2 = 6.

	initializeRun := false.
	[	"initialize method changed and moved to new package"
		self _addOrUpdateMethod: 'initialize CVar1 := 1. CVar2 := 2' forBehavior: theClass class inPackage: packageName3 inProjectNamed: projectName validate: false ]
			on: RwExecuteClassInitializeMethodsAfterLoadNotification
			do: [:ex | 
				initializeRun := true.
				ex resume: true ].

	self assert: initializeRun.
	self assert: theClass cvar1 = 1.
	self assert: theClass cvar2 = 2.

	theClass 
		cvar1: 5;
		cvar2: 6.
	self assert: theClass cvar1 = 5.
	self assert: theClass cvar2 = 6.

	initializeRun := false.
	[	"initialize method not changed and left in same package"
		self _addOrUpdateMethod: 'initialize CVar1 := 1. CVar2 := 2' forBehavior: theClass class inPackage: packageName3 inProjectNamed: projectName validate: false ]
			on: RwExecuteClassInitializeMethodsAfterLoadNotification
			do: [:ex | 
				initializeRun := true.
				ex resume: true ].

	self deny: initializeRun.
	self assert: theClass cvar1 = 5.
	self assert: theClass cvar2 = 6.

	theClass initialize.

	self assert: theClass cvar1 = 1.
	self assert: theClass cvar2 = 2.
]

{ #category : 'tests-issue 41' }
RwRowanProjectIssuesTest >> testIssue41_moveUnchangedInitializeExtensionMethodToPackage [

	"https://github.com/dalehenrich/Rowan/issues/41"

	"initialize method not changed and moved to new package"

	| projectName packageName1 packageName2 packageName3 className theClass initializeRun |
	projectName := 'Issue41_project'.
	packageName1 := 'Issue41-Core'.
	packageName2 := 'Issue41-Extensions1'.
	packageName3 := 'Issue41-Extensions2'.
	className := 'Issue41Class'.

	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject_44/' validate: false.
	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName2 toProjectNamed: projectName validate: false.
	self _addPackageNamed: packageName3 toProjectNamed: projectName validate: false.
	theClass := self _updateClassNamed: className toPackageNamed: packageName1 inProjectNamed: projectName validate: false.

	self _addOrUpdateMethod: 'cvar1 ^CVar1' forBehavior: theClass class inPackage: packageName1 inProjectNamed: projectName validate: false.
	self _addOrUpdateMethod: 'cvar2 ^CVar2' forBehavior: theClass class inPackage: packageName1 inProjectNamed: projectName validate: false.
	self _addOrUpdateMethod: 'cvar1: anInt CVar1 := anInt' forBehavior: theClass class inPackage: packageName1 inProjectNamed: projectName validate: false.
	self _addOrUpdateMethod: 'cvar2: anInt CVar2 := anInt' forBehavior: theClass class inPackage: packageName1 inProjectNamed: projectName validate: false.

	initializeRun := false.
	[ self _addOrUpdateMethod: 'initialize CVar1 := 1. CVar2 := 2' forBehavior: theClass class inPackage: packageName1 inProjectNamed: projectName validate: false ]
			on: RwExecuteClassInitializeMethodsAfterLoadNotification
			do: [:ex | 
				initializeRun := true.
				ex resume: true ].

	self assert: initializeRun.
	self assert: theClass cvar1 = 1.
	self assert: theClass cvar2 = 2.

	theClass 
		cvar1: 3;
		cvar2: 4.
	self assert: theClass cvar1 = 3.
	self assert: theClass cvar2 = 4.

	initializeRun := false.
	[	"initialize method not changed and moved to new package"
		self _addOrUpdateMethod: 'initialize CVar1 := 1. CVar2 := 2' forBehavior: theClass class inPackage: packageName2 inProjectNamed: projectName validate: false ]
			on: RwExecuteClassInitializeMethodsAfterLoadNotification
			do: [:ex | 
				initializeRun := true.
				ex resume: true ].

	self deny: initializeRun.
	self assert: theClass cvar1 = 3.
	self assert: theClass cvar2 = 4.

	theClass initialize.

	self assert: theClass cvar1 = 1.
	self assert: theClass cvar2 = 2.
]

{ #category : 'tests-issue 467' }
RwRowanProjectIssuesTest >> testIssue467_new_version_class_with_subclasses_1 [

	"https://github.com/dalehenrich/Rowan/issues/467"

	"
	1. move shared/common inst var to superclass. [pass]
	2. move shared/common inst var to superclass. One subclass in different symbol dictionary. [pass]
	3. scenario 2 with extra subclasses in hierarchy [reproduce bug]
	4. senario 3 with a few more extra subclasses (passing)
	5. senario 4 moving a subclass to another package (in same symbol dictionary)
	6. senario 4 moving a subclass to another package (in different symbol dictionary)
	"

	| projectName  packageName1 packageName2 projectDefinition classDefinition1 classDefinition2 classDefinition3 packageDefinition className1 className2 className3 
		projectSetDefinition class1 class2 class3 oldClass1 oldClass2 oldClass3 audit |

	projectName := 'Issue467'.
	packageName1 := 'Issue467-Core1'.
	packageName2 := 'Issue467-Core2'.
	className1 := 'Issue461Class1'.
	className2 := 'Issue467Class2'.
	className3 := 'Issue467Class3'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	packageDefinition := projectDefinition packageNamed: packageName1.

	classDefinition1 := (RwClassDefinition
		newForClassNamed: className1
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: classDefinition1.

	classDefinition2 := (RwClassDefinition
		newForClassNamed: className2
			super: className1
			instvars: #(ivar1)
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: classDefinition2.

	packageDefinition := projectDefinition packageNamed: packageName2.

	classDefinition3 := (RwClassDefinition
		newForClassNamed: className3
			super: className1
			instvars: #(ivar1)
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: classDefinition3.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class1 := Rowan globalNamed: className1.
	class2 := Rowan globalNamed: className2.
	self assert: class2 instVarNames = #(ivar1).
	self assert: class2 superclass == class1.
	class3 := Rowan globalNamed: className3.
	self assert: class3 instVarNames = #(ivar1).
	self assert: class3 superclass == class1.

"modify class -- new version"
	classDefinition1 instVarNames: #(ivar1).
	classDefinition2 instVarNames: #().
	classDefinition3 instVarNames: #().

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	oldClass1 := class1.
	oldClass2 := class2.
	oldClass3 := class3.
	class1 := Rowan globalNamed: className1.
	class2 := Rowan globalNamed: className2.
	class3 := Rowan globalNamed: className3.

	self assert: class1 ~~ oldClass1.
	self assert: class1 instVarNames = #(ivar1).

	self assert: class2 ~~ oldClass2.
	self assert: class2 instVarNames = #().
	self assert: class2 superclass == class1.

	self assert: class3 ~~ oldClass3.
	self assert: class3 instVarNames = #().
	self assert: class3 superclass == class1.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
]

{ #category : 'tests-issue 467' }
RwRowanProjectIssuesTest >> testIssue467_new_version_class_with_subclasses_2 [

	"https://github.com/dalehenrich/Rowan/issues/467"

	"
	1. move shared/common inst var to superclass. [pass]
	2. move shared/common inst var to superclass. One subclass in different symbol dictionary. [pass]
	3. scenario 2 with extra subclasses in hierarchy [reproduce bug]
	4. senario 3 with a few more extra subclasses (passing)
	5. senario 4 moving a subclass to another package (in same symbol dictionary)
	6. senario 4 moving a subclass to another package (in different symbol dictionary)
	"

	| projectName  packageName1 packageName2 projectDefinition classDefinition1 classDefinition2 classDefinition3 packageDefinition className1 className2 className3 
		projectSetDefinition class1 class2 class3 oldClass1 oldClass2 oldClass3 audit |

	projectName := 'Issue467'.
	packageName1 := 'Issue467-Core1'.
	packageName2 := 'Issue467-Core2'.
	className1 := 'Issue461Class1'.
	className2 := 'Issue467Class2'.
	className3 := 'Issue467Class3'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		defaultSymbolDictName: self _symbolDictionaryName1;
		setSymbolDictName: self _symbolDictionaryName2 forPackageNamed: packageName2;
		yourself.

	packageDefinition := projectDefinition packageNamed: packageName1.

	classDefinition1 := (RwClassDefinition
		newForClassNamed: className1
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: classDefinition1.

	classDefinition2 := (RwClassDefinition
		newForClassNamed: className2
			super: className1
			instvars: #(ivar1)
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: classDefinition2.

	packageDefinition := projectDefinition packageNamed: packageName2.

	classDefinition3 := (RwClassDefinition
		newForClassNamed: className3
			super: className1
			instvars: #(ivar1)
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: classDefinition3.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class1 := Rowan globalNamed: className1.
	class2 := Rowan globalNamed: className2.
	self assert: class2 instVarNames = #(ivar1).
	self assert: class2 superclass == class1.
	class3 := Rowan globalNamed: className3.
	self assert: class3 instVarNames = #(ivar1).
	self assert: class3 superclass == class1.

"modify class -- new version"
	classDefinition1 instVarNames: #(ivar1).
	classDefinition2 instVarNames: #().
	classDefinition3 instVarNames: #().

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	oldClass1 := class1.
	oldClass2 := class2.
	oldClass3 := class3.
	class1 := Rowan globalNamed: className1.
	class2 := Rowan globalNamed: className2.
	class3 := Rowan globalNamed: className3.

	self assert: class1 ~~ oldClass1.
	self assert: class1 instVarNames = #(ivar1).

	self assert: class2 ~~ oldClass2.
	self assert: class2 instVarNames = #().
	self assert: class2 superclass == class1.

	self assert: class3 ~~ oldClass3.
	self assert: class3 instVarNames = #().
	self assert: class3 superclass == class1.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
]

{ #category : 'tests-issue 467' }
RwRowanProjectIssuesTest >> testIssue467_new_version_class_with_subclasses_3 [

	"https://github.com/dalehenrich/Rowan/issues/467"

	"
	1. move shared/common inst var to superclass. [pass]
	2. move shared/common inst var to superclass. One subclass in different symbol dictionary. [pass]
	3. scenario 2 with extra subclasses in hierarchy [reproduce bug]
	4. senario 3 with a few more extra subclasses (passing)
	5. senario 4 moving a subclass to another package (in same symbol dictionary)
	6. senario 4 moving a subclass to another package (in different symbol dictionary)
	"

	| projectName  packageName1 packageName2 projectDefinition classDefinition1 classDefinition2 classDefinition3 packageDefinition className1 className2 className3 
		projectSetDefinition class1 class2 class3 oldClass1 oldClass2 oldClass3 extraClassDefinition audit |

	projectName := 'Issue467'.
	packageName1 := 'Issue467-Core1'.
	packageName2 := 'Issue467-Core2'.
	className1 := 'Issue461Class1'.
	className2 := 'Issue467Class2'.
	className3 := 'Issue467Class3'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		defaultSymbolDictName: self _symbolDictionaryName1;
		setSymbolDictName: self _symbolDictionaryName2 forPackageNamed: packageName2;
		yourself.

	packageDefinition := projectDefinition packageNamed: packageName1.

	classDefinition1 := (RwClassDefinition
		newForClassNamed: className1
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: classDefinition1.

	classDefinition2 := (RwClassDefinition
		newForClassNamed: className2
			super: className1
			instvars: #(ivar1)
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: classDefinition2.

	extraClassDefinition := (RwClassDefinition
		newForClassNamed: className2, '_extra'
			super: className2
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: extraClassDefinition.

	packageDefinition := projectDefinition packageNamed: packageName2.

	classDefinition3 := (RwClassDefinition
		newForClassNamed: className3
			super: className1
			instvars: #(ivar1)
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: classDefinition3.

	extraClassDefinition := (RwClassDefinition
		newForClassNamed: className3, '_extra'
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: extraClassDefinition.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class1 := Rowan globalNamed: className1.
	class2 := Rowan globalNamed: className2.
	self assert: class2 instVarNames = #(ivar1).
	self assert: class2 superclass == class1.
	class3 := Rowan globalNamed: className3.
	self assert: class3 instVarNames = #(ivar1).
	self assert: class3 superclass == class1.

"modify class -- new version"
	classDefinition1 instVarNames: #(ivar1).
	classDefinition2 instVarNames: #().
	classDefinition3 instVarNames: #().

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	oldClass1 := class1.
	oldClass2 := class2.
	oldClass3 := class3.
	class1 := Rowan globalNamed: className1.
	class2 := Rowan globalNamed: className2.
	class3 := Rowan globalNamed: className3.

	self assert: class1 ~~ oldClass1.
	self assert: class1 instVarNames = #(ivar1).

	self assert: class2 ~~ oldClass2.
	self assert: class2 instVarNames = #().
	self assert: class2 superclass == class1.

	self assert: class3 ~~ oldClass3.
	self assert: class3 instVarNames = #().
	self assert: class3 superclass == class1.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
]

{ #category : 'tests-issue 467' }
RwRowanProjectIssuesTest >> testIssue467_new_version_class_with_subclasses_4 [

	"https://github.com/dalehenrich/Rowan/issues/467"

	"
	1. move shared/common inst var to superclass. [pass]
	2. move shared/common inst var to superclass. One subclass in different symbol dictionary. [pass]
	3. scenario 2 with extra subclasses in hierarchy [reproduce bug]
	4. senario 3 with a few more extra subclasses (passing)
	5. senario 4 moving a subclass to another package (in same symbol dictionary)
	6. senario 4 moving a subclass to another package (in different symbol dictionary)
	"

	| projectName  packageName1 packageName2 projectDefinition classDefinition1 classDefinition2 classDefinition3 packageDefinition className1 className2 className3 
		projectSetDefinition class1 class2 class3 oldClass1 oldClass2 oldClass3 extraClassDefinition audit |

	projectName := 'Issue467'.
	packageName1 := 'Issue467-Core1'.
	packageName2 := 'Issue467-Core2'.
	className1 := 'Issue461Class1'.
	className2 := 'Issue467Class2'.
	className3 := 'Issue467Class3'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		defaultSymbolDictName: self _symbolDictionaryName1;
		setSymbolDictName: self _symbolDictionaryName2 forPackageNamed: packageName2;
		yourself.

	packageDefinition := projectDefinition packageNamed: packageName1.

	classDefinition1 := (RwClassDefinition
		newForClassNamed: className1
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: classDefinition1.

	classDefinition2 := (RwClassDefinition
		newForClassNamed: className2
			super: className1
			instvars: #(ivar1)
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: classDefinition2.

	extraClassDefinition := (RwClassDefinition
		newForClassNamed: className2, '_extra_1'
			super: className2
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: extraClassDefinition.
	extraClassDefinition := (RwClassDefinition
		newForClassNamed: className2, '_extra_2'
			super: className2
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: extraClassDefinition.
	extraClassDefinition := (RwClassDefinition
		newForClassNamed: className2, '_extra_3'
			super: className2, '_extra_2'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: extraClassDefinition.

	packageDefinition := projectDefinition packageNamed: packageName2.

	classDefinition3 := (RwClassDefinition
		newForClassNamed: className3
			super: className1
			instvars: #(ivar1)
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: classDefinition3.

	extraClassDefinition := (RwClassDefinition
		newForClassNamed: className3, '_extra_1'
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: extraClassDefinition.
	extraClassDefinition := (RwClassDefinition
		newForClassNamed: className3, '_extra_2'
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: extraClassDefinition.
	extraClassDefinition := (RwClassDefinition
		newForClassNamed: className3, '_extra_3'
			super:  className3, '_extra_2'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: extraClassDefinition.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class1 := Rowan globalNamed: className1.
	class2 := Rowan globalNamed: className2.
	self assert: class2 instVarNames = #(ivar1).
	self assert: class2 superclass == class1.
	class3 := Rowan globalNamed: className3.
	self assert: class3 instVarNames = #(ivar1).
	self assert: class3 superclass == class1.

"modify class -- new version"
	classDefinition1 instVarNames: #(ivar1).
	classDefinition2 instVarNames: #().
	classDefinition3 instVarNames: #().

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	oldClass1 := class1.
	oldClass2 := class2.
	oldClass3 := class3.
	class1 := Rowan globalNamed: className1.
	class2 := Rowan globalNamed: className2.
	class3 := Rowan globalNamed: className3.

	self assert: class1 ~~ oldClass1.
	self assert: class1 instVarNames = #(ivar1).

	self assert: class2 ~~ oldClass2.
	self assert: class2 instVarNames = #().
	self assert: class2 superclass == class1.

	self assert: class3 ~~ oldClass3.
	self assert: class3 instVarNames = #().
	self assert: class3 superclass == class1.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
]

{ #category : 'tests-issue 467' }
RwRowanProjectIssuesTest >> testIssue467_new_version_class_with_subclasses_5 [

	"https://github.com/dalehenrich/Rowan/issues/467"

	"
	1. move shared/common inst var to superclass. [pass]
	2. move shared/common inst var to superclass. One subclass in different symbol dictionary. [pass]
	3. scenario 2 with extra subclasses in hierarchy [reproduce bug]
	4. senario 3 with a few more extra subclasses (passing)
	5. senario 4 moving a subclass to another package (in same symbol dictionary)
	6. senario 4 moving a subclass to another package (in different symbol dictionary)
	"

	| projectName  packageName1 packageName2 projectDefinition classDefinition1 classDefinition2 classDefinition3 packageDefinition className1 className2 className3 
		projectSetDefinition class1 class2 class3 oldClass1 oldClass2 oldClass3 extraClassDefinition packageName3 audit |

	projectName := 'Issue467'.
	packageName1 := 'Issue467-Core1'.
	packageName2 := 'Issue467-Core2'.
	packageName3 := 'Issue467-Core3'.
	className1 := 'Issue461Class1'.
	className2 := 'Issue467Class2'.
	className3 := 'Issue467Class3'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		addPackageNamed: packageName3;
		defaultSymbolDictName: self _symbolDictionaryName1;
		setSymbolDictName: self _symbolDictionaryName2 forPackageNamed: packageName2;
		setSymbolDictName: self _symbolDictionaryName2 forPackageNamed: packageName3;
		yourself.

	packageDefinition := projectDefinition packageNamed: packageName1.

	classDefinition1 := (RwClassDefinition
		newForClassNamed: className1
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: classDefinition1.

	classDefinition2 := (RwClassDefinition
		newForClassNamed: className2
			super: className1
			instvars: #(ivar1)
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: classDefinition2.

	extraClassDefinition := (RwClassDefinition
		newForClassNamed: className2, '_extra_1'
			super: className2
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: extraClassDefinition.
	extraClassDefinition := (RwClassDefinition
		newForClassNamed: className2, '_extra_2'
			super: className2
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: extraClassDefinition.
	extraClassDefinition := (RwClassDefinition
		newForClassNamed: className2, '_extra_3'
			super: className2, '_extra_2'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: extraClassDefinition.

	packageDefinition := projectDefinition packageNamed: packageName2.

	classDefinition3 := (RwClassDefinition
		newForClassNamed: className3
			super: className1
			instvars: #(ivar1)
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: classDefinition3.

	extraClassDefinition := (RwClassDefinition
		newForClassNamed: className3, '_extra_1'
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: extraClassDefinition.
	extraClassDefinition := (RwClassDefinition
		newForClassNamed: className3, '_extra_2'
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: extraClassDefinition.

	packageDefinition := projectDefinition packageNamed: packageName3.

	extraClassDefinition := (RwClassDefinition
		newForClassNamed: className3, '_extra_3'
			super:  className3, '_extra_2'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName3
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: extraClassDefinition.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class1 := Rowan globalNamed: className1.
	class2 := Rowan globalNamed: className2.
	self assert: class2 instVarNames = #(ivar1).
	self assert: class2 superclass == class1.
	class3 := Rowan globalNamed: className3.
	self assert: class3 instVarNames = #(ivar1).
	self assert: class3 superclass == class1.

"modify class -- new version"
	classDefinition1 instVarNames: #(ivar1).
	classDefinition2 instVarNames: #().
	classDefinition3 instVarNames: #().

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	oldClass1 := class1.
	oldClass2 := class2.
	oldClass3 := class3.
	class1 := Rowan globalNamed: className1.
	class2 := Rowan globalNamed: className2.
	class3 := Rowan globalNamed: className3.

	self assert: class1 ~~ oldClass1.
	self assert: class1 instVarNames = #(ivar1).

	self assert: class2 ~~ oldClass2.
	self assert: class2 instVarNames = #().
	self assert: class2 superclass == class1.

	self assert: class3 ~~ oldClass3.
	self assert: class3 instVarNames = #().
	self assert: class3 superclass == class1.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
]

{ #category : 'tests-issue 467' }
RwRowanProjectIssuesTest >> testIssue467_new_version_class_with_subclasses_6 [

	"https://github.com/dalehenrich/Rowan/issues/467"

	"
	1. move shared/common inst var to superclass. [pass]
	2. move shared/common inst var to superclass. One subclass in different symbol dictionary. [pass]
	3. scenario 2 with extra subclasses in hierarchy [reproduce bug]
	4. senario 3 with a few more extra subclasses (passing)
	5. senario 4 moving a subclass to another package (in same symbol dictionary)
	6. senario 4 moving a subclass to another package (in different symbol dictionary)
	"

	| projectName  packageName1 packageName2 projectDefinition classDefinition1 classDefinition2 classDefinition3 packageDefinition className1 className2 className3 
		projectSetDefinition class1 class2 class3 oldClass1 oldClass2 oldClass3 extraClassDefinition packageName3 audit |

	projectName := 'Issue467'.
	packageName1 := 'Issue467-Core1'.
	packageName2 := 'Issue467-Core2'.
	packageName3 := 'Issue467-Core3'.
	className1 := 'Issue461Class1'.
	className2 := 'Issue467Class2'.
	className3 := 'Issue467Class3'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		addPackageNamed: packageName3;
		defaultSymbolDictName: self _symbolDictionaryName1;
		setSymbolDictName: self _symbolDictionaryName2 forPackageNamed: packageName2;
		setSymbolDictName: self _symbolDictionaryName forPackageNamed: packageName3;
		yourself.

	packageDefinition := projectDefinition packageNamed: packageName1.

	classDefinition1 := (RwClassDefinition
		newForClassNamed: className1
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: classDefinition1.

	classDefinition2 := (RwClassDefinition
		newForClassNamed: className2
			super: className1
			instvars: #(ivar1)
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: classDefinition2.

	extraClassDefinition := (RwClassDefinition
		newForClassNamed: className2, '_extra_1'
			super: className2
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: extraClassDefinition.
	extraClassDefinition := (RwClassDefinition
		newForClassNamed: className2, '_extra_2'
			super: className2
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: extraClassDefinition.
	extraClassDefinition := (RwClassDefinition
		newForClassNamed: className2, '_extra_3'
			super: className2, '_extra_2'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: extraClassDefinition.

	packageDefinition := projectDefinition packageNamed: packageName2.

	classDefinition3 := (RwClassDefinition
		newForClassNamed: className3
			super: className1
			instvars: #(ivar1)
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: classDefinition3.

	extraClassDefinition := (RwClassDefinition
		newForClassNamed: className3, '_extra_1'
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: extraClassDefinition.
	extraClassDefinition := (RwClassDefinition
		newForClassNamed: className3, '_extra_2'
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: extraClassDefinition.

	packageDefinition := projectDefinition packageNamed: packageName3.

	extraClassDefinition := (RwClassDefinition
		newForClassNamed: className3, '_extra_3'
			super:  className3, '_extra_2'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName3
			comment: 'comment'
			pools: #()
			type: 'normal').
	packageDefinition 
		addClassDefinition: extraClassDefinition.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class1 := Rowan globalNamed: className1.
	class2 := Rowan globalNamed: className2.
	self assert: class2 instVarNames = #(ivar1).
	self assert: class2 superclass == class1.
	class3 := Rowan globalNamed: className3.
	self assert: class3 instVarNames = #(ivar1).
	self assert: class3 superclass == class1.

"modify class -- new version"
	classDefinition1 instVarNames: #(ivar1).
	classDefinition2 instVarNames: #().
	classDefinition3 instVarNames: #().

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	oldClass1 := class1.
	oldClass2 := class2.
	oldClass3 := class3.
	class1 := Rowan globalNamed: className1.
	class2 := Rowan globalNamed: className2.
	class3 := Rowan globalNamed: className3.

	self assert: class1 ~~ oldClass1.
	self assert: class1 instVarNames = #(ivar1).

	self assert: class2 ~~ oldClass2.
	self assert: class2 instVarNames = #().
	self assert: class2 superclass == class1.

	self assert: class3 ~~ oldClass3.
	self assert: class3 instVarNames = #().
	self assert: class3 superclass == class1.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
]

{ #category : 'tests-issue 498' }
RwRowanProjectIssuesTest >> testIssue498_constraint_ordering_1 [

	"https://github.com/dalehenrich/Rowan/issues/498 -- expanded validation over https://github.com/dalehenrich/Rowan/issues/293"

	"constraints should be displayed in inst var order, not alphabetical order"

	"non-rowan variant of test ... _2 uses Rowan api"

	| className1 className2 className3 class constraintBlock1 constraintBlock2 constraintBlock3 x
		class1 class2 class3 |
	className1 := 'Issue293Class1'.
	className2 := 'Issue293Subclass2'.
	className3 := 'Issue293Subclass3'.

	{className1 . className2. className3 }
		do: [ :className | 
			UserGlobals removeKey: className1 asSymbol ifAbsent: [].
			UserGlobals removeKey: className2 asSymbol  ifAbsent: [].
			UserGlobals removeKey: className3 asSymbol  ifAbsent: [] ].

	[ 
	class1 := Object
		subclass: className1
		instVarNames: #( ivar2 ivar3 ivar4 ivar1)
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: UserGlobals
		constraints: { { 'ivar4' . Association }. { 'ivar3' . Association }. { 'ivar2' . Association}. { 'ivar1' . Association }. }
		options: {}.

	class2 := class1
		subclass: className2
		instVarNames: #( ivar7 ivar6)
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: UserGlobals
		constraints: { { 'ivar7' . Association }. { 'ivar6' . Association }. }
		options: {}.

	class3 := class2
		subclass: className3
		instVarNames: #( ivar8 ivar9)
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: UserGlobals
		constraints: { { 'ivar9' . Association }. { 'ivar8' . Association }. }
		options: {} ] 
			on: Deprecated 
			do: [:ex | 
				"ignore any Deprecation errors ... we're using the deprecated class creation on purpose"
				ex resume ].

"validate"
	class := Rowan globalNamed: className1.
	self assert: (class _constraintsEqual: {  { #'ivar1' . Association }.  { #'ivar2' . Association }. { #'ivar3' . Association }. { #'ivar4' . Association }.  }).

	class := Rowan globalNamed: className2.
	self assert: (class _constraintsEqual: {  { #'ivar7' . Association }.  { #'ivar6' . Association }. }).

	class := Rowan globalNamed: className3.
	self assert: (class _constraintsEqual: {  { #'ivar8' . Association }.  { #'ivar9' . Association }. }).

	constraintBlock1 := [:theClass |
		self assert: (x := theClass _constraintOn: #ivar1) == Association.
		self assert: (x := theClass _constraintOn: #ivar2) == Association.
		self assert: (x := theClass _constraintOn: #ivar3) == Association.
		self assert: (x := theClass _constraintOn: #ivar4) == Association ].
	constraintBlock2 := [:theClass |
		self assert: (x := theClass _constraintOn: #ivar6) == Association.
		self assert: (x := theClass _constraintOn: #ivar7) == Association ].
	constraintBlock3 := [:theClass |
		self assert: (x := theClass _constraintOn: #ivar8) == Association.
		self assert: (x := theClass _constraintOn: #ivar9) == Association ].

	constraintBlock1 value: (Rowan globalNamed: className1).

	constraintBlock1 value: (Rowan globalNamed: className2).
	constraintBlock2 value: (Rowan globalNamed: className2).

	constraintBlock1 value: (Rowan globalNamed: className3).
	constraintBlock2 value: (Rowan globalNamed: className3).
	constraintBlock3 value: (Rowan globalNamed: className3).
]

{ #category : 'tests-issue 498' }
RwRowanProjectIssuesTest >> testIssue498_constraint_ordering_2 [

	"https://github.com/dalehenrich/Rowan/issues/498 -- expanded validation over https://github.com/dalehenrich/Rowan/issues/293"

	"constraints should be displayed in inst var order, not alphabetical order"

	"rowan variant of test ... _1 uses non-Rowan api"

	| projectName  packageName projectDefinition classDefinition packageDefinition className1 className2 
		className3 projectSetDefinition class constraintBlock1 constraintBlock2 constraintBlock3 x |
	projectName := 'Issue255'.
	packageName := 'Issue255-Core'.
	className1 := 'Issue255Class1'.
	className2 := 'Issue255Subclass2'.
	className3 := 'Issue255Subclass3'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].
"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	packageDefinition := projectDefinition packageNamed: packageName.

	classDefinition := RwClassDefinition
		newForClassNamed: className1
		super: 'Object'
		instvars: #( ivar2 ivar3 ivar4 ivar1)
		classinstvars: #()
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition gs_constraints: { { 'ivar4' . 'Association' }. { 'ivar3' . 'Association' }. { 'ivar2' . 'Association' }. { 'ivar1' . 'Association' }. }.
	packageDefinition 
		addClassDefinition: classDefinition.

	classDefinition := RwClassDefinition
		newForClassNamed: className2
		super: className1
		instvars: #( ivar7 ivar6)
		classinstvars: #()
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition gs_constraints: { { 'ivar7' . 'Association' }. { 'ivar6' . 'Association' }. }.
	packageDefinition 
		addClassDefinition: classDefinition.

	classDefinition := RwClassDefinition
		newForClassNamed: className3
		super: className2
		instvars: #( ivar8 ivar9)
		classinstvars: #()
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition gs_constraints: { { 'ivar9' . 'Association' }. { 'ivar8' . 'Association' }. }.
	packageDefinition 
		addClassDefinition: classDefinition.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class := Rowan globalNamed: className1.
	self assert: (class _constraintsEqual: {  { #'ivar1' . Association }.  { #'ivar2' . Association }. { #'ivar3' . Association }. { #'ivar4' . Association }.  }).

	class := Rowan globalNamed: className2.
	self assert: (class _constraintsEqual: {  { #'ivar7' . Association }.  { #'ivar6' . Association }. }).

	class := Rowan globalNamed: className3.
	self assert: (class _constraintsEqual: {  { #'ivar8' . Association }.  { #'ivar9' . Association }. }).

	constraintBlock1 := [:theClass |
		self assert: (x := theClass _constraintOn: #ivar1) == Association.
		self assert: (x := theClass _constraintOn: #ivar2) == Association.
		self assert: (x := theClass _constraintOn: #ivar3) == Association.
		self assert: (x := theClass _constraintOn: #ivar4) == Association ].
	constraintBlock2 := [:theClass |
		self assert: (x := theClass _constraintOn: #ivar6) == Association.
		self assert: (x := theClass _constraintOn: #ivar7) == Association ].
	constraintBlock3 := [:theClass |
		self assert: (x := theClass _constraintOn: #ivar8) == Association.
		self assert: (x := theClass _constraintOn: #ivar9) == Association ].

	constraintBlock1 value: (Rowan globalNamed: className1).

	constraintBlock1 value: (Rowan globalNamed: className2).
	constraintBlock2 value: (Rowan globalNamed: className2).

	constraintBlock1 value: (Rowan globalNamed: className3).
	constraintBlock2 value: (Rowan globalNamed: className3).
	constraintBlock3 value: (Rowan globalNamed: className3).
]

{ #category : 'tests-issue 498' }
RwRowanProjectIssuesTest >> testIssue498_constraint_ordering_3 [

	"https://github.com/dalehenrich/Rowan/issues/498 -- expanded validation over https://github.com/dalehenrich/Rowan/issues/293"

	"constraints should be displayed in inst var order, not alphabetical order"

	"rowan variant of test ... _1 uses non-Rowan api"

	| projectName  packageName projectDefinition classDefinition packageDefinition className1 className2 
		className3 projectSetDefinition class constraintBlock1 constraintBlock2 constraintBlock3 x |
	projectName := 'Issue255'.
	packageName := 'Issue255-Core'.
	className1 := 'Issue255Class1'.
	className2 := 'Issue255Subclass2'.
	className3 := 'Issue255Subclass3'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].
"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	packageDefinition := projectDefinition packageNamed: packageName.

	classDefinition := RwClassDefinition
		newForClassNamed: className1
		super: 'Object'
		instvars: #( ivar2 ivar3 ivar4 ivar1)
		classinstvars: #()
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition gs_constraints: { { 'ivar4' . 'Association' }. { 'ivar3' . 'Association' }. { 'ivar2' . 'Association' }. { 'ivar1' . 'Association' }. }.
	packageDefinition 
		addClassDefinition: classDefinition.

	classDefinition := RwClassDefinition
		newForClassNamed: className2
		super: className1
		instvars: #( ivar7 ivar6)
		classinstvars: #()
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition gs_constraints: { { 'ivar7' . 'Association' }. }.
	packageDefinition 
		addClassDefinition: classDefinition.

	classDefinition := RwClassDefinition
		newForClassNamed: className3
		super: className2
		instvars: #( ivar8 ivar9)
		classinstvars: #()
		classvars: #()
		category: nil
		comment: ''
		pools: #()
		type: 'normal'.
	classDefinition gs_constraints: { { 'ivar9' . 'Association' }. { 'ivar8' . 'Association' }. }.
	packageDefinition 
		addClassDefinition: classDefinition.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class := Rowan globalNamed: className1.
	self assert: (class _constraintsEqual: {  { #'ivar1' . Association }.  { #'ivar2' . Association }. { #'ivar3' . Association }. { #'ivar4' . Association }.  }).

	class := Rowan globalNamed: className2.
	self assert: (class _constraintsEqual: {  { #'ivar7' . Association } }).

	class := Rowan globalNamed: className3.
	self assert: (class _constraintsEqual: {  { #'ivar8' . Association }.  { #'ivar9' . Association }. }).

"add a constraint to a class in middle of hierchy"
	((projectDefinition packageNamed: packageName) classDefinitions at: className2)
		gs_constraints: { { 'ivar7' . 'Association' }. { 'ivar6' . 'Association' }. }.

"load with new constraint"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	class := Rowan globalNamed: className1.
	self assert: (class _constraintsEqual: {  { #'ivar1' . Association }.  { #'ivar2' . Association }. { #'ivar3' . Association }. { #'ivar4' . Association }.  }).

	class := Rowan globalNamed: className2.
	self assert: (class _constraintsEqual: {  { #'ivar7' . Association }.  { #'ivar6' . Association }. }).

	class := Rowan globalNamed: className3.
	self assert: (class _constraintsEqual: {  { #'ivar8' . Association }.  { #'ivar9' . Association }. }).

	constraintBlock1 := [:theClass |
		self assert: (x := theClass _constraintOn: #ivar1) == Association.
		self assert: (x := theClass _constraintOn: #ivar2) == Association.
		self assert: (x := theClass _constraintOn: #ivar3) == Association.
		self assert: (x := theClass _constraintOn: #ivar4) == Association ].
	constraintBlock2 := [:theClass |
		self assert: (x := theClass _constraintOn: #ivar6) == Association.
		self assert: (x := theClass _constraintOn: #ivar7) == Association ].
	constraintBlock3 := [:theClass |
		self assert: (x := theClass _constraintOn: #ivar8) == Association.
		self assert: (x := theClass _constraintOn: #ivar9) == Association ].

	constraintBlock1 value: (Rowan globalNamed: className1).

	constraintBlock1 value: (Rowan globalNamed: className2).
	constraintBlock2 value: (Rowan globalNamed: className2).

	constraintBlock1 value: (Rowan globalNamed: className3).
	constraintBlock2 value: (Rowan globalNamed: className3).
	constraintBlock3 value: (Rowan globalNamed: className3).
]

{ #category : 'tests-issue 72' }
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

{ #category : 'tests-issue 72' }
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

{ #category : 'tests-issue 72' }
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

{ #category : 'tests-issue 72' }
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

{ #category : 'tests-issue 72' }
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
			(Rowan image loadedProjectNamed: name ifAbsent: [  ])
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
		constraints: #()
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
