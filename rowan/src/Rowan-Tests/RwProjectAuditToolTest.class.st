Class {
	#name : 'RwProjectAuditToolTest',
	#superclass : 'RwToolTest',
	#category : 'Rowan-Tests'
}

{ #category : 'private-issue 24' }
RwProjectAuditToolTest >> _addClassExtensionNamed: className toPackageNamed: packageName inProjectNamed: projectName [

	^ self _addClassExtensionNamed: className toPackageNamed: packageName inProjectNamed: projectName validate: true

]

{ #category : 'private-issue 24' }
RwProjectAuditToolTest >> _addClassExtensionNamed: className toPackageNamed: packageName inProjectNamed: projectName validate: validate [

	"since we create class extension no warries about package"
 | theClass service |

	theClass := (System myUserProfile resolveSymbol: className asSymbol) ifNotNil: [:anAssoc | anAssoc value].
	theClass ifNil: [self error: 'Class named ', className , ' does not exist'].
	service := RowanClassService forClassNamed: className package: packageName meta: false.
	service addCategory:(self _categoryFromPackageName: packageName)

]

{ #category : 'private-issue 24' }
RwProjectAuditToolTest >> _addClassNamed: className toPackageNamed: packageName inProjectNamed: projectName [

	^ self _addClassNamed: className toPackageNamed: packageName inProjectNamed: projectName validate: true

]

{ #category : 'private-issue 24' }
RwProjectAuditToolTest >> _addClassNamed: className toPackageNamed: packageName inProjectNamed: projectName validate: validate [

	"validate the dirty state of project before and after class is added"

	| project package theClass |

	project := RwProject newNamed: projectName.
	package := RwPackage newNamed: packageName.

	validate ifTrue: [ 
		self deny: project isDirty.
		self deny: package isDirty ].

	theClass := Object
		rwSubclass: className
		instVarNames: #(bar)
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
RwProjectAuditToolTest >> _addOrUpdateMethod: methodSource forBehavior: theBehavior inPackage: packageName inProjectNamed: projectName [

	^ self _addOrUpdateMethod: methodSource forBehavior: theBehavior inPackage: packageName inProjectNamed: projectName validate: true

]

{ #category : 'private-issue 24' }
RwProjectAuditToolTest >> _addOrUpdateMethod: methodSource forBehavior: theBehavior inPackage: packageName inProjectNamed: projectName validate: validate [

	"validate the dirty state of project before and after method is added or updated"

	| project package |

	project := RwProject newNamed: projectName.
	package := RwPackage newNamed: packageName.

	validate ifTrue: [ 
		self deny: project isDirty.
		self deny: package isDirty].

	theBehavior
		rwCompileMethod: methodSource
		category:  (self _categoryFromPackageName: packageName).

	validate ifTrue: [ 
		self assert: project isDirty.
		self assert: package isDirty ].

]

{ #category : 'private-issue 24' }
RwProjectAuditToolTest >> _addPackageNamed: packageName toProjectNamed: projectName [

	^ self _addPackageNamed: packageName toProjectNamed: projectName validate: true

]

{ #category : 'private-issue 24' }
RwProjectAuditToolTest >> _addPackageNamed: packageName toProjectNamed: projectName validate: validate [

	"validate the dirty state of project before and after add package call"

	| project |

	project := RwProject newNamed: projectName.

	validate ifTrue: [ self deny: project isDirty ].

	Rowan projectTools browser addPackageNamed: packageName toProjectNamed: projectName.

	validate ifTrue: [ self assert: project isDirty ].

]

{ #category : 'private-issue 24' }
RwProjectAuditToolTest >> _categoryFromPackageName: aPackageName [

	^'*', aPackageName asLowercase

]

{ #category : 'private-issue 24' }
RwProjectAuditToolTest >> _createLoadedProjectNamed: projectName root: rootPath [

	^ self _createLoadedProjectNamed: projectName root: rootPath validate: true

]

{ #category : 'private-issue 24' }
RwProjectAuditToolTest >> _createLoadedProjectNamed: projectName root: rootPath validate: validate [

	| projectDefinition project |

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
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
RwProjectAuditToolTest >> _removeClassNamed: className fromProjectNamed: projectName [

	^ self _removeClassNamed: className fromProjectNamed: projectName validate: true

]

{ #category : 'private-issue 24' }
RwProjectAuditToolTest >> _removeClassNamed: className fromProjectNamed: projectName validate: validate [

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
RwProjectAuditToolTest >> _removePackageNamed: packageName from: projectName [

	^ self _removePackageNamed: packageName from: projectName validate: true

]

{ #category : 'private-issue 24' }
RwProjectAuditToolTest >> _removePackageNamed: packageName from: projectName validate: validate [

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
RwProjectAuditToolTest >> _removeSelector: selector fromBehavior: theBehavior inProjectNamed: projectName [

	^ self _removeSelector: selector fromBehavior: theBehavior inProjectNamed: projectName validate: true

]

{ #category : 'private-issue 24' }
RwProjectAuditToolTest >> _removeSelector: selector fromBehavior: theBehavior inProjectNamed: projectName validate: validate [

	"validate the dirty state of project before and after method is removed"

	| project |

	project := RwProject newNamed: projectName.

	validate ifTrue: [ self deny: project isDirty ].

	theBehavior rwRemoveSelector: selector.

	validate ifTrue: [ self assert: project isDirty ].

]

{ #category : 'private-issue 24' }
RwProjectAuditToolTest >> _standardProjectDefinition: projectName packageNames: packageNames defaultSymbolDictName: defaultSymbolDictName comment: comment [

	^ self
		_standardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: defaultSymbolDictName
		defaultUseSessionMethodsForExtensions: false
		comment: comment

]

{ #category : 'private-issue 24' }
RwProjectAuditToolTest >> _standardProjectDefinition: projectName packageNames: packageNames defaultSymbolDictName: defaultSymbolDictName defaultUseSessionMethodsForExtensions: defaultUseSessionMethodsForExtensions comment: comment [

	| projectDefinition |
	projectDefinition := RwProjectDefinition
		newForGitPackageProjectNamed: projectName.
	projectDefinition
		comment: comment;
		packageNamesInLoadOrder: packageNames;
		defaultUseSessionMethodsForExtensions: defaultUseSessionMethodsForExtensions;
		yourself.
	projectDefinition defaultSymbolDictName: defaultSymbolDictName.

	^ projectDefinition

]

{ #category : 'private-issue 24' }
RwProjectAuditToolTest >> _updateClassNamed: className toPackageNamed: packageName inProjectNamed: projectName [

	^ self _updateClassNamed: className toPackageNamed: packageName inProjectNamed: projectName validate: true

]

{ #category : 'private-issue 24' }
RwProjectAuditToolTest >> _updateClassNamed: className toPackageNamed: packageName inProjectNamed: projectName validate: validate [

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
RwProjectAuditToolTest >> _writeProjectNamed: projectName [

	^ self _writeProjectNamed: projectName validate: true

]

{ #category : 'private-issue 24' }
RwProjectAuditToolTest >> _writeProjectNamed: projectName validate: validate [

	"validate the dirty state of project before and after write"

	| project |

	project := RwProject newNamed: projectName.

	validate ifTrue: [ self assert: project isDirty ]. "a project is dirty if it has changes that are not written to disk"

	Rowan projectTools write writeProjectNamed: projectName.

	validate ifTrue: [ self deny: project isDirty ]. "a project is not dirty if it has been written to disk"

]

{ #category : 'tests' }
RwProjectAuditToolTest >> testClassExtension [

	| projectName packageName  classExtensionName  theClassExtension x |
	projectName := 'Audit_addUpdateRemoveMethods'.
	packageName := 'AuditTests'.
	classExtensionName := 'String'.
	self _createLoadedProjectNamed: projectName root: '/tmp/rowanIssuesProject/'.
	self _writeProjectNamed: projectName.
	self _addPackageNamed: packageName toProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	theClassExtension := String.
	self _addClassExtensionNamed: classExtensionName toPackageNamed: packageName inProjectNamed: projectName.

	self _addOrUpdateMethod:  'foo ^''foo''' forBehavior: theClassExtension inPackage: packageName inProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	self _addOrUpdateMethod:  'foo ^''foo''' forBehavior: theClassExtension class inPackage: packageName inProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	theClassExtension compileMissingAccessingMethods. "this should add: #bar #bar: ouside of rowan"

	((Rowan image loadedPackageNamed: 'AuditTests') loadedClassForClass: (GsSession currentSession resolveSymbol: classExtensionName asSymbol) value ifAbsent: [nil]) 
			ifNotNil: [nil error: 'ClassExtension should not be present in Rowan'].

	self assert: (x := RwPkgAuditTool new auditForPackageName:  'AuditTests') size = 0.
		


"	self _addOrUpdateMethod:  'foo ^''bar''' forBehavior: theClass inPackage: packageName inProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	self _addOrUpdateMethod:  'foo ^''bar''' forBehavior: theClass class inPackage: packageName inProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	self _removeSelector: #foo fromBehavior: theClass inProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	self _removeSelector: #foo fromBehavior: theClass class inProjectNamed: projectName.

	self _writeProjectNamed: projectName.
"

]

{ #category : 'tests' }
RwProjectAuditToolTest >> testMissingMethods [

	| projectName packageName className  theClass  x |
	projectName := 'Audit_addUpdateRemoveMethods'.
	packageName := 'AuditTests'.
	className := 'ClassForAudit'.
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

	theClass compileMissingAccessingMethods. "this should add: #bar #bar:"


	((Rowan image loadedPackageNamed: 'AuditTests') loadedClassForClass: (GsSession currentSession resolveSymbol: className asSymbol) value ifAbsent: [nil error: 'expected a class']) 
			loadedInstanceMethodsDo: [:anLMethod | (anLMethod name isEquivalent: 'bar') ifTrue: [self error: 'bar was compiled without Rowan']] 
			loadedClassMethodsDo: [:cMethod | "do nothing"].

	self assert: (x := RwPkgAuditTool new auditForPackageName:  'AuditTests') size = 2.
		


"	self _addOrUpdateMethod:  'foo ^''bar''' forBehavior: theClass inPackage: packageName inProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	self _addOrUpdateMethod:  'foo ^''bar''' forBehavior: theClass class inPackage: packageName inProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	self _removeSelector: #foo fromBehavior: theClass inProjectNamed: projectName.

	self _writeProjectNamed: projectName.

	self _removeSelector: #foo fromBehavior: theClass class inProjectNamed: projectName.

	self _writeProjectNamed: projectName.
"

]
