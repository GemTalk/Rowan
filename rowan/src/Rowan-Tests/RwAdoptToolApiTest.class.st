Class {
	#name : 'RwAdoptToolApiTest',
	#superclass : 'RwBrowserToolTest',
	#category : 'Rowan-Tests'
}

{ #category : 'tests' }
RwAdoptToolApiTest >> testAdopt_issue481_A [

	"https://github.com/GemTalk/Rowan/issues/481"

	"missing instance method for packaged class"

	| projectName packageNames className1 className2 packageName1 packageName2
		symDictName projectDefinition notified audit |

	projectName := 'AdoptProject_481'.
	packageName1 := 'Adopt-Core'.
	packageName2 := 'Adopt-Extension'.
	packageNames := {packageName1 .  packageName2}.
	symDictName := self _symbolDictionaryName2.
	className1 := 'AdoptedClass'.
	className2 := 'ExtendedUnpackagedClass'.

"create image artifacts and projectDefinition"
	projectDefinition := self 
		_issue481_projectDefinition: projectName  
			packageName1: packageName1 
			packageName2: packageName2 
			className1: className1 
			className2: className2 
			symDictName: symDictName.

"disown the project"
	Rowan projectTools disown disownProjectNamed: projectDefinition name.

"remove #foo method"
	(Rowan globalNamed: className1) removeSelector: #foo.

"create an empty loaded project"
	self
		_issue481_loadProjectDefinition: projectName  
			packageName1: packageName1 
			packageName2: packageName2 
			symDictName: symDictName.

"adopt and expect an error"
	notified := false.
	[ Rowan projectTools adopt adoptProjectDefinition: projectDefinition ]
		on: Error
		do: [:ex | notified := true ].
	self assert: notified.

"disown the project"
	Rowan projectTools disown disownProjectNamed: projectDefinition name.

"create an empty loaded project"
	self
		_issue481_loadProjectDefinition: projectName  
			packageName1: packageName1 
			packageName2: packageName2 
			symDictName: symDictName.

"adopt and handle the missing method"
	notified := false.
	[ Rowan projectTools adopt adoptProjectDefinition: projectDefinition ]
		on: RwAdoptAuditMethodErrorNotification
		do: [:ex |
			notified := true.
			self assert: ex className = className1.
			self deny: ex isClassExtension.
			self assert: ex selector = #foo.
			self deny: ex isMetaclass.
			self assert: ex packageName = packageName1.
			ex resume ].
	self assert: notified.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
]

{ #category : 'tests' }
RwAdoptToolApiTest >> testAdopt_issue481_A_F [

	"https://github.com/GemTalk/Rowan/issues/481"

	"missing unpackaged class and missing instance method for packaged class"

	"test for RwAdoptAuditErrorNotification>>methodErrorDo:classErrorDo:"

	| projectName packageNames className1 className2 packageName1 packageName2
		symDictName projectDefinition notified audit report |

	projectName := 'AdoptProject_481'.
	packageName1 := 'Adopt-Core'.
	packageName2 := 'Adopt-Extension'.
	packageNames := {packageName1 .  packageName2}.
	symDictName := self _symbolDictionaryName2.
	className1 := 'AdoptedClass'.
	className2 := 'ExtendedUnpackagedClass'.

"create image artifacts and projectDefinition"
	projectDefinition := self 
		_issue481_projectDefinition: projectName  
			packageName1: packageName1 
			packageName2: packageName2 
			className1: className1 
			className2: className2 
			symDictName: symDictName.

"disown the project"
	Rowan projectTools disown disownProjectNamed: projectDefinition name.

"remove #foo method"
	(Rowan globalNamed: className1) removeSelector: #foo.
"remove className2"
	(Rowan globalNamed: symDictName) removeKey: className2 asSymbol.

"create an empty loaded project"
	self
		_issue481_loadProjectDefinition: projectName  
			packageName1: packageName1 
			packageName2: packageName2 
			symDictName: symDictName.

"adopt and expect an error"
	notified := false.
	[ Rowan projectTools adopt adoptProjectDefinition: projectDefinition ]
		on: Error
		do: [:ex | notified := true ].
	self assert: notified.

"disown the project"
	Rowan projectTools disown disownProjectNamed: projectDefinition name.

"create an empty loaded project"
	self
		_issue481_loadProjectDefinition: projectName  
			packageName1: packageName1 
			packageName2: packageName2 
			symDictName: symDictName.

"adopt and handle the missing class and missing method"
	notified := false.
	report := WriteStream on: String new.
	[ Rowan projectTools adopt adoptProjectDefinition: projectDefinition ]
		on: RwAdoptMissingMethodErrorNotification, RwAdoptMissingClassErrorNotification
		do: [:ex |
			notified := true.
			ex 
				methodErrorDo: [
					"RwAdoptAuditMethodErrorNotification"
					self assert: ex className = className1.
					self deny: ex isClassExtension.
					self assert: ex selector = #foo.
					self deny: ex isMetaclass.
					self assert: ex packageName = packageName1.
					report nextPutAll: 'Missing loaded method ', ex methodPrintString, ' encountered during adopt ... IGNORED'; lf ]
				classErrorDo: [
					"RwAdoptMissingClassErrorNotification"
					self assert: ex className = className2.
					self assert: ex isClassExtension.
					self assert: ex packageName = packageName2.
					report nextPutAll: 'Missing loaded class ', ex className, ' encountered during adopt ... IGNORED'; lf  ].
			ex resume ].
	self assert: notified.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
]

{ #category : 'tests' }
RwAdoptToolApiTest >> testAdopt_issue481_B [

	"https://github.com/GemTalk/Rowan/issues/481"

	"missing class method for packaged class"

	| projectName packageNames className1 className2 packageName1 packageName2
		symDictName projectDefinition notified audit |

	projectName := 'AdoptProject_481'.
	packageName1 := 'Adopt-Core'.
	packageName2 := 'Adopt-Extension'.
	packageNames := {packageName1 .  packageName2}.
	symDictName := self _symbolDictionaryName2.
	className1 := 'AdoptedClass'.
	className2 := 'ExtendedUnpackagedClass'.

"create image artifacts and projectDefinition"
	projectDefinition := self 
		_issue481_projectDefinition: projectName  
			packageName1: packageName1 
			packageName2: packageName2 
			className1: className1 
			className2: className2 
			symDictName: symDictName.

"disown the project"
	Rowan projectTools disown disownProjectNamed: projectDefinition name.

"remove #bar class method"
	(Rowan globalNamed: className1) class removeSelector: #bar.

"create an empty loaded project"
	self
		_issue481_loadProjectDefinition: projectName  
			packageName1: packageName1 
			packageName2: packageName2 
			symDictName: symDictName.

"adopt and expect an error"
	notified := false.
	[ Rowan projectTools adopt adoptProjectDefinition: projectDefinition ]
		on: Error
		do: [:ex | notified := true ].
	self assert: notified.

"disown the project"
	Rowan projectTools disown disownProjectNamed: projectDefinition name.

"create an empty loaded project"
	self
		_issue481_loadProjectDefinition: projectName  
			packageName1: packageName1 
			packageName2: packageName2 
			symDictName: symDictName.

"adopt and handle the missing method"
	notified := false.
	[ Rowan projectTools adopt adoptProjectDefinition: projectDefinition ]
		on: RwAdoptMissingMethodErrorNotification
		do: [:ex |
			notified := true.
			self assert: ex className = className1.
			self deny: ex isClassExtension.
			self assert: ex selector = #bar.
			self assert: ex isMetaclass.
			self assert: ex packageName = packageName1.
			ex resume ].
	self assert: notified.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
]

{ #category : 'tests' }
RwAdoptToolApiTest >> testAdopt_issue481_C [

	"https://github.com/GemTalk/Rowan/issues/481"

	"missing instance method extension for unpackaged class"

	| projectName packageNames className1 className2 packageName1 packageName2 
		symDictName projectDefinition notified audit |

	projectName := 'AdoptProject_481'.
	packageName1 := 'Adopt-Core'.
	packageName2 := 'Adopt-Extension'.
	packageNames := {packageName1 .  packageName2}.
	symDictName := self _symbolDictionaryName2.
	className1 := 'AdoptedClass'.
	className2 := 'ExtendedUnpackagedClass'.

"create image artifacts and projectDefinition"
	projectDefinition := self 
		_issue481_projectDefinition: projectName  
			packageName1: packageName1 
			packageName2: packageName2 
			className1: className1 
			className2: className2 
			symDictName: symDictName.

"disown the project"
	Rowan projectTools disown disownProjectNamed: projectDefinition name.

"remove #foo method"
	(Rowan globalNamed: className2) removeSelector: #foo.

"create an empty loaded project"
	self
		_issue481_loadProjectDefinition: projectName  
			packageName1: packageName1 
			packageName2: packageName2 
			symDictName: symDictName.

"adopt and expect an error"
	notified := false.
	[ Rowan projectTools adopt adoptProjectDefinition: projectDefinition ]
		on: Error
		do: [:ex | notified := true ].
	self assert: notified.

"disown the project"
	Rowan projectTools disown disownProjectNamed: projectDefinition name.

"create an empty loaded project"
	self
		_issue481_loadProjectDefinition: projectName  
			packageName1: packageName1 
			packageName2: packageName2 
			symDictName: symDictName.

"adopt and handle the missing method"
	notified := false.
	[ Rowan projectTools adopt adoptProjectDefinition: projectDefinition ]
		on: RwAdoptAuditMethodErrorNotification
		do: [:ex |
			notified := true.
			self assert: ex className = className2.
			self assert: ex isClassExtension.
			self assert: ex selector = #foo.
			self deny: ex isMetaclass.
			self assert: ex packageName = packageName2.
			ex resume ].
	self assert: notified.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
]

{ #category : 'tests' }
RwAdoptToolApiTest >> testAdopt_issue481_D [

	"https://github.com/GemTalk/Rowan/issues/481"

	"missing class method extension for unpackaged class"

	| projectName packageNames className1 className2 packageName1 packageName2
		symDictName projectDefinition notified audit |

	projectName := 'AdoptProject_481'.
	packageName1 := 'Adopt-Core'.
	packageName2 := 'Adopt-Extension'.
	packageNames := {packageName1 .  packageName2}.
	symDictName := self _symbolDictionaryName2.
	className1 := 'AdoptedClass'.
	className2 := 'ExtendedUnpackagedClass'.

"create image artifacts and projectDefinition"
	projectDefinition := self 
		_issue481_projectDefinition: projectName  
			packageName1: packageName1 
			packageName2: packageName2 
			className1: className1 
			className2: className2 
			symDictName: symDictName.

"disown the project"
	Rowan projectTools disown disownProjectNamed: projectDefinition name.

"remove #bar class method"
	(Rowan globalNamed: className2) class removeSelector: #bar.

"create an empty loaded project"
	self
		_issue481_loadProjectDefinition: projectName  
			packageName1: packageName1 
			packageName2: packageName2 
			symDictName: symDictName.

"adopt and expect an error"
	notified := false.
	[ Rowan projectTools adopt adoptProjectDefinition: projectDefinition ]
		on: Error
		do: [:ex | notified := true ].
	self assert: notified.

"disown the project"
	Rowan projectTools disown disownProjectNamed: projectDefinition name.

"create an empty loaded project"
	self
		_issue481_loadProjectDefinition: projectName  
			packageName1: packageName1 
			packageName2: packageName2 
			symDictName: symDictName.

"adopt and handle the missing method"
	notified := false.
	[ Rowan projectTools adopt adoptProjectDefinition: projectDefinition ]
		on: RwAdoptAuditMethodErrorNotification
		do: [:ex |
			notified := true.
			self assert: ex className = className2.
			self assert: ex isClassExtension.
			self assert: ex selector = #bar.
			self assert: ex isMetaclass.
			self assert: ex packageName = packageName2.
			ex resume ].
	self assert: notified.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
]

{ #category : 'tests' }
RwAdoptToolApiTest >> testAdopt_issue481_E [

	"https://github.com/GemTalk/Rowan/issues/481"

	"missing packaged class"

	| projectName packageNames className1 className2 packageName1 packageName2  symDictName projectDefinition notified audit |

	projectName := 'AdoptProject_481'.
	packageName1 := 'Adopt-Core'.
	packageName2 := 'Adopt-Extension'.
	packageNames := {packageName1 .  packageName2}.
	symDictName := self _symbolDictionaryName2.
	className1 := 'AdoptedClass'.
	className2 := 'ExtendedUnpackagedClass'.

"create image artifacts and projectDefinition"
	projectDefinition := self 
		_issue481_projectDefinition: projectName  
			packageName1: packageName1 
			packageName2: packageName2 
			className1: className1 
			className2: className2 
			symDictName: symDictName.

"disown the project"
	Rowan projectTools disown disownProjectNamed: projectDefinition name.

"remove className1"
	(Rowan globalNamed: symDictName) removeKey: className1 asSymbol.

"create an empty loaded project"
	self
		_issue481_loadProjectDefinition: projectName  
			packageName1: packageName1 
			packageName2: packageName2 
			symDictName: symDictName.

"adopt and expect an error"
	notified := false.
	[ Rowan projectTools adopt adoptProjectDefinition: projectDefinition ]
		on: Error
		do: [:ex | notified := true ].
	self assert: notified.

"disown the project"
	Rowan projectTools disown disownProjectNamed: projectDefinition name.

"create an empty loaded project"
	self
		_issue481_loadProjectDefinition: projectName  
			packageName1: packageName1 
			packageName2: packageName2 
			symDictName: symDictName.

"adopt and handle the missing class"
	notified := false.
	[ Rowan projectTools adopt adoptProjectDefinition: projectDefinition ]
		on: RwAdoptMissingClassErrorNotification
		do: [:ex |
			notified := true.
			self assert: ex className = className1.
			self deny: ex isClassExtension.
			self assert: ex packageName = packageName1.
			ex resume ].
	self assert: notified.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
]

{ #category : 'tests' }
RwAdoptToolApiTest >> testAdopt_issue481_F [

	"https://github.com/GemTalk/Rowan/issues/481"

	"missing unpackaged class"

	| projectName packageNames className1 className2 packageName1 packageName2  symDictName projectDefinition notified audit|

	projectName := 'AdoptProject_481'.
	packageName1 := 'Adopt-Core'.
	packageName2 := 'Adopt-Extension'.
	packageNames := {packageName1 .  packageName2}.
	symDictName := self _symbolDictionaryName2.
	className1 := 'AdoptedClass'.
	className2 := 'ExtendedUnpackagedClass'.

"create image artifacts and projectDefinition"
	projectDefinition := self 
		_issue481_projectDefinition: projectName  
			packageName1: packageName1 
			packageName2: packageName2 
			className1: className1 
			className2: className2 
			symDictName: symDictName.

"disown the project"
	Rowan projectTools disown disownProjectNamed: projectDefinition name.

"remove className2"
	(Rowan globalNamed: symDictName) removeKey: className2 asSymbol.

"create an empty loaded project"
	self
		_issue481_loadProjectDefinition: projectName  
			packageName1: packageName1 
			packageName2: packageName2 
			symDictName: symDictName.

"adopt and expect an error"
	notified := false.
	[ Rowan projectTools adopt adoptProjectDefinition: projectDefinition ]
		on: Error
		do: [:ex | notified := true ].
	self assert: notified.

"disown the project"
	Rowan projectTools disown disownProjectNamed: projectDefinition name.

"create an empty loaded project"
	self
		_issue481_loadProjectDefinition: projectName  
			packageName1: packageName1 
			packageName2: packageName2 
			symDictName: symDictName.

"adopt and handle the missing class"
	notified := false.
	[ Rowan projectTools adopt adoptProjectDefinition: projectDefinition ]
		on: RwAdoptMissingClassErrorNotification
		do: [:ex |
			notified := true.
			self assert: ex className = className2.
			self assert: ex isClassExtension.
			self assert: ex packageName = packageName2.
			ex resume ].
	self assert: notified.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
]

{ #category : 'tests' }
RwAdoptToolApiTest >> testAdoptClass [
	| projectName packageNames className packageName theClass symDict symDictName instanceMethod classMethod symbolList |
	projectName := 'AdoptProject'.
	packageName := 'Adopt-Core'.
	packageNames := {packageName}.
	symDictName := self _symbolDictionaryName2.
	className := 'AdoptedClass'.
	symbolList := Rowan image symbolList.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: symDictName
		comment: 'project for testing package adopt api'.

	symDict := Rowan image symbolDictNamed: symDictName.

	"Use non-Rowan api to create class and methods"
	theClass := Object subclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().
	theClass category: packageName.

	instanceMethod := theClass
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.
	classMethod := theClass class
		compileMethod: 'bar ^''bar'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	self assert: theClass rowanProjectName = Rowan unpackagedName.
	self assert: instanceMethod rowanProjectName = Rowan unpackagedName.
	self assert: classMethod rowanProjectName = Rowan unpackagedName.

	Rowan packageTools adopt 
		adoptClassNamed: className 
		intoPackageNamed: packageName.

	self assert: theClass rowanProjectName = projectName.
	self assert: instanceMethod rowanProjectName = projectName.
	self assert: classMethod rowanProjectName = projectName.
]

{ #category : 'tests' }
RwAdoptToolApiTest >> testAdoptClassInvalidCategory [
	| projectName packageNames className packageName theClass symDict symDictName instanceMethod classMethod symbolList expectedFailure |
	projectName := 'AdoptProject'.
	packageName := 'Adopt-Core'.
	packageNames := {packageName}.
	symDictName := self _symbolDictionaryName2.
	className := 'AdoptedClass'.
	symbolList := Rowan image symbolList.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: symDictName
		comment: 'project for testing package adopt api'.

	symDict := Rowan image symbolDictNamed: symDictName.

	"Use non-Rowan api to create class and methods"
	theClass := Object subclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().

	instanceMethod := theClass
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.
	classMethod := theClass class
		compileMethod: 'bar ^''bar'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	self assert: theClass rowanProjectName = Rowan unpackagedName.
	self assert: instanceMethod rowanProjectName = Rowan unpackagedName.
	self assert: classMethod rowanProjectName = Rowan unpackagedName.

	expectedFailure := false.
	[ Rowan packageTools adopt adoptClassNamed: className intoPackageNamed: packageName ]
		on: Error do: [:ex |
			self assert: ex description equals: 'Error: Unable to adopt the class ''AdoptedClass'' into the package ''Adopt-Core'' as it''s class category violates the RowanHybrid package convention .'.
			expectedFailure := true ].
	self assert: expectedFailure
]

{ #category : 'tests' }
RwAdoptToolApiTest >> testAdoptClassWithExistingExtensionMethods_1 [
	| projectName packageNames className packageName1 packageName2 theClass symDict symDictName fooMethod barMethod symbolList |
	projectName := 'AdoptProject'.
	packageName1 := 'Adopt-Core1'.
	packageName2 := 'Adopt-Core2'.
	packageNames := {packageName1. packageName2}.
	symDictName := self _symbolDictionaryName2.
	className := 'AdoptedClass'.
	symbolList := Rowan image symbolList.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: symDictName
		comment: 'project for testing package adopt api'.

	symDict := Rowan globalNamed: symDictName.

	"Use non-Rowan api to create class and methods"
	theClass := Object subclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().

	fooMethod := theClass
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	barMethod := theClass
		rwCompileMethod: 'bar ^''bar'''
		category: '*' , packageName2 asLowercase.

	self assert: theClass rowanProjectName = Rowan unpackagedName.
	self assert: fooMethod rowanPackageName = Rowan unpackagedName.
	self assert: barMethod rowanPackageName = packageName2.

	Rowan packageTools adopt 
		adoptClassNamed: className 
		intoPackageNamed: packageName1.

	self assert: theClass rowanProjectName = projectName.
	self assert: fooMethod rowanPackageName = packageName1.
	self assert: barMethod rowanPackageName = packageName2.

]

{ #category : 'tests' }
RwAdoptToolApiTest >> testAdoptMethod [
	| projectName packageNames className packageName theClass symDict symDictName instanceMethod classMethod symbolList |
	projectName := 'AdoptProject'.
	packageName := 'Adopt-Core'.
	packageNames := {packageName}.
	symDictName := self _symbolDictionaryName2.
	className := 'AdoptedClass'.
	symbolList := Rowan image symbolList.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: symDictName
		comment: 'project for testing package adopt api'.

	symDict := Rowan globalNamed: symDictName.

	"Use non-Rowan api to create class and methods"
	theClass := Object subclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().

	instanceMethod := theClass
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.
	classMethod := theClass class
		compileMethod: 'bar ^''bar'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	self assert: theClass rowanProjectName = Rowan unpackagedName.
	self assert: instanceMethod rowanProjectName = Rowan unpackagedName.
	self assert: classMethod rowanProjectName = Rowan unpackagedName.

	Rowan packageTools adopt 
		adoptMethod: #foo 
			protocol: '*', packageName 
			inClassNamed: className  
			isMeta: false 
			intoPackageNamed: packageName;
		adoptMethod: #bar 
			protocol: '*', packageName 
			inClassNamed: className  
			isMeta: true 
			intoPackageNamed: packageName;
		yourself.

	self assert: theClass rowanProjectName = Rowan unpackagedName.
	self assert: instanceMethod rowanProjectName = projectName.
	self assert: classMethod rowanProjectName = projectName.

]

{ #category : 'tests' }
RwAdoptToolApiTest >> testAdoptMethod_issue389_1 [

	"https://github.com/GemTalk/Rowan/issues/389"

	"reproduce original bug using adoptMethod:protocol:inClassNamed:isMeta:intoPackageNamed:"

	| projectName packageNames className packageName1 theClass symDict symDictName instanceMethod classMethod audit |
	projectName := 'AdoptProject'.
	packageName1 := 'Adopt-Core'.
	packageNames := {packageName1 }.
	symDictName := self _symbolDictionaryName2.
	className := 'AdoptedClass'.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: symDictName
		comment: 'project for testing package adopt api'.

	symDict := Rowan globalNamed: symDictName.

	"Use Rowan api to create class and method"
	theClass := Object
		rwSubclass:className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	instanceMethod := theClass
		rwCompileMethod: 'foo ^''foo'''
		category: 'accessing'.
	classMethod := theClass class
		rwCompileMethod: 'bar ^''bar'''
		category: 'accessing'.

"validate"
	self assert: theClass rowanPackageName = packageName1.
	self assert: instanceMethod rowanPackageName = packageName1.
	self assert: classMethod rowanPackageName = packageName1.

"audit --> category not correct according to Monticello conventions"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.

"disown the method so it is no longer packaged"
	Rowan packageTools disown disownMethod: #foo inClassNamed: className isMeta: false.
	instanceMethod := theClass compiledMethodAt: #foo.

"confirm conruption"
	self assert: instanceMethod rowanPackageName =  Rowan unpackagedName.

"repair corruption using adopt"
	Rowan packageTools adopt
		adoptMethod: #foo
		protocol: 'accessing'
		inClassNamed: className
		isMeta: false
		intoPackageNamed:  packageName1.

"validate"
	self assert: theClass rowanPackageName = packageName1.
	self assert: instanceMethod rowanPackageName = packageName1.
	self assert: classMethod rowanPackageName = packageName1.
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty
]

{ #category : 'tests' }
RwAdoptToolApiTest >> testAdoptMethod_issue389_2 [

	"https://github.com/GemTalk/Rowan/issues/389"

	"reproduce original bug using adoptMethod:inClassNamed:isMeta:intoPackageNamed:"

	| projectName packageNames className packageName1 theClass symDict symDictName instanceMethod classMethod audit |
	projectName := 'AdoptProject'.
	packageName1 := 'Adopt-Core'.
	packageNames := {packageName1 }.
	symDictName := self _symbolDictionaryName2.
	className := 'AdoptedClass'.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: symDictName
		comment: 'project for testing package adopt api'.

	symDict := Rowan globalNamed: symDictName.

	"Use Rowan api to create class and method"
	theClass := Object
		rwSubclass:className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	instanceMethod := theClass
		rwCompileMethod: 'foo ^''foo'''
		category: 'accessing'.
	classMethod := theClass class
		rwCompileMethod: 'bar ^''bar'''
		category: 'accessing'.

"validate"
	self assert: theClass rowanPackageName = packageName1.
	self assert: instanceMethod rowanPackageName = packageName1.
	self assert: classMethod rowanPackageName = packageName1.

"audit --> category not correct according to Monticello conventions"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.

"disown the method so it is no longer packaged"
	Rowan packageTools disown disownMethod: #foo inClassNamed: className isMeta: false.
	instanceMethod := theClass compiledMethodAt: #foo.

"confirm conruption"
	self assert: instanceMethod rowanPackageName =  Rowan unpackagedName.

"repair corruption using adopt"
	Rowan packageTools adopt
		adoptMethod: #foo
		inClassNamed: className
		isMeta: false
		intoPackageNamed:  packageName1.

"validate"
	self assert: theClass rowanPackageName = packageName1.
	self assert: instanceMethod rowanPackageName = packageName1.
	self assert: classMethod rowanPackageName = packageName1.
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty
]

{ #category : 'tests' }
RwAdoptToolApiTest >> testAdoptMethod_issue389_3 [

	"https://github.com/GemTalk/Rowan/issues/389"

	"use adoptMethod:protocol:inClassNamed:isMeta:intoPackageNamed: where protocol is using Monticello convention"

	| projectName packageNames className packageName1 theClass symDict symDictName instanceMethod classMethod audit expectedFailure |
	projectName := 'AdoptProject'.
	packageName1 := 'Adopt-Core'.
	packageNames := {packageName1 }.
	symDictName := self _symbolDictionaryName2.
	className := 'AdoptedClass'.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: symDictName
		comment: 'project for testing package adopt api'.

	symDict := Rowan globalNamed: symDictName.

	"Use Rowan api to create class and method"
	theClass := Object
		rwSubclass:className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	instanceMethod := theClass
		rwCompileMethod: 'foo ^''foo'''
		category: 'accessing'.
	classMethod := theClass class
		rwCompileMethod: 'bar ^''bar'''
		category: 'accessing'.

"validate"
	self assert: theClass rowanPackageName = packageName1.
	self assert: instanceMethod rowanPackageName = packageName1.
	self assert: classMethod rowanPackageName = packageName1.

"audit --> category not correct according to Monticello conventions"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.


"disown the method so it is no longer packaged"
	Rowan packageTools disown disownMethod: #foo inClassNamed: className isMeta: false.
	instanceMethod := theClass compiledMethodAt: #foo.

"confirm conruption"
	self assert: instanceMethod rowanPackageName =  Rowan unpackagedName.

"repair corruption using adopt"
	Rowan packageTools adopt
		adoptMethod: #foo
		protocol: '*', packageName1 asLowercase "will cause audit to fail"
		inClassNamed: className
		isMeta: false
		intoPackageNamed:  packageName1.

"validate"
	self assert: theClass rowanPackageName = packageName1.
	self assert: instanceMethod rowanPackageName = packageName1.
	self assert: classMethod rowanPackageName = packageName1.

"audit --> category not correct according to Monticello conventions"
	self deny: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty. "expect: 'AdoptedClass #*adopt-core'->'Extension category name can not be same as class package'"

	self assert: audit size = 1.
	expectedFailure := ((audit at: packageName1) at: className).
	self assert: expectedFailure size = 1.
	self assert: expectedFailure first owner name =  'AdoptedClass'.
	self assert: expectedFailure first owner classCategory = 'Adopt-Core'.
	self assert: expectedFailure first reason equals: #'rowanHybridExtensionCategoryMatchesClassPackage'
]

{ #category : 'tests' }
RwAdoptToolApiTest >> testAdoptMethod_issue389_A [

	"https://github.com/GemTalk/Rowan/issues/389"

	"adopt methods into a different package than the packaged class; 
		don't use Monticello convention.
	 methods should be extension methods."

	| projectName packageNames className packageName1 packageName2 theClass symDict symDictName instanceMethod classMethod symbolList audit |
	projectName := 'AdoptProject'.
	packageName1 := 'Adopt-Core'.
	packageName2 := 'Adopt-Extension'.
	packageNames := {packageName1. packageName2 }.
	symDictName := self _symbolDictionaryName2.
	className := 'AdoptedClass'.
	symbolList := Rowan image symbolList.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: symDictName
		comment: 'project for testing package adopt api'.

	symDict := Rowan globalNamed: symDictName.

	"Use non-Rowan api to create class and methods"
	theClass := Object subclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().
	theClass category: packageName1.

	self assert: theClass rowanProjectName = Rowan unpackagedName.

	Rowan packageTools adopt 
		adoptClassNamed: className  
			intoPackageNamed: packageName1.

	instanceMethod := theClass
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.
	classMethod := theClass class
		compileMethod: 'bar ^''bar'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	self assert: instanceMethod rowanProjectName = Rowan unpackagedName.
	self assert: classMethod rowanProjectName = Rowan unpackagedName.

	Rowan packageTools adopt 
		adoptMethod: #foo 
			inClassNamed: className  
			isMeta: false 
			intoPackageNamed: packageName2;
		adoptMethod: #bar 
			inClassNamed: className  
			isMeta: true 
			intoPackageNamed: packageName2;
		yourself.

"validate"
	self assert: theClass rowanPackageName = packageName1.
	self assert: instanceMethod rowanPackageName = packageName2.
	self assert: classMethod rowanPackageName = packageName2.
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty
]

{ #category : 'tests' }
RwAdoptToolApiTest >> testAdoptMethod_issue389_B [

	"https://github.com/GemTalk/Rowan/issues/389"

	"adopt methods into an unpackaged class; 
		don't use Monticello convention.
	 methods should be extension methods."

	| projectName packageNames className packageName theClass symDict symDictName instanceMethod classMethod symbolList audit |
	projectName := 'AdoptProject'.
	packageName := 'Adopt-Extensions'.
	packageNames := {packageName}.
	symDictName := self _symbolDictionaryName2.
	className := 'AdoptedClass'.
	symbolList := Rowan image symbolList.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: symDictName
		comment: 'project for testing package adopt api'.

	symDict := Rowan globalNamed: symDictName.

	"Use non-Rowan api to create class and methods"
	theClass := Object subclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().

	instanceMethod := theClass
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.
	classMethod := theClass class
		compileMethod: 'bar ^''bar'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	self assert: theClass rowanProjectName = Rowan unpackagedName.
	self assert: instanceMethod rowanProjectName = Rowan unpackagedName.
	self assert: classMethod rowanProjectName = Rowan unpackagedName.

	Rowan packageTools adopt 
		adoptMethod: #foo 
			inClassNamed: className  
			isMeta: false 
			intoPackageNamed: packageName;
		adoptMethod: #bar 
			inClassNamed: className  
			isMeta: true 
			intoPackageNamed: packageName;
		yourself.

"validate"
	self assert: theClass rowanProjectName = Rowan unpackagedName.
	self assert: instanceMethod rowanProjectName = projectName.
	self assert: classMethod rowanProjectName = projectName.
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty
]

{ #category : 'tests' }
RwAdoptToolApiTest >> testAdoptMethod_issue389_C [

	"https://github.com/GemTalk/Rowan/issues/389"

	"adopt methods into a different package than the packaged class; 
		use Monticello convention (_A).
	 methods should be extension methods."

	"adopt methods into a packaged class change protocol to use Monticello convention (_A)"

	| projectName packageNames className packageName1 packageName2 theClass symDict symDictName instanceMethod classMethod symbolList audit |
	projectName := 'AdoptProject'.
	packageName1 := 'Adopt-Core'.
	packageName2 := 'Adopt-Extension'.
	packageNames := {packageName1. packageName2 }.
	symDictName := self _symbolDictionaryName2.
	className := 'AdoptedClass'.
	symbolList := Rowan image symbolList.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: symDictName
		comment: 'project for testing package adopt api'.

	symDict := Rowan globalNamed: symDictName.

	"Use non-Rowan api to create class and methods"
	theClass := Object subclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().
	theClass category: packageName1.

	self assert: theClass rowanProjectName = Rowan unpackagedName.

	Rowan packageTools adopt 
		adoptClassNamed: className  
			intoPackageNamed: packageName1.

	instanceMethod := theClass
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.
	classMethod := theClass class
		compileMethod: 'bar ^''bar'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	self assert: instanceMethod rowanProjectName = Rowan unpackagedName.
	self assert: classMethod rowanProjectName = Rowan unpackagedName.

	Rowan packageTools adopt 
		adoptMethod: #foo 
			protocol: '*', packageName2 asLowercase
			inClassNamed: className  
			isMeta: false 
			intoPackageNamed: packageName2;
		adoptMethod: #bar 
			protocol: '*', packageName2 asLowercase
			inClassNamed: className  
			isMeta: true 
			intoPackageNamed: packageName2;
		yourself.

"validate"
	self assert: theClass rowanPackageName = packageName1.
	self assert: instanceMethod rowanPackageName = packageName2.
	self assert: classMethod rowanPackageName = packageName2.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
]

{ #category : 'tests' }
RwAdoptToolApiTest >> testAdoptMethod_issue389_D [

	"https://github.com/GemTalk/Rowan/issues/389"

	"adopt methods into an unpackaged class; 
		don't use Monticello convention (_B).
	 methods should be extension methods."

	"adopt methods change protocol to use Monticello convention (_B)"

	| projectName packageNames className packageName theClass symDict symDictName instanceMethod classMethod symbolList audit |
	projectName := 'AdoptProject'.
	packageName := 'Adopt-Extension'.
	packageNames := {packageName}.
	symDictName := self _symbolDictionaryName2.
	className := 'AdoptedClass'.
	symbolList := Rowan image symbolList.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: symDictName
		comment: 'project for testing package adopt api'.

	symDict := Rowan globalNamed: symDictName.

	"Use non-Rowan api to create class and methods"
	theClass := Object subclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().

	instanceMethod := theClass
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.
	classMethod := theClass class
		compileMethod: 'bar ^''bar'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	self assert: theClass rowanProjectName = Rowan unpackagedName.
	self assert: instanceMethod rowanProjectName = Rowan unpackagedName.
	self assert: classMethod rowanProjectName = Rowan unpackagedName.

	Rowan packageTools adopt 
		adoptMethod: #foo 
			protocol: 'accessing'
			inClassNamed: className  
			isMeta: false 
			intoPackageNamed: packageName;
		adoptMethod: #bar 
			protocol: 'accessing'
			inClassNamed: className  
			isMeta: true 
			intoPackageNamed: packageName;
		yourself.

"validate"
	self assert: theClass rowanProjectName = Rowan unpackagedName.
	self assert: instanceMethod rowanProjectName = projectName.
	self assert: classMethod rowanProjectName = projectName.
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty
]

{ #category : 'tests' }
RwAdoptToolApiTest >> testAdoptSymbolDictionary_1 [

	"simplest case"

	"https://github.com/dalehenrich/Rowan/issues/208"

	| projectName packageNames className packageName theClass symDict symDictName instanceMethod classMethod symbolList |
	projectName := 'AdoptProject'.
	packageName := 'Adopt-Core'.
	packageNames := {packageName}.
	symDictName := self _symbolDictionaryName2.
	className := 'AdoptedClass'.
	symbolList := Rowan image symbolList.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: symDictName
		comment: 'project for testing package adopt api'.

	symDict := Rowan globalNamed: symDictName.

	"Use non-Rowan api to create class and methods"
	theClass := Object subclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().

	instanceMethod := theClass
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.
	classMethod := theClass class
		compileMethod: 'bar ^''bar'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	self assert: theClass rowanProjectName = Rowan unpackagedName.
	self assert: instanceMethod rowanProjectName = Rowan unpackagedName.
	self assert: classMethod rowanProjectName = Rowan unpackagedName.

	Rowan packageTools adopt 
		adoptSymbolDictionaryNamed: symDictName 
		intoPackageNamed: packageName.

	self assert: theClass rowanProjectName = projectName.
	self assert: instanceMethod rowanProjectName = projectName.
	self assert: classMethod rowanProjectName = projectName.

	self assert: theClass rowanPackageName = packageName.
	self assert: instanceMethod rowanPackageName = packageName.
	self assert: classMethod rowanPackageName = packageName.

]

{ #category : 'tests' }
RwAdoptToolApiTest >> testAdoptSymbolDictionary_2 [

	"packaged and unpackaged code sharing a symbol dictionary"

	"https://github.com/dalehenrich/Rowan/issues/208"

	| projectName packageNames className1 className2 packageName1 packageName2 theClass symDict symDictName instanceMethod classMethod symbolList |
	projectName := 'AdoptProject'.
	packageName1 := 'Adopt-Core1'.
	packageName2 := 'Adopt-Core2'.
	packageNames := {packageName1. packageName2.}.
	symDictName := self _symbolDictionaryName2.
	className1 := 'AdoptedClass1'.
	className2 := 'AdoptedClass2'.
	symbolList := Rowan image symbolList.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: symDictName
		comment: 'project for testing package adopt api'.

	symDict := Rowan globalNamed: symDictName.

	"Use Rowan api to create package class and methods"

	theClass := Object
		rwSubclass: className1
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	instanceMethod := theClass
		rwCompileMethod: 'foo ^''foo'''
		category: 'accessing'.
	classMethod := theClass class
		rwCompileMethod: 'bar ^''bar'''
		category: 'accessing'.

	self assert: theClass rowanProjectName = projectName.
	self assert: instanceMethod rowanProjectName = projectName.
	self assert: classMethod rowanProjectName = projectName.

	self assert: theClass rowanPackageName = packageName1.
	self assert: instanceMethod rowanPackageName = packageName1.
	self assert: classMethod rowanPackageName = packageName1.

	"Use non-Rowan api to create unpackaged class and methods"
	theClass := Object subclass: className2
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().

	instanceMethod := theClass
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.
	classMethod := theClass class
		compileMethod: 'bar ^''bar'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	self assert: theClass rowanProjectName = Rowan unpackagedName.
	self assert: instanceMethod rowanProjectName = Rowan unpackagedName.
	self assert: classMethod rowanProjectName = Rowan unpackagedName.

	Rowan packageTools adopt 
		adoptSymbolDictionaryNamed: symDictName 
		intoPackageNamed: packageName2.

	self assert: theClass rowanProjectName = projectName.
	self assert: instanceMethod rowanProjectName = projectName.
	self assert: classMethod rowanProjectName = projectName.

	self assert: theClass rowanPackageName = packageName2.
	self assert: instanceMethod rowanPackageName = packageName2.
	self assert: classMethod rowanPackageName = packageName2.

]

{ #category : 'tests' }
RwAdoptToolApiTest >> testAdoptSymbolList_1 [

	"https://github.com/dalehenrich/Rowan/issues/208"

	| projectName packageNames className packageName theClass symDict symDictName instanceMethod classMethod symbolList packageNameMap
		excludedSymbolDictionaries |
	projectName := 'AdoptProject'.
	packageName := 'Adopt-Core'.
	packageNames := {packageName}.
	symDictName := self _symbolDictionaryName2.
	className := 'AdoptedClass'.
	symbolList := Rowan image symbolList.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: symDictName
		comment: 'project for testing package adopt api'.

	symDict := Rowan globalNamed: symDictName.

	"Use non-Rowan api to create class and methods"
	theClass := Object subclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().

	instanceMethod := theClass
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.
	classMethod := theClass class
		compileMethod: 'bar ^''bar'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	self assert: theClass rowanProjectName = Rowan unpackagedName.
	self assert: instanceMethod rowanProjectName = Rowan unpackagedName.
	self assert: classMethod rowanProjectName = Rowan unpackagedName.

	packageNameMap := Dictionary new
		at: symDictName asString put: packageName;
		yourself.
	excludedSymbolDictionaries := symbolList reject: [:symbolDictionary | symbolDictionary name = symDictName ].
	Rowan packageTools adopt 
		adoptSymbolList: symbolList 
		excluding: excludedSymbolDictionaries 
		intoPackagesNamed: packageNameMap.

	self assert: theClass rowanProjectName = projectName.
	self assert: instanceMethod rowanProjectName = projectName.
	self assert: classMethod rowanProjectName = projectName.

	self assert: theClass rowanPackageName = packageName.
	self assert: instanceMethod rowanPackageName = packageName.
	self assert: classMethod rowanPackageName = packageName.

]

{ #category : 'tests' }
RwAdoptToolApiTest >> testAdoptSymbolList_2 [

	"https://github.com/dalehenrich/Rowan/issues/208"

	| projectName packageNames className packageName theClass symDict symDictName instanceMethod classMethod symbolList packageNameMap
		excludedSymbolDictionaryNames |
	projectName := 'AdoptProject'.
	packageName := 'Adopt-Core'.
	packageNames := {packageName}.
	symDictName := self _symbolDictionaryName2.
	className := 'AdoptedClass'.
	symbolList := Rowan image symbolList.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: symDictName
		comment: 'project for testing package adopt api'.

	symDict := Rowan globalNamed: symDictName.

	"Use non-Rowan api to create class and methods"
	theClass := Object subclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().

	instanceMethod := theClass
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.
	classMethod := theClass class
		compileMethod: 'bar ^''bar'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	self assert: theClass rowanProjectName = Rowan unpackagedName.
	self assert: instanceMethod rowanProjectName = Rowan unpackagedName.
	self assert: classMethod rowanProjectName = Rowan unpackagedName.

	packageNameMap := Dictionary new
		at: symDictName asString put: packageName;
		yourself.
	excludedSymbolDictionaryNames := (symbolList reject: [:symbolDictionary | symbolDictionary name = symDictName ]) collect: [:each | each name asString ].
	Rowan packageTools adopt 
		adoptSymbolList: symbolList 
		excludingSymbolDictsNamed: excludedSymbolDictionaryNames 
		intoPackagesNamed: packageNameMap.

	self assert: theClass rowanProjectName = projectName.
	self assert: instanceMethod rowanProjectName = projectName.
	self assert: classMethod rowanProjectName = projectName.

	self assert: theClass rowanPackageName = packageName.
	self assert: instanceMethod rowanPackageName = packageName.
	self assert: classMethod rowanPackageName = packageName.

]
