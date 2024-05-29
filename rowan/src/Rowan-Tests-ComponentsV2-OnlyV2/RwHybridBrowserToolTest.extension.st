Extension { #name : 'RwHybridBrowserToolTest' }

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwHybridBrowserToolTest >> _standardProjectDefinition: projectName packageNameMap: packageNameMap defaultSymbolDictName: defaultSymbolDictName [
	| projectDefinition componentName |
	componentName := 'Core'.
	projectDefinition := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: defaultSymbolDictName;
		addLoadComponentNamed: componentName;
		yourself.

	packageNameMap
		keysAndValuesDo: [ :packageName :symDictName | 
			projectDefinition
				addPackageNamed: packageName toComponentNamed: componentName;
				gemstoneSetSymbolDictName: symDictName forPackageNamed: packageName;
				yourself ].

	^ projectDefinition
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwHybridBrowserToolTest >> testHybridComplicatedProjectLoad [

	"Write project to disk, make a few modifications and then reload the project from disk"

	| normalClass1 normalClass2 projectName packageNames packageName1 packageName2 normalInstance1 
		normalInstance2 projectTools className1 className2 theLoadedProject theLoadedPackage 
		theLoadedClassOrClassExtension writtenStateValidationBlock classNames oldNormalClass2 
		project |

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
		root: self _testRowanProjectsSandbox pathString, '/rowanHybridPatchProject/'.
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

	self assert: (normalClass1 perform: #bar) = 'bar'.
	normalInstance1 := normalClass1 new.
	self assert: (normalInstance1 perform: #foo) = 'foo'.

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
	self should: [ normalInstance2 perform: #biz ] raise: MessageNotUnderstood.
	self should: [ normalInstance2 perform: #biff ] raise: MessageNotUnderstood.

	projectTools := Rowan projectTools.

	project := (Rowan projectNamed: projectName).
	project asDefinition
		export;
		exportLoadSpecification.

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
				ifTrue: [ self assert: (#(#'bar') includes: loadedMethod selector) ] ]
		loadedTraitsDo: [ :loadedProject :loadedPackage :loadedTrait | ] ].

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

	self assert: (normalInstance1 perform: #foo) = 'foo'.
	self assert: (normalInstance1 perform: #ivar1) = nil.
	self assert: (normalClass1 perform: #baz) = 'baz'.
	self should: [ normalClass1 perform: #bar ] raise: MessageNotUnderstood.

	normalClass2 rwCompileMethod: 'biz ^''biz''' category: 'accessing'.	"add new instance method"
	normalClass2
		rwCompileMethod: 'biff ^''biff'''
		category: '*' , packageName2 asLowercase.	"add new class-side method"

	normalInstance2 := normalClass2 new.
	self assert: (normalInstance2 perform: #biz) = 'biz'.
	self assert: (normalInstance2 perform: #biff) = 'biff'.

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
				ifTrue: [ self assert: (#(#'baz') includes: loadedMethod selector) ] ]
		loadedTraitsDo: [  :loadedProject :loadedPackage :loadedTrait | ].

	projectTools load loadProjectNamed: projectName.	"restore original state of project"

	normalClass1 := Rowan globalNamed: className1.
	normalInstance1 := normalClass1 new.

	self assert: (normalClass1 perform: #bar) = 'bar'.
	self assert: (normalInstance1 perform: #foo) = 'foo'.
	self should: [ normalInstance1 perform: #ivar1 ] raise: MessageNotUnderstood.
	self should: [ normalClass1 perform: #baz = 'baz' ] raise: MessageNotUnderstood.

	oldNormalClass2 := normalClass2.
	normalClass2 := Rowan globalNamed: className2.
	self assert: oldNormalClass2 == normalClass2.
	normalInstance2 := normalClass2 new.
	self should: [ normalInstance2 perform: #biz ] raise: MessageNotUnderstood.
	self should: [ normalInstance2 perform: #biff ] raise: MessageNotUnderstood.

	writtenStateValidationBlock value	"verify that original state is restored"
]

{ #category : '*rowan-tests-componentsv2-onlyv2' }
RwHybridBrowserToolTest >> testHybridProjectLoad [

	"Write project to disk, make a few modifications and then reload the project from disk"

	| normalClass projectName packageNames packageName1 packageName2 fooMethod 
		barMethod normalInstance projectTools bazMethod ivar1Method className 
		theLoadedProject theLoadedPackage theLoadedClassOrClassExtension 
		writtenStateValidationBlock project |
	projectName := 'HybridPatchProjectA'.
	packageName1 := 'Hybrid-Patch-Core'.
	packageName2 := 'Hybrid-Patch-Extensions'.
	packageNames := { packageName1 . packageName2 }.

	projectTools := Rowan projectTools.

	self
		_loadDiskProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project package patch test'
		format: 'tonel'
		root: self _testRowanProjectsSandbox pathString, '/rowanHybridPatchProject/'.
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

	self assert: (normalClass perform: #bar) = 'bar'.
	normalInstance := normalClass new.
	self assert: (normalInstance perform: #foo) = 'foo'.

	project := RwProject newNamed: projectName.
	project exportLoadSpecification.
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
				ifTrue: [ self assert: (#(#'bar') includes: loadedMethod selector) ] ]
		loadedTraitsDo: [ :loadedProject :loadedPackage :loadedTrait | ] ].

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

	self assert: (normalInstance perform: #foo) = 'foo'.
	self assert: (normalInstance perform: #ivar1) = nil.
	self assert: (normalClass perform: #baz) = 'baz'.
	self should: [ normalClass perform: #bar ] raise: MessageNotUnderstood.

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
				ifTrue: [ self assert: (#(#'baz') includes: loadedMethod selector) ] ]
		loadedTraitsDo: [ :loadedProject :loadedPackage :loadedTrait | ].

	projectTools load loadProjectNamed: projectName.	"restore original state of project"

	normalClass := Rowan globalNamed: className.
	normalInstance := normalClass new.

	self assert: (normalClass perform: #bar) = 'bar'.
	self assert: (normalInstance perform: #foo) = 'foo'.
	self should: [ normalInstance perform: #ivar1 ] raise: MessageNotUnderstood.
	self should: [ normalClass perform: #baz = 'baz' ] raise: MessageNotUnderstood.

	writtenStateValidationBlock value	"verify that original state is restored"
]
