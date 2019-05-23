tests - packages
testHybridComplicatedProjectLoad

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
		root: '/tmp/rowanHybridPatchProject/'.
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