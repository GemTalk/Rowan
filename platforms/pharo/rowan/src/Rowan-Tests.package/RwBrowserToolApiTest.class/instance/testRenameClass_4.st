tests
testRenameClass_4

  "rename a class with a subclass - reference to renamed class in methods subclass references old superclass in method"

  "https://github.com/dalehenrich/Rowan/issues/470"

  | projectName  packageName1 packageName2 projectDefinition classDefinition1 classDefinition packageDefinition className1 className2 className3
    className4 projectSetDefinition class1 class2 class3 classExtensionDefinition validateBlock |

  projectName := 'Issue470'.
  packageName1 := 'Issue470-Core'.
  packageName2 := 'Issue470-Extensions'.
  className1 := 'Issue470Class1'.
  className2 := 'Issue470Class2'.
  className3 := 'Issue470Class3'.
  className4 := 'Issue470Class4'.

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
      instvars: #(ivar1)
      classinstvars: #()
      classvars: #()
      category: packageName1
      comment: 'comment'
      pools: #()
      type: 'normal')
    addInstanceMethodDefinition: (RwMethodDefinition newForSource: 'instanceMethod1 ^1' protocol: 'accessing');
    addClassMethodDefinition: (RwMethodDefinition newForSource: 'classMethod1 ^', className2 protocol: 'accessing');
    yourself.
  packageDefinition
    addClassDefinition: classDefinition1.

  classDefinition := (RwClassDefinition
    newForClassNamed: className2
      super: className1
      instvars: #(ivar2)
      classinstvars: #()
      classvars: #()
      category: packageName1
      comment: 'comment'
      pools: #()
      type: 'normal')
    addInstanceMethodDefinition: (RwMethodDefinition newForSource: 'instanceMethod2 ^2' protocol: 'accessing');
    addClassMethodDefinition: (RwMethodDefinition newForSource: 'classMethod2 ^', className3 protocol: 'accessing');
    yourself.
  packageDefinition
    addClassDefinition: classDefinition.

  classDefinition := (RwClassDefinition
    newForClassNamed: className3
      super: className2
      instvars: #(ivar4 ivar3)
      classinstvars: #()
      classvars: #()
      category: packageName1
      comment: 'comment'
      pools: #()
      type: 'normal')
    addInstanceMethodDefinition: (RwMethodDefinition newForSource: 'instanceMethod3 ^3' protocol: 'accessing');
    addClassMethodDefinition: (RwMethodDefinition newForSource: 'classMethod3 ^', className2 protocol: 'accessing');
    yourself.
  packageDefinition
    addClassDefinition: classDefinition.

"create extension methods"
  packageDefinition := projectDefinition packageNamed: packageName2.

  classExtensionDefinition := (RwClassExtensionDefinition newForClassNamed: className1)
    addInstanceMethodDefinition: (RwMethodDefinition newForSource: 'extensionInstanceMethod1 ^1' protocol: '*', packageName2 asLowercase);
    addClassMethodDefinition: (RwMethodDefinition newForSource: 'extensionClassMethod1 ^1' protocol: '*', packageName2 asLowercase); 
    yourself.
  packageDefinition addClassExtensionDefinition: classExtensionDefinition.

  classExtensionDefinition := (RwClassExtensionDefinition newForClassNamed: className2)
    addInstanceMethodDefinition: (RwMethodDefinition newForSource: 'extensionInstanceMethod2 ^2' protocol: '*', packageName2 asLowercase);
    addClassMethodDefinition: (RwMethodDefinition newForSource: 'extensionClassMethod2 ^2' protocol: '*', packageName2 asLowercase); 
    yourself.
  packageDefinition addClassExtensionDefinition: classExtensionDefinition.

  classExtensionDefinition := (RwClassExtensionDefinition newForClassNamed: className3)
    addInstanceMethodDefinition: (RwMethodDefinition newForSource: 'extensionInstanceMethod3 ^3' protocol: '*', packageName2 asLowercase);
    addClassMethodDefinition: (RwMethodDefinition newForSource: 'extensionClassMethod3 ^', className1 protocol: '*', packageName2 asLowercase); 
    yourself.
  packageDefinition addClassExtensionDefinition: classExtensionDefinition.


"load"
  projectSetDefinition := RwProjectSetDefinition new.
  projectSetDefinition addDefinition: projectDefinition.
  Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
  validateBlock := [:oldClass1 :oldClass2 :oldClass3 |
  class1 := Rowan globalNamed: className1.
  self assert: class1 == oldClass1.
  self assert: class1 instVarNames = #(ivar1).
  self assert: (class1 compiledMethodAt: #instanceMethod1) rowanPackageName = packageName1.
  self assert: (class1 class compiledMethodAt: #classMethod1) rowanPackageName = packageName1.
  self assert: (class1 compiledMethodAt: #extensionInstanceMethod1) rowanPackageName = packageName2.
  self assert: (class1 class compiledMethodAt: #extensionClassMethod1) rowanPackageName = packageName2.

  class2 := Rowan globalNamed: className2.
  self assert: class2 == oldClass2.
  self assert: class2 instVarNames = #(ivar2).
  self assert: (class2 compiledMethodAt: #instanceMethod2) rowanPackageName = packageName1.
  self assert: (class2 class compiledMethodAt: #classMethod2) rowanPackageName = packageName1.
  self assert: (class2 compiledMethodAt: #extensionInstanceMethod2) rowanPackageName = packageName2.
  self assert: (class2 class compiledMethodAt: #extensionClassMethod2) rowanPackageName = packageName2.
  self assert: class2 superClass == class1.

  class3 := Rowan globalNamed: className3.
  self assert: class3 == oldClass3.
  self assert: class3 instVarNames = #(ivar4 ivar3).
  self assert: (class3 compiledMethodAt: #instanceMethod3) rowanPackageName = packageName1.
  self assert: (class3 class compiledMethodAt: #classMethod3) rowanPackageName = packageName1.
  self assert: (class3 compiledMethodAt: #extensionInstanceMethod3) rowanPackageName = packageName2.
  self assert: (class3 class compiledMethodAt: #extensionClassMethod3) rowanPackageName = packageName2.
  self assert: class3 superClass == class2 ].

   validateBlock value: (Rowan globalNamed: className1) value: (Rowan globalNamed: className2) value: (Rowan globalNamed: className3).

"perform rename --- expect to fail with compile error at the moment"
	self should: [
		Rowan projectTools browser
			renameClassNamed: className2 to: className4 ]
		raise: CompileError.

"validate"
  validateBlock value: class1 value: class2 value: class3.