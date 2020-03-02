Extension { #name : 'RwProjectAuditToolTest' }

{ #category : '*rowan-testsv2' }
RwProjectAuditToolTest >> test_Issue_535 [

  "https://github.com/dalehenrich/Rowan/issues/535"

  | projectName  packageName1 packageName2 projectDefinition classDefinition1 classDefinition 
		packageDefinition className1 className2 className3 className4 projectSetDefinition
		class1 class2 class3  classExtensionDefinition audit |

  projectName := 'Issue534'.
  packageName1 := 'Issue534-Core'.
  packageName2 := 'Issue534-Extensions'.
  className1 := 'Issue534Class1'.
  className2 := 'Issue534Class2'.
  className3 := 'Issue534Class3'.
  className4 := 'Issue534Class4'.

  {projectName}
    do: [ :pn |
      (Rowan image loadedProjectNamed: pn ifAbsent: [  ])
        ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
  projectDefinition := ((Rowan globalNamed: 'RwProjectDefinition')
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
    addClassMethodDefinition: (RwMethodDefinition newForSource: 'classMethod3 ^', className1 protocol: 'accessing');
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
    addClassMethodDefinition: (RwMethodDefinition newForSource: 'extensionClassMethod3 ^3' protocol: '*', packageName2 asLowercase); 
    yourself.
  packageDefinition addClassExtensionDefinition: classExtensionDefinition.

"load"
  projectSetDefinition := RwProjectSetDefinition new.
  projectSetDefinition addDefinition: projectDefinition.
  Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

  class1 := Rowan globalNamed: className1.
  class2 := Rowan globalNamed: className2.
  class3 := Rowan globalNamed: className3.

"perform rename"
  Rowan projectTools browser
    renameClassNamed: className2 to: className4.

"audit"
  self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty
]
