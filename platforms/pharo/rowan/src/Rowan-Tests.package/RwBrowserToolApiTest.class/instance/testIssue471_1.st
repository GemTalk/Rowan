tests
testIssue471_1

	"expected failure until issue #471 is addressed"

  "https://github.com/dalehenrich/Rowan/issues/471"

  | projectName  packageName1 packageName2 projectDefinition classDefinition packageDefinition className1 className2 className3
    className4 projectSetDefinition class1 class2 class3 class4 oldClass1 oldClass2 oldClass3  |

  projectName := 'Issue471'.
  packageName1 := 'Issue471-Core'.
  packageName2 := 'Issue471-Extensions'.
  className1 := 'Issue471Class1'.
  className2 := 'Issue471Class2'.
  className3 := 'Issue471Class3'.
  className4 := 'Issue471Class4'.

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
    newForClassNamed: className1
      super: 'Object'
      instvars: #(ivar1)
      classinstvars: #()
      classvars: #()
      category: packageName1
      comment: 'comment'
      pools: #()
      type: 'normal')
    yourself.
  packageDefinition
    addClassDefinition: classDefinition.

  classDefinition := (RwClassDefinition
    newForClassNamed: className2
      super: className1
      instvars: #('ivar2')
      classinstvars: #()
      classvars: #()
      category: packageName1
      comment: 'comment'
      pools: #()
      type: 'normal')
    yourself.
  packageDefinition
    addClassDefinition: classDefinition.

  classDefinition := (RwClassDefinition
    newForClassNamed: className3
      super: className2
      instvars: #('ivar4' 'ivar3')
      classinstvars: #()
      classvars: #()
      category: packageName1
      comment: 'comment'
      pools: #()
      type: 'normal')
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

  class2 := Rowan globalNamed: className2.
  self assert: class2 instVarNames = #(ivar2).
  self assert: class2 superClass == class1.

  class3 := Rowan globalNamed: className3.
  self assert: class3 instVarNames = #(ivar4 ivar3).
  self assert: class3 superClass == class2.

"remove class2 and add class4 -- edit projectDefinition structure in place"
  packageDefinition := projectDefinition packageNamed: packageName1.

  packageDefinition removeClassNamed: className2.

  classDefinition := (RwClassDefinition
    newForClassNamed: className4
      super: className1
      instvars: #('ivar2')
      classinstvars: #()
      classvars: #()
      category: packageName1
      comment: 'comment'
      pools: #()
      type: 'normal')
    yourself.
  packageDefinition
    addClassDefinition: classDefinition.
"load"
  Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
  oldClass1 := class1.
  oldClass2 := class2.
  oldClass3 := class3.
 
  class1 := Rowan globalNamed: className1.
  self assert: class1 instVarNames = #(ivar1).
  self assert: oldClass1 == class1.

  class4 := Rowan globalNamed: className4.
  self assert: class4 instVarNames = #(ivar2).
  self assert: class4 superClass == class1.

  class3 := Rowan globalNamed: className3.
  self assert: class3 instVarNames = #(ivar4 ivar3).
  self assert: class3 superClass == oldClass2.
  self assert: oldClass3 == class3.