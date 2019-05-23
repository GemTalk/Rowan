tests-issue 310
testIssue310

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
