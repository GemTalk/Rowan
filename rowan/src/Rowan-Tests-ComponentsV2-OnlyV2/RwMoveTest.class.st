Class {
	#name : 'RwMoveTest',
	#superclass : 'RwToolTest',
	#category : 'Rowan-Tests-ComponentsV2-OnlyV2'
}

{ #category : 'tests - issue 254' }
RwMoveTest >> testProjectSet_move_class_between_existing_packages_and_symbolDict [

	"https://github.com/dalehenrich/Rowan/issues/316"

	"initial load uses symbol dictionary _1, second load uses symbol dictionary _2"

	| projectName componentName packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition packageDefinition className projectSetDefinition class
		ar oldClass loadedClass symDict registry project audit |
	projectName := 'Issue254'.
	componentName := 'Core'.
	packageName1 := 'Issue254-Core1'.
	packageName2 := 'Issue254-Core2'.
	className := 'Issue254Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addComponentNamed: componentName;
		addPackagesNamed: { packageName1 . packageName2 } toComponentNamed: componentName;
		yourself.

	projectDefinition1
		resolve.

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

	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.

	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName1 asSymbol.


	projectSetDefinition := RwProjectSetDefinition new.

	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 forPackageNamed: packageName2.

	packageDefinition := projectDefinition2 packageNamed: packageName1.
	packageDefinition removeClassDefinition: classDefinition.

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

	loadedClass := Rowan image loadedClassNamed: className. "no error is success"

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: (audit := project audit) isEmpty.
	self assert: registry packageRegistry size = 1.
	self assert: registry methodRegistry size = 0.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry methodRegistry size = 0.
]

{ #category : 'tests - issue 254' }
RwMoveTest >> testProjectSet_move_class_between_packages [

	"Use the regular load stack"

	"https://github.com/dalehenrich/Rowan/issues/254"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition packageDefinition 
		className projectSetDefinition class symDict registry componentName project audit |
	projectName := 'Issue254'.
	componentName := 'Core'.
	packageName1 := 'Issue254-Core1'.
	packageName2 := 'Issue254-Core2'.
	className := 'Issue254Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addComponentNamed: componentName;
		addPackagesNamed: { packageName1 . packageName2 } toComponentNamed: componentName;
		resolve;
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

	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.

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

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: (audit := project audit) isEmpty.
	self assert: registry packageRegistry size = 2.
	self assert: registry methodRegistry size = 0.
]

{ #category : 'tests - issue 254' }
RwMoveTest >> testProjectSet_move_class_between_packages_and_symbolDict [

	"https://github.com/dalehenrich/Rowan/issues/254"

	"initial load uses symbol dictionary _1, second load uses symbol dictionary _2"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition 
		packageDefinition className projectSetDefinition class ar oldClass loadedClass symDict registry
		componentName project audit |
	projectName := 'Issue254'.
	packageName1 := 'Issue254-Core1'.
	packageName2 := 'Issue254-Core2'.
	className := 'Issue254Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addComponentNamed: componentName;
		addPackagesNamed: { packageName1 } toComponentNamed: componentName;
		resolve;
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

	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.

	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName1 asSymbol.

	projectSetDefinition := RwProjectSetDefinition new.

	projectDefinition2 := (Rowan projectNamed: projectName) asDefinition.
	projectDefinition2
		removePackageNamed: packageName1;
		addPackageNamed: packageName2 toComponentNamed: componentName;
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 
			forPackageNamed: packageName2;
		yourself.

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

	loadedClass := Rowan image loadedClassNamed: className. "no error is success"

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: (audit := project audit) isEmpty.
	self assert: registry packageRegistry size = 0.
	self assert: registry methodRegistry size = 0.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry methodRegistry size = 0.
]

{ #category : 'tests - issue 254' }
RwMoveTest >> testProjectSet_move_class_with_changed_method_between_packages [

	"https://github.com/dalehenrich/Rowan/issues/316"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition 
		packageDefinition className projectSetDefinition class oldClass symDict registry loadedClass 
		componentName project audit |
	projectName := 'Issue254'.
	packageName1 := 'Issue254-Core1'.
	packageName2 := 'Issue254-Core2'.
	className := 'Issue254Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].
"setup defintions"
	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addComponentNamed: componentName;
		addPackagesNamed: { packageName1 . packageName2} toComponentNamed: componentName;
		resolve;
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
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.

"validate"
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName1.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry methodRegistry size = 1.

"move class and add change method"
	projectSetDefinition := RwProjectSetDefinition new.
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName1.
	packageDefinition classDefinitions removeKey: classDefinition key.

	classDefinition
		updateInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^2').

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition addDefinition: projectDefinition2.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	oldClass := class.
	class := Rowan globalNamed: className.
	self assert: class == oldClass.
	self assert: class rowanPackageName = packageName2.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName2.

	loadedClass := Rowan image loadedClassNamed: className.
	loadedClass loadedInstanceMethods includesKey: #method1.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: (audit := project audit) isEmpty.
	self assert: registry packageRegistry size = 2.
	self assert: registry methodRegistry size = 1.

]

{ #category : 'tests - issue 254' }
RwMoveTest >> testProjectSet_move_class_with_changed_method_between_packages_and_symbolDict [

	"https://github.com/dalehenrich/Rowan/issues/316"

	"initial load uses symbol dictionary _1, second load uses symbol dictionary _2"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition 
		packageDefinition className projectSetDefinition class project audit componentName
		ar oldClass loadedClass symDict registry |
	projectName := 'Issue254'.
	packageName1 := 'Issue254-Core1'.
	packageName2 := 'Issue254-Core2'.
	className := 'Issue254Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"setup definitions"
	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addComponentNamed: componentName;
		addPackagesNamed: { packageName1 } toComponentNamed: componentName;
		resolve;
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
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName1 asSymbol.

"move class; change method"
	projectSetDefinition := RwProjectSetDefinition new.

	projectDefinition2 := (Rowan projectNamed: projectName) asDefinition.
	projectDefinition2
		removePackageNamed: packageName1;
		addPackageNamed: packageName2 toComponentNamed: componentName;
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 
			forPackageNamed: packageName2;
		yourself.

	packageDefinition := projectDefinition2 packageNamed: packageName2.

	classDefinition
		updateInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^2').

	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition addDefinition: projectDefinition2.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	self assert: (audit := project audit) isEmpty.
	loadedClass := Rowan image loadedClassNamed: className.
	loadedClass loadedInstanceMethods includesKey: #method1.

	oldClass := class.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName2.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName2.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName2 asSymbol.

	self assert: class == oldClass.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 0.
	self assert: registry methodRegistry size = 0.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry methodRegistry size = 1.

]

{ #category : 'tests - issue 254' }
RwMoveTest >> testProjectSet_move_class_with_extension_method_between_packages [

	"Use the regular load stack"

	"https://github.com/dalehenrich/Rowan/issues/316"

	| projectName  packageName1 packageName2 packageName3 projectDefinition1 projectDefinition2 
		classDefinition packageDefinition className projectSetDefinition class oldClass symDict registry 
		loadedClass classExtensionDefinition componentName project audit |
	projectName := 'Issue254'.
	packageName1 := 'Issue254-Core1'.
	packageName2 := 'Issue254-Core2'.
	packageName3 := 'Issue254-Extension3'.
	className := 'Issue254Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"set up  definitions"
	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addComponentNamed: componentName;
		addPackagesNamed: { packageName1 . packageName2 . packageName3 } 
			toComponentNamed: componentName;
		resolve;
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
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	classExtensionDefinition := RwClassExtensionDefinition
		newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #ext1
						protocol: '*', packageName3
						source: 'ext1 ^ 1').

	packageDefinition := projectDefinition1 packageNamed: packageName3.
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.

"validate"
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName1.

"move class"
	projectSetDefinition := RwProjectSetDefinition new.
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName1.
	packageDefinition classDefinitions removeKey: classDefinition key.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition addDefinition: projectDefinition2.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	oldClass := class.
	class := Rowan globalNamed: className.
	self assert: class == oldClass.
	self assert: class rowanPackageName = packageName2.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName2.
	self assert: (class compiledMethodAt: #ext1) rowanPackageName = packageName3.

	loadedClass := Rowan image loadedClassNamed: className.
	loadedClass loadedInstanceMethods includesKey: #method1.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: (audit := project audit) isEmpty.
	self assert: registry packageRegistry size = 3.
	self assert: registry methodRegistry size = 2.

]

{ #category : 'tests - issue 254' }
RwMoveTest >> testProjectSet_move_class_with_extension_method_between_packages_and_symbolDict [

	"https://github.com/dalehenrich/Rowan/issues/316"

	"initial load uses symbol dictionary _1, second load uses symbol dictionary _2"

	| projectName  packageName1 packageName2 packageName3 packageName4 projectDefinition1
		projectDefinition2 classDefinition packageDefinition className projectSetDefinition class 
		classExtensionDefinition ar oldClass loadedClass symDict registry project audit componentName |
	projectName := 'Issue254'.
	packageName1 := 'Issue254-Core1'.
	packageName2 := 'Issue254-Extension2'.
	packageName3 := 'Issue254-Core3'.
	packageName4 := 'Issue254-Extension4'.
	className := 'Issue254Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"setup"
	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addComponentNamed: componentName;
		addPackagesNamed: { packageName1 . packageName2 } toComponentNamed: componentName;
		resolve;
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
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	classExtensionDefinition := RwClassExtensionDefinition
		newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #ext1
						protocol: '*', packageName2 asLowercase
						source: 'ext1 ^ 1').

	packageDefinition := projectDefinition1 packageNamed: packageName2.
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #ext1) rowanPackageName = packageName2.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName1 asSymbol.

	projectSetDefinition := RwProjectSetDefinition new.

"move class and extension method to different packages and symbol dictionaries"
	projectDefinition2 := (Rowan projectNamed: projectName) asDefinition.
	projectDefinition2
		removePackageNamed: packageName1;
		removePackageNamed: packageName2;
		addPackagesNamed: { packageName3 . packageName4 } toComponentNamed: componentName;
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 
			forPackageNamed: packageName3;
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 
			forPackageNamed: packageName4;
		yourself.

	packageDefinition := projectDefinition2 packageNamed: packageName3.
	packageDefinition addClassDefinition: classDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName4.
	(classExtensionDefinition instanceMethodDefinitions at: #ext1)
		protocol: '*', packageName4 asLowercase.
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

	projectSetDefinition addDefinition: projectDefinition2.
"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	self assert: (audit := project audit) isEmpty.
	loadedClass := Rowan image loadedClassNamed: className.
	loadedClass loadedInstanceMethods includesKey: #method1.

	oldClass := class.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName3.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName3.
	self assert: (class compiledMethodAt: #ext1) rowanPackageName = packageName4.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName2 asSymbol.

	self assert: class == oldClass.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 0.
	self assert: registry methodRegistry size = 0.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry methodRegistry size = 2.
]

{ #category : 'tests - issue 254' }
RwMoveTest >> testProjectSet_move_class_with_method_between_packages [

	"Use the regular load stack"

	"https://github.com/dalehenrich/Rowan/issues/254"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition 
		packageDefinition componentName className projectSetDefinition class oldClass symDict 
		registry loadedClass project audit |
	projectName := 'Issue254'.
	packageName1 := 'Issue254-Core1'.
	packageName2 := 'Issue254-Core2'.
	className := 'Issue254Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addComponentNamed: componentName;
		addPackagesNamed: { packageName1 . packageName2 } toComponentNamed: componentName;
		resolve;
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
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.

	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName1.

	projectSetDefinition := RwProjectSetDefinition new.
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName1.
	packageDefinition classDefinitions removeKey: classDefinition key.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition addDefinition: projectDefinition2.

	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.

	oldClass := class.
	class := Rowan globalNamed: className.
	self assert: class == oldClass.
	self assert: class rowanPackageName = packageName2.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName2.

	loadedClass := Rowan image loadedClassNamed: className.
	loadedClass loadedInstanceMethods includesKey: #method1.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry methodRegistry size = 1.

]

{ #category : 'tests - issue 254' }
RwMoveTest >> testProjectSet_move_class_with_method_between_packages_and_symbolDict [

	"https://github.com/dalehenrich/Rowan/issues/254"

	"initial load uses symbol dictionary _1, second load uses symbol dictionary _2"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition 
		packageDefinition className projectSetDefinition class ar oldClass loadedClass symDict registry
		project audit componentName |
	projectName := 'Issue254'.
	packageName1 := 'Issue254-Core1'.
	packageName2 := 'Issue254-Core2'.
	className := 'Issue254Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addComponentNamed: componentName;
		addPackagesNamed: { packageName1 } toComponentNamed: componentName;
		resolve;
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
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName1 asSymbol.

	projectSetDefinition := RwProjectSetDefinition new.

	projectDefinition2 := (Rowan projectNamed: projectName) asDefinition.
	projectDefinition2
		removePackageNamed: packageName1;
		addPackageNamed: packageName2 toComponentNamed: componentName;
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 
			forPackageNamed: packageName2;
		yourself.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	loadedClass := Rowan image loadedClassNamed: className.
	loadedClass loadedInstanceMethods includesKey: #method1.

"validate"
	self assert: (audit := project audit) isEmpty.
	oldClass := class.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName2.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName2.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName2 asSymbol.

	self assert: class == oldClass.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: (audit := project audit) isEmpty.
	self assert: registry packageRegistry size = 0.
	self assert: registry methodRegistry size = 0.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry methodRegistry size = 1.
]

{ #category : 'tests - issue 254' }
RwMoveTest >> testProjectSet_move_class_with_new_method_between_packages [

	"https://github.com/dalehenrich/Rowan/issues/316"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition 
		packageDefinition componentName className projectSetDefinition class oldClass symDict 
		registry loadedClass project audit |
	projectName := 'Issue254'.
	packageName1 := 'Issue254-Core1'.
	packageName2 := 'Issue254-Core2'.
	className := 'Issue254Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].
"setup defintions"
	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addComponentNamed: componentName;
		addPackagesNamed: { packageName1 . packageName2 } toComponentNamed: componentName;
		resolve;
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
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.

"validate"
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName1.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry methodRegistry size = 1.

"move class and add new method"
	projectSetDefinition := RwProjectSetDefinition new.
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName1.
	packageDefinition classDefinitions removeKey: classDefinition key.

	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method2'
					protocol: 'accessing'
					source: 'method2 ^2').

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition addDefinition: projectDefinition2.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	oldClass := class.
	class := Rowan globalNamed: className.
	self assert: class == oldClass.
	self assert: class rowanPackageName = packageName2.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName2.

	loadedClass := Rowan image loadedClassNamed: className.
	loadedClass loadedInstanceMethods includesKey: #method1.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: (audit := project audit) isEmpty.
	self assert: registry packageRegistry size = 2.
	self assert: registry methodRegistry size = 2.

]

{ #category : 'tests - issue 254' }
RwMoveTest >> testProjectSet_move_class_with_new_method_between_packages_and_symbolDict [

	"https://github.com/dalehenrich/Rowan/issues/316"

	"initial load uses symbol dictionary _1, second load uses symbol dictionary _2"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition 
		packageDefinition className projectSetDefinition class ar oldClass loadedClass symDict registry
		project audit componentName |
	projectName := 'Issue254'.
	packageName1 := 'Issue254-Core1'.
	packageName2 := 'Issue254-Core2'.
	className := 'Issue254Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].
"setup definitions"
	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addComponentNamed: componentName;
		addPackagesNamed: { packageName1 } toComponentNamed: componentName;
		resolve;
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
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName1 asSymbol.

"move class; add new method"
	projectSetDefinition := RwProjectSetDefinition new.

	projectDefinition2 := (Rowan projectNamed: projectName) asDefinition.
	projectDefinition2
		removePackageNamed: packageName1;
		addPackageNamed: packageName2 toComponentNamed: componentName;
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 
			forPackageNamed: packageName2;
		yourself.

	packageDefinition := projectDefinition2 packageNamed: packageName2.

	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method2'
					protocol: 'accessing'
					source: 'method2 ^2').

	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition addDefinition: projectDefinition2.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	self assert: (audit := project audit) isEmpty.
	loadedClass := Rowan image loadedClassNamed: className.
	loadedClass loadedInstanceMethods includesKey: #method1.

	oldClass := class.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName2.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName2.
	self assert: (class compiledMethodAt: #method2) rowanPackageName = packageName2.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName2 asSymbol.

	self assert: class == oldClass.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 0.
	self assert: registry methodRegistry size = 0.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry methodRegistry size = 2.
]

{ #category : 'tests - issue 254' }
RwMoveTest >> testProjectSet_move_method_between_packages [

	"Use the regular load stack"

	"https://github.com/dalehenrich/Rowan/issues/254"

	| projectName  packageName1 packageName2 packageName3 projectDefinition1 projectDefinition2 
		classDefinition packageDefinition className projectSetDefinition class symDict registry 
		classExtensionDefinition oldClass loadedPackage loadedClass loadedClassExtensions componentName
		project audit |
	projectName := 'Issue254'.
	packageName1 := 'Issue254-Core1'.
	packageName2 := 'Issue254-Extension2'.
	packageName3 := 'Issue254-Extension3'.
	className := 'Issue254Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addComponentNamed: componentName;
		addPackagesNamed: { packageName1 . packageName2 . packageName3} 
			toComponentNamed: componentName;
		resolve;
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

	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'mover'
					protocol: 'accessing'
					source: 'mover ^1').

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.

	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.

	class := Rowan globalNamed: className.
	self assert: (class new perform: #mover) = 1.
	self assert: class rowanPackageName = packageName1.
	self assert: (class categoryOfSelector: #mover) asString = 'accessing'.
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName1.

	loadedPackage := Rowan image loadedPackageNamed: packageName1.
	loadedClass := loadedPackage loadedClasses at: className.
	self assert: (loadedClass loadedInstanceMethods includesKey: #mover).
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: loadedClassExtensions isEmpty.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 3.
	self assert: registry methodRegistry size = 1.

	projectSetDefinition := RwProjectSetDefinition new.
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName1.
	classDefinition := packageDefinition classDefinitions at: classDefinition key.
	classDefinition removeInstanceMethod: #mover.

	classExtensionDefinition := RwClassExtensionDefinition newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'mover'
					protocol: '*', packageName2 asLowercase
					source: 'mover ^1').

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

	projectSetDefinition addDefinition: projectDefinition2.

	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	oldClass := class.
	class := Rowan globalNamed: className.
	self assert: class == oldClass.
	self assert: (class new perform: #mover) = 1.
	self assert: class rowanPackageName = packageName1.
	self assert: (class categoryOfSelector: #mover) asString =  ('*', packageName2 asLowercase).
	self assert: (class compiledMethodAt: #mover) rowanPackageName = packageName2.

	loadedPackage := Rowan image loadedPackageNamed: packageName1.
	loadedClass := loadedPackage loadedClasses at: className.
	self assert: loadedClass instanceMethodDefinitions isEmpty.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: loadedClassExtensions isEmpty.

	loadedPackage := Rowan image loadedPackageNamed: packageName2.
	loadedClass := loadedPackage loadedClasses at: className ifAbsent: [].
	self assert: loadedClass isNil.
	loadedClassExtensions := loadedPackage loadedClassExtensions.
	self assert: ((loadedClassExtensions at: className) loadedInstanceMethods includesKey: #mover).

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: (audit := project audit) isEmpty.
	self assert: registry packageRegistry size = 3.
	self assert: registry methodRegistry size = 1.

]

{ #category : 'tests - issue 254' }
RwMoveTest >> testProjectSet_move_modified_class_structure_between_packages [

	"https://github.com/dalehenrich/Rowan/issues/286"

	"Class structure moved to a new package located in same symbol dictionary (precursor to #testProjectSet_move_modified_class_structure_between_packages_and_symbol_dicts)"

	"Old package structure:
		Issue_Command
			IssueSaveCommand
		IssueCommand
			IssueFileout
			IssueListClasses
	"

	"New packages structure:
		IssueCommand
			IssueFileout
			IssueListClasses
			IssueHelp
			IssueReport
	"


	| projectName  packageName1 packageName2  projectDefinition1 projectDefinition2 packageDefinition 
		className1 className2 className3 className4 className5 className6 className7
		projectSetDefinition class3 class4 class5  oldClass3 oldClass4 oldClass5 componentName |

	projectName := 'Issue'.
	packageName1 := 'Issue-Core1'.
	packageName2 := 'Issue-Core2'.
	className1 := 'Issue_Command'.
	className2 := 'IssueSaveCommand'.
	className3 := 'IssueCommand'.
	className4 := 'IssueFileout'.
	className5 := 'IssueListClasses'.
	className6 := 'IssueHelp'.
	className7 := 'IssueReport'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create old package structure"
	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addComponentNamed: componentName;
		addPackagesNamed: { packageName1 . packageName2 } toComponentNamed: componentName;
		resolve;
		yourself.

	packageDefinition := projectDefinition1 packageNamed: packageName1.

	packageDefinition 
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className1
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className2
			super: className1
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className3
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className4
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className5
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: ''
			pools: #()
			type: 'normal');
		yourself.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"verify"
	class3 := Rowan globalNamed: className3.
	class4 := Rowan globalNamed: className4.
	class5 := Rowan globalNamed: className5.
	self assert: class3 notNil.
	self assert: class4 notNil.
	self assert: class5 notNil.

"create new package structure"
	projectSetDefinition := RwProjectSetDefinition new.

	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.
	projectSetDefinition addDefinition: projectDefinition2.

	packageDefinition := projectDefinition2 packageNamed: packageName1.
	packageDefinition
		removeClassNamed: className1;	"deleted"
		removeClassNamed: className2;	"deleted"
		removeClassNamed: className3;	"moved"
		removeClassNamed: className4;	"moved"
		removeClassNamed: className5;	"moved"
		yourself.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition 
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className3
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className4
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className5
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className6
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className7
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: ''
			pools: #()
			type: 'normal');
		yourself.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"verify"
	oldClass3 := class3.
	oldClass4 := class4.
	oldClass5 := class5.

	class3 := Rowan globalNamed: className3.
	class4 := Rowan globalNamed: className4.
	class5 := Rowan globalNamed: className5.
	self assert: class3 notNil.
	self assert: class4 notNil.
	self assert: class5 notNil.
	self assert: class3 == oldClass3.
	self assert: class4 == oldClass4.
	self assert: class5 == oldClass5.

]

{ #category : 'tests - issue 254' }
RwMoveTest >> testProjectSet_move_modified_class_structure_between_packages_and_symbol_dicts [

	"https://github.com/dalehenrich/Rowan/issues/286"

	"Class structure moved to a new package located in a different symbol dictionary"

	"Old package structure:
		Issue_Command
			IssueSaveCommand
		IssueCommand
			IssueFileout
			IssueListClasses
	"

	"New packages structure:
		IssueCommand
			IssueFileout
			IssueListClasses
			IssueHelp
			IssueReport
	"


	| projectName  packageName1 packageName2  projectDefinition1 projectDefinition2 packageDefinition 
		className1 className2 className3 className4 className5 className6 className7
		projectSetDefinition class3 class4 class5  oldClass3 oldClass4 oldClass5 project audit  
		componentName |

	projectName := 'Issue'.
	packageName1 := 'Issue-Core1'.
	packageName2 := 'Issue-Core2'.
	className1 := 'Issue_Command'.
	className2 := 'IssueSaveCommand'.
	className3 := 'IssueCommand'.
	className4 := 'IssueFileout'.
	className5 := 'IssueListClasses'.
	className6 := 'IssueHelp'.
	className7 := 'IssueReport'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create old package structure"
	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addComponentNamed: componentName;
		addPackagesNamed: { packageName1 . packageName2 } toComponentNamed: componentName;
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 
			forPackageNamed: packageName2;
		resolve;
		yourself.

	packageDefinition := projectDefinition1 packageNamed: packageName1.

	packageDefinition 
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className1
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className2
			super: className1
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className3
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className4
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className5
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: ''
			pools: #()
			type: 'normal');
		yourself.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"verify"
	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.
	class3 := Rowan globalNamed: className3.
	class4 := Rowan globalNamed: className4.
	class5 := Rowan globalNamed: className5.
	self assert: class3 notNil.
	self assert: class4 notNil.
	self assert: class5 notNil.

"create new package structure"
	projectSetDefinition := RwProjectSetDefinition new.

	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.
	projectSetDefinition addDefinition: projectDefinition2.

	packageDefinition := projectDefinition2 packageNamed: packageName1.
	packageDefinition
		removeClassNamed: className1;	"deleted"
		removeClassNamed: className2;	"deleted"
		removeClassNamed: className3;	"moved"
		removeClassNamed: className4;	"moved"
		removeClassNamed: className5;	"moved"
		yourself.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition 
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className3
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className4
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className5
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className6
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className7
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: ''
			pools: #()
			type: 'normal');
		yourself.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"verify"
	self assert: (audit := project audit) isEmpty.
	oldClass3 := class3.
	oldClass4 := class4.
	oldClass5 := class5.

	class3 := Rowan globalNamed: className3.
	class4 := Rowan globalNamed: className4.
	class5 := Rowan globalNamed: className5.
	self assert: class3 notNil.
	self assert: class4 notNil.
	self assert: class5 notNil.
	self assert: class3 == oldClass3.
	self assert: class4 == oldClass4.
	self assert: class5 == oldClass5.

]

{ #category : 'tests - issue 254' }
RwMoveTest >> testProjectSet_move_modified_class_structure_with_new_class_version_between_packages_and_symbol_dicts_286 [

	"https://github.com/dalehenrich/Rowan/issues/286"

	"Class structure moved to a new package located in a different symbol dictionary. The surviving superclass has a new class version"

	"Old package structure:
		Issue_Command
			IssueSaveCommand
		IssueCommand
			IssueFileout
			IssueListClasses
	"

	"New packages structure:
		IssueCommand
			IssueFileout
			IssueListClasses
			IssueHelp
			IssueReport
	"


	| projectName  packageName1 packageName2  projectDefinition1 projectDefinition2 packageDefinition 
		className1 className2 className3 className4 className5 className6 className7
		projectSetDefinition class3 class4 class5  oldClass3 oldClass4 oldClass5 project audit
		componentName |

	projectName := 'Issue'.
	packageName1 := 'Issue-Core1'.
	packageName2 := 'Issue-Core2'.
	className1 := 'Issue_Command'.
	className2 := 'IssueSaveCommand'.
	className3 := 'IssueCommand'.
	className4 := 'IssueFileout'.
	className5 := 'IssueListClasses'.
	className6 := 'IssueHelp'.
	className7 := 'IssueReport'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create old package structure"
	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addComponentNamed: componentName;
		addPackagesNamed: { packageName1 . packageName2 } toComponentNamed: componentName;
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 
			forPackageNamed: packageName2;
		resolve;
		yourself.

	packageDefinition := projectDefinition1 packageNamed: packageName1.

	packageDefinition 
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className1
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className2
			super: className1
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className3
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className4
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className5
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName1
			comment: ''
			pools: #()
			type: 'normal');
		yourself.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"verify"
	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.
	class3 := Rowan globalNamed: className3.
	class4 := Rowan globalNamed: className4.
	class5 := Rowan globalNamed: className5.
	self assert: class3 notNil.
	self assert: class4 notNil.
	self assert: class5 notNil.

"create new package structure"
	projectSetDefinition := RwProjectSetDefinition new.

	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.
	projectSetDefinition addDefinition: projectDefinition2.

	packageDefinition := projectDefinition2 packageNamed: packageName1.
	packageDefinition
		removeClassNamed: className1;	"deleted"
		removeClassNamed: className2;	"deleted"
		removeClassNamed: className3;	"moved"
		removeClassNamed: className4;	"moved"
		removeClassNamed: className5;	"moved"
		yourself.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition 
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className3
			super: 'Object'
			instvars: #(ivar1)
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className4
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className5
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className6
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: ''
			pools: #()
			type: 'normal');
		addClassDefinition: (RwClassDefinition
			newForClassNamed: className7
			super: className3
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: packageName2
			comment: ''
			pools: #()
			type: 'normal');
		yourself.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"verify"
	self assert: (audit := project audit) isEmpty.
	oldClass3 := class3.
	oldClass4 := class4.
	oldClass5 := class5.

	class3 := Rowan globalNamed: className3.
	class4 := Rowan globalNamed: className4.
	class5 := Rowan globalNamed: className5.
	self assert: class3 notNil.
	self assert: class4 notNil.
	self assert: class5 notNil.
	self assert: class3 ~~ oldClass3.
	self assert: class4 ~~ oldClass4.
	self assert: class5 ~~ oldClass5.
]

{ #category : 'tests - issue 254' }
RwMoveTest >> testProjectSet_move_new_class_version_between_packages [
	"Use the regular load stack"

	"https://github.com/dalehenrich/Rowan/issues/254"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition 
		packageDefinition className projectSetDefinition class x newClass symDict registry 
		componentName project audit |
	projectName := 'Issue254'.
	packageName1 := 'Issue254-Core1'.
	packageName2 := 'Issue254-Core2'.
	className := 'Issue254Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addComponentNamed: componentName;
		addPackagesNamed: { packageName1 . packageName2 } toComponentNamed: componentName;
		resolve;
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

	classDefinition 
		instVarNames: (classDefinition instVarNames copy add: 'ivar1'; yourself). "force new class version"

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition addDefinition: projectDefinition2.

	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.

	newClass := Rowan globalNamed: className.
	self assert: (x := newClass rowanPackageName) = packageName2.
	self assert: newClass ~~ class.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry methodRegistry size = 0.
]

{ #category : 'tests - issue 254' }
RwMoveTest >> testProjectSet_move_new_class_version_between_packages_and_symbolDict [
	"Use the regular load stack"

	"https://github.com/dalehenrich/Rowan/issues/254"

	"initial load uses symbol dictionary _1, second load uses symbol dictionary _2"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition 
		packageDefinition className projectSetDefinition class oldClass ar symDict registry project audit
		componentName |
	projectName := 'Issue254'.
	packageName1 := 'Issue254-Core1'.
	packageName2 := 'Issue254-Core2'.
	className := 'Issue254Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addComponentNamed: componentName;
		addPackagesNamed: { packageName1 } toComponentNamed: componentName;
		resolve;
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

";oad"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName1 asSymbol.

	projectSetDefinition := RwProjectSetDefinition new.
	projectDefinition2 := (Rowan projectNamed: projectName) asDefinition.
	projectDefinition2
		removePackageNamed: packageName1;
		addPackageNamed: packageName2 toComponentNamed: componentName;
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 
			forPackageNamed: packageName2;
		yourself.

	classDefinition 
		instVarNames: (classDefinition instVarNames copy add: 'ivar1'; yourself). "force new class version"

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition addClassDefinition: classDefinition.

"load"
	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	self assert: (audit := project audit) isEmpty.
	oldClass := class.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName2.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName2 asSymbol.

	self assert: class ~~ oldClass.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 0.
	self assert: registry methodRegistry size = 0.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry methodRegistry size = 0.
]

{ #category : 'tests - issue 254' }
RwMoveTest >> testProjectSet_move_new_class_version_with_extension_method_between_existing_packages [

	"https://github.com/dalehenrich/Rowan/issues/316"

	| projectName  packageName1 packageName2 packageName3 packageName4 projectDefinition1 
		projectDefinition2 classDefinition packageDefinition className projectSetDefinition class x 
		newClass symDict registry classExtensionDefinition componentName project audit |
	projectName := 'Issue254'.
	packageName1 := 'Issue254-Core1'.
	packageName2 := 'Issue254-Extension2'.
	packageName3 := 'Issue254-Core3'.
	packageName4 := 'Issue254-Extension4'.
	className := 'Issue254Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].
"setup"
	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addComponentNamed: componentName;
		addPackagesNamed: { packageName1 . packageName2 . packageName3 . packageName4 } 
			toComponentNamed: componentName;
		resolve;
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
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	classExtensionDefinition := RwClassExtensionDefinition
		newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #ext1
						protocol: '*', packageName2
						source: 'ext1 ^ 1').

	packageDefinition := projectDefinition1 packageNamed: packageName2.
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.

"validate"
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #ext1) rowanPackageName = packageName2.

"move class with new version and extension method to new packages"
	projectSetDefinition := RwProjectSetDefinition new.
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.

	classDefinition 
		instVarNames: (classDefinition instVarNames copy add: 'ivar1'; yourself). "force new class version"

	packageDefinition := projectDefinition2 packageNamed: packageName1.
	packageDefinition removeClassDefinition: classDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition removeClassExtensionDefinition: classExtensionDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName3.
	packageDefinition addClassDefinition: classDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName4.
	(classExtensionDefinition
		instanceMethodDefinitions at: #ext1)
 						protocol: '*', packageName4.

	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

	projectSetDefinition addDefinition: projectDefinition2.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	self assert: (audit := project audit) isEmpty.
	newClass := Rowan globalNamed: className.
	self assert: (x := newClass rowanPackageName) = packageName3.
	self assert: (newClass compiledMethodAt: #method1) rowanPackageName = packageName3.
	self assert: (x := (newClass compiledMethodAt: #ext1) rowanPackageName) = packageName4.
	self assert: newClass ~~ class.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 4.
	self assert: registry methodRegistry size = 2.
]

{ #category : 'tests - issue 254' }
RwMoveTest >> testProjectSet_move_new_class_version_with_extension_method_between_existing_packages_and_symbolDict [

	"https://github.com/dalehenrich/Rowan/issues/316"

	"initial load uses symbol dictionary _1, second load uses symbol dictionary _2"

	| projectName  packageName1 packageName2 packageName3 packageName4 projectDefinition1 
		projectDefinition2 classDefinition packageDefinition className projectSetDefinition class 
		oldClass ar loadedClass symDict registry classExtensionDefinition project audit componentName |
	projectName := 'Issue254'.
	packageName1 := 'Issue254-Core1'.
	packageName2 := 'Issue254-Extension2'.
	packageName3 := 'Issue254-Core3'.
	packageName4 := 'Issue254-Extension4'.
	className := 'Issue254Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].
"setup"
	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addComponentNamed: componentName;
		addPackagesNamed: { packageName1 . packageName2 . packageName3 . packageName4 } 
			toComponentNamed: componentName;
		resolve;
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
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	classExtensionDefinition := RwClassExtensionDefinition
		newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #ext1
						protocol: '*', packageName2 asLowercase
						source: 'ext1 ^ 1').

	packageDefinition := projectDefinition1 packageNamed: packageName2.
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	loadedClass := Rowan image loadedClassNamed: className.
	loadedClass loadedInstanceMethods includesKey: #method1.

"validation"
	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #ext1) rowanPackageName = packageName2.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName1 asSymbol.

"move class with new version and extension methods"
	projectSetDefinition := RwProjectSetDefinition new.
	projectDefinition2 := (Rowan projectNamed: projectName) asDefinition.
	projectDefinition2
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 
			forPackageNamed: packageName3;
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 
			forPackageNamed: packageName4;
		yourself.

	classDefinition 
		instVarNames: (classDefinition instVarNames copy add: 'ivar1'; yourself). "force new class version"

	packageDefinition := projectDefinition2 packageNamed: packageName1.
	packageDefinition removeClassDefinition: classDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition removeClassExtensionDefinition: classExtensionDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName3.
	packageDefinition addClassDefinition: classDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName4.
	(classExtensionDefinition instanceMethodDefinitions at: #ext1)
		protocol: '*', packageName4 asLowercase.
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

"load"
	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	loadedClass := Rowan image loadedClassNamed: className.
	loadedClass loadedInstanceMethods includesKey: #method1.

"validate"
	self assert: (audit := project audit) isEmpty.
	oldClass := class.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName3.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName3.
	self assert: (class compiledMethodAt: #ext1) rowanPackageName = packageName4.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName2 asSymbol.

	self assert: class ~~ oldClass.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry methodRegistry size = 0.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry methodRegistry size = (2+2). "see https://github.com/GemTalk/Rowan/issues/624"
]

{ #category : 'tests - issue 254' }
RwMoveTest >> testProjectSet_move_new_class_version_with_extension_method_between_packages [

	"https://github.com/dalehenrich/Rowan/issues/316"

	| projectName  packageName1 packageName2 packageName3 packageName4 projectDefinition1 
		projectDefinition2 classDefinition packageDefinition className projectSetDefinition class x 
		newClass symDict registry classExtensionDefinition componentName project audit |
	projectName := 'Issue254'.
	packageName1 := 'Issue254-Core1'.
	packageName2 := 'Issue254-Extension2'.
	packageName3 := 'Issue254-Core3'.
	packageName4 := 'Issue254-Extension4'.
	className := 'Issue254Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].
"setup"
	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addComponentNamed: componentName;
		addPackagesNamed: { packageName1 . packageName2} toComponentNamed: componentName;
		resolve;
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
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	classExtensionDefinition := RwClassExtensionDefinition
		newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #ext1
						protocol: '*', packageName2
						source: 'ext1 ^ 1').

	packageDefinition := projectDefinition1 packageNamed: packageName2.
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #ext1) rowanPackageName = packageName2.

"move class with new version and extension method to new packages"
	projectSetDefinition := RwProjectSetDefinition new.
	projectDefinition2 := (Rowan projectNamed: projectName) asDefinition.
	projectDefinition2
		removePackageNamed: packageName1;
		removePackageNamed: packageName2;
		addPackageNamed: packageName3 toComponentNamed: componentName;
		addPackageNamed: packageName4 toComponentNamed: componentName;
		yourself.

	classDefinition 
		instVarNames: (classDefinition instVarNames copy add: 'ivar1'; yourself). "force new class version"

	packageDefinition := projectDefinition2 packageNamed: packageName3.
	packageDefinition addClassDefinition: classDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName4.
	(classExtensionDefinition
		instanceMethodDefinitions at: #ext1)
 						protocol: '*', packageName4.
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

	projectSetDefinition addDefinition: projectDefinition2.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	self assert: (audit := project audit) isEmpty.
	newClass := Rowan globalNamed: className.
	self assert: (x := newClass rowanPackageName) = packageName3.
	self assert: (newClass compiledMethodAt: #method1) rowanPackageName = packageName3.
	self assert: (x := (newClass compiledMethodAt: #ext1) rowanPackageName) = packageName4.
	self assert: newClass ~~ class.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry methodRegistry size = 2.
]

{ #category : 'tests - issue 254' }
RwMoveTest >> testProjectSet_move_new_class_version_with_extension_method_between_packages_and_symbolDict [

	"https://github.com/dalehenrich/Rowan/issues/316"

	"initial load uses symbol dictionary _1, second load uses symbol dictionary _2"

	| projectName  packageName1 packageName2 packageName3 packageName4 projectDefinition1 
		projectDefinition2 classDefinition packageDefinition className projectSetDefinition class oldClass 
		ar loadedClass symDict registry classExtensionDefinition project audit componentName |
	projectName := 'Issue254'.
	packageName1 := 'Issue254-Core1'.
	packageName2 := 'Issue254-Extension2'.
	packageName3 := 'Issue254-Core3'.
	packageName4 := 'Issue254-Extension4'.
	className := 'Issue254Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].
"setup"
	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addComponentNamed: componentName;
		addPackagesNamed: { packageName1 . packageName2 } toComponentNamed: componentName;
		resolve;
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
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	classExtensionDefinition := RwClassExtensionDefinition
		newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #ext1
						protocol: '*', packageName2 asLowercase
						source: 'ext1 ^ 1').

	packageDefinition := projectDefinition1 packageNamed: packageName2.
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	loadedClass := Rowan image loadedClassNamed: className.
	loadedClass loadedInstanceMethods includesKey: #method1.

"validation"
	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #ext1) rowanPackageName = packageName2.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName1 asSymbol.

"move class with new version and extension methods"
	projectSetDefinition := RwProjectSetDefinition new.
	projectDefinition2 := (Rowan projectNamed: projectName) asDefinition.
	projectDefinition2
		removePackageNamed: packageName1;
		addPackageNamed: packageName3 toComponentNamed: componentName;
		removePackageNamed: packageName2;
		addPackageNamed: packageName4 toComponentNamed: componentName;
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 
			forPackageNamed: packageName3;
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 
			forPackageNamed: packageName4;
		yourself.

	classDefinition 
		instVarNames: (classDefinition instVarNames copy add: 'ivar1'; yourself). "force new class version"

	packageDefinition := projectDefinition2 packageNamed: packageName3.
	packageDefinition addClassDefinition: classDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName4.
	(classExtensionDefinition instanceMethodDefinitions at: #ext1)
		protocol: '*', packageName4 asLowercase.
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

"load"
	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	loadedClass := Rowan image loadedClassNamed: className.
	loadedClass loadedInstanceMethods includesKey: #method1.

"validate"
	self assert: (audit := project audit) isEmpty.
	oldClass := class.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName3.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName3.
	self assert: (class compiledMethodAt: #ext1) rowanPackageName = packageName4.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName2 asSymbol.

	self assert: class ~~ oldClass.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 0.
	self assert: registry methodRegistry size = 0.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry methodRegistry size = (2+2). "see https://github.com/GemTalk/Rowan/issues/624"
]

{ #category : 'tests - issue 254' }
RwMoveTest >> testProjectSet_move_new_class_version_with_method_between_existing_packages [

	"Use the regular load stack"

	"https://github.com/dalehenrich/Rowan/issues/316"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition 
		packageDefinition className projectSetDefinition class x newClass symDict registry 
		componentName project audit |
	projectName := 'Issue254'.
	packageName1 := 'Issue254-Core1'.
	packageName2 := 'Issue254-Core2'.
	className := 'Issue254Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addComponentNamed: componentName;
		addPackagesNamed: { packageName1 . packageName2 } toComponentNamed: componentName;
		resolve;
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
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.

	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName1.

	projectSetDefinition := RwProjectSetDefinition new.
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName1.
	packageDefinition classDefinitions removeKey: classDefinition key.

	classDefinition 
		instVarNames: (classDefinition instVarNames copy add: 'ivar1'; yourself). "force new class version"

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition addDefinition: projectDefinition2.

	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	self assert: (audit := project audit) isEmpty.
	newClass := Rowan globalNamed: className.
	self assert: (x := newClass rowanPackageName) = packageName2.
	self assert: (newClass compiledMethodAt: #method1) rowanPackageName = packageName2.
	self assert: newClass ~~ class.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry methodRegistry size = 1.
]

{ #category : 'tests - issue 254' }
RwMoveTest >> testProjectSet_move_new_class_version_with_method_between_existing_packages_and_symbolDict [
	"Use the regular load stack"

	"https://github.com/dalehenrich/Rowan/issues/316"

	"initial load uses symbol dictionary _1, second load uses symbol dictionary _2"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition 
		packageDefinition className projectSetDefinition class oldClass ar loadedClass symDict registry 
		project audit componentName |
	projectName := 'Issue254'.
	packageName1 := 'Issue254-Core1'.
	packageName2 := 'Issue254-Core2'.
	className := 'Issue254Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addComponentNamed: componentName;
		addPackagesNamed: { packageName1 } toComponentNamed: componentName;
		resolve;
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
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	loadedClass := Rowan image loadedClassNamed: className.
	loadedClass loadedInstanceMethods includesKey: #method1.

"validate"
	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName1 asSymbol.

	projectSetDefinition := RwProjectSetDefinition new.
	projectDefinition2 := (Rowan projectNamed: projectName) asDefinition.
	projectDefinition2
		addPackageNamed: packageName2 toComponentNamed: componentName;
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 
			forPackageNamed: packageName2;
		yourself.

	classDefinition 
		instVarNames: (classDefinition instVarNames copy add: 'ivar1'; yourself). "force new class version"

	packageDefinition := projectDefinition2 packageNamed: packageName1.
	packageDefinition removeClassNamed: classDefinition name.
	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition addClassDefinition: classDefinition.

"load"
	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	loadedClass := Rowan image loadedClassNamed: className.
	loadedClass loadedInstanceMethods includesKey: #method1.

"validate"
	self assert: (audit := project audit) isEmpty.
	oldClass := class.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName2.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName2.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName2 asSymbol.

	self assert: class ~~ oldClass.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry methodRegistry size = 0.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry methodRegistry size = (1+1). "see https://github.com/GemTalk/Rowan/issues/624"
]

{ #category : 'tests - issue 254' }
RwMoveTest >> testProjectSet_move_new_class_version_with_method_between_packages [

	"Use the regular load stack"

	"https://github.com/dalehenrich/Rowan/issues/316"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition 
		packageDefinition className projectSetDefinition class x newClass symDict registry 
		componentName project audit |
	projectName := 'Issue254'.
	packageName1 := 'Issue254-Core1'.
	packageName2 := 'Issue254-Core2'.
	className := 'Issue254Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addComponentNamed: componentName;
		addPackagesNamed: { packageName1 . packageName2 } toComponentNamed: componentName;
		resolve;
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
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.

	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName1.

	projectSetDefinition := RwProjectSetDefinition new.
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.

	projectDefinition2 removePackageNamed: packageName1.

	classDefinition 
		instVarNames: (classDefinition instVarNames copy add: 'ivar1'; yourself). "force new class version"

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition addDefinition: projectDefinition2.

	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	self assert: (audit := project audit) isEmpty.
	newClass := Rowan globalNamed: className.
	self assert: (x := newClass rowanPackageName) = packageName2.
	self assert: (newClass compiledMethodAt: #method1) rowanPackageName = packageName2.
	self assert: newClass ~~ class.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry methodRegistry size = 1.
]

{ #category : 'tests - issue 254' }
RwMoveTest >> testProjectSet_move_new_class_version_with_method_between_packages_and_symbolDict [
	"Use the regular load stack"

	"https://github.com/dalehenrich/Rowan/issues/316"

	"initial load uses symbol dictionary _1, second load uses symbol dictionary _2"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition 
		packageDefinition className projectSetDefinition class oldClass ar loadedClass symDict registry 
		project audit componentName |
	projectName := 'Issue254'.
	packageName1 := 'Issue254-Core1'.
	packageName2 := 'Issue254-Core2'.
	className := 'Issue254Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addComponentNamed: componentName;
		addPackagesNamed: { packageName1 } toComponentNamed: componentName;
		resolve;
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
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

"load"	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	loadedClass := Rowan image loadedClassNamed: className.
	loadedClass loadedInstanceMethods includesKey: #method1.

"validate"
	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName1 asSymbol.

	projectSetDefinition := RwProjectSetDefinition new.
	projectDefinition2 := (Rowan projectNamed: projectName) asDefinition.
	projectDefinition2
		removePackageNamed: packageName1;
		addPackageNamed: packageName2 toComponentNamed: componentName;
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 
			forPackageNamed: packageName2;
		yourself.

	classDefinition 
		instVarNames: (classDefinition instVarNames copy add: 'ivar1'; yourself). "force new class version"

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition addClassDefinition: classDefinition.

"load"
	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	loadedClass := Rowan image loadedClassNamed: className.
	loadedClass loadedInstanceMethods includesKey: #method1.

"validate"
	self assert: (audit := project audit) isEmpty.
	oldClass := class.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName2.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName2.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName2 asSymbol.

	self assert: class ~~ oldClass.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 0.
	self assert: registry methodRegistry size = 0.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry methodRegistry size = (1+1). "see https://github.com/GemTalk/Rowan/issues/624"
]

{ #category : 'tests - issue 254' }
RwMoveTest >> testProjectSet_move_new_class_version_with_new_method_between_packages [

	"Use the regular load stack"

	"https://github.com/dalehenrich/Rowan/issues/316"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition 
		packageDefinition className projectSetDefinition class x newClass symDict registry 
		componentName project audit |
	projectName := 'Issue254'.
	packageName1 := 'Issue254-Core1'.
	packageName2 := 'Issue254-Core2'.
	className := 'Issue254Class'.

"create project"
	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addComponentNamed: componentName;
		addPackagesNamed: { packageName1 . packageName2 } toComponentNamed: componentName;
		resolve;
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
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.

"load"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName1.

"create new version and add new method"
	projectSetDefinition := RwProjectSetDefinition new.
	projectDefinition2 := (Rowan image loadedProjectNamed: projectName) asDefinition.

	packageDefinition := projectDefinition2 packageNamed: packageName1.
	packageDefinition classDefinitions removeKey: classDefinition key.

	classDefinition 
		instVarNames: (classDefinition instVarNames copy add: 'ivar1'; yourself). "force new class version"
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method2'
					protocol: 'accessing'
					source: 'method2 ^2').

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition addClassDefinition: classDefinition.

"load"
	projectSetDefinition addDefinition: projectDefinition2.

"validate"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	self assert: (audit := project audit) isEmpty.
	newClass := Rowan globalNamed: className.
	self assert: (x := newClass rowanPackageName) = packageName2.
	self assert: (newClass compiledMethodAt: #method1) rowanPackageName = packageName2.
	self assert: newClass ~~ class.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry methodRegistry size = 2.
]

{ #category : 'tests - issue 254' }
RwMoveTest >> testProjectSet_move_new_class_version_with_new_method_between_packages_and_symbolDict [
	"Use the regular load stack"

	"https://github.com/dalehenrich/Rowan/issues/316"

	"initial load uses symbol dictionary _1, second load uses symbol dictionary _2"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition 
		packageDefinition className projectSetDefinition class oldClass ar loadedClass symDict registry 
		project audit componentName |
	projectName := 'Issue254'.
	packageName1 := 'Issue254-Core1'.
	packageName2 := 'Issue254-Core2'.
	className := 'Issue254Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addComponentNamed: componentName;
		addPackagesNamed: { packageName1 } toComponentNamed: componentName;
		resolve;
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
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method1'
					protocol: 'accessing'
					source: 'method1 ^1').

	packageDefinition := projectDefinition1 packageNamed: packageName1.
	packageDefinition addClassDefinition: classDefinition.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	loadedClass := Rowan image loadedClassNamed: className.
	loadedClass loadedInstanceMethods includesKey: #method1.

"validate"
	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName1.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName1 asSymbol.

	projectSetDefinition := RwProjectSetDefinition new.

	projectDefinition2 := (Rowan projectNamed: projectName) asDefinition.
	projectDefinition2
		removePackageNamed: packageName1;
		addPackageNamed: packageName2 toComponentNamed: componentName;
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 
			forPackageNamed: packageName2;
		yourself.

	classDefinition 
		instVarNames: (classDefinition instVarNames copy add: 'ivar1'; yourself). "force new class version"

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	classDefinition
		addInstanceMethodDefinition:
			(RwMethodDefinition
					newForSelector: #'method2'
					protocol: 'accessing'
					source: 'method2 ^2').

	packageDefinition addClassDefinition: classDefinition.

"load"
	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	loadedClass := Rowan image loadedClassNamed: className.
	loadedClass loadedInstanceMethods includesKey: #method1.

"validate"
	self assert: (audit := project audit) isEmpty.
	oldClass := class.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName2.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName2.
	self assert: (class compiledMethodAt: #method2) rowanPackageName = packageName2.
	ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
	self assert: ar size = 1.
	self assert: (ar first at: 1) name = self _symbolDictionaryName2 asSymbol.

	self assert: class ~~ oldClass.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 0.
	self assert: registry methodRegistry size = 0.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry methodRegistry size = (2+1). "see https://github.com/GemTalk/Rowan/issues/624"
]

{ #category : 'tests - issue 254' }
RwMoveTest >> testProjectSet_move_packages_between_symbolDict [

	"https://github.com/dalehenrich/Rowan/issues/316"

	"initial load uses symbol dictionary _1, second load uses symbol dictionary _2"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 projectSetDefinition
		symDict registry x project audit componentName |
	projectName := 'Issue254'.
	packageName1 := 'Issue254-Core1'.
	packageName2 := 'Issue254-Core2'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].
"create packages"
	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addComponentNamed: componentName;
		addPackagesNamed: { packageName1 . packageName2 } toComponentNamed: componentName;
		resolve;
		yourself.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition1.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry methodRegistry size = 0.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	self assert: symDict isNil.

"move package to alternate symbol dict"
	projectSetDefinition := RwProjectSetDefinition new.

	projectDefinition2 := (Rowan projectNamed: projectName) asDefinition.
	projectDefinition2
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 
			forPackageNamed: packageName2;
		yourself.

"load"
	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	self assert: (audit := project audit) isEmpty.
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: (x := registry packageRegistry size) = 1.
	self assert: registry methodRegistry size = 0.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry methodRegistry size = 0.
]

{ #category : 'tests - issue 254' }
RwMoveTest >> testProjectSet_rename_package [

	"Use the regular load stack"

	"https://github.com/dalehenrich/Rowan/issues/254"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition 
		packageDefinition className projectSetDefinition class symDict registry componentName
		project audit |
	projectName := 'Issue254'.
	packageName1 := 'Issue1254-Core'.
	packageName2 := 'Issue254-RenamedCore'.
	className := 'Issue254Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addComponentNamed: componentName;
		addPackagesNamed: { packageName1 } toComponentNamed: componentName;
		resolve;
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
	projectDefinition2 := (Rowan projectNamed: projectName) asDefinition.
	projectDefinition2
		removePackageNamed: packageName1;
		addPackageNamed: packageName2 toComponentNamed: componentName;
		yourself.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
	packageDefinition addClassDefinition: classDefinition.
	projectSetDefinition addDefinition: projectDefinition2.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName2.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.
	self assert: registry packageRegistry size = 1.
	self assert: registry methodRegistry size = 0.

]

{ #category : 'tests - issue 254' }
RwMoveTest >> testProjectSet_rename_package_add_change_remove_method [

	"Use the regular load stack"

	"https://github.com/dalehenrich/Rowan/issues/254"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition 
		packageDefinition className projectSetDefinition class symDict registry x componentName
		project audit |
	projectName := 'Issue254'.
	packageName1 := 'Issue1254-Core'.
	packageName2 := 'Issue254-RenamedCore'.
	className := 'Issue254Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].


	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1;
		addComponentNamed: componentName;
		addPackagesNamed: { packageName1 } toComponentNamed: componentName;
		resolve;
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

	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.

	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #method2) rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #method3) rowanPackageName = packageName1.
	self assert: (class new perform: #method1) = 1.
	self assert: (class new perform: #method2) = 2.
	self assert: (class new perform: #method3) = 3.

	projectSetDefinition := RwProjectSetDefinition new.
	projectDefinition2 := (Rowan projectNamed: projectName) asDefinition.
	projectDefinition2
		removePackageNamed: packageName1;
		addPackageNamed: packageName2 toComponentNamed: componentName;
		yourself.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
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

	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName2.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName2.
	self assert: (class compiledMethodAt: #method3) rowanPackageName = packageName2.
	self assert: (class compiledMethodAt: #method4) rowanPackageName = packageName2.
	self assert: (class new perform: #method1) = 1.
	self should: [ (class new perform: #method2) = 2 ] raise: MessageNotUnderstood.
	self assert: (x := class new perform: #method3) = 5.
	self assert: (class new perform: #method4) = 4.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: (audit := project audit) isEmpty.
	self assert: registry packageRegistry size = 1.
	self assert: registry methodRegistry size = 3.

]

{ #category : 'tests - issue 254' }
RwMoveTest >> testProjectSet_rename_package_and_symbolDict_add_change_remove_method [

	"Use the regular load stack"

	"https://github.com/dalehenrich/Rowan/issues/254"

	| projectName  packageName1 packageName2 projectDefinition1 projectDefinition2 classDefinition 
		packageDefinition className projectSetDefinition class symDict registry x componentName
		project audit |
	projectName := 'Issue254'.
	packageName1 := 'Issue1254-Core'.
	packageName2 := 'Issue254-RenamedCore'.
	className := 'Issue254Class'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	componentName := 'Core'.
	projectDefinition1 := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		addComponentNamed: componentName;
		addPackagesNamed: { packageName1 } toComponentNamed: componentName;
		gemstoneSetSymbolDictName: self _symbolDictionaryName1
			forPackageNamed: packageName1;
		resolve;
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

	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.

	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #method2) rowanPackageName = packageName1.
	self assert: (class compiledMethodAt: #method3) rowanPackageName = packageName1.
	self assert: (class new perform: #method1) = 1.
	self assert: (class new perform: #method2) = 2.
	self assert: (class new perform: #method3) = 3.

	projectSetDefinition := RwProjectSetDefinition new.
	projectDefinition2 := (Rowan projectNamed: projectName) asDefinition.
	projectDefinition2
		removePackageNamed: packageName1;
		addPackageNamed: packageName2 toComponentNamed: componentName;
		gemstoneSetSymbolDictName: self _symbolDictionaryName2 
			forPackageNamed: packageName2;
		yourself.

	packageDefinition := projectDefinition2 packageNamed: packageName2.
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

	self assert: (audit := project audit) isEmpty.
	class := Rowan globalNamed: className.
	self assert: class rowanPackageName = packageName2.
	self assert: (class compiledMethodAt: #method1) rowanPackageName = packageName2.
	self assert: (class compiledMethodAt: #method3) rowanPackageName = packageName2.
	self assert: (class compiledMethodAt: #method4) rowanPackageName = packageName2.
	self assert: (class new perform: #method1) = 1.
	self should: [ (class new perform: #method2) = 2 ] raise: MessageNotUnderstood.
	self assert: (x := class new perform: #method3) = 5.
	self assert: (class new perform: #method4) = 4.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 0.
	self assert: registry methodRegistry size = 0.

	symDict := Rowan globalNamed: self _symbolDictionaryName2.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry methodRegistry size = 3.
]
