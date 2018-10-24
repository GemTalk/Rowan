Class {
	#name : 'RowanPackageService',
	#superclass : 'RowanService',
	#instVars : [
		'projectDefinition',
		'packageName',
		'name',
		'isDirty',
		'classes',
		'defaultTemplate',
		'projectName',
		'testClasses',
		'hierarchyServices',
		'selectedClass'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'instance creation' }
RowanPackageService class >> forPackageNamed: aName [

	| inst |
	inst := self new.  
	inst name: aName.
	aName isNil ifFalse:[
		inst isDirty. "lazy initialize"].
	inst setDefaultTemplate. 
	inst updateProjectName.
	^inst

]

{ #category : 'comparing' }
RowanPackageService >> = packageService [
	(packageService isKindOf: RowanPackageService) ifFalse:[^false].
	^name = packageService name
]

{ #category : 'testing' }
RowanPackageService >> arePackageAndProjectClean [

	^self rowanDirty not and:[self projectIsDirty not]
]

{ #category : 'rowan' }
RowanPackageService >> changes [
   "diffForPackageName: not implemented yet"
 
   ^ (Rowan packageTools diff diffForPackageName: name) asString

]

{ #category : 'commands' }
RowanPackageService >> classHierarchy [
	| superclassChains levels services |
	self update. 
	superclassChains := classes collect:[:classService | | supers | 
			supers := classService classFromName allSuperclasses. 
			supers add: classService classFromName. 
			supers].
	levels := superclassChains inject: Dictionary new into:[:dict :chain | 
		1 to: chain size do: [:index | 
			| cls super classSet |
			cls := chain at: index.
			classSet := dict at: cls ifAbsentPut: [Array new].
			index = 1 
		ifTrue:[
			classSet := dict at: #'nil' ifAbsentPut: [Array new]. 
			(dict at: #'nil') add: cls.
		]
		ifFalse:[
				super := chain at: index - 1.
				(dict at: super) add: cls
				]].
			dict].
	services := Dictionary new. 
	levels keysAndValuesDo: [:key :value |
		| newKey | 
		newKey := key = #'nil' ifTrue:[#'nil'] ifFalse:[(RowanClassService minimalForClassNamed: key name)].
		services at: newKey put: (value collect:[:cls | (RowanClassService minimalForClassNamed: cls name)])].
	hierarchyServices := services reject:[:array | array isEmpty].
	hierarchyServices copy keysAndValuesDo:[:key :value | 
		hierarchyServices at: key put: (value asSet asSortedCollection:[:x :y | x name < y name]) asArray].
	RowanCommandResult addResult: self.
]

{ #category : 'commands' }
RowanPackageService >> compileClass: definitionString [

	| theClass classService packageService |
	theClass := definitionString evaluate.
	classService := RowanClassService forClassNamed: theClass name. 
	RowanCommandResult addResult: classService.
	packageService := RowanPackageService forPackageNamed: classService packageName. 
	packageService update. 
	packageService selectedClass: classService.
	classService selectedPackageServices: (Array with: packageService)
]

{ #category : 'rowan' }
RowanPackageService >> createPackage [
	| projectService default |
	default := RowanProjectService defaultProjectName.
	projectService := RowanProjectService new.
	projectService createProjectNamed: default.  
	(Rowan packageNames includes: name) ifFalse:[
		self browserTool addPackageNamed: name toProjectNamed: default].

]

{ #category : 'rowan' }
RowanPackageService >> createPackageNamed: aString inProject: projName [
	| projectService | 
	name := aString.
	projectService := RowanProjectService new.
	projectDefinition := projectService createProjectNamed: projName.  
	projectDefinition addPackageNamed: name.
	self projectTools load loadProjectDefinition: projectDefinition.
]

{ #category : 'Updating' }
RowanPackageService >> defaultTemplate: newValue [
	defaultTemplate := newValue
]

{ #category : 'rowan' }
RowanPackageService >> definition [

	^(Rowan image loadedPackageNamed: name) asDefinition

]

{ #category : 'rowan' }
RowanPackageService >> deletePackage [

	self browserTool removePackageNamed: name.

]

{ #category : 'rowan' }
RowanPackageService >> genericClassCreationTemplate [

	^self browserTool classCreationTemplateForSubclassOf: 'Object' category: name packageName: nil

]

{ #category : 'comparing' }
RowanPackageService >> hash [
	^self name hash
]

{ #category : 'Accessing' }
RowanPackageService >> hierarchyServices [

	^hierarchyServices
]

{ #category : 'rowan' }
RowanPackageService >> isDirty [

	^isDirty := self rowanDirty

]

{ #category : 'Updating' }
RowanPackageService >> isDirty: boolean [

	isDirty := boolean
]

{ #category : 'rowan' }
RowanPackageService >> loadedClasses [

	| loadedPackage |
	loadedPackage := Rowan image loadedPackageNamed: name ifAbsent:[^KeyValueDictionary new].
	^loadedPackage loadedClasses
]

{ #category : 'rowan' }
RowanPackageService >> loadedClassExtensions [

	| loadedPackage |
	loadedPackage := Rowan image loadedPackageNamed: name ifAbsent:[^KeyValueDictionary new].
	^loadedPackage loadedClassExtensions
]

{ #category : 'rowan' }
RowanPackageService >> loadedClassHandles [
	| loadedClasses |
	loadedClasses := self loadedClasses collect:[:loadedClass | loadedClass handle].
	loadedClasses addAll: (self loadedClassExtensions collect:[:extension | extension handle]).
	^loadedClasses

]

{ #category : 'rowan' }
RowanPackageService >> loadedClassNames [

	^self loadedClasses collect:[:loadedClass | loadedClass name]

]

{ #category : 'Accessing' }
RowanPackageService >> name [
	^name

]

{ #category : 'Updating' }
RowanPackageService >> name: newValue [
	name := newValue

]

{ #category : 'Accessing' }
RowanPackageService >> packageName [
	^name

]

{ #category : 'Updating' }
RowanPackageService >> packageName: newValue [
	name := newValue

]

{ #category : 'printing' }
RowanPackageService >> printOn: aStream [

	super printOn: aStream. 
	aStream nextPut: $:. 
	aStream nextPutAll: (name ifNil: [nil printString])
]

{ #category : 'Accessing' }
RowanPackageService >> projectDefinition [
	^projectDefinition

]

{ #category : 'Updating' }
RowanPackageService >> projectDefinition: newValue [
	projectDefinition := newValue

]

{ #category : 'testing' }
RowanPackageService >> projectIsDirty [

	^(RowanProjectService new name: projectName) rowanDirty
]

{ #category : 'Accessing' }
RowanPackageService >> projectName [
	^projectName
]

{ #category : 'Updating' }
RowanPackageService >> projectName: newValue [
	projectName := newValue
]

{ #category : 'commands' }
RowanPackageService >> removeClass: classService [

	self removeClassNamed: classService name. 
	self update.
	classService updateType: #removedClass:.
	RowanCommandResult addResult: classService
]

{ #category : 'commands' }
RowanPackageService >> removeClassNamed: className [

	self browserTool removeClassNamed: className.

]

{ #category : 'rowan' }
RowanPackageService >> rowanDirty [

	^(RwPackage newNamed: name) isDirty

]

{ #category : 'other' }
RowanPackageService >> selectedClass [
	
	^selectedClass
]

{ #category : 'Accessing' }
RowanPackageService >> selectedClass: classService [

	selectedClass := classService
]

{ #category : 'perform' }
RowanPackageService >> servicePerform: symbol withArguments: collection [
	| wasClean |
	wasClean := self arePackageAndProjectClean.
	super servicePerform: symbol withArguments: collection.
	wasClean ifTrue:[
		self update. 
		self updateProject]
]

{ #category : 'commands' }
RowanPackageService >> setDefaultTemplate [

	defaultTemplate := self genericClassCreationTemplate.
]

{ #category : 'commands' }
RowanPackageService >> testClasses [

	testClasses := Set new. 
	TestCase allSubclasses do:[:testSubclass |
		testSubclass selectors do:[:selector |
			| packageName |
			(selector size >= 4 and:[
			(selector copyFrom: 1 to: 4) asString = 'test']) ifTrue:[
				packageName := (testSubclass compiledMethodAt: selector) rowanPackageName. 
				packageName = name ifTrue:[
					testClasses add: (RowanClassService basicForClassNamed: testSubclass name)]]]]. 
	updateType := #testClasses:. 
	testClasses := testClasses asArray. 
	RowanCommandResult addResult: self.
]

{ #category : 'updates' }
RowanPackageService >> update [
	Rowan image loadedPackageNamed: name ifAbsent: [^self].
	classes := (self loadedClassNames keys collect:[:string | RowanClassService basicForClassNamed: string]) asArray.
	classes addAll: (self loadedClassExtensions keys collect:[:string | 
		| classService | 
		classService := (RowanClassService basicForClassNamed: string) 
			isExtension: true.
		(Rowan image loadedClassForClass: classService classFromName ifAbsent:[]) 
			ifNotNil:[:cls | classService definedPackageName: cls packageName].
		classService]).
	classes do: [:clsService | clsService packageName: self name]. 
	self isDirty. 
	projectName := (Rowan image loadedPackageNamed: name) projectName.
	RowanCommandResult addResult: self
]

{ #category : 'updates' }
RowanPackageService >> updateProject [
	| projectService |

	projectService := RowanProjectService new name: projectName. 
	projectService update.
	RowanCommandResult addResult: projectService.
]

{ #category : 'updates' }
RowanPackageService >> updateProjectName [

	projectName := (Rowan image loadedPackageNamed: name) projectName.
]
