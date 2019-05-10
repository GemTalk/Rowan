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

{ #category : 'client commands' }
RowanPackageService >> classHierarchy [
	| superclassChains levels services hierarchies theClasses toExpand |
	self update. 
	theClasses := classes collect:[:classService | classService theClass].
	superclassChains := self superclassChainsFor: theClasses. 
	hierarchies := self extendHierarchies: superclassChains. 
	levels := self hierarchiesByLevel: hierarchies.
	services := Dictionary new. 
	toExpand := Set new. 
	self services: services from: levels expand: toExpand.
	hierarchyServices := services reject:[:array | array isEmpty].
	hierarchyServices copy keysAndValuesDo:[:key :value | 
		hierarchyServices at: key put: (value asSet asSortedCollection:[:x :y | x name < y name]) asArray].
	RowanCommandResult addResult: self.
]

{ #category : 'client commands' }
RowanPackageService >> compileClass: definitionString [

	|  anonymousMethod |
	anonymousMethod := definitionString _compileInContext: nil 
       symbolList: GsSession currentSession symbolList.
	UserGlobals at: #jadeiteCompileClassMethod put: anonymousMethod.
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

{ #category : 'commands support' }
RowanPackageService >> extendHierarchies: hierarchies [
	
	"extend the hierarchies by one level
	of subclasses"

	| extendedHierarchies |
	extendedHierarchies := Array new. 
	hierarchies do:[:hierarchy |
		| theClass |
		theClass := hierarchy last. 
		theClass subclasses isEmpty 
			ifTrue:[extendedHierarchies add: hierarchy]
			ifFalse:[
				theClass subclasses do:[:sub |
					extendedHierarchies add: (hierarchy copy add: sub; yourself)
				]]].
	^extendedHierarchies
]

{ #category : 'rowan' }
RowanPackageService >> genericClassCreationTemplate [

	^self browserTool classCreationTemplateForSubclassOf: 'Object' category: name packageName: nil

]

{ #category : 'comparing' }
RowanPackageService >> hash [
	^self name hash
]

{ #category : 'commands support' }
RowanPackageService >> hierarchiesByLevel: hierarchies [

	"Return dictionary of classes by level. 
	Example: 
		hierarchies - #(#(Object Collection Array) #(Object AbstractException Exception))
	Return: 
		#(#nil->#(Object) Object->#(Collection AbstractException) Collection->#(Array) AbstractException->#(Exception)
	"
	| levels |
	levels := hierarchies inject: Dictionary new into:[:dict :chain | 
		1 to: chain size do: [:index | 
			| cls theSuper classSet |
			cls := chain at: index.
			classSet := dict at: cls ifAbsentPut: [Array new].
			index = 1 
		ifTrue:[
			classSet := dict at: #'nil' ifAbsentPut: [Array new]. 
			((dict at: #'nil') includes: cls) ifFalse:[(dict at: #'nil') add: cls].
		]
		ifFalse:[
				theSuper := chain at: index - 1.
				((dict at: theSuper) includes: cls) ifFalse:[(dict at: theSuper) add: cls]
				]].
			dict].
	^levels
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

{ #category : 'client commands' }
RowanPackageService >> recompileMethodsAfterClassCompilation [

	"compileClass: must be run first"
	| theClass classService packageService |

	theClass := [(UserGlobals at: #jadeiteCompileClassMethod) _executeInContext: nil] 
		ensure: [UserGlobals at: #jadeiteCompileClassMethod put: nil].
	classService := RowanClassService new name: theClass name. 
	classService update. 
	classService isNewClass: true.
	classService packageName = name 
		ifTrue:[
			self selectedClass: classService
		]
		ifFalse:[
			packageService := RowanPackageService forPackageNamed: classService packageName. 
			packageService update. 
			packageService selectedClass: classService].
	RowanCommandResult addResult: classService.
]

{ #category : 'client commands' }
RowanPackageService >> removeClass: classService [

	self removeClassNamed: classService name. 
	self update.
	self setDefaultTemplate.
	classService updateType: #removedClass:.
	RowanCommandResult addResult: classService
]

{ #category : 'client commands' }
RowanPackageService >> removeClassNamed: className [

	self browserTool removeClassNamed: className.

]

{ #category : 'rowan' }
RowanPackageService >> rowanDirty [

	^(RwPackage newNamed: name) isDirty

]

{ #category : 'rowan' }
RowanPackageService >> rowanProjectName [

	^projectName
]

{ #category : 'other' }
RowanPackageService >> selectedClass [
	
	^selectedClass
]

{ #category : 'Accessing' }
RowanPackageService >> selectedClass: classService [
	selectedClass := classService.
	classService selectedPackageServices: (Array with: self)
]

{ #category : 'perform' }
RowanPackageService >> servicePerform: symbol withArguments: collection [
	| wasClean |
	wasClean := self arePackageAndProjectClean.
	super servicePerform: symbol withArguments: collection.
	self update. 
	wasClean ifTrue:[
		self updateProject]
]

{ #category : 'commands support' }
RowanPackageService >> services: services from: levels expand: toExpand [

	(classes collect:[:svc | svc theClass]) do:[:aClass |
		toExpand addAll: aClass allSuperclasses].
	levels keysAndValuesDo: [:key :value |
		| newKey service  | 
		newKey := key = #'nil' ifTrue:[#'nil'] ifFalse:[
			service := (RowanClassService new classServiceFromOop: key asOop).
			(toExpand includes: service theClass) ifTrue:[service expand: true] ifFalse:[service expand: false].
			service].
		services at: newKey put: (value collect:[:cls | 
				service := (RowanClassService new classServiceFromOop: cls asOop).
				(toExpand includes: service theClass) ifTrue:[service expand: true] ifFalse:[service expand: false].
				service
				])]
]

{ #category : 'client commands' }
RowanPackageService >> setDefaultTemplate [

	defaultTemplate := self genericClassCreationTemplate.
]

{ #category : 'commands support' }
RowanPackageService >> superclassChainsFor: behaviors [

	^behaviors collect:[:behavior | | supers |
			supers := behavior allSuperclasses. 
			supers add: behavior. 
			supers].
]

{ #category : 'client commands' }
RowanPackageService >> testClasses [

	testClasses := Set new. 
	TestCase allSubclasses do:[:testSubclass |
		testSubclass isAbstract ifFalse:[
			testSubclass suite tests do:[:testClassInstance |
				| implementingClass |
				implementingClass := testClassInstance class whichClassIncludesSelector: testClassInstance selector. 
				(implementingClass compiledMethodAt: testClassInstance selector) rowanPackageName = name ifTrue:[
					| classService |
					classService := RowanClassService basicForClassNamed: testSubclass name.
					testClasses detect:[:testClassService | testClassService name = classService name] ifNone:[
						testClasses add: classService]]]]]. 
	updateType := #testClasses:. 
	testClasses := testClasses asArray. 
	RowanCommandResult addResult: self.
]

{ #category : 'updates' }
RowanPackageService >> update [
	Rowan image loadedPackageNamed: name ifAbsent: [^self]. 
	classes := (self loadedClassNames keys collect:[:string | RowanClassService minimalForClassNamed: string]) asArray.
	classes addAll: (self loadedClassExtensions keys collect:[:string | 
		| classService | 
		classService := (RowanClassService minimalForClassNamed: string) 
			isExtension: true.
		(Rowan image loadedClassForClass: classService theClass ifAbsent:[]) 
			ifNotNil:[:cls | classService definedPackageName: cls packageName].
		classService]).
	classes do: [:clsService | clsService packageName: self name]. 
	self isDirty. 
	projectName := (Rowan image loadedPackageNamed: name) projectName.
	RowanCommandResult addResult: self
]

{ #category : 'updates' }
RowanPackageService >> updateInternalService: updatedService [

	"when sending services back to the client,
	verify any services held by this object are 
	updated. Services know what internal services
	they contain." 

	1 to: classes size do:[:index |
		| classesService |
		classesService := classes at: index. 
		classesService = updatedService ifTrue:[
			classes at: index put: updatedService
		]].
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

{ #category : 'testing' }
RowanPackageService >> wasDeleted [

	^(Rowan image loadedPackageNamed: name
			ifAbsent: []) isNil
]
