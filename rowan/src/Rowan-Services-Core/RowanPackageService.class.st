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
  ^ self rowanDirty not and: [ self projectIsDirty not ]
]

{ #category : 'rowan' }
RowanPackageService >> changes [
   "diffForPackageName: not implemented yet"
 
 self error: 'this message is no longer supported'
]

{ #category : 'other' }
RowanPackageService >> classes: collection [

	classes := collection
]

{ #category : 'client commands' }
RowanPackageService >> classHierarchy [
	| theClasses |
	self update. 
	theClasses := classes collect:[:classService | classService theClass].
	hierarchyServices := self classHierarchy: theClasses. 
	RowanCommandResult addResult: self.
]

{ #category : 'commands support' }
RowanPackageService >> classHierarchy: theClasses [
  hierarchyServices := super classHierarchy: theClasses.
  ^ hierarchyServices
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

{ #category : 'testing' }
RowanPackageService >> isPackageService [

	^true
]

{ #category : 'Accessing' }
RowanPackageService >> jadeite_testClasses [

	"for testing" 
	^testClasses
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
  projectName ifNil: [ self updateProjectName ].
  ^ (RowanProjectService new name: projectName) rowanDirty
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
RowanPackageService >> removeClass: classService [
  self removeClassNamed: classService name.
  self setDefaultTemplate.
  classService updateType: #'removedClass:'.
  classService wasRemoved: true.
  RowanCommandResult addResult: classService.
  RowanBrowserService new packagesWithTests	"sunit browser might need updated"
]

{ #category : 'commands support' }
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
  self isUpdatingButFoundToBeDeleted
    ifTrue: [ ^ self handleDeletedService ].
  wasClean := self arePackageAndProjectClean.
  super servicePerform: symbol withArguments: collection.
  self update.
  wasClean
    ifTrue: [ self updateProject ]
]

{ #category : 'commands support' }
RowanPackageService >> services: services from: levels expand: toExpand [
  ^ self
    services: services
    from: levels
    expand: toExpand
    classes: (classes collect: [ :classService | classService theClass ])
]

{ #category : 'client commands' }
RowanPackageService >> setDefaultTemplate [

	defaultTemplate := self genericClassCreationTemplate.
]

{ #category : 'client commands' }
RowanPackageService >> testClasses [
  organizer := ClassOrganizer new.
  testClasses := Set new.
  self loadedClasses
    valuesDo: [ :loadedClass | 
      | cls |
      cls := loadedClass handle.
      (cls inheritsFrom: TestCase)
        ifTrue: [ 
          cls isAbstract
            ifFalse: [ 
              | classService |
              classService := RowanClassService basicForClassNamed: cls name.
              testClasses add: classService ] ] ].
  self loadedClassExtensions
    valuesDo: [ :loadedClass | 
      | cls |
      cls := loadedClass handle.
      (cls inheritsFrom: TestCase)
        ifTrue: [ 
          cls isAbstract
            ifFalse: [ 
              | classService |
              classService := RowanClassService basicForClassNamed: cls name.
              testClasses add: classService ] ] ].
  updateType := #'testClasses:browser:'.
  testClasses := testClasses asArray.
  testClasses do: [ :classService | classService update ].
  RowanCommandResult addResult: self
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
