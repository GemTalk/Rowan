Class {
	#name : 'RowanPackageService',
	#superclass : 'RowanService',
	#instVars : [
		'projectDefinition',
		'packageName',
		'name',
		'isDirty',
		'classes',
		'defaultTemplate'
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
	^inst

]

{ #category : 'rowan' }
RowanPackageService >> changes [
   "diffForPackageName: not implemented yet"
 
   ^ (Rowan packageTools diff diffForPackageName: name) asString

]

{ #category : 'commands' }
RowanPackageService >> compileClass: definitionString [

	| theClass classService packageService |
	theClass := definitionString evaluate.
	classService := RowanClassService forClassNamed: theClass name. 
	RowanCommandResult addResult: classService.
	packageService := RowanPackageService forPackageNamed: classService packageName. 
	packageService update.
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
RowanPackageService >> createPackageNamed: aString inProject: projectName [
	| projectService | 
	name := aString.
	projectService := RowanProjectService new.
	projectDefinition := projectService createProjectNamed: projectName.  
	projectDefinition addPackageNamed: name.
	self projectTools load loadProjectDefinition: projectDefinition.

]

{ #category : 'examples' }
RowanPackageService >> createSamplePackage [

	"assume that the sample project & symbol dictionary 
	were already removed"

	| projectService |
	projectService := RowanProjectService new.
	projectDefinition := projectService createSampleProject.  
	projectDefinition addPackageNamed: self samplePackageName.
	name := self samplePackageName.
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

{ #category : 'rowan' }
RowanPackageService >> isDirty [

	^isDirty := self rowanDirty

]

{ #category : 'rowan' }
RowanPackageService >> loadedClassDefinitions [

	^self loadedClasses collect:[:loadedClass | loadedClass asDefinition]

]

{ #category : 'rowan' }
RowanPackageService >> loadedClasses [

	| loadedPackage |
	loadedPackage := Rowan image loadedPackageNamed: name ifAbsent:[^Array new].
	^loadedPackage loadedClasses

]

{ #category : 'rowan' }
RowanPackageService >> loadedClassExtensions [

	| loadedPackage |
	loadedPackage := Rowan image loadedPackageNamed: name ifAbsent:[^Array new].
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

{ #category : 'Accessing' }
RowanPackageService >> projectDefinition [
	^projectDefinition

]

{ #category : 'Updating' }
RowanPackageService >> projectDefinition: newValue [
	projectDefinition := newValue

]

{ #category : 'commands' }
RowanPackageService >> removeClass: classService [

	self removeClassNamed: classService name. 
	self update.
]

{ #category : 'commands' }
RowanPackageService >> removeClassNamed: className [

	self browserTool removeClassNamed: className.

]

{ #category : 'rowan' }
RowanPackageService >> rowanDirty [

	^(RwPackage newNamed: name) isDirty

]

{ #category : 'examples' }
RowanPackageService >> samplePackageName [
	
	^'SamplePackageName'

]

{ #category : 'examples' }
RowanPackageService >> sampleProjectName [

	^'SampleProjectName'

]

{ #category : 'commands' }
RowanPackageService >> setDefaultTemplate [

	defaultTemplate := self genericClassCreationTemplate.
]

{ #category : 'updates' }
RowanPackageService >> update [
	classes := (self loadedClassNames keys collect:[:string | RowanClassService forClassNamed: string]) asArray.
	classes addAll: (self loadedClassExtensions keys collect:[:string | 
		| classService | 
		classService := (RowanClassService forClassNamed: string) 
			isExtension: true.
		(Rowan image loadedClassForClass: classService classFromName ifAbsent:[]) 
			ifNotNil:[:cls | classService definedPackageName: cls packageName]]).
	classes do: [:clsService | clsService packageName: self name]. 
	self isDirty. 
	RowanCommandResult addResult: self

]
