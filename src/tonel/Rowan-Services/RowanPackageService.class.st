Class {
	#name : 'RowanPackageService',
	#superclass : 'RowanService',
	#instVars : [
		'projectDefinition',
		'packageName',
		'name'
	],
	#category : 'Rowan-Services'
}

{ #category : 'instance creation' }
RowanPackageService class >> forPackageNamed: packageName [

	| inst |
	inst := self new. 
	inst packageName: packageName.
	^inst

]

{ #category : 'rowan' }
RowanPackageService >> changes [
   "diffForPackageName: not implemented yet"
 
   ^ (Rowan packageTools diff diffForPackageName: name) asString

]

{ #category : 'rowan' }
RowanPackageService >> createPackage [
	| projectService default |
	default := RowanProjectService defaultProjectName.
	projectService := RowanProjectService new.
	projectService createProjectNamed: default.  
	(Rowan packageNames includes: packageName) ifFalse:[
		self browserTool addPackageNamed: packageName toProjectNamed: default].

]

{ #category : 'rowan' }
RowanPackageService >> createPackageNamed: aString inProject: projectName [
	| projectService | 
	packageName := aString.
	projectService := RowanProjectService new.
	projectDefinition := projectService createProjectNamed: projectName.  
	projectDefinition addPackageNamed: packageName.
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
	packageName := self samplePackageName.
	self projectTools load loadProjectDefinition: projectDefinition.

]

{ #category : 'rowan' }
RowanPackageService >> definition [

	^(Rowan image loadedPackageNamed: name) asDefinition

]

{ #category : 'rowan' }
RowanPackageService >> deletePackage [

	self browserTool removePackageNamed: packageName.

]

{ #category : 'rowan' }
RowanPackageService >> genericClassCreationTemplate [

	^self browserTool classCreationTemplateForSubclassOf: 'Object' category: name packageName: nil

]

{ #category : 'rowan' }
RowanPackageService >> loadedClassDefinitions [

	^self loadedClasses collect:[:loadedClass | loadedClass asDefinition]

]

{ #category : 'rowan' }
RowanPackageService >> loadedClasses [

	| loadedPackage |
	loadedPackage := Rowan image loadedPackageNamed: packageName ifAbsent:[^Array new].
	^loadedPackage loadedClasses

]

{ #category : 'rowan' }
RowanPackageService >> loadedClassHandles [

	^self loadedClasses collect:[:loadedClass | loadedClass handle]

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
	^packageName

]

{ #category : 'Updating' }
RowanPackageService >> packageName: newValue [
	packageName := newValue

]

{ #category : 'Accessing' }
RowanPackageService >> projectDefinition [
	^projectDefinition

]

{ #category : 'Updating' }
RowanPackageService >> projectDefinition: newValue [
	projectDefinition := newValue

]

{ #category : 'rowan' }
RowanPackageService >> removeClassNamed: className [

	self browserTool removeClassNamed: className

]

{ #category : 'examples' }
RowanPackageService >> samplePackageName [
	
	^'SamplePackageName'

]

{ #category : 'examples' }
RowanPackageService >> sampleProjectName [

	^'SampleProjectName'

]
