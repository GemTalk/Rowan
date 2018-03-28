Class {
	#name : 'RowanPackageDefinitionService',
	#superclass : 'RowanDefinitionService',
	#instVars : [
		'projectDefinition',
		'packageName',
		'name'
	],
	#category : 'Rowan-Services'
}

{ #category : 'instance creation' }
RowanPackageDefinitionService class >> forPackageNamed: packageName [

	| inst |
	inst := self new. 
	inst packageName: packageName.
	^inst

]

{ #category : 'rowan' }
RowanPackageDefinitionService >> createPackage [
	| projectService existingPackage |
	self rowanFixMe. "Eventually, should not use the sample project or loaded projects directly "
	projectService := RowanProjectDefinitionService new.
	projectDefinition := projectService createProjectNamed: self sampleProjectName.  
	existingPackage := Rowan image loadedPackageNamed: packageName ifAbsent:[
		projectDefinition addPackageNamed: packageName.
		^self projectTools load loadProjectDefinition: projectDefinition.].
	(Rowan image loadedProjectNamed: projectDefinition name) addLoadedPackage: existingPackage

]

{ #category : 'rowan' }
RowanPackageDefinitionService >> createPackageNamed: aString inProject: projectName [
	| projectService | 
	packageName := aString.
	projectService := RowanProjectDefinitionService new.
	projectDefinition := projectService createProjectNamed: projectName.  
	projectDefinition addPackageNamed: packageName.
	self projectTools load loadProjectDefinition: projectDefinition.

]

{ #category : 'examples' }
RowanPackageDefinitionService >> createSamplePackage [

	"assume that the sample project & symbol dictionary 
	were already removed"

	| projectService |
	projectService := RowanProjectDefinitionService new.
	projectDefinition := projectService createSampleProject.  
	projectDefinition addPackageNamed: self samplePackageName.
	packageName := self samplePackageName.
	self projectTools load loadProjectDefinition: projectDefinition.

]

{ #category : 'rowan' }
RowanPackageDefinitionService >> deletePackage [

	self browserTool removePackageNamed: packageName.

]

{ #category : 'rowan' }
RowanPackageDefinitionService >> genericClassCreationTemplate [

	^self browserTool classCreationTemplateForSubclassOf: 'Object' category: name packageName: nil

]

{ #category : 'rowan' }
RowanPackageDefinitionService >> loadedClassDefinitions [

	^self loadedClasses collect:[:loadedClass | loadedClass asDefinition]

]

{ #category : 'rowan' }
RowanPackageDefinitionService >> loadedClasses [

	| loadedPackage |
	loadedPackage := Rowan image loadedPackageNamed: packageName ifAbsent:[^Array new].
	^loadedPackage loadedClasses

]

{ #category : 'rowan' }
RowanPackageDefinitionService >> loadedClassHandles [

	^self loadedClasses collect:[:loadedClass | loadedClass handle]

]

{ #category : 'rowan' }
RowanPackageDefinitionService >> loadedClassNames [

	^self loadedClasses collect:[:loadedClass | loadedClass name]

]

{ #category : 'Accessing' }
RowanPackageDefinitionService >> name [
	^name

]

{ #category : 'Updating' }
RowanPackageDefinitionService >> name: newValue [
	name := newValue

]

{ #category : 'Accessing' }
RowanPackageDefinitionService >> packageName [
	^packageName

]

{ #category : 'Updating' }
RowanPackageDefinitionService >> packageName: newValue [
	packageName := newValue

]

{ #category : 'Accessing' }
RowanPackageDefinitionService >> projectDefinition [
	^projectDefinition

]

{ #category : 'Updating' }
RowanPackageDefinitionService >> projectDefinition: newValue [
	projectDefinition := newValue

]

{ #category : 'rowan' }
RowanPackageDefinitionService >> removeClassNamed: className [

	self browserTool removeClassNamed: className

]

{ #category : 'examples' }
RowanPackageDefinitionService >> samplePackageName [
	
	^'SamplePackageName'

]

{ #category : 'examples' }
RowanPackageDefinitionService >> sampleProjectName [

	^projectDefinition name

]
