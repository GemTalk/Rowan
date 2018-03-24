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
RowanPackageDefinitionService >> genericClassCreationTemplate [

	^self browserTool classCreationTemplateForSubclassOf: 'Object' category: name packageName: name

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

{ #category : 'examples' }
RowanPackageDefinitionService >> samplePackageName [
	
	^'SamplePackageName'

]

{ #category : 'examples' }
RowanPackageDefinitionService >> sampleProjectName [

	^projectDefinition name

]
