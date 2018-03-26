Class {
	#name : 'RowanClassDefinitionService',
	#superclass : 'RowanDefinitionService',
	#instVars : [
		'name',
		'comment',
		'instVarNames',
		'classVarNames',
		'classInstVarNames',
		'superclassName',
		'subclassType',
		'poolDictionaryNames',
		'classType',
		'packageService'
	],
	#category : 'Rowan-Services'
}

{ #category : 'instance creation' }
RowanClassDefinitionService class >> forClassNamed: className [ 

	| inst |
	inst := self new. 
	inst name: className.
	^inst

]

{ #category : 'instance creation' }
RowanClassDefinitionService class >> forClassNamed: className package: packageName [

	| inst |
	inst := self forClassNamed: className.
	inst packageName: packageName.
	^inst

]

{ #category : 'rowan' }
RowanClassDefinitionService >> classCreationTemplate [

	^self browserTool classCreationTemplateForClass: self classFromName hybridBrowser: true.

]

{ #category : 'private' }
RowanClassDefinitionService >> classFromName [

	^(System myUserProfile resolveSymbol: name asSymbol) value

]

{ #category : 'Accessing' }
RowanClassDefinitionService >> classInstVarNames [
	^classInstVarNames

]

{ #category : 'Updating' }
RowanClassDefinitionService >> classInstVarNames: newValue [
	classInstVarNames := newValue

]

{ #category : 'Accessing' }
RowanClassDefinitionService >> classType [
	^classType

]

{ #category : 'Updating' }
RowanClassDefinitionService >> classType: newValue [
	classType := newValue

]

{ #category : 'Accessing' }
RowanClassDefinitionService >> classVarNames [
	^classVarNames

]

{ #category : 'Updating' }
RowanClassDefinitionService >> classVarNames: newValue [
	classVarNames := newValue

]

{ #category : 'Accessing' }
RowanClassDefinitionService >> comment [
	^comment

]

{ #category : 'Updating' }
RowanClassDefinitionService >> comment: newValue [
	comment := newValue

]

{ #category : 'examples' }
RowanClassDefinitionService >> createSampleClass [

	| classDefinition |
	packageService := RowanPackageDefinitionService new. 
	packageService createSamplePackage.
	classDefinition := RwClassDefinition
		newForClassNamed: self sampleClassName
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: String new
		comment: 'Sample Rowan Class'
		pools: #()
		type: 'normal'.
	name := classDefinition name.
	self projectTools edit addClass: classDefinition
		inPackageNamed: packageService samplePackageName
		inProject: packageService projectDefinition.
	self projectTools load loadProjectDefinition: packageService projectDefinition.
	^classDefinition

]

{ #category : 'Accessing' }
RowanClassDefinitionService >> instVarNames [
	^instVarNames

]

{ #category : 'Updating' }
RowanClassDefinitionService >> instVarNames: newValue [
	instVarNames := newValue

]

{ #category : 'Accessing' }
RowanClassDefinitionService >> name [
	^name

]

{ #category : 'Updating' }
RowanClassDefinitionService >> name: newValue [
	name := newValue

]

{ #category : 'examples' }
RowanClassDefinitionService >> packageName [
	
	^packageService packageName

]

{ #category : 'examples' }
RowanClassDefinitionService >> packageName: packageName [
	
	packageService ifNil:[packageService := RowanPackageDefinitionService new packageName: packageName].
	^packageService packageName

]

{ #category : 'Accessing' }
RowanClassDefinitionService >> packageService [
	^packageService

]

{ #category : 'Updating' }
RowanClassDefinitionService >> packageService: newValue [
	packageService := newValue

]

{ #category : 'Accessing' }
RowanClassDefinitionService >> poolDictionaryNames [
	^poolDictionaryNames

]

{ #category : 'Updating' }
RowanClassDefinitionService >> poolDictionaryNames: newValue [
	poolDictionaryNames := newValue

]

{ #category : 'examples' }
RowanClassDefinitionService >> sampleClass [
	"return the actual resolved class"
	
	^(GsSession currentSession resolveSymbol: self sampleClassName) value

]

{ #category : 'examples' }
RowanClassDefinitionService >> sampleClassInstance [

	^self sampleClass new

]

{ #category : 'examples' }
RowanClassDefinitionService >> sampleClassName [
	
	^'SampleClassName'

]

{ #category : 'examples' }
RowanClassDefinitionService >> samplePackageName [
	
	^packageService samplePackageName

]

{ #category : 'Accessing' }
RowanClassDefinitionService >> subclassType [
	^subclassType

]

{ #category : 'Updating' }
RowanClassDefinitionService >> subclassType: newValue [
	subclassType := newValue

]

{ #category : 'Accessing' }
RowanClassDefinitionService >> superclassName [
	^superclassName

]

{ #category : 'Updating' }
RowanClassDefinitionService >> superclassName: newValue [
	superclassName := newValue

]
