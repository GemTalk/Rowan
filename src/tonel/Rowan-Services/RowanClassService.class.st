Class {
	#name : 'RowanClassService',
	#superclass : 'RowanService',
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
		'packageService',
		'meta'
	],
	#category : 'Rowan-Services'
}

{ #category : 'instance creation' }
RowanClassService class >> forClassNamed: className [ 

	| inst |
	inst := self new. 
	inst name: className.
	^inst

]

{ #category : 'instance creation' }
RowanClassService class >> forClassNamed: className meta: aBoolean [

	| inst |
	inst := self new. 
	inst name: className;
		meta: aBoolean.
	^inst

]

{ #category : 'instance creation' }
RowanClassService class >> forClassNamed: className package: packageName [

	| inst |
	inst := self forClassNamed: className.
	inst packageName: packageName.
	^inst

]

{ #category : 'rowan' }
RowanClassService >> classCreationTemplate [
	
	^self browserTool classCreationTemplateForClass: self classFromName hybridBrowser: true.

]

{ #category : 'private' }
RowanClassService >> classFromName [

	^(System myUserProfile resolveSymbol: name asSymbol) value

]

{ #category : 'Accessing' }
RowanClassService >> classInstVarNames [
	^classInstVarNames

]

{ #category : 'Updating' }
RowanClassService >> classInstVarNames: newValue [
	classInstVarNames := newValue

]

{ #category : 'Accessing' }
RowanClassService >> classType [
	^classType

]

{ #category : 'Updating' }
RowanClassService >> classType: newValue [
	classType := newValue

]

{ #category : 'Accessing' }
RowanClassService >> classVarNames [
	^classVarNames

]

{ #category : 'Updating' }
RowanClassService >> classVarNames: newValue [
	classVarNames := newValue

]

{ #category : 'Accessing' }
RowanClassService >> comment [
	^comment

]

{ #category : 'Updating' }
RowanClassService >> comment: newValue [
	comment := newValue

]

{ #category : 'examples' }
RowanClassService >> createSampleClass [

	| classDefinition |
	packageService := RowanPackageService new. 
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
RowanClassService >> instVarNames [
	^instVarNames

]

{ #category : 'Updating' }
RowanClassService >> instVarNames: newValue [
	instVarNames := newValue

]

{ #category : 'other' }
RowanClassService >> meta [

	^meta

]

{ #category : 'other' }
RowanClassService >> meta: anObject [

	meta := anObject

]

{ #category : 'Accessing' }
RowanClassService >> name [
	^name

]

{ #category : 'Updating' }
RowanClassService >> name: newValue [
	name := newValue

]

{ #category : 'examples' }
RowanClassService >> packageName [
	
	^packageService packageName

]

{ #category : 'examples' }
RowanClassService >> packageName: packageName [
	
	packageService ifNil:[packageService := RowanPackageService new packageName: packageName].
	^packageService packageName

]

{ #category : 'Accessing' }
RowanClassService >> packageService [
	^packageService

]

{ #category : 'Updating' }
RowanClassService >> packageService: newValue [
	packageService := newValue

]

{ #category : 'Accessing' }
RowanClassService >> poolDictionaryNames [
	^poolDictionaryNames

]

{ #category : 'Updating' }
RowanClassService >> poolDictionaryNames: newValue [
	poolDictionaryNames := newValue

]

{ #category : 'rowan' }
RowanClassService >> removeSelector: selector [

	self browserTool removeMethod: selector forClassNamed: name asString isMeta: meta

]

{ #category : 'rowan' }
RowanClassService >> removeSelector: selector ifAbsent: absentBlock [

	(self classFromName compiledMethodAt: selector otherwise: nil) isNil ifTrue:[ ^absentBlock value ].
	self browserTool removeMethod: selector forClassNamed: name asString isMeta: meta


]

{ #category : 'examples' }
RowanClassService >> sampleClass [
	"return the actual resolved class"
	
	^(GsSession currentSession resolveSymbol: self sampleClassName) value

]

{ #category : 'examples' }
RowanClassService >> sampleClassInstance [

	^self sampleClass new

]

{ #category : 'examples' }
RowanClassService >> sampleClassName [
	
	^'SampleClassName'

]

{ #category : 'examples' }
RowanClassService >> samplePackageName [
	
	^packageService samplePackageName

]

{ #category : 'rowan' }
RowanClassService >> subclassCreationTemplate [

	^self browserTool classCreationTemplateForSubclassOf: name className: 'NewSubclass' category: self classFromName rowanPackageName

]

{ #category : 'Accessing' }
RowanClassService >> subclassType [
	^subclassType

]

{ #category : 'Updating' }
RowanClassService >> subclassType: newValue [
	subclassType := newValue

]

{ #category : 'Accessing' }
RowanClassService >> superclassName [
	^superclassName

]

{ #category : 'Updating' }
RowanClassService >> superclassName: newValue [
	superclassName := newValue

]
