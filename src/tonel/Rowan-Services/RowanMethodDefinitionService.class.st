Class {
	#name : 'RowanMethodDefinitionService',
	#superclass : 'RowanDefinitionService',
	#instVars : [
		'source',
		'selector',
		'methodDefinitions',
		'classService',
		'category',
		'packageName',
		'className',
		'meta'
	],
	#category : 'Rowan-Services'
}

{ #category : 'instance creation' }
RowanMethodDefinitionService class >> source: source selector: selector category: category className: className packageName: packageName meta: boolString [

	| service |
	service := self new. 
	service 
		source: source;
		selector: selector;
		category: category;
		className: className;
		packageName: packageName;
		meta: boolString == true.
	^service

]

{ #category : 'rowan' }
RowanMethodDefinitionService >> addOrUpdateMethod [

		self browserTool
                   addOrUpdateMethod: source
                   inProtocol: category
                   forClassNamed: self classService name
                   isMeta: meta
                   inPackageNamed: self classService packageName

]

{ #category : 'rowan' }
RowanMethodDefinitionService >> browserTool [

	^Rowan projectTools browser.

]

{ #category : 'Accessing' }
RowanMethodDefinitionService >> category [
	^category

]

{ #category : 'Updating' }
RowanMethodDefinitionService >> category: newValue [
	category := newValue

]

{ #category : 'Accessing' }
RowanMethodDefinitionService >> className [
	^className

]

{ #category : 'Updating' }
RowanMethodDefinitionService >> className: newValue [
	className := newValue

]

{ #category : 'Accessing' }
RowanMethodDefinitionService >> classService [

	^classService ifNil:[classService := RowanClassDefinitionService forClassNamed: className package: packageName]

]

{ #category : 'Updating' }
RowanMethodDefinitionService >> classService: newValue [
	classService := newValue

]

{ #category : 'examples' }
RowanMethodDefinitionService >> createSampleMethod [
           
           |   classDefinition | 
		
			classService := RowanClassDefinitionService new.
			classDefinition := classService createSampleClass. 
			source := self sampleMethodSource.
			category := 'sample'.
			meta := false. 
			^self addOrUpdateMethod

]

{ #category : 'rowan' }
RowanMethodDefinitionService >> definitionClass [

	^RwMethodDefinition

]

{ #category : 'Accessing' }
RowanMethodDefinitionService >> meta [
	^meta printString

]

{ #category : 'Updating' }
RowanMethodDefinitionService >> meta: newValue [
	meta := newValue == 'true'

]

{ #category : 'Accessing' }
RowanMethodDefinitionService >> methodDefinitions [
	^methodDefinitions

]

{ #category : 'Updating' }
RowanMethodDefinitionService >> methodDefinitions: newValue [
	methodDefinitions := newValue

]

{ #category : 'Accessing' }
RowanMethodDefinitionService >> packageName [
	^packageName

]

{ #category : 'Updating' }
RowanMethodDefinitionService >> packageName: newValue [
	packageName := newValue

]

{ #category : 'examples' }
RowanMethodDefinitionService >> sampleClassInstance [

	^classService sampleClassInstance

]

{ #category : 'examples' }
RowanMethodDefinitionService >> sampleDefinition [

	definition := self definitionClass newForSelector: 'sampleMethod' protocol: 'sampleProtocol' source: self sampleMethodSource.
	source := definition source.
	selector := definition selector.
	^definition

]

{ #category : 'examples' }
RowanMethodDefinitionService >> sampleDefinitions [
	
	methodDefinitions := Array with: self sampleDefinition.
	^methodDefinitions

]

{ #category : 'examples' }
RowanMethodDefinitionService >> sampleMethodSelector [

	^'sampleMethod'

]

{ #category : 'examples' }
RowanMethodDefinitionService >> sampleMethodSource [

	^'sampleMethod ^''some text'''.

]

{ #category : 'Accessing' }
RowanMethodDefinitionService >> selector [

	^selector

]

{ #category : 'Accessing' }
RowanMethodDefinitionService >> selector: aSymbol [

	selector := aSymbol

]

{ #category : 'Accessing' }
RowanMethodDefinitionService >> source [

	^source

]

{ #category : 'Accessing' }
RowanMethodDefinitionService >> source: aString [
	
	source := aString

]
