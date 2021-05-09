Class {
	#name : 'RwDefinedFromResolvedProject',
	#superclass : 'RwDefinedProject',
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwDefinedFromResolvedProject class >> fromLoadedProject: aLoadedProject [
	^ (self newNamed: aLoadedProject name)
		_concreteProject: aLoadedProject _concreteProject asDefinition;
		yourself
]

{ #category : 'transitions' }
RwDefinedFromResolvedProject >> loadProjectSet [
	"
		refresh the contents of the receiver from disk and create a project set that includes project definitions of
			required projects, also read from disk. Then load the entire project set.
	"

	^ self _concreteProject loadProjectSet
]

{ #category : 'transitions' }
RwDefinedFromResolvedProject >> read [
	"return a RwDefinedProject with definitions read from disk"

	self _concreteProject read
]

{ #category : 'transitions' }
RwDefinedFromResolvedProject >> read: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [
	"return a RwDefinedProject with definitions read from disk, using the specificied conditional attributes"

	self _concreteProject read: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes
]

{ #category : 'transitions' }
RwDefinedFromResolvedProject >> readProjectComponentNames: componentNames [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return the receiver with a new set of definitions read from disk"

	self _concreteProject readProjectComponentNames: componentNames
]

{ #category : 'transitions' }
RwDefinedFromResolvedProject >> readProjectComponentNames: componentNames customConditionalAttributes: customConditionalAttributes [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return the receiver with a new set of definitions read from disk"

	self _concreteProject
		readProjectComponentNames: componentNames
		customConditionalAttributes: customConditionalAttributes
]

{ #category : 'transitions' }
RwDefinedFromResolvedProject >> readProjectComponentNames: componentNames customConditionalAttributes: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return the receiver with a new set of definitions read from disk"

	self _concreteProject
		readProjectComponentNames: componentNames
		customConditionalAttributes: customConditionalAttributes
		platformConditionalAttributes: platformConditionalAttributes
]

{ #category : 'transitions' }
RwDefinedFromResolvedProject >> readProjectSet [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return a project definition set that will contain the project definition along with any dependent project definitions"

	self _concreteProject readProjectSet
]

{ #category : 'transitions' }
RwDefinedFromResolvedProject >> readProjectSet: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return a project definition set that will contain the project definition along with any dependent project definitions"

	^ self _concreteProject readProjectSet: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes
]

{ #category : 'transitions' }
RwDefinedFromResolvedProject >> write [

	self _concreteProject
		export;
		exportLoadSpecification
]
