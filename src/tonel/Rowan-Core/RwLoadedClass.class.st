"
package: the CypLoadedPackage that defines me and all my methods that are not extension methods.
extensions: a collection of CypLoadedExtendedClasses for this class. From these we can tell which methods don't belong to our default package.
"
Class {
	#name : 'RwLoadedClass',
	#superclass : 'RwLoadedThing',
	#instVars : [
		'loadedPackage',
		'loadedInstanceMethods',
		'loadedClassMethods'
	],
	#category : 'Rowan-Core'
}

{ #category : 'definitions' }
RwLoadedClass >> asDefinition [
	"Create a Definition corresponding to what I define in the image."
	^RwClassDefinition
		withProperties: self propertiesForDefinition
		classMethodDefinitions: self classMethodDefinitions
		instanceMethodDefinitions: self instanceMethodDefinitions
]

{ #category : 'private' }
RwLoadedClass >> classMethodDefinitions [

	^self definitionsFor: loadedClassMethods
]

{ #category : 'private' }
RwLoadedClass >> instanceMethodDefinitions [

	^self definitionsFor: loadedInstanceMethods
]

{ #category : 'testing' }
RwLoadedClass >> isEmpty [

	^loadedInstanceMethods isEmpty and: [loadedClassMethods isEmpty]
]

{ #category : 'testing' }
RwLoadedClass >> isLoadedClass [

	^ true
]

{ #category : 'testing' }
RwLoadedClass >> isLoadedClassExtension [

	^ false
]

{ #category : 'enumeration' }
RwLoadedClass >> loadedInstanceMethodsDo: loadedInstanceMethodBlock loadedClassMethodsDo: loadedClassMethodBlock [

	loadedInstanceMethods
		valuesDo: [ :loadedMethod | 
			loadedInstanceMethodBlock
				cull: self loadedProject
				cull: self loadedPackage
				cull: self
				cull: loadedMethod ].
	loadedClassMethods
		valuesDo: [ :loadedMethod | 
			loadedClassMethodBlock
				cull: self loadedProject
				cull: self loadedPackage
				cull: self
				cull: loadedMethod ]
]

{ #category : 'accessing' }
RwLoadedClass >> loadedPackage [

	^loadedPackage
]

{ #category : 'accessing' }
RwLoadedClass >> loadedPackage: aLoadedPackage [

	loadedPackage := aLoadedPackage
]

{ #category : 'private' }
RwLoadedClass >> markPackageDirty [

	loadedPackage markDirty
]

{ #category : 'accessing' }
RwLoadedClass >> packageName [

	^loadedPackage name
]
