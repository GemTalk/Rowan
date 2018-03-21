Class {
	#name : 'TDRowanPackageDefinition',
	#superclass : 'TDPackageBasedDefinition',
	#instVars : [
		'project',
		'packageName'
	],
	#category : 'Rowan-Tode'
}

{ #category : 'accessing' }
TDRowanPackageDefinition >> classDefinitions [

	| classDefinitionMap loadedPackage |
	classDefinitionMap := Dictionary new.
	loadedPackage := self loadedPackage.
	loadedPackage loadedClasses values
		do: [ :rowanLoadedClass | 
			classDefinitionMap
				at: rowanLoadedClass name
				put:
					(TDRowanLoadedClassDefinition new
						className: rowanLoadedClass name;
						loadedClass: rowanLoadedClass;
						yourself) ].
	loadedPackage loadedClassExtensions values
		do: [ :rowanLoadedClassExtension | 
			(classDefinitionMap at: rowanLoadedClassExtension name ifAbsent: [  ])
				ifNil: [ 
					classDefinitionMap
						at: rowanLoadedClassExtension name
						put:
							(TDRowanLoadedClassDefinition new
								className: rowanLoadedClassExtension name;
								addLoadedClassExtension: rowanLoadedClassExtension;
								yourself) ]
				ifNotNil: [ :classDefinition | classDefinition addLoadedClassExtension: rowanLoadedClassExtension ] ].
	^ classDefinitionMap values
		sortWithBlock: [ :a :b | a className <= b className ]
]

{ #category : 'accessing' }
TDRowanPackageDefinition >> label [
  ^ self packageName
]

{ #category : 'accessing' }
TDRowanPackageDefinition >> loadedPackage [

	^ Rowan image loadedPackageNamed: self packageName
]

{ #category : 'accessing' }
TDRowanPackageDefinition >> packageName [

   ^packageName

]

{ #category : 'accessing' }
TDRowanPackageDefinition >> packageName: anObject [

   packageName := anObject

]

{ #category : 'accessing' }
TDRowanPackageDefinition >> project [

   ^project

]

{ #category : 'accessing' }
TDRowanPackageDefinition >> project: anObject [

   project := anObject

]
