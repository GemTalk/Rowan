Class {
	#name : 'TDRowanProjectDefinition',
	#superclass : 'TDPackageBasedDefinition',
	#instVars : [
		'projectName'
	],
	#category : 'Rowan-Tode'
}

{ #category : 'accessing' }
TDRowanProjectDefinition >> classDefinitions [

	| classDefinitionMap |
	classDefinitionMap := Dictionary new.
	self loadedProject loadedPackages
		do: [ :loadedPackage | 
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
						ifNotNil: [ :classDefinition | classDefinition addLoadedClassExtension: rowanLoadedClassExtension ] ] ].
	^ classDefinitionMap values
		sortWithBlock: [ :a :b | a className <= b className ]
]

{ #category : 'accessing' }
TDRowanProjectDefinition >> label [

	^ self projectName
]

{ #category : 'accessing' }
TDRowanProjectDefinition >> loadedProject [

	^ Rowan image loadedProjectNamed: self projectName
]

{ #category : 'accessing' }
TDRowanProjectDefinition >> packageDefinitions [

	^ (self loadedProject loadedPackages
		collect: [ :loadedPackage | 
			TDRowanPackageDefinition new
				project: self;
				packageName: loadedPackage name;
				yourself ])
		sortWithBlock: [ :a :b | a packageName <= b packageName ]
]

{ #category : 'accessing' }
TDRowanProjectDefinition >> projectName [

   ^projectName

]

{ #category : 'accessing' }
TDRowanProjectDefinition >> projectName: anObject [

   projectName := anObject

]
