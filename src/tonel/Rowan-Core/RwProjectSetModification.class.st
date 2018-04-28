"
A modification showing the difference between two PackageSets. The elements are packages. Key is package name.
movedClasses are classes that have moved from one package to another within the package set.
movedMethods are methods that have moved from a class definition to an extension of that class, or vice versa, or between two extensions of the same class.
"
Class {
	#name : 'RwProjectSetModification',
	#superclass : 'RwEntitySetModification',
	#category : 'Rowan-Core'
}

{ #category : 'visiting' }
RwProjectSetModification >> acceptVisitor: aVisitor [

	^ aVisitor visitProjecteSetModification: self
]

{ #category : 'enumerating' }
RwProjectSetModification >> classesModificationAndPackageModificationAndProjectModificationDo: aBlock [

	"enumerate the classesModification in the packagesModification in the projectModification in the receiver.
		The classesModification, packageModification, and projectModification are arguments to the block. "

	elementsModified
		do: [ :projectModification | 
			projectModification packagesModification elementsModified
				do: [ :packageModification | aBlock cull: packageModification classesModification cull: packageModification cull: projectModification ] ]

]
