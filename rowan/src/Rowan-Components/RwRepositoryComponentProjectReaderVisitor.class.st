Class {
	#name : 'RwRepositoryComponentProjectReaderVisitor',
	#superclass : 'RwAbstractReaderWriterVisitor',
	#instVars : [
		'packageNames',
		'currentDirectory',
		'currentProjectReferenceDefinition'
	],
	#category : 'Rowan-Components'
}

{ #category : 'package reading' }
RwRepositoryComponentProjectReaderVisitor >> _packageNameFromPackageDir: packageDir [

	"this is not really correct, but it works as a fallback (filetree does not have independent package name)"

	^ packageDir basenameWithoutExtension
]

{ #category : 'accessing' }
RwRepositoryComponentProjectReaderVisitor >> currentDirectory [

	^ currentDirectory
]

{ #category : 'accessing' }
RwRepositoryComponentProjectReaderVisitor >> currentDirectory: aFileReference [

	currentDirectory := aFileReference
]

{ #category : 'accessing' }
RwRepositoryComponentProjectReaderVisitor >> currentProjectReferenceDefinition [

	^ currentProjectReferenceDefinition
]

{ #category : 'accessing' }
RwRepositoryComponentProjectReaderVisitor >> currentProjectReferenceDefinition: aRwProjectReferenceDefinition [

	currentProjectReferenceDefinition := aRwProjectReferenceDefinition
]

{ #category : 'tonel parser' }
RwRepositoryComponentProjectReaderVisitor >> newClassDefinitionFrom: anArray [
  | metadata |
  metadata := anArray sixth.
	currentClassExtension := nil.
  currentClassDefinition := RwClassDefinition
		newForClassNamed: (metadata at: #'name') 
			super: (metadata at: #'superclass' ifAbsent: [metadata at: #'super']) 
			instvars: (metadata at: #'instvars' ifAbsent: [ #() ]) 
			classinstvars: (metadata at: #'classinstvars' ifAbsent: [ #() ]) 
			classvars: (metadata at: #'classvars' ifAbsent: [ #() ]) 
			category: (metadata at: #'category' ifAbsent: [ ])
			comment: (anArray second ifNil: [ '' ]) 
			pools: (metadata at: #'pools' ifAbsent: [ #() ]) 
			type: (metadata at: #'type' ifAbsent: [ #'normal' ]) asSymbol.
	^ currentClassDefinition
		gs_options: (metadata at: #'gs_options' ifAbsent: [ #() ]);
		gs_reservedOop: (metadata at: #'gs_reservedoop' ifAbsent: [ '' ]);
		gs_constraints: (metadata at: #'gs_constraints' ifAbsent: [ #() ]);
		shebang: (metadata at: #'shebang' ifAbsent: [ ]);
		yourself
]

{ #category : 'tonel parser' }
RwRepositoryComponentProjectReaderVisitor >> newClassExtensionDefinitionFrom: anArray [
	| className |
	className := (anArray sixth) at: #name.
	currentClassDefinition := nil.
	^currentClassExtension := currentPackageDefinition
		classExtensionDefinitionNamed: className 
		ifAbsent: [ currentPackageDefinition addClassExtensionNamed: className ].
]

{ #category : 'package reading' }
RwRepositoryComponentProjectReaderVisitor >> packageExtension [

	^ self subclassResponsibility: #packageExtension
]

{ #category : 'accessing' }
RwRepositoryComponentProjectReaderVisitor >> packageNames [

	^ packageNames
]

{ #category : 'accessing' }
RwRepositoryComponentProjectReaderVisitor >> packageNames: anArray [

	packageNames := anArray
]

{ #category : 'class reading' }
RwRepositoryComponentProjectReaderVisitor >> readClassesFor: packageName packageRoot: packageRoot [

	^ self subclassResponsibility: #readClassesFor:packageRoot:
]

{ #category : 'other' }
RwRepositoryComponentProjectReaderVisitor >> readPackages: aPackagesRoot [
	aPackagesRoot directories do: [:packageDir |
		| packageName |
		packageName := self _packageNameFromPackageDir: packageDir.
		(packageDir extension = self packageExtension and: [ self packageNames includes: packageName ])
			ifTrue: [ self readClassesFor: packageName packageRoot: packageDir ] ]
]

{ #category : 'public' }
RwRepositoryComponentProjectReaderVisitor >> visit: anObject [

	anObject acceptVisitor: self.
]

{ #category : 'visiting' }
RwRepositoryComponentProjectReaderVisitor >> visitComponentProjectDefinition: aRwComponentProjectDefinition [

	self currentProjectDefinition: aRwComponentProjectDefinition.
	aRwComponentProjectDefinition packages: Dictionary new.
	self visit: aRwComponentProjectDefinition projectRef
]

{ #category : 'visiting' }
RwRepositoryComponentProjectReaderVisitor >> visitProjectReferenceDefinition: aRwProjectReferenceDefinition [
	"read the packageNames from the repository"

	self currentProjectReferenceDefinition: aRwProjectReferenceDefinition.
	self readPackages: aRwProjectReferenceDefinition packagesRoot
]
