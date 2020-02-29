Class {
	#name : 'RwRepositoryComponentProjectReaderVisitor',
	#superclass : 'RwAbstractReaderWriterVisitor',
	#instVars : [
		'packageNames',
		'currentDirectory',
		'currentProjectReferenceDefinition'
	],
	#category : 'Rowan-DefinitionsV2'
}

{ #category : 'package reading' }
RwRepositoryComponentProjectReaderVisitor >> _packageNameFromPackageDir: packageDir [

	"this is not really correct, but it works as a fallback (filetree does not have independent package name)"

	^ packageDir basenameWithoutExtension
]

{ #category : 'class reading' }
RwRepositoryComponentProjectReaderVisitor >> compileWhileReading [
  ^ (self dynamicInstVarAt: #compileWhileReading) ifNil:[ false ]
]

{ #category : 'class reading' }
RwRepositoryComponentProjectReaderVisitor >> compileWhileReading: aBoolean [
  self dynamicInstVarAt: #compileWhileReading put: aBoolean 
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

{ #category : 'package reading' }
RwRepositoryComponentProjectReaderVisitor >> readPackages: packagesRoot [
	packagesRoot directories do: [:packageDir | | dir |
    dir := packageDir path basename .
    dir = '.svn' ifFalse:[  "tolerate checkout produced by svn version 1.6"
		  | packageName |
		  packageName := self _packageNameFromPackageDir: packageDir.
		  (packageDir extension = self packageExtension and: [ self packageNames includes: packageName ])
			  ifTrue: [ self readClassesFor: packageName packageRoot: packageDir ] ]
    ].
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

{ #category : 'visiting' }
RwRepositoryComponentProjectReaderVisitor >> visitResolvedProjectV2: aRwResolvedProjectV2 [

	self currentProjectDefinition: aRwResolvedProjectV2.
	aRwResolvedProjectV2 _projectDefinition packages: Dictionary new.
	self readPackages: aRwResolvedProjectV2 packagesRoot
]
