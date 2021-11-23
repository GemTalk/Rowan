Class {
	#name : 'RwRepositoryComponentProjectReaderVisitor',
	#superclass : 'RwAbstractReaderWriterVisitor',
	#instVars : [
		'packageNames',
		'packageNamesBlock',
		'currentProjectReferenceDefinition'
	],
	#category : 'Rowan-DefinitionsV2'
}

{ #category : 'package reading' }
RwRepositoryComponentProjectReaderVisitor >> _packageNameFromPackageDir: packageDir ifAbsent: absentBlock [

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

{ #category : 'tonel parser' }
RwRepositoryComponentProjectReaderVisitor >> newClassDefinitionFrom: anArray [
	| metadata |
	metadata := anArray sixth.
	currentClassExtension := nil.
	currentClassDefinition := RwClassDefinition
		newForClassNamed: (metadata at: #'name') asString
		super:
			(metadata at: #'superclass' ifAbsent: [ metadata at: #'super' ]) asString
		instvars: (metadata at: #'instvars' ifAbsent: [ #() ])
		classinstvars: (metadata at: #'classinstvars' ifAbsent: [ #() ])
		classvars: (metadata at: #'classvars' ifAbsent: [ #() ])
		category: (metadata at: #'category' ifAbsent: [  ])
		comment: (anArray second )
		pools: (metadata at: #'pools' ifAbsent: [ #() ])
		type: (metadata at: #'type' ifAbsent: [ #'normal' ]) asSymbol.
	^ currentClassDefinition
		gs_options: (metadata at: #'gs_options' ifAbsent: [ #() ]);
		gs_reservedOop: (metadata at: #'gs_reservedoop' ifAbsent: [ '' ]);
		gs_constraints: (metadata at: #'gs_constraints' ifAbsent: [ #() ]);
		shebang: (metadata at: #'shebang' ifAbsent: [  ]);
		yourself
]

{ #category : 'tonel parser' }
RwRepositoryComponentProjectReaderVisitor >> newClassExtensionDefinitionFrom: anArray [
	| className |
	className := ((anArray sixth) at: #name) asString.
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

{ #category : 'accessing' }
RwRepositoryComponentProjectReaderVisitor >> packageNamesBlock [
	^ packageNamesBlock
		ifNil: [ [ :packageName | self packageNames includes: packageName ] ]
]

{ #category : 'accessing' }
RwRepositoryComponentProjectReaderVisitor >> packageNamesBlock: object [
	packageNamesBlock := object
]

{ #category : 'class reading' }
RwRepositoryComponentProjectReaderVisitor >> readClassesFor: packageName packageRoot: packageRoot [

	^ self subclassResponsibility: #readClassesFor:packageRoot:
]

{ #category : 'package reading' }
RwRepositoryComponentProjectReaderVisitor >> readPackages: packagesRoot [
	| trace |
	trace := Rowan projectTools trace.
	packagesRoot directories
		do: [ :packageDir | 
			(self _packageNameFromPackageDir: packageDir ifAbsent: [  ])
				ifNil: [ 
					trace
						trace:
							'--- skip reading ' , packageDir printString
								, ' not a tonel package directory (missing or malformed package.st file' ]
				ifNotNil: [ :packageName | 
					trace
						trace:
							'--- reading package ' , packageName asString , ' dir ' , packageDir asString.
					packageDir extension = self packageExtension
						ifFalse: [ trace trace: '      skipped readClasses, extension does not match' ]
						ifTrue: [ 
							(self packageNamesBlock value: packageName)
								ifTrue: [ self readClassesFor: packageName packageRoot: packageDir ]
								ifFalse: [ trace trace: '      skipped readClasses, packageName rejected' ] ] ] ]
]

{ #category : 'public' }
RwRepositoryComponentProjectReaderVisitor >> visit: anObject [

	anObject acceptVisitor: self.
]

{ #category : 'visiting' }
RwRepositoryComponentProjectReaderVisitor >> visitResolvedProjectV2: aRwResolvedProjectV2 [

	self currentProjectDefinition: aRwResolvedProjectV2.
	aRwResolvedProjectV2 _projectDefinition packages: Dictionary new.
	self readPackages: aRwResolvedProjectV2 packagesRoot
]
