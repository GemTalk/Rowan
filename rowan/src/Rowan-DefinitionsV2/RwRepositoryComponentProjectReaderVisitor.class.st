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

{ #category : 'error handling' }
RwRepositoryComponentProjectReaderVisitor class >> lineNumberStringForDefinition: anRwDefinition [
	"used when handling STONReaderError,  TonelParseError, CompileError, CompileWarning"

	(anRwDefinition propertyAt: #'_gsFileOffset' ifAbsent: nil)
		ifNotNil: [ :offset | 
			(anRwDefinition propertyAt: #'_gsFileName' ifAbsent: nil)
				ifNotNil: [ :fName | ^ self lineNumberStringForOffset: offset fileName: fName ] ].
	^ '  (Unable to determine line )'
]

{ #category : 'error handling' }
RwRepositoryComponentProjectReaderVisitor class >> lineNumberStringForOffset: offset fileName: fName [
	| res |
	res := '  (Unable to determine line number)'.
	[ 
	| fRef |
	fRef := fName asFileReference.
	fRef
		readStreamDo: [ :fStream | 
			| lNum |
			offset
				ifNil: [ 
					"line 1 is better than nothing"
					lNum := 1 ]
				ifNotNil: [ 
					| buf |
					buf := fStream contents.
					buf size > offset
						ifTrue: [ buf size: offset ].
					lNum := 1 + (buf occurrencesOf: Character lf) ].
			res := ' near line ' , lNum asString , ' in file ' , fName ] ]
		on: Error
		do: [ :ex | 
			"ignore"
			 ].
	^ res
]

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
	| metadata typeField typeValue |
	metadata := anArray sixth.
	currentTraitDefinition := currentClassExtension := nil.
	typeField := metadata at: #'type' ifAbsent: [ #'normal' ].
	typeField isString
		ifTrue: [ typeValue := typeField asString ]
		ifFalse: [ 
			"assume typeField is an array: slot 1 is type; slot 2 is gs_options"
			typeValue := typeField at: 1.
			metadata at: #'gs_options' put: (typeField at: 2) ].
	currentClassDefinition := RwClassDefinition
		newForClassNamed: (metadata at: #'name') asString
		super:
			(metadata at: #'superclass' ifAbsent: [ metadata at: #'super' ]) asString
		instvars: (metadata at: #'instvars' ifAbsent: [ #() ])
		classinstvars: (metadata at: #'classinstvars' ifAbsent: [ #() ])
		classvars: (metadata at: #'classvars' ifAbsent: [ #() ])
		category: (metadata at: #'category' ifAbsent: [  ])
		comment: anArray second
		pools: #()	"
IGNORED		pools: (metadata at: #'pools' ifAbsent: [ #() ])
"
		type: typeValue asSymbol.
	(metadata at: #'_gsFileOffset' ifAbsent: [  ])
		ifNotNil: [ :offset | currentClassDefinition propertyAt: #'_gsFileOffset' put: offset ].
	(metadata at: #'_gsFileName' ifAbsent: [  ])
		ifNotNil: [ :filename | currentClassDefinition propertyAt: #'_gsFileName' put: filename ].
	(metadata at: #'commentstamp' ifAbsent: [  ])
		ifNotNil: [ :timestampString | currentClassDefinition propertyAt: #'commentStamp' put: timestampString ].
	(metadata at: #'traits' ifAbsent: [  ])
		ifNotNil: [ :traitsSpecString | currentClassDefinition propertyAt: #'traits' put: traitsSpecString ].
	(metadata at: #'classtraits' ifAbsent: [  ])
		ifNotNil: [ :classTraitsSpecString | currentClassDefinition propertyAt: #'classTraits' put: classTraitsSpecString ].
	^ currentClassDefinition
		gs_options: (metadata at: #'gs_options' ifAbsent: [ #() ]);
		gs_reservedOop: (metadata at: #'gs_reservedoop' ifAbsent: [ '' ]);
		gs_constraints: (metadata at: #'gs_constraints' ifAbsent: [ #() ]);
		yourself
]

{ #category : 'tonel parser' }
RwRepositoryComponentProjectReaderVisitor >> newClassExtensionDefinitionFrom: anArray [
	| className |
	className := ((anArray sixth) at: #name) asString.
	currentTraitDefinition := currentClassDefinition := nil.
	^currentClassExtension := currentPackageDefinition
		classExtensionDefinitionNamed: className 
		ifAbsent: [ currentPackageDefinition addClassExtensionNamed: className ].
]

{ #category : 'tonel parser' }
RwRepositoryComponentProjectReaderVisitor >> newTraitDefinitionFrom: anArray [
	| traitName metadata instvars packageName category expectedProperties |
	metadata := anArray sixth.
	currentClassExtension := currentClassDefinition := nil.
	traitName := metadata at: #'name'.
	instvars := #().	"for now"
	category := metadata at: #'category' ifAbsent: [  ].
	packageName := metadata at: #'package' ifAbsent: [ category ].
	currentTraitDefinition := RwTraitDefinition
		newForTraitNamed: traitName
		instanceVariableNames: instvars
		category: category
		packageName: packageName.
	expectedProperties := #(#'name' #'category' #'package').
	metadata
		keysAndValuesDo: [ :propName :propValue | 
			"copy any additional properties, to preserve them for other platforms"
			(expectedProperties includes: propName)
				ifFalse: [ currentTraitDefinition propertyAt: propName put: propValue ] ].
	^ currentTraitDefinition
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
	| trace packageNamesRead |
	packageNamesRead := Set new.
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
					packageDir extension = self packageExtension
						ifFalse: [ trace trace: '      skipped readClasses, extension does not match' ]
						ifTrue: [ 
							(self packageNamesBlock value: packageName)
								ifTrue: [ 
									trace
										trace:
											'--- reading package ' , packageName asString , ' dir ' , packageDir asString.
											self readClassesFor: packageName packageRoot: packageDir.
											packageNamesRead add: packageName ]
										ifFalse: [ trace trace: '      skipped readClasses, package ' , packageName , ' rejected' ] ] ] ].
	^ packageNamesRead
]

{ #category : 'public' }
RwRepositoryComponentProjectReaderVisitor >> visit: anObject [

	anObject acceptVisitor: self.
]

{ #category : 'visiting' }
RwRepositoryComponentProjectReaderVisitor >> visitResolvedProjectV2: aRwResolvedProjectV2 [
	| packageNamesRead pn |
	self currentProjectDefinition: aRwResolvedProjectV2.
	aRwResolvedProjectV2 _projectDefinition packages: Dictionary new.
	packageNamesRead := self readPackages: aRwResolvedProjectV2 packagesRoot.
	(pn := self packageNames asSet) = packageNamesRead
		ifFalse: [ 
			| message unreadPackageNames |
			message := 'Requested ' , pn size printString
				, ' packages to be read, but only ' , packageNamesRead size printString
				, ' packages were read. The following packages were not read:'.
			message lf.
			(unreadPackageNames := pn - packageNamesRead)
				do: [ :pkgName | 
					message
						tab;
						addAll: pkgName;
						lf ].
			message
				addAll:
					'Check that the package directory exists and that the name in the package.st file is correct (tonel format).'.
			RwUnreadPackagesErrorNotification new
				errorMessage: message;
				unreadPackageNames: unreadPackageNames;
				signal ]
]
