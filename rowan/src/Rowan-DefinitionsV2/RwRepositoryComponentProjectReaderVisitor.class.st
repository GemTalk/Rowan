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
	(anRwDefinition propertyAt:'_gsFileOffset' ifAbsent: nil) ifNotNil:[:offset |
		 (anRwDefinition propertyAt:'_gsFileName' ifAbsent: nil) ifNotNil:[:fName | 
			 ^ self lineNumberStringForOffset: offset fileName: fName
			] 
	].
  ^ '  (Unable to determine line )' .
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
			| buf lNum |
			buf := fStream contents.
			buf size > offset
				ifTrue: [ buf size: offset ].
			lNum := 1 + (buf occurrencesOf: Character lf).
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
							trace
								trace:
									'--- reading package ' , packageName asString , ' dir ' , packageDir asString.
							(self packageNamesBlock value: packageName)
								ifTrue: [ 
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
