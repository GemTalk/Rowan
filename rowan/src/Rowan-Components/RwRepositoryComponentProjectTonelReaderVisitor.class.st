Class {
	#name : 'RwRepositoryComponentProjectTonelReaderVisitor',
	#superclass : 'RwRepositoryComponentProjectReaderVisitor',
	#category : 'Rowan-Components'
}

{ #category : 'class reading' }
RwRepositoryComponentProjectTonelReaderVisitor >> classExtensionExtensions [

	^ #( 'extensions' 'st' )
]

{ #category : 'class reading' }
RwRepositoryComponentProjectTonelReaderVisitor >> classExtensions [

	^ #( 'class' 'st' )
]

{ #category : 'tonel parser' }
RwRepositoryComponentProjectTonelReaderVisitor >> definitionForType: aString [
  aString = TonelWriter classLabel
    ifTrue: [ ^ #class ].
  aString = TonelWriter extensionLabel
    ifTrue: [ ^ nil ].
  TonelParseError signal: 'Unknown type declaration.'
]

{ #category : 'tonel parser' }
RwRepositoryComponentProjectTonelReaderVisitor >> newClassDefinitionFrom: anArray [
  | metadata classDef |
  metadata := anArray sixth.
  classDef := RwClassDefinition
		newForClassNamed: (metadata at: #'name') 
			super: (metadata at: #'superclass') 
			instvars: (metadata at: #'instvars' ifAbsent: [ #() ]) 
			classinstvars: (metadata at: #'classinstvars' ifAbsent: [ #() ]) 
			classvars: (metadata at: #'classvars' ifAbsent: [ #() ]) 
			category: (metadata at: #'category' ifAbsent: [ ])
			comment: (anArray second ifNil: [ '' ]) 
			pools: (metadata at: #'pools' ifAbsent: [ #() ]) 
			type: (metadata at: #'type' ifAbsent: [ #'normal' ]) asSymbol.
	^ classDef
		gs_options: (metadata at: #'gs_options' ifAbsent: [ #() ]);
		gs_constraints: (metadata at: #'gs_constraints' ifAbsent: [ #() ]);
		yourself
]

{ #category : 'tonel parser' }
RwRepositoryComponentProjectTonelReaderVisitor >> newMethodDefinitionForClassNamed: className classIsMeta: meta selector: selector category: category source: source [

	^ RwMethodDefinition
		newForSelector: selector 
			protocol: category 
			source: source
]

{ #category : 'tonel parser' }
RwRepositoryComponentProjectTonelReaderVisitor >> newTypeDefinitionFrom: anArray [
  | typeClass |
  typeClass := self definitionForType: anArray fourth.
  typeClass == #class
    ifTrue: [ ^ self newClassDefinitionFrom: anArray ].
  "is extension, no type"
  ^ nil
]

{ #category : 'package reading' }
RwRepositoryComponentProjectTonelReaderVisitor >> packageExtension [

	^ 'package'
]

{ #category : 'class reading' }
RwRepositoryComponentProjectTonelReaderVisitor >> readClassesFor: packageName packageRoot: packageRoot [
	currentProjectDefinition 
		packageNamed: packageName 
		ifAbsent: [ currentProjectDefinition addPackageNamed: packageName ].
	packageRoot files do: [:file |
		| extensions x |
		extensions := file extensions asArray.
		extensions = ( x :=  self classExtensions)
			ifTrue: [ ^ self readClassFile: file inPackage: packageName ].
		extensions = ( x := self classExtensionExtensions)
			ifTrue: [ ^ self readClassExtensionFile: file  inPackage: packageName ] ]
]

{ #category : 'class reading' }
RwRepositoryComponentProjectTonelReaderVisitor >> readClassExtensionFile: file inPackage: packageName [

self error: 'not yet implemented'
]

{ #category : 'class reading' }
RwRepositoryComponentProjectTonelReaderVisitor >> readClassFile: file inPackage: packageName [

	file readStreamDo: [:fileStream |
		| definitions stream classDef |
		stream := ZnBufferedReadStream on: fileStream. "wrap with buffered stream to bypass https://github.com/GemTalk/FileSystemGs/issues/9"
		stream sizeBuffer: 10000. "part of workaround for GemTalk/FileSystemGs#9"
		definitions := (TonelParser on: stream forReader: self) start.
		classDef := definitions at: 1.
		((definitions at: 2) at: 1) do: [:mDef |
			classDef addClassMethodDefinition: mDef ].
		((definitions at: 2) at: 2) do: [:mDef |
			classDef addInstanceMethodDefinition: mDef ].
		(currentProjectDefinition packageNamed: packageName)
			addClassDefinition: classDef ].
]
