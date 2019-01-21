Class {
	#name : 'RwRepositoryComponentProjectTonelReaderVisitor',
	#superclass : 'RwRepositoryComponentProjectReaderVisitor',
	#category : 'Rowan-Components'
}

{ #category : 'package reading' }
RwRepositoryComponentProjectTonelReaderVisitor >> _packageNameFromPackageDir: packageDir [

	"this is not really correct, but it works as a fallback (filetree does not have independent package name)"

	^ (self _readObjectFrom: (packageDir / 'package', 'st')) at: #name
]

{ #category : 'class reading' }
RwRepositoryComponentProjectTonelReaderVisitor >> classExtensionFileExtensions [

	^ #( 'extension' 'st' )
]

{ #category : 'class reading' }
RwRepositoryComponentProjectTonelReaderVisitor >> classFileExtensions [

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
RwRepositoryComponentProjectTonelReaderVisitor >> newMethodDefinitionForClassNamed: className classIsMeta: meta selector: selector category: category source: source [

	self validateMethodProtocol: category  forPackageNamed: self currentPackageDefinition name.
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
    ifTrue: [^  self newClassDefinitionFrom: anArray ].
  "is extension, no definition, but return properties (which includes class name)"
  ^ anArray sixth
]

{ #category : 'package reading' }
RwRepositoryComponentProjectTonelReaderVisitor >> packageExtension [

	^ 'package'
]

{ #category : 'class reading' }
RwRepositoryComponentProjectTonelReaderVisitor >> readClassesFor: packageName packageRoot: packageRoot [

	| classFileExtensions classExtensionFileExtensions |
	currentPackageDefinition := currentProjectDefinition 
		packageNamed: packageName 
		ifAbsent: [ currentProjectDefinition addPackageNamed: packageName ].
	classExtensionFileExtensions := self classExtensionFileExtensions.
	classFileExtensions := self classFileExtensions.
	packageRoot files do: [:file |
		| fileExtensions |
		fileExtensions := file extensions asArray.
		fileExtensions = classFileExtensions
			ifTrue: [ self readClassFile: file inPackage: packageName ]
			ifFalse: [
				fileExtensions = classExtensionFileExtensions
					ifTrue: [ self readClassExtensionFile: file  inPackage: packageName ] ] ]
]

{ #category : 'class reading' }
RwRepositoryComponentProjectTonelReaderVisitor >> readClassExtensionFile: file inPackage: packageName [

	file readStreamDo: [:fileStream |
		| definitions stream classExtensionDef className package |
		stream := ZnBufferedReadStream on: fileStream. "wrap with buffered stream to bypass https://github.com/GemTalk/FileSystemGs/issues/9"
		stream sizeBuffer: 10000. "part of workaround for GemTalk/FileSystemGs#9"
		definitions := (TonelParser on: stream forReader: self) start.
		package := currentProjectDefinition packageNamed: packageName.
		className := (definitions at: 1) at: #name.
		classExtensionDef := package 
			classExtensionDefinitionNamed: className 
			ifAbsent: [ package addClassExtensionNamed: className ].
		((definitions at: 2) at: 1) do: [:mDef |
			classExtensionDef addClassMethodDefinition: mDef ].
		((definitions at: 2) at: 2) do: [:mDef |
			classExtensionDef addInstanceMethodDefinition: mDef ].
		 ].
]

{ #category : 'class reading' }
RwRepositoryComponentProjectTonelReaderVisitor >> readClassFile: file inPackage: packageName [

	file readStreamDo: [:fileStream |
		| definitions stream classDef |
		stream := ZnBufferedReadStream on: fileStream. "wrap with buffered stream to bypass https://github.com/GemTalk/FileSystemGs/issues/9"
		stream sizeBuffer: 10000. "part of workaround for GemTalk/FileSystemGs#9"
		definitions := (TonelParser on: stream forReader: self) start.
		classDef := definitions at: 1.
		self validateClassCategory: classDef category forPackageNamed: self currentPackageDefinition name.
		((definitions at: 2) at: 1) do: [:mDef |
			classDef addClassMethodDefinition: mDef ].
		((definitions at: 2) at: 2) do: [:mDef |
			classDef addInstanceMethodDefinition: mDef ].
		(currentProjectDefinition packageNamed: packageName)
			addClassDefinition: classDef ].
]
