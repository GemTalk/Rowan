Class {
	#name : 'RwRepositoryResolvedProjectTonelReaderVisitorV2',
	#superclass : 'RwRepositoryComponentProjectTonelReaderVisitor',
	#category : 'Rowan-DefinitionsV2'
}

{ #category : 'class reading' }
RwRepositoryResolvedProjectTonelReaderVisitorV2 >> readClassesFor: packageName packageRoot: packageRoot [

	| classFileExtensions classExtensionFileExtensions trace |
  trace := SessionTemps current at: #ROWAN_TRACE otherwise: nil .
	currentPackageDefinition := currentProjectDefinition _projectDefinition 
		packageNamed: packageName 
		ifAbsent: [ currentProjectDefinition  _projectDefinition addRawPackageNamed: packageName ].
	classExtensionFileExtensions := self classExtensionFileExtensions.
	classFileExtensions := self classFileExtensions.
	packageRoot files do: [:file |
		| fileExtensions |
    trace == #gciLogServer ifTrue:[ GsFile gciLogServer: '--- reading ', file asString ].
		fileExtensions := file extensions asArray.
		fileExtensions = classFileExtensions
			ifTrue: [ self readClassFile: file inPackage: packageName ]
			ifFalse: [
				fileExtensions = classExtensionFileExtensions
					ifTrue: [ self readClassExtensionFile: file  inPackage: packageName ] ] ]
]
