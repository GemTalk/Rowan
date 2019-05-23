project writing
processProject: aProjectModification
	"confirm that the project source is written in Tonel format"

	| format propertiesFile |
	packageDefFileNameMap := self _createFileNameMapForClassesOrPackages:  aProjectModification after packages.
	packageDefBeforeFileNameMap := self _createFileNameMapForClassesOrPackages:  aProjectModification before packages.

	(format := self _repositoryFormatFor:  aProjectModification after packagesRoot) = 'tonel' ifFalse: [ self error: 'expected tonel format repository, instead format is ', format printString ].

	propertiesFile := aProjectModification after packagesRoot /  'properties.st'.
	propertiesFile exists
		ifFalse: [
			propertiesFile writeStreamDo: [ :fileStream | 
				fileStream 
					nextPutAll: '{ #format : ' , format printString , '}';
					lf;
					nextPutAll: '{ #convention : ' , currentProjectDefinition packageConvention printString , '}';
					yourself ] ].

	super processProject: aProjectModification.