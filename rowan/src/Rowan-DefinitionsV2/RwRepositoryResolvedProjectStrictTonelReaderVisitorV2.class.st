Class {
	#name : 'RwRepositoryResolvedProjectStrictTonelReaderVisitorV2',
	#superclass : 'RwRepositoryResolvedProjectTonelReaderVisitorV2',
	#category : 'Rowan-DefinitionsV2'
}

{ #category : 'class reading' }
RwRepositoryResolvedProjectStrictTonelReaderVisitorV2 >> readClassExtensionFile: file inPackage: packageName [
	| fileReference |
	fileReference := file asFileReference.
	fileReference
		readStreamDo: [ :fileStream | 
			[ 
			| definitions packageDef tonelParser clsNameFromFile |
			tonelParser := RwStrictTonelParser
				on: fileStream
				filePath: fileReference fullName
				forReader: self.
			definitions := tonelParser start.
			((definitions at: 2) at: 1)
				do: [ :mDef | 
					mDef propertyAt: '_gsFileName' put: fileReference fullName.
					currentClassExtension addClassMethodDefinition: mDef ].
			clsNameFromFile := fileReference base asFileReference base.
			currentClassExtension name = clsNameFromFile
				ifFalse: [ 
					Error
						signal:
							'The base name of a class extension file ('
								, fileReference base asFileReference base printString
								, ') does not match the name of the class  extension ('
								, currentClassExtension name printString , ')' ].
			((definitions at: 2) at: 2)
				do: [ :mDef | 
					mDef propertyAt: '_gsFileName' put: fileReference fullName.
					currentClassExtension addInstanceMethodDefinition: mDef ].
			self checkMethodDefinitions: currentClassExtension.
			packageDef := currentProjectDefinition packageNamed: packageName.
			currentClassExtension
				ifNotNil: [ packageDef addClassExtensionDefinition: currentClassExtension ] ]
				on: STONReaderError , RwTonelParseError , Error
				do: [ :ex | 
					ex
						addText:
							(self class
								lineNumberStringForOffset: fileStream position
								fileName: fileReference fullName).
					ex pass ] ]
]

{ #category : 'class reading' }
RwRepositoryResolvedProjectStrictTonelReaderVisitorV2 >> readClassFile: file inPackage: packageName [
	| fileReference |
	fileReference := file asFileReference.
	fileReference
		readStreamDo: [ :fileStream | 
			[ 
			| definitions clsDef packageDef tonelParser clsNameFromFile |
			tonelParser := RwStrictTonelParser
				on: fileStream
				filePath: fileReference fullName
				forReader: self.
			definitions := tonelParser start.
			clsDef := currentClassDefinition
				ifNotNil: [ :def | 
					currentClassExtension
						ifNotNil: [ 
							Error
								signal:
									'both a class definition and extension in file ' , fileReference fullName ].
					currentTraitDefinition
						ifNotNil: [ 
							Error
								signal:
									'both a trait definition and class definition in file ' , fileReference fullName ].
					def ]
				ifNil: [ currentClassExtension ].
			clsNameFromFile := fileReference base asFileReference base.
			clsDef name = clsNameFromFile
				ifFalse: [ 
					Error
						signal:
							'The base name of a class file ('
								, fileReference base asFileReference base printString
								, ') does not match the name of the class definition ('
								, clsDef name printString , ')' ].
			self validateClassCategory: clsDef forPackageNamed: packageName.
			((definitions at: 2) at: 1)
				do: [ :mDef | 
					mDef propertyAt: '_gsFileName' put: fileReference fullName.
					clsDef addClassMethodDefinition: mDef ].
			((definitions at: 2) at: 2)
				do: [ :mDef | 
					mDef propertyAt: '_gsFileName' put: fileReference fullName.
					clsDef addInstanceMethodDefinition: mDef ].
			self checkMethodDefinitions: clsDef.
			packageDef := currentProjectDefinition packageNamed: packageName.
			currentClassDefinition ifNotNil: [ packageDef addClassDefinition: clsDef ].
			currentClassExtension
				ifNotNil: [ packageDef addClassExtensionDefinition: clsDef ] ]
				on: STONReaderError , RwTonelParseError , Error
				do: [ :ex | 
					(ex messageText includesString: 'near line')
						ifFalse: [ 
							"only add line number info, in absence of existing line number info"
							ex
								addText:
									(self class
										lineNumberStringForOffset: fileStream position
										fileName: fileReference fullName) ].
					ex pass ] ]
]

{ #category : 'class reading' }
RwRepositoryResolvedProjectStrictTonelReaderVisitorV2 >> readClassOrTraitFile: file inPackage: packageName [
	| fileReference |
	fileReference := file asFileReference.
	fileReference
		readStreamDo: [ :fileStream | 
			[ 
			| definitions clsOrTraitDef packageDef tonelParser |
			tonelParser := RwStrictTonelParser
				on: fileStream
				filePath: fileReference fullName
				forReader: self.
			definitions := tonelParser start.
			clsOrTraitDef := currentClassDefinition
				ifNotNil: [ :def | 
					currentClassExtension
						ifNotNil: [ 
							Error
								signal:
									'both a class definition and extension in file ' , fileReference fullName ].
					currentTraitDefinition
						ifNotNil: [ 
							Error
								signal:
									'both a trait definition and class definition in file ' , fileReference fullName ].
					def ]
				ifNil: [ 
					currentClassExtension
						ifNotNil: [ :def | 
							currentTraitDefinition
								ifNotNil: [ 
									Error
										signal:
											'both a trait definition and class extension definition in file '
												, fileReference fullName ].
							def ]
						ifNil: [ currentTraitDefinition ] ].
clsOrTraitDef name = fileReference base asFileReference base
ifFalse: [Error signal: 'The base name of a class/trait file (', fileReference base asFileReference base printString, ') does not match the name of the class/trait definition (', clsOrTraitDef name printString, ')' ].
			self validateClassCategory: clsOrTraitDef forPackageNamed: packageName.
			((definitions at: 2) at: 1)
				do: [ :mDef | 
					mDef propertyAt: '_gsFileName' put: fileReference fullName.
					clsOrTraitDef addClassMethodDefinition: mDef ].
			((definitions at: 2) at: 2)
				do: [ :mDef | 
					mDef propertyAt: '_gsFileName' put: fileReference fullName.
					clsOrTraitDef addInstanceMethodDefinition: mDef ].
			self checkMethodDefinitions: clsOrTraitDef.
			packageDef := currentProjectDefinition packageNamed: packageName.
			currentTraitDefinition
				ifNotNil: [ packageDef addTraitDefinition: clsOrTraitDef ].
			currentClassDefinition
				ifNotNil: [ packageDef addClassDefinition: clsOrTraitDef ].
			currentClassExtension
				ifNotNil: [ packageDef addClassExtensionDefinition: clsOrTraitDef ] ]
				on: STONReaderError , RwTonelParseError , Error
				do: [ :ex | 
					(ex messageText includesString: 'near line')
						ifFalse: [ 
							"only add line number info, in absence of existing line number info"
							ex
								addText:
									(self class
										lineNumberStringForOffset: fileStream position
										fileName: fileReference fullName) ].
					ex pass ] ]
]

{ #category : 'class reading' }
RwRepositoryResolvedProjectStrictTonelReaderVisitorV2 >> readTraitFile: file inPackage: packageName [
	| fileReference |
	fileReference := file asFileReference.
	fileReference
		readStreamDo: [ :fileStream | 
			[ 
			| definitions traitDef projectDef tonelParser |
			tonelParser := RwStrictTonelParser on: fileStream filePath: fileReference fullName forReader: self.
			definitions := tonelParser start.
			traitDef := currentTraitDefinition
				ifNotNil: [ :def | 
					currentClassDefinition
						ifNotNil: [ 
							Error
								signal:
									'both a trait definition and class definition in file ' , fileReference fullName ].
					def ]
				ifNil: [ currentTraitDefinition ].
traitDef name = fileReference base asFileReference base
ifFalse: [Error signal: 'The base name of a trait file (', fileReference base asFileReference base printString, ') does not match the name of the trait definition (', traitDef name printString, ')' ].
			((definitions at: 2) at: 1)
				do: [ :mDef | 
					mDef propertyAt: '_gsFileName' put: fileReference fullName.
					traitDef addClassMethodDefinition: mDef ].
			((definitions at: 2) at: 2)
				do: [ :mDef | 
					mDef propertyAt: '_gsFileName' put: fileReference fullName.
					traitDef addInstanceMethodDefinition: mDef ].
			self checkMethodDefinitions: traitDef.
			projectDef := currentProjectDefinition packageNamed: packageName.
			currentTraitDefinition ifNotNil: [ projectDef addTraitDefinition: traitDef ] ]
				on: STONReaderError , RwTonelParseError , Error
				do: [ :ex | 
					(ex messageText includesString: 'near line')
						ifFalse: [ 
							"only add line number info, in absence of existing line number info"
							ex
								addText:
									(self class
										lineNumberStringForOffset: fileStream position
										fileName: fileReference fullName) ].
					ex pass ] ]
]
