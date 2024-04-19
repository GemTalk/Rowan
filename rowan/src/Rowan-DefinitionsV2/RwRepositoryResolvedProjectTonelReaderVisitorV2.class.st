Class {
	#name : 'RwRepositoryResolvedProjectTonelReaderVisitorV2',
	#superclass : 'RwRepositoryComponentProjectReaderVisitor',
	#category : 'Rowan-DefinitionsV2'
}

{ #category : 'class file reading' }
RwRepositoryResolvedProjectTonelReaderVisitorV2 class >> readClassFile: file [

	^ self readClassFiles: { file }
]

{ #category : 'class file reading' }
RwRepositoryResolvedProjectTonelReaderVisitorV2 class >> readClassFile: file projectName: projectName packageName: packageName [

	^ self readClassFiles: { file } projectName: projectName packageName: packageName
]

{ #category : 'class file reading' }
RwRepositoryResolvedProjectTonelReaderVisitorV2 class >> readClassFiles: fileArray [

	^ self readClassFiles: fileArray projectName: '___READ_CLASS_Project___' packageName: '___READ_CLASS_Package___'
]

{ #category : 'class file reading' }
RwRepositoryResolvedProjectTonelReaderVisitorV2 class >> readClassFiles: fileArray projectName: projectName packageName: packageName [
	| visitor resolvedProject packageDefinition |
	resolvedProject := RwResolvedProjectV2 new
		projectName: projectName;
		packageConvention: 'Rowan';
		gemstoneSetDefaultSymbolDictNameTo: 'Globals';
		yourself.
	resolvedProject
		addLoadComponentNamed: 'Core'
		comment: 'Temporary project to hold class definitions read from disk'.
	1 to: fileArray size do: [ :index | 
		| file |
		file := fileArray at: index.
		packageDefinition := resolvedProject
			addPackageNamed: packageName , '_' , index asString
			toComponentNamed: 'Core'.
		visitor := self new
			currentProjectDefinition: resolvedProject;
			currentPackageDefinition: packageDefinition;
			_packageConvention: 'Rowan';
			yourself.
		visitor readClassFile: file inPackage: packageDefinition name ].
	^ resolvedProject
]

{ #category : 'package reading' }
RwRepositoryResolvedProjectTonelReaderVisitorV2 >> _packageNameFromPackageDir: packageDir ifAbsent: absentBlock [
	"package.st file is REQUIRED for the to be a legal tonel package directory"

	| tonelPackageFile |
	tonelPackageFile := packageDir / 'package' , 'st'.
	tonelPackageFile exists
		ifFalse: [ ^ absentBlock value ].
	^ ((self _readObjectFrom: tonelPackageFile)
		at: #'name'
		ifAbsent: [ ^ absentBlock value ]) asString
]

{ #category : 'class reading' }
RwRepositoryResolvedProjectTonelReaderVisitorV2 >> checkMethodDefinitions: aClassDef [
  | cls clsName methBlk fakeMethDict pkgName |

  self compileWhileReading ifFalse:[ ^ self "do nothing"  ].

  clsName := aClassDef  name asSymbol .
  (pkgName := currentPackageDefinition name) = 'Filein1C' ifTrue:[ 
     "lookup in GemStone_Legacy_Streams first"
     cls := GemStone_Legacy_Streams at: clsName otherwise: nil.
  ].
  cls ifNil:[   
    (cls := GsCurrentSession currentSession resolveSymbol: clsName ) ifNil:[
       "creating the class not implemented yet"
       Warning signal:'class ' , clsName , ' not found by name lookup'.
       ^ self "can't check syntax on the methods until class is defined"
    ].
  ].
  cls := cls"anAssociation" value.
  methBlk := [ :methDef "a RwMethodDefinition" |
    [
      cls compileMethod: methDef source
      dictionaries: GsCurrentSession currentSession symbolList
      category: methDef protocol asSymbol
      intoMethodDict: fakeMethDict
      intoCategories: nil
      environmentId:  0
    ] on: ( CompileError , CompileWarning ) do:[:ex | 
      ex addText: (RwRepositoryComponentProjectReaderVisitor lineNumberStringForDefinition: methDef ).
      ex pass
    ]
  ].
  fakeMethDict := GsMethodDictionary new .
  aClassDef instanceMethodDefinitions do: methBlk .
  cls := cls class .
  fakeMethDict := GsMethodDictionary new .
  aClassDef classMethodDefinitions do: methBlk .
]

{ #category : 'class reading' }
RwRepositoryResolvedProjectTonelReaderVisitorV2 >> classExtensionFileExtensions [

	^ #( 'extension' 'st' )
]

{ #category : 'class reading' }
RwRepositoryResolvedProjectTonelReaderVisitorV2 >> classFileExtensions [

	^ #( 'class' 'st' )
]

{ #category : 'tonel parser' }
RwRepositoryResolvedProjectTonelReaderVisitorV2 >> definitionForType: aString [
  aString = self class tonelClassLabel
    ifTrue: [ ^ #class ].
  aString = self class tonelExtensionLabel
    ifTrue: [ ^ #classExtension ].
  aString = self class tonelTraitLabel
    ifTrue: [ ^ #trait ].
  RwTonelParseError signal: 'Unknown type declaration.'
]

{ #category : 'tonel parser' }
RwRepositoryResolvedProjectTonelReaderVisitorV2 >> newMethodDefinitionForClassNamed: className classIsMeta: meta selector: selector category: protocol source: source [

	| methodDef |
	methodDef := (currentTraitDefinition ifNil: [ RwMethodDefinition ] ifNotNil: [ RwTraitMethodDefinition ])
		newForSelector: selector 
			protocol: protocol 
			source: source.
	self 
		validateMethodDefinitionProtocol: methodDef 
			className: className
			isMeta: meta
			forPackageNamed: self currentPackageDefinition name.
	^ methodDef
]

{ #category : 'tonel parser' }
RwRepositoryResolvedProjectTonelReaderVisitorV2 >> newTypeDefinitionFrom: anArray [
  | typeClass |
  typeClass := self definitionForType: anArray fourth.
  typeClass == #class
    ifTrue: [ ^  self newClassDefinitionFrom: anArray ].
  typeClass == #classExtension
	ifTrue: [ ^ self newClassExtensionDefinitionFrom: anArray ].
  typeClass == #trait
	ifTrue: [ ^ self newTraitDefinitionFrom: anArray ].
  ^ self error: 'unexpected typeClass: ', typeClass printString
]

{ #category : 'package reading' }
RwRepositoryResolvedProjectTonelReaderVisitorV2 >> packageExtension [

	^ ''
]

{ #category : 'class reading' }
RwRepositoryResolvedProjectTonelReaderVisitorV2 >> readClassesFor: packageName packageRoot: packageRoot [
	| classFileExtensions classExtensionFileExtensions traitFileExtensions trace |
	trace := Rowan projectTools trace.
	currentPackageDefinition := currentProjectDefinition
		packageNamed: packageName
		ifAbsent: [ currentProjectDefinition addRawPackageNamed: packageName ].
	classExtensionFileExtensions := self classExtensionFileExtensions.
	classFileExtensions := self classFileExtensions.
	traitFileExtensions := self traitFileExtensions.
	packageRoot files
		do: [ :file | 
			| fileExtensions |
			trace trace: '--- reading class ' , file asString.
			fileExtensions := file extensions asArray.
			fileExtensions = classFileExtensions
				ifTrue: [ self readClassFile: file inPackage: packageName ]
				ifFalse: [ 
					fileExtensions = classExtensionFileExtensions
						ifTrue: [ self readClassExtensionFile: file inPackage: packageName ]
						ifFalse: [ 
							fileExtensions = traitFileExtensions
								ifTrue: [ self readTraitFile: file inPackage: packageName ] ] ] ]
]

{ #category : 'class reading' }
RwRepositoryResolvedProjectTonelReaderVisitorV2 >> readClassExtensionFile: file inPackage: packageName [

	| fileReference |
	fileReference := file asFileReference.
	fileReference readStreamDo: [:fileStream |
		[ | definitions |
		  definitions := (RwTonelParser on: fileStream forReader: self) start.
		  ((definitions at: 2) at: 1) do: [:mDef |
			mDef propertyAt: '_gsFileName' put:  fileReference fullName.
			  currentClassExtension addClassMethodDefinition: mDef ].
		  ((definitions at: 2) at: 2) do: [:mDef |
			mDef propertyAt: '_gsFileName' put:  fileReference fullName.
			  currentClassExtension addInstanceMethodDefinition: mDef ] .
      self checkMethodDefinitions: currentClassExtension .
    ] on: ( STONReaderError , RwTonelParseError, Error) do:[:ex |
      ex addText: (self class lineNumberStringForOffset: fileStream position fileName: fileReference fullName).
      ex pass .
    ].
  ].
]

{ #category : '(as yet unclassified)' }
RwRepositoryResolvedProjectTonelReaderVisitorV2 >> readClassFile: file inPackage: packageName [
	| fileReference |
	fileReference := file asFileReference.
	fileReference
		readStreamDo: [ :fileStream | 
			[ 
			| definitions clsDef projectDef |
			definitions := (RwTonelParser on: fileStream forReader: self) start.
			clsDef := currentClassDefinition
				ifNotNil: [ :def | 
					currentClassExtension
						ifNotNil: [ Error signal: 'both a class definition and extension in file ' , fileReference fullName ].
					currentTraitDefinition
						ifNotNil: [ Error signal: 'both a trait definition and class definition in file ' , fileReference fullName ].
					def ]
				ifNil: [ currentClassExtension ].
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
			projectDef := currentProjectDefinition packageNamed: packageName.
			currentClassDefinition ifNotNil: [ projectDef addClassDefinition: clsDef ] ]
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

{ #category : '(as yet unclassified)' }
RwRepositoryResolvedProjectTonelReaderVisitorV2 >> readTraitFile: file inPackage: packageName [
	| fileReference |
	fileReference := file asFileReference.
	fileReference
		readStreamDo: [ :fileStream | 
			[ 
			| definitions traitDef projectDef |
			definitions := (RwTonelParser on: fileStream forReader: self) start.
			traitDef := currentTraitDefinition
				ifNotNil: [ :def | 
					currentClassDefinition
						ifNotNil: [ 
							Error
								signal:
									'both a trait definition and class definition in file ' , fileReference fullName ].
					currentClassExtension
						ifNotNil: [ Error signal: 'both a trait definition and class extension in file ' , fileReference fullName ].
					def ]
				ifNil: [ currentClassExtension ].
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

{ #category : 'class reading' }
RwRepositoryResolvedProjectTonelReaderVisitorV2 >> traitFileExtensions [

	^ #( 'trait' 'st' )
]
