"
Fundamental concepts:

	read			(disk -> def)
	write			(def -> disk)

	install			(def -> image)
	derive			(image -> def)

	load			(disk ->  image)
	commit			(image -> disk)

	project			manages collections of packages
	package			manages collections of definitions
	definitions	manages properties of classes and methods
	repository		manages disk representation of defintions
"
Class {
	#name : 'RwAbstractTool',
	#superclass : 'Object',
	#instVars : [
		'specification'
	],
	#category : 'Rowan-Tode'
}

{ #category : 'commands' }
RwAbstractTool class >> git [
  ^ RwPrjGitTool new
]

{ #category : 'private' }
RwAbstractTool >> _symbolDictionaryForSpecification [

	^ RwGsImage
		newOrExistingSymbolDictionaryNamed:
			(specification platformSpec at: 'gemstone') defaultSymbolDictName
]

{ #category : 'class lookup' }
RwAbstractTool >> definitionsForClassExtensionNamed: className ifFound: foundBlock ifAbsent: absentBlock [

	"there can be multiple class extensions associated with a single class and only one class extension is currently saved"

	^ Rowan image
		loadedClassExtensionsNamed: className
		ifFound: [ :loadedClassExtensionSet | 
			loadedClassExtensionSet
				do: [ :loadedClassExtension | 
					| loadedPackage projectDef packageDef classExtensionDef |
					loadedPackage := loadedClassExtension loadedPackage.
					projectDef := loadedPackage loadedProject asDefinition.
					packageDef := projectDef packageNamed: loadedPackage name.
					classExtensionDef := packageDef classExtensions
						at: loadedClassExtension name.
					foundBlock value: classExtensionDef value: packageDef value: projectDef ].
			^ self ]
		ifAbsent: absentBlock
]

{ #category : 'class lookup' }
RwAbstractTool >> definitionsForClassNamed: className ifFound: foundBlock ifAbsent: absentBlock [

	Rowan image
		loadedClassNamed: className
		ifFound: [ :loadedClass | 
			| loadedPackage projectDef packageDef classDef |
			loadedPackage := loadedClass loadedPackage.
			projectDef := loadedPackage loadedProject asDefinition.
			packageDef := projectDef packageNamed: loadedPackage name.
			classDef := packageDef classDefinitions at: loadedClass name.
			^ foundBlock value: classDef value: packageDef value: projectDef ]
		ifAbsent: absentBlock
]

{ #category : 'class lookup' }
RwAbstractTool >> definitionsForMethod: selector inClassNamed: className ifFound: foundBlock ifAbsent: absentBlock [

	^ Rowan image
		loadedMethod: selector
		inClassNamed: className
		ifFound: [ :loadedMethod | 
			| loadedPackage loadedClass projectDef packageDef classDef classExtensionDef |
			loadedPackage := loadedMethod loadedPackage.
			loadedClass := loadedMethod loadedClass.
			projectDef := loadedPackage loadedProject asDefinition.
			packageDef := projectDef packageNamed: loadedPackage name.
			loadedClass isLoadedClass
				ifTrue: [ classDef := packageDef classDefinitions at: loadedClass name ]
				ifFalse: [ classExtensionDef := packageDef classExtensions at: loadedClass name ].
			foundBlock
				value: loadedMethod asDefinition
				value: classDef
				value: classExtensionDef
				value: packageDef
				value: projectDef.
			^ self ]
		ifAbsent: absentBlock
]

{ #category : 'git' }
RwAbstractTool >> doGitCommit: messageString [
  | gitTool gitRootDir commitMessageFileName status loadedCommitId |
  gitTool := self class git.
  gitRootDir := ServerFileDirectory
    on: specification repoSpec repositoryRootPath.
  commitMessageFileName := gitTool createTmpFileWith: messageString.
  gitTool gitaddIn: gitRootDir with: '.'.
  gitTool gitcommitIn: gitRootDir with: '-a --file=' , commitMessageFileName.
  status := gitTool gitlogIn: gitRootDir with: '-1'.
  Transcript
    cr;
    show: '==============';
    cr;
    show: status.
  ^ status
]

{ #category : 'git' }
RwAbstractTool >> doGitUpdateLoadedCommitId [

	| gitRootDir repoRootPath |
	repoRootPath := specification repoSpec repositoryRootPath.
	repoRootPath
		ifNil: [ 
			"in memory repo without a rootPath is legal"
			^ nil ].
	gitRootDir := ServerFileDirectory on: repoRootPath.
	^ self doGitUpdateLoadedCommitId: gitRootDir
]

{ #category : 'git' }
RwAbstractTool >> doGitUpdateLoadedCommitId: gitRootDir [

	| gitTool loadedCommitId |
	gitTool := self class git.
	loadedCommitId := [ gitTool gitcommitShaIn: gitRootDir ]
		on: Error
		do: [ :ignored | 
			"most likely no commits yet"
			^ nil ].
	^ specification imageSpec loadedCommitId: loadedCommitId
]

{ #category : 'private' }
RwAbstractTool >> fileUtilities [
  ^ Rowan fileUtilities
]

{ #category : 'smalltalk api' }
RwAbstractTool >> help [

	^self manPage asText
]

{ #category : 'man page' }
RwAbstractTool >> manPage [
  self subclassResponsibility: #'manPage'
]

{ #category : 'smalltalk api' }
RwAbstractTool >> specification: aRwSpecification [
  self validate: aRwSpecification.
  ^ specification := aRwSpecification
]

{ #category : 'smalltalk api' }
RwAbstractTool >> specUrl: aString [
  ^ self specification: (RwSpecification fromUrl: aString)
]

{ #category : 'validation' }
RwAbstractTool >> validate: aRwSpecification [
  aRwSpecification specName
    ifNil: [ self error: 'The specification must have a specName specified.' ]
]

{ #category : 'validation' }
RwAbstractTool >> validateForRead: aRwSpecification [
  aRwSpecification repositoryRootPath
    ifNil: [ 
      self
        error:
          'The repositoryRootPath must be defined in the specification: '
            , aRwSpecification specName printString ]
]

{ #category : 'validation' }
RwAbstractTool >> validateForWrite: aRwSpecification [
  aRwSpecification repositoryRootPath
    ifNil: [ 
      self
        error:
          'The repositoryRootPath must be defined in the specification: '
            , aRwSpecification specName printString ]
]
