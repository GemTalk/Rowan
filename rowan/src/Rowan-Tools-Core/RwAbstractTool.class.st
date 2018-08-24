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
	#category : 'Rowan-Tools-Core'
}

{ #category : 'commands' }
RwAbstractTool class >> git [

	^ RwGitTool new
]

{ #category : 'private' }
RwAbstractTool >> _realpath_source [
	"mac and linux compatible implementation of readlink -f"

	"all on one line because that's what our commandline runner wants"

	^'realpath() { OURPWD=$PWD; cd "$(dirname "$1")"; LINK=$(readlink "$(basename "$1")"); while [ "$LINK" ]; do cd "$(dirname "$LINK")"; LINK=$(readlink "$(basename "$1")"); done; REALPATH="$PWD/$(basename "$1")"; cd "$OURPWD"; echo "$REALPATH"; }'

]

{ #category : 'private' }
RwAbstractTool >> _symbolDictionaryForSpecification [

	^ Rowan image
		newOrExistingSymbolDictionaryNamed:
			(specification platformSpec at: 'gemstone') defaultSymbolDictName
]

{ #category : 'class lookup' }
RwAbstractTool >> definitionsForClassExtensionNamed: className ifFound: foundBlock ifAbsent: absentBlock [

	"validate loaded class extension registry structure"

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
RwAbstractTool >> definitionsForMethod: selector inClassNamed: className isMeta: isMeta ifFound: foundBlock ifAbsent: absentBlock [

	^ Rowan image
		loadedMethod: selector
		inClassNamed: className
		isMeta: isMeta
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

	| gitTool gitRootPath commitMessageFileName status |
	gitTool := Rowan gitTools.
	gitRootPath := specification repoSpec repositoryRootPath.
	commitMessageFileName := gitTool createTmpFileWith: messageString.
	gitTool gitaddIn: gitRootPath with: '-A .'.
	gitTool gitcommitIn: gitRootPath with: '--file=' , commitMessageFileName.
	status := gitTool gitlogIn: gitRootPath with: '-1'.
	Transcript
		cr;
		show: '==============';
		cr;
		show: status.
	^ status

]

{ #category : 'git' }
RwAbstractTool >> doGitUpdateLoadedCommitId [

	| repoRootPath |
	repoRootPath := specification repoSpec repositoryRootPath.
	repoRootPath
		ifNil: [ 
			"in memory repo without a rootPath is legal"
			^ nil ].
	^ self doGitUpdateLoadedCommitId: repoRootPath
]

{ #category : 'git' }
RwAbstractTool >> doGitUpdateLoadedCommitId: gitRootPath [

	| gitTool loadedCommitId |
	gitTool := Rowan gitTools.
	loadedCommitId := [ gitTool gitcommitShaIn: gitRootPath ]
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

{ #category : 'bash utilities' }
RwAbstractTool >> readlink: filepath [

	"resolve (possible) symbolic links in filepath and return an absolute path"
	"NOTE: may need alternate solution on OSX"

	| command |
	command := self _realpath_source, '; realpath ' , filepath.
	^Rowan gitTools performOnServer: command logging: true

]

{ #category : 'bash utilities' }
RwAbstractTool >> readlinkX: filepath [

	"resolve (possible) symbolic links in filepath and return an absolute path"
	"NOTE: may need alternate solution on OSX"

	| command |
	command := self _realpath_source, '; realpath ' , filepath.
	^Rowan gitTools performOnServer: command logging: true

]

{ #category : 'bash utilities' }
RwAbstractTool >> readlinkY: filepath [

	"resolve (possible) symbolic links in filepath and return an absolute path"
	"NOTE: may need alternate solution on OSX"

	| command |
	command := 'set -e; readlink -f ', filepath.
	^Rowan gitTools performOnServer: command logging: true

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
