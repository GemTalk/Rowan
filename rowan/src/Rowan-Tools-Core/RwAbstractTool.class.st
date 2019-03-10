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
RwAbstractTool >> _sh_realpath_source [

	"https://github.com/mkropat/sh-realpath/blob/master/realpath.sh"

	"all on one line because that's what our perform on server call wants - I think:)"

	^'realpath() { canonicalize_path "$(resolve_symlinks "$1")"; }; resolve_symlinks() { local dir_context path ; path=$(readlink -- "$1"); if [ $? -eq 0 ]; then dir_context=$(dirname -- "$1"); resolve_symlinks "$(_prepend_path_if_relative "$dir_context" "$path")"; else printf ''%s\n'' "$1"; fi; }; _prepend_path_if_relative() { case "$2" in /* ) printf ''%s\n'' "$2" ;; * ) printf ''%s\n'' "$1/$2" ;; esac; }; canonicalize_path() { if [ -d "$1" ]; then _canonicalize_dir_path "$1"; else _canonicalize_file_path "$1"; fi; }; _canonicalize_dir_path() { (cd "$1" 2>/dev/null && pwd -P); }; _canonicalize_file_path() { local dir file; dir=$(dirname -- "$1"); file=$(basename -- "$1"); (cd "$dir" 2>/dev/null && printf ''%s/%s\n'' "$(pwd -P)" "$file"); }'
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
	command := self _sh_realpath_source, '; realpath ' , filepath.
	^Rowan gitTools performOnServer: command logging: true
]

{ #category : 'smalltalk api' }
RwAbstractTool >> specification: aRwSpecification [
  ^ specification := aRwSpecification
]

{ #category : 'smalltalk api' }
RwAbstractTool >> specUrl: aString [
  ^ self specification: (RwSpecification fromUrl: aString)
]
