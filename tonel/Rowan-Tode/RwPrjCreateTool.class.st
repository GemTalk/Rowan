Class {
	#name : 'RwPrjCreateTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tode'
}

{ #category : 'private' }
RwPrjCreateTool >> _createProjectRepositoryDirectory: repoPath useGit: useGit abort: abortBlock [

	| utils gitTool |
	utils := self fileUtilities.
	gitTool := Rowan projectTools git.
	(utils directoryExists: repoPath)
		ifTrue: [ 
			(self
				confirm:
					'There is already a directory named ' , repoPath printString
						, '. The contents of the directory will be deleted if you continue.')
				== true
				ifFalse: [ ^ abortBlock value ].
			utils deleteAll: repoPath ]
		ifFalse: [ utils ensureDirectoryExists: repoPath ].
	useGit
		ifTrue: [ 
			| gitRepoDir |
			gitRepoDir := ServerFileDirectory on: repoPath.
			(gitTool gitPresentIn: gitRepoDir)
				ifFalse: [ 
					"create a git repository"
					gitTool gitinitIn: gitRepoDir with: '' ] ]
]

{ #category : 'smalltalk api' }
RwPrjCreateTool >> createMetacelloProject: projectName format: format root: rootPath repoPath: repoPath useGit: useGit [
  self halt: 'not implemented ... yet'
]

{ #category : 'smalltalk api' }
RwPrjCreateTool >> createPackageProject: projectName format: format root: rootPath repoPath: repoPath useGit: useGit [

	| projectSpec |
	projectSpec := self class spec newPackageSpecNamed: projectName useGit: useGit.
	^ self
		createPackageProjectFor: projectSpec
		format: format
		root: rootPath
		repoPath: repoPath
]

{ #category : 'smalltalk api' }
RwPrjCreateTool >> createPackageProjectFor: projectSpec format: format root: rootPath repoPath: repoPath [

	| projectName utils rootRepoPath dirPath useGit |
	projectName := projectSpec specName.
	useGit := projectSpec useGit.
	utils := self fileUtilities.
	rootRepoPath := rootPath , utils pathNameDelimiter , projectName.
	self
		_createProjectRepositoryDirectory: rootRepoPath
		useGit: useGit
		abort: [ ^ self ].
	dirPath := rootRepoPath , utils pathNameDelimiter , repoPath.
	utils ensureDirectoryExists: dirPath.
	utils
		writeStreamFor: '.cypress'
		in: dirPath
		do: [ :fileStream | fileStream nextPutAll: '{ #format : ' , format printString , '}' ].
	dirPath := rootRepoPath , utils pathNameDelimiter , projectSpec specsPath.
	utils ensureDirectoryExists: dirPath.
	projectSpec
		repositoryRootPath: rootRepoPath;
		repoPath: repoPath;
		repositoryUrl: 'cypress:' , rootRepoPath , '/' , repoPath , '/'.
	projectSpec copy export.
	^ projectSpec
]

{ #category : 'man page' }
RwPrjCreateTool >> manPage [
  "TDRowanTool install"

  ^ TDManPage
    commandName: 'create'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  rep create - Create a new repository and project
SYNOPSIS
  rep create --format=[disk|cypress|filetree|tonel] --type=[metacello|package] \
             [--git] --root=<path-to-root-directory> <project-name>
DESCRIPTION
  New repository is created in <path-to-root-directory>. If --git is specified
  a git repository is created.

EXAMPLES
  rep create --format=tonel --type=package --root=$GS_HOME/share/repos Foo
  rep create --git --format=tonel --type=metacello --root=$GS_HOME/share/repos Bar
'
]
