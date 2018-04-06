Class {
	#name : 'TDRowanProjectBrowser',
	#superclass : 'TDAbstractRowanToolBuilder',
	#instVars : [
		'projects',
		'projectBlock',
		'selectedProjectName',
		'projectTool'
	],
	#category : 'Rowan-Tode'
}

{ #category : 'menu actions' }
TDRowanProjectBrowser >> browseProjectClassesMenuAction: listElement selectionIndex: selectionIndex [

	| projectDef block |
	selectionIndex = 0
		ifTrue: [ ^ false ].
	projectDef := self projects at: selectionIndex.
	block := [ projectDef classDefinitions ].
	^ (listElement topez toolInstanceFor: 'browse')
		browseRowanClassesBlock: block
		windowName: #'rowanClasses'
		location: #'app4'
		label: 'Classes in ' , projectDef projectName
]

{ #category : 'menu actions' }
TDRowanProjectBrowser >> browseProjectPackagesMenuAction: listElement selectionIndex: selectionIndex [

	| projectDef block |
	selectionIndex = 0
		ifTrue: [ ^ false ].
	projectDef := self projects at: selectionIndex.
	block := [ 
	projectDef packageDefinitions
		sortWithBlock: [ :a :b | a packageName <= b packageName ] ].
	^ (TDRowanPackageBrowser new
		topez: topez;
		packageBlock: block;
		projectDef: projectDef;
		windowLabel: projectDef projectName , ' Packages';
		yourself) open
]

{ #category : 'tools' }
TDRowanProjectBrowser >> clientList: miniTool listElement: listElement [

	| max labels selectedProjectIndex projectList count |
	theMiniTool := miniTool.
	projects := projectBlock value.
	labels := {}.
	selectedProjectIndex := nil.
	count := 1.
	max := 0.
	projects
		do: [ :projectDef | 
			| projectName emphasis sha repostioryCommitId loadedCommitId |
			projectName := projectDef projectName.
			selectedProjectName = projectName
				ifTrue: [ selectedProjectIndex := count ].
			projectDef isDirty
				ifTrue: [ projectName := '+ ' , projectName ].
			count := count + 1.
			loadedCommitId := projectDef loadedCommitId.
			repostioryCommitId := projectDef repositoryCommitId.
			(loadedCommitId isEmpty or: [ loadedCommitId = repostioryCommitId ])
				ifTrue: [ 
					emphasis := {(TextEmphasis bold)}.
					sha := loadedCommitId ]
				ifFalse: [ 
					emphasis := {(TextEmphasis bold).
					(TextColor red)}.
					sha := loadedCommitId , ' ^ ' , repostioryCommitId ].
			labels
				add:
					{projectName.
					sha.
					emphasis}.
			max := max max: projectName size ].
	max := max + 2.
	projectList := labels
		collect: [ :ar | 
			| nm textAttributes sha stream |
			stream := WriteStream on: String new.
			nm := ar at: 1.
			sha := ar at: 2.
			textAttributes := ar at: 3.
			stream
				nextPutAll: nm;
				space: max - nm size;
				nextPutAll: sha.
			Text string: stream contents attributes: textAttributes ].
	^ projectList -> selectedProjectIndex
]

{ #category : 'menu actions' }
TDRowanProjectBrowser >> commitLogMenuAction: listElement selectionIndex: selectionIndex [

	| projectDef logString |
	selectionIndex = 0
		ifTrue: [ ^ false ].
	projectDef := self projects at: selectionIndex.
	logString := Rowan projectTools log
		commitLogProjectNamed: projectDef projectName
		limit: 100.
	logString
		editUsing:
			((TDEditorSpec topez: topez editorAspect: #'edit')
				windowName: #'gitStatus';
				yourself).
	^ true
]

{ #category : 'menu actions' }
TDRowanProjectBrowser >> commitProjectMenuAction: listElement selectionIndex: selectionIndex [

	| projectDef commitMessage |
	selectionIndex = 0
		ifTrue: [ ^ false ].
	projectDef := self projects at: selectionIndex.
	commitMessage := (GsMultiLineTextInteraction
		prompt: 'Please enter a commit message for project: ' , projectDef projectName
		template: '') signal.
	commitMessage
		ifNil: [ 
			Transcript
				cr;
				show: 'Project save aborted...'.
			^ false ].
	Rowan projectTools write writeProjectNamed: projectDef projectName.
	Rowan projectTools commit
		commitProjectNamed: projectDef projectName
		message: commitMessage.
	listElement topez commitTransaction.
	^ #'refreshView'
]

{ #category : 'menu actions' }
TDRowanProjectBrowser >> deleteAllProjectsMenuAction: listElement selectionIndex: selectionIndex [

	"as long as all of the projects in the project tests have been created by tests, the method is safe to use"

	| user symList |
	user := System myUserProfile.
	symList := user symbolList.
	projects := projectBlock value.
	projects
		do: [ :projectDef | 
			| spec dict platformSpec symDictNames loadedProject |
			loadedProject := projectDef loadedProject.
			spec := loadedProject handle.
			platformSpec := spec platformSpec at: 'gemstone'.
			symDictNames := Set with: 'UnmanagedPackages'.
			symDictNames add: platformSpec defaultSymbolDictName.
			platformSpec packageNameToPlatformPropertiesMap values
				do: [ :packageProperties | 
					packageProperties
						at: 'symbolDictName'
						ifPresent: [ :name | symDictNames add: name ] ].
			#('Globals' 'UserGlobals' 'Published')
				do: [ :reservedName | symDictNames remove: reservedName ifAbsent: [  ] ].
			symDictNames
				do: [ :symDictName | 
					(#('Globals' 'UserGlobals' 'Published') includes: symDictName asString)
						ifTrue: [ self halt ].

					dict := symList objectNamed: symDictName.
					dict
						ifNotNil: [ 
							| index |
							(dict at: GsPackagePolicy globalName otherwise: nil)
								ifNotNil: [ :policy | policy disable ].
							index := symList indexOf: dict.
							index > 0
								ifTrue: [ user removeDictionaryAt: index ] ] ].
			Rowan image _removeLoadedProject: loadedProject ].
	GsPackagePolicy current refreshSessionMethodDictionary.
	projects := projectBlock value.
	^ #'refresh'
]

{ #category : 'menu actions' }
TDRowanProjectBrowser >> diffMenuAction: listElement selectionIndex: selectionIndex [

	| projectDef diffText |
	selectionIndex = 0
		ifTrue: [ ^ false ].
	projectDef := self projects at: selectionIndex.
	diffText := Rowan projectTools diff diffForProjectName: projectDef projectName.
	diffText
		editUsing:
			((TDEditorSpec topez: topez editorAspect: #'edit')
				windowName: #'mcDiff';
				yourself).
	^ #'refreshView'
]

{ #category : 'menu actions (git)' }
TDRowanProjectBrowser >> gitBranchesMenuAction: listElement selectionIndex: selectionIndex [

	| projectDef gitRepoPath gitTool response |
	selectionIndex = 0
		ifTrue: [ ^ false ].
	projectDef := self projects at: selectionIndex.
	gitRepoPath := projectDef repositoryRootPath.
	gitTool := Rowan projectTools git.
	response := gitTool gitbranchIn: gitRepoPath with: '-a -v'.
	response
		editUsing:
			((TDEditorSpec topez: topez editorAspect: #'edit')
				windowName: #'gitStatus';
				yourself).
	^ true
]

{ #category : 'menu actions (git)' }
TDRowanProjectBrowser >> gitCheckoutMenuAction: listElement selectionIndex: selectionIndex [

	| projectDef gitRepoPath gitTool response checkoutArgs |
	selectionIndex = 0
		ifTrue: [ ^ false ].
	projectDef := self projects at: selectionIndex.
	gitRepoPath := projectDef repositoryRootPath.
	checkoutArgs := (GsTextInteraction
		prompt: 'Enter checkout commit or branch'
		template: 'branch | -b newBranch commitish | -b newBranch | args') signal.
	checkoutArgs ifNil: [ ^ false ].
	gitTool := Rowan projectTools git.
	response := gitTool gitcheckoutIn: gitRepoPath with: checkoutArgs.
	response
		editUsing:
			((TDEditorSpec topez: topez editorAspect: #'edit')
				windowName: #'gitStatus';
				yourself).
	^ #'refreshView'
]

{ #category : 'menu actions (git)' }
TDRowanProjectBrowser >> gitPullProjectMenuAction: listElement selectionIndex: selectionIndex [

	| rowanProject response gitRootPath gitTool |
	selectionIndex = 0
		ifTrue: [ ^ false ].
	rowanProject := (self projects at: selectionIndex) rowanProject.
	gitRootPath := rowanProject repositoryRootPath.
	gitTool := Rowan projectTools git.
	response := gitTool
		gitpullIn: gitRootPath
		remote: rowanProject remote
		branch: rowanProject currentBranchName.
	response
		editUsing:
			((TDEditorSpec topez: topez editorAspect: #'edit')
				windowName: #'gitStatus';
				yourself).
	^ #'refreshView'
]

{ #category : 'menu actions (git)' }
TDRowanProjectBrowser >> gitPushProjectMenuAction: listElement selectionIndex: selectionIndex [

	| rowanProject response gitRootPath gitTool |
	selectionIndex = 0
		ifTrue: [ ^ false ].
	rowanProject := (self projects at: selectionIndex) rowanProject.
	gitRootPath := rowanProject repositoryRootPath.
	gitTool := Rowan projectTools git.
	response := gitTool
		gitpushIn: gitRootPath
		remote: rowanProject remote
		branch: rowanProject currentBranchName.
	response
		editUsing:
			((TDEditorSpec topez: topez editorAspect: #'edit')
				windowName: #'gitStatus';
				yourself).
	^ #'refreshView'
]

{ #category : 'menu actions (git)' }
TDRowanProjectBrowser >> gitStatusMenuAction: listElement selectionIndex: selectionIndex [

	| rowanProject status report commitLog gitTool gitRootPath |
	selectionIndex = 0
		ifTrue: [ ^ false ].
	rowanProject := (self projects at: selectionIndex) rowanProject.
	report := '======== Modifications ========' copy.
	gitRootPath := rowanProject repositoryRootPath.
	gitTool := Rowan projectTools git.
	status := gitTool gitstatusIn: gitRootPath with: '--porcelain'.
	commitLog := gitTool
		gitlogFull: rowanProject repositoryCommitId
		limit: 1
		gitRepoDirectory: gitRootPath.
	report
		add: String lf;
		add: status;
		add: String lf;
		add: '======== Last commit ========';
		add: String lf;
		add: commitLog.
	report
		editUsing:
			((TDEditorSpec topez: listElement topez editorAspect: #'edit')
				windowName: #'commitLog';
				windowLabel: 'Status for ' , rowanProject name;
				yourself).
	^ true
]

{ #category : 'menu actions' }
TDRowanProjectBrowser >> inspectLoadedProjectMenuAction: listElement selectionIndex: selectionIndex [

	| projectDef |
	selectionIndex = 0
		ifTrue: [ ^ false ].
	projectDef := self projects at: selectionIndex.
	projectDef loadedProject inspect.
	^ true
]

{ #category : 'menu actions' }
TDRowanProjectBrowser >> inspectProjectSpecMenuAction: listElement selectionIndex: selectionIndex [

	| projectDef |
	selectionIndex = 0
		ifTrue: [ ^ false ].
	projectDef := self projects at: selectionIndex.
	(Rowan projectTools spec specForProjectNamed: projectDef projectName) inspect.
	^ true
]

{ #category : 'tools' }
TDRowanProjectBrowser >> itemSelected: miniTool listElement: listElement selectedIndex: index shiftPressed: shiftPressed [

	| loadedProject enabled disabled enable |
	selectedProjectName := nil.
	(index == 0 or: [ index > self projects size ])
		ifTrue: [ ^ false ].
	self
		browseProjectPackagesMenuAction: listElement selectionIndex: index;
		browseProjectClassesMenuAction: listElement selectionIndex: index.
	loadedProject := self projects at: index.
	selectedProjectName := loadedProject name.
	enabled := {#'rowanProject'.
	#'loadedProject'.
	#'repoBasedProject'.
	#'loggableProject'.
	#'writableRepoBasedProject'.
	#'gitProject'.
	#'project'.
	#'gitBasedProject'}.
	disabled := {#'dirtyProject'.
	#'versionSkew'.
	#'unloadedProject'.
	#'configurationProject'.
	#'baselineProject'.
	#'nonDirtyGitProject'}.
	enable := [ :symbol | 
	enabled add: symbol.
	disabled remove: symbol ifAbsent: [  ] ].
	^ self objectSerializer
		toString:
			{#'setMenuCategories:'.
			enabled.
			#'clearMenuCategories:'.
			disabled}
]

{ #category : 'menu actions' }
TDRowanProjectBrowser >> loadProjectMenuAction: listElement selectionIndex: selectionIndex [

	| projectDef loads |
	selectionIndex = 0
		ifTrue: [ ^ false ].
	projectDef := self projects at: selectionIndex.
	Rowan projectTools load loadProjectNamed: projectDef projectName.
	listElement topez commitTransaction.
	^ #'refreshView'
]

{ #category : 'menu action specs' }
TDRowanProjectBrowser >> menuActionSpec: miniTool [

	theMiniTool := miniTool.
	^ {#('DELETE ALL' nil #'deleteAllProjectsMenuAction:selectionIndex:').
	#('-').
	{('New'
		->
			{#('Github project' nil #'newGitHubProjectSpecMenuAction:selectionIndex:').
			#('disk-based project' nil #'newDiskProjectSpecMenuAction:selectionIndex:')})}.
	#('-').
	{('Browse'
		->
			{#('project spec' nil #'inspectProjectSpecMenuAction:selectionIndex:').
			#('loaded project' nil #'inspectLoadedProjectMenuAction:selectionIndex:').
			#('tests' nil #'testProjectMenuAction:selectionIndex:')})}.
	#('-').
	{('Git'
		->
			{#('branches' nil #'gitBranchesMenuAction:selectionIndex:').
			#('checkout' nil #'gitCheckoutMenuAction:selectionIndex:').
			#('pull' nil #'gitPullProjectMenuAction:selectionIndex:').
			#('push' nil #'gitPushProjectMenuAction:selectionIndex:').
			#('status' nil #'gitStatusMenuAction:selectionIndex:')})}.
	#('-').
	#('changes' nil #'diffMenuAction:selectionIndex:').
	#('load' nil #'loadProjectMenuAction:selectionIndex:').
	#('log' nil #'commitLogMenuAction:selectionIndex:').

	#('save' nil #'commitProjectMenuAction:selectionIndex:')} , {#('-')}
		, self standardMenuActionSpec
]

{ #category : 'menu actions' }
TDRowanProjectBrowser >> newDiskProjectSpecMenuAction: listElement selectionIndex: selectionIndex [

	| spec projectName utils repositoryRootPath dirPath repoPath gitTool |
	projectName := (GsTextInteraction
		prompt: 'Enter the project name'
		template: 'newProject') signal.
	projectName
		ifNil: [ 
			Transcript
				cr;
				show: 'New project aborted...'.
			^ false ].
	utils := Rowan fileUtilities.
	repositoryRootPath := '$GS_HOME/shared/repos/' , projectName.
	repoPath := 'src'.
	spec := RwPackageProjectSpecification new
		specName: projectName;
		repositoryRootPath: '$GS_HOME/shared/repos/' , projectName;
		repoPath: repoPath;
		repositoryUrl: 'cypress:' , repositoryRootPath , '/' , repoPath , '/';
		yourself.
	(spec platformSpec at: 'gemstone')
		projectOwnerId: Rowan image currentUserId;
		defaultSymbolDictName: 'tODE_Rowan_Projects';
		yourself.
	Rowan projectTools create
		_createProjectRepositoryDirectory: repositoryRootPath
		useGit: true
		abort: [ ^ self ].
	dirPath := repositoryRootPath , utils pathNameDelimiter , spec repoPath.
	utils ensureDirectoryExists: dirPath.
	utils
		writeStreamFor: '.cypress'
		in: dirPath
		do: [ :fileStream | fileStream nextPutAll: '{ #format : ' , 'tonel' printString , '}' ].
	dirPath := repositoryRootPath , utils pathNameDelimiter , spec specsPath.
	utils ensureDirectoryExists: dirPath.
	spec register.
	spec copy export.
	gitTool := Rowan projectTools git.
	gitTool gitaddIn: repositoryRootPath with: '.'.
	gitTool gitcommitIn: repositoryRootPath with: '-m"initial commit from Rowan"'.
	^ #'refreshView'
]

{ #category : 'menu actions' }
TDRowanProjectBrowser >> newGitHubProjectSpecMenuAction: listElement selectionIndex: selectionIndex [

	"https://raw.githubusercontent.com/dalehenrich/RowanSample1/master/specs/RowanSample1.ston"

	"file:/home/dhenrich/rogue/_homes/rogue/_home/shared/repos/RowanSampler/specs/RowanSample1.ston"

	| specUrlString spec projectTools |
	specUrlString := (GsTextInteraction
		prompt: 'Please enter an url for the project spec'
		template:
			'file:/home/dhenrich/rogue/_homes/rogue/_home/shared/repos/RowanSampler/specs/RowanSample1.ston')
		signal.
	specUrlString
		ifNil: [ 
			Transcript
				cr;
				show: 'New github project aborted...'.
			^ false ].
	projectTools := Rowan projectTools.
	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: '$GS_HOME/shared/repos/'
		useSsh: true.
	^ #'refreshView'
]

{ #category : 'accessing' }
TDRowanProjectBrowser >> projectBlock [

   ^projectBlock

]

{ #category : 'accessing' }
TDRowanProjectBrowser >> projectBlock: anObject [

   projectBlock := anObject

]

{ #category : 'accessing' }
TDRowanProjectBrowser >> projects [

   ^projects

]

{ #category : 'accessing' }
TDRowanProjectBrowser >> projects: anObject [

   projects := anObject

]

{ #category : 'accessing' }
TDRowanProjectBrowser >> projectTool [

	^ projectTool
]

{ #category : 'accessing' }
TDRowanProjectBrowser >> projectTool: anObject [

	projectTool := anObject
]

{ #category : 'menu actions' }
TDRowanProjectBrowser >> testProjectMenuAction: listElement selectionIndex: selectionIndex [

	| projectDef suite |
	selectionIndex = 0
		ifTrue: [ ^ false ].
	projectDef := self projects at: selectionIndex.
	suite := Rowan projectTools test
		testSuiteForProjectNamed: projectDef projectName.
	(listElement topez toolInstanceFor: 'test') browseTestSuite: suite.
	^ true
]

{ #category : 'accessing' }
TDRowanProjectBrowser >> windowLocation [
  windowLocation ifNil: [ windowLocation := #'projectList' ].
  ^ windowLocation
]

{ #category : 'accessing' }
TDRowanProjectBrowser >> windowName [

	windowName ifNil: [ windowName := #'rowanProjects' ].
	^ windowName
]
