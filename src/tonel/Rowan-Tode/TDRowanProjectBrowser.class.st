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
		browseClassesBlock: block
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
		windowLabel: projectDef projectName , ' Packages';
		yourself) open
]

{ #category : 'tools' }
TDRowanProjectBrowser >> clientList: miniTool listElement: listElement [

	| labels selectedProjectIndex projectList count |
	theMiniTool := miniTool.
	projects := projectBlock value.
	labels := {}.
	selectedProjectIndex := nil.
	count := 1.
	projects
		do: [ :projectDef | 
			| projectName |
			projectName := projectDef projectName.
			selectedProjectName = projectName
				ifTrue: [ selectedProjectIndex := count ].
			count := count + 1.
			labels
				add:
					{projectName.
					{(TextEmphasis bold)}} ].
	projectList := labels
		collect: [ :ar | 
			| nm textAttributes |
			nm := ar at: 1.
			textAttributes := ar at: 2.

			Text string: nm attributes: textAttributes ].
	^ projectList -> selectedProjectIndex
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
			symDictNames := IdentitySet with: #'UnmanagedPackages'.
			symDictNames add: platformSpec defaultSymbolDictName.
			platformSpec packageNameToPlatformPropertiesMap values
				do: [ :packageProperties | 
					packageProperties
						at: 'symbolDictName'
						ifPresent: [ :name | symDictNames add: name ] ].
			symDictNames remove: #'UserGlobals' ifAbsent: [  ].
			symDictNames
				do: [ :symDictName | 
					dict := symList objectNamed: symDictName.
					dict
						ifNotNil: [ 
							| index |
							(dict at: GsPackagePolicy globalName otherwise: nil)
								ifNotNil: [ :policy | policy disable ].
							index := symList indexOf: dict.
							index > 0
								ifTrue: [ user removeDictionaryAt: index ] ] ].
			RwGsImage _removeLoadedProject: loadedProject ].
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
			#('merge' nil #'gitMergeProjectMenuAction:selectionIndex:').
			#('pull' nil #'gitPullProjectMenuAction:selectionIndex:').
			#('push' nil #'gitPushProjectMenuAction:selectionIndex:').
			#('status' nil #'gitStatusMenuAction:selectionIndex:')})}.
	#('-').
	#('changes' nil #'diffMenuAction:selectionIndex:').
	#('load' nil #'loadProjectMenuAction:selectionIndex:').
	#('log' nil #'commitLogMenuAction:selectionIndex:').

	#('save' nil #'commitProjectMenuAction:selectionIndex:')}
		, self standardMenuActionSpec
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
