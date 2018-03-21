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
TDRowanProjectBrowser >> inspectProjectSpecMenuAction: listElement selectionIndex: selectionIndex [

	| loadedProject |
	selectionIndex = 0
		ifTrue: [ ^ false ].
	loadedProject := self projects at: selectionIndex.
	loadedProject specification inspect.
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

{ #category : 'menu action specs' }
TDRowanProjectBrowser >> menuActionSpec: miniTool [

	theMiniTool := miniTool.
	^ {#('DELETE ALL' nil #'deleteAllProjectsMenuAction:selectionIndex:').
	#('-').
	{('Browse'
		->
			{#('project spec' nil #'inspectProjectSpecMenuAction:selectionIndex:' nil #'project' false).
			#('-').
			#('classes' nil #'browseProjectClassesMenuAction:selectionIndex:' nil #'loadedProject' false).
			#('-').
			#('packages' nil #'browseProjectPackagesMenuAction:selectionIndex:' nil #'loadedProject' false).
			#('repositories' nil #'browseProjectRepositoriesMenuAction:selectionIndex:' nil #'metacelloProject' false).
			#('-').
			#('tests' nil #'testProjectMenuAction:selectionIndex:' nil #'loadedProject' false)})}.
	#('-').
	{('Git'
		->
			{#('branches' nil #'gitBranchesMenuAction:selectionIndex:' nil #'gitProject' false).
			#('checkout' nil #'gitCheckoutMenuAction:selectionIndex:' nil #'gitProject' false).
			#('clone' nil #'cloneProjectMenuAction:selectionIndex:' nil #'gitBasedProject' false).
			#('diff' nil #'gitDiffMenuAction:selectionIndex:' nil #'gitProject' false).
			#('diff (cached)' nil #'gitDiffCachedMenuAction:selectionIndex:' nil #'gitProject' false).
			#('fetch' nil #'gitFetchMenuAction:selectionIndex:' nil #'gitProject' false).
			#('pull' nil #'gitPullProjectMenuAction:selectionIndex:' nil #'nonDirtyGitProject' false).
			#('reset' nil #'gitResetProjectMenuAction:selectionIndex:' nil #'gitProject' false).
			#('status' nil #'gitStatusMenuAction:selectionIndex:' nil #'gitProject' false)})}.
	#('-').
	#('changes' nil #'diffMenuAction:selectionIndex:' nil #'repoBasedProject' false).
	#('load' nil #'loadProjectMenuAction:selectionIndex:' nil #'metacelloProject' false).
	#('log' nil #'commitLogMenuAction:selectionIndex:' nil #'loggableProject' false).
	#('merge' nil #'gitMergeProjectMenuAction:selectionIndex:' nil #'nonDirtyGitProject' false).
	#('push' nil #'gitPushProjectMenuAction:selectionIndex:' nil #'nonDirtyGitProject' false).
	#('refresh (unloaded)' nil #'refreshProjectEntriesMenuAction:selectionIndex:').
	#('revert' nil #'revertProjectMenuAction:selectionIndex:' nil #'dirtyProject' false).
	#('save' nil #'commitProjectMenuAction:selectionIndex:' nil #'writableRepoBasedProject' false).
	#('skew diff' nil #'gitVersionSkewDiffMenuAction:selectionIndex:' nil #'versionSkew' false).
	#('skew save' nil #'gitVersionSkewSaveProjectMenuAction:selectionIndex:' nil #'versionSkew' false)}
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
