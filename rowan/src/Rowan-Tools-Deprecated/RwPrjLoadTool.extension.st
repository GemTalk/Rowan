Extension { #name : 'RwPrjLoadTool' }

{ #category : '*rowan-tools-deprecated' }
RwPrjLoadTool >> loadProjectFromSpecUrl: fileUrlOrString [

	| projectDefinition projectSetDefinition res |
	projectDefinition := Rowan projectTools create createProjectDefinitionFromSpecUrl: fileUrlOrString.
	projectSetDefinition := Rowan projectTools read readProjectSetForProjectDefinition: projectDefinition.
	res := self loadProjectSetDefinition: projectSetDefinition.
	"loaded project and loaded packages read from disk - mark them not dirty"
	self markProjectSetNotDirty: projectSetDefinition.
	^ res

]

{ #category : '*rowan-tools-deprecated' }
RwPrjLoadTool >> loadProjectFromSpecUrl: fileUrlOrString  projectRootPath: projectRootPath [

	| projectDefinition projectSetDefinition res |

	projectDefinition := Rowan projectTools create createProjectDefinitionFromSpecUrl: fileUrlOrString projectRootPath: projectRootPath.
	projectSetDefinition := Rowan projectTools read readProjectSetForProjectDefinition: projectDefinition.
	res := self loadProjectSetDefinition: projectSetDefinition.
	"loaded project and loaded packages read from disk - mark them not dirty"
	self markProjectSetNotDirty: projectSetDefinition.
	^ res

]

{ #category : '*rowan-tools-deprecated' }
RwPrjLoadTool >> loadProjectFromSpecUrl: fileUrlOrString withConfigurations: configNames groupNames: groupNames [

	| projectDefinition projectSetDefinition res |
	projectDefinition := Rowan projectTools create createProjectDefinitionFromSpecUrl: fileUrlOrString.
	projectSetDefinition := Rowan projectTools read 
		readProjectSetForProjectDefinition: projectDefinition 
		withConfigurations: configNames
		groupNames: groupNames.
	res := self loadProjectSetDefinition: projectSetDefinition.
	"loaded project and loaded packages read from disk - mark them not dirty"
	self markProjectSetNotDirty: projectSetDefinition.
	^ res
]

{ #category : '*rowan-tools-deprecated' }
RwPrjLoadTool >> loadProjectFromSpecUrl: fileUrlOrString  withConfigurations: configNames groupNames: groupNames projectRootPath: projectRootPath [

	| projectDefinition projectSetDefinition res |

	projectDefinition := Rowan projectTools create createProjectDefinitionFromSpecUrl: fileUrlOrString projectRootPath: projectRootPath.
	projectSetDefinition := Rowan projectTools read 
		readProjectSetForProjectDefinition: projectDefinition 
		withConfigurations: configNames
		groupNames: groupNames.
	res := self loadProjectSetDefinition: projectSetDefinition.
	"loaded project and loaded packages read from disk - mark them not dirty"
	self markProjectSetNotDirty: projectSetDefinition.
	^ res
]

{ #category : '*rowan-tools-deprecated' }
RwPrjLoadTool >> loadProjectNamed_254: projectNamed instanceMigrator: instanceMigrator [

	"load the named project from disk, mark the loaded projects not dirty and run initializers"

	| projectSetDefinition res |
self deprecated: 'Use loadProjectNamed: projectNamed instanceMigrator: instead'.
	projectSetDefinition := self class read
		readProjectSetForProjectNamed: projectNamed.
	res := self 
		_doProjectSetLoad_254: projectSetDefinition
		instanceMigrator: instanceMigrator 
		originalProjectSet: projectSetDefinition 
		processedClassNames: Set new.
	"loaded project and loaded packages read from disk - mark them not dirty"
	self markProjectSetNotDirty: projectSetDefinition.
	^ res
]
