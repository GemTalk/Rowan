Class {
	#name : 'RwPrjReadToolV2',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-CoreV2'
}

{ #category : 'read resolved projects' }
RwPrjReadToolV2 >> readProjectForResolvedProject: resolvedProject withComponentNames: componentNames [
	"read packages and project metadata into projectDefinition ... return the resolvedProject"

	^ self
		readProjectForResolvedProject: resolvedProject
		withComponentNames: componentNames
		platformConditionalAttributes: resolvedProject platformConditionalAttributes
]

{ #category : 'read resolved projects' }
RwPrjReadToolV2 >> readProjectForResolvedProject: resolvedProject withComponentNames: componentNames platformConditionalAttributes: platformConditionalAttributes [
	RwResolvedProjectComponentVisitorV2
		readProjectForResolvedProject: resolvedProject
		withComponentNames: componentNames
		platformConditionalAttributes: platformConditionalAttributes.
	resolvedProject readPackageNames: resolvedProject packageNames.
	^ resolvedProject
]

{ #category : 'read loaded projects' }
RwPrjReadToolV2 >> readProjectSetForProjectNamed: projectName [
	^ (Rowan image loadedProjectNamed: projectName) asDefinition readProjectSet
]

{ #category : 'read loaded projects' }
RwPrjReadToolV2 >> readProjectSetForProjectNamed: projectName platformConditionalAttributes: platformConditionalAttributes [
	^ (Rowan image loadedProjectNamed: projectName) asDefinition
		readProjectSet: platformConditionalAttributes
]

{ #category : 'read resolved projects' }
RwPrjReadToolV2 >> readProjectSetForResolvedProject: resolvedProject withComponentNames: componentNames [ 
	"read packages and project metadata into projectDefinition ... return a project definition
		set that contains projectDefinition and any dependent projects"

	^ self
		readProjectSetForResolvedProject: resolvedProject
		withComponentNames: componentNames
		platformConditionalAttributes: resolvedProject platformConditionalAttributes
]

{ #category : 'read resolved projects' }
RwPrjReadToolV2 >> readProjectSetForResolvedProject: resolvedProject withComponentNames: componentNames platformConditionalAttributes: platformConditionalAttributes [
	^ RwResolvedProjectComponentVisitorV2
		readProjectSetForResolvedProject: resolvedProject
		withComponentNames: componentNames
		platformConditionalAttributes: platformConditionalAttributes
]
