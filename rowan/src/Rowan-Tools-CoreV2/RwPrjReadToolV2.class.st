Class {
	#name : 'RwPrjReadToolV2',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-CoreV2'
}

{ #category : 'read resolved projects' }
RwPrjReadToolV2 >> readLoadedProjectSetForResolvedProject: resolvedProject withComponentNames: componentNames customConditionalAttributes: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [
	^ RwResolvedProjectComponentVisitorV2
		readProjectSetForResolvedProject: resolvedProject
		withComponentNames: componentNames
		customConditionalAttributes: customConditionalAttributes
		platformConditionalAttributes: platformConditionalAttributes
		useLoadedProjects: true
]

{ #category : 'read resolved projects' }
RwPrjReadToolV2 >> readProjectForResolvedProject: resolvedProject withComponentNames: componentNames customConditionalAttributes: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [
	RwResolvedProjectComponentVisitorV2
		readProjectForResolvedProject: resolvedProject
		withComponentNames: componentNames
		customConditionalAttributes: customConditionalAttributes
		platformConditionalAttributes: platformConditionalAttributes.
	resolvedProject readPackageNames: resolvedProject packageNames.
	^ resolvedProject
]

{ #category : 'read loaded projects' }
RwPrjReadToolV2 >> readProjectSetForProjectNamed: projectName [
	| project |
	project := (Rowan image loadedProjectNamed: projectName) asDefinition.
	^ project readLoadedProjectSet
]

{ #category : 'read loaded projects' }
RwPrjReadToolV2 >> readProjectSetForProjectNamed: projectName customConditionalAttributes: customConditionalAttributes [
	| project |
	project := (Rowan image loadedProjectNamed: projectName) asDefinition.
	^ project
		readLoadedProjectSet: customConditionalAttributes
		platformConditionalAttributes: project platformConditionalAttributes
]

{ #category : 'read loaded projects' }
RwPrjReadToolV2 >> readProjectSetForProjectNamed: projectName customConditionalAttributes: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [
	^ (Rowan image loadedProjectNamed: projectName) asDefinition
		readLoadedProjectSet: customConditionalAttributes
		platformConditionalAttributes: platformConditionalAttributes
]

{ #category : 'read resolved projects' }
RwPrjReadToolV2 >> readProjectSetForResolvedProject: resolvedProject withComponentNames: componentNames customConditionalAttributes: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [
	^ RwResolvedProjectComponentVisitorV2
		readProjectSetForResolvedProject: resolvedProject
		withComponentNames: componentNames
		customConditionalAttributes: customConditionalAttributes
		platformConditionalAttributes: platformConditionalAttributes
]

{ #category : 'read resolved projects' }
RwPrjReadToolV2 >> readProjectSetForResolvedProject: resolvedProject withComponentNames: componentNames platformConditionalAttributes: platformConditionalAttributes [
	^ RwResolvedProjectComponentVisitorV2
		readProjectSetForResolvedProject: resolvedProject
		withComponentNames: componentNames
		platformConditionalAttributes: platformConditionalAttributes
]
