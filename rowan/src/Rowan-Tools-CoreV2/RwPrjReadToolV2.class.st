Class {
	#name : 'RwPrjReadToolV2',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-CoreV2'
}

{ #category : 'reading' }
RwPrjReadToolV2 >> readProjectForProject: resolvedProject withComponentNames: componentNames customConditionalAttributes: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [
	RwResolvedProjectComponentVisitorV2
		readProjectForProject: resolvedProject
		withComponentNames: componentNames
		customConditionalAttributes: customConditionalAttributes
		platformConditionalAttributes: platformConditionalAttributes.
	resolvedProject readPackageNames: resolvedProject packageNames.
	^ resolvedProject
]

{ #category : 'read project set' }
RwPrjReadToolV2 >> readProjectSetForProject: resolvedProject [
	^ self
		readProjectSetForProject: resolvedProject
		platformConditionalAttributes: resolvedProject platformConditionalAttributes
]

{ #category : 'read project set' }
RwPrjReadToolV2 >> readProjectSetForProject: resolvedProject platformConditionalAttributes: platformConditionalAttributes [
	^ RwResolvedProjectComponentVisitorV2
		readProjectSetForProject: resolvedProject
		platformConditionalAttributes: platformConditionalAttributes
]

{ #category : 'read project set' }
RwPrjReadToolV2 >> readProjectSetForProject: resolvedProject withComponentNames: componentNames customConditionalAttributes: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [
	^ RwResolvedProjectComponentVisitorV2
		readProjectSetForProject: resolvedProject
		withComponentNames: componentNames
		customConditionalAttributes: customConditionalAttributes
		platformConditionalAttributes: platformConditionalAttributes.
]

{ #category : 'read loaded projects' }
RwPrjReadToolV2 >> readProjectSetForProjectNamed: projectName [
	| project |
	project := (Rowan image loadedProjectNamed: projectName) asDefinition.
	^ project readLoadedProjectSet
]
