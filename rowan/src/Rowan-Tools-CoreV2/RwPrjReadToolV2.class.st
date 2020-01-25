Class {
	#name : 'RwPrjReadToolV2',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-CoreV2'
}

{ #category : 'read resolved projects' }
RwPrjReadToolV2 >> readProjectSetForResolvedProject: resolvedProject withComponentNames: componentNames groupNames: groupNames [
	"read packages and project metadata into projectDefinition ... return a project definition
		set that contains projectDefinition and any dependent projects"

	^ self
		readProjectSetForResolvedProject: resolvedProject
		withComponentNames: componentNames
		groupNames: groupNames
		platformConditionalAttributes: resolvedProject platformConditionalAttributes
]

{ #category : 'read resolved projects' }
RwPrjReadToolV2 >> readProjectSetForResolvedProject: resolvedProject withComponentNames: componentNames groupNames: groupNames platformConditionalAttributes: platformConditionalAttributes [
	^ RwResolvedProjectComponentVisitorV2
		readProjectSetForResolvedProject: resolvedProject
		withComponentNames: componentNames
		groupNames: groupNames
		platformConditionalAttributes: platformConditionalAttributes
]
