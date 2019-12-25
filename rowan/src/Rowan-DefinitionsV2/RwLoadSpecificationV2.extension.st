Extension { #name : 'RwLoadSpecificationV2' }

{ #category : '*rowan-definitionsv2' }
RwLoadSpecificationV2 >> resolve [
	"resolve ensures that the project directory already exists on disk (cloned for git projects) or created on disk for new projects
		answer  the project definition specified by the receiver and any dependent projects"

	"if the project directory already exists on disk, then read the project definition(s) from disk"

	^ RwResolvedProjectV2 loadSpecification: self
]
