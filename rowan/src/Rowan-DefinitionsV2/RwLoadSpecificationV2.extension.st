Extension { #name : 'RwLoadSpecificationV2' }

{ #category : '*rowan-definitionsv2' }
RwLoadSpecificationV2 >> requiredLoadSpecSet [
	"Return an RwLoadSpecSet containing the receiver all load specs for required projects. Note that each of the projects associated with the load spec
		WILL be resolved (i.e. cloned to the local disk)"

	^ RwResolvedProjectV2 requiredLoadSpecSet: self
]

{ #category : '*rowan-definitionsv2' }
RwLoadSpecificationV2 >> resolve [
	"resolve ensures that the project directory already exists on disk (cloned for git projects) or created on disk for new projects
		answer  the project definition specified by the receiver and any dependent projects"

	"if the project directory already exists on disk, then read the project definition(s) from disk"

	^ RwResolvedProjectV2 loadSpecification: self
]

{ #category : '*rowan-definitionsv2' }
RwLoadSpecificationV2 >> resolve: customAttributes platformAttributes: platformAttributes [
	"resolve ensures that the project directory already exists on disk (cloned for git projects) or created on disk for new projects
		answer  the project definition specified by the receiver and any dependent projects"

	"if the project directory already exists on disk, then read the project definition(s) from disk"

	^ RwResolvedProjectV2 loadSpecification: self customConditionalAttributes: customAttributes platformAttributes: platformAttributes
]

{ #category : '*rowan-definitionsv2' }
RwLoadSpecificationV2 >> resolveProjectSet [
	"resolve ensures that the project directory already exists on disk (cloned for git projects) or created on disk for new projects
		answer  the project definition specified by the receiver and any dependent projects"

	"if the project directory already exists on disk, then read the project definition(s) from disk"

	^ RwResolvedProjectV2 loadSpecificationProjectSet: self
]

{ #category : '*rowan-definitionsv2' }
RwLoadSpecificationV2 >> resolveProjectSet: conditionalAttributes [
	"resolve ensures that the project directory already exists on disk (cloned for git projects) or created on disk for new projects
		answer  the project definition specified by the receiver and any dependent projects"

	"if the project directory already exists on disk, then read the project definition(s) from disk"

	^ RwResolvedProjectV2 loadSpecificationProjectSet: self customConditionalAttributes: conditionalAttributes
]

{ #category : '*rowan-definitionsv2' }
RwLoadSpecificationV2 >> resolveProjectSet: customAttributes platformAttributes: platformAttributes [
	"resolve ensures that the project directory already exists on disk (cloned for git projects) or created on disk for new projects
		answer  the project definition specified by the receiver and any dependent projects"

	"if the project directory already exists on disk, then read the project definition(s) from disk"

	^ RwResolvedProjectV2 loadSpecificationProjectSet: self customConditionalAttributes: customAttributes platformAttributes: platformAttributes
]
