project writing
processProject: aProjectModification

	| projectProperties |

	packageDefFileNameMap := self _createFileNameMapForClassesOrPackages:  aProjectModification after packages.
	packageDefBeforeFileNameMap := self _createFileNameMapForClassesOrPackages:  aProjectModification before packages.

	"confirm that the project source is written in Tonel format"
	(self _repositoryFormatFor:  aProjectModification after packagesRoot) = 'filetree' ifFalse: [ self error: 'expected tonel format repository' ].

	projectProperties := self _filetreeProjectPropertyDictFor: aProjectModification.
	packageExtension := projectProperties at: 'packageExtension' ifAbsent: [ self packageExtension ].
	(packageExtension beginsWith: '.')
		ifTrue: [
			"properties file includes leading $., so strip it"
			packageExtension := packageExtension copyFrom: 2 to: packageExtension size ].
	(projectProperties at: 'Metadata' ifAbsent: [ true ])
		ifTrue: [
			"Rowan can read filetree repositories with Monticello metadata, but cannot write Monticello metadata"
			self error: 'Monticello metadata not supported.' ].

	super processProject: aProjectModification.