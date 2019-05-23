initialization
initializeForImport

	"if spec has been imported, clear out any of the fields that represent local disk state"

	super initializeForImport.
	imageSpec := nil.
	platformSpec valuesDo: [:each | each initializeForImport ].
	repoSpec initializeForImport
