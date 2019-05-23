initialization
initializeForExport

	"if spec is to be exported, clear out any of the fields that represent state that should 
	not be shared"

	super initializeForExport.
	conditionalPackageMatchers :=  conditionalPackageMapSpecMatchers := nil
