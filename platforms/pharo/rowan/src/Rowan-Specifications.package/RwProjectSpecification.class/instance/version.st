accessing
version
	"answer the version of the class --- used to mark the version of the class for objects written to disk"

	"see currentVersion method for list of versions"

	"currently RwProjectSpecification is the only instance being versioned as it represents the version of the whole structure, 
		but I don't want to rule out the possiblity that other specs may need to be independently versioned."

	^ version
