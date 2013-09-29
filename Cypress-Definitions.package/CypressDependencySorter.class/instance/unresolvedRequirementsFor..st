"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
unresolvedRequirementsFor: aPatchOperation
	"Answer a list of global names that are required by <aPatchOperation>, but not 
	 provided by patchOperations that have already been processed"

	^ aPatchOperation requirements difference: self provided
