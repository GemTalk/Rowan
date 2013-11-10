CypressLoaderErrorNotification is used to notify a consumer of the CypressLoader that a particular CypressPatchOperation failed.
As a Notification, it resumes by default, logging the error to the Transcript.


Instance Variables:

patchOperation:		the CypressPatchOperation that could not be applied.
exception:			the Error which occurred while trying to apply the Patch Operation.
