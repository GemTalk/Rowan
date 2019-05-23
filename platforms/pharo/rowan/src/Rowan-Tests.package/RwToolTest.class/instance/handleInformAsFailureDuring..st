private
handleInformAsFailureDuring: aBlock

	aBlock
		on: GsInteractionRequest
		do: [ :ex | 
			ex
				response:
					(ex interaction interactWith: self gsInteractionInformFailureHandler) ]