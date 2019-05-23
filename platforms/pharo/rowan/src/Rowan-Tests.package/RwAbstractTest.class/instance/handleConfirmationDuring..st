private
handleConfirmationDuring: aBlock

	"expect a confirmation"

	aBlock
		on: GsInteractionRequest
		do: [ :ex | 
			ex
				response:
					(ex interaction interactWith: self gsInteractionConfirmationHandler) ]