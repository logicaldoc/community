package com.logicaldoc.gui.common.client;

/**
 * Exception used to inform the GUI about errors during the validation
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.1
 */
public class ServerValidationException extends ServerException {

	private static final long serialVersionUID = 1L;

	/**
	 * A map of error descriptions: key is the attribute's name, value is the
	 * error description
	 */
	private ServerValidationError[] errors = new ServerValidationError[0];

	public ServerValidationException() {
		super();
	}

	public ServerValidationException(String message, ServerValidationError[] errors) {
		super(message);
		this.errors = errors;
	}

	public ServerValidationException(ServerValidationError[] errors) {
		this.errors = errors;
	}

	public ServerValidationError[] getErrors() {
		return errors;
	}

	public void setErrors(ServerValidationError[] errors) {
		this.errors = errors;
	}
}