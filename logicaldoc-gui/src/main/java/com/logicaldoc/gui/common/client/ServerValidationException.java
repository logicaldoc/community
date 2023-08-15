package com.logicaldoc.gui.common.client;

import java.util.Arrays;

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
	private final ServerValidationError[] errors;

	public ServerValidationException() {
		super();
		errors = new ServerValidationError[0];
	}

	public ServerValidationException(String message, ServerValidationError[] errors) {
		super(message);
		if (errors != null)
			this.errors = Arrays.copyOf(errors, errors.length);
		else
			this.errors = null;
	}

	public ServerValidationException(ServerValidationError[] errors) {
		this.errors = errors;
	}

	public ServerValidationError[] getErrors() {
		return errors;
	}
}