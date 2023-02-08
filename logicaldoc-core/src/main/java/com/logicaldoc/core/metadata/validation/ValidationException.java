package com.logicaldoc.core.metadata.validation;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.PersistenceException;

/**
 * Raised when the content being stored are not logically valid.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.1
 */
public class ValidationException extends PersistenceException {

	private static final long serialVersionUID = 1L;

	/**
	 * A map of error descriptions: key is the attribute's name, value is the
	 * error descriptor
	 */
	private Map<String, ValidationError> errors = new HashMap<>();

	/**
	 * Creates a new session with the map of errors
	 * 
	 * @param errors map of error descriptions
	 */
	public ValidationException(Collection<ValidationError> errors) {
		super();
		if (errors != null)
			for (ValidationError error : errors)
				this.errors.put(error.getAttribute(), error);
	}

	/**
	 * Creates a new session with the map of errors
	 * 
	 * @param errors collection of error descriptions
	 * @param cause origin of the error
	 */
	public ValidationException(Collection<ValidationError> errors, Throwable cause) {
		super(cause);
		if (errors != null)
			for (ValidationError error : errors)
				this.errors.put(error.getAttribute(), error);
	}

	public ValidationException(String message, Throwable cause) {
		super(message, cause);
		errors.put("", new ValidationError(null, message));
	}

	public ValidationException(String message) {
		super(message);
		errors.put("", new ValidationError(null, message));
	}

	/**
	 * Retrieves the map with the errors descriptions
	 * 
	 * @return the map attribute_name - error_description
	 */
	public Map<String, ValidationError> getErrors() {
		return errors;
	}

	public void addError(ValidationError error) {
		errors.put(error.getAttribute(), error);
	}

	@Override
	public String getMessage() {
		if (errors.isEmpty())
			return super.getMessage();
		else {
			return errors.keySet().stream()
					.map(k -> (StringUtils.isNotEmpty(errors.get(k).getLabel()) ? errors.get(k).getLabel() : k)
							+ (StringUtils.isNotEmpty(k) ? ": " : "") + errors.get(k).getDescription())
					.collect(Collectors.joining("; "));
		}
	}
}