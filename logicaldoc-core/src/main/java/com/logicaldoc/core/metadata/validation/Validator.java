package com.logicaldoc.core.metadata.validation;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.hibernate.LazyInitializationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.RunLevel;
import com.logicaldoc.core.automation.Automation;
import com.logicaldoc.core.automation.AutomationException;
import com.logicaldoc.core.history.History;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.ExtensibleObject;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateAttribute;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.util.Context;

/**
 * A Validator performs the checks on a generic {@link ExtensibleObject}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.1
 */
public class Validator {

	private static final Logger log = LoggerFactory.getLogger(Validator.class);

	/**
	 * Validates an object instance taking the validation logic from a given
	 * template
	 * 
	 * @param object the instance to validate
	 * @param template the template that contains the validation logic
	 * @param transaction the current transaction
	 * 
	 * @throws ValidationException in case of invalid object, this exception
	 *         contains all the errors descriptions
	 * @throws AutomationException the automation has been evaluated but
	 *         produced an error
	 */
	public void validate(ExtensibleObject object, Template template, History transaction)
			throws ValidationException, AutomationException {
		if (!RunLevel.current().aspectEnabled("validation"))
			return;

		// Skip validation if the object is not being changed nor stored
		if (transaction == null)
			return;

		// Validate only if the object has a template
		if (object.getDeleted() != 0 || template == null)
			return;

		setUser(transaction);

		template = initializeAttributes(template);

		Map<String, String> errors = new HashMap<>();

		// Validate each attribute
		validateAttributes(object, template, transaction, errors);

		// Validate the whole object
		executeObjectValidation(object, template, transaction, errors);

		if (!errors.isEmpty()) {
			List<ValidationError> errorsList = new ArrayList<>();
			for (Map.Entry<String, String> entry : errors.entrySet()) {
				Attribute att = template.getAttribute(entry.getKey());
				if (att != null)
					errorsList.add(new ValidationError(entry.getKey(), att.getLabel(), entry.getValue()));
				else
					errorsList.add(new ValidationError(entry.getKey(), entry.getValue()));
			}

			throw new ValidationException(errorsList);
		}
	}

	private Template initializeAttributes(Template template) {
		if (template != null) {
			try {
				int attributesCount = template.getAttributes().size();
				if (log.isDebugEnabled())
					log.debug("Initialized {} attributes", attributesCount);
			} catch (LazyInitializationException e) {
				// If an error happens here it means that the collection could
				// not
				// be loaded, so load the bean again and initialize it.
				log.debug("Got error {} trying to reload the template {}", e.getMessage(), template.getId());
				TemplateDAO tDao = Context.get(TemplateDAO.class);
				try {
					template = tDao.findById(template.getId());
					tDao.initialize(template);
				} catch (PersistenceException pe) {
					log.warn(pe.getMessage(), pe);
				}
			}
		}
		return template;
	}

	private void validateAttributes(ExtensibleObject object, Template template, History transaction,
			Map<String, String> errors) throws AutomationException {
		for (String attributeName : template.getAttributeNames()) {
			Attribute attribute = object.getAttribute(attributeName);
			if (attribute == null)
				continue;

			TemplateAttribute templateAttribute = (TemplateAttribute) template.getAttribute(attributeName);
			if (StringUtils.isNotEmpty(templateAttribute.getValidation()))
				executeAttributeValidation(object, transaction, errors, attributeName, attribute, templateAttribute);
		}
	}

	private void executeObjectValidation(ExtensibleObject object, Template template, History transaction,
			Map<String, String> errors) throws AutomationException {

		Map<String, Object> automationDictionary = new HashMap<>();
		automationDictionary.put("object", object);
		automationDictionary.put("event", transaction);
		automationDictionary.put("errors", errors);

		/*
		 * Skip the general validation script only if the template defines a
		 * validation script and only if there are not errors in fields
		 * validation
		 */
		if (errors.isEmpty() && StringUtils.isNotEmpty(template.getValidation())) {
			Automation script = new Automation("validator",
					transaction != null && transaction.getUser() != null ? transaction.getUser().getLocale()
							: Locale.ENGLISH,
					object.getTenantId());
			script.evaluate(template.getValidation(), automationDictionary);
		}
	}

	private void executeAttributeValidation(ExtensibleObject object, History transaction, Map<String, String> errors,
			String attributeName, Attribute attribute, TemplateAttribute templateAttribute) throws AutomationException {
		Map<String, Object> fieldValidationDictionary = new HashMap<>();
		fieldValidationDictionary.put("object", object);
		fieldValidationDictionary.put("event", transaction);
		fieldValidationDictionary.put("errors", errors);
		fieldValidationDictionary.put("attributeName", attributeName);
		fieldValidationDictionary.put("attribute", attribute);
		fieldValidationDictionary.put("value", attribute.getValue());

		ValidationError error = new ValidationError(attributeName, attribute.getLabel(), null);
		fieldValidationDictionary.put("error", error);
		Automation script = new Automation("validator-" + attributeName,
				transaction != null && transaction.getUser() != null ? transaction.getUser().getLocale()
						: Locale.getDefault(),
				object.getTenantId());
		script.evaluate(templateAttribute.getValidation(), fieldValidationDictionary);

		if (StringUtils.isNotEmpty(error.getDescription()))
			errors.put(attributeName, error.getDescription());
	}

	private void setUser(History transaction) {
		User user = transaction != null && transaction.getUser() != null ? transaction.getUser() : null;
		if (user == null && transaction != null && transaction.getUserId() != null) {
			UserDAO uDao = Context.get(UserDAO.class);
			try {
				user = uDao.findById(transaction.getUserId());
				transaction.setUser(user);
			} catch (PersistenceException e) {
				log.warn(e.getMessage());
			}
		}
	}
}