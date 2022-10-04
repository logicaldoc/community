package com.logicaldoc.core.metadata.validation;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.History;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.RunLevel;
import com.logicaldoc.core.automation.Automation;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.ExtensibleObject;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.util.Context;

/**
 * A Validator performs the checks on a generic {@link ExtensibleObject}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.1
 */
public class Validator {

	protected static Logger log = LoggerFactory.getLogger(Validator.class);

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
	 */
	public void validate(ExtensibleObject object, Template template, History transaction) throws ValidationException {
		if (!RunLevel.current().aspectEnabled("validation"))
			return;

		// Skip validation if the object is not being changed nor stored
		if (transaction == null)
			return;

		// Validate only if the object has a template
		if (object.getDeleted() != 0 || template == null)
			return;

		User user = transaction != null && transaction.getUser() != null ? transaction.getUser() : null;
		if (user == null && transaction != null && transaction.getUserId() != null) {
			UserDAO uDao = (UserDAO) Context.get().getBean(UserDAO.class);
			try {
				user = uDao.findById(transaction.getUserId());
				transaction.setUser(user);
			} catch (PersistenceException e) {
				log.warn(e.getMessage());
			}
		}

		Map<String, String> errors = new HashMap<String, String>();

		Map<String, Object> automationDictionary = new HashMap<String, Object>();
		automationDictionary.put("object", object);
		automationDictionary.put("event", transaction);
		automationDictionary.put("errors", errors);

		TemplateDAO tDao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
		tDao.initialize(template);
		for (String attributeName : template.getAttributeNames()) {
			Attribute attribute = object.getAttribute(attributeName);
			if(attribute==null)
				continue;
			
			Attribute templateAttribute = template.getAttribute(attributeName);
			if (StringUtils.isNotEmpty(templateAttribute.getValidation())) {
				Map<String, Object> fieldValidationDictionary = new HashMap<String, Object>();
				fieldValidationDictionary.put("object", object);
				fieldValidationDictionary.put("event", transaction);
				fieldValidationDictionary.put("errors", errors);
				fieldValidationDictionary.put("attributeName", attributeName);
				fieldValidationDictionary.put("attribute", attribute);
				fieldValidationDictionary.put("value", attribute.getValue());

				ValidationError error = new ValidationError(attributeName, attribute.getLabel(), null);
				fieldValidationDictionary.put("error", error);
				Automation script = new Automation("validator-" + attributeName,
						user != null ? user.getLocale() : Locale.getDefault(), object.getTenantId());
				script.evaluate(templateAttribute.getValidation(), fieldValidationDictionary);

				if (StringUtils.isNotEmpty(error.getDescription()))
					errors.put(attributeName, error.getDescription());
			}
		}

		/*
		 * Skip the general validation script only if the template defines a
		 * validation script and only if there are not errors in fields
		 * validation
		 */
		if (errors.isEmpty() && StringUtils.isNotEmpty(template.getValidation())) {
			Automation script = new Automation("validator", user.getLocale(), object.getTenantId());
			script.evaluate(template.getValidation(), automationDictionary);
		}

		if (!errors.isEmpty()) {
			List<ValidationError> errorsList = new ArrayList<ValidationError>();
			for (String key : errors.keySet()) {
				Attribute att = template.getAttribute(key);
				if (att != null)
					errorsList.add(new ValidationError(key, att.getLabel(), errors.get(key)));
				else
					errorsList.add(new ValidationError(key, errors.get(key)));
			}

			throw new ValidationException(errorsList);
		}

	}
}