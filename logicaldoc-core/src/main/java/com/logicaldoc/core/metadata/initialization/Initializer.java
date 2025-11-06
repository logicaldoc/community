package com.logicaldoc.core.metadata.initialization;

import java.util.HashMap;
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
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;

/**
 * An Initializer performs the checks on a generic {@link ExtensibleObject}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.1
 */
public class Initializer {

	private static final Logger log = LoggerFactory.getLogger(Initializer.class);

	/**
	 * Initializes an object instance taking the initialization logic from a
	 * given template
	 * 
	 * @param object the instance to validate
	 * @param template the template that contains the validation logic
	 * @param transaction the current transaction
	 * 
	 */
	public void initialize(ExtensibleObject object, Template template, History transaction) {
		if (!RunLevel.current().aspectEnabled("initialization"))
			return;

		// Skip initialization if the object has already been saved
		if (object.getId() != 0L)
			return;

		// Initialize only if the object has a template
		if (object.getDeleted() != 0 || template == null)
			return;

		setUser(transaction);

		template = initializeAttributesCollection(template);

		// Initialize the object with general initalization defined at template
		// level(if any)
		try {
			executeInitialization(object, transaction, template);
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}

		// We access the collection as is without initializing the bean
		// because that would lead to hibernate errors
		for (String attributeName : template.getAttributeNames()) {
			try {
				Attribute attribute = object.getAttribute(attributeName);
				Attribute templateAttribute = template.getAttribute(attributeName);
				if (attribute != null && templateAttribute != null && attribute.getValue() == null
						&& StringUtils.isNotEmpty(templateAttribute.getInitialization()))
					executeInitialization(object, transaction, attributeName, attribute, templateAttribute);
			} catch (Exception e) {
				log.error(e.getMessage(), e);
			}
		}
	}

	private Template initializeAttributesCollection(Template template) {
		try {
			int attributesCount = template.getAttributeNames().size();
			if (log.isDebugEnabled())
				log.debug("Initialized {} attributes", attributesCount);
		} catch (LazyInitializationException e) {
			// If an error happens here it means that the collection could not
			// be loaded, so load the bean again and initialize it.
			log.debug("Got error {} trying to reload the template {}", e.getMessage(), template.getId());
			TemplateDAO tDao = TemplateDAO.get();
			try {
				template = tDao.findById(template.getId());
				tDao.initialize(template);
			} catch (PersistenceException pe) {
				log.warn(pe.getMessage(), pe);
			}
		}
		return template;
	}

	private void executeInitialization(ExtensibleObject object, History transaction, Template template)
			throws AutomationException {
		if (StringUtils.isEmpty(template.getInitialization()))
			return;

		Map<String, Object> fieldValidationDictionary = new HashMap<>();
		fieldValidationDictionary.put("object", object);
		fieldValidationDictionary.put("event", transaction);

		Automation script = new Automation("initializer-" + template.getName(),
				transaction != null && transaction.getUser() != null ? transaction.getUser().getLocale()
						: Locale.getDefault(),
				object.getTenantId());
		script.evaluate(template.getInitialization(), fieldValidationDictionary);
	}

	private void executeInitialization(ExtensibleObject object, History transaction, String attributeName,
			Attribute attribute, Attribute templateAttribute) throws AutomationException {
		Map<String, Object> fieldValidationDictionary = new HashMap<>();
		fieldValidationDictionary.put("object", object);
		fieldValidationDictionary.put("event", transaction);
		fieldValidationDictionary.put("attributeName", attributeName);
		fieldValidationDictionary.put("attribute", attribute);

		Automation script = new Automation("initializer-" + attributeName,
				transaction != null && transaction.getUser() != null ? transaction.getUser().getLocale()
						: Locale.getDefault(),
				object.getTenantId());
		script.evaluate(templateAttribute.getInitialization(), fieldValidationDictionary);
	}

	private void setUser(History transaction) {
		User user = transaction != null && transaction.getUser() != null ? transaction.getUser() : null;
		if (user == null && transaction != null && transaction.getUserId() != null) {
			UserDAO uDao = UserDAO.get();
			try {
				user = uDao.findById(transaction.getUserId());
				transaction.setUser(user);
			} catch (PersistenceException e) {
				log.warn(e.getMessage());
			}
		}
	}
}