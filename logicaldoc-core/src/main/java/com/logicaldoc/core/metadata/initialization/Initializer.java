package com.logicaldoc.core.metadata.initialization;

import java.util.HashMap;
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
 * An Initializer performs the checks on a generic {@link ExtensibleObject}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.1
 */
public class Initializer {

	protected static Logger log = LoggerFactory.getLogger(Initializer.class);

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

		try {
			TemplateDAO tDao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
			tDao.initialize(template);
			for (String attributeName : template.getAttributeNames()) {
				try {
					Attribute attribute = object.getAttribute(attributeName);
					Attribute templateAttribute = template.getAttribute(attributeName);
					if (attribute.getValue() == null && StringUtils.isNotEmpty(templateAttribute.getInitialization())) {
						System.out.println("found initialization for " + attributeName + " : "
								+ templateAttribute.getInitialization());

						Map<String, Object> fieldValidationDictionary = new HashMap<String, Object>();
						fieldValidationDictionary.put("object", object);
						fieldValidationDictionary.put("event", transaction);
						fieldValidationDictionary.put("attributeName", attributeName);
						fieldValidationDictionary.put("attribute", attribute);

						Automation script = new Automation("initializer-" + attributeName,
								user != null ? user.getLocale() : Locale.getDefault(), object.getTenantId());
						script.evaluate(templateAttribute.getInitialization(), fieldValidationDictionary);
					}
				} catch (Throwable e) {
					log.error(e.getMessage(), e);
				}
			}
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
		}
	}
}