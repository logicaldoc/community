package com.logicaldoc.core.metadata.initialization;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.RowMapper;

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

		setUser(transaction);

		try {
			loadTemplateAttributes(template);
			for (String attributeName : template.getAttributeNames()) {
				try {
					Attribute attribute = object.getAttribute(attributeName);
					Attribute templateAttribute = template.getAttribute(attributeName);
					if (attribute != null && templateAttribute != null)
						if (attribute.getValue() == null
								&& StringUtils.isNotEmpty(templateAttribute.getInitialization())) {

							executeInitialization(object, transaction, attributeName, attribute, templateAttribute);
						}
				} catch (Throwable e) {
					log.error(e.getMessage(), e);
				}
			}
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
		}
	}

	private void loadTemplateAttributes(Template template) throws PersistenceException {
		TemplateDAO tDao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
		tDao.query("SELECT ld_name,ld_label,ld_mandatory,ld_type,ld_position,ld_stringvalue,ld_intvalue,ld_doublevalue,"
				+ "ld_datevalue,ld_editor,ld_setid,ld_hidden,ld_multiple,ld_parent,ld_stringvalues,ld_validation,"
				+ "ld_initialization,ld_dependson FROM ld_template_ext where ld_templateid = " + template.getId(), null,
				new RowMapper<Attribute>() {

					@Override
					public Attribute mapRow(ResultSet rs, int row) throws SQLException {
						Attribute attribute = new Attribute();
						attribute.setName(rs.getString(1));
						attribute.setLabel(rs.getString(2));
						attribute.setMandatory(rs.getInt(3));
						attribute.setType(rs.getInt(4));
						attribute.setPosition(rs.getInt(5));
						attribute.setStringValue(rs.getString(6));
						attribute.setIntValue(rs.getLong(7));
						attribute.setDoubleValue(rs.getDouble(8));
						attribute.setDateValue(rs.getDate(9));
						attribute.setEditor(rs.getInt(10));
						attribute.setSetId(rs.getLong(11));
						attribute.setHidden(rs.getInt(12));
						attribute.setMultiple(rs.getInt(13));
						attribute.setParent(rs.getString(14));
						attribute.setStringValues(rs.getString(15));
						attribute.setValidation(rs.getString(16));
						attribute.setInitialization(rs.getString(17));
						attribute.setDependsOn(rs.getString(18));

						template.setAttribute(attribute.getName(), attribute);
						return attribute;
					}
				}, null);
	}

	private void executeInitialization(ExtensibleObject object, History transaction, String attributeName,
			Attribute attribute, Attribute templateAttribute) {
		Map<String, Object> fieldValidationDictionary = new HashMap<String, Object>();
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
			UserDAO uDao = (UserDAO) Context.get().getBean(UserDAO.class);
			try {
				user = uDao.findById(transaction.getUserId());
				transaction.setUser(user);
			} catch (PersistenceException e) {
				log.warn(e.getMessage());
			}
		}
	}
}