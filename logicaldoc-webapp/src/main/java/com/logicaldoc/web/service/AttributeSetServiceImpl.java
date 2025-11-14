package com.logicaldoc.web.service;

import java.io.File;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.AttributeOption;
import com.logicaldoc.core.metadata.AttributeOptionDAO;
import com.logicaldoc.core.metadata.AttributeSet;
import com.logicaldoc.core.metadata.AttributeSetDAO;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIAttributeSet;
import com.logicaldoc.gui.common.client.beans.GUIValue;
import com.logicaldoc.gui.frontend.client.services.AttributeSetService;
import com.logicaldoc.util.csv.CSVFileReader;
import com.logicaldoc.web.UploadServlet;

/**
 * Implementation of the AttributeSetService
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class AttributeSetServiceImpl extends AbstractRemoteService implements AttributeSetService {

	private static final long serialVersionUID = 1L;

	private static final Logger log = LoggerFactory.getLogger(AttributeSetServiceImpl.class);

	@Override
	public void delete(long setId) throws ServerException {
		Session session = validateSession();

		try {
			AttributeSetDAO dao = AttributeSetDAO.get();
			dao.delete(setId);
		} catch (Exception t) {
			throwServerException(session, log, t);
		}
	}

	@Override
	public void saveOptions(long setId, String attribute, List<GUIValue> values) throws ServerException {
		Session session = validateSession();

		try {
			AttributeOptionDAO dao = AttributeOptionDAO.get();
			dao.deleteBySetIdAndAttribute(setId, attribute);
			int i = 0;
			for (GUIValue value : values) {
				AttributeOption option = new AttributeOption(setId, attribute, value.getValue(), value.getCode());
				option.setPosition(i++);
				store(setId, option);
			}
		} catch (Exception t) {
			throwServerException(session, log, t);
		}
	}

	private void store(long setId, AttributeOption option) throws ServerException {
		try {
			AttributeOptionDAO dao = AttributeOptionDAO.get();
			dao.store(option);
		} catch (Exception e) {
			throw new ServerException(String.format("Options have not been %s", setId != 0L ? "updated" : "stored"), e);
		}
	}

	@Override
	public void deleteOptions(long setId, String attribute, List<String> values) throws ServerException {
		Session session = validateSession();
		try {
			AttributeOptionDAO dao = AttributeOptionDAO.get();
			List<AttributeOption> options = dao.findByAttribute(setId, attribute);
			for (AttributeOption option : options)
				for (String value : values)
					if (value.equals(option.getValue())) {
						delete(option);
						break;
					}
		} catch (Exception t) {
			throwServerException(session, log, t);
		}
	}

	private void delete(AttributeOption option) throws ServerException {
		try {
			AttributeOptionDAO dao = AttributeOptionDAO.get();
			dao.delete(option.getId());
		} catch (Exception e) {
			throw new ServerException("Option has not been deleted", e);
		}
	}

	@Override
	public GUIAttributeSet save(GUIAttributeSet guiAttributeSet) throws ServerException {
		Session session = validateSession();

		try {
			AttributeSet attributeSet;
			if (guiAttributeSet.getId() != 0) {
				AttributeSetDAO dao = AttributeSetDAO.get();
				attributeSet = dao.findById(guiAttributeSet.getId());
				dao.initialize(attributeSet);
			} else {
				attributeSet = new AttributeSet();
			}

			attributeSet.setTenantId(session.getTenantId());
			attributeSet.setName(guiAttributeSet.getName());
			attributeSet.setLabel(guiAttributeSet.getLabel());
			attributeSet.setDescription(guiAttributeSet.getDescription());
			attributeSet.setReadonly(guiAttributeSet.isReadonly());
			attributeSet.setType(guiAttributeSet.getType());

			saveAttributes(guiAttributeSet, attributeSet);

			store(guiAttributeSet, attributeSet);

			guiAttributeSet.setId(attributeSet.getId());
		} catch (Exception t) {
			return throwServerException(session, log, t);
		}

		return guiAttributeSet;
	}

	private void store(GUIAttributeSet guiAttributeSet, AttributeSet attributeSet) throws ServerException {
		try {
			AttributeSetDAO dao = AttributeSetDAO.get();
			dao.store(attributeSet);
		} catch (Exception e) {
			throw new ServerException(
					String.format("AttributeSet has not been %s", guiAttributeSet.getId() != 0L ? "updated" : "stored"),
					e);
		}
	}

	private void saveAttributes(GUIAttributeSet guiAttributeSet, AttributeSet attributeSet) {
		Map<String, Attribute> attrs = new HashMap<>();
		attributeSet.getAttributes().clear();
		if (guiAttributeSet != null)
			for (GUIAttribute attribute : guiAttributeSet.getAttributes()) {
				if (attribute != null) {
					saveAttribute(attribute, guiAttributeSet, attrs);
				}
			}
		if (attrs.size() > 0)
			attributeSet.setAttributes(attrs);
	}

	private void saveAttribute(GUIAttribute attribute, GUIAttributeSet attributeSet, Map<String, Attribute> attrs) {
		Attribute att = new Attribute();
		att.setSetId(attributeSet.getId());
		att.setPosition(attribute.getPosition());
		att.setMandatory(attribute.isMandatory());
		att.setHidden(attribute.isHidden());
		att.setReadonly(attribute.isReadonly());
		att.setMultiple(attribute.isMultiple());
		att.setParent(attribute.getParent());
		att.setDependsOn(attribute.getDependsOn());
		att.setStringValues(attribute.getStringValues());
		att.setType(attribute.getType());
		att.setLabel(attribute.getLabel());
		att.setEditor(attribute.getEditor());
		att.setValidation(attribute.getValidation());
		att.setInitialization(attribute.getInitialization());
		att.setStringValue(attribute.getStringValue());
		if (StringUtils.isEmpty(attribute.getLabel()))
			att.setLabel(attribute.getName());
		if (attribute.getValue() instanceof String)
			att.setStringValue(attribute.getStringValue());
		else if (attribute.getValue() instanceof Long)
			att.setIntValue(attribute.getIntValue());
		else if (attribute.getValue() instanceof Double)
			att.setDoubleValue(attribute.getDoubleValue());
		else if (attribute.getValue() instanceof Date)
			att.setDateValue(attribute.getDateValue());
		else if (attribute.getValue() instanceof Boolean)
			att.setBooleanValue(attribute.getBooleanValue());
		attrs.put(attribute.getName(), att);
	}

	public GUIAttributeSet getAttributeSet(String name) throws ServerException {
		Session session = validateSession();
		AttributeSetDAO dao = AttributeSetDAO.get();
		try {
			AttributeSet set = dao.findByName(name, session.getTenantId());
			if (set != null)
				return getAttributeSet(set.getId());
			else
				return null;
		} catch (Exception t) {
			return throwServerException(session, log, t);
		}
	}

	@Override
	public GUIAttributeSet getAttributeSet(long setId) throws ServerException {
		validateSession();

		AttributeSetDAO dao = AttributeSetDAO.get();
		try {
			AttributeSet attributeSet = dao.findById(setId);
			if (attributeSet == null)
				return null;

			GUIAttributeSet attSet = new GUIAttributeSet();
			attSet.setId(setId);
			attSet.setName(attributeSet.getName());
			attSet.setLabel(attributeSet.getLabel());
			attSet.setDescription(attributeSet.getDescription());
			attSet.setReadonly(attributeSet.isReadonly());
			attSet.setType(attributeSet.getType());

			List<GUIAttribute> attributes = readAttributes(attributeSet);
			attributes.sort(null);
			attSet.setAttributes(attributes);

			return attSet;
		} catch (Exception t) {
			log.error(t.getMessage(), t);
		}

		return null;
	}

	private List<GUIAttribute> readAttributes(AttributeSet attributeSet) {
		List<GUIAttribute> attributes = new ArrayList<>();
		for (String attrName : attributeSet.getAttributeNames())
			attributes.add(readAttribute(attrName, attributeSet));
		return attributes;
	}

	private GUIAttribute readAttribute(String attrName, AttributeSet attributeSet) {
		Attribute extAttr = attributeSet.getTemplateAttributes().get(attrName);
		GUIAttribute att = new GUIAttribute();
		att.setName(attrName);
		att.setParent(extAttr.getParent());
		att.setStringValues(extAttr.getStringValues());
		att.setSet(attributeSet.getName());
		att.setSetId(attributeSet.getId());
		att.setPosition(extAttr.getPosition());
		att.setMandatory(extAttr.isMandatory());
		att.setHidden(extAttr.isHidden());
		att.setReadonly(extAttr.isReadonly());
		att.setMultiple(extAttr.isMultiple());
		att.setType(extAttr.getType());
		att.setValidation(extAttr.getValidation());
		att.setInitialization(extAttr.getInitialization());
		if (StringUtils.isEmpty(extAttr.getLabel()))
			att.setLabel(attrName);
		else
			att.setLabel(extAttr.getLabel());
		if (extAttr.getValue() instanceof String)
			att.setStringValue(extAttr.getStringValue());
		else if (extAttr.getValue() instanceof Long)
			att.setIntValue(extAttr.getIntValue());
		else if (extAttr.getValue() instanceof Double)
			att.setDoubleValue(extAttr.getDoubleValue());
		else if (extAttr.getValue() instanceof Date)
			att.setDateValue(convertToDate(extAttr.getDateValue()));
		else if (extAttr.getValue() instanceof Boolean)
			att.setBooleanValue(extAttr.getBooleanValue());

		att.setEditor(extAttr.getEditor());

		if (extAttr.getType() == Attribute.TYPE_USER || extAttr.getEditor() == Attribute.EDITOR_LISTBOX) {
			readAttributeOptions(extAttr, att);
		}
		return att;
	}

	private void readAttributeOptions(Attribute extAttr, GUIAttribute att) {
		String buf = extAttr.getStringValue();
		List<String> list = new ArrayList<>();
		if (buf != null) {
			if (buf.contains(",")) {
				StringTokenizer st = new StringTokenizer(buf, ",");
				while (st.hasMoreElements()) {
					String val = (String) st.nextElement();
					if (!list.contains(val))
						list.add(val);
				}
			} else
				list.add(buf.trim());
			att.setStringValue(buf);
		}
		att.setOptions(list);
	}

	@Override
	public List<GUIAttributeSet> getAttributeSets() throws ServerException {
		Session session = validateSession();
		try {
			AttributeSetDAO dao = AttributeSetDAO.get();
			List<GUIAttributeSet> guiSets = new ArrayList<>();
			List<Long> setIds = dao.findAllIds(session.getTenantId());
			for (Long setId : setIds)
				guiSets.add(getAttributeSet(setId));
			return guiSets;
		} catch (Exception t) {
			return throwServerException(session, log, t);
		}
	}

	@Override
	public List<GUIValue> parseOptions(long setId, String attribute) throws ServerException {
		Session session = validateSession();

		Map<String, File> uploadedFilesMap = UploadServlet.getUploads(session.getSid());
		File file = uploadedFilesMap.values().iterator().next();
		List<GUIValue> options = new ArrayList<>();

		try (CSVFileReader reader = new CSVFileReader(file.getPath());) {
			List<String> row = reader.readFields();
			if (row != null && "value".equalsIgnoreCase(row.get(0)))
				row = reader.readFields();
			while (row != null && !row.isEmpty()) {
				GUIValue option = new GUIValue();
				option.setValue(row.get(0));
				if (row.size() > 1)
					option.setCode(row.get(1));
				options.add(option);
				row = reader.readFields();
			}
		} catch (Exception e) {
			log.error("Unable to parse options in CSV file", e);
		} finally {
			UploadServlet.cleanUploads(session.getSid());
		}

		saveOptions(setId, attribute, options);
		return options;
	}

	@Override
	public void applyValidationToTemplates(long setId, String attribute) throws ServerException {
		Session session = validateSession();
		try {
			AttributeSetDAO dao = AttributeSetDAO.get();
			AttributeSet set = dao.findById(setId);
			dao.initialize(set);

			Attribute setAttribute = set.getTemplateAttributes().get(attribute);
			if (setAttribute == null)
				return;

			Map<String, Object> params = new HashMap<>();
			params.put("validation", setAttribute.getValidation());
			params.put("name", attribute);
			params.put("setId", setId);
			int count = dao.jdbcUpdate(
					"update ld_template_ext set ld_validation = :validation where ld_setid = :setId and ld_name = :name",
					params);
			log.info("Updated the validation of {} template attributes named {}", count, attribute);
		} catch (Exception t) {
			throwServerException(session, log, t);
		}
	}

	@Override
	public void applyInitializationToTemplates(long setId, String attribute) throws ServerException {
		Session session = validateSession();
		try {
			AttributeSetDAO dao = AttributeSetDAO.get();
			AttributeSet set = dao.findById(setId);
			dao.initialize(set);
			Attribute setAttribute = set.getTemplateAttributes().get(attribute);
			if (setAttribute == null)
				return;

			Map<String, Object> params = new HashMap<>();
			params.put("initialization", setAttribute.getInitialization());
			params.put("name", attribute);
			params.put("setId", setId);
			int count = dao.jdbcUpdate(
					"update ld_template_ext set ld_initialization = :initialization where ld_setid = :setId and ld_name = :name",
					params);
			log.info("Updated the initialization of {} template attributes named {}", count, attribute);
		} catch (Exception t) {
			throwServerException(session, log, t);
		}
	}

	@Override
	public void applyAllToTemplates(long setId, String attributeName) throws ServerException {
		Session session = validateSession();
		try {
			AttributeSetDAO dao = AttributeSetDAO.get();
			AttributeSet set = dao.findById(setId);
			Attribute setAttribute = set.getTemplateAttributes().get(attributeName);

			TemplateDAO templateDao = TemplateDAO.get();

			/*
			 * Update the attributes referenced in the templates
			 */
			List<Template> templates = templateDao.findAll(set.getTenantId());
			for (Template template : templates) {
				templateDao.initialize(template);
				template.getTemplateAttributes().put(attributeName, new Attribute(setAttribute));
				templateDao.store(template);
			}
		} catch (Exception t) {
			throwServerException(session, log, t);
		}
	}
}