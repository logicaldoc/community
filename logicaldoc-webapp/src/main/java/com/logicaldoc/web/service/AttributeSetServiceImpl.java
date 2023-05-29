package com.logicaldoc.web.service;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
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
import com.logicaldoc.core.security.Session;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIAttributeSet;
import com.logicaldoc.gui.common.client.beans.GUIValue;
import com.logicaldoc.gui.frontend.client.services.AttributeSetService;
import com.logicaldoc.util.Context;
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

	private static Logger log = LoggerFactory.getLogger(AttributeSetServiceImpl.class);

	@Override
	public void delete(long setId) throws ServerException {
		Session session = validateSession(getThreadLocalRequest());

		try {
			AttributeSetDAO dao = (AttributeSetDAO) Context.get().getBean(AttributeSetDAO.class);
			dao.delete(setId);
		} catch (Exception t) {
			throwServerException(session, log, t);
		}
	}

	@Override
	public void saveOptions(long setId, String attribute, GUIValue[] values) throws ServerException {
		Session session = validateSession(getThreadLocalRequest());

		try {
			AttributeOptionDAO dao = (AttributeOptionDAO) Context.get().getBean(AttributeOptionDAO.class);
			dao.deleteBySetIdAndAttribute(setId, attribute);
			for (int i = 0; i < values.length; i++) {
				GUIValue value = values[i];
				AttributeOption option = new AttributeOption(setId, attribute, value.getValue(), value.getCode());
				option.setPosition(i);

				store(setId, option);
			}
		} catch (Exception t) {
			throwServerException(session, log, t);
		}
	}

	private void store(long setId, AttributeOption option) throws ServerException {
		try {
			AttributeOptionDAO dao = (AttributeOptionDAO) Context.get().getBean(AttributeOptionDAO.class);
			dao.store(option);
		} catch (Exception e) {
			throw new ServerException(String.format("Options have not been %s", setId != 0L ? "updated" : "stored"), e);
		}
	}

	@Override
	public void deleteOptions(long setId, String attribute, String[] values) throws ServerException {
		Session session = validateSession(getThreadLocalRequest());
		try {
			AttributeOptionDAO dao = (AttributeOptionDAO) Context.get().getBean(AttributeOptionDAO.class);
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
			AttributeOptionDAO dao = (AttributeOptionDAO) Context.get().getBean(AttributeOptionDAO.class);
			dao.delete(option.getId());
		} catch (Exception e) {
			throw new ServerException("Option has not been deleted", e);
		}
	}

	@Override
	public GUIAttributeSet save(GUIAttributeSet guiAttributeSet) throws ServerException {
		Session session = validateSession(getThreadLocalRequest());

		try {
			AttributeSet attributeSet;
			if (guiAttributeSet.getId() != 0) {
				AttributeSetDAO dao = (AttributeSetDAO) Context.get().getBean(AttributeSetDAO.class);
				attributeSet = dao.findById(guiAttributeSet.getId());
				dao.initialize(attributeSet);
			} else {
				attributeSet = new AttributeSet();
			}

			attributeSet.setTenantId(session.getTenantId());
			attributeSet.setName(guiAttributeSet.getName());
			attributeSet.setDescription(guiAttributeSet.getDescription());
			attributeSet.setReadonly(guiAttributeSet.isReadonly() ? 1 : 0);
			attributeSet.setType(guiAttributeSet.getType());

			saveAttributes(guiAttributeSet, attributeSet);

			store(guiAttributeSet, attributeSet);

			guiAttributeSet.setId(attributeSet.getId());
		} catch (Exception t) {
			return (GUIAttributeSet) throwServerException(session, log, t);
		}

		return guiAttributeSet;
	}

	private void store(GUIAttributeSet guiAttributeSet, AttributeSet attributeSet) throws ServerException {
		try {
			AttributeSetDAO dao = (AttributeSetDAO) Context.get().getBean(AttributeSetDAO.class);
			dao.store(attributeSet);
		} catch (Exception e) {
			throw new ServerException(
					String.format("AttributeSet has not been %s", guiAttributeSet.getId() != 0L ? "updated" : "stored"),
					e);
		}
	}

	private void saveAttributes(GUIAttributeSet attributeSet, AttributeSet attSet) {
		Map<String, Attribute> attrs = new HashMap<>();
		if (attributeSet.getAttributes() != null && attributeSet.getAttributes().length > 0) {
			attSet.getAttributes().clear();
			for (GUIAttribute attribute : attributeSet.getAttributes()) {
				if (attribute != null) {
					saveAttribute(attribute, attributeSet, attrs);
				}
			}
		}
		if (attrs.size() > 0)
			attSet.setAttributes(attrs);
	}

	private void saveAttribute(GUIAttribute attribute, GUIAttributeSet attributeSet, Map<String, Attribute> attrs) {
		Attribute att = new Attribute();
		att.setSetId(attributeSet.getId());
		att.setPosition(attribute.getPosition());
		att.setMandatory(attribute.isMandatory() ? 1 : 0);
		att.setHidden(attribute.isHidden() ? 1 : 0);
		att.setReadonly(attribute.isReadonly() ? 1 : 0);
		att.setMultiple(attribute.isMultiple() ? 1 : 0);
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
		Session session = validateSession(getThreadLocalRequest());
		AttributeSetDAO dao = (AttributeSetDAO) Context.get().getBean(AttributeSetDAO.class);
		AttributeSet set = dao.findByName(name, session.getTenantId());
		if (set != null)
			return getAttributeSet(set.getId());
		else
			return null;
	}

	@Override
	public GUIAttributeSet getAttributeSet(long setId) throws ServerException {
		validateSession(getThreadLocalRequest());

		AttributeSetDAO dao = (AttributeSetDAO) Context.get().getBean(AttributeSetDAO.class);
		try {
			AttributeSet attributeSet = dao.findById(setId);
			if (attributeSet == null)
				return null;

			GUIAttributeSet attSet = new GUIAttributeSet();
			attSet.setId(setId);
			attSet.setName(attributeSet.getName());
			attSet.setDescription(attributeSet.getDescription());
			attSet.setReadonly(attributeSet.getReadonly() == 1);
			attSet.setType(attributeSet.getType());

			GUIAttribute[] attributes = readAttributes(attributeSet);

			if (attributes.length > 0) {
				Arrays.sort(attributes);
				attSet.setAttributes(attributes);
			}

			return attSet;
		} catch (Exception t) {
			log.error(t.getMessage(), t);
		}

		return null;
	}

	private GUIAttribute[] readAttributes(AttributeSet attributeSet) {
		GUIAttribute[] attributes = new GUIAttribute[attributeSet.getAttributeNames().size()];
		int i = 0;
		for (String attrName : attributeSet.getAttributeNames()) {
			attributes[i] = readAttribute(attrName, attributeSet);
			i++;
		}
		return attributes;
	}

	private GUIAttribute readAttribute(String attrName, AttributeSet attributeSet) {
		Attribute extAttr = attributeSet.getAttributes().get(attrName);
		GUIAttribute att = new GUIAttribute();
		att.setName(attrName);
		att.setParent(extAttr.getParent());
		att.setStringValues(extAttr.getStringValues());
		att.setSet(attributeSet.getName());
		att.setSetId(attributeSet.getId());
		att.setPosition(extAttr.getPosition());
		att.setMandatory(intToBoolean(extAttr.getMandatory()));
		att.setHidden(intToBoolean(extAttr.getHidden()));
		att.setReadonly(intToBoolean(extAttr.getReadonly()));
		att.setMultiple(intToBoolean(extAttr.getMultiple()));
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
		String buf = (String) extAttr.getStringValue();
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
		att.setOptions(list.toArray(new String[0]));
	}

	private boolean intToBoolean(int val) {
		return val == 1 ? true : false;
	}

	@Override
	public GUIAttributeSet[] getAttributeSets() throws ServerException {
		Session session = validateSession(getThreadLocalRequest());
		try {
			AttributeSetDAO dao = (AttributeSetDAO) Context.get().getBean(AttributeSetDAO.class);
			List<GUIAttributeSet> guiSets = new ArrayList<>();
			List<Long> setIds = dao.findAllIds(session.getTenantId());
			for (Long setId : setIds)
				guiSets.add(getAttributeSet(setId));
			return guiSets.toArray(new GUIAttributeSet[0]);
		} catch (Exception t) {
			return (GUIAttributeSet[]) throwServerException(session, log, t);
		}
	}

	@Override
	public GUIValue[] parseOptions(long setId, String attribute) throws ServerException {
		Session session = validateSession(getThreadLocalRequest());

		Map<String, File> uploadedFilesMap = UploadServlet.getReceivedFiles(session.getSid());
		File file = uploadedFilesMap.values().iterator().next();
		List<GUIValue> options = new ArrayList<>();

		try (CSVFileReader reader = new CSVFileReader(file.getPath());) {
			List<String> row = reader.readFields();
			if (row != null && "value".equals(row.get(0).toLowerCase()))
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
			UploadServlet.cleanReceivedFiles(session.getSid());
		}

		GUIValue[] optionsArray = options.toArray(new GUIValue[0]);
		saveOptions(setId, attribute, optionsArray);
		return optionsArray;
	}

	@Override
	public void applyValidationToTemplates(long setId, String attribute) throws ServerException {
		Session session = validateSession(getThreadLocalRequest());
		try {
			AttributeSetDAO dao = (AttributeSetDAO) Context.get().getBean(AttributeSetDAO.class);
			AttributeSet set = dao.findById(setId);
			dao.initialize(set);

			Attribute setAttribute = set.getAttribute(attribute);
			if (setAttribute == null)
				return;

			int count = dao.jdbcUpdate(
					"update ld_template_ext set ld_validation = ? where ld_setid=" + setId + " and ld_name = ?",
					setAttribute.getValidation(), attribute);
			log.info("Updated the validation of {} template attributes named {}", count, attribute);
		} catch (Exception t) {
			throwServerException(session, log, t);
		}
	}

	@Override
	public void applyInitializationToTemplates(long setId, String attribute) throws ServerException {
		Session session = validateSession(getThreadLocalRequest());
		try {
			AttributeSetDAO dao = (AttributeSetDAO) Context.get().getBean(AttributeSetDAO.class);
			AttributeSet set = dao.findById(setId);
			dao.initialize(set);
			Attribute setAttribute = set.getAttribute(attribute);
			if (setAttribute == null)
				return;

			int count = dao.jdbcUpdate(
					"update ld_template_ext set ld_initialization = ? where ld_setid=" + setId + " and ld_name = ?",
					setAttribute.getInitialization(), attribute);
			log.info("Updated the initialization of {} template attributes named {}", count, attribute);
		} catch (Exception t) {
			throwServerException(session, log, t);
		}
	}
}