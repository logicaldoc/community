package com.logicaldoc.web.service;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.Vector;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gwt.user.server.rpc.RemoteServiceServlet;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.AttributeOption;
import com.logicaldoc.core.metadata.AttributeOptionDAO;
import com.logicaldoc.core.metadata.AttributeSet;
import com.logicaldoc.core.metadata.AttributeSetDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIAttributeSet;
import com.logicaldoc.gui.frontend.client.services.AttributeSetService;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.csv.CSVFileReader;
import com.logicaldoc.web.UploadServlet;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * Implementation of the AttributeSetService
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class AttributeSetServiceImpl extends RemoteServiceServlet implements AttributeSetService {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(AttributeSetServiceImpl.class);

	@Override
	public void delete(long setId) throws ServerException {
		ServiceUtil.validateSession(getThreadLocalRequest());

		AttributeSetDAO dao = (AttributeSetDAO) Context.get().getBean(AttributeSetDAO.class);
		dao.delete(setId);
	}

	@Override
	public void saveOptions(long setId, String attribute, String[] values) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		AttributeOptionDAO dao = (AttributeOptionDAO) Context.get().getBean(AttributeOptionDAO.class);
		try {
			Map<String, AttributeOption> optionsMap = new HashMap<String, AttributeOption>();
			List<AttributeOption> options = dao.findBySetIdAndAttribute(setId, attribute);
			for (AttributeOption option : options)
				optionsMap.put(option.getValue(), option);

			for (int i = 0; i < values.length; i++) {
				String value = values[i];
				AttributeOption option = optionsMap.get(value);
				if (option == null) {
					option = new AttributeOption(setId, attribute, value);
				} else {
					if (value.equals(option.getValue()) && option.getPosition() == i)
						continue;
					option.setValue(value);
				}

				option.setPosition(i);
				dao.store(option);
			}
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void deleteOptions(long setId, String attribute, String[] values) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		AttributeOptionDAO dao = (AttributeOptionDAO) Context.get().getBean(AttributeOptionDAO.class);
		try {
			List<AttributeOption> options = dao.findBySetIdAndAttribute(setId, attribute);
			for (AttributeOption option : options)
				for (String value : values)
					if (value.equals(option.getValue())) {
						dao.delete(option.getId());
						break;
					}
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public GUIAttributeSet save(GUIAttributeSet attributeSet) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		AttributeSetDAO dao = (AttributeSetDAO) Context.get().getBean(AttributeSetDAO.class);
		try {
			AttributeSet attSet;
			if (attributeSet.getId() != 0) {
				attSet = dao.findById(attributeSet.getId());
				dao.initialize(attSet);
			} else {
				attSet = new AttributeSet();
			}

			attSet.setTenantId(session.getTenantId());
			attSet.setName(attributeSet.getName());
			attSet.setDescription(attributeSet.getDescription());
			attSet.setReadonly(attributeSet.isReadonly() ? 1 : 0);
			attSet.setType(attributeSet.getType());

			Map<String, Attribute> attrs = new HashMap<String, Attribute>();
			if (attributeSet.getAttributes() != null && attributeSet.getAttributes().length > 0) {
				attSet.getAttributes().clear();
				for (GUIAttribute attribute : attributeSet.getAttributes()) {
					if (attribute != null) {
						Attribute att = new Attribute();
						att.setSetId(attributeSet.getId());
						att.setPosition(attribute.getPosition());
						att.setMandatory(attribute.isMandatory() ? 1 : 0);
						att.setType(attribute.getType());
						att.setLabel(attribute.getLabel());
						att.setEditor(attribute.getEditor());
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
				}
			}
			if (attrs.size() > 0)
				attSet.setAttributes(attrs);

			dao.store(attSet);

			attributeSet.setId(attSet.getId());
		} catch (Throwable t) {
			return (GUIAttributeSet) ServiceUtil.throwServerException(session, log, t);
		}

		return attributeSet;
	}

	public GUIAttributeSet getAttributeSet(String name) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());
		AttributeSetDAO dao = (AttributeSetDAO) Context.get().getBean(AttributeSetDAO.class);
		AttributeSet set = dao.findByName(name, session.getTenantId());
		if (set != null)
			return getAttributeSet(set.getId());
		else
			return null;
	}

	@Override
	public GUIAttributeSet getAttributeSet(long setId) throws ServerException {
		ServiceUtil.validateSession(getThreadLocalRequest());

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

			GUIAttribute[] attributes = new GUIAttribute[attributeSet.getAttributeNames().size()];
			int i = 0;
			for (String attrName : attributeSet.getAttributeNames()) {
				Attribute extAttr = attributeSet.getAttributes().get(attrName);
				GUIAttribute att = new GUIAttribute();
				att.setName(attrName);
				att.setSet(attributeSet.getName());
				att.setSetId(attributeSet.getId());
				att.setPosition(extAttr.getPosition());
				att.setMandatory(extAttr.getMandatory() == 1 ? true : false);
				att.setType(extAttr.getType());
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
					att.setDateValue(ServiceUtil.convertToDate(extAttr.getDateValue()));
				else if (extAttr.getValue() instanceof Boolean)
					att.setBooleanValue(extAttr.getBooleanValue());

				att.setEditor(extAttr.getEditor());
				if (extAttr.getType() == Attribute.TYPE_USER || extAttr.getEditor() == Attribute.EDITOR_LISTBOX) {
					String buf = (String) extAttr.getStringValue();
					List<String> list = new ArrayList<String>();
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
				attributes[i] = att;
				i++;
			}
			if (attributes.length > 0)
				attSet.setAttributes(attributes);

			return attSet;
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
		}

		return null;
	}

	@Override
	public String[] parseOptions(long setId, String attribute) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		Map<String, File> uploadedFilesMap = UploadServlet.getReceivedFiles(getThreadLocalRequest(), session.getSid());
		List<String> options = new ArrayList<String>();

		CSVFileReader reader = null;
		try {
			File file = null;
			for (String fileId : uploadedFilesMap.keySet())
				if (fileId.startsWith("LDOC_OPT")) {
					file = uploadedFilesMap.get(fileId);
					break;
				}

			if (file != null) {
				reader = new CSVFileReader(file.getPath());
				Vector<String> row = reader.readFields();
				if (row != null && "value".equals(row.get(0).toLowerCase()))
					row = reader.readFields();
				while (row != null && !row.isEmpty()) {
					options.add(row.get(0).trim());
					row = reader.readFields();
				}
			}
		} catch (Throwable e) {
			log.error("Unable to parse options in CSV file", e);
		} finally {
			if (reader != null)
				try {
					reader.close();
				} catch (IOException e) {
				}
		}

		String[] values = options.toArray(new String[0]);
		if (values.length > 0)
			saveOptions(setId, attribute, values);
		return values;
	}
}