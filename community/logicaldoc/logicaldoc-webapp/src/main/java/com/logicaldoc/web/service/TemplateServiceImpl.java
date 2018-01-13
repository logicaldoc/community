package com.logicaldoc.web.service;

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

import com.google.gwt.user.server.rpc.RemoteServiceServlet;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.AttributeSet;
import com.logicaldoc.core.metadata.AttributeSetDAO;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUITemplate;
import com.logicaldoc.gui.frontend.client.services.TemplateService;
import com.logicaldoc.util.Context;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * Implementation of the TemplateService
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 6.0
 */
public class TemplateServiceImpl extends RemoteServiceServlet implements TemplateService {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(TemplateServiceImpl.class);

	@Override
	public void delete(long templateId) throws ServerException {
		ServiceUtil.validateSession(getThreadLocalRequest());

		TemplateDAO dao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
		dao.delete(templateId);
	}

	@Override
	public long countDocuments(long templateId) throws ServerException {
		ServiceUtil.validateSession(getThreadLocalRequest());

		TemplateDAO dao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
		return dao.countDocs(templateId);
	}

	@Override
	public GUITemplate save(GUITemplate template) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		TemplateDAO dao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
		try {
			Template templ;
			if (template.getId() != 0) {
				templ = dao.findById(template.getId());
				dao.initialize(templ);
			} else {
				templ = new Template();
			}

			templ.setTenantId(session.getTenantId());
			templ.setName(template.getName());
			templ.setDescription(template.getDescription());
			templ.setReadonly(template.isReadonly() ? 1 : 0);
			templ.setType(template.getType());

			Map<String, Attribute> attrs = new HashMap<String, Attribute>();
			if (template.getAttributes() != null && template.getAttributes().length > 0) {
				templ.getAttributes().clear();
				int position = 0;
				for (GUIAttribute attribute : template.getAttributes()) {
					if (attribute != null) {
						Attribute att = new Attribute();
						att.setPosition(position++);
						att.setMandatory(attribute.isMandatory() ? 1 : 0);
						att.setType(attribute.getType());
						att.setLabel(attribute.getLabel());
						att.setEditor(attribute.getEditor());
						att.setStringValue(attribute.getStringValue());
						att.setSetId(attribute.getSetId());
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
				templ.setAttributes(attrs);

			dao.store(templ);

			template.setId(templ.getId());
		} catch (Throwable t) {
			return (GUITemplate) ServiceUtil.throwServerException(session, log, t);
		}

		return template;
	}

	@Override
	public GUITemplate getTemplate(long templateId) throws ServerException {
		ServiceUtil.validateSession(getThreadLocalRequest());

		TemplateDAO dao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
		try {
			Template template = dao.findById(templateId);
			if (template == null)
				return null;

			GUITemplate templ = new GUITemplate();
			templ.setId(templateId);
			templ.setName(template.getName());
			templ.setDescription(template.getDescription());
			templ.setReadonly(template.getReadonly() == 1);
			templ.setType(template.getType());

			AttributeSetDAO setDao = (AttributeSetDAO) Context.get().getBean(AttributeSetDAO.class);
			Map<Long, AttributeSet> sets = setDao.load(template.getTenantId());

			GUIAttribute[] attributes = new GUIAttribute[template.getAttributeNames().size()];
			int i = 0;
			for (String attrName : template.getAttributeNames()) {
				Attribute extAttr = template.getAttributes().get(attrName);
				GUIAttribute att = new GUIAttribute();
				att.setName(attrName);
				att.setSetId(extAttr.getSetId());
				att.setSet(sets.get(extAttr.getSetId()) != null ? sets.get(extAttr.getSetId()).getName() : null);
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
			if (attributes.length > 0) {
				Arrays.sort(attributes);
				templ.setAttributes(attributes);
			}

			return templ;
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
		}

		return null;
	}
}