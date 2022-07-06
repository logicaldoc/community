package com.logicaldoc.web.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
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
import com.logicaldoc.core.metadata.TemplateGroup;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.User;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIRight;
import com.logicaldoc.gui.common.client.beans.GUITemplate;
import com.logicaldoc.gui.frontend.client.services.TemplateService;
import com.logicaldoc.util.Context;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * Implementation of the TemplateService
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class TemplateServiceImpl extends RemoteServiceServlet implements TemplateService {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(TemplateServiceImpl.class);

	@Override
	public void delete(long templateId) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			TemplateDAO dao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
			Template template = dao.findById(templateId);
			if (template == null)
				return;

			if (template.getReadonly() == 1 || !dao.isWriteEnable(templateId, session.getUserId()))
				throw new Exception("You do not have the permission");

			boolean deleted = dao.delete(templateId);
			if (!deleted)
				throw new Exception("Template has not been deleted");
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
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
		User sessionUser = session.getUser();

		TemplateDAO dao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
		try {
			Template templ;
			if (template.getId() != 0) {
				templ = dao.findById(template.getId());
				dao.initialize(templ);
				if (!sessionUser.isMemberOf("admin")
						&& (templ.getReadonly() == 1 || !dao.isWriteEnable(templ.getId(), session.getUserId())))
					throw new Exception("You do not have the permission");
			} else {
				templ = new Template();
				if (!sessionUser.isMemberOf("admin")
						&& (template.getRights() == null || template.getRights().length == 0)) {
					// At least the current user must have access to this
					// template
					TemplateGroup wg = new TemplateGroup(session.getUser().getUserGroup().getId());
					wg.setWrite(1);
					templ.addTemplateGroup(wg);
				}
			}

			if (templ.getReadonly() == 0) {
				templ.setTenantId(session.getTenantId());
				templ.setName(template.getName());
				templ.setDescription(template.getDescription());
				templ.setReadonly(template.isReadonly() ? 1 : 0);
				templ.setType(template.getType());

				Map<String, Attribute> attrs = new HashMap<String, Attribute>();
				if (template.getAttributes() != null && template.getAttributes().length > 0) {
					templ.getAttributes().clear();
					for (GUIAttribute attribute : template.getAttributes()) {
						if (attribute != null) {
							Attribute att = new Attribute();
							att.setMandatory(attribute.isMandatory() ? 1 : 0);
							att.setHidden(attribute.isHidden() ? 1 : 0);
							att.setMultiple(attribute.isMultiple() ? 1 : 0);
							att.setParent(attribute.getParent());
							att.setType(attribute.getType());
							att.setLabel(attribute.getLabel());
							att.setEditor(attribute.getEditor());
							att.setStringValue(attribute.getStringValue());
							att.setStringValues(attribute.getStringValues());
							att.setSetId(attribute.getSetId());
							att.setPosition(attribute.getPosition());
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
			}

			/*
			 * Save the security settings
			 */
			if (templ.getReadonly() == 0 || sessionUser.isMemberOf("admin")) {
				Set<TemplateGroup> grps = new HashSet<TemplateGroup>();
				for (GUIRight right : template.getRights()) {
					boolean isAdmin = right.getEntityId() == 1;
					TemplateGroup wg = null;
					if (right.isRead()) {
						wg = new TemplateGroup();
						wg.setGroupId(right.getEntityId());
					}
					grps.add(wg);

					if (isAdmin || right.isWrite())
						wg.setWrite(1);
					else
						wg.setWrite(0);
				}
				templ.getTemplateGroups().clear();
				templ.getTemplateGroups().addAll(grps);
			}

			boolean stored = dao.store(templ);
			if (!stored)
				throw new Exception(
						String.format("Template has not been %s", templ.getId() != 0L ? "updated" : "stored"));

			template.setId(templ.getId());
		} catch (Throwable t) {
			return (GUITemplate) ServiceUtil.throwServerException(session, log, t);
		}

		return template;
	}

	@Override
	public GUITemplate getTemplate(long templateId) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		TemplateDAO dao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
		try {
			Template template = dao.findById(templateId);
			if (template == null)
				return null;

			dao.initialize(template);

			GUITemplate templ = new GUITemplate();
			templ.setId(templateId);
			templ.setName(template.getName());
			templ.setDescription(template.getDescription());
			templ.setReadonly(template.getReadonly() == 1);
			templ.setType(template.getType());

			Set<Permission> permissions = dao.getEnabledPermissions(templateId, session.getUserId());
			List<String> permissionsList = new ArrayList<String>();
			for (Permission permission : permissions)
				permissionsList.add(permission.toString());
			templ.setPermissions(permissionsList.toArray(new String[permissionsList.size()]));

			AttributeSetDAO setDao = (AttributeSetDAO) Context.get().getBean(AttributeSetDAO.class);
			Map<Long, AttributeSet> sets = setDao.load(template.getTenantId());

			GUIAttribute[] attributes = new GUIAttribute[template.getAttributeNames().size()];
			int i = 0;
			for (String attrName : template.getAttributeNames()) {
				Attribute templateExtAttr = template.getAttributes().get(attrName);
				AttributeSet aSet = sets.get(templateExtAttr.getSetId());
				Attribute setExtAttr = aSet != null ? aSet.getAttribute(attrName) : null;

				GUIAttribute att = new GUIAttribute();
				att.setName(attrName);
				att.setSetId(templateExtAttr.getSetId());
				att.setSet(aSet != null ? aSet.getName() : null);
				att.setPosition(templateExtAttr.getPosition());
				att.setMandatory(templateExtAttr.getMandatory() == 1 ? true : false);
				att.setHidden(templateExtAttr.getHidden() == 1 ? true : false);
				att.setMultiple(templateExtAttr.getMultiple() == 1 ? true : false);
				att.setParent(templateExtAttr.getParent());
				att.setStringValues(templateExtAttr.getStringValues());
				att.setType(templateExtAttr.getType());
				if (StringUtils.isEmpty(templateExtAttr.getLabel()))
					att.setLabel(attrName);
				else
					att.setLabel(templateExtAttr.getLabel());
				if (templateExtAttr.getValue() instanceof String)
					att.setStringValue(templateExtAttr.getStringValue());
				else if (templateExtAttr.getValue() instanceof Long)
					att.setIntValue(templateExtAttr.getIntValue());
				else if (templateExtAttr.getValue() instanceof Double)
					att.setDoubleValue(templateExtAttr.getDoubleValue());
				else if (templateExtAttr.getValue() instanceof Date)
					att.setDateValue(ServiceUtil.convertToDate(templateExtAttr.getDateValue()));
				else if (templateExtAttr.getValue() instanceof Boolean)
					att.setBooleanValue(templateExtAttr.getBooleanValue());

				att.setEditor(templateExtAttr.getEditor());
				if (templateExtAttr.getType() == Attribute.TYPE_USER
						|| templateExtAttr.getEditor() == Attribute.EDITOR_LISTBOX) {

					String buf = setExtAttr != null ? (String) setExtAttr.getStringValue()
							: (String) templateExtAttr.getStringValue();
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