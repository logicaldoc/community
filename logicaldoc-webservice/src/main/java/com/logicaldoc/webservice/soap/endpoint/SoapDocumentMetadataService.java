package com.logicaldoc.webservice.soap.endpoint;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.AttributeOption;
import com.logicaldoc.core.metadata.AttributeOptionDAO;
import com.logicaldoc.core.metadata.AttributeSet;
import com.logicaldoc.core.metadata.AttributeSetDAO;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.metadata.TemplateGroup;
import com.logicaldoc.core.security.Group;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.dao.GroupDAO;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.webservice.AbstractService;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSAttribute;
import com.logicaldoc.webservice.model.WSAttributeOption;
import com.logicaldoc.webservice.model.WSAttributeSet;
import com.logicaldoc.webservice.model.WSRight;
import com.logicaldoc.webservice.model.WSTemplate;
import com.logicaldoc.webservice.model.WSUtil;
import com.logicaldoc.webservice.soap.DocumentMetadataService;

/**
 * Document Metadata Web Service Implementation
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
public class SoapDocumentMetadataService extends AbstractService implements DocumentMetadataService {

	private static final String TEMPLATE = "template ";

	@Override
	public WSTemplate[] listTemplates(String sid)
			throws AuthenticationException, WebserviceException, PersistenceException {
		User user = validateSession(sid);
		List<WSTemplate> templates = new ArrayList<>();
		TemplateDAO dao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
		for (Template template : dao.findAll(user.getTenantId()))
			if (dao.isReadEnable(template.getId(), user.getId()))
				templates.add(WSUtil.toWSTemplate(template));
		return templates.toArray(new WSTemplate[0]);
	}

	@Override
	public WSTemplate getTemplate(String sid, String name)
			throws AuthenticationException, WebserviceException, PersistenceException {
		User user = validateSession(sid);
		TemplateDAO dao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
		Template template = dao.findByName(name, user.getTenantId());
		if (template != null && dao.isReadEnable(template.getId(), user.getId()))
			return WSUtil.toWSTemplate(template);
		else
			return null;
	}

	@Override
	public WSTemplate getTemplateById(String sid, long templateId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		User user = validateSession(sid);
		TemplateDAO dao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
		Template template = dao.findById(templateId);
		if (template != null && dao.isReadEnable(template.getId(), user.getId()))
			return WSUtil.toWSTemplate(template);
		else
			return null;
	}

	@Override
	public long storeTemplate(String sid, WSTemplate wsTemplate)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		User user = validateSession(sid);

		Template template = loadTemplate(sid, wsTemplate, user);

		TemplateDAO dao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
		Map<String, Attribute> attrs = new HashMap<>();
		if (wsTemplate.getAttributes() != null && wsTemplate.getAttributes().length > 0) {
			template.getAttributes().clear();
			for (WSAttribute attribute : wsTemplate.getAttributes()) {
				if (attribute != null) {
					Attribute att = new Attribute();
					att.setPosition(attribute.getPosition());
					att.setMandatory(attribute.getMandatory());
					att.setHidden(attribute.getHidden());
					att.setReadonly(attribute.getReadonly());
					att.setMultiple(attribute.getMultiple());
					att.setParent(attribute.getParent());
					att.setLabel(attribute.getLabel());
					if (StringUtils.isEmpty(attribute.getLabel()))
						att.setLabel(attribute.getName());
					att.setStringValue(attribute.getStringValue());
					att.setIntValue(attribute.getIntValue());
					att.setDateValue(AbstractService.convertStringToDate(attribute.getDateValue()));
					att.setDoubleValue(attribute.getDoubleValue());
					att.setType(attribute.getType());
					att.setEditor(attribute.getEditor());
					att.setSetId(attribute.getSetId());
					att.setDependsOn(attribute.getDependsOn());
					att.setValidation(attribute.getValidation());
					att.setInitialization(attribute.getInitialization());

					attrs.put(attribute.getName(), att);
				}
			}
		}
		if (attrs.size() > 0)
			template.setAttributes(attrs);

		dao.store(template);
		return template.getId();
	}

	private Template loadTemplate(String sid, WSTemplate wsTemplate, User user)
			throws PersistenceException, WebserviceException, PermissionException {
		TemplateDAO dao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
		Template template = WSUtil.toTemplate(wsTemplate);
		template.setTenantId(user.getTenantId());
		if (wsTemplate.getId() != 0) {
			template = dao.findById(wsTemplate.getId());
			dao.initialize(template);
			template.setName(wsTemplate.getName());
			template.setDescription(wsTemplate.getDescription());

			if (template.getReadonly() == 1 || !isTemplateWritable(sid, template.getId()))
				throw new PermissionException(user.getUsername(), TEMPLATE + wsTemplate.getName(), "read");
		}

		if (StringUtils.isEmpty(template.getName()))
			throw new WebserviceException("Missing mandatory value 'Name'");
		return template;
	}

	@Override
	public void deleteTemplate(String sid, long templateId)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		User user = validateSession(sid);
		TemplateDAO dao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
		if (dao.countDocs(templateId) > 0)
			throw new WebserviceException("You cannot delete template with id " + templateId
					+ " because some documents belongs to that template.");
		Template templ = dao.findById(templateId);
		if (templ.getReadonly() == 1 || !isTemplateWritable(sid, templateId))
			throw new PermissionException(user.getUsername(), TEMPLATE + templ.getName(), "write");

		dao.delete(templateId);
	}

	@Override
	public void setAttributeOptions(String sid, long setId, String attribute, WSAttributeOption[] wsOptions)
			throws WebserviceException, PersistenceException {
		checkAdministrator(sid);

		AttributeOptionDAO dao = (AttributeOptionDAO) Context.get().getBean(AttributeOptionDAO.class);
		dao.deleteBySetIdAndAttribute(setId, attribute);

		if (wsOptions == null || wsOptions.length == 0) {
			return;
		}
		for (int i = 0; i < wsOptions.length; i++) {
			AttributeOption option = new AttributeOption(setId, attribute, wsOptions[i].getValue(),
					wsOptions[i].getCategory());
			option.setPosition(i);
			dao.store(option);
		}
	}

	@Override
	public String[] getAttributeOptions(String sid, long setId, String attribute)
			throws AuthenticationException, WebserviceException, PersistenceException {
		validateSession(sid);
		AttributeOptionDAO dao = (AttributeOptionDAO) Context.get().getBean(AttributeOptionDAO.class);

		List<AttributeOption> options = dao.findByAttribute(setId, attribute);
		return options.stream().map(o -> o.getValue()).collect(Collectors.toList()).toArray(new String[0]);
	}

	@Override
	public WSAttributeOption[] getAttributeOptionsByCategory(String sid, long setId, String attribute, String category)
			throws AuthenticationException, WebserviceException, PersistenceException {
		validateSession(sid);
		AttributeOptionDAO dao = (AttributeOptionDAO) Context.get().getBean(AttributeOptionDAO.class);

		List<AttributeOption> options = dao.findByAttributeAndCategory(setId, attribute, category);
		
		
		return options.stream().map(o -> new WSAttributeOption(o.getValue(), o.getCategory()))
				.collect(Collectors.toList()).toArray(new WSAttributeOption[0]);
	}

	@Override
	public WSAttributeSet[] listAttributeSets(String sid)
			throws AuthenticationException, WebserviceException, PersistenceException {
		User user = validateSession(sid);
		List<WSAttributeSet> templates = new ArrayList<>();
		AttributeSetDAO dao = (AttributeSetDAO) Context.get().getBean(AttributeSetDAO.class);
		for (AttributeSet set : dao.findAll(user.getTenantId()))
			templates.add(WSUtil.toWSAttributeSet(set));
		return templates.toArray(new WSAttributeSet[0]);
	}

	@Override
	public WSAttributeSet getAttributeSet(String sid, String name)
			throws AuthenticationException, WebserviceException, PersistenceException {
		User user = validateSession(sid);
		AttributeSetDAO dao = (AttributeSetDAO) Context.get().getBean(AttributeSetDAO.class);
		AttributeSet set = dao.findByName(name, user.getTenantId());
		if (set != null)
			return WSUtil.toWSAttributeSet(set);
		else
			return null;
	}

	@Override
	public WSAttributeSet getAttributeSetById(String sid, long setId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		validateSession(sid);
		AttributeSetDAO dao = (AttributeSetDAO) Context.get().getBean(AttributeSetDAO.class);
		AttributeSet set = dao.findById(setId);
		if (set != null)
			return WSUtil.toWSAttributeSet(set);
		else
			return null;
	}

	@Override
	public long storeAttributeSet(String sid, WSAttributeSet attributeSet)
			throws WebserviceException, PersistenceException {
		checkAdministrator(sid);
		User user = validateSession(sid);

		AttributeSetDAO dao = (AttributeSetDAO) Context.get().getBean(AttributeSetDAO.class);
		AttributeSet set = WSUtil.toAttributeSet(attributeSet);
		set.setTenantId(user.getTenantId());

		if (attributeSet.getId() != 0) {
			set = dao.findById(attributeSet.getId());
			dao.initialize(set);
			set.setName(attributeSet.getName());
			set.setDescription(attributeSet.getDescription());
		}

		if (StringUtils.isEmpty(set.getName()))
			throw new WebserviceException("Missing mandatory value 'Name'");

		Map<String, Attribute> attrs = new HashMap<>();
		if (attributeSet.getAttributes() != null && attributeSet.getAttributes().length > 0) {
			set.getAttributes().clear();
			for (WSAttribute attribute : attributeSet.getAttributes()) {
				if (attribute != null) {
					Attribute att = new Attribute();
					att.setPosition(attribute.getPosition());
					att.setMandatory(attribute.getMandatory());
					att.setHidden(attribute.getHidden());
					att.setReadonly(attribute.getReadonly());
					att.setMultiple(attribute.getMultiple());
					att.setParent(attribute.getParent());
					att.setLabel(attribute.getLabel());
					if (StringUtils.isEmpty(attribute.getLabel()))
						att.setLabel(attribute.getName());
					att.setStringValue(attribute.getStringValue());
					att.setIntValue(attribute.getIntValue());
					att.setDateValue(AbstractService.convertStringToDate(attribute.getDateValue()));
					att.setDoubleValue(attribute.getDoubleValue());
					att.setType(attribute.getType());
					att.setEditor(attribute.getEditor());
					att.setSetId(attribute.getSetId());
					att.setDependsOn(attribute.getDependsOn());
					att.setValidation(attribute.getValidation());
					att.setInitialization(attribute.getInitialization());
					attrs.put(attribute.getName(), att);
				}
			}
		}
		if (attrs.size() > 0)
			set.setAttributes(attrs);

		dao.store(set);
		return set.getId();
	}

	@Override
	public void deleteAttributeSet(String sid, long setId) throws WebserviceException, PersistenceException {
		checkAdministrator(sid);
		AttributeSetDAO dao = (AttributeSetDAO) Context.get().getBean(AttributeSetDAO.class);
		dao.delete(setId);
	}

	@Override
	public boolean isTemplateReadable(String sid, long templateId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		User user = validateSession(sid);
		TemplateDAO dao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
		return dao.isReadEnable(templateId, user.getId());
	}

	@Override
	public boolean isTemplateWritable(String sid, long templateId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		User user = validateSession(sid);
		TemplateDAO dao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
		return dao.isWriteEnable(templateId, user.getId());
	}

	@Override
	public void grantUserToTemplate(String sid, long templateId, long userId, int permissions)
			throws PersistenceException, AuthenticationException, WebserviceException, PermissionException {
		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
		User user = userDao.findById(userId);
		grantGroupToTemplate(sid, templateId, user.getUserGroup().getId(), permissions);
	}

	@Override
	public void grantGroupToTemplate(String sid, long templateId, long groupId, int permissions)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		User user = validateSession(sid);
		if (!isTemplateWritable(sid, templateId))
			throw new PermissionException(user.getUsername(), TEMPLATE + templateId, "write");

		TemplateDAO dao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
		Template templ = dao.findById(templateId);
		if (templ == null || templ.getReadonly() == 1)
			return;

		TemplateGroup fg = new TemplateGroup();
		fg.setGroupId(groupId);
		fg.setPermissions(permissions);
		templ.addTemplateGroup(fg);

		dao.store(templ);
	}

	private WSRight[] getGranted(String sid, long templateId, boolean users)
			throws AuthenticationException, WebserviceException, PersistenceException {
		validateSession(sid);

		List<WSRight> rightsList = new ArrayList<>();
		TemplateDAO templateDao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
		GroupDAO groupDao = (GroupDAO) Context.get().getBean(GroupDAO.class);

		Template template = templateDao.findById(templateId);
		templateDao.initialize(template);
		for (TemplateGroup tg : template.getTemplateGroups()) {
			Group group = groupDao.findById(tg.getGroupId());
			if (group.getName().startsWith("_user_") && users) {
				rightsList.add(
						new WSRight(Long.parseLong(group.getName().substring(group.getName().lastIndexOf('_') + 1)),
								tg.getPermissions()));
			} else if (!group.getName().startsWith("_user_") && !users)
				rightsList.add(new WSRight(group.getId(), tg.getPermissions()));
		}

		return rightsList.toArray(new WSRight[rightsList.size()]);
	}

	@Override
	public WSRight[] getGrantedUsers(String sid, long templateId)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		User user = validateSession(sid);
		if (!isTemplateReadable(sid, templateId))
			throw new PermissionException(user.getUsername(), TEMPLATE + templateId, "read");

		return getGranted(sid, templateId, true);
	}

	@Override
	public WSRight[] getGrantedGroups(String sid, long templateId)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		User user = validateSession(sid);
		if (!isTemplateReadable(sid, templateId))
			throw new PermissionException(user.getUsername(), TEMPLATE + templateId, "read");

		return getGranted(sid, templateId, false);
	}

	@Override
	public void addAttributeOption(String sid, long setId, String attribute, WSAttributeOption wsoption) throws AuthenticationException, WebserviceException, PersistenceException {

		validateSession(sid);
	
		if (wsoption == null)
			return;

		AttributeOptionDAO dao = (AttributeOptionDAO) Context.get().getBean(AttributeOptionDAO.class);
		
		AttributeOption option = new AttributeOption(setId, attribute, wsoption.getValue(), wsoption.getCategory());
		dao.store(option);
	}
	
}