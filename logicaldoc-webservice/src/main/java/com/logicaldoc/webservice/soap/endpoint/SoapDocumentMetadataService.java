package com.logicaldoc.webservice.soap.endpoint;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.AttributeOption;
import com.logicaldoc.core.metadata.AttributeOptionDAO;
import com.logicaldoc.core.metadata.AttributeSet;
import com.logicaldoc.core.metadata.AttributeSetDAO;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.security.AccessControlEntry;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.util.Context;
import com.logicaldoc.webservice.AbstractService;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSAccessControlEntry;
import com.logicaldoc.webservice.model.WSAttribute;
import com.logicaldoc.webservice.model.WSAttributeOption;
import com.logicaldoc.webservice.model.WSAttributeSet;
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

	private static final Logger log = LoggerFactory.getLogger(SoapDocumentMetadataService.class);

	private static final String TEMPLATE = "template ";

	@Override
	public List<WSTemplate> listTemplates(String sid)
			throws AuthenticationException, WebserviceException, PersistenceException {
		User user = validateSession(sid);
		List<WSTemplate> templates = new ArrayList<>();
		TemplateDAO dao = Context.get(TemplateDAO.class);
		for (Template template : dao.findAll(user.getTenantId()))
			if (dao.isReadEnable(template.getId(), user.getId()))
				templates.add(WSUtil.toWSTemplate(template));
		return templates;
	}

	@Override
	public WSTemplate getTemplate(String sid, String name)
			throws AuthenticationException, WebserviceException, PersistenceException {
		User user = validateSession(sid);
		TemplateDAO dao = Context.get(TemplateDAO.class);
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
		TemplateDAO dao = Context.get(TemplateDAO.class);
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

		TemplateDAO dao = Context.get(TemplateDAO.class);
		Map<String, Attribute> attrs = new HashMap<>();
		if (CollectionUtils.isNotEmpty(wsTemplate.getAttributes())) {
			template.getAttributes().clear();
			for (WSAttribute wsAttribute : wsTemplate.getAttributes()) {
				if (wsAttribute != null) {
					Attribute attribute = new Attribute();
					attribute.setPosition(wsAttribute.getPosition());
					attribute.setMandatory(wsAttribute.getMandatory());
					attribute.setHidden(wsAttribute.getHidden());
					attribute.setReadonly(wsAttribute.getReadonly());
					attribute.setMultiple(wsAttribute.getMultiple());
					attribute.setParent(wsAttribute.getParent());
					attribute.setLabel(wsAttribute.getLabel());
					if (StringUtils.isEmpty(wsAttribute.getLabel()))
						attribute.setLabel(wsAttribute.getName());
					attribute.setStringValue(wsAttribute.getStringValue());
					attribute.setIntValue(wsAttribute.getIntValue());
					attribute.setDateValue(AbstractService.convertStringToDate(wsAttribute.getDateValue()));
					attribute.setDoubleValue(wsAttribute.getDoubleValue());
					attribute.setType(wsAttribute.getType());
					attribute.setEditor(wsAttribute.getEditor());
					attribute.setSetId(wsAttribute.getSetId());
					attribute.setDependsOn(wsAttribute.getDependsOn());
					attribute.setValidation(wsAttribute.getValidation());
					attribute.setInitialization(wsAttribute.getInitialization());

					attrs.put(wsAttribute.getName(), attribute);
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
		TemplateDAO dao = Context.get(TemplateDAO.class);
		Template template = WSUtil.toTemplate(wsTemplate);
		template.setTenantId(user.getTenantId());
		if (wsTemplate.getId() != 0) {
			template = dao.findById(wsTemplate.getId());
			dao.initialize(template);
			template.setName(wsTemplate.getName());
			template.setDescription(wsTemplate.getDescription());

			if (template.getReadonly() == 1 || !isWritable(sid, template.getId()))
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
		TemplateDAO dao = Context.get(TemplateDAO.class);
		if (dao.countDocs(templateId) > 0)
			throw new WebserviceException("You cannot delete template with id " + templateId
					+ " because some documents belongs to that template.");
		Template templ = dao.findById(templateId);
		if (templ.getReadonly() == 1 || !isWritable(sid, templateId))
			throw new PermissionException(user.getUsername(), TEMPLATE + templ.getName(), "write");

		dao.delete(templateId);
	}

	@Override
	public void setAttributeOptions(String sid, long setId, String attribute, List<WSAttributeOption> wsOptions)
			throws WebserviceException, PersistenceException {
		checkAdministrator(sid);

		AttributeOptionDAO dao = Context.get(AttributeOptionDAO.class);
		dao.deleteBySetIdAndAttribute(setId, attribute);

		if (CollectionUtils.isEmpty(wsOptions))
			return;
		int i = 0;
		for (WSAttributeOption wsOption : wsOptions) {
			AttributeOption option = new AttributeOption(setId, attribute, wsOption.getValue(), wsOption.getCategory());
			option.setPosition(i++);
			dao.store(option);
		}
	}

	@Override
	public List<String> getAttributeOptions(String sid, long setId, String attribute)
			throws AuthenticationException, WebserviceException, PersistenceException {
		validateSession(sid);
		AttributeOptionDAO dao = Context.get(AttributeOptionDAO.class);

		List<AttributeOption> options = dao.findByAttribute(setId, attribute);
		return options.stream().map(o -> o.getValue()).collect(Collectors.toList());
	}

	@Override
	public List<WSAttributeOption> getAttributeOptionsByCategory(String sid, long setId, String attribute,
			String category) throws AuthenticationException, WebserviceException, PersistenceException {
		validateSession(sid);
		AttributeOptionDAO dao = Context.get(AttributeOptionDAO.class);

		List<AttributeOption> options = dao.findByAttributeAndCategory(setId, attribute, category);

		return options.stream().map(o -> new WSAttributeOption(o.getValue(), o.getCategory()))
				.collect(Collectors.toList());
	}

	@Override
	public List<WSAttributeSet> listAttributeSets(String sid)
			throws AuthenticationException, WebserviceException, PersistenceException {
		User user = validateSession(sid);
		List<WSAttributeSet> templates = new ArrayList<>();
		AttributeSetDAO dao = Context.get(AttributeSetDAO.class);
		for (AttributeSet set : dao.findAll(user.getTenantId()))
			templates.add(WSUtil.toWSAttributeSet(set));
		return templates;
	}

	@Override
	public WSAttributeSet getAttributeSet(String sid, String name)
			throws AuthenticationException, WebserviceException, PersistenceException {
		User user = validateSession(sid);
		AttributeSetDAO dao = Context.get(AttributeSetDAO.class);
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
		AttributeSetDAO dao = Context.get(AttributeSetDAO.class);
		AttributeSet set = dao.findById(setId);
		if (set != null)
			return WSUtil.toWSAttributeSet(set);
		else
			return null;
	}

	@Override
	public long storeAttributeSet(String sid, WSAttributeSet wsAttributeSet)
			throws WebserviceException, PersistenceException {
		checkAdministrator(sid);
		User user = validateSession(sid);

		AttributeSetDAO dao = Context.get(AttributeSetDAO.class);
		AttributeSet set = WSUtil.toAttributeSet(wsAttributeSet);
		set.setTenantId(user.getTenantId());

		if (wsAttributeSet.getId() != 0) {
			set = dao.findById(wsAttributeSet.getId());
			dao.initialize(set);
			set.setName(wsAttributeSet.getName());
			set.setDescription(wsAttributeSet.getDescription());
		}

		if (StringUtils.isEmpty(set.getName()))
			throw new WebserviceException("Missing mandatory value 'Name'");

		Map<String, Attribute> attrs = new HashMap<>();
		if (CollectionUtils.isNotEmpty(wsAttributeSet.getAttributes())) {
			set.getAttributes().clear();
			for (WSAttribute wsAttribute : wsAttributeSet.getAttributes()) {
				if (wsAttribute != null) {
					Attribute attribute = new Attribute();
					attribute.setPosition(wsAttribute.getPosition());
					attribute.setMandatory(wsAttribute.getMandatory());
					attribute.setHidden(wsAttribute.getHidden());
					attribute.setReadonly(wsAttribute.getReadonly());
					attribute.setMultiple(wsAttribute.getMultiple());
					attribute.setParent(wsAttribute.getParent());
					attribute.setLabel(wsAttribute.getLabel());
					if (StringUtils.isEmpty(wsAttribute.getLabel()))
						attribute.setLabel(wsAttribute.getName());
					attribute.setStringValue(wsAttribute.getStringValue());
					attribute.setIntValue(wsAttribute.getIntValue());
					attribute.setDateValue(AbstractService.convertStringToDate(wsAttribute.getDateValue()));
					attribute.setDoubleValue(wsAttribute.getDoubleValue());
					attribute.setType(wsAttribute.getType());
					attribute.setEditor(wsAttribute.getEditor());
					attribute.setSetId(wsAttribute.getSetId());
					attribute.setDependsOn(wsAttribute.getDependsOn());
					attribute.setValidation(wsAttribute.getValidation());
					attribute.setInitialization(wsAttribute.getInitialization());
					attrs.put(wsAttribute.getName(), attribute);
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
		AttributeSetDAO dao = Context.get(AttributeSetDAO.class);
		dao.delete(setId);
	}

	@Override
	public boolean isReadable(String sid, long templateId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		User user = validateSession(sid);
		TemplateDAO dao = Context.get(TemplateDAO.class);
		return dao.isReadEnable(templateId, user.getId());
	}

	@Override
	public boolean isWritable(String sid, long templateId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		User user = validateSession(sid);
		TemplateDAO dao = Context.get(TemplateDAO.class);
		return dao.isWriteEnable(templateId, user.getId());
	}

	@Override
	public void setAccessControlList(String sid, long templateId, List<WSAccessControlEntry> acl)
			throws PersistenceException, PermissionException, AuthenticationException, WebserviceException {
		User sessionUser = validateSession(sid);

		TemplateDAO templateDAO = Context.get(TemplateDAO.class);
		// Check if the session user has the Write Permission of this template
		if (!templateDAO.isWriteEnable(templateId, sessionUser.getId()))
			throw new PermissionException(sessionUser.getUsername(), "Template " + templateId, Permission.WRITE);

		Template template = templateDAO.findById(templateId);
		templateDAO.initialize(template);
		template.getAccessControlList().clear();
		for (WSAccessControlEntry wsAcwe : acl)
			template.addAccessControlEntry(WSUtil.toAccessControlEntry(wsAcwe));
		templateDAO.store(template);

	}

	@Override
	public List<WSAccessControlEntry> getAccessControlList(String sid, long templateId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		validateSession(sid);

		List<WSAccessControlEntry> acl = new ArrayList<>();
		TemplateDAO templateDao = Context.get(TemplateDAO.class);

		Template template = templateDao.findById(templateId);
		templateDao.initialize(template);

		for (AccessControlEntry ace : template.getAccessControlList())
			acl.add(WSUtil.toWSAccessControlEntry(ace));

		return acl;
	}

	@Override
	public void addAttributeOption(String sid, long setId, String attribute, WSAttributeOption wsoption)
			throws AuthenticationException, WebserviceException, PersistenceException {

		validateSession(sid);

		if (wsoption == null)
			return;

		AttributeOptionDAO dao = Context.get(AttributeOptionDAO.class);

		AttributeOption option = new AttributeOption(setId, attribute, wsoption.getValue(), wsoption.getCategory());
		dao.store(option);
	}

}