package com.logicaldoc.web.service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;

import org.apache.commons.lang.StringUtils;
import org.hibernate.LazyInitializationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.AbstractDocumentHistory;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.folder.FolderHistory;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.AttributeSet;
import com.logicaldoc.core.metadata.AttributeSetDAO;
import com.logicaldoc.core.metadata.ExtensibleObject;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.metadata.initialization.Initializer;
import com.logicaldoc.core.security.AccessControlEntry;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIExtensibleObject;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUIForm;
import com.logicaldoc.gui.common.client.beans.GUITemplate;
import com.logicaldoc.gui.frontend.client.services.TemplateService;
import com.logicaldoc.util.spring.Context;

/**
 * Implementation of the TemplateService
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class TemplateServiceImpl extends AbstractRemoteService implements TemplateService {

	private static final long serialVersionUID = 1L;

	private static final Logger log = LoggerFactory.getLogger(TemplateServiceImpl.class);

	@Override
	public void delete(long templateId) throws ServerException {
		Session session = validateSession();

		try {
			TemplateDAO dao = Context.get(TemplateDAO.class);
			Template template = dao.findById(templateId);
			if (template == null)
				return;

			if (template.getReadonly() == 1 || !dao.isWriteEnable(templateId, session.getUserId()))
				throw new ServerException("You do not have the permission");

			deleteTemplate(templateId);
		} catch (Exception t) {
			throwServerException(session, log, t);
		}
	}

	private void deleteTemplate(long templateId) throws ServerException {
		try {
			TemplateDAO dao = Context.get(TemplateDAO.class);
			dao.delete(templateId);
		} catch (Exception e) {
			throw new ServerException("Template has not been deleted", e);
		}
	}

	@Override
	public long countDocuments(long templateId) throws ServerException {
		Session session = validateSession();

		TemplateDAO dao = Context.get(TemplateDAO.class);
		try {
			return dao.countDocs(templateId);
		} catch (Exception e) {
			return throwServerException(session, log, e);
		}
	}

	@Override
	public GUITemplate save(GUITemplate guiTemplate) throws ServerException {
		Session session = validateSession();
		User sessionUser = session.getUser();

		try {
			Template template = getTemplate(guiTemplate, session, sessionUser);

			if (template.getReadonly() == 0)
				updateTemplate(guiTemplate, template, session);

			/*
			 * Save the security settings
			 */
			saveACL(template, guiTemplate, session, sessionUser);

			store(template);

			guiTemplate.setId(template.getId());
		} catch (Exception t) {
			return throwServerException(session, log, t);
		}

		return guiTemplate;
	}

	private void store(Template template) throws ServerException {
		try {
			TemplateDAO dao = Context.get(TemplateDAO.class);
			dao.store(template);
		} catch (Exception e) {
			throw new ServerException(
					String.format("Template has not been %s", template.getId() != 0L ? "updated" : "stored"), e);
		}
	}

	@Override
	public GUITemplate clone(long templateId, String cloneName) throws ServerException {
		try {
			TemplateDAO dao = Context.get(TemplateDAO.class);
			Template clone = dao.clone(templateId, cloneName);
			return getTemplate(clone.getId());
		} catch (Exception e) {
			throw new ServerException(String.format("Template %d has not been cloned", templateId), e);
		}
	}

	private void updateTemplate(GUITemplate guiTemplate, Template template, Session session) {
		template.setTenantId(session.getTenantId());
		template.setName(guiTemplate.getName());
		template.setLabel(guiTemplate.getLabel());
		template.setDescription(guiTemplate.getDescription());
		template.setValidation(guiTemplate.getValidation());
		template.setReadonly(guiTemplate.isReadonly() ? 1 : 0);
		template.setType(guiTemplate.getType());

		template.getAttributes().clear();
		for (GUIAttribute attribute : guiTemplate.getAttributes())
			template.getAttributes().put(attribute.getName(), prepareAttribute(attribute));
	}

	private Attribute prepareAttribute(GUIAttribute attribute) {
		Attribute att = new Attribute();
		att.setMandatory(attribute.isMandatory() ? 1 : 0);
		att.setHidden(attribute.isHidden() ? 1 : 0);
		att.setReadonly(attribute.isReadonly() ? 1 : 0);
		att.setMultiple(attribute.isMultiple() ? 1 : 0);
		att.setParent(attribute.getParent());
		att.setDependsOn(attribute.getDependsOn());
		att.setType(attribute.getType());
		att.setLabel(attribute.getLabel());
		att.setEditor(attribute.getEditor());
		att.setStringValue(attribute.getStringValue());
		att.setStringValues(attribute.getStringValues());
		att.setSetId(attribute.getSetId());
		att.setPosition(attribute.getPosition());
		att.setValidation(attribute.getValidation());
		att.setInitialization(attribute.getInitialization());
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
		return att;
	}

	private Template getTemplate(GUITemplate guiTemplate, Session session, User sessionUser)
			throws PersistenceException, ServerException {
		TemplateDAO dao = Context.get(TemplateDAO.class);
		Template template;
		if (guiTemplate.getId() != 0) {
			template = dao.findById(guiTemplate.getId());
			dao.initialize(template);
			if (!sessionUser.isAdmin()
					&& (template.getReadonly() == 1 || !dao.isWriteEnable(template.getId(), session.getUserId())))
				throw new ServerException("You do not have the permission");
		} else {
			template = newTemplate(guiTemplate, session, sessionUser);
		}
		return template;
	}

	private Template newTemplate(GUITemplate guiTemplate, Session session, User sessionUser)
			throws PersistenceException {
		Template template;
		template = new Template();
		if (!sessionUser.isAdmin() && (guiTemplate.getAccessControlList().isEmpty())) {
			// At least the current user must have write permission to
			// this
			// template
			AccessControlEntry ace = new AccessControlEntry(session.getUser().getUserGroup().getId());
			ace.setWrite(1);
			template.addAccessControlEntry(ace);

			GUIAccessControlEntry r = new GUIAccessControlEntry();
			r.setEntityId(ace.getGroupId());
			r.setWrite(true);
			guiTemplate.getAccessControlList().add(r);
		}
		return template;
	}

	private void saveACL(Template template, GUITemplate guiTemplate, Session session, User sessionUser) {
		TemplateDAO dao = Context.get(TemplateDAO.class);
		if ((template.getReadonly() == 1 || !dao.isWriteEnable(template.getId(), session.getUserId()))
				&& !sessionUser.isAdmin())
			return;

		template.getAccessControlList().clear();
		for (GUIAccessControlEntry guiAce : guiTemplate.getAccessControlList()) {
			AccessControlEntry ace = new AccessControlEntry();
			ace.setGroupId(guiAce.getEntityId());
			ace.setRead(guiAce.isRead() ? 1 : 0);
			ace.setWrite(guiAce.isWrite() ? 1 : 0);
			template.getAccessControlList().add(ace);
		}
	}

	@Override
	public GUITemplate getTemplate(long templateId) throws ServerException {
		Session session = validateSession();

		try {
			TemplateDAO dao = Context.get(TemplateDAO.class);
			Template template = dao.findById(templateId);
			if (template == null)
				return null;
			dao.initialize(template);

			return toGuiTemplate(templateId, template, session);
		} catch (Exception t) {
			log.error(t.getMessage(), t);
		}

		return null;
	}

	private GUITemplate toGuiTemplate(long templateId, Template template, Session session) throws PersistenceException {
		GUITemplate guiTemplate = new GUITemplate();
		guiTemplate.setId(templateId);
		guiTemplate.setName(template.getName());
		guiTemplate.setLabel(template.getLabel());
		guiTemplate.setDescription(template.getDescription());
		guiTemplate.setValidation(template.getValidation());
		guiTemplate.setReadonly(template.getReadonly() == 1);
		guiTemplate.setType(template.getType());

		TemplateDAO dao = Context.get(TemplateDAO.class);
		Set<Permission> permissions = dao.getAllowedPermissions(templateId, session.getUserId());
		List<String> permissionsList = new ArrayList<>();
		for (Permission permission : permissions)
			permissionsList.add(permission.toString());
		guiTemplate.setPermissions(permissionsList);

		AttributeSetDAO setDao = Context.get(AttributeSetDAO.class);
		Map<Long, AttributeSet> sets = setDao.load(template.getTenantId());

		toGuiAttributes(template, guiTemplate, sets);
		guiTemplate.getAccessControlList().clear();
		for (AccessControlEntry tg : template.getAccessControlList()) {
			GUIAccessControlEntry guiAce = new GUIAccessControlEntry();
			guiAce.setEntityId(tg.getGroupId());
			guiAce.setRead(tg.getRead() == 1);
			guiAce.setWrite(tg.getWrite() == 1);
			guiTemplate.getAccessControlList().add(guiAce);
		}

		return guiTemplate;
	}

	private void toGuiAttributes(Template template, GUITemplate guiTemplate, Map<Long, AttributeSet> sets) {
		guiTemplate.getAttributes().clear();
		for (String attrName : template.getAttributeNames()) {
			Attribute templateExtAttr = template.getTemplateAttributes().get(attrName);
			AttributeSet aSet = sets.get(templateExtAttr.getSetId());
			Attribute setExtAttr = aSet != null ? aSet.getAttribute(attrName) : null;

			guiTemplate.getAttributes().add(toGuiAttribute(attrName, templateExtAttr, setExtAttr, aSet));
		}
		guiTemplate.getAttributes().sort(null);
	}

	private GUIAttribute toGuiAttribute(String attrName, Attribute templateExtAttr, Attribute setExtAttr,
			AttributeSet aSet) {
		GUIAttribute guiAttribute = new GUIAttribute();
		guiAttribute.setName(attrName);
		guiAttribute.setSetId(templateExtAttr.getSetId());
		guiAttribute.setSet(aSet != null ? aSet.getName() : null);
		guiAttribute.setPosition(templateExtAttr.getPosition());
		guiAttribute.setMandatory(templateExtAttr.getMandatory() == 1);
		guiAttribute.setHidden(templateExtAttr.getHidden() == 1);
		guiAttribute.setReadonly(templateExtAttr.getReadonly() == 1);
		guiAttribute.setMultiple(templateExtAttr.getMultiple() == 1);
		guiAttribute.setParent(templateExtAttr.getParent());
		guiAttribute.setDependsOn(templateExtAttr.getDependsOn());
		guiAttribute.setStringValues(templateExtAttr.getStringValues());
		guiAttribute.setType(templateExtAttr.getType());
		guiAttribute.setValidation(templateExtAttr.getValidation());
		guiAttribute.setInitialization(templateExtAttr.getInitialization());

		if (StringUtils.isEmpty(templateExtAttr.getLabel()))
			guiAttribute.setLabel(attrName);
		else
			guiAttribute.setLabel(templateExtAttr.getLabel());
		if (templateExtAttr.getValue() instanceof String)
			guiAttribute.setStringValue(templateExtAttr.getStringValue());
		else if (templateExtAttr.getValue() instanceof Long)
			guiAttribute.setIntValue(templateExtAttr.getIntValue());
		else if (templateExtAttr.getValue() instanceof Double)
			guiAttribute.setDoubleValue(templateExtAttr.getDoubleValue());
		else if (templateExtAttr.getValue() instanceof Date)
			guiAttribute.setDateValue(convertToDate(templateExtAttr.getDateValue()));
		else if (templateExtAttr.getValue() instanceof Boolean)
			guiAttribute.setBooleanValue(templateExtAttr.getBooleanValue());

		guiAttribute.setEditor(templateExtAttr.getEditor());
		setGuiAttributeOptions(guiAttribute, templateExtAttr, setExtAttr);
		return guiAttribute;
	}

	@Override
	public List<GUIAttribute> getAttributes(long templateId, GUIExtensibleObject extensibleObject)
			throws ServerException {
		User sessionUser = null;
		Session session = null;
		try {
			session = validateSession();
			sessionUser = session.getUser();
		} catch (Exception t) {
			// Nothing to do
		}

		try {
			TemplateDAO templateDao = Context.get(TemplateDAO.class);
			Template template = templateDao.findById(templateId);
			templateDao.initialize(template);

			List<GUIAttribute> attributes;

			if (extensibleObject == null) {
				attributes = prepareGUIAttributes(template, null, sessionUser);
			} else {
				if (extensibleObject instanceof GUIDocument guiDocument)
					attributes = prepareGUIAttributes(template, DocumentServiceImpl.toDocument(guiDocument),
							sessionUser);
				else if (extensibleObject instanceof GUIFolder guiFolder) {
					Folder folder = new Folder();
					folder.setId(guiFolder.getId());
					folder.setName(guiFolder.getName());
					folder.setType(guiFolder.getType());
					folder.setTenantId(session != null ? session.getTenantId() : Tenant.DEFAULT_ID);
					folder.setTemplate(template);
					attributes = prepareGUIAttributes(template, folder, sessionUser);
				} else if (extensibleObject instanceof GUIForm) {
					Document dummyDoc = new Document();
					dummyDoc.setFileName("webform");
					dummyDoc.setTenantId(template.getTenantId());
					dummyDoc.setTemplate(template);
					attributes = prepareGUIAttributes(template, dummyDoc, sessionUser);
				} else
					attributes = prepareGUIAttributes(template, null, sessionUser);
			}

			attributes.sort((o1, o2) -> Integer.compare(o1.getPosition(), o2.getPosition()));

			return attributes;
		} catch (Exception t) {
			return throwServerException(session, log, t);
		}
	}

	List<GUIAttribute> prepareGUIAttributes(Template template, ExtensibleObject extensibleObject, User sessionUser) {
		List<GUIAttribute> attributes = new ArrayList<>();
		if (template == null)
			return attributes;

		template = initializeTemplateAttributes(template);

		AttributeSetDAO setDao = Context.get(AttributeSetDAO.class);
		Map<String, Attribute> attrs = template.getAttributes();
		if (attrs == null || attrs.isEmpty())
			return attributes;

		try {
			Template currentTemplate = loadExtensibleObjectTemplate(template, extensibleObject);

			initializeExtensibleObjectValues(template, currentTemplate, extensibleObject, sessionUser);

			Map<Long, AttributeSet> sets = setDao.load(template.getTenantId());

			for (String attrName : template.getAttributeNames()) {
				Attribute templateExtAttr = attrs.get(attrName);
				AttributeSet aSet = sets.get(templateExtAttr.getSetId());
				Attribute setExtAttr = aSet != null ? aSet.getAttribute(attrName) : null;

				addGuiAttribute(extensibleObject, attrName, attributes, templateExtAttr, setExtAttr);
			}

			Collections.sort(attributes);
			return attributes;
		} catch (Exception t) {
			log.error(t.getMessage(), t);
			return new ArrayList<>();
		}
	}

	private Template initializeTemplateAttributes(Template template) {
		if (template != null) {
			try {
				template.getAttributes();
				int attrsCount = template.getAttributes().size();
				if (log.isDebugEnabled())
					log.debug("Initialized {} attributes", attrsCount);
			} catch (LazyInitializationException e) {
				// If an error happens here it means that the collection could
				// not
				// be loaded, so load the bean again and initialize it.
				log.debug("Got error {} trying to reload the template {}", e.getMessage(), template.getId());
				TemplateDAO tDao = Context.get(TemplateDAO.class);
				try {
					template = tDao.findById(template.getId());
					tDao.initialize(template);
				} catch (PersistenceException pe) {
					log.warn(pe.getMessage(), pe);
				}
			}
		}
		return template;
	}

	private void addGuiAttribute(ExtensibleObject extensibleObject, String attrName, List<GUIAttribute> attributes,
			Attribute templateExtAttr, Attribute setExtAttr) {
		GUIAttribute guiAttribute = new GUIAttribute();
		guiAttribute.setName(attrName);
		guiAttribute.setSetId(templateExtAttr.getSetId());
		guiAttribute.setPosition(templateExtAttr.getPosition());
		guiAttribute.setLabel(templateExtAttr.getLabel());
		guiAttribute.setMandatory(templateExtAttr.getMandatory() == 1);
		guiAttribute.setHidden(templateExtAttr.getHidden() == 1);
		guiAttribute.setReadonly(templateExtAttr.getReadonly() == 1);
		guiAttribute.setMultiple(templateExtAttr.getMultiple() == 1);
		guiAttribute.setParent(templateExtAttr.getParent());
		guiAttribute.setDependsOn(templateExtAttr.getDependsOn());
		guiAttribute.setStringValues(templateExtAttr.getStringValues());
		guiAttribute.setEditor(templateExtAttr.getEditor());
		guiAttribute.setStringValue(templateExtAttr.getStringValue());
		guiAttribute.setIntValue(templateExtAttr.getIntValue());
		guiAttribute.setBooleanValue(templateExtAttr.getBooleanValue());
		guiAttribute.setDoubleValue(templateExtAttr.getDoubleValue());
		guiAttribute.setDateValue(templateExtAttr.getDateValue());

		if (extensibleObject != null) {
			Attribute attribute = extensibleObject.getAttribute(attrName);
			if (attribute != null) {
				guiAttribute.setStringValues(attribute.getStringValues());
				guiAttribute.setStringValue(attribute.getStringValue());
				guiAttribute.setIntValue(attribute.getIntValue());
				guiAttribute.setBooleanValue(attribute.getBooleanValue());
				guiAttribute.setDoubleValue(attribute.getDoubleValue());
				guiAttribute.setDateValue(fixDateForGUI(attribute.getDateValue()));
				if (attribute.getType() == Attribute.TYPE_USER)
					guiAttribute.setUsername(attribute.getStringValue());
			} else
				guiAttribute.setValue(templateExtAttr.getValue());
		}

		// Normalize dates
		if (guiAttribute.getValue() instanceof Date date)
			guiAttribute.setValue(convertToDate(date));

		guiAttribute.setType(templateExtAttr.getType());
		attributes.add(guiAttribute);

		addMultipleGuiAttributes(guiAttribute, extensibleObject, attributes);

		setGuiAttributeOptions(guiAttribute, templateExtAttr, setExtAttr);
	}

	private void setGuiAttributeOptions(GUIAttribute guiAttribute, Attribute templateExtAttr, Attribute setExtAttr) {
		if (templateExtAttr.getType() != Attribute.TYPE_USER && templateExtAttr.getEditor() != Attribute.EDITOR_LISTBOX)
			return;

		String buf = setExtAttr != null ? (String) setExtAttr.getStringValue()
				: (String) templateExtAttr.getStringValue();

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
			guiAttribute.setStringValue(buf);
		}
		guiAttribute.setOptions(list);
	}

	private void addMultipleGuiAttributes(GUIAttribute guiAttribute, ExtensibleObject extensibleObject,
			List<GUIAttribute> attributes) {
		if (guiAttribute.isMultiple() && extensibleObject != null) {
			// Get the other values
			List<Attribute> values = extensibleObject.getValueAttributes(guiAttribute.getName());
			if (values.size() > 1) {
				// Skip the parent attribute
				values.remove(0);

				// Create the GUI attributes for the values
				for (Attribute valAttribute : values) {
					GUIAttribute valAtt = new GUIAttribute(guiAttribute);
					valAtt.setName(valAttribute.getName());
					valAtt.setParent(guiAttribute.getName());
					valAtt.setDependsOn(guiAttribute.getDependsOn());
					valAtt.setMultiple(false);
					valAtt.setPosition(guiAttribute.getPosition());
					valAtt.setBooleanValue(valAttribute.getBooleanValue());
					valAtt.setDateValue(valAttribute.getDateValue());
					valAtt.setDoubleValue(valAttribute.getDoubleValue());
					valAtt.setIntValue(valAttribute.getIntValue());
					valAtt.setStringValue(valAttribute.getStringValue());
					valAtt.setStringValues(null);

					// Normalize dates
					if (valAtt.getValue() instanceof Date date)
						valAtt.setValue(convertToDate(date));

					if (valAtt.getType() == Attribute.TYPE_USER)
						valAtt.setUsername(valAttribute.getStringValue());

					attributes.add(valAtt);
				}
			}
		}
	}

	private void initializeExtensibleObjectValues(Template template, Template currentTemplate,
			ExtensibleObject extensibleObject, User sessionUser) {
		if (extensibleObject != null && (extensibleObject.getId() == 0L || !template.equals(currentTemplate))) {
			// Probably the GUI did not fill the attributes map at this
			// point, so put the template's attributes
			if (extensibleObject.getAttributes().isEmpty())
				extensibleObject.setAttributes(template.getAttributes());

			AbstractDocumentHistory transaction = null;
			if (extensibleObject instanceof Document document) {
				transaction = new DocumentHistory();
				transaction.setDocument(document);
				transaction.setUser(sessionUser);
			} else if (extensibleObject instanceof Folder folder) {
				transaction = new FolderHistory();
				transaction.setFolder(folder);
				transaction.setUser(sessionUser);
			}

			Initializer initializer = new Initializer();
			initializer.initialize(extensibleObject, template, transaction);
		}
	}

	private Template loadExtensibleObjectTemplate(Template template, ExtensibleObject extensibleObject)
			throws PersistenceException {
		Template currentTemplate = null;
		if (extensibleObject instanceof Document) {
			DocumentDAO docDao = Context.get(DocumentDAO.class);
			Document doc = docDao.findDocument(extensibleObject.getId());
			if (doc != null)
				currentTemplate = doc.getTemplate();
		} else if (extensibleObject instanceof Folder) {
			FolderDAO foldDao = Context.get(FolderDAO.class);
			Folder folder = foldDao.findFolder(extensibleObject.getId());
			if (folder != null)
				currentTemplate = folder.getTemplate();
		} else
			currentTemplate = template;
		return currentTemplate;
	}
}