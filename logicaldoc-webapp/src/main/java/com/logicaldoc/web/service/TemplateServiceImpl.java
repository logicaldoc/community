package com.logicaldoc.web.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
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

import com.logicaldoc.core.History;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.folder.FolderHistory;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.AttributeSet;
import com.logicaldoc.core.metadata.AttributeSetDAO;
import com.logicaldoc.core.metadata.ExtensibleObject;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.metadata.TemplateGroup;
import com.logicaldoc.core.metadata.initialization.Initializer;
import com.logicaldoc.core.security.Group;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.User;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIExtensibleObject;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUIForm;
import com.logicaldoc.gui.common.client.beans.GUIRight;
import com.logicaldoc.gui.common.client.beans.GUITemplate;
import com.logicaldoc.gui.frontend.client.services.TemplateService;
import com.logicaldoc.util.Context;

/**
 * Implementation of the TemplateService
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class TemplateServiceImpl extends AbstractRemoteService implements TemplateService {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(TemplateServiceImpl.class);

	@Override
	public void delete(long templateId) throws ServerException {
		Session session = validateSession(getThreadLocalRequest());

		try {
			TemplateDAO dao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
			Template template = dao.findById(templateId);
			if (template == null)
				return;

			if (template.getReadonly() == 1 || !dao.isWriteEnable(templateId, session.getUserId()))
				throw new Exception("You do not have the permission");

			try {
				dao.delete(templateId);
			} catch (Exception e) {
				throw new Exception("Template has not been deleted", e);
			}
		} catch (Throwable t) {
			throwServerException(session, log, t);
		}
	}

	@Override
	public long countDocuments(long templateId) throws ServerException {
		validateSession(getThreadLocalRequest());

		TemplateDAO dao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
		return dao.countDocs(templateId);
	}

	@Override
	public GUITemplate save(GUITemplate template) throws ServerException {
		Session session = validateSession(getThreadLocalRequest());
		User sessionUser = session.getUser();

		TemplateDAO dao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
		try {
			Template templ;
			if (template.getId() != 0) {
				templ = dao.findById(template.getId());
				dao.initialize(templ);
				if (!sessionUser.isMemberOf(Group.GROUP_ADMIN)
						&& (templ.getReadonly() == 1 || !dao.isWriteEnable(templ.getId(), session.getUserId())))
					throw new Exception("You do not have the permission");
			} else {
				templ = new Template();
				if (!sessionUser.isMemberOf(Group.GROUP_ADMIN)
						&& (template.getRights() == null || template.getRights().length == 0)) {
					// At least the current user must have write permission to
					// this
					// template
					TemplateGroup wg = new TemplateGroup(session.getUser().getUserGroup().getId());
					wg.setWrite(1);
					templ.addTemplateGroup(wg);

					GUIRight r = new GUIRight();
					r.setEntityId(wg.getGroupId());
					r.setWrite(true);
					template.setRights(new GUIRight[] { r });
				}
			}

			if (templ.getReadonly() == 0) {
				templ.setTenantId(session.getTenantId());
				templ.setName(template.getName());
				templ.setDescription(template.getDescription());
				templ.setValidation(template.getValidation());
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
			if ((templ.getReadonly() == 0 && dao.isWriteEnable(templ.getId(), session.getUserId()))
					|| sessionUser.isMemberOf(Group.GROUP_ADMIN)) {
				Set<TemplateGroup> grps = new HashSet<TemplateGroup>();
				for (GUIRight right : template.getRights()) {
					boolean isAdmin = right.getEntityId() == 1;
					TemplateGroup wg = null;
					if (right.isRead()) {
						wg = new TemplateGroup();
						wg.setGroupId(right.getEntityId());
					}

					if (wg == null)
						continue;

					grps.add(wg);
					if (isAdmin || right.isWrite())
						wg.setWrite(1);
					else
						wg.setWrite(0);
				}
				templ.getTemplateGroups().clear();
				templ.getTemplateGroups().addAll(grps);
			}

			try {
				dao.store(templ);
			} catch (Exception e) {
				throw new Exception(
						String.format("Template has not been %s", templ.getId() != 0L ? "updated" : "stored"), e);
			}

			template.setId(templ.getId());
		} catch (Throwable t) {
			return (GUITemplate) throwServerException(session, log, t);
		}

		return template;
	}

	@Override
	public GUITemplate getTemplate(long templateId) throws ServerException {
		Session session = validateSession(getThreadLocalRequest());

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
			templ.setValidation(template.getValidation());
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
				att.setDependsOn(templateExtAttr.getDependsOn());
				att.setStringValues(templateExtAttr.getStringValues());
				att.setType(templateExtAttr.getType());
				att.setValidation(templateExtAttr.getValidation());
				att.setInitialization(templateExtAttr.getInitialization());

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
					att.setDateValue(convertToDate(templateExtAttr.getDateValue()));
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

			if (template.getTemplateGroups() != null && !template.getTemplateGroups().isEmpty()) {
				List<GUIRight> rights = new ArrayList<GUIRight>();
				for (TemplateGroup tg : template.getTemplateGroups()) {
					GUIRight right = new GUIRight();
					right.setEntityId(tg.getGroupId());
					right.setRead(true);
					right.setWrite(tg.getWrite() == 1);
					rights.add(right);
				}
				templ.setRights(rights.toArray(new GUIRight[0]));
			}

			return templ;
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
		}

		return null;
	}

	@Override
	public GUIAttribute[] getAttributes(long templateId, GUIExtensibleObject extensibleObject) throws ServerException {
		Session session = null;
		try {
			session = validateSession(getThreadLocalRequest());
		} catch (Throwable t) {
			// Nothing to do
		}

		try {
			TemplateDAO templateDao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
			Template template = templateDao.findById(templateId);
			templateDao.initialize(template);

			GUIAttribute[] attributes = null;

			if (extensibleObject == null) {
				attributes = prepareGUIAttributes(template, null, session != null ? session.getUser() : null);
			} else {
				if (extensibleObject instanceof GUIDocument)
					attributes = prepareGUIAttributes(template,
							DocumentServiceImpl.toDocument((GUIDocument) extensibleObject),
							session != null ? session.getUser() : null);
				else if (extensibleObject instanceof GUIFolder) {
					GUIFolder guiFolder = (GUIFolder) extensibleObject;
					Folder folder = new Folder();
					folder.setId(guiFolder.getId());
					folder.setName(guiFolder.getName());
					folder.setType(guiFolder.getType());
					folder.setTenantId(session != null ? session.getTenantId() : Tenant.DEFAULT_ID);
					folder.setTemplate(template);
					attributes = prepareGUIAttributes(template, folder, session != null ? session.getUser() : null);
				} else if (extensibleObject instanceof GUIForm) {
					Document dummyDoc = new Document();
					dummyDoc.setFileName("webform");
					dummyDoc.setTenantId(template.getTenantId());
					attributes = prepareGUIAttributes(template, dummyDoc, session != null ? session.getUser() : null);
				} else
					attributes = prepareGUIAttributes(template, null, session != null ? session.getUser() : null);
			}

			if (attributes != null)
				Arrays.sort(attributes, new Comparator<GUIAttribute>() {

					@Override
					public int compare(GUIAttribute o1, GUIAttribute o2) {
						return Integer.valueOf(o1.getPosition()).compareTo(Integer.valueOf(o2.getPosition()));
					}
				});

			return attributes;
		} catch (Throwable t) {
			return (GUIAttribute[]) throwServerException(session, log, t);
		}
	}

	public static GUIAttribute[] prepareGUIAttributes(Template template, ExtensibleObject extensibleObject,
			User sessionUser) {
		TemplateDAO tDao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
		tDao.initialize(template);

		AttributeSetDAO setDao = (AttributeSetDAO) Context.get().getBean(AttributeSetDAO.class);

		List<GUIAttribute> attributes = new ArrayList<GUIAttribute>();
		if (template == null || template.getAttributes() == null || template.getAttributes().isEmpty())
			return new GUIAttribute[0];
		try {
			if (template != null) {
				Template currentTemplate = null;
				if (extensibleObject != null && extensibleObject instanceof Document) {
					DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
					Document doc = docDao.findDocument(extensibleObject.getId());
					if (doc != null)
						currentTemplate = doc.getTemplate();
				} else if (extensibleObject != null && extensibleObject instanceof Folder) {
					FolderDAO foldDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
					Folder folder = foldDao.findFolder(extensibleObject.getId());
					if (folder != null)
						currentTemplate = folder.getTemplate();
				} else
					currentTemplate = template;

				if (extensibleObject != null && (extensibleObject.getId() == 0L || !template.equals(currentTemplate))) {
					// Probably the GUI did not fill the attributes map at this
					// point, so put the template's attributes
					if (extensibleObject.getAttributes().isEmpty())
						extensibleObject.setAttributes(template.getAttributes());

					History transaction = null;
					if (extensibleObject instanceof Document) {
						transaction = new DocumentHistory();
						transaction.setDocument((Document) extensibleObject);
					} else if (extensibleObject instanceof Folder) {
						transaction = new FolderHistory();
						transaction.setFolder((Folder) extensibleObject);
					}

					transaction.setUser(sessionUser);

					Initializer initializer = new Initializer();
					initializer.initialize(extensibleObject, template, transaction);
				}

				Map<Long, AttributeSet> sets = setDao.load(template.getTenantId());
				for (String attrName : template.getAttributeNames()) {
					Attribute templateExtAttr = template.getAttributes().get(attrName);
					AttributeSet aSet = sets.get(templateExtAttr.getSetId());
					Attribute setExtAttr = aSet != null ? aSet.getAttribute(attrName) : null;

					GUIAttribute guiAttribute = new GUIAttribute();
					guiAttribute.setName(attrName);
					guiAttribute.setSetId(templateExtAttr.getSetId());
					guiAttribute.setPosition(templateExtAttr.getPosition());
					guiAttribute.setLabel(templateExtAttr.getLabel());
					guiAttribute.setMandatory(templateExtAttr.getMandatory() == 1);
					guiAttribute.setHidden(templateExtAttr.getHidden() == 1);
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
							guiAttribute.setDateValue(attribute.getDateValue());
							if (attribute.getType() == Attribute.TYPE_USER)
								guiAttribute.setUsername(attribute.getStringValue());
						} else
							guiAttribute.setValue(templateExtAttr.getValue());
					}

					// Normalize dates
					if (guiAttribute.getValue() instanceof Date)
						guiAttribute.setValue(convertToDate((Date) guiAttribute.getValue()));

					guiAttribute.setType(templateExtAttr.getType());
					attributes.add(guiAttribute);

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
								if (valAtt.getValue() instanceof Date)
									valAtt.setValue(convertToDate((Date) valAtt.getValue()));

								if (valAtt.getType() == Attribute.TYPE_USER)
									valAtt.setUsername(valAttribute.getStringValue());

								attributes.add(valAtt);
							}
						}
					}

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
							guiAttribute.setStringValue(buf);
						}
						guiAttribute.setOptions(list.toArray(new String[0]));
					}
				}
			}

			Collections.sort(attributes);
			return attributes.toArray(new GUIAttribute[0]);
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			return null;
		}
	}
}