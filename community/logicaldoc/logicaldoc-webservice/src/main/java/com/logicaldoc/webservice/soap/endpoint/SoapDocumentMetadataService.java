package com.logicaldoc.webservice.soap.endpoint;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
import com.logicaldoc.core.security.User;
import com.logicaldoc.util.Context;
import com.logicaldoc.webservice.AbstractService;
import com.logicaldoc.webservice.model.WSAttribute;
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
	protected static Logger log = LoggerFactory.getLogger(SoapDocumentMetadataService.class);

	@Override
	public WSTemplate[] listTemplates(String sid) throws Exception {
		User user = validateSession(sid);
		try {
			List<WSTemplate> templates = new ArrayList<WSTemplate>();
			TemplateDAO dao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
			for (Template template : dao.findAll(user.getTenantId()))
				templates.add(WSUtil.toWSTemplate(template));
			return templates.toArray(new WSTemplate[0]);
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			throw new Exception(t.getMessage());
		}
	}

	@Override
	public WSTemplate getTemplate(String sid, String name) throws Exception {
		User user = validateSession(sid);
		try {
			TemplateDAO dao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
			Template template = dao.findByName(name, user.getTenantId());
			if (template != null)
				return WSUtil.toWSTemplate(template);
			else
				return null;
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			throw new Exception(t.getMessage());
		}
	}

	@Override
	public WSTemplate getTemplateById(String sid, long templateId) throws Exception {
		validateSession(sid);
		try {
			TemplateDAO dao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
			Template template = dao.findById(templateId);
			if (template != null)
				return WSUtil.toWSTemplate(template);
			else
				return null;
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			throw new Exception(t.getMessage());
		}
	}

	@Override
	public long storeTemplate(String sid, WSTemplate template) throws Exception {
		checkAdministrator(sid);
		User user = validateSession(sid);

		try {
			TemplateDAO dao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
			Template templ = WSUtil.toTemplate(template);
			templ.setTenantId(user.getTenantId());

			if (template.getId() != 0) {
				templ = dao.findById(template.getId());
				dao.initialize(templ);
				templ.setName(template.getName());
				templ.setDescription(template.getDescription());
			}

			if (StringUtils.isEmpty(templ.getName()))
				throw new Exception("Missing mandatory value 'Name'");

			Map<String, Attribute> attrs = new HashMap<String, Attribute>();
			if (template.getAttributes() != null && template.getAttributes().length > 0) {
				templ.getAttributes().clear();
				for (WSAttribute attribute : template.getAttributes()) {
					if (attribute != null) {
						Attribute att = new Attribute();
						att.setPosition(attribute.getPosition());
						att.setMandatory(attribute.getMandatory());
						att.setHidden(attribute.getHidden());
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
						attrs.put(attribute.getName(), att);
					}
				}
			}
			if (attrs.size() > 0)
				templ.setAttributes(attrs);

			if (dao.store(templ)) {
				return templ.getId();
			} else
				throw new Exception("Unable to store the attributeSet");
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			throw new Exception(t.getMessage());
		}
	}

	@Override
	public void deleteTemplate(String sid, long templateId) throws Exception {
		checkAdministrator(sid);

		try {
			TemplateDAO dao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
			if (dao.countDocs(templateId) > 0)
				throw new Exception("You cannot delete attributeSet with id " + templateId
						+ " because some documents belongs to that attributeSet.");
			dao.delete(templateId);
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			throw new Exception(t.getMessage());
		}
	}

	@Override
	public void setAttributeOptions(String sid, long setId, String attribute, String[] values) throws Exception {
		checkAdministrator(sid);

		try {
			AttributeOptionDAO dao = (AttributeOptionDAO) Context.get().getBean(AttributeOptionDAO.class);

			if (values == null || values.length == 0) {
				dao.deleteBySetIdAndAttribute(setId, attribute);
				return;
			}

			List<String> valuesList = Arrays.asList(values);

			List<AttributeOption> options = dao.findBySetIdAndAttribute(setId, attribute);
			List<String> oldValues = new ArrayList<String>();
			for (AttributeOption option : options) {
				oldValues.add(option.getValue());
				int index = valuesList.indexOf(option.getValue());
				if (index == -1)
					dao.delete(option.getId());
				else if (options.indexOf(option) != index) {
					option.setPosition(index);
					dao.store(option);
				}
			}
			for (int i = 0; i < values.length; i++) {
				if (!oldValues.contains(values[i])) {
					AttributeOption option = new AttributeOption(setId, attribute, values[i]);
					option.setPosition(i);
					dao.store(option);
				}
			}

		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			throw new Exception(t.getMessage());
		}
	}

	@Override
	public String[] getAttributeOptions(String sid, long setId, String attribute) throws Exception {
		validateSession(sid);
		try {
			AttributeOptionDAO dao = (AttributeOptionDAO) Context.get().getBean(AttributeOptionDAO.class);

			List<AttributeOption> options = dao.findBySetIdAndAttribute(setId, attribute);
			List<String> values = new ArrayList<String>();
			for (AttributeOption option : options)
				values.add(option.getValue());

			return values.toArray(new String[0]);
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			throw new Exception(t.getMessage());
		}
	}

	@Override
	public WSAttributeSet[] listAttributeSets(String sid) throws Exception {
		User user = validateSession(sid);
		try {
			List<WSAttributeSet> templates = new ArrayList<WSAttributeSet>();
			AttributeSetDAO dao = (AttributeSetDAO) Context.get().getBean(AttributeSetDAO.class);
			for (AttributeSet set : dao.findAll(user.getTenantId()))
				templates.add(WSUtil.toWSAttributeSet(set));
			return templates.toArray(new WSAttributeSet[0]);
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			throw new Exception(t.getMessage());
		}
	}

	@Override
	public WSAttributeSet getAttributeSet(String sid, String name) throws Exception {
		User user = validateSession(sid);
		try {
			AttributeSetDAO dao = (AttributeSetDAO) Context.get().getBean(AttributeSetDAO.class);
			AttributeSet set = dao.findByName(name, user.getTenantId());
			if (set != null)
				return WSUtil.toWSAttributeSet(set);
			else
				return null;
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			throw new Exception(t.getMessage());
		}
	}

	@Override
	public WSAttributeSet getAttributeSetById(String sid, long setId) throws Exception {
		validateSession(sid);
		try {
			AttributeSetDAO dao = (AttributeSetDAO) Context.get().getBean(AttributeSetDAO.class);
			AttributeSet set = dao.findById(setId);
			if (set != null)
				return WSUtil.toWSAttributeSet(set);
			else
				return null;
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			throw new Exception(t.getMessage());
		}
	}

	@Override
	public long storeAttributeSet(String sid, WSAttributeSet attributeSet) throws Exception {
		checkAdministrator(sid);
		User user = validateSession(sid);

		try {
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
				throw new Exception("Missing mandatory value 'Name'");

			Map<String, Attribute> attrs = new HashMap<String, Attribute>();
			if (attributeSet.getAttributes() != null && attributeSet.getAttributes().length > 0) {
				set.getAttributes().clear();
				for (WSAttribute attribute : attributeSet.getAttributes()) {
					if (attribute != null) {
						Attribute att = new Attribute();
						att.setPosition(attribute.getPosition());
						att.setMandatory(attribute.getMandatory());
						att.setHidden(attribute.getHidden());
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
						attrs.put(attribute.getName(), att);
					}
				}
			}
			if (attrs.size() > 0)
				set.setAttributes(attrs);

			if (dao.store(set)) {
				return set.getId();
			} else
				throw new Exception("Unable to store the AttributeSet");
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			throw new Exception(t.getMessage());
		}
	}

	@Override
	public void deleteAttributeSet(String sid, long setId) throws Exception {
		checkAdministrator(sid);

		try {
			AttributeSetDAO dao = (AttributeSetDAO) Context.get().getBean(AttributeSetDAO.class);
			dao.delete(setId);
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			throw new Exception(t.getMessage());
		}
	}
}