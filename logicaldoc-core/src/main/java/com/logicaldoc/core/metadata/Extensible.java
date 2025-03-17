package com.logicaldoc.core.metadata;

import java.util.List;
import java.util.Map;

public interface Extensible {

	public Long getTemplateId();

	public void setTemplateId(Long templateId);

	public String getTemplateName();

	public void setTemplateName(String templateName);

	public Template getTemplate();

	public void setTemplate(Template template);

	public Map<String, Attribute> getAttributes();

	public void setAttributes(Map<String, Attribute> attributes);

	public List<Attribute> getValueAttributes(String name);

	public List<Object> getValues(String name);

	public Object getValue(String name);

	public Attribute getAttribute(String name);

	public List<String> getAttributeNames();

	public List<String> getAttributeNames(long setId);

	public void removeAttribute(String name);

	public Attribute getAttributeAtPosition(int position);

	public List<Attribute> setValues(String name, List<Object> values);

	/**
	 * Puts a new attribute, if the attribute already exists his position and
	 * label are left untouched
	 * 
	 * @param name name of the attribute
	 * @param attribute the attribute instance
	 */
	public void setAttribute(String name, Attribute attribute);

	public Attribute setValue(String name, Object value);

}