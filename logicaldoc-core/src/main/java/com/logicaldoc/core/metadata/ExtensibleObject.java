package com.logicaldoc.core.metadata;

import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.apache.commons.collections.CollectionUtils;

import com.logicaldoc.core.PersistentObject;

/**
 * An extensible object is able to store an undeterminate number of attributes.
 * Each attribute has a name and a string value.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
public abstract class ExtensibleObject extends PersistentObject {

	private static final long serialVersionUID = 1L;

	private Map<String, Attribute> attributes = new HashMap<>();

	/**
	 * Implementations may persist or not this attribute
	 */
	private Template template;

	/**
	 * Not persistent. Used sometimes to carry the name of the template
	 */
	private String templateName;

	/**
	 * Not persistent. Used sometimes to carry the name of the template
	 */
	private Long templateId;

	public Long getTemplateId() {
		if (templateId != null)
			return templateId;
		else if (getTemplate() != null)
			return getTemplate().getId();
		else
			return null;
	}

	public void setTemplateId(Long templateId) {
		this.templateId = templateId;
	}

	public String getTemplateName() {
		return templateName;
	}

	public void setTemplateName(String templateName) {
		this.templateName = templateName;
	}

	public Template getTemplate() {
		return template;
	}

	public void setTemplate(Template template) {
		this.template = template;
	}

	public Map<String, Attribute> getAttributes() {
		return attributes;
	}

	public void setAttributes(Map<String, Attribute> attributes) {
		this.attributes = attributes;
	}

	/**
	 * Retrieves the ordered set of the names of the attributes representing the
	 * different values
	 */
	private Set<String> getValueAttributesName(String name) {
		TreeSet<String> attNames = new TreeSet<>();
		for (String n : attributes.keySet()) {
			if (n.equals(name) || name.equals(getAttribute(n).getParent()))
				attNames.add(n);
		}
		return attNames;
	}

	public List<Attribute> getValueAttributes(String name) {
		Set<String> valueNames = getValueAttributesName(name);

		// The names are ordered
		List<Attribute> values = new ArrayList<>();
		for (String n : valueNames) {
			Attribute val = attributes.get(n);
			val.setName(n);
			values.add(val);
		}

		return values;
	}

	public List<Object> getValues(String name) {
		List<Attribute> attrs = getValueAttributes(name);
		return attrs.stream().map(a -> a.getValue()).toList();
	}

	public Object getValue(String name) {
		Attribute att = getAttribute(name);
		if (att != null)
			return att.getValue();
		else
			return null;
	}

	public Attribute getAttribute(String name) {
		if (attributes != null && attributes.get(name) != null)
			return attributes.get(name);
		else
			return null;
	}

	public List<String> getAttributeNames() {
		List<String> names = new ArrayList<>();
		if (attributes != null)
			names = attributes.keySet().stream().toList();
		return names;
	}

	public List<String> getAttributeNames(long setId) {
		List<String> names = new ArrayList<>();
		if (attributes != null) {
			for (String name : attributes.keySet()) {
				Attribute att = getAttribute(name);
				if (att.getSetId() != null && setId == att.getSetId())
					names.add(name);
			}
		}
		return names;
	}

	public void removeAttribute(String name) {
		if (attributes != null && attributes.containsKey(name)) {
			Set<String> valueNames = getValueAttributesName(name);
			for (String n : valueNames)
				attributes.remove(n);
		}
	}

	public Attribute getAttributeAtPosition(int position) {
		if (position < 0)
			return null;
		List<Attribute> attrs = new ArrayList<>(attributes.values());
		if (position >= attrs.size())
			return null;
		Attribute attribute = null;
		for (Attribute extendedAttribute : attrs) {
			if (extendedAttribute.getPosition() == position) {
				attribute = extendedAttribute;
				break;
			}
		}
		return attribute;
	}

	public List<Attribute> setValues(String name, List<Object> values) {
		// clean the attributes that store the actual multiple values
		Set<String> valNames = getValueAttributesName(name);
		for (String n : valNames) {
			if (name.equals(n))
				continue;
			removeAttribute(n);
		}

		List<Attribute> attrs = new ArrayList<>();
		
		if (CollectionUtils.isEmpty(values))
			return attrs;

		Attribute master = setValue(name, values.get(0));
		attrs.add(master);

		if (values.size() > 1) {
			master.setMultiple(1);
			NumberFormat nf = new DecimalFormat("0000");
			for (int i = 1; i < values.size(); i++) {
				Attribute attribute = setValue(name + "-" + nf.format(i), values.get(i));
				attribute.setParent(name);
				attrs.add(attribute);
			}
		}

		for (Attribute attribute : attrs)
			attribute.setPosition(master.getPosition());

		return attrs;
	}

	/**
	 * Puts a new attribute, if the attribute already exists his position and
	 * label are left untouched
	 * 
	 * @param name name of the attribute
	 * @param attribute the attribute instance
	 */
	public void setAttribute(String name, Attribute attribute) {
		Attribute newAttribute = new Attribute(attribute);
		Attribute oldAttribute = attributes.get(name);
		if (oldAttribute != null) {
			newAttribute.setPosition(oldAttribute.getPosition());
			newAttribute.setLabel(oldAttribute.getLabel());
		} else {
			newAttribute.setPosition(getLastPosition() + 1);
		}
		attributes.put(name, newAttribute);
	}

	public Attribute setValue(String name, Object value) {
		Attribute ext = getAttribute(name);
		if (ext == null) {
			ext = new Attribute();
			ext.setPosition(getLastPosition() + 1);
		}
		ext.setValue(value);
		attributes.put(name, ext);
		return attributes.get(name);
	}

	private int getLastPosition() {
		int position = 0;

		if (attributes != null)
			for (Attribute att : attributes.values()) {
				if (position < att.getPosition())
					position = att.getPosition();
			}

		return position;
	}
}