package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

import com.google.gwt.i18n.client.NumberFormat;

/**
 * Main class for extensible objects
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GUIExtensibleObject implements Serializable {
	private static final long serialVersionUID = 1L;

	private String template;

	private Long templateId;

	private GUIAttribute[] attributes = new GUIAttribute[0];

	public String getTemplate() {
		return template;
	}

	public void setTemplate(String template) {
		this.template = template;
	}

	public Long getTemplateId() {
		return templateId;
	}

	public void setTemplateId(Long templateId) {
		this.templateId = templateId;
	}

	public GUIAttribute[] getAttributes() {
		return attributes;
	}

	public String[] getAttributeNames() {
		List<String> names = new ArrayList<String>();
		if (attributes != null)
			for (GUIAttribute guiAttribute : attributes)
				names.add(guiAttribute.getName());
		return names.toArray(new String[0]);
	}

	public void setAttributes(GUIAttribute[] attributes) {
		this.attributes = attributes;
	}

	public Object getValue(String attributeName) {
		for (GUIAttribute att : attributes) {
			if (att.getName().equals(attributeName) && att.getValue() != null)
				return att.getValue();
		}
		return null;
	}

	public GUIAttribute getAttribute(String attributeName) {
		if (attributes != null)
			for (GUIAttribute att : attributes) {
				if (att.getName().equals(attributeName))
					return att;
			}
		return null;
	}

	public GUIAttribute setValue(String name, Object value) {
		GUIAttribute att = getAttribute(name);
		if (att != null)
			att.setValue(value);
		else {
			att = new GUIAttribute();
			att.setName(name);
			att.setValue(value);
			addAttribute(att);
		}

		return att;
	}

	/**
	 * Retrieves the values of a multiple attribute
	 * 
	 * @param name name of the attribute
	 * 
	 * @return the list of values
	 */
	public List<GUIAttribute> getValues(String name) {
		List<GUIAttribute> values = new ArrayList<GUIAttribute>();

		if (getAttributes() != null)
			for (GUIAttribute att : getAttributes()) {
				if (att.getName().equals(name)) {
					values.add(att);
				} else if (name.equals(att.getParent())) {
					values.add(att);
				}
			}

		values.sort(new Comparator<GUIAttribute>() {
			@Override
			public int compare(GUIAttribute arg0, GUIAttribute arg1) {
				return arg0.getName().compareTo(arg1.getName());
			}
		});

		return values;
	}

	/**
	 * Appends a new value for a given multiple attribute. Returns the new
	 * attribute that represent the value
	 *
	 * @param name name of the attribute
	 * 
	 * @return the attribute
	 */
	public GUIAttribute addAttributeValue(String name) {
		List<GUIAttribute> actualValues = getValues(name);

		GUIAttribute lastValue = actualValues.get(actualValues.size() - 1);
		GUIAttribute newAtt = (GUIAttribute) lastValue.clone();
		newAtt.setValue(null);
		newAtt.setMultiple(false);
		newAtt.setParent(name);
		NumberFormat nf = NumberFormat.getFormat("0000");
		newAtt.setName(name + "-" + nf.format(actualValues.size()));
		putAttributeAfter(lastValue.getName(), newAtt);

		return newAtt;
	}

	public void putAttributeAfter(String name, GUIAttribute att) {
		ArrayList<GUIAttribute> attrs = new ArrayList<GUIAttribute>();
		attrs.addAll(Arrays.asList(attributes));
		int index = attrs.indexOf(getAttribute(name));
		if (index < 0 || index == attrs.size() - 1)
			attrs.add(att);
		else
			attrs.add(index + 1, att);

		// Save back
		attributes = attrs.toArray(new GUIAttribute[0]);
	}

	public void addAttribute(GUIAttribute att) {
		ArrayList<GUIAttribute> attrs = new ArrayList<GUIAttribute>();
		if (attributes != null && attributes.length > 0)
			attrs.addAll(Arrays.asList(attributes));
		attrs.add(att);

		// Save back
		attributes = attrs.toArray(new GUIAttribute[0]);
	}

	public void sortAttributes() {
		if (attributes == null || attributes.length < 1)
			return;
		ArrayList<GUIAttribute> attrs = new ArrayList<GUIAttribute>();
		attrs.addAll(Arrays.asList(attributes));
		attrs.sort(null);
	}

	public void removeAttribute(String name) {
		if (attributes == null || attributes.length < 1)
			return;

		GUIAttribute attribute = getAttribute(name);
		ArrayList<GUIAttribute> attrs = new ArrayList<GUIAttribute>();
		attrs.addAll(Arrays.asList(attributes));
		int index = attrs.indexOf(attribute);
		if (index >= 0 && index < attrs.size())
			attrs.remove(index);
		else
			return;

		attributes = attrs.toArray(new GUIAttribute[0]);

		// Fix the name of multiple values
		if (attribute.getParent() != null) {
			List<GUIAttribute> values = getValues(attribute.getParent());
			values.remove(0);
			int i = 1;
			for (GUIAttribute val : values) {
				NumberFormat nf = NumberFormat.getFormat("0000");
				val.setName(attribute.getParent() + "-" + nf.format(i++));
			}
		}

		attributes = attrs.toArray(new GUIAttribute[0]);

		// Now reorder
		int i = 0;
		for (GUIAttribute guiAttribute : attrs) {
			if (guiAttribute.getParent() != null)
				guiAttribute.setPosition(getAttribute(guiAttribute.getParent()).getPosition());
			else
				guiAttribute.setPosition(i++);
		}

		// And save back
		attributes = attrs.toArray(new GUIAttribute[0]);
	}

	/**
	 * Shifts a value in the list of different values of a multi-value attribute
	 * 
	 * @param name of the attribute
	 * @param up it the position must be moved up
	 */
	public void shiftValue(String name, boolean up) {
		GUIAttribute attribute = getAttribute(name);
		if (attribute == null)
			return;

		/*
		 * Operate in the list of values and shift just the values(not all the
		 * attribue object)
		 */
		List<GUIAttribute> values = getValues(
				attribute.getParent() != null ? attribute.getParent() : attribute.getName());
		int index = values.indexOf(attribute);
		if ((!up && index > values.size() - 1) || (up && index < 1))
			return;

		GUIAttribute actual = attribute.clone();
		GUIAttribute next = values.get(index + (up ? -1 : 1));
		attribute.setIntValue(next.getIntValue());
		attribute.setDateValue(next.getDateValue());
		attribute.setBooleanValue(next.getBooleanValue());
		attribute.setDoubleValue(next.getDoubleValue());
		attribute.setStringValue(next.getStringValue());
		next.setIntValue(actual.getIntValue());
		next.setDateValue(actual.getDateValue());
		next.setBooleanValue(actual.getBooleanValue());
		next.setDoubleValue(actual.getDoubleValue());
		next.setStringValue(actual.getStringValue());

		/*
		 * Report the list to the document
		 */
		for (GUIAttribute attr : values) {
			GUIAttribute a = getAttribute(attr.getName());
			a.setIntValue(attr.getIntValue());
			a.setDateValue(attr.getDateValue());
			a.setBooleanValue(attr.getBooleanValue());
			a.setDoubleValue(attr.getDoubleValue());
			a.setStringValue(attr.getStringValue());
		}
	}
}