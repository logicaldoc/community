package com.logicaldoc.core.metadata;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.logicaldoc.core.PersistentObject;

/**
 * An extensible object is able to store an undeterminate number of attributes.
 * Each attribute has a name and a string value.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
public abstract class ExtensibleObject extends PersistentObject {
	private Map<String, Attribute> attributes = new HashMap<String, Attribute>();

	public Map<String, Attribute> getAttributes() {
		return attributes;
	}

	public void setAttributes(Map<String, Attribute> attributes) {
		this.attributes = attributes;
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
		List<String> names = new ArrayList<String>();
		if (attributes != null)
			names.addAll(attributes.keySet());
		return names;
	}

	public List<String> getAttributeNames(long setId) {
		List<String> names = new ArrayList<String>();
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
		if (attributes != null && attributes.containsKey(name))
			attributes.remove(name);
	}

	public Attribute getAttributeAtPosition(int position) {
		if (position < 0)
			return null;
		List<Attribute> attrs = new ArrayList<Attribute>(attributes.values());
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

	/**
	 * Puts a new attribute, if the attribute already exists his position and
	 * label are left untouched
	 */
	public void setAttribute(String name, Attribute attribute) {
		try {
			Attribute newAttribute = (Attribute) attribute.clone();
			Attribute oldAttribute = attributes.get(name);
			if (oldAttribute != null) {
				newAttribute.setPosition(oldAttribute.getPosition());
				newAttribute.setLabel(oldAttribute.getLabel());
			} else {
				newAttribute.setPosition(getLastPosition() + 1);
			}
			attributes.put(name, newAttribute);
		} catch (CloneNotSupportedException e) {

		}
	}

	public Attribute setValue(String name, Object value) {
		Attribute ext = getAttribute(name);
		if (ext == null) {
			ext = new Attribute();
			ext.setPosition(getLastPosition() + 1);
		}
		ext.setValue(value);
		return attributes.put(name, ext);
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