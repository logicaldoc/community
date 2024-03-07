package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class GUIAttributeSet implements Serializable {

	private static final long serialVersionUID = 1L;

	public static final int TYPE_DEFAULT = 0;

	private long id = 0;

	private String name;

	private String label;

	private String description;

	private boolean readonly = false;

	private int type = TYPE_DEFAULT;

	private List<GUIAttribute> attributes = new ArrayList<>();

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public List<GUIAttribute> getAttributes() {
		return attributes;
	}

	public List<GUIAttribute> getAttributesOrderedByPosition() {
		attributes.sort((arg0, arg1) -> Integer.compare(arg0.getPosition(), arg1.getPosition()));
		return attributes;
	}

	public GUIAttribute getAttribute(String name) {
		if (getAttributes() != null)
			for (GUIAttribute att : getAttributes()) {
				if (att.getName().equals(name))
					return att;
			}
		return null;
	}

	public void appendAttribute(GUIAttribute attribute) {
		attributes.add(attribute);
	}

	public void removeAttribute(String name) {
		List<GUIAttribute> newAttrs = new ArrayList<>();
		for (GUIAttribute att : getAttributes())
			if (!att.getName().equals(name))
				newAttrs.add(att);

		attributes = newAttrs;
	}

	public void repositionAttributes(List<String> names) {
		List<GUIAttribute> newAttrs = new ArrayList<>();
		int i = 0;
		for (String attributeName : names) {
			GUIAttribute att = getAttribute(attributeName);
			att.setPosition(i++);
			newAttrs.add(att);
		}

		attributes = newAttrs;
	}

	public void setAttributes(List<GUIAttribute> attributes) {
		this.attributes = attributes;
	}

	public boolean isReadonly() {
		return readonly;
	}

	public void setReadonly(boolean readonly) {
		this.readonly = readonly;
	}

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}

	public String getLabel() {
		return label;
	}

	public void setLabel(String label) {
		this.label = label;
	}
}