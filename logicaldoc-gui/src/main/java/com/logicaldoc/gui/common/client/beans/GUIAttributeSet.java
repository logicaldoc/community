package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class GUIAttributeSet implements Serializable {

	private static final long serialVersionUID = 1L;

	public static int TYPE_DEFAULT = 0;

	private long id = 0;

	private String name;

	private String description;

	private boolean readonly = false;

	private int type = TYPE_DEFAULT;

	private GUIAttribute[] attributes;

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

	public GUIAttribute[] getAttributes() {
		return attributes;
	}

	public GUIAttribute[] getAttributesOrderedByPosition() {
		if (attributes == null)
			return new GUIAttribute[0];

		Arrays.sort(attributes, (arg0, arg1) -> {
			return Integer.valueOf(arg0.getPosition()).compareTo(Integer.valueOf(arg1.getPosition()));
		});
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

	public void appendAttribute(GUIAttribute a) {
		List<GUIAttribute> newAttrs = new ArrayList<>();
		if (getAttributes() != null)
			newAttrs.addAll(Arrays.asList(getAttributes()));
		newAttrs.add(a);
		attributes = newAttrs.toArray(new GUIAttribute[0]);
	}

	public void removeAttribute(String name) {
		if (getAttribute(name) == null)
			return;

		List<GUIAttribute> newAttrs = new ArrayList<>();
		for (GUIAttribute att : getAttributes())
			if (!att.getName().equals(name))
				newAttrs.add(att);

		attributes = newAttrs.toArray(new GUIAttribute[0]);
	}

	public void reorderAttributes(List<String> names) {
		List<GUIAttribute> newAttrs = new ArrayList<>();
		int i = 0;
		for (String attributeName : names) {
			GUIAttribute att = getAttribute(attributeName);
			att.setPosition(i++);
			newAttrs.add(att);
		}

		attributes = newAttrs.toArray(new GUIAttribute[0]);
	}

	public void setAttributes(GUIAttribute[] attributes) {
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
}