package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

import com.logicaldoc.gui.common.client.Constants;

/**
 * This user interface bean to model a document template
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GUITemplate implements Serializable {

	private static final long serialVersionUID = 1L;

	public static int TYPE_DEFAULT = 0;

	private long id = 0;

	private String name;

	private String description;

	private boolean readonly = false;

	private int type = TYPE_DEFAULT;

	private String validation; 
	
	private GUIAttribute[] attributes;

	private GUIRight[] rights = new GUIRight[] {};
	
	private String[] permissions = new String[] {};

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

	public GUIAttribute getAttribute(String name) {
		if (getAttributes() != null)
			for (GUIAttribute att : getAttributes()) {
				if (att.getName().equals(name))
					return att;
			}
		return null;
	}

	public void appendAttribute(GUIAttribute a) {
		List<GUIAttribute> newAttrs = new ArrayList<GUIAttribute>();
		if (getAttributes() != null)
			newAttrs.addAll(Arrays.asList(getAttributes()));
		newAttrs.add(a);
		attributes = newAttrs.toArray(new GUIAttribute[0]);
	}

	public void removeAttribute(String name) {
		if (getAttribute(name) == null)
			return;

		List<GUIAttribute> newAttrs = new ArrayList<GUIAttribute>();
		for (GUIAttribute att : getAttributes())
			if (!att.getName().equals(name))
				newAttrs.add(att);

		attributes = newAttrs.toArray(new GUIAttribute[0]);
	}

	public void reorderAttributes(List<String> names) {
		List<GUIAttribute> newAttrs = new ArrayList<GUIAttribute>();
		int i = 0;
		for (String name : names) {
			GUIAttribute att = getAttribute(name);
			att.setPosition(i++);
			newAttrs.add(att);
		}
		attributes = newAttrs.toArray(new GUIAttribute[0]);
	}

	public GUIAttribute[] getAttributes() {
		return attributes;
	}

	public GUIAttribute[] getAttributesOrderedByPosition() {
		if (attributes == null)
			return null;

		Arrays.sort(attributes, new Comparator<GUIAttribute>() {

			@Override
			public int compare(GUIAttribute arg0, GUIAttribute arg1) {
				return Integer.valueOf(arg0.getPosition()).compareTo(Integer.valueOf(arg1.getPosition()));
			}
		});
		return attributes;
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

	public GUIRight[] getRights() {
		return rights;
	}

	public void setRights(GUIRight[] rights) {
		this.rights = rights;
	}

	public String[] getPermissions() {
		return permissions;
	}

	public void setPermissions(String[] permissions) {
		this.permissions = permissions;
	}

	public boolean isWrite() {
		return hasPermission(Constants.PERMISSION_WRITE);
	}

	public boolean hasPermission(String permission) {
		if (permissions == null)
			return false;
		for (String p : permissions)
			if (p.equals(permission))
				return true;
		return false;
	}

	public String getValidation() {
		return validation;
	}

	public void setValidation(String validation) {
		this.validation = validation;
	}
}