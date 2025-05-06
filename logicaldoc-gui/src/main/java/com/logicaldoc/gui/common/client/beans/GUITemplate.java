package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * This user interface bean to model a document template
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GUITemplate implements Serializable {

	private static final long serialVersionUID = 1L;

	public static final int TYPE_DEFAULT = 0;

	private long id = 0;

	private String name;

	private String label;

	private String description;

	private boolean readonly = false;

	private int type = TYPE_DEFAULT;

	private String validation;

	private List<GUIAttribute> attributes = new ArrayList<>();

	private List<String> permissions = new ArrayList<>();

	private List<GUIAccessControlEntry> accessControlList = new ArrayList<>();

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

	public void appendAttribute(GUIAttribute attribute) {
		int maxPosition = attributes.stream().mapToInt(a -> a.getPosition()).max().orElse(0);
		attribute.setPosition(++maxPosition);
		attributes.add(attribute);
	}

	public void removeAttribute(String name) {
		if (getAttribute(name) == null)
			return;

		List<GUIAttribute> newAttrs = new ArrayList<>();
		for (GUIAttribute att : getAttributes())
			if (!att.getName().equals(name))
				newAttrs.add(att);

		attributes = newAttrs;
	}

	public void reorderAttributes(List<String> names) {
		List<GUIAttribute> newAttrs = new ArrayList<>();
		int i = 0;
		for (String attributeName : names) {
			GUIAttribute att = getAttribute(attributeName);
			att.setPosition(i++);
			newAttrs.add(att);
		}
		attributes = newAttrs;
	}

	public List<GUIAttribute> getAttributes() {
		return attributes;
	}

	public List<GUIAttribute> getAttributesOrderedByPosition() {
		attributes.sort((guiAttr1, guiAttr2) -> Integer.compare(guiAttr1.getPosition(), guiAttr2.getPosition()));
		return attributes;
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

	public List<String> getPermissions() {
		return permissions;
	}

	public void setPermissions(List<String> permissions) {
		this.permissions = permissions;
	}

	public GUIAccessControlEntry getAce(long entityId) {
		for (GUIAccessControlEntry acl : accessControlList) {
			if (acl.getEntityId() == entityId)
				return acl;
		}
		return null;
	}

	public void removeAce(long entityId) {
		List<GUIAccessControlEntry> newAce = new ArrayList<>();
		for (GUIAccessControlEntry ace : accessControlList) {
			if (ace.getEntityId() != entityId)
				newAce.add(ace);
		}
		accessControlList = newAce;
	}
	
	public void addAce(GUIAccessControlEntry ace) {
		GUIAccessControlEntry existingAce = getAce(ace.getEntityId());
		if(existingAce==null) {
			accessControlList.add(ace);
		} else {
			existingAce.setRead(ace.isRead());
			existingAce.setWrite(ace.isWrite());
		}
	}

	public List<GUIAccessControlEntry> getAccessControlList() {
		return accessControlList;
	}

	public void setAccessControlList(List<GUIAccessControlEntry> accessControlList) {
		this.accessControlList = accessControlList;
	}

	public boolean isWrite() {
		return hasPermission(GUIAccessControlEntry.PERMISSION_WRITE.toUpperCase());
	}

	public boolean hasPermission(String permission) {
		return permissions.contains(permission);
	}

	public String getValidation() {
		return validation;
	}

	public void setValidation(String validation) {
		this.validation = validation;
	}

	public String getLabel() {
		return label;
	}

	public void setLabel(String label) {
		this.label = label;
	}
}