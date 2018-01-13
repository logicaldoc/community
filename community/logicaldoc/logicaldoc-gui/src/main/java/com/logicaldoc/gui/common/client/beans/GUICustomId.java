package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

public class GUICustomId implements Serializable, Comparable<GUICustomId> {

	public static final String CUSTOMID_SCHEME = "customid-scheme";

	public static final String AUTONAMING_SCHEME = "autonaming-scheme";

	public static final String AUTOFOLDING_SCHEME = "autofolding-scheme";

	private static final long serialVersionUID = 1L;

	private long templateId;

	private String templateName;

	private String scheme;

	private boolean regenerate = false;

	private String type = CUSTOMID_SCHEME;

	public long getTemplateId() {
		return templateId;
	}

	public void setTemplateId(long templateId) {
		this.templateId = templateId;
	}

	public String getScheme() {
		return scheme;
	}

	public void setScheme(String scheme) {
		this.scheme = scheme;
	}

	public boolean isRegenerate() {
		return regenerate;
	}

	public void setRegenerate(boolean regenerate) {
		this.regenerate = regenerate;
	}

	public String getTemplateName() {
		return templateName;
	}

	public void setTemplateName(String templateName) {
		this.templateName = templateName;
	}

	@Override
	public int compareTo(GUICustomId o) {
		int comp = this.type.compareTo(o.getType());
		if (comp == 0)
			return this.templateName.compareTo(o.templateName);
		else
			return comp;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}
}